unit UCovers;

{********************
  UCover
  Contains Class managing Covers.Cache File
  File Structure:
  TCC_FileHeader

  TextureData
    * Array of TCC_TextureData

  Indexes
    * TCC_FileIndex Header Block
    * TCC_FileIndex
    * String containing Filename of Last IndexEntry. Ending with #0
         .
         .
    * TCC_FileIndex Footer Block
*********************}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses OpenGL12,
     {$IFDEF win32}
     windows,
     {$ENDIF}
     Math,
     Classes,
     SysUtils,
     {$IFNDEF FPC}
     Graphics,
     {$ENDIF}
     UThemes;

const
  cCC_CoverW        = 128;
  cCC_CoverH        = 128;
  cCC_CoverSize     = cCC_CoverW * cCC_CoverH * 3;
  cCC_HeaderText    = 'USDxCo' + #0 + #1;
  cCC_HeaderVersion = 1000;
  cCC_IndexIndicator= 'I' + 'N' + 'D' + #0;

type
  TCover = record
    Name:       string;
    W:          word;
    H:          word;
    Size:       integer;
    Position:   integer; //position of picture in the cache file
//    Data:       array of byte;
  end;

  //-------------------------------------------------------------------
  //Covers.Cache File Parts

  TCC_Hash = Array [1..32] of Char;
  TCC_FileHeader = record
    FileTyp:    Array [1..8] of Char;  //Some String to detect if this is the file we want to open
    Version:    DWord;      //Version of Covers.Cache File
    Hash:       TCC_Hash;   //Some Randomly Created Alphanumeric String to Identify w/ SongDB
    CoverW:     Word;       //Width of all Covers in Cache
    CoverH:     Word;       //Height of all Covers in Cache
    DataStart:  Cardinal;   //Start Position in Bytes of Data Part
    DataLength: Cardinal;   //Start Position in Bytes of Data Part
    IndexStart: Cardinal;   //Start of Index Block in Bytes
  end;

  PCC_TextureData = ^TCC_TextureData;
  TCC_TextureData = Array [0..cCC_CoverSize - 1] of Byte;

  TCC_FileIndexHeader = record
    Indicator: Array [1..4] of Char;    //Contains INDE
    HighestID: Integer;                //Highest ID of a Cover
  end;

  TCC_FileIndex = record
    LastUpdated: Integer;   //Time of LastFilechange
    DataStart: Cardinal;    //Position of TextureData of this Cover in Bytes.
                            //0 if this is empty slot(Deleted Cover)(Id not available)
                            //1 if the Texture Data is not already loaded to Cache
                            //High(Cardinal) if this is IndexFooter
    //Filename: String;
  end;

  TCC_IndexListItem = record
    Filename: String;
    TexID:    Integer;
    FileIndex: TCC_FileIndex;
  end;
  TCC_IndexList = Array of TCC_IndexListItem;

  TCovers = class
    private
      Filename:      String;

      Header:        TCC_FileHeader;
      HighestID:     Integer;
      Count:         Cardinal;

      Index:         TCC_IndexList;

      IndexNeedRewrite: Boolean; //Index in CacheFile is overwritten by other Data
      CacheReadOnly: Boolean;    //Cache File is read only

      Function WriteHeader(const ReWriteCache: Boolean = false):Boolean;
      Function ReadHeader: Boolean;
      Function ReadIndex:  Boolean;
      Function WriteIndex(const ReWriteCache: Boolean = false): Boolean;
      Function AddTexData(Data: PCC_TextureData): Cardinal;
    public
      W:          word;
      H:          word;
      Size:       integer;
      Data:       array of byte;
      Cover:      array of TCover;

      property Hash: TCC_Hash read Header.Hash;

      constructor Create(const Filename: String);
      procedure Load(const Filename: String);
      Function  AddCover(FileName: string): Integer;    //Returns ID, Checks Cover for Change, Updates Cover if required
      function  CoverExists(FileName: string): Integer; //Returns ID by FilePath
      procedure PrepareData(FileName: string);
      Procedure LoadTextures;
      Function  ReWriteCache: Boolean;                 //Deletes old cover.cache file and writes new one

      Function  GetTexbyID(ID: Cardinal): Integer;
  end;

var
  Covers:     TCovers;
 // to - do : new Song management
implementation

uses UMain,
     // UFiles,
     ULog,
     UTexture,
     DateUtils;

constructor TCovers.Create(const Filename: String);
begin
  HighestID := -1;
  SetLength(Index, HighestID + 2);
  //Load(Filename);
end;

//----------------------------------------------
// Some File handling helpers

//--------
// Reads and Checks Header. Returns True if Header is correct
//--------
Function TCovers.ReadHeader:   Boolean;
var
  F: File of TCC_FileHeader;
begin
  try
    //Read Header
    AssignFile(F, Filename);

    try
      Reset(F);
      Read(F, Header);
    finally
      CloseFile(F);
    end;

    //Check Header
    If (Header.FileTyp = cCC_HeaderText) AND
       (Header.Version = cCC_HeaderVersion) then
    begin
      Result := True;
      IndexNeedRewrite := True;
    end
    Else
      Result := False;


  except
    Result := False;
  end;
end;

//--------
// Writes Header(Resets File). Returns True if Writing succeed 
//--------
Function TCovers.WriteHeader(const ReWriteCache: Boolean):Boolean;
var
  F: File of TCC_FileHeader;
begin
  try
    Result := True;
    //Read Header
    AssignFile(F, Filename);
    try
      If (not FileExists(Filename)) OR (ReWriteCache) then
        ReWrite(F)
      else
        Reset(F);
      
      Write(F, Header);
    finally
      CloseFile(F);
    end;
  except
    Result := False;
  end;
end;

//--------
// Reads and Checks Index. Returns True if Index is correct
//--------
Function TCovers.ReadIndex:  Boolean;
var
  F: File of Byte;
  IndexHeader: TCC_FileIndexHeader;
  I: Integer;

  Procedure mReadLn(var S: String);
  var
    Len: Integer;
  begin
    S := '';

    BlockRead(F, Len, 4); //Read Len of Filename String

    //Read Filename String
    SetLength(S, Len);
    BlockRead(F, S[1], Len);
  end;
begin
  try
    //Read Header
    AssignFile(F, Filename);
      try
        Reset(F);
        Seek(F, Header.IndexStart);

        BlockRead(F, IndexHeader, SizeOf(TCC_FileIndexHeader));

        If (IndexHeader.Indicator = cCC_IndexIndicator) then
        begin
          Log.LogError('TCovers: loading Cover Index Header. HighestID: ' + InttoStr(IndexHeader.HighestID));
          HighestID := IndexHeader.HighestID;
          SetLength(Index, HighestID + 2);

          Count := 0;
          Result := True;
          If (HighestID >= 0) then
          begin
            //Read File Infos until (Eof or Footer)
            I := 0;
            //While (Not Eof(F)) AND ((I <= 0) OR (Index[I-1].FileIndex.DataStart <> High(Cardinal))) do
            Repeat
              Log.LogError('TCovers: loading Cover Index. Position #' + InttoStr(I));
              If (I > HighestID + 1) then
              begin //Header IndexCOunt was wrong, running out of array
                Log.LogError('TCovers: Wrong HighestID in Index Header. Running out of Array at Postion #' + InttoStr(I));
                Inc(HighestID);
                IndexNeedReWrite := True;
                SetLength(Index, HighestID + 2);
              end;

              BlockRead(F, Index[I].FileIndex, SizeOf(TCC_FileIndex));
              Index[I].TexID    := -1;

              If (Index[I].FileIndex.DataStart = High(Cardinal)) then
              begin //Found Footer
                Log.LogError('TCovers: Found footer at Position #' + InttoStr(I));
                Break;
              end;

              If (Not Eof(F)) then
              begin
                //Read Filename
                mReadLn(Index[I].Filename);

                Log.LogError('TCovers: Cover loaded: ' + Index[I].Filename);
                If (Index[I].FileIndex.DataStart <> 0) AND (Index[I].FileIndex.DataStart <> 1) then
                  Inc(Count);
              end;

              Inc(I);
            Until Eof(F);

            If (Index[HighestID + 1].FileIndex.DataStart = High(Cardinal)) then
            begin //No Footer found
              IndexNeedReWrite := True;
            end;
          end;

        end;

      finally
        CloseFile(F);
      end;
  except
    Result := False;
  end;
end;

//--------
// Writes Index. Returns True if Writing succeed
//--------
Function TCovers.WriteIndex(const ReWriteCache: Boolean): Boolean;
var
  F: File of Byte;
  IndexHeader: TCC_FileIndexHeader;
  I: Integer;

  Procedure mWriteLn(var S: String);
  var Len: Integer;
  begin
    //Write Length of String
    Len := Length(S);
    BlockWrite(F, Len, 4);

    //Write String
    BlockWrite(F, S[1], Len);
  end;
begin
  Result := WriteHeader(ReWriteCache);

  If (Result) then
  begin
    try
      //Read Header
      AssignFile(F, Filename);
      try
        Reset(F);

        Seek(F, Header.IndexStart);
        //Write Header
        IndexHeader.Indicator := cCC_IndexIndicator;
        IndexHeader.HighestID := HighestID;

        BlockWrite(F, IndexHeader, SizeOf(TCC_FileIndexHeader));

        Count := 0;
        Result := True;

        //Prepare Footer
        Index[HighestID + 1].FileIndex.DataStart := High(Cardinal);

        // Write Fileinfo
        For I := 0 to HighestID+1 do
        begin
          BlockWrite(F, Index[I].FileIndex, SizeOf(TCC_FileIndex));
          If (I <= HighestID) then
            mWriteLn(Index[I].Filename);
        end;

        IndexNeedRewrite := False;

      finally
        CloseFile(F);
      end;
    except
      Result := False;
    end;
  end;
end;

//--------
// Writes some Texture Data to the End of TextureData Block
//--------
Function TCovers.AddTexData(Data: PCC_TextureData): Cardinal;
var
  F: File of Byte;
begin
  try
    AssignFile(F, Filename);
    try
      Reset(F);
      Seek(F, Header.DataStart + Header.DataLength);

      BlockWrite(F, Data^, SizeOf(TCC_TextureData));

      Result := Header.DataStart + Header.DataLength;
      Inc(Header.DataLength, SizeOf(TCC_TextureData));

      Header.IndexStart := Header.DataStart + Header.DataLength + 1;
      IndexNeedReWrite := True;
    finally
      CloseFile(F);
    end;
  except
    Result := 0;
  end;
end;

procedure TCovers.Load(const Filename: String);
var
  Succeed: Boolean;
begin
  Log.LogError('TCovers: Load cache from file: ''' + Filename + '''');

  Self.Filename := Filename;
  Succeed := False;
  If (FileExists(Filename)) then
  begin
    CacheReadOnly := FileisReadOnly(Filename);
    If (ReadHeader) then
    begin //Header successful read
      If (ReadIndex) then
      begin
        Succeed := True;
      end;
    end;
  end;

  If not Succeed and not CacheReadOnly then
    If not (ReWriteCache) then
    begin
      CacheReadOnly := True;
      Log.LogError('TCovers: Cache readonly!');
    end;
end;

Function TCovers.AddCover(FileName: string): Integer;
var I: Integer;
begin
  Result := CoverExists(Filename);
  If (Result = -1) then
  begin //Add Cover(Does not exist)
    Log.LogError('TCovers: Adding cover: ''' + Filename + '''');
    If (Count <= HighestID) then
    begin //There is an empty slot, Search It
      Log.LogError('TCovers: Searching for Empty Slot');
      For I := 0 to HighestID do
        If (Index[I].FileIndex.DataStart = 0) then
        begin //Found that Slot
          Result := I;
          Break;
        end;
    end;

    If (Result = -1) then
    begin //Attach it to the End
      Log.LogError('TCovers: Attach Cover to the end');
      Inc(HighestID);
      SetLength(Index, HighestID + 2);
      Result := HighestID;
    end;

    Index[Result].Filename := Filename;
    Index[Result].TexID    := -1;
    Index[Result].FileIndex.DataStart := 1;
    Log.LogError('TCovers: Cover Added, ID: ' + InttoStr(Result));
  end
  else
  begin //Check if File has Changed
    If (Index[Result].FileIndex.LastUpdated < 0) then
    begin

    end;
  end;
end;

Function TCovers.CoverExists(FileName: string): integer;
var
  I:    integer;
begin
  Result := -1;
  {$IFDEF MSWINDOWS}
    Filename := Uppercase(Filename);
  {$ENDIF}

  For I := 0 to HighestID do
  begin
    If (Index[I].FileIndex.DataStart <> 0) AND
   {$IFDEF MSWINDOWS}
       (Uppercase(Index[I].Filename) = Filename)
   {$ELSE}
       (Index[I].Filename = Filename)
   {$ENDIF}  then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

//--------
// Deletes old cover.cache file and writes new one
//--------
Function  TCovers.ReWriteCache: Boolean;

  Function MakeHash: TCC_Hash;
    const AlphaNumeric: Array[0..35] of Char = ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
    var I: Integer;
  begin
    For I := Low(Result) to High(Result) do
      Result[I] := AlphaNumeric[Random(Length(AlphaNumeric))];
  end;
begin
  If not CacheReadOnly then
  begin
    Log.LogError('TCovers: Rewriting Cache');
    
    Header.FileTyp    := cCC_HeaderText;
    Header.Version    := cCC_HeaderVersion;
    Header.Hash       := MakeHash;
    Header.CoverW     := cCC_CoverW;
    Header.CoverH     := cCC_CoverH;
    Header.DataStart  := SizeOf(TCC_FileHeader) + 4; //Total of 64 Bytes (4 Bytes Space)
    Header.DataLength := 0;
    Header.IndexStart := Header.DataStart + Header.DataLength + 4;

    HighestID := -1;
    SetLength(Index, HighestID + 2);

    Result := WriteIndex(True);
  end
  else
    Result := False;
end;

procedure TCovers.PrepareData(FileName: string);
var
  F:  File;
  C:  integer;
begin
  if FileExists(GamePath + 'covers.cache') then
  begin
    AssignFile(F, GamePath + 'covers.cache');
    Reset(F, 1);

    C := CoverExists(FileName);
    SetLength(Data, Cover[C].Size);
    if Length(Data) < 6 then
      Log.LogStatus('Length(Data) < 6', 'TCovers.PrepareData');
    Seek(F, Cover[C].Position);
    BlockRead(F, Data[0], Cover[C].Size);
    CloseFile(F);
  end;
end;

Procedure TCovers.LoadTextures;
var
  I: Integer;
  TexData: PCC_TextureData;
  CachedData: TCC_TextureData;
  F: File of Byte;

  Function LoadCover: Integer;
  begin
    Result := -1;
    
    If (Index[I].FileIndex.DataStart = 1) then
    begin //This Texture is new and has to be loaded
      TexData := Texture.GetCoverThumbnail(Index[I].Filename);
      If (TexData <> nil) then
      begin
        If not (CacheReadonly) then
        begin //Save this Tex to Cache
          Index[I].FileIndex.DataStart :=  AddTexData(TexData);
          If (Index[I].FileIndex.DataStart = 0) then
          begin
            CacheReadOnly := True; //Failed to write Data
            Log.LogError('Failed to Write TextureData to Cache');
          end;
        end;

        //Create Texture
        glGenTextures(1, @Result);

        glBindTexture(GL_TEXTURE_2D, Result);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

        //glTexImage2D(GL_TEXTURE_2D, 0, 3, cCC_CoverW, cCC_CoverH, 0, GL_RGB, GL_UNSIGNED_BYTE, Data);
        glTexImage2D(GL_TEXTURE_2D, 0, 3, cCC_CoverW, cCC_CoverH, 0, GL_RGB, GL_UNSIGNED_BYTE, TexData);
      end
      else
        Log.LogError('Couldn''t get Thumbnail Data');
    end
    Else If (Index[I].FileIndex.DataStart > 1) then
    begin //This texture is already in Cache, Load it from there
      try
        Log.LogError('TCovers: Loading Cover #' + InttoStr(I) + ' from Cache at Position: ' + InttoStr(Index[I].FileIndex.DataStart));
        Assign(F, Filename);
        try
          Reset(F);
          Seek(F, Index[I].FileIndex.DataStart);
          BlockRead(F, CachedData, SizeOf(TCC_TextureData));
        finally
          CloseFile(F);
        end;


        //Create Texture
        glGenTextures(1, @Result);

        if (Result > 0) then
        begin

          glBindTexture(GL_TEXTURE_2D, Result);
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

          //glTexImage2D(GL_TEXTURE_2D, 0, 3, cCC_CoverW, cCC_CoverH, 0, GL_RGB, GL_UNSIGNED_BYTE, Data);
          glTexImage2D(GL_TEXTURE_2D, 0, 3, cCC_CoverW, cCC_CoverH, 0, GL_RGB, GL_UNSIGNED_BYTE, @CachedData[0]);
        end
        else
          Log.LogError('TCovers: Error Generating Texture');
      except
        Log.LogError('TCovers: Error during loading');
      end;
    end;
  end;
begin
  Texture.SetCoverSize(cCC_CoverW, cCC_CoverH);
  Log.LogError('TCovers: LoadingTextures');

  For I := 0 to HighestID do
  begin //Load all the Covers
    If (Index[I].FileIndex.DataStart > 0) then
      Index[I].TexID := LoadCover; //No empty SLot -> Load the Texture

    Log.LogError('TCovers: Texture for ID#' + InttoStr(I) + ': ' + InttoStr(Index[I].TexID));
  end;

  If IndexNeedRewrite then
    WriteIndex;
end;

Function  TCovers.GetTexbyID(ID: Cardinal): Integer;
begin
  If (ID <= HighestID) then
    Result := Index[ID].TexID
  else
    Result := -1;
end;

end.
