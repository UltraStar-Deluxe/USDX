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
    HighestID: Cardinal;                //Highest ID of a Cover
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

      Function WriteHeader:Boolean;
      Function ReadHeader: Boolean;
      Function ReadIndex:  Boolean;
      Function WriteIndex: Boolean;
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
      Function AddCover(FileName: string): Integer;    //Returns ID, Checks Cover for Change, Updates Cover if required
      function CoverExists(FileName: string): Integer; //Returns ID by FilePath
      procedure PrepareData(FileName: string);
      Procedure LoadTextures;
      Function  ReWriteCache: Boolean;                 //Deletes old cover.cache file and writes new one
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
  Load(Filename);
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
Function TCovers.WriteHeader:Boolean;
var
  F: File of TCC_FileHeader;
begin
  try
    Result := True;
    //Read Header
    AssignFile(F, Filename);
    try
      If (not FileExists(Filename)) then
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
  var J: Integer;
  begin
    S := '';
    J := 0;

    Repeat
      Inc(J);
      SetLength(S, J);
      Read(F, Byte(S[J]));
    Until S[J] = #10
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
          Result := False;

          HighestID := IndexHeader.HighestID;
          SetLength(Index, HighestID + 2);

          Count := 0;
          Result := True;
          If (HighestID > 0) then
          begin
            //Read File Infos until (Eof or Footer)
            I := 0;
            While (Not Eof(F)) AND ((I <= 0) OR (Index[I].FileIndex.DataStart <> High(Cardinal))) do
            begin
              If (I > HighestID) then
              begin //Header IndexCOunt was wrong, running out of array
                Inc(HighestID);
                IndexNeedReWrite := True;
                SetLength(Index, HighestID + 2);
              end;

              BlockRead(F, Index[I].FileIndex, SizeOf(TCC_FileIndex));
              Index[I].TexID    := -1;

              If (Index[I].FileIndex.DataStart <> High(Cardinal)) AND (Not Eof(F)) then
              begin
                //Read Filename
                mReadLn(Index[I].Filename);
                Inc(Count);
              end;

              Inc(I);
            end;

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
Function TCovers.WriteIndex: Boolean;
var
  F: File of Byte;
  IndexHeader: TCC_FileIndexHeader;
  I: Integer;

  Procedure mWriteLn(var S: String);
  var N: Byte;
  begin
    BlockWrite(F, S, Length(S));
    N := Byte(#10);
    Write(F, N);
  end;
begin
  Result := WriteHeader;

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
          mWriteLn(Index[I].Filename);
        end;

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

      BlockWrite(F, Data, SizeOf(TCC_TextureData));

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
      CacheReadOnly := True;
end;

Function TCovers.AddCover(FileName: string): Integer;
var I: Integer;
begin
  Result := CoverExists(Filename);
  If (Result = -1) then
  begin //Add Cover(Does not exist)
    If (Count <= HighestID) then
    begin //There is an empty slot, Search It
      For I := 0 to HighestID do
        If (Index[I].FileIndex.DataStart = 0) then
        begin //Found the Slot
          Result := I;
          Break;
        end;
    end;

    If (Result = -1) then
    begin //Attach it to the End
      Inc(HighestID);
      SetLength(Index, HighestID + 2);
      Result := HighestID;
    end;

    Index[Result].Filename := Filename;
    Index[Result].TexID    := -1;
    Index[Result].FileIndex.DataStart := 1;
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
    Header.FileTyp    := cCC_HeaderText;
    Header.Version    := cCC_HeaderVersion;
    Header.Hash       := MakeHash;
    Header.CoverW     := cCC_CoverW;
    Header.CoverH     := cCC_CoverH;
    Header.DataStart  := SizeOf(TCC_FileHeader) + 4; //Total of 64 Bytes (4 Bytes Space)
    Header.DataLength := 0;
    Header.IndexStart := Header.DataStart + Header.DataLength + 4;

    Result := WriteIndex;
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
  
  Function LoadCover: Integer;
  begin

  end;
begin
  //Texture.SetCoverSize(cCC_CoverW, cCC_CoverH);

  For I := 0 to HighestID do
  begin //Load all the Covers
    {Index[I].TexID

    Index[I].FileIndex.}
  end;
end;

end.
