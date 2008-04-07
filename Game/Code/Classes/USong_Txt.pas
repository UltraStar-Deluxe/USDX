unit USong_Txt;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}


uses
  Classes,
  SysUtils,
  USong_TextFile;

type
  {*******************
    Child of the new TSong class.
    implements methods to load a song form a txt file (ultrastar file format)
   *******************}
  TSong_Txt = class(TSong_TextFile)
    public
      Procedure ReadHeader; override;        //Reads Fileheader (Implemented by Child only)
      Procedure ReadFile; override;          //Reads complete File (Header + Notes) (Implemented by Child only)
      Procedure WriteFile; override;         //Writes complete File (Header + Notes) (Implemented by Child only)
  end;  
  
implementation

uses
  UMusic,
  UMain,
  UPlatform,
  ULog;

type
  TTags = (ArtistTag, TitleTag, Mp3Tag, BPMTag);
  
//--------
// Reads Fileheader
//--------
Procedure TSong_Txt.ReadHeader;
var
  started: Boolean;
  line, key, value: String;
  FloatValue: Real;
  FoundTags: Set of TTags;
  
  // splits a headerline in key an value
  // returns true on success and false if this is not a valid headerline
  Function SplitHeaderLine(const Line: String; var Key, Value: String): Boolean;
  var
    idx: Integer;
  begin
    Result := False;
    
    idx := Pos(':', Line);
    if idx > 0 then
    begin
      Key := Uppercase(Trim(Copy(Line, 2, idx - 2))); //Uppercase is for Case Insensitive Checks
      Value := Trim(Copy(Line, idx + 1,Length(Line) - idx));
      
      if (Length(Key) > 0) AND (Length(Value) > 0) then
        Result := True
    end;
  end;
  
  function song_StrToFloat(const aValue : String): Extended;
  var
    lValue: String;
  begin
    lValue := aValue;

    if (Pos(',', lValue) <> 0) then
      lValue[Pos(',', lValue)] := '.';

    Result := StrToFloatDef(lValue, 0);
  end;
  
begin
  if not OpenSongFile then
    exit;
  
  started := False;
  FoundTags := [];
  
  while isDataAvailable do
  begin
    line := GetNextLine();
    
    // break if header has finished
    if started AND ((Length(line) > 0) AND (not (line[1] = '#'))) then
      break;
    
    // skip invalid lines at beginning
    if (not started) AND (line[1] = '#') then
      started := True;
    
    // parse line
    if started then
    begin
      if (Length(line) > 0) AND not SplitHeaderLine(line, key, value) then
      begin
        Log.LogError('Invalide line in Header of file: ' + FilePath + FileName);
        Log.LogError('Line: ' + line);
        break;
      end;
      
      {$IFDEF UTF8_FILENAMES}
      if ((Key = 'MP3') or (Key = 'BACKGROUND') or (Key = 'COVER') or (Key = 'VIDEO')) then
        Value := Utf8Encode(Value);
      {$ENDIF}

      //Title
      if (Key = 'TITLE') then
      begin
        Title := Value;
        FoundTags := FoundTags + [TitleTag];
        continue;
      end;

      //Artist
      if (Key = 'ARTIST') then
      begin
        Artist := Value;
        FoundTags := FoundTags + [ArtistTag];
        continue;
      end;

      //MP3 File //Test if Exists
      if (Key = 'MP3') then
      begin
        if FileExists(FilePath + Value) then
        begin
          Mp3 := FilePath + Value;
          FoundTags := FoundTags + [Mp3Tag];
        end
        else
          Log.LogError('Can''t find MP3 File: ' + FilePath + Value + ' in Song: ' + FilePath + FileName);
        continue;
      end;    

      //Beats per Minute
      if (Key = 'BPM') then
      begin
        FloatValue := song_StrtoFloat( Value ) * Mult * MultBPM;

        if FloatValue <> 0 then
        begin
          SetLength(BPM, 1);
          BPM[0].StartBeat := 0;
          BPM[0].BPM := floatValue;
          FoundTags := FoundTags + [BPMTag];
        end;
        
        continue;
      end;

      //---------
      //Additional Header Information
      //---------

      // Gap
      if (Key = 'GAP') then
      begin
        GAP := song_StrtoFloat( Value );
        
        continue;
      end;

      //Cover Picture
      if (Key = 'COVER') then
      begin
        if FileExists(FilePath + Value) then
          Cover := FilePath + Value
        else
          Log.LogError('Can''t find Cover File: ' + FilePath + Value + ' in Song: ' + FilePath + FileName);
        
        continue;  
      end;

      //Background Picture
      if (Key = 'BACKGROUND') then
      begin
        if FileExists(FilePath + Value) then
          Background := FilePath + Value
        else
          Log.LogError('Can''t find Background File: ' + FilePath + Value + ' in Song: ' + FilePath + FileName);
        
        continue;
      end;

      // Video File
      if (Key = 'VIDEO') then
      begin
        if FileExists(FilePath + Value) then
          Video := FilePath + Value
        else
          Log.LogError('Can''t find Video File: ' + FilePath + Value + ' in Song: ' + FilePath + FileName);
        
        continue;
      end;

      // Video Gap
      if (Key = 'VIDEOGAP') then
      begin
        VideoGAP := song_StrtoFloat(Value);
        continue;
      end;

      //Genre Sorting
      if (Key = 'GENRE') then
      begin
        Genre := Value;
        continue;
      end;

      //Edition Sorting
      if (Key = 'EDITION') then
      begin
        Edition := Value;
        continue;
      end;

      //Creator Tag
      if (Key = 'CREATOR') then
      begin
        Creator := Value;
        continue;
      end;

      //Language Sorting
      if (Key = 'LANGUAGE') then
      begin
        Language := Value;
        continue;
      end;

      // Song Start
      if (Key = 'START') then
      begin
        Start := song_StrtoFloat( Value );
        continue;
      end;

      // Song Ending
      if (Key = 'END') then
      begin
        TryStrtoInt(Value, Finish);
        continue;
      end;

      // Resolution
      if (Key = 'RESOLUTION') then
      begin
        TryStrtoInt(Value, Resolution);
        continue;
      end;

      // Notes Gap
      if (Key = 'NOTESGAP') then
      begin
        TryStrtoInt(Value, NotesGAP);
        continue;
      end;
      
      // Relative Notes
      if (Key = 'RELATIVE') AND (uppercase(Value) = 'YES') then
      begin  
        Relative := True;
        continue;
      end;
      
    end; // if started
  end; // while
  
  if Cover = '' then
  begin
    Cover := platform.FindSongFile(FilePath, '*[CO].jpg');
  end;
  
  // check if all required infos are given
  if not (BPMTag in FoundTags) then
    Log.LogError('BPM Tag missing: ' + FilePath + FileName);
  
  if not (MP3Tag in FoundTags) then
    Log.LogError('MP3 Tag missing or invalid file: ' + FilePath + FileName);
  
  if not (ArtistTag in FoundTags) then
    Log.LogError('Artist Tag missing: ' + FilePath + FileName);
  
  if not (TitleTag in FoundTags) then
    Log.LogError('Title Tag missing: ' + FilePath + FileName);
  
  CloseSongFile();
end;

//--------
// Reads complete File (Header + Notes)
//--------
Procedure TSong_Txt.ReadFile;
var
  Line: String;
  NotesRead: Boolean;
  Values: TStringList;
  
  Procedure ParseDelimited(const StringList: TStringList; const Value: String; const Delimiter: String; const MaxParts: Integer);
  var
    idx: Integer;
    Source: String;
    Delta: Integer;
  begin
    Delta := Length(Delimiter);
    Source := Value;
    StringList.BeginUpdate;
    StringList.Clear;
    try
      while ((Pos(Delimiter, Source) > 0) OR ((MaxParts > 0) AND  (StringList.count+1 >= MaxParts))) do
      begin
        idx := Pos(Delimiter, Source);
        StringList.Add(Copy(Source,0,idx-1));
        Source := Copy(Source,idx+Delta, MaxInt);
      end;
             
      if (Length(Source) > 0) then
        StringList.Add(Source);
    finally
      StringList.EndUpdate;
    end;
  end;
  
begin
  // read Header
  Self.ReadHeader;
  
  OpenSongFile();
  
  ResetLyrics;
  NotesRead := False;
  
  while isDataAvailable do
  begin
    line := GetNextLine();
    
    // end of song
    if (line[1] = 'E') then
      break;
    
    // skip invalid lines
    if (line[1] <> ':') OR (line[1] <> 'F') OR (line[1] <> '*') OR (line[1] <> '-') OR (line[1] <> 'B') then
      continue;
    
    // aktuelle Zeile in einzelne Werte aufteilen
    Values := TStringList.Create;
    try
      ParseDelimited(Values, line, ' ', 5);
    
      try
        if (line[1] = '-') then
          if (not NotesRead) then
            // skip newline if no notes before
            continue
          else
          begin
            // new lyric line
            // param count: 1 (relative: 2)
          
            if (Values.Count > 2) then
              // relativ offset  
            begin
              // P1
              AddLyricLine(0, StrToInt(Values[1]), StrToInt(Values[2]));
              
              // P2
              if Length(Player) = 2 Then
                AddLyricLine(1, StrToInt(Values[1]), StrToInt(Values[2]))
            end
            else
            begin
              AddLyricLine(0, StrToInt(Values[1]));
              
              // P2
              if Length(Player) = 2 then
                AddLyricLine(1, StrToInt(Values[1]))
            end;
          end;
    
        // new BPM set
        if (line[1] = 'B') then
        begin
          // param count: 2
          AddBPM(StrToInt(Values[1]), StrToFloat(Values[2]));
        end;
    
        if (line[1] = ':') OR (line[1] = '*') OR (line[1] = 'F') then
        begin
          // param count: 4
          if (Values.Count < 5) then
            Log.LogError('Error parsing line: ' + line, 'Not enough arguments.')
          else
          begin
            // Check for ZeroNote
            if StrToInt(Values[2]) = 0 then
              Log.LogError('Found ZeroNote: "' + line + '" -> Note ignored!')
            else
            begin
              // P1
              AddNote(0, line[1], StrToInt(Values[1]), StrToInt(Values[2]), StrToInt(Values[3]), Values[4]);
          
              // P2
              if Length(Player) = 2 then
                AddNote(1, line[1], StrToInt(Values[1]), StrToInt(Values[2]), StrToInt(Values[3]), Values[4]);
            end;
          end;
        end;
      except
        on E : Exception do
          Log.LogError('Error parsing line: ' + line, E.ClassName + ': ' + E.Message);
      end;
    finally
      Values.Free();
    end;
  end;
  
  CloseSongFile();
end;

//--------
// Writes complete File (Header + Notes)
//--------
Procedure TSong_Txt.WriteFile;
begin
end;

end.