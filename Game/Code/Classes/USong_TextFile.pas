unit USong_TextFile;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  SysUtils,
  USong;

type
  {*******************
    Child of the new TSong class.
    implements filehandling to load a song from a text file
   *******************}
  TSong_TextFile = class(TSong)
    protected
      SongFile: TextFile;
      
      Function OpenSongFile: Boolean;
      Function IsDataAvailable: Boolean;
      Function GetNextLine(): String;
      Procedure CloseSongFile;
  end;
  
implementation

uses
  ULog;

//--------
// Open the SongFile
//--------
Function TSong_TextFile.OpenSongFile: Boolean;
begin
  Result := False;
  
  if not FileExists(FilePath + FileName) then
    Log.LogError('File does not exsist', FilePath + FileName)
  else
  begin
    try
      AssignFile(SongFile, FilePath + FileName);
      Reset(SongFile);
      Result := True;
    except
      Log.LogError('Faild to open file', FilePath + FileName)
    end;
  end;
end;

//--------
// More data in songfile available?
//--------
Function TSong_TextFile.IsDataAvailable: Boolean;
begin
  Result := not eof(SongFile);
end;

//--------
// Returns the next line from the SongFile
//--------
Function TSong_TextFile.GetNextLine(): String;
begin
  ReadLn(SongFile, Result);
  Result := Trim(Result);
end;

//--------
// Close the SongFile
//--------
Procedure TSong_TextFile.CloseSongFile;
begin
  try
    CloseFile(SongFile);
  except
    Log.LogError('Error closing file', FilePath + FileName);
  end;
end;

end.