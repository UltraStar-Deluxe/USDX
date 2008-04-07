program ResourceExtractor;

{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

uses
  Classes,
  SysUtils,
  StrUtils;
  

var
  ResCount: integer;
  OutStream: TStringList;


procedure Init();
begin
  OutStream := TStringList.Create();

  OutStream.Add('const');
  // placeholder for array-header (will be filled on file-saving)
  OutStream.Add('');
end;

procedure SaveToFile(const OutFileName: string);
begin
  // add array-header
  OutStream[1] := '  resources: array[0..'+IntToStr(ResCount-1)+', 0..2] of string = (';
  // add trailer
  OutStream.Add('  );');

  // save file
  try
    OutStream.SaveToFile(OutFileName);
  except
    OutStream.Free();
    raise Exception.Create('Could not save to file: "' + OutFileName + '"');
  end;

  OutStream.Free();
end;

function AddResource(Fields: TStringList; const RCFileDir, ResDir: string): boolean;
var
  ResName, ResType, ResFile: string;
begin
  if (Fields.Count < 3) or
     (AnsiStartsStr('//', Fields[0])) or
     (Length(Fields[2]) < 3) then
  begin
    Result := false;
    Exit;
  end;

  // add a trailing comma to the last line
  if (ResCount > 0) then
    OutStream[OutStream.Count-1] := OutStream[OutStream.Count-1] + ',';

  ResName := Fields[0];
  ResType := Fields[1];
  ResFile := Fields[2];
  
  Writeln('ADD:  [' + ResType + ':' + ResName + ' = ' +ResFile + ']');

  // quote fields
  ResName := AnsiQuotedStr(ResName, '''')+',';
  ResType := AnsiQuotedStr(ResType, '''')+',';
  // strip surrounding quotes of filename
  ResFile := AnsiMidStr(ResFile, 2, Length(Fields[2])-2);
  // now translate the resource filename (relative to the RC-file) to be relative to the resource-dir
  // 1st step: get absolute path of the resource
  ResFile := ExpandFileName(RCFileDir + ResFile);
  // 2nd step: get path of the resource relative to the resource-dir
  // Note: both paths must be absolute and the base-path must have a trailing '/' or '\'
  ResFile := ExtractRelativepath(ResDir, ResFile);
  // 3rd step: ... and quote
  ResFile := AnsiQuotedStr(ResFile, '''');

  // compose line
  OutStream.Add(Format('    (%-20S%-8S%S)', [ResName, ResType, ResFile]));

  Inc(ResCount);
  
  Result := true;
end;

procedure ExtractResources(const InFileName, ResDir: string);
var
  Fields: TStringList;
  LineIndex: integer;
  Lines: TStringList;
  RCFileDirAbs, ResDirAbs: string;
begin
  // get absolute paths
  RCFileDirAbs := ExtractFilePath(ExpandFileName(InFileName));
  ResDirAbs := ExpandFileName(ResDir) + '/';

  Lines := TStringList.Create();
  try
    Lines.LoadFromFile(InFileName);
  except
    raise Exception.Create('Failed to open file: "' + InFileName + '"');
  end;

  Fields := TStringList.Create();
  for LineIndex := 0 to Lines.Count-1 do
  begin
    Fields.Clear();
    // split line into [Name, Type, File]
    ExtractStrings([' ', #9], [], PChar(Lines[LineIndex]), Fields);
    if (not AddResource(Fields, RCFileDirAbs, ResDirAbs)) then
      Writeln( 'SKIP: "'+Lines[LineIndex]+'"');
  end;

  Lines.Free();
  Fields.Free();
end;

var
  ProgName: string;
begin
  if (ParamCount <> 3) then
  begin
    ProgName := ExtractFileName(ParamStr(0));
    WriteLn('Usage: ' + ProgName + ' RC-File Resource-Dir Output-File');
    Exit;
  end;

  WriteLn('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
  WriteLn('Converting "' + ParamStr(1) + '" to "' + ParamStr(3) + '"');
  WriteLn('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');

  try
    Init();
    ExtractResources(ParamStr(1), ParamStr(2));
    SaveToFile(ParamStr(3));
  except on E:Exception do
  begin
    WriteLn('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
    WriteLn('Conversion failed: ' + E.Message + '!');
    WriteLn('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
    Exit;
  end;
  end;

  WriteLn('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<');
  WriteLn('Conversion finished!');
  WriteLn('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<');
end.

