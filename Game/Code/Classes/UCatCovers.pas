unit UCatCovers;
/////////////////////////////////////////////////////////////////////////
//                   UCatCovers by Whiteshark                          //
//          Class for listing and managing the Category Covers         //
/////////////////////////////////////////////////////////////////////////

interface

{$I switches.inc}

uses UIni;

type
  TCatCovers = class
    protected
      cNames:    array [low(ISorting)..high(ISorting)] of array of string;
      cFiles:    array [low(ISorting)..high(ISorting)] of array of string;
    public
      constructor Create;
      procedure Load; //Load Cover aus Cover.ini and Cover Folder
      procedure Add(Sorting: integer; Name, Filename: string); //Add a Cover
      function  CoverExists(Sorting: integer; Name: string): boolean; //Returns True when a cover with the given Name exists
      function  GetCover(Sorting: integer; Name: string): string; //Returns the Filename of a Cover
  end;

var
CatCovers: TCatCovers;

implementation
uses IniFiles,
     SysUtils,
     Classes,
     // UFiles,
     UMain,
     ULog;

constructor TCatCovers.Create;
begin
  Load;
end;

  //Load Cover aus Cover.ini and Cover Folder
procedure TCatCovers.Load;
var
  Ini: TMemIniFile;
  SR:  TSearchRec;
  List: TStringlist;
  I, J: Integer;
  Name, Filename, Temp: string;
begin
try
  Ini  := TMemIniFile.Create(CoversPath + 'covers.ini');
  List := TStringlist.Create;

  //Add every Cover in Covers Ini for Every Sorting option
  for I := low(ISorting) to high(ISorting) do
  begin
    Ini.ReadSection(ISorting[I], List);

    for J := 0 to List.Count - 1 do
      Add(I, List.Strings[J], CoversPath + Ini.ReadString(ISorting[I], List.Strings[J], 'NoCover.jpg'));
  end;

finally
  Ini.Free;
  List.Free;
end;

try
  //Add Covers from Folder
  if (FindFirst (CoversPath + '*.jpg', faAnyFile, SR) = 0) then
    repeat
      //Add Cover if it doesn't exist for every Section
      Name := SR.Name;
      Filename := CoversPath + Name;
      Delete (Name, length(Name) - 3, 4);

      for I := low(ISorting) to high(ISorting) do
      begin
        Temp := Name;
        if ((I = sTitle) or (I = sTitle2)) and (Pos ('Title', Temp) <> 0) then
          Delete (Temp, Pos ('Title', Temp), 5)
        else if (I = sArtist) or (I = sArtist2) and (Pos ('Artist', Temp) <> 0) then
          Delete (Temp, Pos ('Artist', Temp), 6);

        if not CoverExists(I, Temp) then
          Add (I, Temp, Filename);
      end;
    until FindNext (SR) <> 0;

finally
  FindClose (SR);
end;

end;

  //Add a Cover
procedure TCatCovers.Add(Sorting: integer; Name, Filename: string);
begin
if FileExists (Filename) then //If Exists -> Add
begin
SetLength (CNames[Sorting], Length(CNames[Sorting]) + 1);
SetLength (CFiles[Sorting], Length(CNames[Sorting]) + 1);

CNames[Sorting][high(cNames[Sorting])] := Uppercase(Name);
CFiles[Sorting][high(cNames[Sorting])] := FileName;
end;
end;

  //Returns True when a cover with the given Name exists
function TCatCovers.CoverExists(Sorting: integer; Name: string): boolean;
var
I: Integer;
begin
Result := False;
Name := Uppercase(Name); //Case Insensitiv

for I := low(cNames[Sorting]) to high(cNames[Sorting]) do
begin
  if (cNames[Sorting][I] = Name) then //Found Name
  begin
    Result := true;
    break; //Break For Loop
  end;
end;
end;

  //Returns the Filename of a Cover
function TCatCovers.GetCover(Sorting: integer; Name: string): string;
var
I: Integer;
begin
Result := '';
Name := Uppercase(Name);

for I := low(cNames[Sorting]) to high(cNames[Sorting]) do
begin
  if cNames[Sorting][I] = Name then
  begin
    Result := cFiles[Sorting][I];
    Break;
  end;
end;

//No Cover
if (Result = '') AND (FileExists(CoversPath + 'NoCover.jpg')) then
  Result := CoversPath + 'NoCover.jpg';

end;

end.
