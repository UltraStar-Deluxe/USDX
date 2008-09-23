{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *}

unit UCatCovers;
/////////////////////////////////////////////////////////////////////////
//                   UCatCovers by Whiteshark                          //
//          Class for listing and managing the Category Covers         //
/////////////////////////////////////////////////////////////////////////

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UIni;

type
  TCatCovers = class
    protected
      cNames:    array [0..high(ISorting)] of array of string;
      cFiles:    array [0..high(ISorting)] of array of string;
    public
      constructor Create;
      procedure Load; //Load Cover aus Cover.ini and Cover Folder
      procedure LoadPath(const CoversPath: string);
      procedure Add(Sorting: integer; Name, Filename: string); //Add a Cover
      function  CoverExists(Sorting: integer; Name: string): boolean; //Returns True when a cover with the given Name exists
      function  GetCover(Sorting: integer; Name: string): string; //Returns the Filename of a Cover
  end;

var
  CatCovers: TCatCovers;

implementation

uses
  IniFiles,
  SysUtils,
  Classes,
  // UFiles,
  UMain,
  ULog;

constructor TCatCovers.Create;
begin
  inherited;
  Load;
end;

procedure TCatCovers.Load;
var
  I: integer;
begin
  for I := 0 to CoverPaths.Count-1 do
    LoadPath(CoverPaths[I]);
end;

(**
 * Load Cover from Cover.ini and Cover Folder
 *)
procedure TCatCovers.LoadPath(const CoversPath: string);
var
  Ini: TMemIniFile;
  SR:  TSearchRec;
  List: TStringlist;
  I, J: Integer;
  Name, Filename, Temp: string;
begin
  Ini := nil;
  List := nil;

  try
    Ini  := TMemIniFile.Create(CoversPath + 'covers.ini');
    List := TStringlist.Create;

    //Add every Cover in Covers Ini for Every Sorting option
    for I := 0 to High(ISorting) do
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

        for I := 0 to high(ISorting) do
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

  for I := 0 to high(cNames[Sorting]) do
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

  for I := 0 to high(cNames[Sorting]) do
  begin
    if cNames[Sorting][I] = Name then
    begin
      Result := cFiles[Sorting][I];
      Break;
    end;
  end;

  //No Cover
  if (Result = '') then
  begin
    for I := 0 to CoverPaths.Count-1 do
    begin
      if (FileExists(CoverPaths[I] + 'NoCover.jpg')) then
      begin
        Result := CoverPaths[I] + 'NoCover.jpg';
        Break;
      end;
    end;
  end;
end;

end.
