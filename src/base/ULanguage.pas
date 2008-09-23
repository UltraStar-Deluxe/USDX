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

unit ULanguage;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

type
  TLanguageEntry = record
    ID:     string;
    Text:   string;
  end;

  TLanguageList = record
    Name:     string;
    {FileName: string; }
  end;

  TLanguage = class
    public
      Entry:  array of TLanguageEntry; //Entrys of Chosen Language
      SEntry: array of TLanguageEntry; //Entrys of Standard Language
      CEntry: array of TLanguageEntry; //Constant Entrys e.g. Version
      Implode_Glue1, Implode_Glue2: String;
    public
      List:   array of TLanguageList;

      constructor Create;
      procedure LoadList;
      function Translate(Text: String): String;
      procedure ChangeLanguage(Language: String);
      procedure AddConst(ID, Text: String);
      procedure ChangeConst(ID, Text: String);
      function Implode(Pieces: Array of String): String;
  end;

var
  Language:     TLanguage;

implementation

uses
  UMain,
  // UFiles,
  UIni,
  IniFiles,
  Classes,
  SysUtils,
  {$IFDEF win32}
    windows,
  {$ENDIF}
  ULog;

//----------
//Create - Construct Class then LoadList + Standard Language + Set Standard Implode Glues
//----------
constructor TLanguage.Create;
var
  I, J: Integer;
begin
  inherited;

  LoadList;

  //Set Implode Glues for Backward Compatibility
  Implode_Glue1 := ', ';
  Implode_Glue2 := ' and ';

  if (Length(List) = 0) then //No Language Files Loaded -> Abort Loading
    Log.CriticalError('Could not load any Language File');

  //Standard Language (If a Language File is Incomplete)
  //Then use English Language
  for I := 0 to high(List) do //Search for English Language
  begin
    //English Language Found -> Load
    if Uppercase(List[I].Name) = 'ENGLISH' then
    begin
      ChangeLanguage('English');

      SetLength(SEntry, Length(Entry));
      for J := low(Entry) to high(Entry) do
        SEntry[J] := Entry[J];

      SetLength(Entry, 0);
      
      Break;
    end;

    if (I = high(List)) then
      Log.LogError('English Languagefile missing! No standard Translation loaded');
  end;
  //Standard Language END
  
end;

//----------
//LoadList - Parse the Language Dir searching Translations
//----------
procedure TLanguage.LoadList;
var
  SR:     TSearchRec;   // for parsing directory
begin
  SetLength(List, 0);
  SetLength(ILanguage, 0);

  if FindFirst(LanguagesPath + '*.ini', 0, SR) = 0 then begin
    repeat
      SetLength(List, Length(List)+1);
      SetLength(ILanguage, Length(ILanguage)+1);
      SR.Name := ChangeFileExt(SR.Name, '');

      List[High(List)].Name := SR.Name;
      ILanguage[High(ILanguage)] := SR.Name;

    until FindNext(SR) <> 0;
  SysUtils.FindClose(SR);
  end; // if FindFirst
end;

//----------
//ChangeLanguage - Load the specified LanguageFile
//----------
procedure TLanguage.ChangeLanguage(Language: String);
var
  IniFile:    TIniFile;
  E:          integer; // entry
  S:          TStringList;
begin
  SetLength(Entry, 0);
  IniFile := TIniFile.Create(LanguagesPath + Language + '.ini');
  S := TStringList.Create;

  IniFile.ReadSectionValues('Text', S);
  SetLength(Entry, S.Count);
  for E := 0 to high(Entry) do
  begin
    if S.Names[E] = 'IMPLODE_GLUE1' then
      Implode_Glue1 := S.ValueFromIndex[E]+ ' '
    else if S.Names[E] = 'IMPLODE_GLUE2' then
      Implode_Glue2 := ' ' + S.ValueFromIndex[E] + ' ';

    Entry[E].ID := S.Names[E];
    Entry[E].Text := S.ValueFromIndex[E];
  end;

  S.Free;
  IniFile.Free;
end;

//----------
//Translate - Translate the Text
//----------
Function TLanguage.Translate(Text: String): String;
var
  E:    integer; // entry
begin
  Result := Text;
  Text := Uppercase(Result);

  //Const Mod
  for E := 0 to high(CEntry) do
    if Text = CEntry[E].ID then
    begin
     Result := CEntry[E].Text;
     exit;
    end;
  //Const Mod End

  for E := 0 to high(Entry) do
    if Text = Entry[E].ID then
    begin
     Result := Entry[E].Text;
     exit;
    end;

  //Standard Language (If a Language File is Incomplete)
  //Then use Standard Language
    for E := low(SEntry) to high(SEntry) do
      if Text = SEntry[E].ID then
      begin
        Result := SEntry[E].Text;
        Break;
      end;
  //Standard Language END
end;

//----------
//AddConst - Add a Constant ID that will be Translated but not Loaded from the LanguageFile
//----------
procedure TLanguage.AddConst (ID, Text: String);
begin
  SetLength (CEntry, Length(CEntry) + 1);
  CEntry[high(CEntry)].ID := ID;
  CEntry[high(CEntry)].Text := Text;
end;

//----------
//ChangeConst - Change a Constant Value by ID
//----------
procedure TLanguage.ChangeConst(ID, Text: String);
var
  I: Integer;
begin
  for I := 0 to high(CEntry) do
  begin
    if CEntry[I].ID = ID then
    begin
     CEntry[I].Text := Text;
     Break;
    end;
  end;
end;

//----------
//Implode - Connect an Array of Strings with ' and ' or ', ' to one String
//----------
function TLanguage.Implode(Pieces: Array of String): String;
var
  I: Integer;
begin
  Result := '';
  //Go through Pieces
  for I := low(Pieces) to high(Pieces) do
  begin
    //Add Value
    Result := Result + Pieces[I];

    //Add Glue
    if (I < high(Pieces) - 1) then
      Result := Result + Implode_Glue1
    else if (I < high(Pieces)) then
      Result := Result + Implode_Glue2;
  end;
end;

end.
