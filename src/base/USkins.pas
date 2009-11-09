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

unit USkins;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UPath;

type
  TSkinTexture = record
    Name:     string;
    FileName: IPath;
  end;

  TSkinEntry = record
    Theme:    string;
    Name:     string;
    Path:     IPath;
    FileName: IPath;
    Creator:  string; // not used yet
  end;

  TSkin = class
    Skin:        array of TSkinEntry;
    SkinTexture: array of TSkinTexture;
    SkinPath:    IPath;
    Color:       integer;
    constructor Create;
    procedure LoadList;
    procedure ParseDir(Dir: IPath);
    procedure LoadHeader(FileName: IPath);
    procedure LoadSkin(Name: string);
    function GetTextureFileName(TextureName: string): IPath;
    function GetSkinNumber(Name: string): integer;
    procedure onThemeChange;
  end;

var
  Skin: TSkin;

implementation

uses
  IniFiles,
  Classes,
  SysUtils,
  UIni,
  ULog,
  UMain,
  UPathUtils,
  UFileSystem;

constructor TSkin.Create;
begin
  inherited;
  LoadList;
//  LoadSkin('...');
//  SkinColor := Color;
end;

procedure TSkin.LoadList;
var
  Iter: IFileIterator;
  DirInfo: TFileInfo;
begin
  Iter := FileSystem.FileFind(SkinsPath.Append('*'), faDirectory);
  while Iter.HasNext do
  begin
    DirInfo := Iter.Next();
    if (not DirInfo.Name.Equals('.')) and (not DirInfo.Name.Equals('..')) then
      ParseDir(SkinsPath.Append(DirInfo.Name, pdAppend));
  end;
end;

procedure TSkin.ParseDir(Dir: IPath);
var
  Iter: IFileIterator;
  IniInfo: TFileInfo;
begin
  Iter := FileSystem.FileFind(Dir.Append('*.ini'), 0);
  while Iter.HasNext do
  begin
    IniInfo := Iter.Next;
    LoadHeader(Dir.Append(IniInfo.Name));
  end;
end;

procedure TSkin.LoadHeader(FileName: IPath);
var
  SkinIni: TMemIniFile;
  S:       integer;
begin
  SkinIni := TMemIniFile.Create(FileName.ToNative);

  S := Length(Skin);
  SetLength(Skin, S+1);
  
  Skin[S].Path     := FileName.GetPath;
  Skin[S].FileName := FileName.GetName;
  Skin[S].Theme    := SkinIni.ReadString('Skin', 'Theme', '');
  Skin[S].Name     := SkinIni.ReadString('Skin', 'Name', '');
  Skin[S].Creator  := SkinIni.ReadString('Skin', 'Creator', '');

  SkinIni.Free;
end;

procedure TSkin.LoadSkin(Name: string);
var
  SkinIni: TMemIniFile;
  SL:      TStringList;
  T:       integer;
  S:       integer;
begin
  S        := GetSkinNumber(Name);
  SkinPath := Skin[S].Path;

  SkinIni  := TMemIniFile.Create(SkinPath.Append(Skin[S].FileName).ToNative);

  SL := TStringList.Create;
  SkinIni.ReadSection('Textures', SL);

  SetLength(SkinTexture, SL.Count);
  for T := 0 to SL.Count-1 do
  begin
    SkinTexture[T].Name     := SL.Strings[T];
    SkinTexture[T].FileName := Path(SkinIni.ReadString('Textures', SL.Strings[T], ''));
  end;

  SL.Free;
  SkinIni.Free;
end;

function TSkin.GetTextureFileName(TextureName: string): IPath;
var
  T: integer;
begin
  Result := PATH_NONE;
  
  for T := 0 to High(SkinTexture) do
  begin
    if (SkinTexture[T].Name = TextureName) and
       (SkinTexture[T].FileName.IsSet) then
    begin
      Result := SkinPath.Append(SkinTexture[T].FileName);
    end;
  end;

  if (TextureName <> '') and (Result.IsSet) then
  begin
    //Log.LogError('', '-----------------------------------------');
    //Log.LogError(TextureName+' - '+ Result, 'TSkin.GetTextureFileName');
  end;

{  Result := SkinPath + 'Bar.jpg';
  if TextureName = 'Ball' then
    Result := SkinPath + 'Ball.bmp';
  if Copy(TextureName, 1, 4) = 'Gray' then
    Result := SkinPath + 'Ball.bmp';
  if Copy(TextureName, 1, 6) = 'NoteBG' then
    Result := SkinPath + 'Ball.bmp';}
end;

function TSkin.GetSkinNumber(Name: string): integer;
var
  S: integer;
begin
  Result := 0; // set default to the first available skin
  for S := 0 to High(Skin) do
    if Skin[S].Name = Name then
      Result := S;
end;

procedure TSkin.onThemeChange;
var
  S:    integer;
  Name: String;
begin
  Ini.SkinNo:=0;
  SetLength(ISkin, 0);
  Name := Uppercase(ITheme[Ini.Theme]);
  for S := 0 to High(Skin) do
    if Name = Uppercase(Skin[S].Theme) then
    begin
      SetLength(ISkin, Length(ISkin)+1);
      ISkin[High(ISkin)] := Skin[S].Name;
    end;

end;

end.
