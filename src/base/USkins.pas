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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/base/USkins.pas $
 * $Id: USkins.pas 3137 2015-09-14 00:11:19Z basisbit $
 *}

unit USkins;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UPath,
  UCommon;

type
  TSkinTexture = record
    Name:     string;
    FileName: IPath;
    Path:     IPath;
  end;

  TSkinEntry = record
    Theme:    string;
    Name:     string;
    Path:     IPath;
    FileName: IPath;

    DefaultColor: integer;
    Creator:  string; // not used yet
  end;

  TSkin = class
    Skin:        array of TSkinEntry;
    SkinTexture: array of TSkinTexture;
    SkinPath:    IPath;
    Color:       integer;
    procedure LoadDefaultSkinForTheme(const ThemeName: string; Depth: integer);
    procedure LoadSkinTextures(const SkinEntry: TSkinEntry; UseAsBasePath: boolean);
    procedure SetTextureEntry(const TextureName: string; const FileName: IPath; const TexturePath: IPath);
    constructor Create;
    procedure LoadList;
    procedure ParseDir(Dir: IPath);
    procedure LoadHeader(FileName: IPath);
    procedure LoadSkin(Name, Theme: string);
    function GetTextureFileName(TextureName: string): IPath;
    function GetSkinNumber(Name, Theme: string): integer;
    function GetDefaultColor(SkinNo: integer): integer;

    procedure GetSkinsByTheme(Theme: string; out Skins: TUTF8StringDynArray);

    procedure onThemeChange;
  end;

var
  Skin: TSkin;

implementation

uses
  IniFiles,
  Classes,
  SysUtils,
  Math,
  UIni,
  ULog,
  UMain,
  UThemes,
  UPathUtils,
  UFileSystem;

const
  MAX_SKIN_INHERITANCE = 10;

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
  Skin[S].DefaultColor := Max(0, GetArrayIndex(IColor, SkinIni.ReadString('Skin', 'Color', ''), true));

  SkinIni.Free;
end;

procedure TSkin.SetTextureEntry(const TextureName: string; const FileName: IPath; const TexturePath: IPath);
var
  T: integer;
  CleanTextureName: string;
begin
  CleanTextureName := Trim(TextureName);

  for T := 0 to High(SkinTexture) do
  begin
    if CompareText(SkinTexture[T].Name, CleanTextureName) = 0 then
    begin
      if FileName.IsSet and (not TexturePath.Append(FileName).IsFile) then
        Exit;
      SkinTexture[T].FileName := FileName;
      SkinTexture[T].Path := TexturePath;
      Exit;
    end;
  end;

  T := Length(SkinTexture);
  SetLength(SkinTexture, T + 1);
  SkinTexture[T].Name := CleanTextureName;
  SkinTexture[T].FileName := FileName;
  SkinTexture[T].Path := TexturePath;
end;

procedure TSkin.LoadSkinTextures(const SkinEntry: TSkinEntry; UseAsBasePath: boolean);
var
  SkinIni: TMemIniFile;
  SL: TStringList;
  T: integer;
begin
  if UseAsBasePath then
    SkinPath := SkinEntry.Path;

  SkinIni := TMemIniFile.Create(SkinEntry.Path.Append(SkinEntry.FileName).ToNative);
  SL := TStringList.Create;
  SkinIni.ReadSection('Textures', SL);

  for T := 0 to SL.Count - 1 do
    SetTextureEntry(SL.Strings[T], Path(SkinIni.ReadString('Textures', SL.Strings[T], '')), SkinEntry.Path);

  SL.Free;
  SkinIni.Free;
end;

procedure TSkin.LoadDefaultSkinForTheme(const ThemeName: string; Depth: integer);
var
  ThemeIndex: integer;
  DefaultSkinIndex: integer;
  I: integer;
  ThemeSkins: TUTF8StringDynArray;
begin
  if (ThemeName = '') or (Depth >= MAX_SKIN_INHERITANCE) then
    Exit;

  ThemeIndex := -1;
  for I := Low(UThemes.Theme.Themes) to High(UThemes.Theme.Themes) do
  begin
    if CompareText(UThemes.Theme.Themes[I].Name, ThemeName) = 0 then
    begin
      ThemeIndex := I;
      Break;
    end;
  end;

  if (ThemeIndex >= 0) and (UThemes.Theme.Themes[ThemeIndex].DefaultSkin >= 0) and
     (UThemes.Theme.Themes[ThemeIndex].DefaultSkin < Length(ISkin)) then
  begin
    LoadDefaultSkinForTheme(UThemes.Theme.Themes[ThemeIndex].BaseTheme, Depth + 1);

    GetSkinsByTheme(ThemeName, ThemeSkins);
    if UThemes.Theme.Themes[ThemeIndex].DefaultSkin < Length(ThemeSkins) then
    begin
      DefaultSkinIndex := GetSkinNumber(ThemeSkins[UThemes.Theme.Themes[ThemeIndex].DefaultSkin], ThemeName);
      LoadSkinTextures(Skin[DefaultSkinIndex], Length(SkinTexture) = 0);
    end;
  end;
end;

procedure TSkin.LoadSkin(Name, Theme: string);
var
  SelectedSkinIndex: integer;
begin
  SelectedSkinIndex := GetSkinNumber(Name, Theme);
  SetLength(SkinTexture, 0);

  LoadDefaultSkinForTheme(Theme, 0);

  if Length(SkinTexture) = 0 then
    SkinPath := Skin[SelectedSkinIndex].Path;

  LoadSkinTextures(Skin[SelectedSkinIndex], Length(SkinTexture) = 0);
end;

function TSkin.GetTextureFileName(TextureName: string): IPath;
var
  T: integer;
begin
  Result := PATH_NONE;
  
  for T := 0 to High(SkinTexture) do
  begin
    if (CompareText(SkinTexture[T].Name, Trim(TextureName)) = 0) and
       (SkinTexture[T].FileName.IsSet) then
    begin
      Result := SkinTexture[T].Path.Append(SkinTexture[T].FileName);
    end;
  end;

  if (TextureName <> '') and (Result.IsSet) then
  begin
    //Log.LogError('', '-----------------------------------------');
    //Log.LogError('Was not able to retrieve Texture for ' + TextureName + ' - ' + Result.ToNative, 'TSkin.GetTextureFileName');
  end;
end;

function TSkin.GetSkinNumber(Name, Theme: string): integer;
var
  S: integer;
begin
  Result := 0; // set default to the first available skin
  for S := 0 to High(Skin) do
    if (CompareText(Skin[S].Name, Name) = 0) and (CompareText(Skin[S].Theme, Theme) = 0) then
      Result := S;
end;

procedure TSkin.GetSkinsByTheme(Theme: string; out Skins: TUTF8StringDynArray);
  var
    I: Integer;
    Len: integer;
begin
  SetLength(Skins, 0);
  Len := 0;

  for I := 0 to High(Skin) do
    if CompareText(Theme, Skin[I].Theme) = 0 then
    begin
      SetLength(Skins, Len + 1);
      Skins[Len] := Skin[I].Name;
      Inc(Len);
    end;
end;

{ returns number of default color for skin with
  index SkinNo in ISkin (not in the actual skin array) }
function TSkin.GetDefaultColor(SkinNo: integer): integer;
  var
    I: Integer;
begin
  Result := 0;

  for I := 0 to High(Skin) do
    if CompareText(ITheme[Ini.Theme], Skin[I].Theme) = 0 then
    begin
      if SkinNo > 0 then
        Dec(SkinNo)
      else
      begin
        Result := Skin[I].DefaultColor;
        Break;
      end;
    end;
end;

procedure TSkin.onThemeChange;
begin
  Ini.SkinNo:=0;
  GetSkinsByTheme(ITheme[Ini.Theme], ISkin);
end;

end.
