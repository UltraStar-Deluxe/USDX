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
  private
    FallbackSkinPath: IPath;
    function FindTextureIndex(const TextureName: string): integer;
    function FindSkinIndex(const Name, Theme: string): integer;
    procedure LoadSkinTextures(const SkinIndex: integer; AppendMissingOnly: boolean);
  public
    Skin:        array of TSkinEntry;
    SkinTexture: array of TSkinTexture;
    SkinPath:    IPath;
    Color:       integer;
    constructor Create;
    procedure LoadList;
    procedure ParseDir(Dir: IPath);
    procedure LoadHeader(FileName: IPath);
    procedure LoadSkin(Name, Theme: string; const FallbackName: string = ''; const FallbackTheme: string = '');
    function GetTextureFileName(TextureName: string): IPath;
    function GetSkinFile(const RelativeFileName: IPath): IPath;
    function GetSkinNumber(Name, Theme: string): integer;
    function HasSkin(Name, Theme: string): boolean;
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
  Skin[S].DefaultColor := Max(0, GetArrayIndex(IColor, SkinIni.ReadString('Skin', 'Color', ''), true));

  SkinIni.Free;
end;

function TSkin.FindTextureIndex(const TextureName: string): integer;
var
  T: integer;
begin
  Result := -1;

  for T := 0 to High(SkinTexture) do
  begin
    if (SkinTexture[T].Name = TextureName) then
    begin
      Result := T;
      Break;
    end;
  end;
end;

function TSkin.FindSkinIndex(const Name, Theme: string): integer;
var
  S: integer;
begin
  Result := -1;

  for S := 0 to High(Skin) do
  begin
    if (CompareText(Skin[S].Name, Name) = 0) and
       (CompareText(Skin[S].Theme, Theme) = 0) then
    begin
      Result := S;
      Break;
    end;
  end;
end;

procedure TSkin.LoadSkinTextures(const SkinIndex: integer; AppendMissingOnly: boolean);
var
  SkinIni: TMemIniFile;
  SL:      TStringList;
  T:       integer;
  TextureIndex: integer;
  TextureFile: IPath;
begin
  if (SkinIndex < Low(Skin)) or (SkinIndex > High(Skin)) then
    Exit;

  SkinIni := TMemIniFile.Create(Skin[SkinIndex].Path.Append(Skin[SkinIndex].FileName).ToNative);

  SL := TStringList.Create;
  SkinIni.ReadSection('Textures', SL);

  for T := 0 to SL.Count-1 do
  begin
    TextureIndex := FindTextureIndex(SL.Strings[T]);
    TextureFile := Path(SkinIni.ReadString('Textures', SL.Strings[T], ''));
    if TextureFile.IsSet then
      TextureFile := Skin[SkinIndex].Path.Append(TextureFile);

    if TextureIndex = -1 then
    begin
      TextureIndex := Length(SkinTexture);
      SetLength(SkinTexture, TextureIndex + 1);
      SkinTexture[TextureIndex].Name := SL.Strings[T];
    end
    else if AppendMissingOnly and
            SkinTexture[TextureIndex].FileName.IsSet and
            SkinTexture[TextureIndex].FileName.IsFile then
    begin
      Continue;
    end;

    SkinTexture[TextureIndex].FileName := TextureFile;
  end;

  SL.Free;
  SkinIni.Free;
end;

procedure TSkin.LoadSkin(Name, Theme: string; const FallbackName: string = ''; const FallbackTheme: string = '');
var
  SkinIndex: integer;
begin
  SetLength(SkinTexture, 0);
  SkinPath := PATH_NONE;
  FallbackSkinPath := PATH_NONE;

  SkinIndex := FindSkinIndex(Name, Theme);
  if (SkinIndex = -1) then
  begin
    Log.LogError('Could not find skin "' + Name + '" for theme "' + Theme + '"', 'TSkin.LoadSkin');
    Exit;
  end;

  SkinPath := Skin[SkinIndex].Path;
  LoadSkinTextures(SkinIndex, false);

  if (FallbackName = '') or (FallbackTheme = '') then
    Exit;

  SkinIndex := FindSkinIndex(FallbackName, FallbackTheme);
  if (SkinIndex = -1) then
    Exit;

  FallbackSkinPath := Skin[SkinIndex].Path;
  if not FallbackSkinPath.Equals(SkinPath) then
    LoadSkinTextures(SkinIndex, true);
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
      Result := SkinTexture[T].FileName;
      Break;
    end;
  end;

  if (TextureName <> '') and (Result.IsSet) then
  begin
    //Log.LogError('', '-----------------------------------------');
    //Log.LogError('Was not able to retrieve Texture for ' + TextureName + ' - ' + Result.ToNative, 'TSkin.GetTextureFileName');
  end;
end;

function TSkin.GetSkinFile(const RelativeFileName: IPath): IPath;
begin
  Result := PATH_NONE;

  if (RelativeFileName = nil) or RelativeFileName.IsUnset then
    Exit;

  if SkinPath.IsSet then
  begin
    Result := SkinPath.Append(RelativeFileName);
    if Result.IsFile then
      Exit;
  end;

  if FallbackSkinPath.IsSet then
  begin
    Result := FallbackSkinPath.Append(RelativeFileName);
    if Result.IsFile then
      Exit;
  end;

  Result := PATH_NONE;
end;

function TSkin.GetSkinNumber(Name, Theme: string): integer;
begin
  Result := FindSkinIndex(Name, Theme);
  if Result < 0 then
    Result := 0; // set default to the first available skin
end;

function TSkin.HasSkin(Name, Theme: string): boolean;
begin
  Result := (FindSkinIndex(Name, Theme) <> -1);
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
