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
 * $URL: $
 * $Id:  $
 *}

unit UAvatars;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UIni,
  URenderer,
  UPath;

type
  TAvatar = class
    private
      Filename: IPath;
    public
      constructor Create(const Filename: IPath);
      function GetTexture(): TTexture;
  end;

  TAvatarManager = class
    public
      constructor Create();
      function AddAvatar(const Filename: IPath): TAvatar;
  end;

var
  Avatars: TAvatarManager;
  AvatarsList: array of IPath;
  AvatarsMD5: array of UTF8String;
  NoAvatarTexture: array[1..UIni.IMaxPlayerCount] of TTexture;
  AvatarPlayerTextures: array[1..UIni.IMaxPlayerCount] of TTexture;

implementation

uses
  md5,
  sysutils,
  UFilesystem,
  UPathUtils;

function AvatarExists(Hash: string): boolean;
var
  I: integer;
begin
  Result := false;
  for I := Low(AvatarsMD5) to High(AvatarsMD5) do
  begin
    if (AvatarsMD5[I] = Hash) then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

constructor TAvatar.Create(const Filename: IPath);
begin
  Self.Filename := Filename;
end;

function TAvatar.GetTexture(): TTexture;
begin
  Result := Renderer.LoadTexture(Filename);
end;

constructor TAvatarManager.Create();
const
  Extensions: array[0..3] of string = ('.jpg', '.jpeg', '.png', '.webp');
var
  I, J, Len: Integer;
  Iter: IFileIterator;
  FileInfo: TFileInfo;
  AvatarPath: IPath;
  Hash: string;
begin
  // first position for no-avatar
  SetLength(AvatarsList, 1);

  // Find avatars
  for I := 0 to AvatarsPaths.Count - 1 do
  begin
    for J := Low(Extensions) to High(Extensions) do
    begin
      Iter := FileSystem.FileFind(IPath(AvatarsPaths[I]).Append('*' + Extensions[J]), 0);
      while (Iter.HasNext) do
      begin
        FileInfo := Iter.Next;
        AvatarPath := IPath(AvatarsPaths[I]).Append(FileInfo.Name);
        Hash := UpperCase(MD5Print(MD5File(AvatarPath.ToNative)));
        if (not AvatarExists(Hash)) then
        begin
          Len := Length(AvatarsList);
          SetLength(AvatarsList, Len + 1);
          SetLength(AvatarsMD5, Len + 1);
          AvatarsList[Len] := AvatarPath;
          AvatarsMD5[Len] := Hash;
        end;
      end;
    end;
  end;
end;

function TAvatarManager.AddAvatar(const Filename: IPath): TAvatar;
begin
  Result := TAvatar.Create(Filename);
end;

end.
