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
  NoAvatarTexture: array[1..UIni.IMaxPlayerCount] of TTexture;
  AvatarPlayerTextures: array[1..UIni.IMaxPlayerCount] of TTexture;

implementation

uses
  UFilesystem,
  UPathUtils;

constructor TAvatar.Create(const Filename: IPath);
begin
  Self.Filename := Filename;
end;

function TAvatar.GetTexture(): TTexture;
begin
  Result := Renderer.LoadTexture(Filename);
end;

constructor TAvatarManager.Create();
var
  Len: Integer;
  IterJPG, IterPNG: IFileIterator;
  FileInfo: TFileInfo;
begin
  // first position for no-avatar
  SetLength(AvatarsList, 1);

  // jpg
  IterJPG := FileSystem.FileFind(AvatarsPath.Append('*.jpg'), 0);
  while (IterJPG.HasNext) do
  begin
    Len := Length(AvatarsList);
    SetLength(AvatarsList, Len + 1);

    FileInfo := IterJPG.Next;

    AvatarsList[High(AvatarsList)] := AvatarsPath.Append(FileInfo.Name);
  end;

  // png
  IterPNG := FileSystem.FileFind(AvatarsPath.Append('*.png'), 0);
  while (IterPNG.HasNext) do
  begin
    Len := Length(AvatarsList);
    SetLength(AvatarsList, Len + 1);

    FileInfo := IterPNG.Next;

    AvatarsList[High(AvatarsList)] := AvatarsPath.Append(FileInfo.Name);
  end;
end;

function TAvatarManager.AddAvatar(const Filename: IPath): TAvatar;
begin
  Result := TAvatar.Create(Filename);
end;

end.
