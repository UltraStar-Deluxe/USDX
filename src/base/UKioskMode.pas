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
 *}

unit UKioskMode;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Math,
  SysUtils;

type
  TKioskMode = class
    private
      FActive: boolean;
      FAgeLimit: integer;
      FPasswordHash: UTF8String;
      class function NormalizePassword(const Password: UTF8String): UTF8String; static;
    public
      constructor Create;
      class function HashPassword(const Password: UTF8String): UTF8String; static;
      procedure Enable(AgeLimit: integer; const Password: UTF8String);
      procedure SetPasswordHash(const Hash: UTF8String);
      procedure ActivateWithHash(AgeLimit: integer);
      procedure Restore(AgeLimit: integer; const PasswordHash: UTF8String; Active: boolean);
      procedure Disable;
      function HasPassword: boolean;
      function CheckPassword(const Candidate: UTF8String): boolean;
      function Unlock(const Candidate: UTF8String): boolean;
      function AllowsAge(SongAge: integer): boolean;
      property Active: boolean read FActive;
      property AgeLimit: integer read FAgeLimit;
      property PasswordHash: UTF8String read FPasswordHash;
  end;

var
  KioskMode: TKioskMode;
const
  KioskAgeNoLimit = -1;

implementation

uses
  md5,
  UUnicodeUtils;

{ TKioskMode }

class function TKioskMode.NormalizePassword(const Password: UTF8String): UTF8String;
var
  WidePassword: UnicodeString;
begin
  WidePassword := UTF8Decode(Password);
  WidePassword := Trim(WidePassword);
  Result := UTF8Encode(WidePassword);
end;

class function TKioskMode.HashPassword(const Password: UTF8String): UTF8String;
var
  Digest: TMD5Digest;
begin
  Digest := MD5String(AnsiString(UTF8UpperCase(NormalizePassword(Password))));
  Result := UTF8String(MD5Print(Digest));
end;

constructor TKioskMode.Create;
begin
  inherited Create;
  FActive := false;
  FAgeLimit := KioskAgeNoLimit;
  FPasswordHash := '';
end;

procedure TKioskMode.Disable;
begin
  FActive := false;
end;

procedure TKioskMode.Enable(AgeLimit: integer; const Password: UTF8String);
begin
  FAgeLimit := Max(KioskAgeNoLimit, AgeLimit);
  if Password <> '' then
    SetPasswordHash(HashPassword(Password))
  else
    FPasswordHash := '';
  FActive := HasPassword;
end;

procedure TKioskMode.SetPasswordHash(const Hash: UTF8String);
begin
  FPasswordHash := Hash;
  if FPasswordHash = '' then
    FActive := false;
end;

procedure TKioskMode.ActivateWithHash(AgeLimit: integer);
begin
  FAgeLimit := Max(KioskAgeNoLimit, AgeLimit);
  if not HasPassword then
    raise Exception.Create('Kiosk password missing');
  FActive := true;
end;

procedure TKioskMode.Restore(AgeLimit: integer; const PasswordHash: UTF8String; Active: boolean);
begin
  FAgeLimit := Max(KioskAgeNoLimit, AgeLimit);
  FPasswordHash := PasswordHash;
  FActive := Active and HasPassword;
end;

function TKioskMode.HasPassword: boolean;
begin
  Result := FPasswordHash <> '';
end;

function TKioskMode.CheckPassword(const Candidate: UTF8String): boolean;
begin
  if not HasPassword then
    Exit(true);
  Result := HashPassword(Candidate) = FPasswordHash;
end;

function TKioskMode.Unlock(const Candidate: UTF8String): boolean;
begin
  Result := FActive and CheckPassword(Candidate);
  if Result then
    Disable;
end;

function TKioskMode.AllowsAge(SongAge: integer): boolean;
begin
  if (not FActive) or (FAgeLimit <= KioskAgeNoLimit) or (SongAge <= 0) then
    Exit(true);
  Result := SongAge <= FAgeLimit;
end;

initialization
  KioskMode := TKioskMode.Create;

finalization
  FreeAndNil(KioskMode);

end.
