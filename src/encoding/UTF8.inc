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

{*
 * UTF-8
 *}

type
  TEncoderUTF8 = class(TEncoder)
  public
    function GetName(): AnsiString; override;
    function Encode(const InStr: UCS4String; out OutStr: AnsiString): boolean; override;
    function Decode(const InStr: AnsiString; out OutStr: UCS4String): boolean; override;
  end;

function TEncoderUTF8.GetName(): AnsiString;
begin
  Result := 'UTF8';
end;

function TEncoderUTF8.Decode(const InStr: AnsiString; out OutStr: UCS4String): boolean;
var
  I: integer;
  StrPtr: PAnsiChar;
begin
  // UTF8Decode() may crash with FPC < 2.2.2 if the input string is not UTF-8
  // encoded. Newer versions do not crash but do not signal errors either.
  // So let's implement this stuff again.
  Result := true;
  SetLength(OutStr, Length(InStr)+1);
  I := 0;
  StrPtr := PChar(InStr);
  while (StrPtr^ <> #0) do
  begin
    if (not NextCharUTF8(StrPtr, OutStr[I])) then
      Result := false;;
    Inc(I);
  end;
  SetLength(OutStr, I+1);
  OutStr[High(OutStr)] := 0;
end;

function TEncoderUTF8.Encode(const InStr: UCS4String; out OutStr: AnsiString): boolean;
begin
  OutStr := UCS4ToUTF8String(InStr);
  Result := true;
end;

