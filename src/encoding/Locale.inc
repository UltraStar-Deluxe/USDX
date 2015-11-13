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
 * Locale
 *}

type
  TEncoderLocale = class(TEncoder)
  public
    function GetName(): AnsiString; override;
    function Encode(const InStr: UCS4String; out OutStr: AnsiString): boolean; override;
    function Decode(const InStr: AnsiString; out OutStr: UCS4String): boolean; override;
  end;

function TEncoderLocale.GetName(): AnsiString;
begin
  Result := 'LOCALE';
end;

function TEncoderLocale.Decode(const InStr: AnsiString; out OutStr: UCS4String): boolean;
begin
  OutStr := WideStringToUCS4String(InStr); // use implicit conversion
  Result := true;
end;

function TEncoderLocale.Encode(const InStr: UCS4String; out OutStr: AnsiString): boolean;
begin
  OutStr := UCS4StringToWideString(InStr); // use implicit conversion
  // any way to check for errors?
  Result := true;
end;

