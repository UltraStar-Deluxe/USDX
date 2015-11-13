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
 * Windows-1252 Western Europe
 * (used by UltraStar Deluxe < 1.1)
 *}

type
  TEncoderCP1252 = class(TSingleByteEncoder)
  public
    function GetName(): AnsiString; override;
    function DecodeChar(InChr: AnsiChar; out OutChr: UCS4Char): boolean; override;
    function EncodeChar(InChr: UCS4Char; out OutChr: AnsiChar): boolean; override;
  end;

function TEncoderCP1252.GetName(): AnsiString;
begin
  Result := 'CP1252';
end;

const
  // Positions marked as #0 are invalid.
  CP1252Table: array[128..159] of UCS4Char = (
    { $80 }
    $20AC,     0, $201A, $0192, $201E, $2026, $2020, $2021,
    $02C6, $2030, $0160, $2039, $0152,     0, $017D,     0,
    { $90 }
        0, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $02DC, $2122, $0161, $203A, $0153,     0, $017E, $0178
  );

function TEncoderCP1252.DecodeChar(InChr: AnsiChar; out OutChr: UCS4Char): boolean;
begin
  Result := true;
  if (InChr < #128) or (InChr >= #160) then
    OutChr := UCS4Char(Ord(InChr)) // use Ord() to avoid automatic conversion
  else
  begin
    OutChr := CP1252Table[Ord(InChr)];
    if (OutChr = 0) then
    begin
      Result := false;
      OutChr := Ord(ERROR_CHAR);
    end;
  end;
end;

function TEncoderCP1252.EncodeChar(InChr: UCS4Char; out OutChr: AnsiChar): boolean;
begin
  if (InChr < 128) or ((InChr >= 160) and (InChr <= 255)) then
  begin
    OutChr := AnsiChar(Ord(InChr));
    Result := true;
  end
  else
  begin
    case InChr of
      $20AC: OutChr := #128;
      // invalid: #129
      $201A: OutChr := #130;
      $0192: OutChr := #131;
      $201E: OutChr := #132;
      $2026: OutChr := #133;
      $2020: OutChr := #134;
      $2021: OutChr := #135;
      $02C6: OutChr := #136;
      $2030: OutChr := #137;
      $0160: OutChr := #138;
      $2039: OutChr := #139;
      $0152: OutChr := #140;
      // invalid: #141
      $017D: OutChr := #142;
      // invalid: #143
      // invalid: #144
      $2018: OutChr := #145;
      $2019: OutChr := #146;
      $201C: OutChr := #147;
      $201D: OutChr := #148;
      $2022: OutChr := #149;
      $2013: OutChr := #150;
      $2014: OutChr := #151;
      $02DC: OutChr := #152;
      $2122: OutChr := #153;
      $0161: OutChr := #154;
      $203A: OutChr := #155;
      $0153: OutChr := #156;
      // invalid: #157
      $017E: OutChr := #158;
      $0178: OutChr := #159;
      else begin
        OutChr := ERROR_CHAR;
        Result := false;
        Exit;
      end;
    end;
    Result := true;
  end;
end;

