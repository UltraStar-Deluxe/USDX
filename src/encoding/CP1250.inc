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
 * Windows-1250 Central/Eastern Europe
 * (used by Ultrastar)
 *}

type
  TEncoderCP1250 = class(TSingleByteEncoder)
  public
    function GetName(): AnsiString; override;
    function DecodeChar(InChr: AnsiChar; out OutChr: UCS4Char): boolean; override;
    function EncodeChar(InChr: UCS4Char; out OutChr: AnsiChar): boolean; override;
  end;

function TEncoderCP1250.GetName(): AnsiString;
begin
  Result := 'CP1250';
end;

const
  // Positions marked as #0 are invalid.
  CP1250Table: array[128..255] of UCS4Char = (
    { $80 }
    $20AC,     0, $201A,     0, $201E, $2026, $2020, $2021,
        0, $2030, $0160, $2039, $015A, $0164, $017D, $0179,
    { $90 }
        0, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
        0, $2122, $0161, $203A, $015B, $0165, $017E, $017A,
    { $A0 }
    $00A0, $02C7, $02D8, $0141, $00A4, $0104, $00A6, $00A7,
    $00A8, $00A9, $015E, $00AB, $00AC, $00AD, $00AE, $017B,
    { $B0 }
    $00B0, $00B1, $02DB, $0142, $00B4, $00B5, $00B6, $00B7,
    $00B8, $0105, $015F, $00BB, $013D, $02DD, $013E, $017C,
    { $C0 }
    $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7,
    $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
    { $D0 }
    $0110, $0143, $0147, $00D3, $00D4, $0150, $00D6, $00D7,
    $0158, $016E, $00DA, $0170, $00DC, $00DD, $0162, $00DF,
    { $E0 }
    $0155, $00E1, $00E2, $0103, $00E4, $013A, $0107, $00E7,
    $010D, $00E9, $0119, $00EB, $011B, $00ED, $00EE, $010F,
    { $F0 }
    $0111, $0144, $0148, $00F3, $00F4, $0151, $00F6, $00F7,
    $0159, $016F, $00FA, $0171, $00FC, $00FD, $0163, $02D9
  );

function TEncoderCP1250.DecodeChar(InChr: AnsiChar; out OutChr: UCS4Char): boolean;
begin
  Result := true;
  if (InChr < #128) then
    OutChr := UCS4Char(Ord(InChr)) // use Ord() to avoid automatic conversion
  else
  begin
    OutChr := CP1250Table[Ord(InChr)];
    if (OutChr = 0) then
    begin
      Result := false;
      OutChr := Ord(ERROR_CHAR);
    end;
  end;
end;

function TEncoderCP1250.EncodeChar(InChr: UCS4Char; out OutChr: AnsiChar): boolean;
begin
  if (InChr < 128) then
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
        // invalid: #131
        $201E: OutChr := #132;
        $2026: OutChr := #133;
        $2020: OutChr := #134;
        $2021: OutChr := #135;
        // invalid: #136
        $2030: OutChr := #137;
        $0160: OutChr := #138;
        $2039: OutChr := #139;
        $015A: OutChr := #140;
        $0164: OutChr := #141;
        $017D: OutChr := #142;
        $0179: OutChr := #143;
        // invalid: #144
        $2018: OutChr := #145;
        $2019: OutChr := #146;
        $201C: OutChr := #147;
        $201D: OutChr := #148;
        $2022: OutChr := #149;
        $2013: OutChr := #150;
        $2014: OutChr := #151;
        // invalid: #152
        $2122: OutChr := #153;
        $0161: OutChr := #154;
        $203A: OutChr := #155;
        $015B: OutChr := #156;
        $0165: OutChr := #157;
        $017E: OutChr := #158;
        $017A: OutChr := #159;
        $00A0: OutChr := #160;
        $02C7: OutChr := #161;
        $02D8: OutChr := #162;
        $0141: OutChr := #163;
        $00A4: OutChr := #164;
        $0104: OutChr := #165;
        $00A6: OutChr := #166;
        $00A7: OutChr := #167;
        $00A8: OutChr := #168;
        $00A9: OutChr := #169;
        $015E: OutChr := #170;
        $00AB: OutChr := #171;
        $00AC: OutChr := #172;
        $00AD: OutChr := #173;
        $00AE: OutChr := #174;
        $017B: OutChr := #175;
        $00B0: OutChr := #176;
        $00B1: OutChr := #177;
        $02DB: OutChr := #178;
        $0142: OutChr := #179;
        $00B4: OutChr := #180;
        $00B5: OutChr := #181;
        $00B6: OutChr := #182;
        $00B7: OutChr := #183;
        $00B8: OutChr := #184;
        $0105: OutChr := #185;
        $015F: OutChr := #186;
        $00BB: OutChr := #187;
        $013D: OutChr := #188;
        $02DD: OutChr := #189;
        $013E: OutChr := #190;
        $017C: OutChr := #191;
        $0154: OutChr := #192;
        $00C1: OutChr := #193;
        $00C2: OutChr := #194;
        $0102: OutChr := #195;
        $00C4: OutChr := #196;
        $0139: OutChr := #197;
        $0106: OutChr := #198;
        $00C7: OutChr := #199;
        $010C: OutChr := #200;
        $00C9: OutChr := #201;
        $0118: OutChr := #202;
        $00CB: OutChr := #203;
        $011A: OutChr := #204;
        $00CD: OutChr := #205;
        $00CE: OutChr := #206;
        $010E: OutChr := #207;
        $0110: OutChr := #208;
        $0143: OutChr := #209;
        $0147: OutChr := #210;
        $00D3: OutChr := #211;
        $00D4: OutChr := #212;
        $0150: OutChr := #213;
        $00D6: OutChr := #214;
        $00D7: OutChr := #215;
        $0158: OutChr := #216;
        $016E: OutChr := #217;
        $00DA: OutChr := #218;
        $0170: OutChr := #219;
        $00DC: OutChr := #220;
        $00DD: OutChr := #221;
        $0162: OutChr := #222;
        $00DF: OutChr := #223;
        $0155: OutChr := #224;
        $00E1: OutChr := #225;
        $00E2: OutChr := #226;
        $0103: OutChr := #227;
        $00E4: OutChr := #228;
        $013A: OutChr := #229;
        $0107: OutChr := #230;
        $00E7: OutChr := #231;
        $010D: OutChr := #232;
        $00E9: OutChr := #233;
        $0119: OutChr := #234;
        $00EB: OutChr := #235;
        $011B: OutChr := #236;
        $00ED: OutChr := #237;
        $00EE: OutChr := #238;
        $010F: OutChr := #239;
        $0111: OutChr := #240;
        $0144: OutChr := #241;
        $0148: OutChr := #242;
        $00F3: OutChr := #243;
        $00F4: OutChr := #244;
        $0151: OutChr := #245;
        $00F6: OutChr := #246;
        $00F7: OutChr := #247;
        $0159: OutChr := #248;
        $016F: OutChr := #249;
        $00FA: OutChr := #250;
        $0171: OutChr := #251;
        $00FC: OutChr := #252;
        $00FD: OutChr := #253;
        $0163: OutChr := #254;
        $02D9: OutChr := #255;
      else begin
        OutChr := ERROR_CHAR;
        Result := false;
        Exit;
      end;
    end;
    Result := true;
  end;
end;

