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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/menu/UMenuText.pas $
 * $Id: UMenuText.pas 1485 2008-10-28 20:16:05Z tobigun $
 *}

unit UTextEncoding;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils;

type
  TEncoding = (encCP1250, encCP1252, encUTF8, encNative);

function RecodeString(const Src: string; SrcEncoding: TEncoding): WideString;

implementation

type
  TConversionTable = array[0..127] of WideChar;

const
  // Windows-1250 Central/Eastern Europe (used by Ultrastar)
  CP1250Table: TConversionTable = (
    { $80 }
    #$20AC,     #0, #$201A,     #0, #$201E, #$2026, #$2020, #$2021,
        #0, #$2030, #$0160, #$2039, #$015A, #$0164, #$017D, #$0179,
    { $90 }
        #0, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
        #0, #$2122, #$0161, #$203A, #$015B, #$0165, #$017E, #$017A,
    { $A0 }
    #$00A0, #$02C7, #$02D8, #$0141, #$00A4, #$0104, #$00A6, #$00A7,
    #$00A8, #$00A9, #$015E, #$00AB, #$00AC, #$00AD, #$00AE, #$017B,
    { $B0 }
    #$00B0, #$00B1, #$02DB, #$0142, #$00B4, #$00B5, #$00B6, #$00B7,
    #$00B8, #$0105, #$015F, #$00BB, #$013D, #$02DD, #$013E, #$017C,
    { $C0 }
    #$0154, #$00C1, #$00C2, #$0102, #$00C4, #$0139, #$0106, #$00C7,
    #$010C, #$00C9, #$0118, #$00CB, #$011A, #$00CD, #$00CE, #$010E,
    { $D0 }
    #$0110, #$0143, #$0147, #$00D3, #$00D4, #$0150, #$00D6, #$00D7,
    #$0158, #$016E, #$00DA, #$0170, #$00DC, #$00DD, #$0162, #$00DF,
    { $E0 }
    #$0155, #$00E1, #$00E2, #$0103, #$00E4, #$013A, #$0107, #$00E7,
    #$010D, #$00E9, #$0119, #$00EB, #$011B, #$00ED, #$00EE, #$010F,
    { $F0 }
    #$0111, #$0144, #$0148, #$00F3, #$00F4, #$0151, #$00F6, #$00F7,
    #$0159, #$016F, #$00FA, #$0171, #$00FC, #$00FD, #$0163, #$02D9
  );

  // Windows-1252 Western Europe (used by UltraStar Deluxe < 1.1)
  CP1252Table: TConversionTable = (
    { $80 }
    #$20AC,     #0, #$201A, #$0192, #$201E, #$2026, #$2020, #$2021,
    #$02C6, #$2030, #$0160, #$2039, #$0152,     #0, #$017D,     #0,
    { $90 }
        #0, #$2018, #$2019, #$201C, #$201D, #$2022, #$2013, #$2014,
    #$02DC, #$2122, #$0161, #$203A, #$0153,     #0, #$017E, #$0178,
    { $A0 }
    #$00A0, #$00A1, #$00A2, #$00A3, #$00A4, #$00A5, #$00A6, #$00A7,
    #$00A8, #$00A9, #$00AA, #$00AB, #$00AC, #$00AD, #$00AE, #$00AF,
    { $B0 }
    #$00B0, #$00B1, #$00B2, #$00B3, #$00B4, #$00B5, #$00B6, #$00B7,
    #$00B8, #$00B9, #$00BA, #$00BB, #$00BC, #$00BD, #$00BE, #$00BF,
    { $C0 }
    #$00C0, #$00C1, #$00C2, #$00C3, #$00C4, #$00C5, #$00C6, #$00C7,
    #$00C8, #$00C9, #$00CA, #$00CB, #$00CC, #$00CD, #$00CE, #$00CF,
    { $D0 }
    #$00D0, #$00D1, #$00D2, #$00D3, #$00D4, #$00D5, #$00D6, #$00D7,
    #$00D8, #$00D9, #$00DA, #$00DB, #$00DC, #$00DD, #$00DE, #$00DF,
    { $E0 }
    #$00E0, #$00E1, #$00E2, #$00E3, #$00E4, #$00E5, #$00E6, #$00E7,
    #$00E8, #$00E9, #$00EA, #$00EB, #$00EC, #$00ED, #$00EE, #$00EF,
    { $F0 }
    #$00F0, #$00F1, #$00F2, #$00F3, #$00F4, #$00F5, #$00F6, #$00F7,
    #$00F8, #$00F9, #$00FA, #$00FB, #$00FC, #$00FD, #$00FE, #$00FF
  );


function Convert(const Src: string; const Table: TConversionTable): WideString;
var
  SrcPos, DstPos: integer;
begin
  SetLength(Result, Length(Src));
  DstPos := 1;
  for SrcPos := 1 to Length(Src) do
  begin
    if (Src[SrcPos] < #128) then
    begin
      // copy ASCII char
      Result[DstPos] := Src[SrcPos];
      Inc(DstPos);
    end
    else
    begin
      // look-up char
      Result[DstPos] := Table[Ord(Src[SrcPos]) - 128];
      // ignore invalid characters
      if (Result[DstPos] <> #0) then
        Inc(DstPos);
    end;
  end;
  SetLength(Result, DstPos-1);
end;

function RecodeString(const Src: string; SrcEncoding: TEncoding): WideString;
begin
  case SrcEncoding of
    encCP1250:
      Result := Convert(Src, CP1250Table);
    encCP1252:
      Result := Convert(Src, CP1252Table);
    encUTF8:
      Result := UTF8Decode(Src);
    encNative:
      Result := UTF8Decode(AnsiToUtf8(Src));
  end;
end;

end.
