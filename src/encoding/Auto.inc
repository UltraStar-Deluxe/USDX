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

// Auto
// try to match the w3c regex and decode as unicode on match and as fallback if not match
// (copied from http://www.w3.org/International/questions/qa-forms-utf-8.en.php)
//
// m/\A(
//    [\x09\x0A\x0D\x20-\x7E]            # ASCII
//  | [\xC2-\xDF][\x80-\xBF]             # non-overlong 2-byte
//  |  \xE0[\xA0-\xBF][\x80-\xBF]        # excluding overlongs
//  | [\xE1-\xEC\xEE\xEF][\x80-\xBF]{2}  # straight 3-byte
//  |  \xED[\x80-\x9F][\x80-\xBF]        # excluding surrogates
//  |  \xF0[\x90-\xBF][\x80-\xBF]{2}     # planes 1-3
//  | [\xF1-\xF3][\x80-\xBF]{3}          # planes 4-15
//  |  \xF4[\x80-\x8F][\x80-\xBF]{2}     # plane 16
// )*\z/x

type
  TEncoderAuto = class(TEncoder)
  public
    function GetName(): AnsiString; override;
    function Encode(const InStr: UCS4String; out OutStr: AnsiString): boolean; override;
    function Decode(const InStr: AnsiString; out OutStr: UCS4String): boolean; override;

    constructor Create(const UTF8Encoder, FallbackEncoder: IEncoder);

  private
    FallbackEncoder: IEncoder;
    UTF8Encoder: IEncoder;
  end;

function IsUtf8(const str: AnsiString): boolean;
var
  i, len: LongInt;
  c, state: Integer;
begin
  len := Length(str);
  state := 0;
  for i := 1 to len do
  begin
    c := $100 * state + Ord(str[i]);
    case c of
    $09,$0A,$0D,$20..$7E: state := 0; // [\x09\x0A\x0D\x20-\x7E]
    $C2..$DF:             state := 1; // [\xC2-\xDF][\x80-\xBF]
    $E0:                  state := 2; // \xE0[\xA0-\xBF][\x80-\xBF]
    $E1..$EC,$EE,$EF:     state := 3; // [\xE1-\xEC\xEE\xEF][\x80-\xBF]{2}
    $ED:                  state := 4; // \xED[\x80-\x9F][\x80-\xBF]
    $F0:                  state := 5; // \xF0[\x90-\xBF][\x80-\xBF]{2}
    $F1..$F3:             state := 6; // [\xF1-\xF3][\x80-\xBF]{3}
    $F4:                  state := 7; // \xF4[\x80-\x8F][\x80-\xBF]{2}
    $180..$1BF:           state := 0; // [\x80-\xBF]
    $2A0..$2BF:           state := 1; // [\xA0-\xBF][\x80-\xBF]
    $380..$3BF:           state := 1; // [\x80-\xBF]{2}
    $480..$49F:           state := 1; // [\x80-\x9F][\x80-\xBF]
    $590..$5BF:           state := 3; // [\x90-\xBF][\x80-\xBF]{2}
    $680..$6BF:           state := 3; // [\x80-\xBF]{3}
    $780..$78F:           state := 3; // [\x80-\x8F][\x80-\xBF]{2}
    else begin
        Result := False;
        exit;
      end;
    end;
  end;
  Result := (state = 0);
end;

constructor TEncoderAuto.Create(const UTF8Encoder, FallbackEncoder: IEncoder);
begin
  inherited Create();
  self.FallbackEncoder := FallbackEncoder;
  self.UTF8Encoder := UTF8Encoder;
end;

function TEncoderAuto.GetName(): AnsiString;
begin
  Result := 'Auto';
end;

function TEncoderAuto.Decode(const InStr: AnsiString; out OutStr: UCS4String): boolean;
begin
  if IsUtf8(InStr) then
    Result := UTF8Encoder.Decode(InStr, OutStr)
  else
    Result := FallbackEncoder.Decode(InStr, OutStr);
end;

function TEncoderAuto.Encode(const InStr: UCS4String; out OutStr: AnsiString): boolean;
begin
  Result := UTF8Encoder.Encode(InStr, OutStr);
end;
