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
    Regex: PPCRE;
    RegexExtra: PPCREExtra;
  end;

function PCREGetMem(Size: SizeInt): Pointer; cdecl;
begin
  GetMem(Result, Size);
end;

procedure PCREFreeMem(P: Pointer); cdecl;
begin
  FreeMem(P);
end;

// NOTICE: Log.LogError/ConsoleWriteLn/DebugWriteLn are initialized yet
procedure ShowError(const msg: string);
begin
  {$IFDEF CONSOLE}
  WriteLn('ERROR: ', msg);
  {$ENDIF}
end;

constructor TEncoderAuto.Create(const UTF8Encoder, FallbackEncoder: IEncoder);
var
  Error: PChar;
  ErrorOffset: Integer;
begin
  inherited Create();
  self.FallbackEncoder := FallbackEncoder;
  self.UTF8Encoder := UTF8Encoder;

  // Load and initialize PCRE Library
  if LoadPCRE() then
  begin
    // compile regex
    self.Regex := pcre_compile('\A([\x09\x0A\x0D\x20-\x7E]|[\xC2-\xDF][\x80-\xBF]|\xE0[\xA0-\xBF][\x80-\xBF]|[\xE1-\xEC\xEE\xEF][\x80-\xBF]{2}|\xED[\x80-\x9F][\x80-\xBF]|\xF0[\x90-\xBF][\x80-\xBF]{2}|[\xF1-\xF3][\x80-\xBF]{3}|\xF4[\x80-\x8F][\x80-\xBF]{2})*\z', 0, @Error, @ErrorOffset, nil);

    if self.Regex = Nil then
    begin
      ShowError(Format('UTF8 Regex compilation failed: %s at %d', [Error, ErrorOffset]));
    end
    else
    begin
      // if compiled successfull, try to get more informations the speed up the matching
      self.RegexExtra := pcre_study(self.Regex, 0, @Error);

      if Error <> Nil then
      begin
        ShowError('UTF8 Regex study failed: ' + Error);
      end;
    end;
  end
  else
  begin
    ShowError('pcre not loaded. utf-8 autodetection will not work.');
  end;
end;

function TEncoderAuto.GetName(): AnsiString;
begin
  Result := 'Auto';
end;

function TEncoderAuto.Decode(const InStr: AnsiString; out OutStr: UCS4String): boolean;
var
  RegexResults: Integer;
begin
  if (self.Regex <> Nil) then
  begin
    RegexResults := pcre_exec(Regex, RegexExtra, PChar(InStr), Length(InStr), 0, 0, Nil, 0);

    if RegexResults >= 0 then
    begin
      Result := UTF8Encoder.Decode(InStr, OutStr);
      Exit;
    end;
  end;

  Result := FallbackEncoder.Decode(InStr, OutStr);
end;

function TEncoderAuto.Encode(const InStr: UCS4String; out OutStr: AnsiString): boolean;
begin
  Result := UTF8Encoder.Encode(InStr, OutStr);
end;
