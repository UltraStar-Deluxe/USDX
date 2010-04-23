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

unit UTextEncoding;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  UUnicodeUtils;

type
  TEncoding = (
    encLocale,  // current locale (needs cwstring on linux)
    encUTF8,    // UTF-8
    encCP1250,  // Windows-1250 Central/Eastern Europe (used by Ultrastar)
    encCP1252,  // Windows-1252 Western Europe (used by UltraStar Deluxe < 1.1)
    encAuto     // try to match the w3c regex and decode as unicode on match
                // and as fallback if not match
  );

const
  UTF8_BOM: UTF8String = #$EF#$BB#$BF;

{**
 * Decodes Src encoded in SrcEncoding to a UTF-16 or UTF-8 encoded Dst string.
 * Returns true if the conversion was successful.
 *}
function DecodeString(const Src: RawByteString; out Dst: WideString; SrcEncoding: TEncoding): boolean; overload;
function DecodeString(const Src: RawByteString; SrcEncoding: TEncoding): WideString; overload;
function DecodeStringUTF8(const Src: RawByteString; out Dst: UTF8String; SrcEncoding: TEncoding): boolean; overload;
function DecodeStringUTF8(const Src: RawByteString; SrcEncoding: TEncoding): UTF8String; overload;

{**
 * Encodes the UTF-16 or UTF-8 encoded Src string to Dst using DstEncoding
 * Returns true if the conversion was successful.
 *}
function EncodeString(const Src: WideString; out Dst: RawByteString; DstEncoding: TEncoding): boolean; overload;
function EncodeString(const Src: WideString; DstEncoding: TEncoding): RawByteString; overload;
function EncodeStringUTF8(const Src: UTF8String; out Dst: RawByteString; DstEncoding: TEncoding): boolean; overload;
function EncodeStringUTF8(const Src: UTF8String; DstEncoding: TEncoding): RawByteString; overload;

{**
 * If Text starts with an UTF-8 BOM, the BOM is removed and true will
 * be returned.
 *}
function CheckReplaceUTF8BOM(var Text: RawByteString): boolean;

{**
 * Parses an encoding string to its TEncoding equivalent.
 * Surrounding whitespace and dashes ('-') are removed, the upper-cased
 * resulting value is then compared with TEncodingNames.
 * If the encoding was not found, the result is set to the Default encoding.
 *}
function ParseEncoding(const EncodingStr: AnsiString; Default: TEncoding): TEncoding;

{**
 * Returns the name of an encoding.
 *}
function EncodingName(Encoding: TEncoding): AnsiString;

implementation

uses
  StrUtils,
  pcre,
  UCommon,
  ULog;

type
  IEncoder = interface
    function GetName(): AnsiString;
    function Encode(const InStr: UCS4String; out OutStr: RawByteString): boolean;
    function Decode(const InStr: RawByteString; out OutStr: UCS4String): boolean;
  end;

  TEncoder = class(TInterfacedObject, IEncoder)
  public
    function GetName(): AnsiString; virtual; abstract;
    function Encode(const InStr: UCS4String; out OutStr: RawByteString): boolean; virtual; abstract;
    function Decode(const InStr: RawByteString; out OutStr: UCS4String): boolean; virtual; abstract;
  end;

  TSingleByteEncoder = class(TEncoder)
  public
    function Encode(const InStr: UCS4String; out OutStr: RawByteString): boolean; override;
    function Decode(const InStr: RawByteString; out OutStr: UCS4String): boolean; override;
    function DecodeChar(InChr: AnsiChar; out OutChr: UCS4Char): boolean; virtual; abstract;
    function EncodeChar(InChr: UCS4Char; out OutChr: AnsiChar): boolean; virtual; abstract;
  end;

const
  ERROR_CHAR = '?';

var
  Encoders: array[TEncoding] of IEncoder;

function TSingleByteEncoder.Encode(const InStr: UCS4String; out OutStr: RawByteString): boolean;
var
  I: integer;
begin
  SetLength(OutStr, LengthUCS4(InStr));
  Result := true;
  for I := 1 to Length(OutStr) do
  begin
    if (not EncodeChar(InStr[I-1], OutStr[I])) then
      Result := false;
  end;
end;

function TSingleByteEncoder.Decode(const InStr: RawByteString; out OutStr: UCS4String): boolean;
var
  I: integer;
begin
  SetLength(OutStr, Length(InStr)+1);
  Result := true;
  for I := 1 to Length(InStr) do
  begin
    if (not DecodeChar(InStr[I], OutStr[I-1])) then
      Result := false;
  end;
  OutStr[High(OutStr)] := 0;
end;

function DecodeString(const Src: RawByteString; out Dst: WideString; SrcEncoding: TEncoding): boolean;
var
  DstUCS4: UCS4String;
begin
  Result := Encoders[SrcEncoding].Decode(Src, DstUCS4);
  Dst := UCS4StringToWideString(DstUCS4);
end;

function DecodeString(const Src: RawByteString; SrcEncoding: TEncoding): WideString;
begin
  DecodeString(Src, Result, SrcEncoding);
end;

function DecodeStringUTF8(const Src: RawByteString; out Dst: UTF8String; SrcEncoding: TEncoding): boolean;
var
  DstUCS4: UCS4String;
begin
  Result := Encoders[SrcEncoding].Decode(Src, DstUCS4);
  Dst := UCS4ToUTF8String(DstUCS4);
end;

function DecodeStringUTF8(const Src: RawByteString; SrcEncoding: TEncoding): UTF8String;
begin
  DecodeStringUTF8(Src, Result, SrcEncoding);
end;

function EncodeString(const Src: WideString; out Dst: RawByteString; DstEncoding: TEncoding): boolean;
begin
  Result := Encoders[DstEncoding].Encode(WideStringToUCS4String(Src), Dst);
end;

function EncodeString(const Src: WideString; DstEncoding: TEncoding): RawByteString;
begin
  EncodeString(Src, Result, DstEncoding);
end;

function EncodeStringUTF8(const Src: UTF8String; out Dst: RawByteString; DstEncoding: TEncoding): boolean;
begin
  Result := Encoders[DstEncoding].Encode(UTF8ToUCS4String(Src), Dst);
end;

function EncodeStringUTF8(const Src: UTF8String; DstEncoding: TEncoding): RawByteString;
begin
  EncodeStringUTF8(Src, Result, DstEncoding);
end;

function CheckReplaceUTF8BOM(var Text: RawByteString): boolean;
begin
  if AnsiStartsStr(UTF8_BOM, Text) then
  begin
    Text := Copy(Text, Length(UTF8_BOM)+1, Length(Text)-Length(UTF8_BOM));
    Result := true;
    Exit;
  end;
  Result := false;
end;

function ParseEncoding(const EncodingStr: AnsiString; Default: TEncoding): TEncoding;
var
  PrepStr: AnsiString; // prepared encoding string
  Encoding: TEncoding;
begin
  // remove surrounding whitespace, replace dashes, to upper case
  PrepStr := UpperCase(AnsiReplaceStr(Trim(EncodingStr), '-', ''));
  for Encoding := Low(TEncoding) to High(TEncoding) do
  begin
    if (Encoders[Encoding].GetName() = PrepStr) then
    begin
      Result := Encoding;
      Exit;
    end;
  end;
  Result := Default;
end;

function EncodingName(Encoding: TEncoding): AnsiString;
begin
  Result := Encoders[Encoding].GetName();
end;

{$I ..\\encoding\\Locale.inc}
{$I ..\\encoding\\UTF8.inc}
{$I ..\\encoding\\CP1250.inc}
{$I ..\\encoding\\CP1252.inc}
{$I ..\\encoding\\Auto.inc}

initialization
  Encoders[encLocale] := TEncoderLocale.Create;
  Encoders[encUTF8]   := TEncoderUTF8.Create;
  Encoders[encCP1250] := TEncoderCP1250.Create;
  Encoders[encCP1252] := TEncoderCP1252.Create;

  // use USDX < 1.1 encoding for backward compatibility (encCP1252)
  Encoders[encAuto] := TEncoderAuto.Create(Encoders[encUTF8], Encoders[encCP1252]);

end.
