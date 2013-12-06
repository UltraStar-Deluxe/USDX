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

unit UUnicodeUtils;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  StrUtils,
  SysUtils;

type
  // String with unknown encoding. Introduced with Delphi 2009 and maybe soon
  // with FPC.
  RawByteString = AnsiString;

{**
 * Returns true if the system uses UTF-8 as default string type
 * (filesystem or API calls).
 * This is always true on Mac OS X and always false on Win32. On Unix it depends
 * on the LC_CTYPE setting.
 * Do not use AnsiToUTF8() or UTF8ToAnsi() if this function returns true.
 *}
function IsNativeUTF8(): boolean;

(*
 * Character classes
 *)

function IsAlphaChar(ch: WideChar): boolean; overload;
function IsAlphaChar(ch: UCS4Char): boolean; overload;

function IsNumericChar(ch: WideChar): boolean; overload;
function IsNumericChar(ch: UCS4Char): boolean; overload;

function IsAlphaNumericChar(ch: WideChar): boolean; overload;
function IsAlphaNumericChar(ch: UCS4Char): boolean; overload;

function IsPunctuationChar(ch: WideChar): boolean; overload;
function IsPunctuationChar(ch: UCS4Char): boolean; overload;

function IsControlChar(ch: WideChar): boolean; overload;
function IsControlChar(ch: UCS4Char): boolean; overload;

function IsPrintableChar(ch: WideChar): boolean; overload;
function IsPrintableChar(ch: UCS4Char): boolean; overload;

{**
 * Checks if the given string is a valid UTF-8 string.
 * If an ANSI encoded string (with char codes >= 128) is passed, the
 * function will most probably return false, as most ANSI strings sequences
 * are illegal in UTF-8.
 *}
function IsUTF8String(const str: RawByteString): boolean;

{**
 * Iterates over an UTF-8 encoded string.
 * StrPtr will be  increased to the beginning of the next character on each
 * call.
 * Results true if the given string starts with an UTF-8 encoded char.
 *}
function NextCharUTF8(var StrPtr: PAnsiChar; out Ch: UCS4Char): boolean;

{**
 * Deletes Count chars (not bytes) beginning at char- (not byte-) position Index.
 * Index values start with 1.
 *}
procedure UTF8Delete(var Str: UTF8String; Index: Integer; Count: Integer);
procedure UCS4Delete(var Str: UCS4String; Index: Integer; Count: Integer);

{**
 * Checks if the string is composed of ASCII characters.
 *}
function IsASCIIString(const str: RawByteString): boolean;

{*
 * String format conversion
 *}

function UTF8ToUCS4String(const str: UTF8String): UCS4String;
function UCS4ToUTF8String(const str: UCS4String): UTF8String; overload;
function UCS4ToUTF8String(ch: UCS4Char): UTF8String; overload;

{**
 * Returns the number of characters (not bytes) in string str.
 *}
function LengthUTF8(const str: UTF8String): integer;

{**
 * Returns the length of an UCS4String. Note that Length(UCS4String) returns
 * the length+1 as UCS4Strings are zero-terminated.
 *}
function LengthUCS4(const str: UCS4String): integer;

{** @seealso WideCompareStr *}
function UTF8CompareStr(const S1, S2: UTF8String): integer;
{** @seealso WideCompareText *}
function UTF8CompareText(const S1, S2: UTF8String): integer;

function UTF8StartsText(const SubText, Text: UTF8String): boolean;

function UTF8ContainsStr(const Text, SubText: UTF8String): boolean;
function UTF8ContainsText(const Text, SubText: UTF8String): boolean;

{** @seealso WideUpperCase *}
function UTF8UpperCase(const str: UTF8String): UTF8String;
{** @seealso WideCompareText *}
function UTF8LowerCase(const str: UTF8String): UTF8String;

{**
 * Converts a UCS-4 char ch to its upper-case representation.
 *}
function UCS4UpperCase(ch: UCS4Char): UCS4Char; overload;

{**
 * Converts a UCS-4 string str to its upper-case representation.
 *}
function UCS4UpperCase(const str: UCS4String): UCS4String; overload;

{**
 * Converts a UCS4Char to an UCS4String.
 * Note that UCS4Strings are zero-terminated dynamic arrays.
 *}
function UCS4CharToString(ch: UCS4Char): UCS4String;

{**
 * @seealso System.Pos()
 *}
function UTF8Pos(const substr: UTF8String; const str: UTF8String): Integer;

{**
 * Copies a segment of str starting with Index (1-based) with Count characters (not bytes).
 *}
function UTF8Copy(const str: UTF8String; Index: Integer = 1; Count: Integer = -1): UTF8String;

{**
 * Copies a segment of str starting with Index (0-based) with Count characters.
 * Note: Do not use Copy() to copy UCS4Strings as the result will not contain
 * a trailing #0 character and hence is invalid.
 *}
function UCS4Copy(const str: UCS4String; Index: Integer = 0; Count: Integer = -1): UCS4String;

(*
 * Converts a WideString to its upper- or lower-case representation.
 * Wrapper for WideUpper/LowerCase. Needed because some plattforms have
 * problems with unicode support.
 *
 * Note that characters in UTF-16 might consist of one or two WideChar valus
 * (see surrogates). So instead of using WideStringUpperCase(ch)[1] for single
 * character access, convert to UCS-4 where each character is represented by
 * one UCS4Char. 
 *)
function WideStringUpperCase(const str: WideString) : WideString; overload;
function WideStringUpperCase(ch: WideChar): WideString; overload;
function WideStringLowerCase(const str: WideString): WideString; overload;
function WideStringLowerCase(ch: WideChar): WideString; overload;

function WideStringReplaceChar(const text: WideString; search, rep: WideChar): WideString;

implementation

{$IFDEF UNIX}
{$IFNDEF DARWIN}
const
  LC_CTYPE = 0;

function setlocale(category: integer; locale: PChar): PChar; cdecl; external 'c';
{$ENDIF}
{$ENDIF}

var
  NativeUTF8: boolean;

procedure InitUnicodeUtils();
{$IFDEF UNIX}
{$IFNDEF DARWIN}
var
  localeName: PChar;
{$ENDIF}
{$ENDIF}
begin
  {$IF Defined(DARWIN)}
    NativeUTF8 := true;
  {$ELSEIF Defined(MSWindows)}
    NativeUTF8 := false;
  {$ELSEIF Defined(UNIX)}
    // check if locale name contains UTF8 or UTF-8
    localeName := setlocale(LC_CTYPE, nil);
    NativeUTF8 := Pos('UTF8', UpperCase(AnsiReplaceStr(localeName, '-', ''))) > 0;
  {$ELSE}
    raise Exception.Create('Unknown system');
  {$IFEND}
end;

function IsNativeUTF8(): boolean;
begin
  Result := NativeUTF8;
end;

function IsAlphaChar(ch: WideChar): boolean;
begin
  {$IFDEF MSWINDOWS}
    Result := IsCharAlphaW(ch);
  {$ELSE}
    // TODO: add chars > 255 (or replace with libxml2 functions?)
    case ch of
      'A'..'Z',  // A-Z
      'a'..'z',  // a-z
      #170,#181,#186,
      #192..#214,
      #216..#246,
      #248..#255:
        Result := true;
      else
        Result := false;
    end;
  {$ENDIF}
end;

function IsAlphaChar(ch: UCS4Char): boolean;
begin
  Result := IsAlphaChar(WideChar(Ord(ch)));
end;

function IsNumericChar(ch: WideChar): boolean;
begin
  // TODO: replace with libxml2 functions?
  // ignore non-arabic numerals as we do not want to handle them
  case ch of
    '0'..'9':
      Result := true;
    else
      Result := false;
  end;
end;

function IsNumericChar(ch: UCS4Char): boolean;
begin
  Result := IsNumericChar(WideChar(Ord(ch)));
end;

function IsAlphaNumericChar(ch: WideChar): boolean;
begin
  Result := (IsAlphaChar(ch) or IsNumericChar(ch));
end;

function IsAlphaNumericChar(ch: UCS4Char): boolean;
begin
  Result := (IsAlphaChar(ch) or IsNumericChar(ch));
end;

function IsPunctuationChar(ch: WideChar): boolean;
begin
  // TODO: add chars > 255 (or replace with libxml2 functions?)
  case ch of
    ' '..'/',':'..'@','['..'`','{'..'~',
    #160..#191,#215,#247:
      Result := true;
    else
      Result := false;
  end;
end;

function IsPunctuationChar(ch: UCS4Char): boolean;
begin
  Result := IsPunctuationChar(WideChar(Ord(ch)));
end;

function IsControlChar(ch: WideChar): boolean;
begin
  case ch of
    #0..#31,
    #127..#159:
      Result := true;
    else
      Result := false;
  end;
end;

function IsControlChar(ch: UCS4Char): boolean;
begin
  Result := IsControlChar(WideChar(Ord(ch)));
end;

function IsPrintableChar(ch: WideChar): boolean;
begin
  Result := not IsControlChar(ch);
end;

function IsPrintableChar(ch: UCS4Char): boolean;
begin
  Result := IsPrintableChar(WideChar(Ord(ch)));
end;


function NextCharUTF8(var StrPtr: PAnsiChar; out Ch: UCS4Char): boolean;

  // find the most significant zero bit (Result: [7..-1])
  function FindZeroMSB(b: byte): integer;
  var
    Mask: byte;
  begin
    Mask := $80;
    Result := 7;
    while (b and Mask <> 0) do
    begin
      Mask := Mask shr 1;
      Dec(Result);
    end;
  end;

var
  ZeroBit: integer;
  SeqCount: integer; // number of trailing bytes to follow
const
  Mask: array[1..3] of byte = ($1F, $0F, $07);
begin
  Result := false;
  SeqCount := 0;
  Ch := 0;

  while (StrPtr^ <> #0) do
  begin
    if (StrPtr^ < #128) then
    begin
      // check that no more trailing bytes are expected
      if (SeqCount = 0) then
      begin
        Ch := Ord(StrPtr^);
        Inc(StrPtr);
        Result := true;
      end;
      Break;
    end
    else
    begin
      ZeroBit := FindZeroMSB(Ord(StrPtr^));
      // trailing byte expected
      if (SeqCount > 0) then
      begin
        // check if trailing byte has pattern 10xxxxxx
        if (ZeroBit <> 6) then
        begin
          Inc(StrPtr);
          Break;
        end;

        Dec(SeqCount);
        Ch := (Ch shl 6) or (Ord(StrPtr^) and $3F);

        // check if char is finished
        if (SeqCount = 0) then
        begin
          Inc(StrPtr);
          Result := true;
          Break;
        end;
      end
      else // leading byte expected
      begin
        // check if pattern is one of 110xxxxx/1110xxxx/11110xxx
        if (ZeroBit > 5) or (ZeroBit < 3) then
        begin
          Inc(StrPtr);
          Break;
        end;
        // calculate number of trailing bytes (1, 2 or 3)
        SeqCount := 6 - ZeroBit;
        // extract first part of char
        Ch := Ord(StrPtr^) and Mask[SeqCount];
      end;
    end;

    Inc(StrPtr);
  end;

  if (not Result) then
    Ch := Ord('?');
end;

function IsUTF8String(const str: RawByteString): boolean;
var
  Ch: UCS4Char;
  StrPtr: PAnsiChar;
begin
  Result := true;
  StrPtr := PChar(str);
  while (StrPtr^ <> #0) do
  begin
    if (not NextCharUTF8(StrPtr, Ch)) then
    begin
      Result := false;
      Exit;
    end;
  end;
end;

function IsASCIIString(const str: RawByteString): boolean;
var
  I: integer;
begin
  for I := 1 to Length(str) do
  begin
    if (str[I] >= #128) then
    begin
      Result := false;
      Exit;
    end;
  end;    
  Result := true;
end;


function UTF8ToUCS4String(const str: UTF8String): UCS4String;
begin
  Result := WideStringToUCS4String(UTF8Decode(str));
end;

function UCS4ToUTF8String(const str: UCS4String): UTF8String;
begin
  Result := UTF8Encode(UCS4StringToWideString(str));
end;

function UCS4ToUTF8String(ch: UCS4Char): UTF8String;
begin
  Result := UCS4ToUTF8String(UCS4CharToString(ch));
end;

function LengthUTF8(const str: UTF8String): integer;
begin
  Result := LengthUCS4(UTF8ToUCS4String(str));
end;

function LengthUCS4(const str: UCS4String): integer;
begin
  Result := High(str);
  if (Result = -1) then
    Result := 0;
end;

function UTF8CompareStr(const S1, S2: UTF8String): integer;
begin
  Result := WideCompareStr(UTF8Decode(S1), UTF8Decode(S2));
end;

function UTF8CompareText(const S1, S2: UTF8String): integer;
begin
  Result := WideCompareText(UTF8Decode(S1), UTF8Decode(S2));
end;

function UTF8StartsStr(const SubText, Text: UTF8String): boolean;
begin
  // TODO: use WideSameStr (slower but handles different representations of the same char)?
  Result := (Pos(SubText, Text) = 1);
end;

function UTF8StartsText(const SubText, Text: UTF8String): boolean;
begin
  // TODO: use WideSameText (slower but handles different representations of the same char)?
  Result := (Pos(UTF8UpperCase(SubText), UTF8UpperCase(Text)) = 1);
end;

function UTF8ContainsStr(const Text, SubText: UTF8String): boolean;
begin
  Result := Pos(SubText, Text) > 0;
end;

function UTF8ContainsText(const Text, SubText: UTF8String): boolean;
begin
  Result := Pos(UTF8UpperCase(SubText), UTF8UpperCase(Text)) > 0;
end;

function UTF8UpperCase(const str: UTF8String): UTF8String;
begin
  Result := UTF8Encode(WideStringUpperCase(UTF8Decode(str)));
end;

function UTF8LowerCase(const str: UTF8String): UTF8String;
begin
  Result := UTF8Encode(WideStringLowerCase(UTF8Decode(str)));
end;

function UCS4UpperCase(ch: UCS4Char): UCS4Char;
begin
  Result := UCS4UpperCase(UCS4CharToString(ch))[0];
end;

function UCS4UpperCase(const str: UCS4String): UCS4String;
begin
  // convert to upper-case as WideString and convert result back to UCS-4
  Result := WideStringToUCS4String(
            WideStringUpperCase(
            UCS4StringToWideString(str)));
end;

function UCS4CharToString(ch: UCS4Char): UCS4String;
begin
  SetLength(Result, 2);
  Result[0] := ch;
  Result[1] := 0;
end;

function UTF8Pos(const substr: UTF8String; const str: UTF8String): Integer;
begin
  Result := Pos(substr, str);
end;

function UTF8Copy(const str: UTF8String; Index: Integer; Count: Integer): UTF8String;
begin
  Result := UCS4ToUTF8String(UCS4Copy(UTF8ToUCS4String(str), Index-1, Count));
end;

function UCS4Copy(const str: UCS4String; Index: Integer; Count: Integer): UCS4String;
var
  I: integer;
  MaxCount: integer;
begin
  // calculate max. copy count
  MaxCount := LengthUCS4(str)-Index;
  if (MaxCount < 0) then
    MaxCount := 0;
  // adjust copy count
  if (Count > MaxCount) or (Count < 0) then
    Count := MaxCount;

  // copy (and add zero terminator)
  SetLength(Result, Count + 1);
  for I := 0 to Count-1 do
    Result[I] := str[Index+I];
  Result[Count] := 0;
end;

procedure UTF8Delete(var Str: UTF8String; Index: Integer; Count: Integer);
var
  StrUCS4: UCS4String;
begin
  StrUCS4 := UTF8ToUCS4String(str);
  UCS4Delete(StrUCS4, Index-1, Count);
  Str := UCS4ToUTF8String(StrUCS4);
end;

procedure UCS4Delete(var Str: UCS4String; Index: Integer; Count: Integer);
var
  Len: integer;
  OldStr: UCS4String;
  I: integer;
begin
  Len := LengthUCS4(Str);
  if (Count <= 0) or (Index < 0) or (Index >= Len) then
    Exit;
  if (Index + Count > Len) then
    Count := Len-Index;
  
  OldStr := Str;
  SetLength(Str, Len-Count+1);
  for I := 0 to Index-1 do
    Str[I] := OldStr[I];
  for I := Index+Count to Len-1 do
    Str[I-Count] := OldStr[I];
  Str[High(Str)] := 0;
end;

function WideStringUpperCase(ch: WideChar): WideString;
begin
  // If WideChar #0 is converted to a WideString in Delphi, a string with
  // length 1 and a single char #0 is returned. In FPC an empty (length=0)
  // string will be returned. This will crash, if a non printable key was
  // pressed, its char code (#0) is translated to upper-case and the the first
  // character is accessed with Result[1].
  // We cannot catch this error in the WideString parameter variant as the string
  // has length 0 already.
  
  // Force min. string length of 1
  if (ch = #0) then
    Result := #0
  else
    Result := WideStringUpperCase(WideString(ch));
end;

function WideStringUpperCase(const str: WideString): WideString;
begin
  // On Linux and MacOSX the cwstring unit is necessary for Unicode function-calls.
  // Otherwise you will get an EIntOverflow exception (thrown by unimplementedwidestring()).
  // The Unicode manager cwstring does not work with MacOSX at the moment because
  // of missing references to iconv.
  // Note: Should be fixed now

  {.$IFNDEF DARWIN}
  {.$IFDEF NOIGNORE}
    Result := WideUpperCase(str)
  {.$ELSE}
    //Result := UTF8Decode(UpperCase(UTF8Encode(str)));
  {.$ENDIF}
end;

function WideStringLowerCase(ch: WideChar): WideString;
begin
  // see WideStringUpperCase
  if (ch = #0) then
    Result := #0
  else
    Result := WideStringLowerCase(WideString(ch));
end;

function WideStringLowerCase(const str: WideString): WideString;
begin
  // see WideStringUpperCase
  Result := WideLowerCase(str)
end;

function WideStringReplaceChar(const text: WideString; search, rep: WideChar): WideString;
var
  iPos  : integer;
//  sTemp : WideString;
begin
(*
  result := text;
  iPos   := Pos(search, result);
  while (iPos > 0) do
  begin
    sTemp  := copy(result, iPos + length(search), length(result));
    result := copy(result, 1, iPos - 1) + rep + sTEmp;
    iPos   := Pos(search, result);
  end;
*)
  result := text;

  if search = rep then
    exit;

  for iPos := 1 to length(result) do
  begin
    if result[iPos] = search then
      result[iPos] := rep;
  end;
end;

initialization
  InitUnicodeUtils;

end.
