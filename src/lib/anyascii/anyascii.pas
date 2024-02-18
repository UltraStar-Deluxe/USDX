{
The data tables in anyascii.inc and the anyascii function in here have been
converted from the C implementation in anyascii 0.3.2. The original copyright
header is:

    ISC License

    Copyright (c) 2020-2023, Hunter WB <hunterwb.com>

    Permission to use, copy, modify, and/or distribute this software for any
    purpose with or without fee is hereby granted, provided that the above
    copyright notice and this permission notice appear in all copies.

    THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
    WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
    MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
    ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
    WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
    ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
}

unit anyascii;

{$MODESWITCH OUT}

interface

function anyascii(utf32: cardinal; out ascii: PAnsiChar): integer;
function transliterate(s: UCS4String): UCS4String;

implementation

uses sysutils;

{$i anyascii.inc}

function anyascii(utf32: cardinal; out ascii: PAnsiChar): integer;
var
	b: PAnsiChar;
	loffset: integer;
	boffset: integer;
begin
	anyascii := 0;
	b := block(utf32 shr 8);
	if b <> nil then
	begin
		loffset := utf32 and $ff;
		if byte(b[0]) >= loffset then
		begin
			loffset := 1 + loffset * 3;
			anyascii := 3;
			if (byte(b[loffset + 2]) and $80) <> 0 then
				anyascii := byte(b[loffset + 2]) and $7f;
			if anyascii <= 3 then
				ascii := @b[loffset]
			else
			begin
				boffset := (byte(b[loffset]) shl 8) or byte(b[loffset + 1]);
				ascii := @bank[boffset];
			end;
		end;
	end;
end;

function transliterate(s: UCS4String): UCS4String;
var
	i: Integer;
	n: Integer;
	l: Integer;
	a: PAnsiChar;
	ret: UCS4String; // Free Pascal doesn't like us working directly on the result variable
begin
	n := 0;
	for i := 0 to Length(s) - 1 do
	begin
		l := anyascii(s[i], a);
		if l > 0 then
			n := n + l
		else
			Inc(n);
	end;
	SetLength(ret, n);
	n := 0;
	for i := 0 to Length(s) - 1 do
	begin
		l := anyascii(s[i], a);
		if l > 0 then
			while l > 0 do
			begin
				ret[n] := UCS4Char(a^);
				Inc(n);
				Inc(a);
				Dec(l);

			end
		else
		begin
			ret[n] := s[i];
			Inc(n);
		end
	end;
	transliterate := ret;
end;

end.
