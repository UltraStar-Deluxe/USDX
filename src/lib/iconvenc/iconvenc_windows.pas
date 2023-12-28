{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Marco van de Voort(marco@freepascal.org)
    member of the Free Pascal development team

    libiconv header translation + a helper routine
    http://wiki.freepascal.org/iconvenc, adapted for usage with Windows

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright. (LGPL)

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

}
unit iconvenc_windows;

interface
{$mode objfpc}{$H+}


uses
  ctypes;

type
  iconv_t = Pointer;
  psize_t = ^size_t;

const
  iconvlib = 'libiconv-2.dll';
  ESysE2BIG = 7;
  ESysEILSEQ = 42;

function iconv_open(ToCode, FromCode: PChar): iconv_t; cdecl; external iconvlib name 'libiconv_open';
function iconv(__cd: iconv_t; __inbuf: PPChar; __inbytesleft: psize_t; __outbuf: ppchar; __outbytesleft: psize_t): size_t; cdecl; external iconvlib name 'libiconv';
function iconv_close(cd: iconv_t): Integer; cdecl; external iconvlib name 'libiconv_close';
function errno_location: PInteger; cdecl; external 'msvcrt.dll' name '_errno';

function Iconvert(s: string; var res: string; const  FromEncoding, ToEncoding: string): cint;

implementation

function Iconvert(S: string; var Res: string; const FromEncoding, ToEncoding: string): cint;
var
  InLen, OutLen, Offset: size_t;
  Src, Dst: pchar;
  H: iconv_t;
  lerr: cint;
  iconvres: size_t;
begin
  H := iconv_open(PChar(ToEncoding), PChar(FromEncoding));
  if h=Iconv_t(-1) then
  begin
    Res := S;
    exit(-1);
  end;

  if Length(s) = 0 then
  begin
    Res := '';
    exit(0);
  end;
  try
    InLen:=Length(s);
    outlen:=InLen;
    setlength(res,outlen);

    Src := PChar(S);
    Dst := PChar(Res);

    while InLen > 0 do
    begin
      iconvres := iconv(H, @Src, @InLen, @Dst, @OutLen);
      if iconvres = size_t(-1) then
      begin
        lerr := errno_location^;
        if lerr = ESysEILSEQ then // unknown char, skip
          begin
            Dst^ := Src^;
            Inc(Src);
            Inc(Dst);
            Dec(InLen);
            Dec(OutLen);
          end
        else
          if lerr = ESysE2BIG then
            begin
              Offset := Dst - PChar(Res);
              SetLength(Res, Length(Res)+InLen*2+5); // 5 is minimally one utf-8 char
              Dst := PChar(Res) + Offset;
              OutLen := Length(Res) - Offset;
            end
          else
            exit(-1)
      end;
    end;

    // iconv has a buffer that needs flushing, specially if the last char is not #0
    iconvres:=iconv(H, nil, nil, @Dst, @Outlen);
    lerr:=errno_location^;
    if (iconvres=size_t(-1)) and (lerr=ESysE2BIG) then
      begin
        Offset:=Dst-PChar(Res);
        SetLength(Res, Length(Res)+InLen*2+5); // 5 is minimally one utf-8 char
        Dst:=PChar(Res)+Offset;
        OutLen:=Length(Res)-Offset;
        InLen:=0;
        iconv(H, nil, @InLen, @Dst, @Outlen);
      end;
    // trim output buffer
    SetLength(Res, Length(Res) - Outlen);
  finally
        Offset:=Dst-PChar(Res);
        SetLength(Res, Length(Res)+InLen*2+5); // 5 is minimally one utf-8 char
        Dst:=PChar(Res)+Offset;
        OutLen:=Length(Res)-Offset;
        InLen:=0;
        iconvres:=iconv(H, nil, @InLen, @Dst, @Outlen);
        setlength(Res,Length(Res) - Outlen);
    iconv_close(H);
  end;

  Result := 0;
end;

end.
