{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of Serpent ******************************}
{* Based on C source written by Brian Gladman (gladman@seven77.demon.co.uk) ***}
{* Thanks to Bruce Christensen for the initial Delphi translation *************}
{******************************************************************************}
{* Copyright (c) 2002 David Barton                                            *}
{* Permission is hereby granted, free of charge, to any person obtaining a    *}
{* copy of this software and associated documentation files (the "Software"), *}
{* to deal in the Software without restriction, including without limitation  *}
{* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *}
{* and/or sell copies of the Software, and to permit persons to whom the      *}
{* Software is furnished to do so, subject to the following conditions:       *}
{*                                                                            *}
{* The above copyright notice and this permission notice shall be included in *}
{* all copies or substantial portions of the Software.                        *}
{*                                                                            *}
{* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *}
{* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *}
{* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *}
{* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *}
{* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *}
{* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *}
{* DEALINGS IN THE SOFTWARE.                                                  *}
{******************************************************************************}
unit DCPserpent;

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPconst, DCPblockciphers;

type
  TDCP_serpent= class(TDCP_blockcipher128)
  protected
    l_key: array[0..131] of dword;
    procedure InitKey(const Key; Size: longword); override;
  public
    class function GetID: integer; override;
    class function GetAlgorithm: string; override;
    class function GetMaxKeySize: integer; override;
    class function SelfTest: boolean; override;
    procedure Burn; override;
    procedure EncryptECB(const InData; var OutData); override;
    procedure DecryptECB(const InData; var OutData); override;
  end;


{******************************************************************************}
{******************************************************************************}
implementation
{$R-}{$Q-}

class function TDCP_serpent.GetID: integer;
begin
  Result:= DCP_serpent;
end;

class function TDCP_serpent.GetAlgorithm: string;
begin
  Result:= 'Serpent';
end;

class function TDCP_serpent.GetMaxKeySize: integer;
begin
  Result:= 256;
end;

class function TDCP_serpent.SelfTest: boolean;
const
  Key1: array[0..15] of byte=
    ($ff,$ee,$dd,$cc,$bb,$aa,$99,$88,$77,$66,$55,$44,$33,$22,$11,$00);
  InData1: array[0..15] of byte=
    ($10,$32,$54,$76,$98,$ba,$dc,$fe,$ef,$cd,$ab,$89,$67,$45,$23,$01);
  OutData1: array[0..15] of byte=
    ($d5,$ba,$a0,$0a,$4b,$b9,$d8,$a7,$c9,$81,$c8,$dc,$90,$d8,$9d,$92);
  Key2: array[0..23] of byte=
    ($88,$99,$aa,$bb,$cc,$dd,$ee,$ff,$ff,$ee,$dd,$cc,$bb,$aa,$99,$88,
     $77,$66,$55,$44,$33,$22,$11,$00);
  InData2: array[0..15] of byte=
    ($10,$32,$54,$76,$98,$ba,$dc,$fe,$ef,$cd,$ab,$89,$67,$45,$23,$01);
  OutData2: array[0..15] of byte=
    ($da,$86,$08,$42,$b7,$20,$80,$2b,$f4,$04,$a4,$c7,$10,$34,$87,$9a);
  Key3: array[0..31] of byte=
    ($00,$11,$22,$33,$44,$55,$66,$77,$88,$99,$aa,$bb,$cc,$dd,$ee,$ff,
     $ff,$ee,$dd,$cc,$bb,$aa,$99,$88,$77,$66,$55,$44,$33,$22,$11,$00);
  InData3: array[0..15] of byte=
    ($10,$32,$54,$76,$98,$ba,$dc,$fe,$ef,$cd,$ab,$89,$67,$45,$23,$01);
  OutData3: array[0..15] of byte=
    ($93,$df,$9a,$3c,$af,$e3,$87,$bd,$99,$9e,$eb,$e3,$93,$a1,$7f,$ca);
var
  Block: array[0..15] of byte;
  Cipher: TDCP_serpent;
begin
  Cipher:= TDCP_serpent.Create(nil);
  Cipher.Init(Key1,Sizeof(Key1)*8,nil);
  Cipher.EncryptECB(InData1,Block);
  Result:= boolean(CompareMem(@Block,@OutData1,16));
  Cipher.DecryptECB(Block,Block);
  Cipher.Burn;
  Result:= Result and boolean(CompareMem(@Block,@InData1,16));
  Cipher.Init(Key2,Sizeof(Key2)*8,nil);
  Cipher.EncryptECB(InData2,Block);
  Result:= Result and boolean(CompareMem(@Block,@OutData2,16));
  Cipher.DecryptECB(Block,Block);
  Cipher.Burn;
  Result:= Result and boolean(CompareMem(@Block,@InData2,16));
  Cipher.Init(Key3,Sizeof(Key3)*8,nil);
  Cipher.EncryptECB(InData3,Block);
  Result:= Result and boolean(CompareMem(@Block,@OutData3,16));
  Cipher.DecryptECB(Block,Block);
  Cipher.Burn;
  Result:= Result and boolean(CompareMem(@Block,@InData3,16));
  Cipher.Free;
end;

procedure TDCP_serpent.InitKey(const Key; Size: longword);
var
  kp: array[0..139] of dword;
  i, n: integer;
  t, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17: dword;
  a, b, c, d: dword;
begin
  FillChar(kp,256 div 8,0);
  Move(Key,kp,Size div 8);
  if Size < 256 then
  begin
    i:= Size div 32;
    t:= 1 shl (Size mod 32);
    kp[i]:= (kp[i] and (t - 1)) or t;
  end;
  for i:= 8 to 139 do
  begin
    t:= kp[i - 8] xor kp[i - 5] xor kp[i - 3] xor kp[i - 1] xor $9e3779b9 xor longword(i-8);
    kp[i]:= (t shl 11) or (t shr 21);
  end;
  for i:= 0 to 3 do
  begin
    n:= i*32;
    a:= kp[n + 4*0 + 8]; b:= kp[n + 4*0 + 9]; c:= kp[n + 4*0 + 10]; d:= kp[n + 4*0 + 11];
    t1:= a xor c; t2:= a or d; t3:= a and b; t4:= a and d; t5:= b or t4; t6:= t1 and t2; kp[ 9+n]:= t5 xor t6; t8:= b xor d; t9:= c or t3; t10:= t6 xor t8; kp[ 11+n]:= t9 xor t10; t12:= c xor t3; t13:= t2 and kp[ 11+n]; kp[ 10+n]:= t12 xor t13; t15:= not kp[ 10+n]; t16:= t2 xor t3; t17:= kp[ 9+n] and t15; kp[ 8+n]:= t16 xor t17;
    a:= kp[n + 4*1 + 8]; b:= kp[n + 4*1 + 9]; c:= kp[n + 4*1 + 10]; d:= kp[n + 4*1 + 11];
    t1:= not a; t2:= b xor d; t3:= c and t1; kp[ 12+n]:= t2 xor t3; t5:= c xor t1; t6:= c xor kp[ 12+n]; t7:= b and t6; kp[ 15+n]:= t5 xor t7; t9:= d or t7; t10:= kp[ 12+n] or t5; t11:= t9 and t10; kp[ 14+n]:= a xor t11; t13:= d or t1; t14:= t2 xor kp[ 15+n]; t15:= kp[ 14+n] xor t13; kp[ 13+n]:= t14 xor t15;
    a:= kp[n + 4*2 + 8]; b:= kp[n + 4*2 + 9]; c:= kp[n + 4*2 + 10]; d:= kp[n + 4*2 + 11];
    t1:= a xor d; t2:= b xor d; t3:= a and b; t4:= not c; t5:= t2 xor t3; kp[ 18+n]:= t4 xor t5; t7:= a xor t2; t8:= b or t4; t9:= d or kp[ 18+n]; t10:= t7 and t9; kp[ 17+n]:= t8 xor t10; t12:= c xor d; t13:= t1 or t2; t14:= kp[ 17+n] xor t12; kp[ 19+n]:= t13 xor t14; t16:= t1 or kp[ 18+n]; t17:= t8 xor t14; kp[ 16+n]:= t16 xor t17;
    a:= kp[n + 4*3 + 8]; b:= kp[n + 4*3 + 9]; c:= kp[n + 4*3 + 10]; d:= kp[n + 4*3 + 11];
    t1:= b xor d; t2:= not t1; t3:= a or d; t4:= b xor c; kp[ 23+n]:= t3 xor t4; t6:= a xor b; t7:= a or t4; t8:= c and t6; t9:= t2 or t8; kp[ 20+n]:= t7 xor t9; t11:= a xor kp[ 23+n]; t12:= t1 and t6; t13:= kp[ 20+n] xor t11; kp[ 21+n]:= t12 xor t13; t15:= kp[ 20+n] or kp[ 21+n]; t16:= t3 and t15; kp[ 22+n]:= b xor t16;
    a:= kp[n + 4*4 + 8]; b:= kp[n + 4*4 + 9]; c:= kp[n + 4*4 + 10]; d:= kp[n + 4*4 + 11];
    t1:= not c; t2:= b xor c; t3:= b or t1; t4:= d xor t3; t5:= a and t4; kp[ 27+n]:= t2 xor t5; t7:= a xor d; t8:= b xor t5; t9:= t2 or t8; kp[ 25+n]:= t7 xor t9; t11:= d and t3; t12:= t5 xor kp[ 25+n]; t13:= kp[ 27+n] and t12; kp[ 26+n]:= t11 xor t13; t15:= t1 or t4; t16:= t12 xor kp[ 26+n]; kp[ 24+n]:= t15 xor t16;
    a:= kp[n + 4*5 + 8]; b:= kp[n + 4*5 + 9]; c:= kp[n + 4*5 + 10]; d:= kp[n + 4*5 + 11];
    t1:= a xor c; t2:= b or d; t3:= b xor c; t4:= not t3; t5:= a and d; kp[ 29+n]:= t4 xor t5; t7:= b or c; t8:= d xor t1; t9:= t7 and t8; kp[ 31+n]:= t2 xor t9; t11:= t1 and t7; t12:= t4 xor t8; t13:= kp[ 31+n] and t11; kp[ 28+n]:= t12 xor t13; t15:= t3 xor t11; t16:= kp[ 31+n] or t15; kp[ 30+n]:= t12 xor t16;
    a:= kp[n + 4*6 + 8]; b:= kp[n + 4*6 + 9]; c:= kp[n + 4*6 + 10]; d:= kp[n + 4*6 + 11];
    t1:= not a; t2:= a xor b; t3:= a xor d; t4:= c xor t1; t5:= t2 or t3; kp[ 32+n]:= t4 xor t5; t7:= not d; t8:= kp[ 32+n] and t7; kp[ 33+n]:= t2 xor t8; t10:= b or kp[ 33+n]; t11:= c or kp[ 32+n]; t12:= t7 xor t10; kp[ 35+n]:= t11 xor t12; t14:= d or kp[ 33+n]; t15:= t1 xor t14; t16:= kp[ 32+n] or kp[ 35+n]; kp[ 34+n]:= t15 xor t16;
    a:= kp[n + 4*7 + 8]; b:= kp[n + 4*7 + 9]; c:= kp[n + 4*7 + 10]; d:= kp[n + 4*7 + 11];
    t1:= not a; t2:= a xor d; t3:= a xor b; t4:= c xor t1; t5:= t2 or t3; kp[ 36+n]:= t4 xor t5; t7:= not kp[ 36+n]; t8:= b or t7; kp[ 39+n]:= t2 xor t8; t10:= a and kp[ 36+n]; t11:= b xor kp[ 39+n]; t12:= t8 and t11; kp[ 38+n]:= t10 xor t12; t14:= a or t7; t15:= t3 xor t14; t16:= kp[ 39+n] and kp[ 38+n]; kp[ 37+n]:= t15 xor t16;
  end;
  a:= kp[136]; b:= kp[137]; c:= kp[138]; d:= kp[139];
  t1:= a xor c; t2:= a or d; t3:= a and b; t4:= a and d; t5:= b or t4; t6:= t1 and t2; kp[137]:= t5 xor t6; t8:= b xor d; t9:= c or t3; t10:= t6 xor t8; kp[139]:= t9 xor t10; t12:= c xor t3; t13:= t2 and kp[139]; kp[138]:= t12 xor t13; t15:= not kp[138]; t16:= t2 xor t3; t17:= kp[137] and t15; kp[136]:= t16 xor t17;
  Move(kp[8],l_key,Sizeof(l_key));
  FillChar(kp,Sizeof(kp),0);
end;

procedure TDCP_serpent.Burn;
begin
  FillChar(l_key,Sizeof(l_key),0);
  inherited Burn;
end;

procedure TDCP_serpent.EncryptECB(const InData; var OutData);
var
  i: integer;
  a, b, c, d, e, f, g, h: dword;
  t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17: dword;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');

  a:= PDWord(@InData)^;
  b:= PDWord(longword(@InData)+4)^;
  c:= PDWord(longword(@InData)+8)^;
  d:= PDWord(longword(@InData)+12)^;

  i:= 0;
  while i < 32 do
  begin
    a:= a xor l_key[4*(i)]; b:= b xor l_key[4*(i)+1]; c:= c xor l_key[4*(i)+2]; d:= d xor l_key[4*(i)+3];
    t1:= b xor d; t2:= not t1; t3:= a or d; t4:= b xor c; h:= t3 xor t4; t6:= a xor b; t7:= a or t4; t8:= c and t6; t9:= t2 or t8; e:= t7 xor t9; t11:= a xor h; t12:= t1 and t6; t13:= e xor t11; f:= t12 xor t13; t15:= e or f; t16:= t3 and t15; g:= b xor t16;
    e:= (e shl 13) or (e shr 19); g:= (g shl 3) or (g shr 29); f:= f xor e xor g; h:= h xor g xor (e shl 3); f:= (f shl 1) or (f shr 31); h:= (h shl 7) or (h shr 25); e:= e xor f xor h; g:= g xor h xor (f shl 7); e:= (e shl 5) or (e shr 27); g:= (g shl 22) or (g shr 10);
    e:= e xor l_key[4*(i+1)]; f:= f xor l_key[4*(i+1)+1]; g:= g xor l_key[4*(i+1)+2]; h:= h xor l_key[4*(i+1)+3];
    t1:= e xor h; t2:= f xor h; t3:= e and f; t4:= not g; t5:= t2 xor t3; c:= t4 xor t5; t7:= e xor t2; t8:= f or t4; t9:= h or c; t10:= t7 and t9; b:= t8 xor t10; t12:= g xor h; t13:= t1 or t2; t14:= b xor t12; d:= t13 xor t14; t16:= t1 or c; t17:= t8 xor t14; a:= t16 xor t17;
    a:= (a shl 13) or (a shr 19); c:= (c shl 3) or (c shr 29); b:= b xor a xor c; d:= d xor c xor (a shl 3); b:= (b shl 1) or (b shr 31); d:= (d shl 7) or (d shr 25); a:= a xor b xor d; c:= c xor d xor (b shl 7); a:= (a shl 5) or (a shr 27); c:= (c shl 22) or (c shr 10);
    a:= a xor l_key[4*(i+2)]; b:= b xor l_key[4*(i+2)+1]; c:= c xor l_key[4*(i+2)+2]; d:= d xor l_key[4*(i+2)+3];
    t1:= not a; t2:= b xor d; t3:= c and t1; e:= t2 xor t3; t5:= c xor t1; t6:= c xor e; t7:= b and t6; h:= t5 xor t7; t9:= d or t7; t10:= e or t5; t11:= t9 and t10; g:= a xor t11; t13:= d or t1; t14:= t2 xor h; t15:= g xor t13; f:= t14 xor t15;
    e:= (e shl 13) or (e shr 19); g:= (g shl 3) or (g shr 29); f:= f xor e xor g; h:= h xor g xor (e shl 3); f:= (f shl 1) or (f shr 31); h:= (h shl 7) or (h shr 25); e:= e xor f xor h; g:= g xor h xor (f shl 7); e:= (e shl 5) or (e shr 27); g:= (g shl 22) or (g shr 10);
    e:= e xor l_key[4*(i+3)]; f:= f xor l_key[4*(i+3)+1]; g:= g xor l_key[4*(i+3)+2]; h:= h xor l_key[4*(i+3)+3];
    t1:= e xor g; t2:= e or h; t3:= e and f; t4:= e and h; t5:= f or t4; t6:= t1 and t2; b:= t5 xor t6; t8:= f xor h; t9:= g or t3; t10:= t6 xor t8; d:= t9 xor t10; t12:= g xor t3; t13:= t2 and d; c:= t12 xor t13; t15:= not c; t16:= t2 xor t3; t17:= b and t15; a:= t16 xor t17;
    a:= (a shl 13) or (a shr 19); c:= (c shl 3) or (c shr 29); b:= b xor a xor c; d:= d xor c xor (a shl 3); b:= (b shl 1) or (b shr 31); d:= (d shl 7) or (d shr 25); a:= a xor b xor d; c:= c xor d xor (b shl 7); a:= (a shl 5) or (a shr 27); c:= (c shl 22) or (c shr 10);
    a:= a xor l_key[4*(i+4)]; b:= b xor l_key[4*(i+4)+1]; c:= c xor l_key[4*(i+4)+2]; d:= d xor l_key[4*(i+4)+3];
    t1:= not a; t2:= a xor d; t3:= a xor b; t4:= c xor t1; t5:= t2 or t3; e:= t4 xor t5; t7:= not e; t8:= b or t7; h:= t2 xor t8; t10:= a and e; t11:= b xor h; t12:= t8 and t11; g:= t10 xor t12; t14:= a or t7; t15:= t3 xor t14; t16:= h and g; f:= t15 xor t16;
    e:= (e shl 13) or (e shr 19); g:= (g shl 3) or (g shr 29); f:= f xor e xor g; h:= h xor g xor (e shl 3); f:= (f shl 1) or (f shr 31); h:= (h shl 7) or (h shr 25); e:= e xor f xor h; g:= g xor h xor (f shl 7); e:= (e shl 5) or (e shr 27); g:= (g shl 22) or (g shr 10);
    e:= e xor l_key[4*(i+5)]; f:= f xor l_key[4*(i+5)+1]; g:= g xor l_key[4*(i+5)+2]; h:= h xor l_key[4*(i+5)+3];
    t1:= not e; t2:= e xor f; t3:= e xor h; t4:= g xor t1; t5:= t2 or t3; a:= t4 xor t5; t7:= not h; t8:= a and t7; b:= t2 xor t8; t10:= f or b; t11:= g or a; t12:= t7 xor t10; d:= t11 xor t12; t14:= h or b; t15:= t1 xor t14; t16:= a or d; c:= t15 xor t16;
    a:= (a shl 13) or (a shr 19); c:= (c shl 3) or (c shr 29); b:= b xor a xor c; d:= d xor c xor (a shl 3); b:= (b shl 1) or (b shr 31); d:= (d shl 7) or (d shr 25); a:= a xor b xor d; c:= c xor d xor (b shl 7); a:= (a shl 5) or (a shr 27); c:= (c shl 22) or (c shr 10);
    a:= a xor l_key[4*(i+6)]; b:= b xor l_key[4*(i+6)+1]; c:= c xor l_key[4*(i+6)+2]; d:= d xor l_key[4*(i+6)+3];
    t1:= a xor c; t2:= b or d; t3:= b xor c; t4:= not t3; t5:= a and d; f:= t4 xor t5; t7:= b or c; t8:= d xor t1; t9:= t7 and t8; h:= t2 xor t9; t11:= t1 and t7; t12:= t4 xor t8; t13:= h and t11; e:= t12 xor t13; t15:= t3 xor t11; t16:= h or t15; g:= t12 xor t16;
    e:= (e shl 13) or (e shr 19); g:= (g shl 3) or (g shr 29); f:= f xor e xor g; h:= h xor g xor (e shl 3); f:= (f shl 1) or (f shr 31); h:= (h shl 7) or (h shr 25); e:= e xor f xor h; g:= g xor h xor (f shl 7); e:= (e shl 5) or (e shr 27); g:= (g shl 22) or (g shr 10);
    e:= e xor l_key[4*(i+7)]; f:= f xor l_key[4*(i+7)+1]; g:= g xor l_key[4*(i+7)+2]; h:= h xor l_key[4*(i+7)+3];
    t1:= not g; t2:= f xor g; t3:= f or t1; t4:= h xor t3; t5:= e and t4; d:= t2 xor t5; t7:= e xor h; t8:= f xor t5; t9:= t2 or t8; b:= t7 xor t9; t11:= h and t3; t12:= t5 xor b; t13:= d and t12; c:= t11 xor t13; t15:= t1 or t4; t16:= t12 xor c; a:= t15 xor t16;

    Inc(i,8);
    if i < 32 then
    begin
      a:= (a shl 13) or (a shr 19); c:= (c shl 3) or (c shr 29); b:= b xor a xor c; d:= d xor c xor (a shl 3); b:= (b shl 1) or (b shr 31); d:= (d shl 7) or (d shr 25); a:= a xor b xor d; c:= c xor d xor (b shl 7); a:= (a shl 5) or (a shr 27); c:= (c shl 22) or (c shr 10);
    end;
  end;
  a:= a xor l_key[128]; b:= b xor l_key[128+1]; c:= c xor l_key[128+2]; d:= d xor l_key[128+3];

  PDWord(longword(@OutData)+ 0)^:= a;
  PDWord(longword(@OutData)+ 4)^:= b;
  PDWord(longword(@OutData)+ 8)^:= c;
  PDWord(longword(@OutData)+12)^:= d;
end;

procedure TDCP_serpent.DecryptECB(const InData; var OutData);
var
  i: integer;
  a, b, c, d, e, f, g, h: dword;
  t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16: dword;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');

  a:= PDWord(@InData)^;
  b:= PDWord(longword(@InData)+4)^;
  c:= PDWord(longword(@InData)+8)^;
  d:= PDWord(longword(@InData)+12)^;

  i:= 32;
  a:= a xor l_key[4*32]; b:= b xor l_key[4*32+1]; c:= c xor l_key[4*32+2]; d:= d xor l_key[4*32+3]; 
  while i > 0 do
  begin
    if i < 32 then
    begin
      c:= (c shr 22) or (c shl 10); a:= (a shr 5) or (a shl 27); c:= c xor d xor (b shl 7); a:= a xor b xor d; d:= (d shr 7) or (d shl 25); b:= (b shr 1) or (b shl 31); d:= d xor c xor (a shl 3); b:= b xor a xor c; c:= (c shr 3) or (c shl 29); a:= (a shr 13) or (a shl 19); 
    end;

    t1:= a and b; t2:= a or b; t3:= c or t1; t4:= d and t2; h:= t3 xor t4; t6:= not d; t7:= b xor t4; t8:= h xor t6; t9:= t7 or t8; f:= a xor t9; t11:= c xor t7; t12:= d or f; e:= t11 xor t12; t14:= a and h; t15:= t3 xor f; t16:= e xor t14; g:= t15 xor t16; 
    e:= e xor l_key[4*(i-1)]; f:= f xor l_key[4*(i-1)+1]; g:= g xor l_key[4*(i-1)+2]; h:= h xor l_key[4*(i-1)+3]; 
    g:= (g shr 22) or (g shl 10); e:= (e shr 5) or (e shl 27); g:= g xor h xor (f shl 7); e:= e xor f xor h; h:= (h shr 7) or (h shl 25); f:= (f shr 1) or (f shl 31); h:= h xor g xor (e shl 3); f:= f xor e xor g; g:= (g shr 3) or (g shl 29); e:= (e shr 13) or (e shl 19); 
    t1:= not g; t2:= e xor g; t3:= f xor h; t4:= e or t1; b:= t3 xor t4; t6:= e or f; t7:= f and t2; t8:= b xor t6; t9:= t7 or t8; a:= g xor t9; t11:= not b; t12:= h or t2; t13:= t9 xor t11; d:= t12 xor t13; t15:= f xor t11; t16:= a and d; c:= t15 xor t16; 
    a:= a xor l_key[4*(i-2)]; b:= b xor l_key[4*(i-2)+1]; c:= c xor l_key[4*(i-2)+2]; d:= d xor l_key[4*(i-2)+3]; 
    c:= (c shr 22) or (c shl 10); a:= (a shr 5) or (a shl 27); c:= c xor d xor (b shl 7); a:= a xor b xor d; d:= (d shr 7) or (d shl 25); b:= (b shr 1) or (b shl 31); d:= d xor c xor (a shl 3); b:= b xor a xor c; c:= (c shr 3) or (c shl 29); a:= (a shr 13) or (a shl 19); 
    t1:= not c; t2:= b and t1; t3:= d xor t2; t4:= a and t3; t5:= b xor t1; h:= t4 xor t5; t7:= b or h; t8:= a and t7; f:= t3 xor t8; t10:= a or d; t11:= t1 xor t7; e:= t10 xor t11; t13:= a xor c; t14:= b and t10; t15:= t4 or t13; g:= t14 xor t15; 
    e:= e xor l_key[4*(i-3)]; f:= f xor l_key[4*(i-3)+1]; g:= g xor l_key[4*(i-3)+2]; h:= h xor l_key[4*(i-3)+3]; 
    g:= (g shr 22) or (g shl 10); e:= (e shr 5) or (e shl 27); g:= g xor h xor (f shl 7); e:= e xor f xor h; h:= (h shr 7) or (h shl 25); f:= (f shr 1) or (f shl 31); h:= h xor g xor (e shl 3); f:= f xor e xor g; g:= (g shr 3) or (g shl 29); e:= (e shr 13) or (e shl 19); 
    t1:= g xor h; t2:= g or h; t3:= f xor t2; t4:= e and t3; b:= t1 xor t4; t6:= e xor h; t7:= f or h; t8:= t6 and t7; d:= t3 xor t8; t10:= not e; t11:= g xor d; t12:= t10 or t11; a:= t3 xor t12; t14:= g or t4; t15:= t7 xor t14; t16:= d or t10; c:= t15 xor t16; 
    a:= a xor l_key[4*(i-4)]; b:= b xor l_key[4*(i-4)+1]; c:= c xor l_key[4*(i-4)+2]; d:= d xor l_key[4*(i-4)+3]; 
    c:= (c shr 22) or (c shl 10); a:= (a shr 5) or (a shl 27); c:= c xor d xor (b shl 7); a:= a xor b xor d; d:= (d shr 7) or (d shl 25); b:= (b shr 1) or (b shl 31); d:= d xor c xor (a shl 3); b:= b xor a xor c; c:= (c shr 3) or (c shl 29); a:= (a shr 13) or (a shl 19); 
    t1:= b xor c; t2:= b or c; t3:= a xor c; t4:= t2 xor t3; t5:= d or t4; e:= t1 xor t5; t7:= a xor d; t8:= t1 or t5; t9:= t2 xor t7; g:= t8 xor t9; t11:= a and t4; t12:= e or t9; f:= t11 xor t12; t14:= a and g; t15:= t2 xor t14; t16:= e and t15; h:= t4 xor t16; 
    e:= e xor l_key[4*(i-5)]; f:= f xor l_key[4*(i-5)+1]; g:= g xor l_key[4*(i-5)+2]; h:= h xor l_key[4*(i-5)+3]; 
    g:= (g shr 22) or (g shl 10); e:= (e shr 5) or (e shl 27); g:= g xor h xor (f shl 7); e:= e xor f xor h; h:= (h shr 7) or (h shl 25); f:= (f shr 1) or (f shl 31); h:= h xor g xor (e shl 3); f:= f xor e xor g; g:= (g shr 3) or (g shl 29); e:= (e shr 13) or (e shl 19); 
    t1:= f xor h; t2:= not t1; t3:= e xor g; t4:= g xor t1; t5:= f and t4; a:= t3 xor t5; t7:= e or t2; t8:= h xor t7; t9:= t3 or t8; d:= t1 xor t9; t11:= not t4; t12:= a or d; b:= t11 xor t12; t14:= h and t11; t15:= t3 xor t12; c:= t14 xor t15; 
    a:= a xor l_key[4*(i-6)]; b:= b xor l_key[4*(i-6)+1]; c:= c xor l_key[4*(i-6)+2]; d:= d xor l_key[4*(i-6)+3]; 
    c:= (c shr 22) or (c shl 10); a:= (a shr 5) or (a shl 27); c:= c xor d xor (b shl 7); a:= a xor b xor d; d:= (d shr 7) or (d shl 25); b:= (b shr 1) or (b shl 31); d:= d xor c xor (a shl 3); b:= b xor a xor c; c:= (c shr 3) or (c shl 29); a:= (a shr 13) or (a shl 19);
    t1:= a xor d; t2:= a and b; t3:= b xor c; t4:= a xor t3; t5:= b or d; h:= t4 xor t5; t7:= c or t1; t8:= b xor t7; t9:= t4 and t8; f:= t1 xor t9; t11:= not t2; t12:= h and f; t13:= t9 xor t11; g:= t12 xor t13; t15:= a and d; t16:= c xor t13; e:= t15 xor t16;
    e:= e xor l_key[4*(i-7)]; f:= f xor l_key[4*(i-7)+1]; g:= g xor l_key[4*(i-7)+2]; h:= h xor l_key[4*(i-7)+3];
    g:= (g shr 22) or (g shl 10); e:= (e shr 5) or (e shl 27); g:= g xor h xor (f shl 7); e:= e xor f xor h; h:= (h shr 7) or (h shl 25); f:= (f shr 1) or (f shl 31); h:= h xor g xor (e shl 3); f:= f xor e xor g; g:= (g shr 3) or (g shl 29); e:= (e shr 13) or (e shl 19);
    t1:= e xor h; t2:= g xor h; t3:= not t2; t4:= e or f; c:= t3 xor t4; t6:= f xor t1; t7:= g or t6; t8:= e xor t7; t9:= t2 and t8; b:= t6 xor t9; t11:= not t8; t12:= f and h; t13:= b or t12; d:= t11 xor t13; t15:= t2 xor t12; t16:= b or d; a:= t15 xor t16;
    a:= a xor l_key[4*(i-8)]; b:= b xor l_key[4*(i-8)+1]; c:= c xor l_key[4*(i-8)+2]; d:= d xor l_key[4*(i-8)+3];
    Dec(i,8);
  end;

  PDWord(longword(@OutData)+ 0)^:= a;
  PDWord(longword(@OutData)+ 4)^:= b;
  PDWord(longword(@OutData)+ 8)^:= c;
  PDWord(longword(@OutData)+12)^:= d;
end;

end.
