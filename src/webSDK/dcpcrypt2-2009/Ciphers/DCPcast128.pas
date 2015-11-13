{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of Cast128 ******************************}
{******************************************************************************}
{* Copyright (c) 1999-2002 David Barton                                       *}
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
unit DCPcast128;

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPconst, DCPblockciphers;

type
  TDCP_cast128= class(TDCP_blockcipher64)
  protected
    KeyData: array[0..31] of DWord;
    Rounds: longword;
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

{$I DCPcast128.inc}

function LRot32(a, n: dword): dword;
begin
  Result:= (a shl n) or (a shr (32-n));
end;

class function TDCP_cast128.GetMaxKeySize: integer;
begin
  Result:= 128;
end;

class function TDCP_cast128.GetID: integer;
begin
  Result:= DCP_cast128;
end;

class function TDCP_cast128.GetAlgorithm: string;
begin
  Result:= 'Cast128';
end;

class function TDCP_cast128.SelfTest: boolean;
const
  Key: array[0..15] of byte=
    ($01,$23,$45,$67,$12,$34,$56,$78,$23,$45,$67,$89,$34,$56,$78,$9A);
  InBlock: array[0..7] of byte=
    ($01,$23,$45,$67,$89,$AB,$CD,$EF);
  Out128: array[0..7] of byte=
    ($23,$8B,$4F,$E5,$84,$7E,$44,$B2);
  Out80: array[0..7] of byte=
    ($EB,$6A,$71,$1A,$2C,$02,$27,$1B);
  Out40: array[0..7] of byte=
    ($7A,$C8,$16,$D1,$6E,$9B,$30,$2E);
var
  Block: array[0..7] of byte;
  Cipher: TDCP_cast128;
begin
  Cipher:= TDCP_cast128.Create(nil);
  Cipher.Init(Key,128,nil);
  Cipher.EncryptECB(InBlock,Block);
  Result:= boolean(CompareMem(@Block,@Out128,8));
  Cipher.DecryptECB(Block,Block);
  Result:= Result and boolean(CompareMem(@Block,@InBlock,8));
  Cipher.Burn;
  Cipher.Init(Key,80,nil);
  Cipher.EncryptECB(InBlock,Block);
  Result:= Result and boolean(CompareMem(@Block,@Out80,8));
  Cipher.DecryptECB(Block,Block);
  Result:= Result and boolean(CompareMem(@Block,@InBlock,8));
  Cipher.Burn;
  Cipher.Init(Key,40,nil);
  Cipher.EncryptECB(InBlock,Block);
  Result:= Result and boolean(CompareMem(@Block,@Out40,8));
  Cipher.DecryptECB(Block,Block);
  Result:= Result and boolean(CompareMem(@Block,@InBlock,8));
  Cipher.Burn;
  Cipher.Free;
end;

procedure TDCP_cast128.InitKey(const Key; Size: longword);
var
  x, t, z: array[0..3] of DWord;
  i: longword;
begin
  Size:= Size div 8;
  if Size<= 10 then
    Rounds:= 12
  else
    Rounds:= 16;
  FillChar(x,Sizeof(x),0);
  Move(Key,x,Size);
  x[0]:= (x[0] shr 24) or ((x[0] shr 8) and $FF00) or ((x[0] shl 8) and $FF0000) or (x[0] shl 24);
  x[1]:= (x[1] shr 24) or ((x[1] shr 8) and $FF00) or ((x[1] shl 8) and $FF0000) or (x[1] shl 24);
  x[2]:= (x[2] shr 24) or ((x[2] shr 8) and $FF00) or ((x[2] shl 8) and $FF0000) or (x[2] shl 24);
  x[3]:= (x[3] shr 24) or ((x[3] shr 8) and $FF00) or ((x[3] shl 8) and $FF0000) or (x[3] shl 24);
  i:= 0;
  while i< 32 do
  begin
    case (i and 4) of
      0:
        begin
          z[0]:= x[0] xor cast_sbox5[(x[3] shr 16) and $FF] xor
           cast_sbox6[x[3] and $FF] xor cast_sbox7[x[3] shr 24] xor
           cast_sbox8[(x[3] shr 8) and $FF] xor cast_sbox7[x[2] shr 24];
          t[0]:= z[0];
          z[1]:= x[2] xor cast_sbox5[z[0] shr 24] xor
           cast_sbox6[(z[0] shr 8) and $FF] xor cast_sbox7[(z[0] shr 16) and $FF] xor
           cast_sbox8[z[0] and $FF] xor cast_sbox8[(x[2] shr 8) and $FF];
          t[1]:= z[1];
          z[2]:= x[3] xor cast_sbox5[z[1] and $FF] xor
           cast_sbox6[(z[1] shr 8) and $FF] xor cast_sbox7[(z[1] shr 16) and $FF] xor
           cast_sbox8[z[1] shr 24] xor cast_sbox5[(x[2] shr 16) and $FF];
          t[2]:= z[2];
          z[3]:= x[1] xor cast_sbox5[(z[2] shr 8) and $FF] xor
           cast_sbox6[(z[2] shr 16) and $FF] xor cast_sbox7[z[2] and $FF] xor
           cast_sbox8[z[2] shr 24] xor cast_sbox6[x[2] and $FF];
          t[3]:= z[3];
        end;
      4:
        begin
          x[0]:= z[2] xor cast_sbox5[(z[1] shr 16) and $FF] xor
           cast_sbox6[z[1] and $FF] xor cast_sbox7[z[1] shr 24] xor
           cast_sbox8[(z[1] shr 8) and $FF] xor cast_sbox7[z[0] shr 24];
          t[0]:= x[0];
          x[1]:= z[0] xor cast_sbox5[x[0] shr 24] xor
           cast_sbox6[(x[0] shr 8) and $FF] xor cast_sbox7[(x[0] shr 16) and $FF] xor
           cast_sbox8[x[0] and $FF] xor cast_sbox8[(z[0] shr 8) and $FF];
          t[1]:= x[1];
          x[2]:= z[1] xor cast_sbox5[x[1] and $FF] xor
           cast_sbox6[(x[1] shr 8) and $FF] xor cast_sbox7[(x[1] shr 16) and $FF] xor
           cast_sbox8[x[1] shr 24] xor cast_sbox5[(z[0] shr 16) and $FF];
          t[2]:= x[2];
          x[3]:= z[3] xor cast_sbox5[(x[2] shr 8) and $FF] xor
           cast_sbox6[(x[2] shr 16) and $FF] xor cast_sbox7[x[2] and $FF] xor
           cast_sbox8[x[2] shr 24] xor cast_sbox6[z[0] and $FF];
          t[3]:= x[3];
        end;
    end;
    case (i and 12) of
      0,12:
        begin
          KeyData[i+0]:= cast_sbox5[t[2] shr 24] xor cast_sbox6[(t[2] shr 16) and $FF] xor
           cast_sbox7[t[1] and $FF] xor cast_sbox8[(t[1] shr 8) and $FF];
          KeyData[i+1]:= cast_sbox5[(t[2] shr 8) and $FF] xor cast_sbox6[t[2] and $FF] xor
           cast_sbox7[(t[1] shr 16) and $FF] xor cast_sbox8[t[1] shr 24];
          KeyData[i+2]:= cast_sbox5[t[3] shr 24] xor cast_sbox6[(t[3] shr 16) and $FF] xor
           cast_sbox7[t[0] and $FF] xor cast_sbox8[(t[0] shr 8) and $FF];
          KeyData[i+3]:= cast_sbox5[(t[3] shr 8) and $FF] xor cast_sbox6[t[3] and $FF] xor
           cast_sbox7[(t[0] shr 16) and $FF] xor cast_sbox8[t[0] shr 24];
        end;
      4,8:
        begin
          KeyData[i+0]:= cast_sbox5[t[0] and $FF] xor cast_sbox6[(t[0] shr 8) and $FF] xor
           cast_sbox7[t[3] shr 24] xor cast_sbox8[(t[3] shr 16) and $FF];
          KeyData[i+1]:= cast_sbox5[(t[0] shr 16) and $FF] xor cast_sbox6[t[0] shr 24] xor
           cast_sbox7[(t[3] shr 8) and $FF] xor cast_sbox8[t[3] and $FF];
          KeyData[i+2]:= cast_sbox5[t[1] and $FF] xor cast_sbox6[(t[1] shr 8) and $FF] xor
           cast_sbox7[t[2] shr 24] xor cast_sbox8[(t[2] shr 16) and $FF];
          KeyData[i+3]:= cast_sbox5[(t[1] shr 16) and $FF] xor cast_sbox6[t[1] shr 24] xor
           cast_sbox7[(t[2] shr 8) and $FF] xor cast_sbox8[t[2] and $FF];
        end;
    end;
    case (i and 12) of
      0:
        begin
          KeyData[i+0]:= KeyData[i+0] xor cast_sbox5[(z[0] shr 8) and $FF];
          KeyData[i+1]:= KeyData[i+1] xor cast_sbox6[(z[1] shr 8) and $FF];
          KeyData[i+2]:= KeyData[i+2] xor cast_sbox7[(z[2] shr 16) and $FF];
          KeyData[i+3]:= KeyData[i+3] xor cast_sbox8[z[3] shr 24];
        end;
      4:
        begin
          KeyData[i+0]:= KeyData[i+0] xor cast_sbox5[x[2] shr 24];
          KeyData[i+1]:= KeyData[i+1] xor cast_sbox6[(x[3] shr 16) and $FF];
          KeyData[i+2]:= KeyData[i+2] xor cast_sbox7[x[0] and $FF];
          KeyData[i+3]:= KeyData[i+3] xor cast_sbox8[x[1] and $FF];
        end;
      8:
        begin
          KeyData[i+0]:= KeyData[i+0] xor cast_sbox5[(z[2] shr 16) and $FF];
          KeyData[i+1]:= KeyData[i+1] xor cast_sbox6[z[3] shr 24];
          KeyData[i+2]:= KeyData[i+2] xor cast_sbox7[(z[0] shr 8) and $FF];
          KeyData[i+3]:= KeyData[i+3] xor cast_sbox8[(z[1] shr 8) and $FF];
        end;
      12:
        begin
          KeyData[i+0]:= KeyData[i+0] xor cast_sbox5[x[0] and $FF];
          KeyData[i+1]:= KeyData[i+1] xor cast_sbox6[x[1] and $FF];
          KeyData[i+2]:= KeyData[i+2] xor cast_sbox7[x[2] shr 24];
          KeyData[i+3]:= KeyData[i+3] xor cast_sbox8[(x[3] shr 16) and $FF];
        end;
    end;
    if (i >= 16) then
    begin
      KeyData[i+0]:= KeyData[i+0] and 31;
      KeyData[i+1]:= KeyData[i+1] and 31;
      KeyData[i+2]:= KeyData[i+2] and 31;
      KeyData[i+3]:= KeyData[i+3] and 31;
    end;
    Inc(i,4);
  end;
end;

procedure TDCP_cast128.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),$FF);
  Rounds:= 0;
  inherited Burn;
end;

procedure TDCP_cast128.EncryptECB(const InData; var OutData);
var
  t, l, r: DWord;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  l:= Pdword(@InData)^;
  r:= Pdword(longword(@InData)+4)^;
  l:= (l shr 24) or ((l shr 8) and $FF00) or ((l shl 8) and $FF0000) or (l shl 24);
  r:= (r shr 24) or ((r shr 8) and $FF00) or ((r shl 8) and $FF0000) or (r shl 24);
  t:= LRot32(KeyData[0]+r, KeyData[0+16]);
  l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[1] xor l, KeyData[1+16]);
  r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[2]-r, KeyData[2+16]);
  l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[3]+l, KeyData[3+16]);
  r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[4] xor r, KeyData[4+16]);
  l:= l xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[5]-l, KeyData[5+16]);
  r:= r xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[6]+r, KeyData[6+16]);
  l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[7] xor l, KeyData[7+16]);
  r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[8]-r, KeyData[8+16]);
  l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[9]+l, KeyData[9+16]);
  r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[10] xor r, KeyData[10+16]);
  l:= l xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[11]-l, KeyData[11+16]);
  r:= r xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  if Rounds> 12 then
  begin
    t:= LRot32(KeyData[12]+r, KeyData[12+16]);
    l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
      cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[13] xor l, KeyData[13+16]);
    r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
      cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[14]-r, KeyData[14+16]);
    l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
      cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[15]+l, KeyData[15+16]);
    r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
      cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  end;
  l:= (l shr 24) or ((l shr 8) and $FF00) or ((l shl 8) and $FF0000) or (l shl 24);
  r:= (r shr 24) or ((r shr 8) and $FF00) or ((r shl 8) and $FF0000) or (r shl 24);
  Pdword(@OutData)^:= r;
  Pdword(longword(@OutData)+4)^:= l;
end;

procedure TDCP_cast128.DecryptECB(const InData; var OutData);
var
  t, l, r: DWord;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  r:= Pdword(@InData)^;
  l:= Pdword(longword(@InData)+4)^;
  l:= (l shr 24) or ((l shr 8) and $FF00) or ((l shl 8) and $FF0000) or (l shl 24);
  r:= (r shr 24) or ((r shr 8) and $FF00) or ((r shl 8) and $FF0000) or (r shl 24);
  if Rounds> 12 then
  begin
    t:= LRot32(KeyData[15]+l, KeyData[15+16]);
    r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
      cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[14]-r, KeyData[14+16]);
    l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
      cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[13] xor l, KeyData[13+16]);
    r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
      cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
    t:= LRot32(KeyData[12]+r, KeyData[12+16]);
    l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
      cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  end;
  t:= LRot32(KeyData[11]-l, KeyData[11+16]);
  r:= r xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[10] xor r, KeyData[10+16]);
  l:= l xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[9]+l, KeyData[9+16]);
  r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[8]-r, KeyData[8+16]);
  l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[7] xor l, KeyData[7+16]);
  r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[6]+r, KeyData[6+16]);
  l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[5]-l, KeyData[5+16]);
  r:= r xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[4] xor r, KeyData[4+16]);
  l:= l xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[3]+l, KeyData[3+16]);
  r:= r xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[2]-r, KeyData[2+16]);
  l:= l xor (((cast_sbox1[t shr 24] + cast_sbox2[(t shr 16) and $FF]) xor
    cast_sbox3[(t shr 8) and $FF]) - cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[1] xor l, KeyData[1+16]);
  r:= r xor (((cast_sbox1[t shr 24] - cast_sbox2[(t shr 16) and $FF]) +
    cast_sbox3[(t shr 8) and $FF]) xor cast_sbox4[t and $FF]);
  t:= LRot32(KeyData[0]+r, KeyData[0+16]);
  l:= l xor (((cast_sbox1[t shr 24] xor cast_sbox2[(t shr 16) and $FF]) -
    cast_sbox3[(t shr 8) and $FF]) + cast_sbox4[t and $FF]);
  l:= (l shr 24) or ((l shr 8) and $FF00) or ((l shl 8) and $FF0000) or (l shl 24);
  r:= (r shr 24) or ((r shr 8) and $FF00) or ((r shl 8) and $FF0000) or (r shl 24);
  Pdword(@OutData)^:= l;
  Pdword(longword(@OutData)+4)^:= r;
end;


end.
