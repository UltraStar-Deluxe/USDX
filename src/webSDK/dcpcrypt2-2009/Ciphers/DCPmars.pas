{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of Mars *********************************}
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
unit DCPmars;

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPconst, DCPblockciphers;

type
  TDCP_mars= class(TDCP_blockcipher128)
  protected
    KeyData: array[0..39] of DWord;
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
{$I DCPmars.inc}

function LRot32(X: DWord; c: longword): DWord;
begin
  LRot32:= (X shl c) or (X shr (32 - c));
end;

function RRot32(X: DWord; c: longword): DWord;
begin
  RRot32:= (X shr c) or (X shl (32 - c));
end;

class function TDCP_mars.GetID: integer;
begin
  Result:= DCP_mars;
end;

class function TDCP_mars.GetAlgorithm: string;
begin
  Result:= 'Mars';
end;

class function TDCP_mars.GetMaxKeySize: integer;
begin
  Result:= 1248;
end;

class function TDCP_mars.SelfTest: boolean;
const
  Key1: array[0..3] of dword=
    ($deb35132,$83c296de,$39069e6b,$994c2438);
  Key2: array[0..5] of dword=
    ($a5391779,$1a58048b,$a853a993,$1d41102c,$088658d1,$954d8738);
  Key3: array[0..7] of dword=
    ($9867a1fb,$22ef7a3e,$8ce27c31,$a3e1aa02,$3ccce5e8,$2aa8beed,$9ac3db99,$27725ed6);
  Plain1: array[0..3] of dword= ($deb35132,$83c296de,$39069e6b,$994c2438);
  Plain2: array[0..3] of dword= ($2dc46167,$d242613e,$adbf4fa8,$8f1583b3);
  Plain3: array[0..3] of dword= ($a4ab4413,$0847c4d3,$1621a7a8,$8493f4d4);
  Cipher1: array[0..3] of dword= ($a91245f9,$4e032db4,$042279c4,$9ba608d7);
  Cipher2: array[0..3] of dword= ($260334cb,$6d587f45,$e0d2bd54,$bd191c57);
  Cipher3: array[0..3] of dword= ($67a1acdd,$be3163e3,$5f9f1c2c,$b8a48fe3);
var
  Cipher: TDCP_mars;
  Block: array[0..3] of dword;
begin
  Cipher:= TDCP_mars.Create(nil);
  Cipher.Init(Key1,Sizeof(Key1)*8,nil);
  Cipher.EncryptECB(Plain1,Block);
  Result:= CompareMem(@Cipher1,@Block,Sizeof(Block));
  Cipher.DecryptECB(Block,Block);
  Result:= Result and CompareMem(@Plain1,@Block,Sizeof(Block));
  Cipher.Burn;
  Cipher.Init(Key2,Sizeof(Key2)*8,nil);
  Cipher.EncryptECB(Plain2,Block);
  Result:= Result and CompareMem(@Cipher2,@Block,Sizeof(Block));
  Cipher.DecryptECB(Block,Block);
  Result:= Result and CompareMem(@Plain2,@Block,Sizeof(Block));
  Cipher.Burn;
  Cipher.Init(Key3,Sizeof(Key3)*8,nil);
  Cipher.EncryptECB(Plain3,Block);
  Result:= Result and CompareMem(@Cipher3,@Block,Sizeof(Block));
  Cipher.DecryptECB(Block,Block);
  Result:= Result and CompareMem(@Plain3,@Block,Sizeof(Block));
  Cipher.Burn;
  Cipher.Free;
end;

procedure gen_mask(var x, m: DWord);
var
  u: DWord;
begin
  u:= x and (x shr 1); u:= u and (u shr 2);
  u:= u and (u shr 4); u:= u and (u shr 1) and (u shr 2);
  m:= u;
  u:= (x xor $FFFFFFFF) and ((x xor $FFFFFFFF) shr 1); u:= u and (u shr 2);
  u:= u and (u shr 4); u:= u and (u shr 1) and (u shr 2);
  u:= u or m;
  m:= (u shl 1) or (u shl 2) or (u shl 3)
       or (u shl 4) or (u shl 5) or (u shl 6)
       or (u shl 7) or (u shl 8);
  m:= (m or u or (u shl 9)) and ((x xor $FFFFFFFF) xor (x shl 1)) and ((x xor $FFFFFFFF) xor (x shr 1));
  m:= m and $FFFFFFFC;
end;

procedure TDCP_mars.InitKey(const Key; Size: longword);
var
  i, j, m, u, w: DWord;
  t: array[-7..39] of DWord;
  KeyB: array[0..39] of DWord;
begin
  Size:= Size div 8;
  FillChar(KeyB,Sizeof(KeyB),0);
  Move(Key,KeyB,Size);
  Size:= Size div 4;
  Move(vk,t,Sizeof(vk));
  for i:= 0 to 38 do
  begin
    u:= t[i-7] xor t[i-2];
    t[i]:= LRot32(u,3) xor KeyB[i mod DWord(Size)] xor i;
  end;
  t[39]:= Size;
  for j:= 0 to 6 do
  begin
    for i:= 1 to 39 do
    begin
      u:= t[i] + s_box[t[i-1] and $1FF];
      t[i]:= LRot32(u,9);
    end;
    u:= t[0] + s_box[t[39] and $1FF];
    t[0]:= LRot32(u,9);
  end;
  for i:= 0 to 39 do
    KeyData[(7*i) mod 40]:= t[i];
  i:= 5;
  repeat
    u:= s_box[265+(KeyData[i] and $3)];
    j:= KeyData[i+3] and $1f;
    w:= KeyData[i] or $3;
    gen_mask(w,m);
    KeyData[i]:= w xor (LRot32(u,j) and m);
    Inc(i,2);
  until i>= 37;
end;

procedure TDCP_mars.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),$FF);
  inherited Burn;
end;

procedure TDCP_mars.EncryptECB(const InData; var OutData);
var
  l, m, r, t: DWord;
  blk: array[0..3] of DWord;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  Blk[0]:= PDWord(@InData)^;
  Blk[1]:= PDWord(longword(@InData)+4)^;
  Blk[2]:= PDWord(longword(@InData)+8)^;
  Blk[3]:= PDWord(longword(@InData)+12)^;

  blk[0]:= blk[0] + KeyData[0]; blk[1]:= blk[1] + KeyData[1];
  blk[2]:= blk[2] + KeyData[2]; blk[3]:= blk[3] + KeyData[3];
  blk[1]:= blk[1] xor s_box[  blk[0]         and $FF];
  blk[1]:= blk[1]  +  s_box[((blk[0] shr  8) and $FF) + 256];
  blk[2]:= blk[2]  +  s_box[ (blk[0] shr 16) and $FF];
  blk[3]:= blk[3] xor s_box[((blk[0] shr 24) and $FF) + 256];
  blk[0]:= RRot32(blk[0], 24); blk[0]:= blk[0] + blk[3];
  blk[2]:= blk[2] xor s_box[  blk[1]         and $FF];
  blk[2]:= blk[2]  +  s_box[((blk[1] shr  8) and $FF) + 256];
  blk[3]:= blk[3]  +  s_box[ (blk[1] shr 16) and $FF];
  blk[0]:= blk[0] xor s_box[((blk[1] shr 24) and $FF) + 256];
  blk[1]:= RRot32(blk[1], 24); blk[1]:= blk[1] + blk[2];
  blk[3]:= blk[3] xor s_box[  blk[2]         and $FF];
  blk[3]:= blk[3]  +  s_box[((blk[2] shr  8) and $FF) + 256];
  blk[0]:= blk[0]  +  s_box[ (blk[2] shr 16) and $FF];
  blk[1]:= blk[1] xor s_box[((blk[2] shr 24) and $FF) + 256];
  blk[2]:= RRot32(blk[2], 24);
  blk[0]:= blk[0] xor s_box[  blk[3]         and $FF];
  blk[0]:= blk[0]  +  s_box[((blk[3] shr  8) and $FF) + 256];
  blk[1]:= blk[1]  +  s_box[ (blk[3] shr 16) and $FF];
  blk[2]:= blk[2] xor s_box[((blk[3] shr 24) and $FF) + 256];
  blk[3]:= RRot32(blk[3], 24);
  blk[1]:= blk[1] xor s_box[  blk[0]         and $FF];
  blk[1]:= blk[1]  +  s_box[((blk[0] shr  8) and $FF) + 256];
  blk[2]:= blk[2]  +  s_box[ (blk[0] shr 16) and $FF];
  blk[3]:= blk[3] xor s_box[((blk[0] shr 24) and $FF) + 256];
  blk[0]:= RRot32(blk[0], 24); blk[0]:= blk[0] + blk[3];
  blk[2]:= blk[2] xor s_box[  blk[1]         and $FF];
  blk[2]:= blk[2]  +  s_box[((blk[1] shr  8) and $FF) + 256];
  blk[3]:= blk[3]  +  s_box[ (blk[1] shr 16) and $FF];
  blk[0]:= blk[0] xor s_box[((blk[1] shr 24) and $FF) + 256];
  blk[1]:= RRot32(blk[1], 24); blk[1]:= blk[1] + blk[2];
  blk[3]:= blk[3] xor s_box[  blk[2]         and $FF];
  blk[3]:= blk[3]  +  s_box[((blk[2] shr  8) and $FF) + 256];
  blk[0]:= blk[0]  +  s_box[ (blk[2] shr 16) and $FF];
  blk[1]:= blk[1] xor s_box[((blk[2] shr 24) and $FF) + 256];
  blk[2]:= RRot32(blk[2], 24);
  blk[0]:= blk[0] xor s_box[  blk[3]         and $FF];
  blk[0]:= blk[0]  +  s_box[((blk[3] shr  8) and $FF) + 256];
  blk[1]:= blk[1]  +  s_box[ (blk[3] shr 16) and $FF];
  blk[2]:= blk[2] xor s_box[((blk[3] shr 24) and $FF) + 256];
  blk[3]:= RRot32(blk[3], 24);
  m:= blk[0] + KeyData[4];
  r:= LRot32(blk[0],13) * KeyData[5];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= LRot32(blk[0],13);
  blk[1]:= blk[1] + l;
  blk[2]:= blk[2] + m;
  blk[3]:= blk[3] xor r;
  m:= blk[1] + KeyData[6];
  r:= LRot32(blk[1],13) * KeyData[7];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= LRot32(blk[1],13);
  blk[2]:= blk[2] + l;
  blk[3]:= blk[3] + m;
  blk[0]:= blk[0] xor r;
  m:= blk[2] + KeyData[8];
  r:= LRot32(blk[2],13) * KeyData[9];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= LRot32(blk[2],13);
  blk[3]:= blk[3] + l;
  blk[0]:= blk[0] + m;
  blk[1]:= blk[1] xor r;
  m:= blk[3] + KeyData[10];
  r:= LRot32(blk[3],13) * KeyData[11];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= LRot32(blk[3],13);
  blk[0]:= blk[0] + l;
  blk[1]:= blk[1] + m;
  blk[2]:= blk[2] xor r;
  m:= blk[0] + KeyData[12];
  r:= LRot32(blk[0],13) * KeyData[13];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= LRot32(blk[0],13);
  blk[1]:= blk[1] + l;
  blk[2]:= blk[2] + m;
  blk[3]:= blk[3] xor r;
  m:= blk[1] + KeyData[14];
  r:= LRot32(blk[1],13) * KeyData[15];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= LRot32(blk[1],13);
  blk[2]:= blk[2] + l;
  blk[3]:= blk[3] + m;
  blk[0]:= blk[0] xor r;
  m:= blk[2] + KeyData[16];
  r:= LRot32(blk[2],13) * KeyData[17];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= LRot32(blk[2],13);
  blk[3]:= blk[3] + l;
  blk[0]:= blk[0] + m;
  blk[1]:= blk[1] xor r;
  m:= blk[3] + KeyData[18];
  r:= LRot32(blk[3],13) * KeyData[19];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= LRot32(blk[3],13);
  blk[0]:= blk[0] + l;
  blk[1]:= blk[1] + m;
  blk[2]:= blk[2] xor r;
  m:= blk[0] + KeyData[20];
  r:= LRot32(blk[0],13) * KeyData[21];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= LRot32(blk[0],13);
  blk[3]:= blk[3] + l;
  blk[2]:= blk[2] + m;
  blk[1]:= blk[1] xor r;
  m:= blk[1] + KeyData[22];
  r:= LRot32(blk[1],13) * KeyData[23];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= LRot32(blk[1],13);
  blk[0]:= blk[0] + l;
  blk[3]:= blk[3] + m;
  blk[2]:= blk[2] xor r;
  m:= blk[2] + KeyData[24];
  r:= LRot32(blk[2],13) * KeyData[25];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= LRot32(blk[2],13);
  blk[1]:= blk[1] + l;
  blk[0]:= blk[0] + m;
  blk[3]:= blk[3] xor r;
  m:= blk[3] + KeyData[26];
  r:= LRot32(blk[3],13) * KeyData[27];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= LRot32(blk[3],13);
  blk[2]:= blk[2] + l;
  blk[1]:= blk[1] + m;
  blk[0]:= blk[0] xor r;
  m:= blk[0] + KeyData[28];
  r:= LRot32(blk[0],13) * KeyData[29];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= LRot32(blk[0],13);
  blk[3]:= blk[3] + l;
  blk[2]:= blk[2] + m;
  blk[1]:= blk[1] xor r;
  m:= blk[1] + KeyData[30];
  r:= LRot32(blk[1],13) * KeyData[31];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= LRot32(blk[1],13);
  blk[0]:= blk[0] + l;
  blk[3]:= blk[3] + m;
  blk[2]:= blk[2] xor r;
  m:= blk[2] + KeyData[32];
  r:= LRot32(blk[2],13) * KeyData[33];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= LRot32(blk[2],13);
  blk[1]:= blk[1] + l;
  blk[0]:= blk[0] + m;
  blk[3]:= blk[3] xor r;
  m:= blk[3] + KeyData[34];
  r:= LRot32(blk[3],13) * KeyData[35];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= LRot32(blk[3],13);
  blk[2]:= blk[2] + l;
  blk[1]:= blk[1] + m;
  blk[0]:= blk[0] xor r;
  blk[1]:= blk[1] xor s_box[ (blk[0]         and $FF) + 256];
  blk[2]:= blk[2]  -  s_box[ (blk[0] shr 24) and $FF];
  blk[3]:= blk[3]  -  s_box[((blk[0] shr 16) and $FF) + 256];
  blk[3]:= blk[3] xor s_box[ (blk[0] shr  8) and $FF];
  blk[0]:= LRot32(blk[0], 24);
  blk[2]:= blk[2] xor s_box[ (blk[1]         and $FF) + 256];
  blk[3]:= blk[3]  -  s_box[ (blk[1] shr 24) and $FF];
  blk[0]:= blk[0]  -  s_box[((blk[1] shr 16) and $FF) + 256];
  blk[0]:= blk[0] xor s_box[ (blk[1] shr  8) and $FF];
  blk[1]:= LRot32(blk[1], 24); blk[2]:= blk[2] - blk[1];
  blk[3]:= blk[3] xor s_box[ (blk[2]         and $FF) + 256];
  blk[0]:= blk[0]  -  s_box[ (blk[2] shr 24) and $FF];
  blk[1]:= blk[1]  -  s_box[((blk[2] shr 16) and $FF) + 256];
  blk[1]:= blk[1] xor s_box[ (blk[2] shr  8) and $FF];
  blk[2]:= LRot32(blk[2], 24); blk[3]:= blk[3] - blk[0];
  blk[0]:= blk[0] xor s_box[ (blk[3]         and $FF) + 256];
  blk[1]:= blk[1]  -  s_box[ (blk[3] shr 24) and $FF];
  blk[2]:= blk[2]  -  s_box[((blk[3] shr 16) and $FF) + 256];
  blk[2]:= blk[2] xor s_box[ (blk[3] shr  8) and $FF];
  blk[3]:= LRot32(blk[3], 24);
  blk[1]:= blk[1] xor s_box[ (blk[0]         and $FF) + 256];
  blk[2]:= blk[2]  -  s_box[ (blk[0] shr 24) and $FF];
  blk[3]:= blk[3]  -  s_box[((blk[0] shr 16) and $FF) + 256];
  blk[3]:= blk[3] xor s_box[ (blk[0] shr  8) and $FF];
  blk[0]:= LRot32(blk[0], 24);
  blk[2]:= blk[2] xor s_box[ (blk[1]         and $FF) + 256];
  blk[3]:= blk[3]  -  s_box[ (blk[1] shr 24) and $FF];
  blk[0]:= blk[0]  -  s_box[((blk[1] shr 16) and $FF) + 256];
  blk[0]:= blk[0] xor s_box[ (blk[1] shr  8) and $FF];
  blk[1]:= LRot32(blk[1], 24); blk[2]:= blk[2] - blk[1];
  blk[3]:= blk[3] xor s_box[ (blk[2]         and $FF) + 256];
  blk[0]:= blk[0]  -  s_box[ (blk[2] shr 24) and $FF];
  blk[1]:= blk[1]  -  s_box[((blk[2] shr 16) and $FF) + 256];
  blk[1]:= blk[1] xor s_box[ (blk[2] shr  8) and $FF];
  blk[2]:= LRot32(blk[2], 24); blk[3]:= blk[3] - blk[0];
  blk[0]:= blk[0] xor s_box[ (blk[3]         and $FF) + 256];
  blk[1]:= blk[1]  -  s_box[ (blk[3] shr 24) and $FF];
  blk[2]:= blk[2]  -  s_box[((blk[3] shr 16) and $FF) + 256];
  blk[2]:= blk[2] xor s_box[ (blk[3] shr  8) and $FF];
  blk[3]:= LRot32(blk[3], 24);
  blk[0]:= blk[0] - KeyData[36]; blk[1]:= blk[1] - KeyData[37];
  blk[2]:= blk[2] - KeyData[38]; blk[3]:= blk[3] - KeyData[39];

  PDWord(@OutData)^:= Blk[0];
  PDWord(longword(@OutData)+4)^:= Blk[1];
  PDWord(longword(@OutData)+8)^:= Blk[2];
  PDWord(longword(@OutData)+12)^:= Blk[3];
end;

procedure TDCP_mars.DecryptECB(const InData; var OutData);
var
  l, m, r, t: DWord;
  blk: array[0..3] of DWord;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  Blk[0]:= PDWord(@InData)^;
  Blk[1]:= PDWord(longword(@InData)+4)^;
  Blk[2]:= PDWord(longword(@InData)+8)^;
  Blk[3]:= PDWord(longword(@InData)+12)^;

  blk[0]:= blk[0] + KeyData[36]; blk[1]:= blk[1] + KeyData[37];
  blk[2]:= blk[2] + KeyData[38]; blk[3]:= blk[3] + KeyData[39];
  blk[3]:= RRot32(blk[3], 24);
  blk[2]:= blk[2] xor s_box[ (blk[3] shr  8) and $FF];
  blk[2]:= blk[2]  +  s_box[((blk[3] shr 16) and $FF) + 256];
  blk[1]:= blk[1]  +  s_box[ (blk[3] shr 24) and $FF];
  blk[0]:= blk[0] xor s_box[ (blk[3]         and $FF) + 256];
  blk[3]:= blk[3] + blk[0]; blk[2]:= RRot32(blk[2], 24);
  blk[1]:= blk[1] xor s_box[ (blk[2] shr  8) and $FF];
  blk[1]:= blk[1]  +  s_box[((blk[2] shr 16) and $FF) + 256];
  blk[0]:= blk[0]  +  s_box[ (blk[2] shr 24) and $FF];
  blk[3]:= blk[3] xor s_box[ (blk[2]         and $FF) + 256];
  blk[2]:= blk[2] + blk[1]; blk[1]:= RRot32(blk[1], 24);
  blk[0]:= blk[0] xor s_box[ (blk[1] shr  8) and $FF];
  blk[0]:= blk[0]  +  s_box[((blk[1] shr 16) and $FF) + 256];
  blk[3]:= blk[3]  +  s_box[ (blk[1] shr 24) and $FF];
  blk[2]:= blk[2] xor s_box[ (blk[1]         and $FF) + 256];
  blk[0]:= RRot32(blk[0], 24);
  blk[3]:= blk[3] xor s_box[ (blk[0] shr  8) and $FF];
  blk[3]:= blk[3]  +  s_box[((blk[0] shr 16) and $FF) + 256];
  blk[2]:= blk[2]  +  s_box[ (blk[0] shr 24) and $FF];
  blk[1]:= blk[1] xor s_box[ (blk[0]         and $FF) + 256];
  blk[3]:= RRot32(blk[3], 24);
  blk[2]:= blk[2] xor s_box[ (blk[3] shr  8) and $FF];
  blk[2]:= blk[2]  +  s_box[((blk[3] shr 16) and $FF) + 256];
  blk[1]:= blk[1]  +  s_box[ (blk[3] shr 24) and $FF];
  blk[0]:= blk[0] xor s_box[ (blk[3]         and $FF) + 256];
  blk[3]:= blk[3] + blk[0]; blk[2]:= RRot32(blk[2], 24);
  blk[1]:= blk[1] xor s_box[ (blk[2] shr  8) and $FF];
  blk[1]:= blk[1]  +  s_box[((blk[2] shr 16) and $FF) + 256];
  blk[0]:= blk[0]  +  s_box[ (blk[2] shr 24) and $FF];
  blk[3]:= blk[3] xor s_box[ (blk[2]         and $FF) + 256];
  blk[2]:= blk[2] + blk[1]; blk[1]:= RRot32(blk[1], 24);
  blk[0]:= blk[0] xor s_box[ (blk[1] shr  8) and $FF];
  blk[0]:= blk[0]  +  s_box[((blk[1] shr 16) and $FF) + 256];
  blk[3]:= blk[3]  +  s_box[ (blk[1] shr 24) and $FF];
  blk[2]:= blk[2] xor s_box[ (blk[1]         and $FF) + 256];
  blk[0]:= RRot32(blk[0], 24);
  blk[3]:= blk[3] xor s_box[ (blk[0] shr  8) and $FF];
  blk[3]:= blk[3]  +  s_box[((blk[0] shr 16) and $FF) + 256];
  blk[2]:= blk[2]  +  s_box[ (blk[0] shr 24) and $FF];
  blk[1]:= blk[1] xor s_box[ (blk[0]         and $FF) + 256];
  blk[3]:= RRot32(blk[3],13);
  m:= blk[3] + KeyData[34];
  r:= LRot32(blk[3],13) * KeyData[35];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= blk[2] - l;
  blk[1]:= blk[1] - m;
  blk[0]:= blk[0] xor r;
  blk[2]:= RRot32(blk[2],13);
  m:= blk[2] + KeyData[32];
  r:= LRot32(blk[2],13) * KeyData[33];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= blk[1] - l;
  blk[0]:= blk[0] - m;
  blk[3]:= blk[3] xor r;
  blk[1]:= RRot32(blk[1],13);
  m:= blk[1] + KeyData[30];
  r:= LRot32(blk[1],13) * KeyData[31];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= blk[0] - l;
  blk[3]:= blk[3] - m;
  blk[2]:= blk[2] xor r;
  blk[0]:= RRot32(blk[0],13);
  m:= blk[0] + KeyData[28];
  r:= LRot32(blk[0],13) * KeyData[29];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= blk[3] - l;
  blk[2]:= blk[2] - m;
  blk[1]:= blk[1] xor r;
  blk[3]:= RRot32(blk[3],13);
  m:= blk[3] + KeyData[26];
  r:= LRot32(blk[3],13) * KeyData[27];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= blk[2] - l;
  blk[1]:= blk[1] - m;
  blk[0]:= blk[0] xor r;
  blk[2]:= RRot32(blk[2],13);
  m:= blk[2] + KeyData[24];
  r:= LRot32(blk[2],13) * KeyData[25];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= blk[1] - l;
  blk[0]:= blk[0] - m;
  blk[3]:= blk[3] xor r;
  blk[1]:= RRot32(blk[1],13);
  m:= blk[1] + KeyData[22];
  r:= LRot32(blk[1],13) * KeyData[23];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= blk[0] - l;
  blk[3]:= blk[3] - m;
  blk[2]:= blk[2] xor r;
  blk[0]:= RRot32(blk[0],13);
  m:= blk[0] + KeyData[20];
  r:= LRot32(blk[0],13) * KeyData[21];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= blk[3] - l;
  blk[2]:= blk[2] - m;
  blk[1]:= blk[1] xor r;
  blk[3]:= RRot32(blk[3],13);
  m:= blk[3] + KeyData[18];
  r:= LRot32(blk[3],13) * KeyData[19];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= blk[0] - l;
  blk[1]:= blk[1] - m;
  blk[2]:= blk[2] xor r;
  blk[2]:= RRot32(blk[2],13);
  m:= blk[2] + KeyData[16];
  r:= LRot32(blk[2],13) * KeyData[17];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= blk[3] - l;
  blk[0]:= blk[0] - m;
  blk[1]:= blk[1] xor r;
  blk[1]:= RRot32(blk[1],13);
  m:= blk[1] + KeyData[14];
  r:= LRot32(blk[1],13) * KeyData[15];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= blk[2] - l;
  blk[3]:= blk[3] - m;
  blk[0]:= blk[0] xor r;
  blk[0]:= RRot32(blk[0],13);
  m:= blk[0] + KeyData[12];
  r:= LRot32(blk[0],13) * KeyData[13];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= blk[1] - l;
  blk[2]:= blk[2] - m;
  blk[3]:= blk[3] xor r;
  blk[3]:= RRot32(blk[3],13);
  m:= blk[3] + KeyData[10];
  r:= LRot32(blk[3],13) * KeyData[11];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[0]:= blk[0] - l;
  blk[1]:= blk[1] - m;
  blk[2]:= blk[2] xor r;
  blk[2]:= RRot32(blk[2],13);
  m:= blk[2] + KeyData[8];
  r:= LRot32(blk[2],13) * KeyData[9];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[3]:= blk[3] - l;
  blk[0]:= blk[0] - m;
  blk[1]:= blk[1] xor r;
  blk[1]:= RRot32(blk[1],13);
  m:= blk[1] + KeyData[6];
  r:= LRot32(blk[1],13) * KeyData[7];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[2]:= blk[2] - l;
  blk[3]:= blk[3] - m;
  blk[0]:= blk[0] xor r;
  blk[0]:= RRot32(blk[0],13);
  m:= blk[0] + KeyData[4];
  r:= LRot32(blk[0],13) * KeyData[5];
  l:= s_box[m and $1FF]; r:= LRot32(r,5);
  t:= r and $1f; m:= LRot32(m,t);
  l:= l xor r; r:= LRot32(r,5); l:= l xor r;
  t:= r and $1f; l:= LRot32(l,t);
  blk[1]:= blk[1] - l;
  blk[2]:= blk[2] - m;
  blk[3]:= blk[3] xor r;
  blk[3]:= LRot32(blk[3], 24);
  blk[2]:= blk[2] xor s_box[((blk[3] shr 24) and $FF) + 256];
  blk[1]:= blk[1]  -  s_box[ (blk[3] shr 16) and $FF];
  blk[0]:= blk[0]  -  s_box[((blk[3] shr  8) and $FF) + 256];
  blk[0]:= blk[0] xor s_box[  blk[3]         and $FF];
  blk[2]:= LRot32(blk[2], 24);
  blk[1]:= blk[1] xor s_box[((blk[2] shr 24) and $FF) + 256];
  blk[0]:= blk[0]  -  s_box[ (blk[2] shr 16) and $FF];
  blk[3]:= blk[3]  -  s_box[((blk[2] shr  8) and $FF) + 256];
  blk[3]:= blk[3] xor s_box[  blk[2]         and $FF];
  blk[1]:= blk[1] - blk[2]; blk[1]:= LRot32(blk[1], 24);
  blk[0]:= blk[0] xor s_box[((blk[1] shr 24) and $FF) + 256];
  blk[3]:= blk[3]  -  s_box[ (blk[1] shr 16) and $FF];
  blk[2]:= blk[2]  -  s_box[((blk[1] shr  8) and $FF) + 256];
  blk[2]:= blk[2] xor s_box[  blk[1]         and $FF];
  blk[0]:= blk[0] - blk[3]; blk[0]:= LRot32(blk[0], 24);
  blk[3]:= blk[3] xor s_box[((blk[0] shr 24) and $FF) + 256];
  blk[2]:= blk[2]  -  s_box[ (blk[0] shr 16) and $FF];
  blk[1]:= blk[1]  -  s_box[((blk[0] shr  8) and $FF) + 256];
  blk[1]:= blk[1] xor s_box[  blk[0]         and $FF];
  blk[3]:= LRot32(blk[3], 24);
  blk[2]:= blk[2] xor s_box[((blk[3] shr 24) and $FF) + 256];
  blk[1]:= blk[1]  -  s_box[ (blk[3] shr 16) and $FF];
  blk[0]:= blk[0]  -  s_box[((blk[3] shr  8) and $FF) + 256];
  blk[0]:= blk[0] xor s_box[  blk[3]         and $FF];
  blk[2]:= LRot32(blk[2], 24);
  blk[1]:= blk[1] xor s_box[((blk[2] shr 24) and $FF) + 256];
  blk[0]:= blk[0]  -  s_box[ (blk[2] shr 16) and $FF];
  blk[3]:= blk[3]  -  s_box[((blk[2] shr  8) and $FF) + 256];
  blk[3]:= blk[3] xor s_box[  blk[2]         and $FF];
  blk[1]:= blk[1] - blk[2]; blk[1]:= LRot32(blk[1], 24);
  blk[0]:= blk[0] xor s_box[((blk[1] shr 24) and $FF) + 256];
  blk[3]:= blk[3]  -  s_box[ (blk[1] shr 16) and $FF];
  blk[2]:= blk[2]  -  s_box[((blk[1] shr  8) and $FF) + 256];
  blk[2]:= blk[2] xor s_box[  blk[1]         and $FF];
  blk[0]:= blk[0] - blk[3]; blk[0]:= LRot32(blk[0], 24);
  blk[3]:= blk[3] xor s_box[((blk[0] shr 24) and $FF) + 256];
  blk[2]:= blk[2]  -  s_box[ (blk[0] shr 16) and $FF];
  blk[1]:= blk[1]  -  s_box[((blk[0] shr  8) and $FF) + 256];
  blk[1]:= blk[1] xor s_box[  blk[0]         and $FF];
  blk[0]:= blk[0] - KeyData[0]; blk[1]:= blk[1] - KeyData[1];
  blk[2]:= blk[2] - KeyData[2]; blk[3]:= blk[3] - KeyData[3];

  PDWord(@OutData)^:= Blk[0];
  PDWord(longword(@OutData)+4)^:= Blk[1];
  PDWord(longword(@OutData)+8)^:= Blk[2];
  PDWord(longword(@OutData)+12)^:= Blk[3];
end;

end.
