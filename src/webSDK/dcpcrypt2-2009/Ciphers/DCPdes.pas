{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of DES and Triple DES *******************}
{* Based on C source code by Eric Young ***************************************}
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
{******************************************************************************}
{*       This implementation of DES is based on the C implementation by       *}
{*                       Eric Young (eay@mincom.oz.au)                        *}
{******************************************************************************}
{*    DES takes a 64bit key and discards every 8th bit (56bit effectively)    *}
{*    3DES takes either a <= 128bit key and uses one key twice or takes a     *}
{*     <= 192bit key and uses each once (again discarding every 8th bit)      *}
{******************************************************************************}
unit DCPdes;

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPconst, DCPblockciphers;

type
  TDCP_customdes= class(TDCP_blockcipher64)
  protected
    procedure DoInit(KeyB: PByteArray; KeyData: PDWordArray);
    procedure EncryptBlock(const InData; var OutData; KeyData: PDWordArray);
    procedure DecryptBlock(const InData; var OutData; KeyData: PDWordArray);
  end;

type
  TDCP_des= class(TDCP_customdes)
  protected
    KeyData: array[0..31] of dword;
    procedure InitKey(const Key; Size: longword); override;
  public
    class function GetId: integer; override;
    class function GetAlgorithm: string; override;
    class function GetMaxKeySize: integer; override;
    class function SelfTest: boolean; override;
    procedure Burn; override;
    procedure EncryptECB(const InData; var OutData); override;
    procedure DecryptECB(const InData; var OutData); override;
  end;

  TDCP_3des= class(TDCP_customdes)
  protected
    KeyData: array[0..2,0..31] of dword;
    procedure InitKey(const Key; Size: longword); override;
  public
    class function GetId: integer; override;
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

{$I DCPdes.inc}

procedure hperm_op(var a, t: dword; n, m: dword);
begin
  t:= ((a shl (16 - n)) xor a) and m;
  a:= a xor t xor (t shr (16 - n));
end;

procedure perm_op(var a, b, t: dword; n, m: dword);
begin
  t:= ((a shr n) xor b) and m;
  b:= b xor t;
  a:= a xor (t shl n);
end;

procedure TDCP_customdes.DoInit(KeyB: PByteArray; KeyData: PDwordArray);
var
  c, d, t, s, t2, i: dword;
begin
  c:= KeyB^[0] or (KeyB^[1] shl 8) or (KeyB^[2] shl 16) or (KeyB^[3] shl 24);
  d:= KeyB^[4] or (KeyB^[5] shl 8) or (KeyB^[6] shl 16) or (KeyB^[7] shl 24);
  perm_op(d,c,t,4,$0f0f0f0f);
  hperm_op(c,t,dword(-2),$cccc0000);
  hperm_op(d,t,dword(-2),$cccc0000);
  perm_op(d,c,t,1,$55555555);
  perm_op(c,d,t,8,$00ff00ff);
  perm_op(d,c,t,1,$55555555);
  d:= ((d and $ff) shl 16) or (d and $ff00) or ((d and $ff0000) shr 16) or
        ((c and $f0000000) shr 4);
  c:= c and $fffffff;
  for i:= 0 to 15 do
  begin
    if shifts2[i]<> 0 then
    begin
      c:= ((c shr 2) or (c shl 26));
      d:= ((d shr 2) or (d shl 26));
    end
    else
    begin
      c:= ((c shr 1) or (c shl 27));
      d:= ((d shr 1) or (d shl 27));
    end;
    c:= c and $fffffff;
    d:= d and $fffffff;
    s:= des_skb[0,c and $3f] or
        des_skb[1,((c shr  6) and $03) or ((c shr  7) and $3c)] or
        des_skb[2,((c shr 13) and $0f) or ((c shr 14) and $30)] or
        des_skb[3,((c shr 20) and $01) or ((c shr 21) and $06) or ((c shr 22) and $38)];
    t:= des_skb[4,d and $3f] or
        des_skb[5,((d shr  7) and $03) or ((d shr  8) and $3c)] or
        des_skb[6, (d shr 15) and $3f                         ] or
        des_skb[7,((d shr 21) and $0f) or ((d shr 22) and $30)];
    t2:= ((t shl 16) or (s and $ffff));
    KeyData^[(i shl 1)]:= ((t2 shl 2) or (t2 shr 30));
    t2:= ((s shr 16) or (t and $ffff0000));
    KeyData^[(i shl 1)+1]:= ((t2 shl 6) or (t2 shr 26));
  end;
end;

procedure TDCP_customdes.EncryptBlock(const InData; var OutData; KeyData: PDWordArray);
var
  l, r, t, u: dword;
  i: longint;
begin
  r:= PDword(@InData)^;
  l:= PDword(dword(@InData)+4)^;
  t:= ((l shr 4) xor r) and $0f0f0f0f;
  r:= r xor t;
  l:= l xor (t shl 4);
  t:= ((r shr 16) xor l) and $0000ffff;
  l:= l xor t;
  r:= r xor (t shl 16);
  t:= ((l shr 2) xor r) and $33333333;
  r:= r xor t;
  l:= l xor (t shl 2);
  t:= ((r shr 8) xor l) and $00ff00ff;
  l:= l xor t;
  r:= r xor (t shl 8);
  t:= ((l shr 1) xor r) and $55555555;
  r:= r xor t;
  l:= l xor (t shl 1);
  r:= (r shr 29) or (r shl 3);
  l:= (l shr 29) or (l shl 3);
  i:= 0;
  while i< 32 do
  begin
    u:= r xor KeyData^[i  ];
    t:= r xor KeyData^[i+1];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData^[i+2];
    t:= l xor KeyData^[i+3];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= r xor KeyData^[i+4];
    t:= r xor KeyData^[i+5];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData^[i+6];
    t:= l xor KeyData^[i+7];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    Inc(i,8);
  end;
  r:= (r shr 3) or (r shl 29);
  l:= (l shr 3) or (l shl 29);
  t:= ((r shr 1) xor l) and $55555555;
  l:= l xor t;
  r:= r xor (t shl 1);
  t:= ((l shr 8) xor r) and $00ff00ff;
  r:= r xor t;
  l:= l xor (t shl 8);
  t:= ((r shr 2) xor l) and $33333333;
  l:= l xor t;
  r:= r xor (t shl 2);
  t:= ((l shr 16) xor r) and $0000ffff;
  r:= r xor t;
  l:= l xor (t shl 16);
  t:= ((r shr 4) xor l) and $0f0f0f0f;
  l:= l xor t;
  r:= r xor (t shl 4);
  PDword(@OutData)^:= l;
  PDword(dword(@OutData)+4)^:= r;
end;

procedure TDCP_customdes.DecryptBlock(const InData; var OutData; KeyData: PDWordArray);
var
  l, r, t, u: dword;
  i: longint;
begin
  r:= PDword(@InData)^;
  l:= PDword(dword(@InData)+4)^;
  t:= ((l shr 4) xor r) and $0f0f0f0f;
  r:= r xor t;
  l:= l xor (t shl 4);
  t:= ((r shr 16) xor l) and $0000ffff;
  l:= l xor t;
  r:= r xor (t shl 16);
  t:= ((l shr 2) xor r) and $33333333;
  r:= r xor t;
  l:= l xor (t shl 2);
  t:= ((r shr 8) xor l) and $00ff00ff;
  l:= l xor t;
  r:= r xor (t shl 8);
  t:= ((l shr 1) xor r) and $55555555;
  r:= r xor t;
  l:= l xor (t shl 1);
  r:= (r shr 29) or (r shl 3);
  l:= (l shr 29) or (l shl 3);
  i:= 30;
  while i> 0 do
  begin
    u:= r xor KeyData^[i  ];
    t:= r xor KeyData^[i+1];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData^[i-2];
    t:= l xor KeyData^[i-1];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= r xor KeyData^[i-4];
    t:= r xor KeyData^[i-3];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData^[i-6];
    t:= l xor KeyData^[i-5];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    Dec(i,8);
  end;
  r:= (r shr 3) or (r shl 29);
  l:= (l shr 3) or (l shl 29);
  t:= ((r shr 1) xor l) and $55555555;
  l:= l xor t;
  r:= r xor (t shl 1);
  t:= ((l shr 8) xor r) and $00ff00ff;
  r:= r xor t;
  l:= l xor (t shl 8);
  t:= ((r shr 2) xor l) and $33333333;
  l:= l xor t;
  r:= r xor (t shl 2);
  t:= ((l shr 16) xor r) and $0000ffff;
  r:= r xor t;
  l:= l xor (t shl 16);
  t:= ((r shr 4) xor l) and $0f0f0f0f;
  l:= l xor t;
  r:= r xor (t shl 4);
  PDword(@OutData)^:= l;
  PDword(dword(@OutData)+4)^:= r;
end;

class function TDCP_des.GetMaxKeySize: integer;
begin
  Result:= 64;
end;

class function TDCP_des.GetID: integer;
begin
  Result:= DCP_des;
end;

class function TDCP_des.GetAlgorithm: string;
begin
  Result:= 'DES';
end;

class function TDCP_des.SelfTest: boolean;
const
  InData1: array[0..7] of byte=
    ($07,$56,$D8,$E0,$77,$47,$61,$D2);
  OutData1: array[0..7] of byte=
    ($0C,$D3,$DA,$02,$00,$21,$DC,$09);
  Key1: array[0..7] of byte=
    ($01,$70,$F1,$75,$46,$8F,$B5,$E6);
  InData2: array[0..7] of byte=
    ($48,$0D,$39,$00,$6E,$E7,$62,$F2);
  OutData2: array[0..7] of byte=
    ($A1,$F9,$91,$55,$41,$02,$0B,$56);
  Key2: array[0..7] of byte=
    ($02,$58,$16,$16,$46,$29,$B0,$07);
var
  Cipher: TDCP_des;
  Data: array[0..7] of byte;
begin
  Cipher:= TDCP_des.Create(nil);
  Cipher.Init(Key1,Sizeof(Key1)*8,nil);
  Cipher.EncryptECB(InData1,Data);
  Result:= boolean(CompareMem(@Data,@OutData1,Sizeof(Data)));
  Cipher.DecryptECB(Data,Data);
  Result:= Result and boolean(CompareMem(@Data,@InData1,Sizeof(Data)));
  Cipher.Burn;
  Cipher.Init(Key2,Sizeof(Key2)*8,nil);
  Cipher.EncryptECB(InData2,Data);
  Result:= Result and boolean(CompareMem(@Data,@OutData2,Sizeof(Data)));
  Cipher.DecryptECB(Data,Data);
  Result:= Result and boolean(CompareMem(@Data,@InData2,Sizeof(Data)));
  Cipher.Burn;
  Cipher.Free;
end;

procedure TDCP_des.InitKey(const Key; Size: longword);
var
  KeyB: array[0..7] of byte;
begin
  FillChar(KeyB,Sizeof(KeyB),0);
  Move(Key,KeyB,Size div 8);
  DoInit(@KeyB,@KeyData);
end;

procedure TDCP_des.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),0);
  inherited Burn;
end;

procedure TDCP_des.EncryptECB(const InData; var OutData);
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  EncryptBlock(InData,OutData,@KeyData);
end;

procedure TDCP_des.DecryptECB(const InData; var OutData);
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  DecryptBlock(InData,OutData,@KeyData);
end;

{******************************************************************************}
class function TDCP_3des.GetMaxKeySize: integer;
begin
  Result:= 192;
end;

class function TDCP_3des.GetID: integer;
begin
  Result:= DCP_3des;
end;

class function TDCP_3des.GetAlgorithm: string;
begin
  Result:= '3DES';
end;

class function TDCP_3des.SelfTest: boolean;
const
  Key: array[0..23] of byte=
    ($01,$23,$45,$67,$89,$ab,$cd,$ef,$fe,$dc,$ba,$98,
     $76,$54,$32,$10,$89,$ab,$cd,$ef,$01,$23,$45,$67);
  PlainText: array[0..7] of byte=
    ($01,$23,$45,$67,$89,$ab,$cd,$e7);
  CipherText: array[0..7] of byte=
    ($de,$0b,$7c,$06,$ae,$5e,$0e,$d5);
var
  Cipher: TDCP_3des;
  Block: array[0..7] of byte;
begin
  Cipher:= TDCP_3des.Create(nil);
  Cipher.Init(Key,Sizeof(Key)*8,nil);
  Cipher.EncryptECB(PlainText,Block);
  Result:= CompareMem(@Block,@CipherText,Sizeof(CipherText));
  Cipher.DecryptECB(Block,Block);
  Result:= Result and CompareMem(@Block,@PlainText,Sizeof(PlainText));
  Cipher.Free;
end;

procedure TDCP_3des.InitKey(const Key; Size: longword);
var
  KeyB: array[0..2,0..7] of byte;
begin
  FillChar(KeyB,Sizeof(KeyB),0);
  Move(Key,KeyB,Size div 8);
  DoInit(@KeyB[0],@KeyData[0]);
  DoInit(@KeyB[1],@KeyData[1]);
  if Size> 128 then
    DoInit(@KeyB[2],@KeyData[2])
  else
    Move(KeyData[0],KeyData[2],128);
end;

procedure TDCP_3des.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),0);
  inherited Burn;
end;

procedure TDCP_3des.EncryptECB(const InData; var OutData);
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  EncryptBlock(InData,OutData,@KeyData[0]);
  DecryptBlock(OutData,OutData,@KeyData[1]);
  EncryptBlock(OutData,OutData,@KeyData[2]);
end;

procedure TDCP_3des.DecryptECB(const InData; var OutData);
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  DecryptBlock(InData,OutData,@KeyData[2]);
  EncryptBlock(OutData,OutData,@KeyData[1]);
  DecryptBlock(OutData,OutData,@KeyData[0]);
end;


end.
