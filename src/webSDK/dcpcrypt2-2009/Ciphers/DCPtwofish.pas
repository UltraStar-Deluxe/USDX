{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of Twofish ******************************}
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
unit DCPtwofish;

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPconst, DCPblockciphers;

const
  INPUTWHITEN= 0;
  OUTPUTWHITEN= 4;
  NUMROUNDS= 16;
  ROUNDSUBKEYS= (OUTPUTWHITEN + 4);
  TOTALSUBKEYS= (ROUNDSUBKEYS + NUMROUNDS * 2);
  RS_GF_FDBK= $14d;
  MDS_GF_FDBK= $169;
  SK_STEP= $02020202;
  SK_BUMP= $01010101;
  SK_ROTL= 9;

type
  TDCP_twofish= class(TDCP_blockcipher128)
  protected
    SubKeys: array[0..TOTALSUBKEYS-1] of DWord;
    sbox: array[0..3,0..255] of DWord;
    procedure InitKey(const Key; Size: longword); override;
  public
    class function GetID: integer; override;
    class function GetAlgorithm: string; override;
    class function GetMaxKeySize: integer; override;
    class function SelfTest: boolean; override;
    procedure Burn; override;
    procedure EncryptECB(const InData; var OutData); override;
    procedure DecryptECB(const InData; var OutData); override;
    constructor Create(AOwner: TComponent); override;
  end;


{******************************************************************************}
{******************************************************************************}
implementation
{$R-}{$Q-}
{$I DCPtwofish.inc}

var
  MDS: array[0..3,0..255] of dword;
  MDSDone: boolean;

class function TDCP_twofish.GetID: integer;
begin
  Result:= DCP_twofish;
end;

class function TDCP_twofish.GetAlgorithm: string;
begin
  Result:= 'Twofish';
end;

class function TDCP_twofish.GetMaxKeySize: integer;
begin
  Result:= 256;
end;

class function TDCP_twofish.SelfTest: boolean;
const
  Out128: array[0..15] of byte=
    ($5D,$9D,$4E,$EF,$FA,$91,$51,$57,$55,$24,$F1,$15,$81,$5A,$12,$E0);
  Out192: array[0..15] of byte=
    ($E7,$54,$49,$21,$2B,$EE,$F9,$F4,$A3,$90,$BD,$86,$0A,$64,$09,$41);
  Out256: array[0..15] of byte=
    ($37,$FE,$26,$FF,$1C,$F6,$61,$75,$F5,$DD,$F4,$C3,$3B,$97,$A2,$05);
var
  i: integer;
  Key: array[0..31] of byte;
  Block: array[0..15] of byte;
  Cipher: TDCP_twofish;
begin
  Cipher:= TDCP_twofish.Create(nil);
  FillChar(Key,Sizeof(Key),0);
  FillChar(Block,Sizeof(Block),0);
  for i:= 1 to 49 do
  begin
    Cipher.Init(Key,128,nil);
    Move(Block,Key,16);
    Cipher.EncryptECB(Block,Block);
    Cipher.Burn;
  end;
  Result:= boolean(CompareMem(@Block,@Out128,16));
  FillChar(Key,Sizeof(Key),0);
  FillChar(Block,Sizeof(Block),0);
  for i:= 1 to 49 do
  begin
    Cipher.Init(Key,192,nil);
    Move(Key[0],Key[16],8);
    Move(Block,Key,16);
    Cipher.EncryptECB(Block,Block);
    Cipher.Burn;
  end;
  Result:= Result and boolean(CompareMem(@Block,@Out192,16));
  FillChar(Key,Sizeof(Key),0);
  FillChar(Block,Sizeof(Block),0);
  for i:= 1 to 49 do
  begin
    Cipher.Init(Key,256,nil);
    Move(Key[0],Key[16],16);
    Move(Block,Key,16);
    Cipher.EncryptECB(Block,Block);
    Cipher.Burn;
  end;
  Result:= Result and boolean(CompareMem(@Block,@Out256,16));
  Cipher.Burn;
  Cipher.Free;
end;

function LFSR1(x: DWord): DWord;
begin
  if (x and 1)<> 0 then
    Result:= (x shr 1) xor (MDS_GF_FDBK div 2)
  else
    Result:= (x shr 1);
end;
function LFSR2(x: DWord): DWord;
begin
  if (x and 2)<> 0 then
    if (x and 1)<> 0 then
      Result:= (x shr 2) xor (MDS_GF_FDBK div 2) xor (MDS_GF_FDBK div 4)
    else
      Result:= (x shr 2) xor (MDS_GF_FDBK div 2)
  else
    if (x and 1)<> 0 then
      Result:= (x shr 2) xor (MDS_GF_FDBK div 4)
    else
      Result:= (x shr 2);
end;
function Mul_X(x: DWord): DWord;
begin
  Result:= x xor LFSR2(x);
end;
function Mul_Y(x: DWord): DWord;
begin
  Result:= x xor LFSR1(x) xor LFSR2(x);
end;

function RS_MDS_Encode(lK0, lK1: DWord): DWord;
var
  lR, nJ, lG2, lG3: DWord;
  bB: byte;
begin
  lR:= lK1;
  for nJ:= 0 to 3 do
  begin
    bB:= lR shr 24;
    if (bB and $80)<> 0 then
      lG2:= ((bB shl 1) xor RS_GF_FDBK) and $FF
    else
      lG2:= (bB shl 1) and $FF;
    if (bB and 1)<> 0 then
      lG3:= ((bB shr 1) and $7f) xor (RS_GF_FDBK shr 1) xor lG2
    else
      lG3:= ((bB shr 1) and $7f) xor lG2;
    lR:= (lR shl 8) xor (lG3 shl 24) xor (lG2 shl 16) xor (lG3 shl 8) xor bB;
  end;
  lR:= lR xor lK0;
  for nJ:= 0 to 3 do
  begin
    bB:= lR shr 24;
    if (bB and $80)<> 0 then
      lG2:= ((bB shl 1) xor RS_GF_FDBK) and $FF
    else
      lG2:= (bB shl 1) and $FF;
    if (bB and 1)<> 0 then
      lG3:= ((bB shr 1) and $7f) xor (RS_GF_FDBK shr 1) xor lG2
    else
      lG3:= ((bB shr 1) and $7f) xor lG2;
    lR:= (lR shl 8) xor (lG3 shl 24) xor (lG2 shl 16) xor (lG3 shl 8) xor bB;
  end;
  Result:= lR;
end;

function f32(x: DWord; K32: PDWordArray; Len: DWord): DWord;
var
  t0, t1, t2, t3: DWord;
begin
  t0:= x and $FF;
  t1:= (x shr 8) and $FF;
  t2:= (x shr 16) and $FF;
  t3:= x shr 24;
  if Len= 256 then
  begin
    t0:= p8x8[1,t0] xor ((K32^[3]) and $FF);
    t1:= p8x8[0,t1] xor ((K32^[3] shr  8) and $FF);
    t2:= p8x8[0,t2] xor ((K32^[3] shr 16) and $FF);
    t3:= p8x8[1,t3] xor ((K32^[3] shr 24));
  end;
  if Len>= 192 then
  begin
    t0:= p8x8[1,t0] xor ((K32^[2]) and $FF);
    t1:= p8x8[1,t1] xor ((K32^[2] shr  8) and $FF);
    t2:= p8x8[0,t2] xor ((K32^[2] shr 16) and $FF);
    t3:= p8x8[0,t3] xor ((K32^[2] shr 24));
  end;
  Result:= MDS[0,p8x8[0,p8x8[0,t0] xor ((K32^[1]) and $FF)] xor ((K32^[0]) and $FF)] xor
           MDS[1,p8x8[0,p8x8[1,t1] xor ((K32^[1] shr  8) and $FF)] xor ((K32^[0] shr  8) and $FF)] xor
           MDS[2,p8x8[1,p8x8[0,t2] xor ((K32^[1] shr 16) and $FF)] xor ((K32^[0] shr 16) and $FF)] xor
           MDS[3,p8x8[1,p8x8[1,t3] xor ((K32^[1] shr 24))] xor ((K32^[0] shr 24))];
end;

procedure Xor256(Dst, Src: PDWordArray; v: byte);
var
  i, j: DWord;
begin
  i:= 0;
  j:= v * $01010101;
  while i< 64 do
  begin
    Dst^[i]:= Src^[i] xor j;
    Dst^[i+1]:= Src^[i+1] xor j;
    Dst^[i+2]:= Src^[i+2] xor j;
    Dst^[i+3]:= Src^[i+3] xor j;
    Inc(i,4);
  end;
end;

procedure TDCP_twofish.InitKey(const Key; Size: longword);
const
  subkeyCnt= ROUNDSUBKEYS + 2*NUMROUNDS;
var
  key32: array[0..7] of DWord;
  k32e, k32o, sboxkeys: array[0..3] of DWord;
  k64Cnt, i, j, A, B, q: DWord;
  L0, L1: array[0..255] of byte;
begin
  FillChar(Key32,Sizeof(Key32),0);
  Move(Key,Key32,Size div 8);
  if Size<= 128 then           { pad the key to either 128bit, 192bit or 256bit}
    Size:= 128
  else if Size<= 192 then
    Size:= 192
  else
    Size:= 256;
  k64Cnt:= Size div 64;
  j:= k64Cnt-1;
  for i:= 0 to j do
  begin
    k32e[i]:= key32[2*i];
    k32o[i]:= key32[2*i+1];
    sboxKeys[j]:= RS_MDS_Encode(k32e[i],k32o[i]);
    Dec(j);
  end;
  q:= 0;
  for i:= 0 to ((subkeyCnt div 2)-1) do
  begin
    A:= f32(q,@k32e,Size);
    B:= f32(q+SK_BUMP,@k32o,Size);
    B:= (B shl 8) or (B shr 24);
    SubKeys[2*i]:= A+B;
    B:= A + 2*B;
    SubKeys[2*i+1]:= (B shl SK_ROTL) or (B shr (32 - SK_ROTL)); 
    Inc(q,SK_STEP);
  end;
  case Size of
    128: begin
           Xor256(@L0,@p8x8[0],(sboxKeys[1] and $FF));
           A:= (sboxKeys[0] and $FF);
           i:= 0;
           while i< 256 do
           begin
             sBox[0 and 2,2*i+(0 and 1)]:= MDS[0,p8x8[0,L0[i]] xor A];
             sBox[0 and 2,2*i+(0 and 1)+2]:= MDS[0,p8x8[0,L0[i+1]] xor A];
             Inc(i,2);
           end;
           Xor256(@L0,@p8x8[1],(sboxKeys[1] shr 8) and $FF);
           A:= (sboxKeys[0] shr 8) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[1 and 2,2*i+(1 and 1)]:= MDS[1,p8x8[0,L0[i]] xor A];
             sBox[1 and 2,2*i+(1 and 1)+2]:= MDS[1,p8x8[0,L0[i+1]] xor A];
             Inc(i,2);
           end;
           Xor256(@L0,@p8x8[0],(sboxKeys[1] shr 16) and $FF);
           A:= (sboxKeys[0] shr 16) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[2 and 2,2*i+(2 and 1)]:= MDS[2,p8x8[1,L0[i]] xor A];
             sBox[2 and 2,2*i+(2 and 1)+2]:= MDS[2,p8x8[1,L0[i+1]] xor A];
             Inc(i,2);
           end;
           Xor256(@L0,@p8x8[1],(sboxKeys[1] shr 24));
           A:= (sboxKeys[0] shr 24);
           i:= 0;
           while i< 256 do
           begin
             sBox[3 and 2,2*i+(3 and 1)]:= MDS[3,p8x8[1,L0[i]] xor A];
             sBox[3 and 2,2*i+(3 and 1)+2]:= MDS[3,p8x8[1,L0[i+1]] xor A];
             Inc(i,2);
           end;
         end;
    192: begin
           Xor256(@L0,@p8x8[1],sboxKeys[2] and $FF);
           A:= sboxKeys[0] and $FF;
           B:= sboxKeys[1] and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[0 and 2,2*i+(0 and 1)]:= MDS[0,p8x8[0,p8x8[0,L0[i]] xor B] xor A];
             sBox[0 and 2,2*i+(0 and 1)+2]:= MDS[0,p8x8[0,p8x8[0,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(@L0,@p8x8[1],(sboxKeys[2] shr 8) and $FF);
           A:= (sboxKeys[0] shr 8) and $FF;
           B:= (sboxKeys[1] shr 8) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[1 and 2,2*i+(1 and 1)]:= MDS[1,p8x8[0,p8x8[1,L0[i]] xor B] xor A];
             sBox[1 and 2,2*i+(1 and 1)+2]:= MDS[1,p8x8[0,p8x8[1,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(@L0,@p8x8[0],(sboxKeys[2] shr 16) and $FF);
           A:= (sboxKeys[0] shr 16) and $FF;
           B:= (sboxKeys[1] shr 16) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[2 and 2,2*i+(2 and 1)]:= MDS[2,p8x8[1,p8x8[0,L0[i]] xor B] xor A];
             sBox[2 and 2,2*i+(2 and 1)+2]:= MDS[2,p8x8[1,p8x8[0,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(@L0,@p8x8[0],(sboxKeys[2] shr 24));
           A:= (sboxKeys[0] shr 24);
           B:= (sboxKeys[1] shr 24);
           i:= 0;
           while i< 256 do
           begin
             sBox[3 and 2,2*i+(3 and 1)]:= MDS[3,p8x8[1,p8x8[1,L0[i]] xor B] xor A];
             sBox[3 and 2,2*i+(3 and 1)+2]:= MDS[3,p8x8[1,p8x8[1,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
         end;
    256: begin
           Xor256(@L1,@p8x8[1],(sboxKeys[3]) and $FF);
           i:= 0;
           while i< 256 do
           begin
             L0[i  ]:= p8x8[1,L1[i]];
             L0[i+1]:= p8x8[1,L1[i+1]];
             Inc(i,2);
           end;
           Xor256(@L0,@L0,(sboxKeys[2]) and $FF);
           A:= (sboxKeys[0]) and $FF;
           B:= (sboxKeys[1]) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[0 and 2,2*i+(0 and 1)]:= MDS[0,p8x8[0,p8x8[0,L0[i]] xor B] xor A];
             sBox[0 and 2,2*i+(0 and 1)+2]:= MDS[0,p8x8[0,p8x8[0,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(@L1,@p8x8[0],(sboxKeys[3] shr  8) and $FF);
           i:= 0;
           while i< 256 do
           begin
             L0[i  ]:= p8x8[1,L1[i]];
             L0[i+1]:= p8x8[1,L1[i+1]];
             Inc(i,2);
           end;
           Xor256(@L0,@L0,(sboxKeys[2] shr  8) and $FF);
           A:= (sboxKeys[0] shr  8) and $FF;
           B:= (sboxKeys[1] shr  8) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[1 and 2,2*i+(1 and 1)]:= MDS[1,p8x8[0,p8x8[1,L0[i]] xor B] xor A];
             sBox[1 and 2,2*i+(1 and 1)+2]:= MDS[1,p8x8[0,p8x8[1,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;

           Xor256(@L1,@p8x8[0],(sboxKeys[3] shr 16) and $FF);
           i:= 0;
           while i< 256 do
           begin
             L0[i  ]:= p8x8[0,L1[i]];
             L0[i+1]:= p8x8[0,L1[i+1]];
             Inc(i,2);
           end;
           Xor256(@L0,@L0,(sboxKeys[2] shr 16) and $FF);
           A:= (sboxKeys[0] shr 16) and $FF;
           B:= (sboxKeys[1] shr 16) and $FF;
           i:= 0;
           while i< 256 do
           begin
             sBox[2 and 2,2*i+(2 and 1)]:= MDS[2,p8x8[1,p8x8[0,L0[i]] xor B] xor A];
             sBox[2 and 2,2*i+(2 and 1)+2]:= MDS[2,p8x8[1,p8x8[0,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
           Xor256(@L1,@p8x8[1],(sboxKeys[3] shr 24));
           i:= 0;
           while i< 256 do
           begin
             L0[i  ]:= p8x8[0,L1[i]];
             L0[i+1]:= p8x8[0,L1[i+1]];
             Inc(i,2);
           end;
           Xor256(@L0,@L0,(sboxKeys[2] shr 24));
           A:= (sboxKeys[0] shr 24);
           B:= (sboxKeys[1] shr 24);
           i:= 0;
           while i< 256 do
           begin
             sBox[3 and 2,2*i+(3 and 1)]:= MDS[3,p8x8[1,p8x8[1,L0[i]] xor B] xor A];
             sBox[3 and 2,2*i+(3 and 1)+2]:= MDS[3,p8x8[1,p8x8[1,L0[i+1]] xor B] xor A];
             Inc(i,2);
           end;
         end;
  end;
end;

procedure TDCP_twofish.Burn;
begin
  FillChar(sBox,Sizeof(sBox),$FF);
  FillChar(SubKeys,Sizeof(SubKeys),$FF);
  inherited Burn;
end;

procedure TDCP_twofish.EncryptECB(const InData; var OutData);
var
  i: longword;
  t0, t1: DWord;
  X: array[0..3] of DWord;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  x[0]:= PDWord(@InData)^ xor SubKeys[INPUTWHITEN];
  x[1]:= PDWord(longword(@InData)+4)^ xor SubKeys[INPUTWHITEN+1];
  x[2]:= PDWord(longword(@InData)+8)^ xor SubKeys[INPUTWHITEN+2];
  x[3]:= PDWord(longword(@InData)+12)^ xor SubKeys[INPUTWHITEN+3];
  i:= 0;
  while i<= NUMROUNDS-2 do
  begin
    t0:= sBox[0,(x[0] shl 1) and $1fe] xor sBox[0,((x[0] shr 7) and $1fe)+1]
      xor sBox[2,(x[0] shr 15) and $1fe] xor sBox[2,((x[0] shr 23) and $1fe)+1];
    t1:= sBox[0,((x[1] shr 23) and $1fe)] xor sBox[0,((x[1] shl 1) and $1fe)+1]
      xor sBox[2,((x[1] shr 7) and $1fe)] xor sBox[2,((x[1] shr 15) and $1fe)+1];
    x[3]:= (x[3] shl 1) or (x[3] shr 31);
    x[2]:= x[2] xor (t0 +   t1 + SubKeys[ROUNDSUBKEYS+2*i]);
    x[3]:= x[3] xor (t0 + 2*t1 + SubKeys[ROUNDSUBKEYS+2*i+1]);
    x[2]:= (x[2] shr 1) or (x[2] shl 31);

    t0:= sBox[0,(x[2] shl 1) and $1fe] xor sBox[0,((x[2] shr 7) and $1fe)+1]
      xor sBox[2,((x[2] shr 15) and $1fe)] xor sBox[2,((x[2] shr 23) and $1fe)+1];
    t1:= sBox[0,((x[3] shr 23) and $1fe)] xor sBox[0,((x[3] shl 1) and $1fe)+1]
      xor sBox[2,((x[3] shr 7) and $1fe)] xor sBox[2,((x[3] shr 15) and $1fe)+1];
    x[1]:= (x[1] shl 1) or (x[1] shr 31);
    x[0]:= x[0] xor (t0 +   t1 + SubKeys[ROUNDSUBKEYS+2*(i+1)]);
    x[1]:= x[1] xor (t0 + 2*t1 + SubKeys[ROUNDSUBKEYS+2*(i+1)+1]);
    x[0]:= (x[0] shr 1) or (x[0] shl 31);
    Inc(i,2);
  end;
  PDWord(longword(@OutData)+ 0)^:= x[2] xor SubKeys[OUTPUTWHITEN];
  PDWord(longword(@OutData)+ 4)^:= x[3] xor SubKeys[OUTPUTWHITEN+1];
  PDWord(longword(@OutData)+ 8)^:= x[0] xor SubKeys[OUTPUTWHITEN+2];
  PDWord(longword(@OutData)+12)^:= x[1] xor SubKeys[OUTPUTWHITEN+3];
end;

procedure TDCP_twofish.DecryptECB(const InData; var OutData);
var
  i: integer;
  t0, t1: DWord;
  X: array[0..3] of DWord;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  X[2]:= PDWord(@InData)^ xor SubKeys[OUTPUTWHITEN];
  X[3]:= PDWord(longword(@InData)+4)^ xor SubKeys[OUTPUTWHITEN+1];
  X[0]:= PDWord(longword(@InData)+8)^ xor SubKeys[OUTPUTWHITEN+2];
  X[1]:= PDWord(longword(@InData)+12)^ xor SubKeys[OUTPUTWHITEN+3];
  i:= NUMROUNDS-2;
  while i>= 0 do
  begin
    t0:= sBox[0,(x[2] shl 1) and $1fe] xor sBox[0,((x[2] shr 7) and $1fe)+1]
      xor sBox[2,((x[2] shr 15) and $1fe)] xor sBox[2,((x[2] shr 23) and $1fe)+1];
    t1:= sBox[0,((x[3] shr 23) and $1fe)] xor sBox[0,((x[3] shl 1) and $1fe)+1]
      xor sBox[2,((x[3] shr 7) and $1fe)] xor sBox[2,((x[3] shr 15) and $1fe)+1];
    x[0]:= (x[0] shl 1) or (x[0] shr 31);
    x[0]:= x[0] xor (t0 +   t1 + SubKeys[ROUNDSUBKEYS+2*(i+1)]);
    x[1]:= x[1] xor (t0 + 2*t1 + SubKeys[ROUNDSUBKEYS+2*(i+1)+1]);
    x[1]:= (x[1] shr 1) or (x[1] shl 31);

    t0:= sBox[0,(x[0] shl 1) and $1fe] xor sBox[0,((x[0] shr 7) and $1fe)+1]
      xor sBox[2,(x[0] shr 15) and $1fe] xor sBox[2,((x[0] shr 23) and $1fe)+1];
    t1:= sBox[0,((x[1] shr 23) and $1fe)] xor sBox[0,((x[1] shl 1) and $1fe)+1]
      xor sBox[2,((x[1] shr 7) and $1fe)] xor sBox[2,((x[1] shr 15) and $1fe)+1];
    x[2]:= (x[2] shl 1) or (x[2] shr 31);
    x[2]:= x[2] xor (t0 +   t1 + SubKeys[ROUNDSUBKEYS+2*i]);
    x[3]:= x[3] xor (t0 + 2*t1 + SubKeys[ROUNDSUBKEYS+2*i+1]);
    x[3]:= (x[3] shr 1) or (x[3] shl 31);
    Dec(i,2);
  end;
  PDWord(longword(@OutData)+ 0)^:= X[0] xor SubKeys[INPUTWHITEN];
  PDWord(longword(@OutData)+ 4)^:= X[1] xor SubKeys[INPUTWHITEN+1];
  PDWord(longword(@OutData)+ 8)^:= X[2] xor SubKeys[INPUTWHITEN+2];
  PDWord(longword(@OutData)+12)^:= X[3] xor SubKeys[INPUTWHITEN+3];
end;

procedure PreCompMDS;
var
  m1, mx, my: array[0..1] of DWord;
  nI: longword;
begin
  for nI:= 0 to 255 do
  begin
    m1[0]:= p8x8[0,nI];
    mx[0]:= Mul_X(m1[0]);
    my[0]:= Mul_Y(m1[0]);
    m1[1]:= p8x8[1,nI];
    mx[1]:= Mul_X(m1[1]);
    my[1]:= Mul_Y(m1[1]);
    mds[0,nI]:= (m1[1] shl 0) or
                (mx[1] shl 8) or
                (my[1] shl 16) or
                (my[1] shl 24);
    mds[1,nI]:= (my[0] shl 0) or
                (my[0] shl 8) or
                (mx[0] shl 16) or
                (m1[0] shl 24);
    mds[2,nI]:= (mx[1] shl 0) or
                (my[1] shl 8) or
                (m1[1] shl 16) or
                (my[1] shl 24);
    mds[3,nI]:= (mx[0] shl 0) or
                (m1[0] shl 8) or
                (my[0] shl 16) or
                (mx[0] shl 24);
  end;
end;

constructor TDCP_twofish.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not MDSDone then
  begin
    PreCompMDS;
    MDSDone:= true;
  end;
end;

initialization
  MDSdone:= false;

end.
