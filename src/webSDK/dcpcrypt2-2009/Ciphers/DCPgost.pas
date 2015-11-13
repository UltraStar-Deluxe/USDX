{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of Gost *********************************}
{******************************************************************************}
{******************************************************************************}
{* !!WARNING!!  !!WARNING!!  !!WARNING!!!!WARNING!!  !!WARNING!!  !!WARNING!! *}
{*                                                                            *}
{*     I am not sure if this is compatible with the actual GOST standard      *}
{*         as a result, use this implementation with extreme caution.         *}
{*                                                                            *}
{* !!WARNING!!  !!WARNING!!  !!WARNING!!!!WARNING!!  !!WARNING!!  !!WARNING!! *}
{******************************************************************************}
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
unit DCPgost;

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPconst, DCPblockciphers;

type
  TDCP_gost= class(TDCP_blockcipher64)
  protected
    KeyData: array[0..7] of DWord;
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
{$I DCPgost.inc}

class function TDCP_gost.GetMaxKeySize: integer;
begin
  Result:= 256;
end;

class function TDCP_gost.GetID: integer;
begin
  Result:= DCP_gost;
end;

class function TDCP_gost.GetAlgorithm: string;
begin
  Result:= 'Gost';
end;

class function TDCP_gost.SelfTest: boolean;
const
  Key1: array[0..31] of byte=
    ($BE,$5E,$C2,$00,$6C,$FF,$9D,$CF,$52,$35,$49,$59,$F1,$FF,$0C,$BF,
     $E9,$50,$61,$B5,$A6,$48,$C1,$03,$87,$06,$9C,$25,$99,$7C,$06,$72);
  InData1: array[0..7] of byte=
    ($0D,$F8,$28,$02,$B7,$41,$A2,$92);
  OutData1: array[0..7] of byte=
    ($07,$F9,$02,$7D,$F7,$F7,$DF,$89);
  Key2: array[0..31] of byte=
    ($B3,$85,$27,$2A,$C8,$D7,$2A,$5A,$8B,$34,$4B,$C8,$03,$63,$AC,$4D,
     $09,$BF,$58,$F4,$1F,$54,$06,$24,$CB,$CB,$8F,$DC,$F5,$53,$07,$D7);
  InData2: array[0..7] of byte=
    ($13,$54,$EE,$9C,$0A,$11,$CD,$4C);
  OutData2: array[0..7] of byte=
    ($4F,$B5,$05,$36,$F9,$60,$A7,$B1);
var
  Block: array[0..7] of byte;
  Cipher: TDCP_gost;
begin
  Cipher:= TDCP_gost.Create(nil);
  Cipher.Init(Key1,Sizeof(Key1)*8,nil);
  Cipher.EncryptECB(InData1,Block);
  Result:= boolean(CompareMem(@Block,@OutData1,8));
  Cipher.DecryptECB(Block,Block);
  Cipher.Burn;
  Result:= Result and boolean(CompareMem(@Block,@InData1,8));
  Cipher.Init(Key2,Sizeof(Key2)*8,nil);
  Cipher.EncryptECB(InData2,Block);
  Result:= Result and boolean(CompareMem(@Block,@OutData2,8));
  Cipher.DecryptECB(Block,Block);
  Cipher.Burn;
  Result:= Result and boolean(CompareMem(@Block,@InData2,8));
  Cipher.Free;
end;

procedure TDCP_gost.InitKey(const Key; Size: longword);
var
  i: longword;
  userkey: array[0..31] of byte;
begin
  Size:= Size div 8;

  FillChar(userkey,Sizeof(userkey),0);
  Move(Key,userkey,Size);
  for i:= 0 to 7 do
    KeyData[i]:= (dword(UserKey[4*i+3]) shl 24) or (dword(UserKey[4*i+2]) shl 16) or
      (dword(UserKey[4*i+1]) shl 8) or (dword(UserKey[4*i+0]));
end;

procedure TDCP_gost.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),0);
  inherited Burn;
end;

procedure TDCP_gost.EncryptECB(const InData; var OutData);
var
  n1, n2: DWord;
  i: longword;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  n1:= PDword(@InData)^;
  n2:= PDword(dword(@InData)+4)^;
  for i:= 0 to 2 do
  begin
    n2:= n2 xor (sTable[3,(n1+KeyData[0]) shr 24] xor sTable[2,((n1+KeyData[0]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[0]) shr 8) and $FF] xor sTable[0,(n1+KeyData[0]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[1]) shr 24] xor sTable[2,((n2+KeyData[1]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[1]) shr 8) and $FF] xor sTable[0,(n2+KeyData[1]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[2]) shr 24] xor sTable[2,((n1+KeyData[2]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[2]) shr 8) and $FF] xor sTable[0,(n1+KeyData[2]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[3]) shr 24] xor sTable[2,((n2+KeyData[3]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[3]) shr 8) and $FF] xor sTable[0,(n2+KeyData[3]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[4]) shr 24] xor sTable[2,((n1+KeyData[4]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[4]) shr 8) and $FF] xor sTable[0,(n1+KeyData[4]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[5]) shr 24] xor sTable[2,((n2+KeyData[5]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[5]) shr 8) and $FF] xor sTable[0,(n2+KeyData[5]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[6]) shr 24] xor sTable[2,((n1+KeyData[6]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[6]) shr 8) and $FF] xor sTable[0,(n1+KeyData[6]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[7]) shr 24] xor sTable[2,((n2+KeyData[7]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[7]) shr 8) and $FF] xor sTable[0,(n2+KeyData[7]) and $FF]);
  end;
  n2:= n2 xor (sTable[3,(n1+KeyData[7]) shr 24] xor sTable[2,((n1+KeyData[7]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[7]) shr 8) and $FF] xor sTable[0,(n1+KeyData[7]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[6]) shr 24] xor sTable[2,((n2+KeyData[6]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[6]) shr 8) and $FF] xor sTable[0,(n2+KeyData[6]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[5]) shr 24] xor sTable[2,((n1+KeyData[5]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[5]) shr 8) and $FF] xor sTable[0,(n1+KeyData[5]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[4]) shr 24] xor sTable[2,((n2+KeyData[4]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[4]) shr 8) and $FF] xor sTable[0,(n2+KeyData[4]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[3]) shr 24] xor sTable[2,((n1+KeyData[3]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[3]) shr 8) and $FF] xor sTable[0,(n1+KeyData[3]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[2]) shr 24] xor sTable[2,((n2+KeyData[2]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[2]) shr 8) and $FF] xor sTable[0,(n2+KeyData[2]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[1]) shr 24] xor sTable[2,((n1+KeyData[1]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[1]) shr 8) and $FF] xor sTable[0,(n1+KeyData[1]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[0]) shr 24] xor sTable[2,((n2+KeyData[0]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[0]) shr 8) and $FF] xor sTable[0,(n2+KeyData[0]) and $FF]);
  PDword(@OutData)^:= n2;
  PDword(dword(@OutData)+4)^:= n1;
end;

procedure TDCP_gost.DecryptECB(const InData; var OutData);
var
  n1, n2: DWord;
  i: longword;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  n1:= PDword(@InData)^;
  n2:= PDword(dword(@InData)+4)^;
  n2:= n2 xor (sTable[3,(n1+KeyData[0]) shr 24] xor sTable[2,((n1+KeyData[0]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[0]) shr 8) and $FF] xor sTable[0,(n1+KeyData[0]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[1]) shr 24] xor sTable[2,((n2+KeyData[1]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[1]) shr 8) and $FF] xor sTable[0,(n2+KeyData[1]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[2]) shr 24] xor sTable[2,((n1+KeyData[2]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[2]) shr 8) and $FF] xor sTable[0,(n1+KeyData[2]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[3]) shr 24] xor sTable[2,((n2+KeyData[3]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[3]) shr 8) and $FF] xor sTable[0,(n2+KeyData[3]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[4]) shr 24] xor sTable[2,((n1+KeyData[4]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[4]) shr 8) and $FF] xor sTable[0,(n1+KeyData[4]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[5]) shr 24] xor sTable[2,((n2+KeyData[5]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[5]) shr 8) and $FF] xor sTable[0,(n2+KeyData[5]) and $FF]);
  n2:= n2 xor (sTable[3,(n1+KeyData[6]) shr 24] xor sTable[2,((n1+KeyData[6]) shr 16) and $FF]
    xor sTable[1,((n1+KeyData[6]) shr 8) and $FF] xor sTable[0,(n1+KeyData[6]) and $FF]);
  n1:= n1 xor (sTable[3,(n2+KeyData[7]) shr 24] xor sTable[2,((n2+KeyData[7]) shr 16) and $FF]
    xor sTable[1,((n2+KeyData[7]) shr 8) and $FF] xor sTable[0,(n2+KeyData[7]) and $FF]);
  for i:= 0 to 2 do
  begin
    n2:= n2 xor (sTable[3,(n1+KeyData[7]) shr 24] xor sTable[2,((n1+KeyData[7]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[7]) shr 8) and $FF] xor sTable[0,(n1+KeyData[7]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[6]) shr 24] xor sTable[2,((n2+KeyData[6]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[6]) shr 8) and $FF] xor sTable[0,(n2+KeyData[6]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[5]) shr 24] xor sTable[2,((n1+KeyData[5]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[5]) shr 8) and $FF] xor sTable[0,(n1+KeyData[5]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[4]) shr 24] xor sTable[2,((n2+KeyData[4]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[4]) shr 8) and $FF] xor sTable[0,(n2+KeyData[4]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[3]) shr 24] xor sTable[2,((n1+KeyData[3]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[3]) shr 8) and $FF] xor sTable[0,(n1+KeyData[3]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[2]) shr 24] xor sTable[2,((n2+KeyData[2]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[2]) shr 8) and $FF] xor sTable[0,(n2+KeyData[2]) and $FF]);
    n2:= n2 xor (sTable[3,(n1+KeyData[1]) shr 24] xor sTable[2,((n1+KeyData[1]) shr 16) and $FF]
      xor sTable[1,((n1+KeyData[1]) shr 8) and $FF] xor sTable[0,(n1+KeyData[1]) and $FF]);
    n1:= n1 xor (sTable[3,(n2+KeyData[0]) shr 24] xor sTable[2,((n2+KeyData[0]) shr 16) and $FF]
      xor sTable[1,((n2+KeyData[0]) shr 8) and $FF] xor sTable[0,(n2+KeyData[0]) and $FF]);
  end;
  PDword(@OutData)^:= n2;
  PDword(dword(@OutData)+4)^:= n1;
end;


end.
