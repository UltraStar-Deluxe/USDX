{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of RC5 **********************************}
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
unit DCPrc5;

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPconst, DCPblockciphers;

const
  NUMROUNDS= 12;    { number of rounds must be between 12-16 }

type
  TDCP_rc5= class(TDCP_blockcipher64)
  protected
    KeyData: array[0..((NUMROUNDS*2)+1)] of DWord;
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

const
   sBox: array[0..33] of dword= (
    $B7E15163,$5618CB1C,$F45044D5,$9287BE8E,$30BF3847,$CEF6B200,
    $6D2E2BB9,$0B65A572,$A99D1F2B,$47D498E4,$E60C129D,$84438C56,
    $227B060F,$C0B27FC8,$5EE9F981,$FD21733A,$9B58ECF3,$399066AC,
    $D7C7E065,$75FF5A1E,$1436D3D7,$B26E4D90,$50A5C749,$EEDD4102,
    $8D14BABB,$2B4C3474,$C983AE2D,$67BB27E6,$05F2A19F,$A42A1B58,
    $42619511,$E0990ECA,$7ED08883,$1D08023C);

function LRot32(a, b: longword): longword;
begin
  Result:= (a shl b) or (a shr (32-b));
end;

function RRot32(a, b: longword): longword;
begin
  Result:= (a shr b) or (a shl (32-b));
end;

class function TDCP_rc5.GetID: integer;
begin
  Result:= DCP_rc5;
end;

class function TDCP_rc5.GetAlgorithm: string;
begin
  Result:= 'RC5';
end;

class function TDCP_rc5.GetMaxKeySize: integer;
begin
  Result:= 2048;
end;

class function TDCP_rc5.SelfTest: boolean;
const
  Key1: array[0..15] of byte=
    ($DC,$49,$DB,$13,$75,$A5,$58,$4F,$64,$85,$B4,$13,$B5,$F1,$2B,$AF);
  Plain1: array[0..1] of dword=
    ($B7B3422F,$92FC6903);
  Cipher1: array[0..1] of dword=
    ($B278C165,$CC97D184);
  Key2: array[0..15] of byte=
    ($52,$69,$F1,$49,$D4,$1B,$A0,$15,$24,$97,$57,$4D,$7F,$15,$31,$25);
  Plain2: array[0..1] of dword=
    ($B278C165,$CC97D184);
  Cipher2: array[0..1] of dword=
    ($15E444EB,$249831DA);
var
  Cipher: TDCP_rc5;
  Data: array[0..1] of dword;
begin
  Cipher:= TDCP_rc5.Create(nil);
  Cipher.Init(Key1,Sizeof(Key1)*8,nil);
  Cipher.EncryptECB(Plain1,Data);
  Result:= boolean(CompareMem(@Data,@Cipher1,Sizeof(Data)));
  Cipher.DecryptECB(Data,Data);
  Result:= Result and boolean(CompareMem(@Data,@Plain1,Sizeof(Data)));
  Cipher.Burn;
  Cipher.Init(Key2,Sizeof(Key2)*8,nil);
  Cipher.EncryptECB(Plain2,Data);
  Result:= Result and boolean(CompareMem(@Data,@Cipher2,Sizeof(Data)));
  Cipher.DecryptECB(Data,Data);
  Result:= Result and boolean(CompareMem(@Data,@Plain2,Sizeof(Data)));
  Cipher.Burn;
  Cipher.Free;
end;

procedure TDCP_rc5.InitKey(const Key; Size: longword);
var
  xKeyD: array[0..63] of DWord;
  i, j, k, xKeyLen: longword;
  A, B: DWord;
begin
  FillChar(xKeyD,Sizeof(xKeyD),0);
  Size:= Size div 8;
  Move(Key,xKeyD,Size);
  xKeyLen:= Size div 4;
  if (Size mod 4)<> 0 then
    Inc(xKeyLen);
  Move(sBox,KeyData,(NUMROUNDS+1)*8);
  i:= 0; j:= 0;
  A:= 0; B:= 0;
  if xKeyLen> ((NUMROUNDS+1)*2) then
    k:= xKeyLen*3
  else
    k:= (NUMROUNDS+1)*6;
  for k:= k downto 1 do
  begin
    A:= LRot32(KeyData[i]+A+B,3);
    KeyData[i]:= A;
    B:= LRot32(xKeyD[j]+A+B,A+B);
    xKeyD[j]:= B;
    i:= (i+1) mod ((NUMROUNDS+1)*2);
    j:= (j+1) mod xKeyLen;
  end;
  FillChar(xKeyD,Sizeof(xKeyD),0);
end;

procedure TDCP_rc5.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),$FF);
  inherited Burn;
end;

procedure TDCP_rc5.EncryptECB(const InData; var OutData);
var
  A, B: DWord;
  i: longword;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  A:= PDword(@InData)^ + KeyData[0];
  B:= PDword(longword(@InData)+4)^ + KeyData[1];
  for i:= 1 to NUMROUNDS do
  begin
    A:= A xor B;
    A:= LRot32(A,B)+KeyData[2*i];
    B:= B xor A;
    B:= LRot32(B,A)+KeyData[(2*i)+1];
  end;
  PDword(@OutData)^:= A;
  PDword(longword(@OutData)+4)^:= B;
end;

procedure TDCP_rc5.DecryptECB(const InData; var OutData);
var
  A, B: DWord;
  i: longword;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  A:= PDword(@InData)^;
  B:= PDword(longword(@InData)+4)^;
  for i:= NUMROUNDS downto 1 do
  begin
    B:= RRot32(B-KeyData[(2*i)+1],A);
    B:= B xor A;
    A:= RRot32(A-KeyData[2*i],B);
    A:= A xor B;
  end;
  PDword(@OutData)^:= A - KeyData[0];
  PDword(longword(@OutData)+4)^:= B - KeyData[1];
end;

end.
