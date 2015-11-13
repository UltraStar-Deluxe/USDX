{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of RC2 **********************************}
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
unit DCPrc2;

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPconst, DCPblockciphers;

type
  TDCP_rc2= class(TDCP_blockcipher64)
  protected
    KeyData: array[0..63] of word;
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

{$I DCPrc2.inc}

function LRot16(a, n: word): word;
begin
  Result:= (a shl n) or (a shr (16-n));
end;

function RRot16(a, n: word): word;
begin
  Result:= (a shr n) or (a shl (16-n));
end;

class function TDCP_rc2.GetMaxKeySize: integer;
begin
  Result:= 1024;
end;

class function TDCP_rc2.GetID: integer;
begin
  Result:= DCP_rc2;
end;

class function TDCP_rc2.GetAlgorithm: string;
begin
  Result:= 'RC2';
end;

class function TDCP_rc2.SelfTest: boolean;
const
  Key1: array[0..15] of byte=
    ($00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F);
  InData1: array[0..7] of byte=
    ($00,$00,$00,$00,$00,$00,$00,$00);
  OutData1: array[0..7] of byte=
    ($50,$DC,$01,$62,$BD,$75,$7F,$31);
  Key2: array[0..15] of byte=
    ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01);
  InData2: array[0..7] of byte=
    ($00,$00,$00,$00,$00,$00,$00,$00);
  OutData2: array[0..7] of byte=
    ($21,$82,$9C,$78,$A9,$F9,$C0,$74);
var
  Cipher: TDCP_rc2;
  Data: array[0..7] of byte;
begin
  Cipher:= TDCP_rc2.Create(nil);
  Cipher.Init(Key1,Sizeof(Key1)*8,nil);
  Cipher.EncryptECB(InData1,Data);
  Result:= boolean(CompareMem(@Data,@OutData1,Sizeof(Data)));
  Cipher.DecryptECB(Data,Data);
  Result:= boolean(CompareMem(@Data,@InData1,Sizeof(Data))) and Result;
  Cipher.Burn;
  Cipher.Init(Key2,Sizeof(Key2)*8,nil);
  Cipher.EncryptECB(InData2,Data);
  Result:= boolean(CompareMem(@Data,@OutData2,Sizeof(Data))) and Result;
  Cipher.DecryptECB(Data,Data);
  Result:= boolean(CompareMem(@Data,@InData2,Sizeof(Data))) and Result;
  Cipher.Burn;
  Cipher.Free;
end;

procedure TDCP_rc2.InitKey(const Key; Size: longword);
var
  i: longword;
  KeyB: array[0..127] of byte;
begin
  Move(Key,KeyB,Size div 8);
  for i:= (Size div 8) to 127 do
    KeyB[i]:= sBox[(KeyB[i-(Size div 8)]+KeyB[i-1]) and $FF];
  KeyB[0]:= sBox[KeyB[0]];
  Move(KeyB,KeyData,Sizeof(KeyData));
end;

procedure TDCP_rc2.Burn;
begin
  FillChar(KeyData,Sizeof(KeyData),0);
  inherited Burn;
end;

procedure TDCP_rc2.EncryptECB(const InData; var OutData);
var
  i, j: longword;
  w: array[0..3] of word;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  Pdword(@w[0])^:= Pdword(@InData)^;
  Pdword(@w[2])^:= Pdword(longword(@InData)+4)^;
  for i:= 0 to 15 do
  begin
    j:= i*4;
    w[0]:= LRot16((w[0]+(w[1] and (not w[3]))+(w[2] and w[3])+KeyData[j+0]),1);
    w[1]:= LRot16((w[1]+(w[2] and (not w[0]))+(w[3] and w[0])+KeyData[j+1]),2);
    w[2]:= LRot16((w[2]+(w[3] and (not w[1]))+(w[0] and w[1])+KeyData[j+2]),3);
    w[3]:= LRot16((w[3]+(w[0] and (not w[2]))+(w[1] and w[2])+KeyData[j+3]),5);
    if (i= 4) or (i= 10) then
    begin
      w[0]:= w[0]+KeyData[w[3] and 63];
      w[1]:= w[1]+KeyData[w[0] and 63];
      w[2]:= w[2]+KeyData[w[1] and 63];
      w[3]:= w[3]+KeyData[w[2] and 63];
    end;
  end;
  Pdword(@OutData)^:= Pdword(@w[0])^;
  Pdword(longword(@OutData)+4)^:= Pdword(@w[2])^;
end;

procedure TDCP_rc2.DecryptECB(const InData; var OutData);
var
  i, j: longword;
  w: array[0..3] of word;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  Pdword(@w[0])^:= Pdword(@InData)^;
  Pdword(@w[2])^:= Pdword(longword(@InData)+4)^;
  for i:= 15 downto 0 do
  begin
    j:= i*4;
    w[3]:= RRot16(w[3],5)-(w[0] and (not w[2]))-(w[1] and w[2])-KeyData[j+3];
    w[2]:= RRot16(w[2],3)-(w[3] and (not w[1]))-(w[0] and w[1])-KeyData[j+2];
    w[1]:= RRot16(w[1],2)-(w[2] and (not w[0]))-(w[3] and w[0])-KeyData[j+1];
    w[0]:= RRot16(w[0],1)-(w[1] and (not w[3]))-(w[2] and w[3])-KeyData[j+0];
    if (i= 5) or (i= 11) then
    begin
      w[3]:= w[3]-KeyData[w[2] and 63];
      w[2]:= w[2]-KeyData[w[1] and 63];
      w[1]:= w[1]-KeyData[w[0] and 63];
      w[0]:= w[0]-KeyData[w[3] and 63];
    end;
  end;
  Pdword(@OutData)^:= Pdword(@w[0])^;
  Pdword(longword(@OutData)+4)^:= Pdword(@w[2])^;
end;

end. 
