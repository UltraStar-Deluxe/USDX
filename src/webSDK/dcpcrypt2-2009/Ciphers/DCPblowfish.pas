{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of Blowfish *****************************}
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
unit DCPblowfish;

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPconst, DCPblockciphers;

type
  TDCP_blowfish= class(TDCP_blockcipher64)
  protected
    SBox: array[0..3,0..255] of DWord;
    PBox: array[0..17] of DWord;
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
{$I DCPblowfish.inc}

class function TDCP_blowfish.GetID: integer;
begin
  Result:= DCP_blowfish;
end;

class function TDCP_blowfish.GetAlgorithm: string;
begin
  Result:= 'Blowfish';
end;

class function TDCP_blowfish.GetMaxKeySize: integer;
begin
  Result:= 448;
end;

class function TDCP_blowfish.SelfTest: boolean;
const
  Key1: array[0..7] of byte= ($00,$00,$00,$00,$00,$00,$00,$00);
  Key2: array[0..7] of byte= ($7C,$A1,$10,$45,$4A,$1A,$6E,$57);
  InData1: array[0..7] of byte= ($00,$00,$00,$00,$00,$00,$00,$00);
  InData2: array[0..7] of byte= ($01,$A1,$D6,$D0,$39,$77,$67,$42);
  OutData1: array[0..7] of byte= ($4E,$F9,$97,$45,$61,$98,$DD,$78);
  OutData2: array[0..7] of byte= ($59,$C6,$82,$45,$EB,$05,$28,$2B);
var
  Cipher: TDCP_blowfish;
  Data: array[0..7] of byte;
begin
  Cipher:= TDCP_blowfish.Create(nil);
  Cipher.Init(Key1,Sizeof(Key1)*8,nil);
  Cipher.EncryptECB(InData1,Data);
  Result:= boolean(CompareMem(@Data,@OutData1,Sizeof(Data)));
  Cipher.Reset;
  Cipher.DecryptECB(Data,Data);
  Result:= boolean(CompareMem(@Data,@InData1,Sizeof(Data))) and Result;
  Cipher.Burn;
  Cipher.Init(Key2,Sizeof(Key2)*8,nil);
  Cipher.EncryptECB(InData2,Data);
  Result:= boolean(CompareMem(@Data,@OutData2,Sizeof(Data))) and Result;
  Cipher.Reset;
  Cipher.DecryptECB(Data,Data);
  Result:= boolean(CompareMem(@Data,@InData2,Sizeof(Data))) and Result;
  Cipher.Burn;
  Cipher.Free;
end;

procedure TDCP_blowfish.InitKey(const Key; Size: longword);
var
  i, k: longword;
  A: DWord;
  KeyB: PByteArray;
  Block: array[0..7] of byte;
begin
  Size:= Size div 8;
  KeyB:= @Key;
  Move(SBoxOrg,SBox,Sizeof(SBox));
  Move(PBoxOrg,PBox,Sizeof(PBox));
  k:= 0;
  for i:= 0 to 17 do
  begin
    A:= dword(KeyB^[(k+3) mod Size]);
    A:= A + (dword(KeyB^[(k+2) mod Size]) shl 8);
    A:= A + (dword(KeyB^[(k+1) mod Size]) shl 16);
    A:= A + (dword(KeyB^[k]) shl 24);
    PBox[i]:= PBox[i] xor A;
    k:= (k+4) mod Size;
  end;
  FillChar(Block,Sizeof(Block),0);
  for i:= 0 to 8 do
  begin
    EncryptECB(Block,Block);
    PBox[i*2]:= dword(Block[3]) + (dword(Block[2]) shl 8) + (dword(Block[1]) shl 16) + (dword(Block[0]) shl 24);
    PBox[i*2+1]:= dword(Block[7]) + (dword(Block[6]) shl 8) + (dword(Block[5]) shl 16) + (dword(Block[4]) shl 24);
  end;
  for k:= 0 to 3 do
  begin
    for i:= 0 to 127 do
    begin
      EncryptECB(Block,Block);
      SBox[k,i*2]:= dword(Block[3]) + (dword(Block[2]) shl 8) + (dword(Block[1]) shl 16) + (dword(Block[0]) shl 24);
      SBox[k,i*2+1]:= dword(Block[7]) + (dword(Block[6]) shl 8) + (dword(Block[5]) shl 16) + (dword(Block[4]) shl 24);
    end;
  end;
end;

procedure TDCP_blowfish.Burn;
begin
  FillChar(SBox,Sizeof(SBox),$FF);
  FillChar(PBox,Sizeof(PBox),$FF);
  inherited Burn;
end;

procedure TDCP_blowfish.EncryptECB(const InData; var OutData);
var
  xL, xR: DWord;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  xL:= Pdword(@InData)^;
  xR:= Pdword(longword(@InData)+4)^;
  xL:= ((xL and $FF) shl 24) or ((xL and $FF00) shl 8) or ((xL and $FF0000) shr 8) or ((xL and $FF000000) shr 24);
  xR:= ((xR and $FF) shl 24) or ((xR and $FF00) shl 8) or ((xR and $FF0000) shr 8) or ((xR and $FF000000) shr 24);
  xL:= xL xor PBox[0];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[1];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[2];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[3];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[4];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[5];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[6];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[7];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[8];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[9];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[10];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[11];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[12];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[13];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[14];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[15];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[16];
  xR:= xR xor PBox[17];
  xL:= ((xL and $FF) shl 24) or ((xL and $FF00) shl 8) or ((xL and $FF0000) shr 8) or ((xL and $FF000000) shr 24);
  xR:= ((xR and $FF) shl 24) or ((xR and $FF00) shl 8) or ((xR and $FF0000) shr 8) or ((xR and $FF000000) shr 24);
  Pdword(@OutData)^:= xR;
  Pdword(longword(@OutData)+4)^:= xL;
end;

procedure TDCP_blowfish.DecryptECB(const InData; var OutData);
var
  xL, xR: DWord;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  xL:= Pdword(@InData)^;
  xR:= Pdword(longword(@InData)+4)^;
  xL:= (xL shr 24) or ((xL shr 8) and $FF00) or ((xL shl 8) and $FF0000) or (xL shl 24);
  xR:= (xR shr 24) or ((xR shr 8) and $FF00) or ((xR shl 8) and $FF0000) or (xR shl 24);
  xL:= xL xor PBox[17];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[16];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[15];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[14];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[13];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[12];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[11];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[10];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[9];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[8];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[7];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[6];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[5];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[4];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[3];
  xR:= xR xor (((SBox[0,(xL shr 24) and $FF] + SBox[1,(xL shr 16) and $FF]) xor
    SBox[2,(xL shr 8) and $FF]) + SBox[3,xL and $FF]) xor PBox[2];
  xL:= xL xor (((SBox[0,(xR shr 24) and $FF] + SBox[1,(xR shr 16) and $FF]) xor
    SBox[2,(xR shr 8) and $FF]) + SBox[3,xR and $FF]) xor PBox[1];
  xR:= xR xor PBox[0];
  xL:= (xL shr 24) or ((xL shr 8) and $FF00) or ((xL shl 8) and $FF0000) or (xL shl 24);
  xR:= (xR shr 24) or ((xR shr 8) and $FF00) or ((xR shl 8) and $FF0000) or (xR shl 24);
  Pdword(@OutData)^:= xR;
  Pdword(longword(@OutData)+4)^:= xL;
end;

end.
