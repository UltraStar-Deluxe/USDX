{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A file hashing demo ********************************************************}
{******************************************************************************}
{* Copyright (c) 2003 David Barton                                            *}
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
unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, CheckLst, DCPtiger, DCPsha512, DCPsha256,
  DCPsha1, DCPripemd160, DCPripemd128, DCPmd5, DCPmd4, DCPcrypt2, DCPhaval;

type
  TfrmMain = class(TForm)
    DCP_haval1: TDCP_haval;
    DCP_md41: TDCP_md4;
    DCP_md51: TDCP_md5;
    DCP_ripemd1281: TDCP_ripemd128;
    DCP_ripemd1601: TDCP_ripemd160;
    DCP_sha11: TDCP_sha1;
    DCP_sha2561: TDCP_sha256;
    DCP_sha3841: TDCP_sha384;
    DCP_sha5121: TDCP_sha512;
    DCP_tiger1: TDCP_tiger;
    grpInputFile: TGroupBox;
    boxInputFile: TEdit;
    grpHashes: TGroupBox;
    lstHashes: TCheckListBox;
    grpOutput: TGroupBox;
    txtOutput: TMemo;
    btnHash: TButton;
    btnBrowseFiles: TSpeedButton;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseFilesClick(Sender: TObject);
    procedure btnHashClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: integer;
  Hash: TDCP_hash;
begin
  ClientHeight := 416;
  ClientWidth := 408;
  MessageDlg('This is a file hashing demo using the DCPcrypt component set.'+#13+'For more information see http://www.cityinthesky.co.uk/cryptography.html',mtInformation,[mbOK],0);

  // find all the hash algorithms on the form
  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i] is TDCP_hash then
    begin
      Hash := TDCP_hash(Components[i]);
      lstHashes.Items.AddObject(Hash.Algorithm + ' (Digest size: ' + IntToStr(Hash.HashSize) + ' bits)',Components[i]);
    end;
  end;
end;

procedure TfrmMain.btnBrowseFilesClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    boxInputFile.Text := dlgOpen.FileName;
end;

procedure TfrmMain.btnHashClick(Sender: TObject);
var
  Hashes: array of TDCP_hash;
  HashDigest: array of byte;
  i, j, read: integer;
  s: string;
  buffer: array[0..16383] of byte;
  strmInput: TFileStream;
begin
  txtOutput.Clear;
  if not FileExists(boxInputFile.Text) then
  begin
    MessageDlg('File does not exist',mtInformation,[mbOK],0);
    Exit;
  end;
  Hashes := nil;
  // make a list of all the hash algorithms to use
  for i := 0 to lstHashes.Items.Count - 1 do
  begin
    if lstHashes.Checked[i] then
    begin
      // yes I know this is inefficient but it's also easy ;-)
      SetLength(Hashes,Length(Hashes) + 1);
      Hashes[Length(Hashes) - 1] := TDCP_hash(lstHashes.Items.Objects[i]);
      TDCP_hash(lstHashes.Items.Objects[i]).Init;
    end;
  end;
  strmInput := nil;
  try
    strmInput := TFileStream.Create(boxInputFile.Text,fmOpenRead);
    repeat
      // read into the buffer
      read := strmInput.Read(buffer,Sizeof(buffer));
      // hash the buffer with each of the selected hashes
      for i := 0 to Length(Hashes) - 1 do
        Hashes[i].Update(buffer,read);
    until read <> Sizeof(buffer);
    strmInput.Free;
    // iterate through the selected hashes
    for i := 0 to Length(Hashes) - 1 do
    begin
      SetLength(HashDigest,Hashes[i].HashSize div 8);
      Hashes[i].Final(HashDigest[0]);  // get the output
      s := '';
      for j := 0 to Length(HashDigest) - 1 do  // convert it into a hex string
        s := s + IntToHex(HashDigest[j],2);
      txtOutput.Lines.Add(Hashes[i].Algorithm + ': ' + s);
    end;
  except
    strmInput.Free;
    MessageDlg('An error occurred while reading the file',mtError,[mbOK],0);
  end;
end;

end.
