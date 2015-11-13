{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A file encryption/decryption demo ******************************************}
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
{*                                                                            *}
{* This isn't the most well written of demos but it should be relatively      *}
{* informative. Any problems, queries, (bugs?) feel free to email me at the   *}
{* above address (I may not reply depending on my workload at the time, but I *}
{* will do my best).                                                          *}
{*                               Dave.                                        *}
{*                                                                            *}
{* Note: this program does not store the cipher or hash used to encrypt the   *}
{* original file and so you will need to note this yourself. Also it will     *}
{* happily decrypt with the wrong cipher/hash/passphrase and give you utter   *}
{* garbage out :-)                                                            *}
{*                                                                            *}
{******************************************************************************}
unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DCPtiger, DCPsha512, DCPsha256, DCPsha1, DCPripemd160,
  DCPripemd128, DCPmd5, DCPmd4, DCPcrypt2, DCPhaval, DCPtwofish, DCPtea,
  DCPserpent, DCPblockciphers, DCPrijndael, DCPrc4, DCPrc2, DCPice, DCPdes,
  DCPcast128, DCPblowfish, StdCtrls, Buttons;

type
  TfrmMain = class(TForm)
    grpInput: TGroupBox;
    boxInputFile: TEdit;
    btnInputBrowse: TSpeedButton;
    lblInputFileSize: TLabel;
    dblInputFileSize: TLabel;
    grpOutput: TGroupBox;
    boxOutputFile: TEdit;
    btnOutputBrowse: TSpeedButton;
    grpOptions: TGroupBox;
    cbxCipher: TComboBox;
    lblCipher: TLabel;
    lblHash: TLabel;
    cbxHash: TComboBox;
    lblKeySize: TLabel;
    dblKeySize: TLabel;
    boxPassphrase: TEdit;
    lblPassphrase: TLabel;
    boxConfirmPassphrase: TEdit;
    lblConfirmPassphrase: TLabel;
    btnEncrypt: TButton;
    btnDecrypt: TButton;
    btnClose: TButton;
    DCP_blowfish1: TDCP_blowfish;
    DCP_cast1281: TDCP_cast128;
    DCP_des1: TDCP_des;
    DCP_3des1: TDCP_3des;
    DCP_ice1: TDCP_ice;
    DCP_thinice1: TDCP_thinice;
    DCP_ice21: TDCP_ice2;
    DCP_rc21: TDCP_rc2;
    DCP_rc41: TDCP_rc4;
    DCP_rijndael1: TDCP_rijndael;
    DCP_serpent1: TDCP_serpent;
    DCP_tea1: TDCP_tea;
    DCP_twofish1: TDCP_twofish;
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
    dlgInput: TOpenDialog;
    dlgOutput: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure boxInputFileExit(Sender: TObject);
    procedure btnInputBrowseClick(Sender: TObject);
    procedure btnOutputBrowseClick(Sender: TObject);
    procedure cbxCipherChange(Sender: TObject);
    procedure boxPassphraseChange(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
    procedure DisableForm;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.DisableForm;
begin
  grpInput.Enabled:= false;
  grpOutput.Enabled:= false;
  grpOptions.Enabled:= false;
  btnEncrypt.Enabled:= false;
  btnDecrypt.Enabled:= false;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Randomize;
  ClientWidth:= 296;
  ClientHeight:= 440;
  MessageDlg('This is a file encryption demo using the DCPcrypt component set.'+#13+'For more information see http://www.cityinthesky.co.uk/cryptography.html',mtInformation,[mbOK],0);

  // iterate through all the components and find the ciphers/hashes
  for i := 0 to (ComponentCount - 1) do
  begin
    if (Components[i] is TDCP_cipher) then
      cbxCipher.Items.AddObject(TDCP_cipher(Components[i]).Algorithm,Components[i])
    else if (Components[i] is TDCP_hash) then
      cbxHash.Items.AddObject(TDCP_hash(Components[i]).Algorithm,Components[i]);
  end;
  if (cbxCipher.Items.Count = 0) then
  begin
    MessageDlg('No ciphers were found',mtError,[mbOK],0);
    DisableForm;
  end
  else
  begin
    cbxCipher.ItemIndex := 0;
    if (cbxHash.Items.Count = 0) then
    begin
      MessageDlg('No hashes were found',mtError,[mbOK],0);
      DisableForm;
    end
    else
    begin
      cbxHash.ItemIndex := 0;
      cbxCipher.OnChange(cbxCipher);
    end;
  end;
end;

// Add commas into a numerical string (e.g. 12345678 becomes 12,345,678)
// Not the best way but I can't find the code I wrote last time...
function AddCommas(const S: string): string;
var
  i, j: integer;
begin
  i := Length(S) mod 3;
  if ((i <> 0) and (Length(S) > 3)) then
    Result := Copy(S,1,i) + ',';
  for j := 0 to ((Length(S) div 3) - 2) do
    Result := Result + Copy(S,1 + i + j*3,3) + ',';
  if (Length(S) > 3) then
    Result := Result + Copy(S,Length(S) - 2,3)
  else
    Result := S;
end;

procedure TfrmMain.boxInputFileExit(Sender: TObject);
var
  strmInput: TFileStream;
begin
  if (boxInputFile.Text = '') then
    dblInputFileSize.Caption := 'no file specified'
  else if FileExists(boxInputFile.Text) then
  begin
    // If the file exists then see how big it is
    strmInput := nil;
    try
      strmInput := TFileStream.Create(boxInputFile.Text,fmOpenRead);
      dblInputFileSize.Caption := AddCommas(IntToStr(strmInput.Size)) + ' bytes';
      strmInput.Free;
    except
      strmInput.Free;
      dblInputFileSize.Caption := 'unable to open file';
    end;
  end
  else
    dblInputFileSize.Caption := 'file does not exist';
end;

procedure TfrmMain.btnInputBrowseClick(Sender: TObject);
begin
  if dlgInput.Execute then
  begin
    boxInputFile.Text := dlgInput.FileName;
    boxInputFile.OnExit(boxInputFile);
  end;
end;

procedure TfrmMain.btnOutputBrowseClick(Sender: TObject);
begin
  if dlgOutput.Execute then
    boxOutputFile.Text := dlgOutput.FileName;
end;

procedure TfrmMain.cbxCipherChange(Sender: TObject);
var
  Cipher: TDCP_cipher;
  Hash: TDCP_hash;
begin
  // Set the effective keysize to be the minimum of the hash size and the max key size
  // i.e. if the max key size is sufficiently large then use the entire hash as the
  //  key, other wise truncate the hash
  Cipher := TDCP_cipher(cbxCipher.Items.Objects[cbxCipher.ItemIndex]);
  Hash := TDCP_hash(cbxHash.Items.Objects[cbxHash.ItemIndex]);
  if (Cipher.MaxKeySize < Hash.HashSize) then
    dblKeySize.Caption := IntToStr(Cipher.MaxKeySize) + ' bits'
  else
    dblKeySize.Caption := IntToStr(Hash.HashSize) + ' bits'
end;

procedure TfrmMain.boxPassphraseChange(Sender: TObject);
begin
  if (Length(boxPassphrase.Text) > 0) then
  begin
    btnDecrypt.Enabled := true;
    if (boxPassphrase.Text = boxConfirmPassphrase.Text) then
      btnEncrypt.Enabled := true
    else
      btnEncrypt.Enabled := false;
  end
  else
    btnDecrypt.Enabled := false;
end;

function Min(a, b: integer): integer;
begin
  if (a < b) then
    Result := a
  else
    Result := b;
end;

procedure TfrmMain.btnEncryptClick(Sender: TObject);
var
  Cipher: TDCP_cipher;         // the cipher to use
  CipherIV: array of byte;     // the initialisation vector (for chaining modes)
  Hash: TDCP_hash;             // the hash to use
  HashDigest: array of byte;   // the result of hashing the passphrase with the salt
  Salt: array[0..7] of byte;   // a random salt to help prevent precomputated attacks
  strmInput, strmOutput: TFileStream;
  i: integer;
begin
  if FileExists(boxOutputFile.Text) then
    if (MessageDlg('Output file already exists. Overwrite?',mtConfirmation,mbYesNoCancel,0) <> mrYes) then
      Exit;
  strmInput := nil;
  strmOutput := nil;
  try
    strmInput := TFileStream.Create(boxInputFile.Text,fmOpenRead);
    strmOutput := TFileStream.Create(boxOutputFile.Text,fmCreate);

    Hash := TDCP_hash(cbxHash.Items.Objects[cbxHash.ItemIndex]);
    SetLength(HashDigest,Hash.HashSize div 8);
    for i := 0 to 7 do
      Salt[i] := Random(256);  // just fill the salt with random values (crypto secure PRNG would be better but not _really_ necessary)
    strmOutput.WriteBuffer(Salt,Sizeof(Salt));  // write out the salt so we can decrypt!
    Hash.Init;
    Hash.Update(Salt[0],Sizeof(Salt));   // hash the salt
    Hash.UpdateStr(boxPassphrase.Text);  // and the passphrase
    Hash.Final(HashDigest[0]);           // store the output in HashDigest

    Cipher := TDCP_cipher(cbxCipher.Items.Objects[cbxCipher.ItemIndex]);
    if (Cipher is TDCP_blockcipher) then      // if the cipher is a block cipher we need an initialisation vector
    begin
      SetLength(CipherIV,TDCP_blockcipher(Cipher).BlockSize div 8);
      for i := 0 to (Length(CipherIV) - 1) do
        CipherIV[i] := Random(256);           // again just random values for the IV
      strmOutput.WriteBuffer(CipherIV[0],Length(CipherIV));  // write out the IV so we can decrypt!
      Cipher.Init(HashDigest[0],Min(Cipher.MaxKeySize,Hash.HashSize),CipherIV);  // initialise the cipher with the hash as key
      TDCP_blockcipher(Cipher).CipherMode := cmCBC;   // use CBC chaining when encrypting
    end
    else
      Cipher.Init(HashDigest[0],Min(Cipher.MaxKeySize,Hash.HashSize),nil); // initialise the cipher with the hash as key

    Cipher.EncryptStream(strmInput,strmOutput,strmInput.Size); // encrypt the entire file
    Cipher.Burn;   // important! get rid of keying information
    strmInput.Free;
    strmOutput.Free;
    MessageDlg('File encrypted',mtInformation,[mbOK],0);

  except
    strmInput.Free;
    strmOutput.Free;
    MessageDlg('An error occurred while processing the file',mtError,[mbOK],0);
  end;
end;

procedure TfrmMain.btnDecryptClick(Sender: TObject);
var
  Cipher: TDCP_cipher;         // the cipher to use
  CipherIV: array of byte;     // the initialisation vector (for chaining modes)
  Hash: TDCP_hash;             // the hash to use
  HashDigest: array of byte;   // the result of hashing the passphrase with the salt
  Salt: array[0..7] of byte;   // a random salt to help prevent precomputated attacks
  strmInput, strmOutput: TFileStream;
begin
  if FileExists(boxOutputFile.Text) then
    if (MessageDlg('Output file already exists. Overwrite?',mtConfirmation,mbYesNoCancel,0) <> mrYes) then
      Exit;
  strmInput := nil;
  strmOutput := nil;
  try
    strmInput := TFileStream.Create(boxInputFile.Text,fmOpenRead);
    strmOutput := TFileStream.Create(boxOutputFile.Text,fmCreate);

    Hash := TDCP_hash(cbxHash.Items.Objects[cbxHash.ItemIndex]);
    SetLength(HashDigest,Hash.HashSize div 8);
    strmInput.ReadBuffer(Salt[0],Sizeof(Salt));  // read the salt in from the file
    Hash.Init;
    Hash.Update(Salt[0],Sizeof(Salt));   // hash the salt
    Hash.UpdateStr(boxPassphrase.Text);  // and the passphrase
    Hash.Final(HashDigest[0]);           // store the hash in HashDigest

    Cipher := TDCP_cipher(cbxCipher.Items.Objects[cbxCipher.ItemIndex]);
    if (Cipher is TDCP_blockcipher) then            // if it is a block cipher we need the IV
    begin
      SetLength(CipherIV,TDCP_blockcipher(Cipher).BlockSize div 8);
      strmInput.ReadBuffer(CipherIV[0],Length(CipherIV));       // read the initialisation vector from the file
      Cipher.Init(HashDigest[0],Min(Cipher.MaxKeySize,Hash.HashSize),CipherIV);  // initialise the cipher
      TDCP_blockcipher(Cipher).CipherMode := cmCBC;
    end
    else
      Cipher.Init(HashDigest[0],Min(Cipher.MaxKeySize,Hash.HashSize),nil);  // initialise the cipher

    Cipher.DecryptStream(strmInput,strmOutput,strmInput.Size - strmInput.Position); // decrypt!
    Cipher.Burn;
    strmInput.Free;
    strmOutput.Free;
    MessageDlg('File decrypted',mtInformation,[mbOK],0);

  except
    strmInput.Free;
    strmOutput.Free;
    MessageDlg('An error occurred while processing the file',mtError,[mbOK],0);
  end;
end;

procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;


end.

