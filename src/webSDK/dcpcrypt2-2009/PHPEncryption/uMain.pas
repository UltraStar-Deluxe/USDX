unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmMain = class(TForm)
    boxKey: TEdit;
    boxIV: TEdit;
    boxPlainTextIn: TEdit;
    boxCipherTextOut: TEdit;
    boxCipherTextIn: TEdit;
    boxPlainTextOut: TEdit;
    lblKey: TLabel;
    lblIV: TLabel;
    lblPlainTextOut: TLabel;
    lblCipherTextOut: TLabel;
    lblCipherTextIn: TLabel;
    lblPlainTextIn: TLabel;
    btnEncrypt: TButton;
    btnDecrypt: TButton;
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  DCPcrypt2, DCPrijndael, DCPbase64;

// Some constants that are dependant on the cipher being used
// Assuming MCRYPT_RIJNDAEL_128 (i.e., 128bit blocksize, 256bit keysize)
const
  KeySize = 32; // 32 bytes = 256 bits
  BlockSize = 16; // 16 bytes = 128 bits

{$R *.dfm}

// Pad a string with zeros so that it is a multiple of size
function PadWithZeros(const str : string; size : integer) : string;
var
  origsize, i : integer;
begin
  Result := str;
  origsize := Length(Result);
  if ((origsize mod size) <> 0) or (origsize = 0) then
  begin
    SetLength(Result,((origsize div size)+1)*size);
    for i := origsize+1 to Length(Result) do
      Result[i] := #0;
  end;
end;

// Encrypt a string and return the Base64 encoded result
procedure TfrmMain.btnEncryptClick(Sender: TObject);
var
  Cipher : TDCP_rijndael;
  Data, Key, IV : string;
begin
  // Pad Key, IV and Data with zeros as appropriate
  Key := PadWithZeros(boxKey.Text,KeySize);
  IV := PadWithZeros(boxIV.Text,BlockSize);
  Data := PadWithZeros(boxPlainTextIn.Text,BlockSize);
  // Create the cipher and initialise according to the key length
  Cipher := TDCP_rijndael.Create(Self);
  if Length(boxKey.Text) <= 16 then
    Cipher.Init(Key[1],128,@IV[1])
  else if Length(boxKey.Text) <= 24 then
    Cipher.Init(Key[1],192,@IV[1])
  else
    Cipher.Init(Key[1],256,@IV[1]);
  // Encrypt the data
  Cipher.EncryptCBC(Data[1],Data[1],Length(Data));
  // Free the cipher and clear sensitive information
  Cipher.Free;
  FillChar(Key[1],Length(Key),0);
  // Display the Base64 encoded result
  boxCipherTextOut.Text := Base64EncodeStr(Data);
end;

procedure TfrmMain.btnDecryptClick(Sender: TObject);
var
  Cipher : TDCP_rijndael;
  Data, Key, IV : string;
begin
  // Pad Key and IV with zeros as appropriate
  Key := PadWithZeros(boxKey.Text,KeySize);
  IV := PadWithZeros(boxIV.Text,BlockSize);
  // Decode the Base64 encoded string
  Data := Base64DecodeStr(boxCipherTextIn.Text);
  // Create the cipher and initialise according to the key length
  Cipher := TDCP_rijndael.Create(Self);
  if Length(boxKey.Text) <= 16 then
    Cipher.Init(Key[1],128,@IV[1])
  else if Length(boxKey.Text) <= 24 then
    Cipher.Init(Key[1],192,@IV[1])
  else
    Cipher.Init(Key[1],256,@IV[1]);
  // Decrypt the data
  Cipher.DecryptCBC(Data[1],Data[1],Length(Data));
  // Free the cipher and clear sensitive information
  Cipher.Free;
  FillChar(Key[1],Length(Key),0);
  // Display the result
  boxPlainTextOut.Text := Data;
end;

end.
