object frmMain: TfrmMain
  Left = 211
  Top = 116
  BorderStyle = bsDialog
  Caption = 'Encrypt Test'
  ClientHeight = 136
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblKey: TLabel
    Left = 8
    Top = 8
    Width = 18
    Height = 13
    Caption = 'Key'
  end
  object lblIV: TLabel
    Left = 256
    Top = 8
    Width = 10
    Height = 13
    Caption = 'IV'
  end
  object lblPlainTextOut: TLabel
    Left = 256
    Top = 72
    Width = 58
    Height = 13
    Caption = 'Plaintext out'
  end
  object lblCipherTextOut: TLabel
    Left = 8
    Top = 72
    Width = 65
    Height = 13
    Caption = 'Ciphertext out'
  end
  object lblCipherTextIn: TLabel
    Left = 256
    Top = 40
    Width = 58
    Height = 13
    Caption = 'Ciphertext in'
  end
  object lblPlainTextIn: TLabel
    Left = 8
    Top = 40
    Width = 51
    Height = 13
    Caption = 'Plaintext in'
  end
  object boxKey: TEdit
    Left = 80
    Top = 8
    Width = 160
    Height = 21
    TabOrder = 0
    Text = 'My key'
  end
  object boxIV: TEdit
    Left = 336
    Top = 8
    Width = 160
    Height = 21
    TabOrder = 1
  end
  object boxPlainTextIn: TEdit
    Left = 80
    Top = 40
    Width = 160
    Height = 21
    TabOrder = 2
    Text = 'My data'
  end
  object boxCipherTextOut: TEdit
    Left = 80
    Top = 72
    Width = 160
    Height = 21
    TabOrder = 3
  end
  object boxCipherTextIn: TEdit
    Left = 336
    Top = 40
    Width = 160
    Height = 21
    TabOrder = 5
  end
  object boxPlainTextOut: TEdit
    Left = 336
    Top = 72
    Width = 160
    Height = 21
    TabOrder = 6
  end
  object btnEncrypt: TButton
    Left = 160
    Top = 104
    Width = 81
    Height = 25
    Caption = 'Encrypt'
    TabOrder = 4
    OnClick = btnEncryptClick
  end
  object btnDecrypt: TButton
    Left = 416
    Top = 104
    Width = 81
    Height = 25
    Caption = 'Decrypt'
    TabOrder = 7
    OnClick = btnDecryptClick
  end
end
