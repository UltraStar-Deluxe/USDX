object frmMain: TfrmMain
  Left = 327
  Top = 112
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'DCPcrypt File Encryption Demo'
  ClientHeight = 440
  ClientWidth = 528
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object grpInput: TGroupBox
    Left = 8
    Top = 8
    Width = 281
    Height = 81
    Caption = 'Input file'
    TabOrder = 0
    object btnInputBrowse: TSpeedButton
      Left = 248
      Top = 24
      Width = 24
      Height = 24
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        5555555555555555555555555555555555555555555555555555555555555555
        555555555555555555555555555555555555555FFFFFFFFFF555550000000000
        55555577777777775F55500B8B8B8B8B05555775F555555575F550F0B8B8B8B8
        B05557F75F555555575F50BF0B8B8B8B8B0557F575FFFFFFFF7F50FBF0000000
        000557F557777777777550BFBFBFBFB0555557F555555557F55550FBFBFBFBF0
        555557F555555FF7555550BFBFBF00055555575F555577755555550BFBF05555
        55555575FFF75555555555700007555555555557777555555555555555555555
        5555555555555555555555555555555555555555555555555555}
      NumGlyphs = 2
      OnClick = btnInputBrowseClick
    end
    object lblInputFileSize: TLabel
      Left = 8
      Top = 56
      Width = 53
      Height = 16
      Caption = 'File size:'
    end
    object dblInputFileSize: TLabel
      Left = 64
      Top = 56
      Width = 89
      Height = 16
      Caption = 'no file specified'
    end
    object boxInputFile: TEdit
      Left = 8
      Top = 24
      Width = 241
      Height = 24
      TabOrder = 0
      OnExit = boxInputFileExit
    end
  end
  object grpOutput: TGroupBox
    Left = 8
    Top = 96
    Width = 281
    Height = 57
    Caption = 'Output file'
    TabOrder = 1
    object btnOutputBrowse: TSpeedButton
      Left = 248
      Top = 24
      Width = 24
      Height = 24
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        5555555555555555555555555555555555555555555555555555555555555555
        555555555555555555555555555555555555555FFFFFFFFFF555550000000000
        55555577777777775F55500B8B8B8B8B05555775F555555575F550F0B8B8B8B8
        B05557F75F555555575F50BF0B8B8B8B8B0557F575FFFFFFFF7F50FBF0000000
        000557F557777777777550BFBFBFBFB0555557F555555557F55550FBFBFBFBF0
        555557F555555FF7555550BFBFBF00055555575F555577755555550BFBF05555
        55555575FFF75555555555700007555555555557777555555555555555555555
        5555555555555555555555555555555555555555555555555555}
      NumGlyphs = 2
      OnClick = btnOutputBrowseClick
    end
    object boxOutputFile: TEdit
      Left = 8
      Top = 24
      Width = 241
      Height = 24
      TabOrder = 0
    end
  end
  object grpOptions: TGroupBox
    Left = 8
    Top = 160
    Width = 281
    Height = 241
    Caption = 'Options'
    TabOrder = 2
    object lblCipher: TLabel
      Left = 8
      Top = 24
      Width = 37
      Height = 16
      Caption = 'Cipher'
    end
    object lblHash: TLabel
      Left = 8
      Top = 72
      Width = 30
      Height = 16
      Caption = 'Hash'
    end
    object lblKeySize: TLabel
      Left = 8
      Top = 120
      Width = 105
      Height = 16
      Caption = 'Effective key size:'
    end
    object dblKeySize: TLabel
      Left = 116
      Top = 120
      Width = 66
      Height = 16
      Caption = 'dblKeySize'
    end
    object lblPassphrase: TLabel
      Left = 8
      Top = 144
      Width = 69
      Height = 16
      Caption = 'Passphrase'
    end
    object lblConfirmPassphrase: TLabel
      Left = 8
      Top = 192
      Width = 115
      Height = 16
      Caption = 'Confirm passphrase'
    end
    object cbxCipher: TComboBox
      Left = 8
      Top = 40
      Width = 265
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      Sorted = True
      TabOrder = 0
      OnChange = cbxCipherChange
    end
    object cbxHash: TComboBox
      Left = 8
      Top = 88
      Width = 265
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      Sorted = True
      TabOrder = 1
      OnChange = cbxCipherChange
    end
    object boxPassphrase: TEdit
      Left = 8
      Top = 160
      Width = 265
      Height = 24
      PasswordChar = '*'
      TabOrder = 2
      OnChange = boxPassphraseChange
    end
    object boxConfirmPassphrase: TEdit
      Left = 8
      Top = 208
      Width = 265
      Height = 24
      PasswordChar = '*'
      TabOrder = 3
      OnChange = boxPassphraseChange
    end
  end
  object btnEncrypt: TButton
    Left = 8
    Top = 408
    Width = 81
    Height = 25
    Caption = 'Encrypt'
    Enabled = False
    TabOrder = 3
    OnClick = btnEncryptClick
  end
  object btnDecrypt: TButton
    Left = 96
    Top = 408
    Width = 81
    Height = 25
    Caption = 'Decrypt'
    Enabled = False
    TabOrder = 4
    OnClick = btnDecryptClick
  end
  object btnClose: TButton
    Left = 208
    Top = 408
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 5
    OnClick = btnCloseClick
  end
  object DCP_blowfish1: TDCP_blowfish
    Id = 5
    Algorithm = 'Blowfish'
    MaxKeySize = 448
    BlockSize = 64
    Left = 296
    Top = 16
  end
  object DCP_cast1281: TDCP_cast128
    Id = 7
    Algorithm = 'Cast128'
    MaxKeySize = 128
    BlockSize = 64
    Left = 328
    Top = 16
  end
  object DCP_des1: TDCP_des
    Id = 23
    Algorithm = 'DES'
    MaxKeySize = 64
    BlockSize = 64
    Left = 360
    Top = 16
  end
  object DCP_3des1: TDCP_3des
    Id = 24
    Algorithm = '3DES'
    MaxKeySize = 192
    BlockSize = 64
    Left = 392
    Top = 16
  end
  object DCP_ice1: TDCP_ice
    Id = 20
    Algorithm = 'Ice'
    MaxKeySize = 64
    BlockSize = 64
    Left = 424
    Top = 16
  end
  object DCP_thinice1: TDCP_thinice
    Id = 21
    Algorithm = 'Thin Ice'
    MaxKeySize = 64
    BlockSize = 64
    Left = 456
    Top = 16
  end
  object DCP_ice21: TDCP_ice2
    Id = 22
    Algorithm = 'Ice2'
    MaxKeySize = 128
    BlockSize = 64
    Left = 488
    Top = 16
  end
  object DCP_rc21: TDCP_rc2
    Id = 1
    Algorithm = 'RC2'
    MaxKeySize = 1024
    BlockSize = 64
    Left = 296
    Top = 48
  end
  object DCP_rc41: TDCP_rc4
    Id = 19
    Algorithm = 'RC4'
    MaxKeySize = 2048
    Left = 328
    Top = 48
  end
  object DCP_rijndael1: TDCP_rijndael
    Id = 9
    Algorithm = 'Rijndael'
    MaxKeySize = 256
    BlockSize = 128
    Left = 360
    Top = 48
  end
  object DCP_serpent1: TDCP_serpent
    Id = 26
    Algorithm = 'Serpent'
    MaxKeySize = 256
    BlockSize = 128
    Left = 392
    Top = 48
  end
  object DCP_tea1: TDCP_tea
    Id = 25
    Algorithm = 'Tea'
    MaxKeySize = 128
    BlockSize = 64
    Left = 424
    Top = 48
  end
  object DCP_twofish1: TDCP_twofish
    Id = 6
    Algorithm = 'Twofish'
    MaxKeySize = 256
    BlockSize = 128
    Left = 456
    Top = 48
  end
  object DCP_haval1: TDCP_haval
    Id = 14
    Algorithm = 'Haval (256bit, 5 passes)'
    HashSize = 256
    Left = 296
    Top = 104
  end
  object DCP_md41: TDCP_md4
    Id = 17
    Algorithm = 'MD4'
    HashSize = 128
    Left = 328
    Top = 104
  end
  object DCP_md51: TDCP_md5
    Id = 16
    Algorithm = 'MD5'
    HashSize = 128
    Left = 360
    Top = 104
  end
  object DCP_ripemd1281: TDCP_ripemd128
    Id = 27
    Algorithm = 'RipeMD-128'
    HashSize = 128
    Left = 392
    Top = 104
  end
  object DCP_ripemd1601: TDCP_ripemd160
    Id = 10
    Algorithm = 'RipeMD-160'
    HashSize = 160
    Left = 424
    Top = 104
  end
  object DCP_sha11: TDCP_sha1
    Id = 2
    Algorithm = 'SHA1'
    HashSize = 160
    Left = 456
    Top = 104
  end
  object DCP_sha2561: TDCP_sha256
    Id = 28
    Algorithm = 'SHA256'
    HashSize = 256
    Left = 488
    Top = 104
  end
  object DCP_sha3841: TDCP_sha384
    Id = 29
    Algorithm = 'SHA384'
    HashSize = 384
    Left = 296
    Top = 136
  end
  object DCP_sha5121: TDCP_sha512
    Id = 30
    Algorithm = 'SHA512'
    HashSize = 512
    Left = 328
    Top = 136
  end
  object DCP_tiger1: TDCP_tiger
    Id = 18
    Algorithm = 'Tiger'
    HashSize = 192
    Left = 360
    Top = 136
  end
  object dlgInput: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    Title = 'Input file'
    Left = 296
    Top = 192
  end
  object dlgOutput: TSaveDialog
    Filter = 'All files (*.*)|*.*'
    Title = 'Output file'
    Left = 328
    Top = 192
  end
end
