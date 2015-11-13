object frmMain: TfrmMain
  Left = 224
  Top = 110
  Width = 416
  Height = 450
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'DCPcrypt File Hashing Demo'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    408
    416)
  PixelsPerInch = 96
  TextHeight = 16
  object grpInputFile: TGroupBox
    Left = 8
    Top = 8
    Width = 393
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Input File'
    TabOrder = 0
    DesignSize = (
      393
      57)
    object btnBrowseFiles: TSpeedButton
      Left = 360
      Top = 24
      Width = 24
      Height = 24
      Anchors = [akTop, akRight]
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
      OnClick = btnBrowseFilesClick
    end
    object boxInputFile: TEdit
      Left = 8
      Top = 24
      Width = 345
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
  object grpHashes: TGroupBox
    Left = 8
    Top = 72
    Width = 393
    Height = 153
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Hashes'
    TabOrder = 1
    DesignSize = (
      393
      153)
    object lstHashes: TCheckListBox
      Left = 8
      Top = 24
      Width = 289
      Height = 121
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 16
      Sorted = True
      TabOrder = 0
    end
    object btnHash: TButton
      Left = 304
      Top = 120
      Width = 81
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Hash Files'
      TabOrder = 1
      OnClick = btnHashClick
    end
  end
  object grpOutput: TGroupBox
    Left = 8
    Top = 232
    Width = 393
    Height = 177
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Output'
    TabOrder = 2
    DesignSize = (
      393
      177)
    object txtOutput: TMemo
      Left = 8
      Top = 24
      Width = 377
      Height = 145
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object DCP_haval1: TDCP_haval
    Id = 14
    Algorithm = 'Haval (256bit, 5 passes)'
    HashSize = 256
    Left = 24
    Top = 104
  end
  object DCP_md41: TDCP_md4
    Id = 17
    Algorithm = 'MD4'
    HashSize = 128
    Left = 56
    Top = 104
  end
  object DCP_md51: TDCP_md5
    Id = 16
    Algorithm = 'MD5'
    HashSize = 128
    Left = 88
    Top = 104
  end
  object DCP_ripemd1281: TDCP_ripemd128
    Id = 27
    Algorithm = 'RipeMD-128'
    HashSize = 128
    Left = 120
    Top = 104
  end
  object DCP_ripemd1601: TDCP_ripemd160
    Id = 10
    Algorithm = 'RipeMD-160'
    HashSize = 160
    Left = 152
    Top = 104
  end
  object DCP_sha11: TDCP_sha1
    Id = 2
    Algorithm = 'SHA1'
    HashSize = 160
    Left = 24
    Top = 136
  end
  object DCP_sha2561: TDCP_sha256
    Id = 28
    Algorithm = 'SHA256'
    HashSize = 256
    Left = 56
    Top = 136
  end
  object DCP_sha3841: TDCP_sha384
    Id = 29
    Algorithm = 'SHA384'
    HashSize = 384
    Left = 88
    Top = 136
  end
  object DCP_sha5121: TDCP_sha512
    Id = 30
    Algorithm = 'SHA512'
    HashSize = 512
    Left = 120
    Top = 136
  end
  object DCP_tiger1: TDCP_tiger
    Id = 18
    Algorithm = 'Tiger'
    HashSize = 192
    Left = 152
    Top = 136
  end
  object dlgOpen: TOpenDialog
    Filter = 'All Files (*.*)|*.*'
    Title = 'Choose input file'
    Left = 24
    Top = 168
  end
end
