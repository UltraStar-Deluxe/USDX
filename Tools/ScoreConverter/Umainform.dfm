object mainform: Tmainform
  Left = 328
  Top = 228
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Ultrastar Deluxe Score Converter'
  ClientHeight = 159
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 60
    Height = 13
    Caption = 'SongFolder: '
  end
  object lFolder: TLabel
    Left = 8
    Top = 24
    Width = 29
    Height = 13
    Caption = 'Folder'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 49
    Height = 13
    Caption = 'Database:'
  end
  object lDatabase: TLabel
    Left = 8
    Top = 64
    Width = 46
    Height = 13
    Caption = 'Database'
  end
  object lDatabase2: TLabel
    Left = 72
    Top = 48
    Width = 54
    Height = 13
    Caption = 'lDatabase2'
  end
  object lFolder2: TLabel
    Left = 72
    Top = 8
    Width = 37
    Height = 13
    Caption = 'lFolder2'
  end
  object lStatus: TLabel
    Left = 0
    Top = 96
    Width = 449
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'lStatus'
  end
  object bFLoad: TButton
    Left = 176
    Top = 8
    Width = 57
    Height = 17
    Caption = 'Load'
    TabOrder = 0
    OnClick = bFLoadClick
  end
  object bDLoad: TButton
    Left = 176
    Top = 48
    Width = 57
    Height = 17
    Caption = 'Load'
    TabOrder = 1
    OnClick = bDLoadClick
  end
  object bToDB: TButton
    Left = 16
    Top = 112
    Width = 153
    Height = 17
    Caption = 'Convert *.SCO to Database'
    Enabled = False
    TabOrder = 2
    OnClick = bToDBClick
  end
  object bFromDB: TButton
    Left = 288
    Top = 112
    Width = 145
    Height = 17
    Caption = 'Convert Database to *.SCO'
    Enabled = False
    TabOrder = 3
    OnClick = bFromDBClick
  end
  object pProgress: TProgressBar
    Left = 8
    Top = 136
    Width = 433
    Height = 17
    TabOrder = 4
  end
  object oDatabase: TOpenDialog
    Filter = 'Ultrastar Deluxe Database|ultrastar.db'
    Left = 136
    Top = 48
  end
end
