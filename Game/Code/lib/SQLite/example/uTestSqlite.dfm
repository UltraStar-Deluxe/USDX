object Form1: TForm1
  Left = 199
  Top = 280
  Width = 541
  Height = 308
  Caption = 'Test SQLite 3'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 104
    Width = 28
    Height = 13
    Caption = 'Notes'
  end
  object Label2: TLabel
    Left = 24
    Top = 44
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object Label3: TLabel
    Left = 24
    Top = 72
    Width = 40
    Height = 13
    Caption = 'Number:'
  end
  object Label4: TLabel
    Left = 24
    Top = 12
    Width = 11
    Height = 13
    Caption = 'ID'
  end
  object Image1: TImage
    Left = 272
    Top = 12
    Width = 241
    Height = 165
    Proportional = True
    Stretch = True
  end
  object btnTest: TButton
    Left = 24
    Top = 224
    Width = 161
    Height = 37
    Caption = 'Test SQLite 3'
    TabOrder = 0
    OnClick = btnTestClick
  end
  object memNotes: TMemo
    Left = 24
    Top = 124
    Width = 185
    Height = 89
    Lines.Strings = (
      '')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object ebName: TEdit
    Left = 72
    Top = 40
    Width = 173
    Height = 21
    TabOrder = 2
  end
  object ebNumber: TEdit
    Left = 72
    Top = 68
    Width = 173
    Height = 21
    TabOrder = 3
  end
  object ebID: TEdit
    Left = 72
    Top = 12
    Width = 173
    Height = 21
    TabOrder = 4
  end
  object btnLoadImage: TButton
    Left = 192
    Top = 224
    Width = 157
    Height = 37
    Caption = 'Load image'
    TabOrder = 5
    OnClick = btnLoadImageClick
  end
  object btnDisplayImage: TButton
    Left = 360
    Top = 224
    Width = 157
    Height = 37
    Caption = 'Display image'
    TabOrder = 6
    OnClick = btnDisplayImageClick
  end
end
