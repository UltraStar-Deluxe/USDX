object Form1: TForm1
  Left = 250
  Top = 200
  Width = 783
  Height = 540
  VertScrollBar.Range = 101
  ActiveControl = Button1
  Caption = 'CurlPas demo'
  Color = clBackground
  OnCreate = Form1Create
  Position = poScreenCenter
  PixelsPerInch = 91
  TextHeight = 17
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 783
    Height = 41
    Align = alTop
    TabOrder = 0
    object Button1: TButton
      Left = 624
      Top = 8
      Width = 43
      Height = 25
      Caption = 'GO'
      Default = True
      TabOrder = 0
      OnClick = Button1Click
    end
    object Label1: TLabel
      Left = 32
      Top = 16
      Width = 30
      Height = 17
      Caption = 'URL:'
    end
    object Edit1: TEdit
      Left = 72
      Top = 8
      Width = 545
      Height = 25
      TabOrder = 2
      Text = 'http://www.ietf.org/rfc/rfc1000.txt'
    end
    object CheckBox1: TCheckBox
      Left = 680
      Top = 8
      Width = 100
      Height = 30
      Caption = 'Threaded'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 480
    Width = 783
    Height = 60
    Align = alBottom
    TabOrder = 1
    object ProgressBar1: TProgressBar
      Left = 1
      Top = 41
      Width = 781
      Height = 18
      Align = alBottom
    end
    object Label2: TLabel
      Left = 32
      Top = 16
      Width = 9
      Height = 17
      Caption = '...'
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 783
    Height = 439
    Align = alClient
    TabOrder = 2
  end
end
