object ServerForm: TServerForm
  Left = 285
  Top = 189
  ActiveControl = SendButton
  Caption = 'ab'
  ClientHeight = 211
  ClientWidth = 344
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 360
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 12
  object ClientListBox: TListBox
    Left = 0
    Top = 30
    Width = 344
    Height = 151
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #65325#65331' '#12468#12471#12483#12463
    Font.Style = []
    ImeName = 'MS-IME97 '#26085#26412#35486#20837#21147#65404#65405#65411#65425
    ItemHeight = 12
    ParentFont = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 344
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      344
      30)
    object Button1: TButton
      Left = 152
      Top = 8
      Width = 75
      Height = 17
      Caption = 'Button1'
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Visible = False
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 276
      Top = 4
      Width = 63
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '&Clear'
      TabOrder = 1
      OnClick = Button2Click
    end
    object CheckBox2: TCheckBox
      Left = 6
      Top = 7
      Width = 113
      Height = 17
      Caption = 'Monitor chat only'
      TabOrder = 2
      OnClick = CheckBox2Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 181
    Width = 344
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      344
      30)
    object SendButton: TButton
      Left = 276
      Top = 5
      Width = 63
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '&Send'
      Default = True
      TabOrder = 0
      OnClick = SendButtonClick
    end
    object SendEdit: TEdit
      Left = 6
      Top = 6
      Width = 265
      Height = 18
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      AutoSelect = False
      AutoSize = False
      ImeName = 'MS-IME97 '#26085#26412#35486#20837#21147#65404#65405#65411#65425
      TabOrder = 1
    end
  end
  object SrvSocket: TWSocket
    LineEnd = #13#10
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    SocksLevel = '5'
    ExclusiveAddr = False
    ComponentOptions = []
    ListenBacklog = 15
    OnSessionAvailable = SrvSocketSessionAvailable
    SocketErrs = wsErrTech
    Left = 24
    Top = 56
  end
  object MainMenu1: TMainMenu
    Left = 224
    Top = 8
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open File'
        OnClick = Open1Click
      end
      object MergeFile1: TMenuItem
        Caption = '&Merge File'
        Enabled = False
        OnClick = MergeFile1Click
      end
      object mLog: TMenuItem
        Caption = 'Start &Log'
        OnClick = mLogClick
      end
      object Save1: TMenuItem
        Caption = '&Save'
        OnClick = Save1Click
      end
      object SaveAs1: TMenuItem
        Caption = 'Save &As'
        OnClick = SaveAs1Click
      end
      object DeleteDupes1: TMenuItem
        Caption = 'Delete Dupes'
        OnClick = DeleteDupes1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Windows1: TMenuItem
      Caption = '&Windows'
      OnClick = Windows1Click
      object ScoreandStatistics1: TMenuItem
        Caption = '&Score and Statistics'
        OnClick = ScoreandStatistics1Click
      end
      object Multipliers1: TMenuItem
        Caption = '&Multipliers'
        OnClick = Multipliers1Click
      end
      object Connections1: TMenuItem
        Caption = '&Connections'
        OnClick = Connections1Click
      end
      object CurrentFrequencies1: TMenuItem
        Caption = 'Current &Frequencies'
        OnClick = CurrentFrequencies1Click
      end
      object Graph1: TMenuItem
        Caption = 'Graph'
        Visible = False
        OnClick = Graph1Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object Contents1: TMenuItem
        Caption = '&Contents'
        Visible = False
      end
      object N1: TMenuItem
        Caption = '-'
        Visible = False
      end
      object About1: TMenuItem
        Caption = '&About'
        OnClick = About1Click
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 50
    OnTimer = Timer1Timer
    Left = 64
    Top = 112
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'zlo'
    Filter = 'zLog binary files (*.ZLO)|*.ZLO'
    Left = 232
    Top = 88
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'ZLO'
    Filter = 'zLog binary files (*.ZLO)|*.ZLO'
    Left = 144
    Top = 72
  end
end
