object ServerForm: TServerForm
  Left = 285
  Top = 189
  ActiveControl = SendButton
  Caption = 'ab'
  ClientHeight = 211
  ClientWidth = 348
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 360
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 12
  object ClientListBox: TListBox
    Left = 0
    Top = 30
    Width = 348
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
    ExplicitWidth = 344
    ExplicitHeight = 150
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 348
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 344
    DesignSize = (
      348
      30)
    object buttonMergeLock: TSpeedButton
      Left = 152
      Top = 3
      Width = 81
      Height = 25
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'MERGE LOCK'
      Visible = False
      OnClick = buttonMergeLockClick
    end
    object Button2: TButton
      Left = 280
      Top = 4
      Width = 63
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '&Clear'
      TabOrder = 0
      OnClick = Button2Click
      ExplicitLeft = 276
    end
    object CheckBox2: TCheckBox
      Left = 6
      Top = 7
      Width = 113
      Height = 17
      Caption = 'Monitor chat only'
      TabOrder = 1
      OnClick = CheckBox2Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 181
    Width = 348
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 180
    ExplicitWidth = 344
    DesignSize = (
      348
      30)
    object SendButton: TButton
      Left = 280
      Top = 5
      Width = 63
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '&Send'
      Default = True
      TabOrder = 0
      OnClick = SendButtonClick
      ExplicitLeft = 276
    end
    object SendEdit: TEdit
      Left = 6
      Top = 6
      Width = 269
      Height = 18
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      AutoSelect = False
      AutoSize = False
      ImeName = 'MS-IME97 '#26085#26412#35486#20837#21147#65404#65405#65411#65425
      TabOrder = 1
      ExplicitWidth = 265
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
    OnError = SrvSocketError
    OnSocksError = SrvSocketSocksError
    SocketErrs = wsErrTech
    onException = SrvSocketException
    Left = 24
    Top = 56
  end
  object MainMenu1: TMainMenu
    AutoHotkeys = maManual
    AutoLineReduction = maManual
    Left = 240
    Top = 40
    object File1: TMenuItem
      Caption = '&File'
      OnClick = File1Click
      object Open1: TMenuItem
        Caption = #38283#12367
        OnClick = Open1Click
      end
      object MergeFile1: TMenuItem
        Caption = #12510#12540#12472
        Enabled = False
        OnClick = MergeFile1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Save1: TMenuItem
        Caption = #20445#23384
        OnClick = Save1Click
      end
      object SaveAs1: TMenuItem
        Caption = #21517#21069#12434#12388#12369#12390#20445#23384
        OnClick = SaveAs1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object DeleteDupes1: TMenuItem
        Caption = #37325#35079#12487#12540#12479#12434#21066#38500
        OnClick = DeleteDupes1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object menuTakeChatLog: TMenuItem
        AutoCheck = True
        Caption = #12481#12515#12483#12488#12525#12464#12434#35352#37682
        Checked = True
        GroupIndex = 1
        OnClick = menuTakeChatLogClick
      end
      object menuTakeCommandLog: TMenuItem
        AutoCheck = True
        Caption = #12467#12510#12531#12489#12525#12464#12434#35352#37682
        GroupIndex = 2
        OnClick = menuTakeCommandLogClick
      end
      object N5: TMenuItem
        Caption = '-'
        GroupIndex = 2
      end
      object Exit1: TMenuItem
        Caption = #32066#20102
        GroupIndex = 2
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
    Filter = 'zLog Extended binary files (*.ZLOX)|*.ZLOX'
    Left = 232
    Top = 88
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'ZLO'
    Filter = 'zLog binary files (*.ZLO;*.ZLOX)|*.ZLO;*.ZLOX'
    Left = 144
    Top = 72
  end
end
