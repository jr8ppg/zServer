object CliForm: TCliForm
  Left = 54
  Top = 283
  Caption = 'Client'
  ClientHeight = 162
  ClientWidth = 344
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 360
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 12
  object Panel1: TPanel
    Left = 0
    Top = 132
    Width = 344
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      344
      30)
    object SendEdit: TEdit
      Left = 6
      Top = 6
      Width = 264
      Height = 18
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      ImeName = 'MS-IME97 '#26085#26412#35486#20837#21147#65404#65405#65411#65425
      TabOrder = 0
      Text = 'SendEdit'
    end
    object SendButton: TButton
      Left = 275
      Top = 5
      Width = 63
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '&Send'
      Default = True
      TabOrder = 1
      OnClick = SendButtonClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 344
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object DisconnectButton: TButton
      Left = 6
      Top = 4
      Width = 82
      Height = 22
      Caption = '&Disconnect'
      TabOrder = 0
      OnClick = DisconnectButtonClick
    end
  end
  object ListBox: TListBox
    Left = 0
    Top = 30
    Width = 344
    Height = 102
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #65325#65331' '#12468#12471#12483#12463
    Font.Style = []
    ImeName = 'MS-IME97 '#26085#26412#35486#20837#21147#65404#65405#65411#65425
    ItemHeight = 12
    ParentFont = False
    TabOrder = 2
  end
  object CliSocket: TWSocket
    LineMode = True
    LineEnd = #13#10
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    MultiThreaded = True
    SocksLevel = '5'
    ExclusiveAddr = False
    ComponentOptions = []
    ListenBacklog = 15
    OnDataAvailable = CliSocketDataAvailable
    OnSessionClosed = CliSocketSessionClosed
    OnSessionConnected = CliSocketSessionConnected
    OnError = CliSocketError
    OnSocksError = CliSocketSocksError
    SocketErrs = wsErrTech
    onException = CliSocketException
    Left = 40
    Top = 72
  end
end
