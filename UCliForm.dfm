object CliForm: TCliForm
  Left = 54
  Top = 283
  Caption = 'Client'
  ClientHeight = 193
  ClientWidth = 365
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 11
  object Panel1: TPanel
    Left = 0
    Top = 159
    Width = 365
    Height = 34
    Align = alBottom
    TabOrder = 0
    object SendEdit: TEdit
      Left = 6
      Top = 8
      Width = 210
      Height = 18
      AutoSize = False
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #65325#65331' '#12468#12471#12483#12463
      Font.Style = []
      ImeName = 'MS-IME97 '#26085#26412#35486#20837#21147#65404#65405#65411#65425
      ParentFont = False
      TabOrder = 0
      Text = 'SendEdit'
    end
    object SendButton: TButton
      Left = 229
      Top = 8
      Width = 63
      Height = 19
      Caption = '&Send'
      Default = True
      TabOrder = 1
      OnClick = SendButtonClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 365
    Height = 35
    Align = alTop
    TabOrder = 1
    object DisconnectButton: TButton
      Left = 7
      Top = 7
      Width = 63
      Height = 18
      Caption = '&Disconnect'
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = DisconnectButtonClick
    end
    object Button1: TButton
      Left = 272
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 1
      Visible = False
      OnClick = Button1Click
    end
  end
  object ListBox: TListBox
    Left = 0
    Top = 35
    Width = 365
    Height = 124
    Align = alClient
    ImeName = 'MS-IME97 '#26085#26412#35486#20837#21147#65404#65405#65411#65425
    ItemHeight = 11
    TabOrder = 2
  end
  object CliSocket: TWSocket
    LineEnd = #13#10
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    SocksLevel = '5'
    ExclusiveAddr = False
    ComponentOptions = []
    ListenBacklog = 15
    OnDataAvailable = CliSocketDataAvailable
    OnSessionClosed = CliSocketSessionClosed
    SocketErrs = wsErrTech
    Left = 40
    Top = 88
  end
end
