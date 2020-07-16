object AboutBox: TAboutBox
  Left = 319
  Top = 309
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 242
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    344
    242)
  PixelsPerInch = 96
  TextHeight = 12
  object Panel1: TPanel
    Left = 8
    Top = 7
    Width = 330
    Height = 136
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    TabOrder = 0
    object ProgramIcon: TImage
      Left = 15
      Top = 15
      Width = 32
      Height = 32
      Picture.Data = {
        055449636F6E0000010001002020100000000000E80200001600000028000000
        2000000040000000010004000000000080020000000000000000000000000000
        0000000000000000000080000080000000808000800000008000800080800000
        C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
        FFFFFF0000000000000000000000000000000000088000880000000000000000
        00000000000000000909099000000000000000000EEEE0AA0000069000000000
        000000000EEEE0AA090906900000000000000000000000000000009000000000
        0000000008888888888800900000000000000000000000000000009000000000
        0000000000000000000000900000000000000000000000000000009000000000
        0000000600000000000000967777707777777778000000000000009677777808
        8777888808800088000000967777780007778000000000000909009677777877
        777777700EEEE0AA000009997777787CCCCCCC780EEEE0AA090900967777787C
        CCCCCC7800000000000000967777787CCCCCCC7808888888888800967777787C
        CCCCCC78000000000000009677A7A87CCCCCCC7800000000000000967777787C
        CCCCCC7800000000000000967777787777777778000000000000009068888808
        8888888800000000000000900000000000000000088000880000009000000000
        00000000000000000909009000000000000000000EEEE0AA0000099000000000
        000000000EEEE0AA090900000000000000000000000000000000000000000000
        0000000008888888888800000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000007FFFF0007FFFF0001FFFF0001FFFF0001FFFF0005FFFF800DFFFF
        C01DFFFFFFFDFFFFFFFDFFFEFFFC040000040200000403870004000100000000
        0004000000040000800C0000C01C0000FFFC0000FFFC0000FFFD02000005FFFF
        0005FFFF0005FFFF0001FFFF0007FFFF0007FFFF800FFFFFC01FFFFFFFFFFFFF
        FFFFFFFF}
      IsControl = True
    end
    object ProductName: TLabel
      Left = 90
      Top = 8
      Width = 168
      Height = 44
      Caption = 'Z-Server  '
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -31
      Font.Name = 'Comic Sans MS'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
      IsControl = True
    end
    object Copyright: TLabel
      Left = 7
      Top = 92
      Width = 314
      Height = 12
      Caption = 'Copyright 1997-2002 by Yohei Yokobayashi AD6AJ/JJ1MED'
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      IsControl = True
    end
    object Comments: TLabel
      Left = 7
      Top = 104
      Width = 144
      Height = 12
      Caption = 'Comments to : zlog@zlog.org'
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      WordWrap = True
      IsControl = True
    end
    object Label2: TLabel
      Left = 7
      Top = 68
      Width = 70
      Height = 12
      Caption = 'April 21, 2002'
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      IsControl = True
    end
  end
  object OKButton: TButton
    Left = 144
    Top = 215
    Width = 63
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    Font.Charset = SHIFTJIS_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 1
    OnClick = OKButtonClick
    ExplicitTop = 235
  end
  object Panel2: TPanel
    Left = 8
    Top = 149
    Width = 330
    Height = 60
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object label1: TLabel
      Left = 6
      Top = 5
      Width = 317
      Height = 12
      Alignment = taCenter
      AutoSize = False
      Caption = 'ver xx'
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      IsControl = True
    end
    object Label7: TLabel
      Left = 8
      Top = 23
      Width = 287
      Height = 13
      Caption = 'Portions created by JR8PPG are Copyright (C) 2020 JR8PPG'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      IsControl = True
    end
    object LinkLabel1: TLinkLabel
      Left = 8
      Top = 40
      Width = 179
      Height = 16
      Caption = 
        '<A HREF="https://github.com/jr8ppg/zServer">https://github.com/j' +
        'r8ppg/zServer</A>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnLinkClick = LinkLabel1LinkClick
    end
  end
end
