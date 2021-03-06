object MergeBand: TMergeBand
  Left = 347
  Top = 145
  BorderStyle = bsDialog
  Caption = 'Merge file'
  ClientHeight = 343
  ClientWidth = 173
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 118
    Height = 12
    Caption = 'Check bands to update'
  end
  object Label2: TLabel
    Left = 16
    Top = 24
    Width = 139
    Height = 12
    Caption = 'All the data in the checked'
  end
  object Label3: TLabel
    Left = 16
    Top = 40
    Width = 114
    Height = 12
    Caption = 'bands will be replaced'
  end
  object cb19: TCheckBox
    Left = 35
    Top = 58
    Width = 97
    Height = 17
    Caption = '1.9 MHz'
    TabOrder = 0
  end
  object cb35: TCheckBox
    Tag = 1
    Left = 35
    Top = 77
    Width = 97
    Height = 17
    Caption = '3.5 MHz'
    TabOrder = 1
  end
  object cb7: TCheckBox
    Tag = 2
    Left = 35
    Top = 95
    Width = 97
    Height = 17
    Caption = '7 MHz'
    TabOrder = 2
  end
  object cb14: TCheckBox
    Tag = 4
    Left = 35
    Top = 114
    Width = 97
    Height = 17
    Caption = '14 MHz'
    TabOrder = 3
  end
  object cb21: TCheckBox
    Tag = 6
    Left = 35
    Top = 133
    Width = 97
    Height = 17
    Caption = '21 MHz'
    TabOrder = 4
  end
  object cb28: TCheckBox
    Tag = 8
    Left = 35
    Top = 151
    Width = 97
    Height = 17
    Caption = '28 MHz'
    TabOrder = 5
  end
  object cb50: TCheckBox
    Tag = 9
    Left = 35
    Top = 170
    Width = 97
    Height = 17
    Caption = '50 MHz'
    TabOrder = 6
  end
  object cb144: TCheckBox
    Tag = 10
    Left = 35
    Top = 189
    Width = 97
    Height = 17
    Caption = '144 MHz'
    TabOrder = 7
  end
  object cb430: TCheckBox
    Tag = 11
    Left = 35
    Top = 207
    Width = 97
    Height = 17
    Caption = '430 MHz'
    TabOrder = 8
  end
  object cb1200: TCheckBox
    Tag = 12
    Left = 35
    Top = 226
    Width = 97
    Height = 17
    Caption = '1200 MHz'
    TabOrder = 9
  end
  object cb2400: TCheckBox
    Tag = 13
    Left = 35
    Top = 245
    Width = 97
    Height = 17
    Caption = '2400 MHz'
    TabOrder = 10
  end
  object cb10g: TCheckBox
    Tag = 15
    Left = 35
    Top = 282
    Width = 97
    Height = 17
    Caption = '10 GHz && up'
    TabOrder = 11
  end
  object Panel1: TPanel
    Left = 0
    Top = 310
    Width = 173
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 12
    object CancelBtn: TButton
      Left = 93
      Top = 4
      Width = 63
      Height = 21
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
      OnClick = CancelBtnClick
    end
    object OKButton: TButton
      Left = 13
      Top = 4
      Width = 63
      Height = 21
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = OKButtonClick
    end
  end
  object cb5600: TCheckBox
    Tag = 14
    Left = 35
    Top = 264
    Width = 97
    Height = 17
    Caption = '5600 MHz'
    TabOrder = 13
  end
end
