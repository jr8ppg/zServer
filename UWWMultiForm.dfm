inherited WWMultiForm: TWWMultiForm
  Left = 451
  Top = 109
  Caption = 'Multipliers'
  ClientWidth = 333
  OnDestroy = FormDestroy
  OnShow = FormShow
  ExplicitWidth = 349
  PixelsPerInch = 96
  TextHeight = 12
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 333
    Height = 41
    Align = alTop
    TabOrder = 0
    object RotateLabel1: TRotateLabel
      Left = 252
      Top = 20
      Width = 15
      Height = 14
      Escapement = 90
      TextStyle = tsNone
      Caption = '1.9'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RotateLabel2: TRotateLabel
      Left = 264
      Top = 20
      Width = 15
      Height = 14
      Escapement = 90
      TextStyle = tsNone
      Caption = '3.5'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RotateLabel3: TRotateLabel
      Left = 276
      Top = 29
      Width = 6
      Height = 14
      Escapement = 90
      TextStyle = tsNone
      Caption = '7'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RotateLabel4: TRotateLabel
      Left = 287
      Top = 23
      Width = 12
      Height = 14
      Escapement = 90
      TextStyle = tsNone
      Caption = '14'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RotateLabel5: TRotateLabel
      Left = 299
      Top = 23
      Width = 12
      Height = 14
      Escapement = 90
      TextStyle = tsNone
      Caption = '21'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RotateLabel6: TRotateLabel
      Left = 311
      Top = 23
      Width = 12
      Height = 14
      Escapement = 90
      TextStyle = tsNone
      Caption = '28'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object SortBy: TRadioGroup
      Left = 8
      Top = 3
      Width = 121
      Height = 30
      Caption = 'Sort by'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Prefix'
        'Zone')
      TabOrder = 0
      OnClick = SortByClick
    end
  end
  object Grid: TStringGrid
    Left = 0
    Top = 41
    Width = 333
    Height = 71
    Align = alClient
    ColCount = 1
    DefaultColWidth = 500
    DefaultRowHeight = 14
    FixedCols = 0
    FixedRows = 0
    Font.Charset = SHIFTJIS_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #65325#65331' '#12468#12471#12483#12463
    Font.Style = []
    GridLineWidth = 0
    Options = [goRowSelect]
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 112
    Width = 333
    Height = 41
    Align = alBottom
    TabOrder = 2
    object Button1: TButton
      Left = 8
      Top = 10
      Width = 65
      Height = 22
      Caption = 'OK'
      TabOrder = 0
      OnClick = Button1Click
    end
    object GoButton: TButton
      Left = 264
      Top = 11
      Width = 57
      Height = 21
      Caption = 'Go'
      TabOrder = 1
      OnClick = GoButtonClick
    end
    object Edit1: TEdit
      Left = 224
      Top = 11
      Width = 33
      Height = 21
      AutoSize = False
      CharCase = ecUpperCase
      ImeName = 'MS-IME97 '#26085#26412#35486#20837#21147#65404#65405#65411#65425
      TabOrder = 2
      OnKeyPress = Edit1KeyPress
    end
    object cbStayOnTop: TCheckBox
      Left = 80
      Top = 11
      Width = 81
      Height = 17
      Caption = 'Stay on top'
      TabOrder = 3
      OnClick = cbStayOnTopClick
    end
  end
end
