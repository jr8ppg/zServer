inherited ALLJAMultiForm: TALLJAMultiForm
  Left = 57
  Top = 319
  Caption = 'Multipliers'
  ClientHeight = 197
  ClientWidth = 437
  ExplicitWidth = 453
  ExplicitHeight = 236
  PixelsPerInch = 96
  TextHeight = 12
  object TabControl: TTabControl
    Left = 0
    Top = 0
    Width = 437
    Height = 197
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      '1.9 MHz'
      '3.5 MHz'
      '7 MHz'
      '14 MHz'
      '21 MHz'
      '28 MHz'
      '50 MHz'
      'All')
    TabIndex = 0
    OnChange = TabControlChange
    object CheckListBox: TCheckListBox
      Left = 8
      Top = 64
      Width = 113
      Height = 169
      OnClickCheck = CheckListBoxClickCheck
      Columns = 3
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #65325#65331' '#12468#12471#12483#12463
      Font.Style = []
      ImeName = 'MS-IME97 '#26085#26412#35486#20837#21147#65404#65405#65411#65425
      ItemHeight = 13
      ParentFont = False
      TabOrder = 0
    end
    object ListBox: TListBox
      Left = 128
      Top = 64
      Width = 121
      Height = 169
      Font.Charset = SHIFTJIS_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #65325#65331' '#12468#12471#12483#12463
      Font.Style = []
      ImeName = 'MS-IME97 '#26085#26412#35486#20837#21147#65404#65405#65411#65425
      ItemHeight = 12
      ParentFont = False
      TabOrder = 1
    end
    object Panel1: TPanel
      Left = 4
      Top = 24
      Width = 429
      Height = 34
      Align = alTop
      TabOrder = 2
    end
  end
end
