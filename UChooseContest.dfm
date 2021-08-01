object ChooseContest: TChooseContest
  Left = 451
  Top = 241
  BorderStyle = bsDialog
  Caption = 'Choose a Contest'
  ClientHeight = 195
  ClientWidth = 202
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TButton
    Left = 33
    Top = 166
    Width = 63
    Height = 21
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 102
    Top = 166
    Width = 63
    Height = 21
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    Visible = False
  end
  object ContestBox: TRadioGroup
    Left = 29
    Top = 8
    Width = 140
    Height = 153
    Caption = 'Contest'
    Font.Charset = SHIFTJIS_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
    Font.Style = []
    ItemIndex = 0
    Items.Strings = (
      'ALL JA'
      '6m && Down'
      #12501#12451#12540#12523#12489#12487#12540
      #20840#24066#20840#37089
      'CQWW'
      'Simple (No Multi)')
    ParentFont = False
    TabOrder = 2
  end
end
