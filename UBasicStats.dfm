object BasicStats: TBasicStats
  Left = 197
  Top = 196
  Caption = 'BasicStats'
  ClientHeight = 176
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Grid: TMgrid
    Left = 8
    Top = 8
    Width = 329
    Height = 169
    TabStop = False
    BorderStyle = bsNone
    Color = clBtnFace
    ColCount = 4
    DefaultColWidth = 56
    DefaultRowHeight = 16
    Enabled = False
    FixedCols = 0
    FixedRows = 0
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    GridLineWidth = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 0
    Alignment = taRightJustify
    BorderColor = clSilver
    OddRowColor = clBtnFace
    EvenRowColor = clBtnFace
    OnSetting = GridSetting
  end
end
