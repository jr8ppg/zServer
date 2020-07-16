object BasicStats: TBasicStats
  Left = 197
  Top = 196
  Caption = 'BasicStats'
  ClientHeight = 176
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Grid: TStringGrid
    Left = 0
    Top = 0
    Width = 339
    Height = 176
    TabStop = False
    Align = alClient
    BorderStyle = bsNone
    Color = clBtnFace
    ColCount = 4
    DefaultColWidth = 56
    DefaultRowHeight = 16
    DefaultDrawing = False
    Enabled = False
    FixedCols = 0
    FixedRows = 0
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    GridLineWidth = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 0
    OnDrawCell = GridDrawCell
  end
end
