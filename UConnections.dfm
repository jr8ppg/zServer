object Connections: TConnections
  Left = 150
  Top = 197
  Caption = 'Connections'
  ClientHeight = 165
  ClientWidth = 438
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 438
    Height = 165
    Align = alClient
    Columns = <
      item
        Caption = #30058#21495
      end
      item
        Caption = #12450#12489#12524#12473
        Width = 80
      end
      item
        Caption = #25509#32154#26178#21051
        Width = 80
      end
      item
        Caption = #20999#26029#26178#21051
        Width = 80
      end
      item
        Caption = #12496#12531#12489
        Width = 80
      end
      item
        Caption = #12458#12506#12524#12540#12479#12540
        Width = 120
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = ListView1DblClick
  end
end
