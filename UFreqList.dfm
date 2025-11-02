object FreqList: TFreqList
  Left = 324
  Top = 179
  Caption = 'Current Frequencies'
  ClientHeight = 184
  ClientWidth = 575
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  TextHeight = 15
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 575
    Height = 184
    Align = alClient
    Columns = <
      item
        Caption = #30058#21495
      end
      item
        Caption = #12496#12531#12489
        Width = 60
      end
      item
        Caption = #21608#27874#25968
        Width = 100
      end
      item
        Caption = #12514#12540#12489
        Width = 60
      end
      item
        Caption = 'CQ/SP'
        Width = 60
      end
      item
        Caption = #26356#26032#26178#21051
        Width = 80
      end
      item
        Caption = 'PC'#21517
        Width = 80
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
end
