object formOptions: TformOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #35373#23450
  ClientHeight = 358
  ClientWidth = 331
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 12
  object panelFooter: TPanel
    Left = 0
    Top = 325
    Width = 331
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      331
      33)
    object buttonOK: TButton
      Left = 161
      Top = 4
      Width = 74
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object buttonCancel: TButton
      Left = 241
      Top = 4
      Width = 81
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = #12461#12515#12531#12475#12523
      ModalResult = 2
      TabOrder = 1
    end
  end
  object groupSecureOptions: TGroupBox
    Left = 8
    Top = 80
    Width = 313
    Height = 121
    Caption = #12475#12461#12517#12450#12458#12503#12471#12519#12531
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 56
      Width = 59
      Height = 12
      Caption = #12518#12540#12470#12540'ID'
    end
    object Label2: TLabel
      Left = 16
      Top = 82
      Width = 54
      Height = 12
      Caption = #12497#12473#12527#12540#12489
    end
    object checkUseSecure: TCheckBox
      Left = 16
      Top = 24
      Width = 129
      Height = 25
      Caption = #12475#12461#12517#12450#12514#12540#12489#12434#20351#12358
      TabOrder = 0
    end
    object editUserId: TEdit
      Left = 88
      Top = 53
      Width = 153
      Height = 20
      TabOrder = 1
    end
    object editUserPassword: TEdit
      Left = 88
      Top = 79
      Width = 153
      Height = 20
      TabOrder = 2
    end
  end
  object groupNetworkOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 313
    Height = 57
    Caption = #12493#12483#12488#12527#12540#12463#12458#12503#12471#12519#12531
    TabOrder = 0
    object Label3: TLabel
      Left = 16
      Top = 29
      Width = 31
      Height = 12
      Caption = #12509#12540#12488
    end
    object comboPort: TComboBox
      Left = 88
      Top = 26
      Width = 113
      Height = 20
      TabOrder = 0
      Items.Strings = (
        'telnet')
    end
  end
  object groupOtherOptions: TGroupBox
    Left = 8
    Top = 216
    Width = 313
    Height = 97
    Caption = #12381#12398#20182#12458#12503#12471#12519#12531
    TabOrder = 2
    object checkTakeChatLog: TCheckBox
      Left = 16
      Top = 24
      Width = 145
      Height = 25
      Caption = #12481#12515#12483#12488#12525#12464#12434#21462#24471#12377#12427
      TabOrder = 0
    end
    object checkTakeCommandLog: TCheckBox
      Left = 16
      Top = 55
      Width = 145
      Height = 25
      Caption = #12467#12510#12531#12489#12525#12464#12434#21462#24471#12377#12427
      TabOrder = 2
    end
    object checkLongDateTime: TCheckBox
      Left = 167
      Top = 24
      Width = 130
      Height = 25
      Caption = #26085#26178#12399#38263#12356#24418#24335
      TabOrder = 1
    end
    object checkUseAutoSave: TCheckBox
      Left = 167
      Top = 55
      Width = 130
      Height = 25
      Caption = #33258#21205#20445#23384
      TabOrder = 3
    end
  end
end
