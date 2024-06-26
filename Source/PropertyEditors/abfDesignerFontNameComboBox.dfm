object frmDesignerFontNameComboBox: TfrmDesignerFontNameComboBox
  Left = 211
  Top = 142
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'OneTouch� Designer'
  ClientHeight = 299
  ClientWidth = 468
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00FFF00000000000FF6F6FF0000000FF666F666FF000FF666F6F6F666FF0F6F6
    FF6F6FF666F0F6FFFF6F6FFF66F0F6FFF66F6F6666F0F66FF66F6FFF66F0F66F
    66FFF66FF6F0F666FF666FF666F0F6FF666FF66FF6F0FF666FF66FF66FF0FF66
    FF6FFF666FF000FF66FF666FF0000000FF666FF00000000000FFF0000000FC7F
    4804F01F8080C00780800001FFFF00018080000180800001FFFF0001FFFF0001
    8080000180800001FFFF0001FFFF00018080C0078080F01FFFFFFC7F8080}
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object grpItemStyle: TGroupBox
    Left = 8
    Top = 4
    Width = 146
    Height = 174
    Caption = 'Item style'
    TabOrder = 0
    object lbItemSample: TLabel
      Left = 12
      Top = 102
      Width = 69
      Height = 15
      Caption = '&Item sample'
      FocusControl = edItemSample
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object btnItemStyle0: TSpeedButton
      Left = 8
      Top = 16
      Width = 130
      Height = 28
      Hint = 'fisFont'
      GroupIndex = 1
      Glyph.Data = {
        76050000424D760500000000000076000000280000007A000000140000000100
        0400000000000005000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00D00000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000000D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FF00FFFFFFFFFF007F700F00FFF00F800800F00FFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFF8888
        8800FFFFFFFFFF807F708F00FFF00F007800F00FFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFF88
        FF00FFFFFFFFFF8000008F00FFF00F00FF00F00FFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFF80
        FF00FF0FFFFFFF708F807F00FFF00F808700F00FFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFF80
        FF00FF0FFFFFFFF00F00FF00FFF00FF78800F00FFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFF80
        0F00F00FFFFFFFF80800FF007FF00F87F700F00FFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFF8FF80
        0000000FFFFFFFF80808FF0088F00F780007F00FFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFF8FF88
        FF8FFFFFFFFFFFF80008FFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFF88F88
        F88FFFFFFFFFFFF70007FFFFFFF00FFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFF88888
        888FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D00000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000000D000000}
      OnClick = ItemStyleClick
    end
    object btnItemStyle1: TSpeedButton
      Tag = 1
      Left = 8
      Top = 44
      Width = 130
      Height = 28
      Hint = 'fisNameOnly'
      GroupIndex = 1
      Glyph.Data = {
        76050000424D760500000000000076000000280000007A000000140000000100
        0400000000000005000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00D00000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000000D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFF007F7
        00F00FFF00F800800F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFF807F7
        08F00FFF00F007800F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFF80000
        08F00FFF00F00FF00F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFF708F8
        07F00FFF00F808700F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFF00F0
        0FF00FFF00FF78800F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFF8080
        0FF007FF00F87F700F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFF8080
        8FF0088F00F780007F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFF8000
        8FFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFF7000
        7FFFFFFF00FFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D00000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000000D000000}
      OnClick = ItemStyleClick
    end
    object btnItemStyle2: TSpeedButton
      Tag = 2
      Left = 8
      Top = 72
      Width = 130
      Height = 28
      Hint = 'fisSample'
      GroupIndex = 1
      Glyph.Data = {
        76050000424D760500000000000076000000280000007A000000140000000100
        0400000000000005000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00D00000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000000D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFF8888
        8800FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFF88
        FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFF80
        FF00FF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFF80
        FF00FF0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFF80
        0F00F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFF8FF80
        0000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFF8FF88
        FF8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFF88F88
        F88FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFF88888
        888FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D0FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0D000000D00000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000000D000000}
      OnClick = ItemStyleClick
    end
    object edItemSample: TEdit
      Left = 12
      Top = 118
      Width = 122
      Height = 23
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = edItemSampleChange
    end
    object chbUseSameFont: TCheckBox
      Left = 12
      Top = 148
      Width = 129
      Height = 17
      Caption = 'One font fo&r items'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = chbUseSameFontClick
    end
    object pnItemStyleSample: TPanel
      Left = 38
      Top = 78
      Width = 92
      Height = 14
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = 'AaBbCc'
      Color = clWhite
      Enabled = False
      TabOrder = 2
    end
  end
  object grpSample: TGroupBox
    Left = 8
    Top = 182
    Width = 146
    Height = 74
    Caption = 'Sample'
    TabOrder = 1
    object Sample: TabfFontNameComboBox
      Left = 12
      Top = 20
      Width = 122
      Height = 23
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      FontDevice = fdBoth
      FontItemSample = 'AaBb����'
      FontItemStyle = fisNameOnly
      FontName = 'MS Sans Serif'
      ParentFont = False
      TabOrder = 0
    end
  end
  object btnOk: TBitBtn
    Left = 300
    Top = 266
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    NumGlyphs = 2
  end
  object btnCancel: TBitBtn
    Left = 385
    Top = 266
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
    NumGlyphs = 2
  end
  object grpDevice: TGroupBox
    Left = 300
    Top = 160
    Width = 160
    Height = 96
    Caption = 'Device'
    TabOrder = 2
    object btnDevice0: TSpeedButton
      Left = 8
      Top = 16
      Width = 144
      Height = 24
      Hint = 'fdScreen'
      GroupIndex = 1
      Caption = '&Screen                       '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDD8888888888880DDD87777777777780DDD80707070707080DDD8080808080
        8880DDD8777777777780DDD8888888888080DDD8F77777777800DDD8F0666666
        7880DDD8F06666667844DDD8F06666667880DDD8F06666667880DDD8F0000000
        7880DDD8FFFFFFF77880DDD8777777777780DDDD88888888888D}
      ParentFont = False
      OnClick = DeviceClick
    end
    object btnDevice1: TSpeedButton
      Tag = 1
      Left = 8
      Top = 40
      Width = 144
      Height = 24
      Hint = 'fdPrinter'
      GroupIndex = 1
      Caption = '&Printer                        '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDDD00000000000DDDD0777777777070DD000000000000070D0777777BBB77
        000D077777788877070D00000000000007700777777777707070D00000000007
        0700DD0FFFFFFFF07070DDD0F00000F0000DDDD0FFFFFFFF0DDDDDDD0F00000F
        0DDDDDDD0FFFFFFFF0DDDDDDD000000000DDDDDDDDDDDDDDDDDD}
      ParentFont = False
      OnClick = DeviceClick
    end
    object btnDevice2: TSpeedButton
      Tag = 2
      Left = 8
      Top = 64
      Width = 144
      Height = 24
      Hint = 'fdBoth'
      GroupIndex = 1
      Caption = 'Bot&h types        '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        F6010000424DF601000000000000760000002800000030000000100000000100
        0400000000008001000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD8888888888880DDDDDDDDDDDDDD
        DDDDD00000000000DDDDD87777777777780DDDDDDDDDDDDDDDDD077777777707
        0DDDDD80707070707080DDDDDDDDDDDDDDD000000000000070DDDDD808080808
        08880DDDDD000DD0DDD0777777BBB77000DDDDDD8777777777780DDDD0DDD00D
        DDD077777788877070DDDDDD8888888888080DDDD0DDD0DDDDD0000000000000
        770DDDDD8F77777777800DDDD0DD0D0DDDD0777777777707070DDDDD8F066666
        67880DDDDD00DDDDDDDD000000000070700DDDDD8F06666667844DDDDD0D0DDD
        DDDDD0FFFFFFFF07070DDDDD8F06666667880DDDDD0DD0DDDDDDDD0F00000F00
        00DDDDDD8F06666667880DDDDD0DD0DDDDDDDD0FFFFFFFF0DDDDDDDD8F000000
        07880DDDDDD00DDDDDDDDDD0F00000F0DDDDDDDD8FFFFFFF77880DDDDDDDDDDD
        DDDDDDD0FFFFFFFF0DDDDDDD8777777777780DDDDDDDDDDDDDDDDDDD00000000
        0DDDDDDDD88888888888DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD}
      ParentFont = False
      OnClick = DeviceClick
    end
  end
  object grpFontOptions: TGroupBox
    Left = 164
    Top = 116
    Width = 126
    Height = 140
    Caption = 'Font options'
    TabOrder = 3
    object chbFontOptions0: TCheckBox
      Left = 12
      Top = 16
      Width = 108
      Height = 17
      Caption = '&ANSI only'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = chbFontOptionsClick
    end
    object chbFontOptions1: TCheckBox
      Tag = 1
      Left = 12
      Top = 36
      Width = 108
      Height = 17
      Caption = '&TTF only'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = chbFontOptionsClick
    end
    object chbFontOptions2: TCheckBox
      Tag = 2
      Left = 12
      Top = 56
      Width = 108
      Height = 17
      Caption = 'Fi&xed pitch only'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = chbFontOptionsClick
    end
    object chbFontOptions3: TCheckBox
      Tag = 3
      Left = 12
      Top = 76
      Width = 108
      Height = 17
      Caption = '&No OEM'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = chbFontOptionsClick
    end
    object chbFontOptions4: TCheckBox
      Tag = 4
      Left = 12
      Top = 96
      Width = 108
      Height = 17
      Caption = 'OE&M only'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = chbFontOptionsClick
    end
    object chbFontOptions5: TCheckBox
      Tag = 5
      Left = 12
      Top = 116
      Width = 108
      Height = 17
      Caption = 'Sca&lable only'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = chbFontOptionsClick
    end
  end
  object grpStyle: TGroupBox
    Left = 164
    Top = 4
    Width = 126
    Height = 108
    Caption = 'ComboBox style'
    TabOrder = 6
    object btnStyle0: TSpeedButton
      Left = 8
      Top = 16
      Width = 110
      Height = 28
      Hint = 'csDropDown'
      GroupIndex = 1
      Glyph.Data = {
        86040000424D8604000000000000760000002800000062000000140000000100
        0400000000001004000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF000000877777777777777777777777777777777777
        7777777777777777777777777777777777777777777777777777777777777F00
        000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF00000000000000007F00000080FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7888
        8888888888807F00000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F777777777777807F00000080FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF7F777777777777807F00000080FF007F700F00FFF00F800800F0
        0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F7777777777
        77807F00000080FF807F708F00FFF00F007800F00FFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF7F777777777777807F00000080FF8000008F
        00FFF00F00FF00F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFF7F777770777777807F00000080FF708F807F00FFF00F808700F00FFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F777700077777807F00
        000080FFF00F00FF00FFF00FF78800F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF7F777000007777807F00000080FFF80800FF007FF00F
        87F700F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F77
        0000000777807F00000080FFF80808FF0088F00F780007F00FFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F777777777777807F00000080FF
        F80008FFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF7F777777777777807F00000080FFF70007FFFFFFF00FFFFFFFF0
        0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F7777777777
        77807F00000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF7F777777777777807F00000080FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFF7FFFFFFFFFFFFF807F00000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF77777777777777707F00
        0000800000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000007F00000088888888888888888888
        8888888888888888888888888888888888888888888888888888888888888888
        8888888888888F000000}
      Layout = blGlyphTop
      OnClick = StyleClick
    end
    object btnStyle1: TSpeedButton
      Tag = 1
      Left = 8
      Top = 44
      Width = 110
      Height = 28
      Hint = 'csSimple'
      GroupIndex = 1
      Glyph.Data = {
        86040000424D8604000000000000760000002800000062000000140000000100
        0400000000001004000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF000000877777777777777777777777777777777777
        7777777777777777777777777777777777777777777777777777777777777F00
        000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F00000080FFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF7F00000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F00000080FF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF7F00000080FF007F700F00FFF00F800800F0
        0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFF7F00000080FF807F708F00FFF00F007800F00FFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F00000080FF8000008F
        00FFF00F00FF00F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF7F00000080FF708F807F00FFF00F808700F00FFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F00
        000080FFF00F00FF00FFF00FF78800F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F00000080FFF80800FF007FF00F
        87F700F00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF7F00000080FFF80808FF0088F00F780007F00FFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F00000080FF
        F80008FFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF7F00000080FFF70007FFFFFFF00FFFFFFFF0
        0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFF7F00000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F00000080FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF7F00000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F00
        0000800000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000007F00000088888888888888888888
        8888888888888888888888888888888888888888888888888888888888888888
        8888888888888F000000}
      Layout = blGlyphTop
      OnClick = StyleClick
    end
    object btnStyle2: TSpeedButton
      Tag = 2
      Left = 8
      Top = 72
      Width = 110
      Height = 28
      Hint = 'csDropDownList'
      GroupIndex = 1
      Glyph.Data = {
        86040000424D8604000000000000760000002800000062000000140000000100
        0400000000001004000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFF000000877777777777777777777777777777777777
        7777777777777777777777777777777777777777777777777777777777777F00
        000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF00000000000000007F00000080F00000000000000000
        00000000000000000000000000000000000000000000000000000000000F7888
        8888888888807F00000080F0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0F7F777777777777807F00000080F0
        FFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFF0F7F777777777777807F00000080F0FFFFFFFFFFF00FFFFFFFFF00
        7F700F00FFF00F800800F00FFFFFFFFFFFFFFFFFFFFFFFFFFF0F7F7777777777
        77807F00000080F0FFFFF88888800FFFFFFFFF807F708F00FFF00F007800F00F
        FFFFFFFFFFFFFFFFFFFFFFFFFF0F7F777777777777807F00000080F0FFFFFFF8
        8FF00FFFFFFFFF8000008F00FFF00F00FF00F00FFFFFFFFFFFFFFFFFFFFFFFFF
        FF0F7F777770777777807F00000080F0FFFFFFF80FF00FF0FFFFFF708F807F00
        FFF00F808700F00FFFFFFFFFFFFFFFFFFFFFFFFFFF0F7F777700077777807F00
        000080F0FFFFFFF80FF00FF0FFFFFFF00F00FF00FFF00FF78800F00FFFFFFFFF
        FFFFFFFFFFFFFFFFFF0F7F777000007777807F00000080F0FFFFFFF800F00F00
        FFFFFFF80800FF007FF00F87F700F00FFFFFFFFFFFFFFFFFFFFFFFFFFF0F7F77
        0000000777807F00000080F0FFFF8FF800000000FFFFFFF80808FF0088F00F78
        0007F00FFFFFFFFFFFFFFFFFFFFFFFFFFF0F7F777777777777807F00000080F0
        FFFF8FF88FF8FFFFFFFFFFF80008FFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFF
        FFFFFFFFFF0F7F777777777777807F00000080F0FFFF88888888FFFFFFFFFFF7
        0007FFFFFFF00FFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFF0F7F7777777777
        77807F00000080F0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFF0F7F777777777777807F00000080F000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000F7FFFFFFFFFFFFF807F00000080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF77777777777777707F00
        0000800000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000007F00000088888888888888888888
        8888888888888888888888888888888888888888888888888888888888888888
        8888888888888F000000}
      Layout = blGlyphTop
      OnClick = StyleClick
    end
  end
  object grpProperties: TGroupBox
    Left = 300
    Top = 4
    Width = 160
    Height = 152
    Caption = 'Properties'
    TabOrder = 7
    object lbColor: TLabel
      Left = 12
      Top = 16
      Width = 30
      Height = 15
      Caption = '&Color'
      FocusControl = cmbColor
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object lbDropDownCount: TLabel
      Left = 12
      Top = 56
      Width = 59
      Height = 15
      Caption = '&DropDown'
      FocusControl = edDropDownCount
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object btnFont: TSpeedButton
      Left = 80
      Top = 72
      Width = 67
      Height = 23
      Caption = '&Font'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00D99999999999
        999DD99999999999999DD99999999999999DDDDDDDDDDDDDDDDDDD4444DDD444
        444DDDD84DDDDD8448DDDDDD44DDDD844DDDDDDD84DDDD448DDDDDDDD4444444
        DDDDDDDDD84DD448DDDDDDDDDD44D44DDDDDDDDDDD84448DDDDDDDDDDDD444DD
        DDDDDDDDDDD848DDDDDDDDDDDDDD4DDDDDDDDDDDDDDDDDDDDDDD}
      ParentFont = False
      OnClick = btnFontClick
    end
    object chbEnabled: TCheckBox
      Tag = 1
      Left = 12
      Top = 104
      Width = 70
      Height = 17
      Caption = '&Enabled'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = PropertiesCheckClick
    end
    object chbFlat: TCheckBox
      Tag = 3
      Left = 12
      Top = 128
      Width = 70
      Height = 17
      Caption = 'F&lat'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = PropertiesCheckClick
    end
    object chbFocusBorder: TCheckBox
      Tag = 4
      Left = 60
      Top = 128
      Width = 96
      Height = 17
      Caption = 'Focus&Border'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnClick = PropertiesCheckClick
    end
    object cmbColor: TabfColorComboBox
      Left = 12
      Top = 32
      Width = 135
      Height = 23
      ColorsSet = clsAll
      Ctl3D = True
      CustomColorCaption = 'Custom...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      SelectedColor = clWindow
      TabOrder = 0
      UseColorCustom = True
      OnClick = cmbColorClick
    end
    object edDropDownCount: TEdit
      Left = 12
      Top = 72
      Width = 40
      Height = 23
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = '8'
      OnChange = edDropDownCountChange
    end
    object udDropDownCount: TUpDown
      Left = 52
      Top = 72
      Width = 18
      Height = 23
      Associate = edDropDownCount
      Min = 0
      Max = 32000
      Position = 8
      TabOrder = 2
      Wrap = False
    end
    object chbEtched: TCheckBox
      Tag = 2
      Left = 84
      Top = 104
      Width = 70
      Height = 17
      Caption = 'E&tched'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = PropertiesCheckClick
    end
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 384
  end
end
