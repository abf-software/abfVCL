object abfStatusPanelsEditor: TabfStatusPanelsEditor
  Left = 192
  Top = 107
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Editing'
  ClientHeight = 349
  ClientWidth = 511
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 262
    Top = 318
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 346
    Top = 318
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnApply: TButton
    Left = 430
    Top = 318
    Width = 75
    Height = 25
    Caption = '&Apply'
    Enabled = False
    TabOrder = 4
    OnClick = btnApplyClick
  end
  object gbxPanels: TGroupBox
    Left = 7
    Top = 4
    Width = 126
    Height = 305
    Caption = ' Panels '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object plToolbar: TPanel
      Left = 2
      Top = 15
      Width = 122
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object sbtnNewItem: TSpeedButton
        Left = 6
        Top = 4
        Width = 25
        Height = 25
        Hint = 'Add new (Ins)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Glyph.Data = {
          36050000424D3605000000000000360400002800000010000000100000000100
          0800000000000001000000000000000000000001000000010000000000004000
          000080000000FF000000002000004020000080200000FF200000004000004040
          000080400000FF400000006000004060000080600000FF600000008000004080
          000080800000FF80000000A0000040A0000080A00000FFA0000000C0000040C0
          000080C00000FFC0000000FF000040FF000080FF0000FFFF0000000020004000
          200080002000FF002000002020004020200080202000FF202000004020004040
          200080402000FF402000006020004060200080602000FF602000008020004080
          200080802000FF80200000A0200040A0200080A02000FFA0200000C0200040C0
          200080C02000FFC0200000FF200040FF200080FF2000FFFF2000000040004000
          400080004000FF004000002040004020400080204000FF204000004040004040
          400080404000FF404000006040004060400080604000FF604000008040004080
          400080804000FF80400000A0400040A0400080A04000FFA0400000C0400040C0
          400080C04000FFC0400000FF400040FF400080FF4000FFFF4000000060004000
          600080006000FF006000002060004020600080206000FF206000004060004040
          600080406000FF406000006060004060600080606000FF606000008060004080
          600080806000FF80600000A0600040A0600080A06000FFA0600000C0600040C0
          600080C06000FFC0600000FF600040FF600080FF6000FFFF6000000080004000
          800080008000FF008000002080004020800080208000FF208000004080004040
          800080408000FF408000006080004060800080608000FF608000008080004080
          800080808000FF80800000A0800040A0800080A08000FFA0800000C0800040C0
          800080C08000FFC0800000FF800040FF800080FF8000FFFF80000000A0004000
          A0008000A000FF00A0000020A0004020A0008020A000FF20A0000040A0004040
          A0008040A000FF40A0000060A0004060A0008060A000FF60A0000080A0004080
          A0008080A000FF80A00000A0A00040A0A00080A0A000FFA0A00000C0A00040C0
          A00080C0A000FFC0A00000FFA00040FFA00080FFA000FFFFA0000000C0004000
          C0008000C000FF00C0000020C0004020C0008020C000FF20C0000040C0004040
          C0008040C000FF40C0000060C0004060C0008060C000FF60C0000080C0004080
          C0008080C000FF80C00000A0C00040A0C00080A0C000FFA0C00000C0C00040C0
          C00080C0C000FFC0C00000FFC00040FFC00080FFC000FFFFC0000000FF004000
          FF008000FF00FF00FF000020FF004020FF008020FF00FF20FF000040FF004040
          FF008040FF00FF40FF000060FF004060FF008060FF00FF60FF000080FF004080
          FF008080FF00FF80FF0000A0FF0040A0FF0080A0FF00FFA0FF0000C0FF0040C0
          FF0080C0FF00FFC0FF0000FFFF0040FFFF0080FFFF00FFFFFF00E3E3E3E3E3E3
          E3E3E3E3E3E3E3E3E3E3E3E39292929292929292929292929292E30000000000
          00000000000000000092E300FFFCFFFCFFFCFFFC0000FFFC0092E300FCFFFCFF
          FCFFFCFF00DB00FF0092E300FFFCFFFCFFFCFFFC00FCDB000092E300FCFFFCFF
          FCFFFCFF000000000092FF00FF92FCFFFCFFFCFCFFFCFFFC009292FCFC92FFFC
          92FCFFFFFCFFFCFF0092E392FF92FC92FCFFFCFCFFFCFFFC0092929292FF92FF
          FCFFFCFFFCFFFCFF0092FFFC92FCFF92929292000000000000E3E392FC92FC92
          FCE3E3E3E3E3E3E3E3E392FCE392FFE392FCE3E3E3E3E3E3E3E3FCE3E392FCE3
          E392E3E3E3E3E3E3E3E3E3E3E392FFE3E3E3E3E3E3E3E3E3E3E3}
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = AddClick
      end
      object sbtnDelItem: TSpeedButton
        Left = 32
        Top = 4
        Width = 25
        Height = 25
        Hint = 'Delete Selected (Del)'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Glyph.Data = {
          36050000424D3605000000000000360400002800000010000000100000000100
          0800000000000001000000000000000000000001000000010000000000004000
          000080000000FF000000002000004020000080200000FF200000004000004040
          000080400000FF400000006000004060000080600000FF600000008000004080
          000080800000FF80000000A0000040A0000080A00000FFA0000000C0000040C0
          000080C00000FFC0000000FF000040FF000080FF0000FFFF0000000020004000
          200080002000FF002000002020004020200080202000FF202000004020004040
          200080402000FF402000006020004060200080602000FF602000008020004080
          200080802000FF80200000A0200040A0200080A02000FFA0200000C0200040C0
          200080C02000FFC0200000FF200040FF200080FF2000FFFF2000000040004000
          400080004000FF004000002040004020400080204000FF204000004040004040
          400080404000FF404000006040004060400080604000FF604000008040004080
          400080804000FF80400000A0400040A0400080A04000FFA0400000C0400040C0
          400080C04000FFC0400000FF400040FF400080FF4000FFFF4000000060004000
          600080006000FF006000002060004020600080206000FF206000004060004040
          600080406000FF406000006060004060600080606000FF606000008060004080
          600080806000FF80600000A0600040A0600080A06000FFA0600000C0600040C0
          600080C06000FFC0600000FF600040FF600080FF6000FFFF6000000080004000
          800080008000FF008000002080004020800080208000FF208000004080004040
          800080408000FF408000006080004060800080608000FF608000008080004080
          800080808000FF80800000A0800040A0800080A08000FFA0800000C0800040C0
          800080C08000FFC0800000FF800040FF800080FF8000FFFF80000000A0004000
          A0008000A000FF00A0000020A0004020A0008020A000FF20A0000040A0004040
          A0008040A000FF40A0000060A0004060A0008060A000FF60A0000080A0004080
          A0008080A000FF80A00000A0A00040A0A00080A0A000FFA0A00000C0A00040C0
          A00080C0A000FFC0A00000FFA00040FFA00080FFA000FFFFA0000000C0004000
          C0008000C000FF00C0000020C0004020C0008020C000FF20C0000040C0004040
          C0008040C000FF40C0000060C0004060C0008060C000FF60C0000080C0004080
          C0008080C000FF80C00000A0C00040A0C00080A0C000FFA0C00000C0C00040C0
          C00080C0C000FFC0C00000FFC00040FFC00080FFC000FFFFC0000000FF004000
          FF008000FF00FF00FF000020FF004020FF008020FF00FF20FF000040FF004040
          FF008040FF00FF40FF000060FF004060FF008060FF00FF60FF000080FF004080
          FF008080FF00FF80FF0000A0FF0040A0FF0080A0FF00FFA0FF0000C0FF0040C0
          FF0080C0FF00FFC0FF0000FFFF0040FFFF0080FFFF00FFFFFF00E3E3E3E3E3E3
          E3E3E3E3E3E3E3E3E3E3E3E39292929292929292929292929292E30000000000
          00000000000000000092E300FFFCFFFCFFFCFFFC0000FFFC00929200FCFFFCFF
          FCFFFCFF00DB00FF00928000FFFCFFFCFFFCFFFC00FCDB0000928080FCFFFCFF
          9280FCFF000000000092928092FCFF928092FFFCFFFCFFFC0092E3808092FC80
          80FFFCFFFCFFFCFF0092E39280808080FFFCFFFCFFFCFFFC0092E392808080FF
          FCFFFCFFFCFFFCFF0092928080808092000000000000000000E3808092E38080
          92E3E3E3E3E3E3E3E3E3E3E3E3E3E3808092E3E3E3E3E3E3E3E3E3E3E3E3E3E3
          808092E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3}
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = DeleteClick
      end
      object sbtnUpItem: TSpeedButton
        Left = 64
        Top = 4
        Width = 25
        Height = 25
        Hint = 'Move Selected Up (Ctrl+Up)'
        Enabled = False
        Glyph.Data = {
          36050000424D3605000000000000360400002800000010000000100000000100
          0800000000000001000000000000000000000001000000010000000000004000
          000080000000FF000000002000004020000080200000FF200000004000004040
          000080400000FF400000006000004060000080600000FF600000008000004080
          000080800000FF80000000A0000040A0000080A00000FFA0000000C0000040C0
          000080C00000FFC0000000FF000040FF000080FF0000FFFF0000000020004000
          200080002000FF002000002020004020200080202000FF202000004020004040
          200080402000FF402000006020004060200080602000FF602000008020004080
          200080802000FF80200000A0200040A0200080A02000FFA0200000C0200040C0
          200080C02000FFC0200000FF200040FF200080FF2000FFFF2000000040004000
          400080004000FF004000002040004020400080204000FF204000004040004040
          400080404000FF404000006040004060400080604000FF604000008040004080
          400080804000FF80400000A0400040A0400080A04000FFA0400000C0400040C0
          400080C04000FFC0400000FF400040FF400080FF4000FFFF4000000060004000
          600080006000FF006000002060004020600080206000FF206000004060004040
          600080406000FF406000006060004060600080606000FF606000008060004080
          600080806000FF80600000A0600040A0600080A06000FFA0600000C0600040C0
          600080C06000FFC0600000FF600040FF600080FF6000FFFF6000000080004000
          800080008000FF008000002080004020800080208000FF208000004080004040
          800080408000FF408000006080004060800080608000FF608000008080004080
          800080808000FF80800000A0800040A0800080A08000FFA0800000C0800040C0
          800080C08000FFC0800000FF800040FF800080FF8000FFFF80000000A0004000
          A0008000A000FF00A0000020A0004020A0008020A000FF20A0000040A0004040
          A0008040A000FF40A0000060A0004060A0008060A000FF60A0000080A0004080
          A0008080A000FF80A00000A0A00040A0A00080A0A000FFA0A00000C0A00040C0
          A00080C0A000FFC0A00000FFA00040FFA00080FFA000FFFFA0000000C0004000
          C0008000C000FF00C0000020C0004020C0008020C000FF20C0000040C0004040
          C0008040C000FF40C0000060C0004060C0008060C000FF60C0000080C0004080
          C0008080C000FF80C00000A0C00040A0C00080A0C000FFA0C00000C0C00040C0
          C00080C0C000FFC0C00000FFC00040FFC00080FFC000FFFFC0000000FF004000
          FF008000FF00FF00FF000020FF004020FF008020FF00FF20FF000040FF004040
          FF008040FF00FF40FF000060FF004060FF008060FF00FF60FF000080FF004080
          FF008080FF00FF80FF0000A0FF0040A0FF0080A0FF00FFA0FF0000C0FF0040C0
          FF0080C0FF00FFC0FF0000FFFF0040FFFF0080FFFF00FFFFFF00E3E3E3E3E3E3
          E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3
          E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E30000000000E3E3E3E3E3E3E3E3E3E3E3
          0012121200E3E3E3E3E3E3E3E3E3E3E30012121200E3E3E3E3E3E3E3E3E3E3E3
          0012121200E3E3E3E3E3E3E3E30000000012121200000000E3E3E3E3E3E30012
          12121212121200E3E3E3E3E3E3E3E300121212121200E3E3E3E3E3E3E3E3E3E3
          0012121200E3E3E3E3E3E3E3E3E3E3E3E3001200E3E3E3E3E3E3E3E3E3E3E3E3
          E3E300E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3
          E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3}
        ParentShowHint = False
        ShowHint = True
        OnClick = MoveUpClick
      end
      object sbtnDownItem: TSpeedButton
        Left = 90
        Top = 4
        Width = 25
        Height = 25
        Hint = 'Move Selected Down (Ctrl+Down)'
        Enabled = False
        Glyph.Data = {
          36050000424D3605000000000000360400002800000010000000100000000100
          0800000000000001000000000000000000000001000000010000000000004000
          000080000000FF000000002000004020000080200000FF200000004000004040
          000080400000FF400000006000004060000080600000FF600000008000004080
          000080800000FF80000000A0000040A0000080A00000FFA0000000C0000040C0
          000080C00000FFC0000000FF000040FF000080FF0000FFFF0000000020004000
          200080002000FF002000002020004020200080202000FF202000004020004040
          200080402000FF402000006020004060200080602000FF602000008020004080
          200080802000FF80200000A0200040A0200080A02000FFA0200000C0200040C0
          200080C02000FFC0200000FF200040FF200080FF2000FFFF2000000040004000
          400080004000FF004000002040004020400080204000FF204000004040004040
          400080404000FF404000006040004060400080604000FF604000008040004080
          400080804000FF80400000A0400040A0400080A04000FFA0400000C0400040C0
          400080C04000FFC0400000FF400040FF400080FF4000FFFF4000000060004000
          600080006000FF006000002060004020600080206000FF206000004060004040
          600080406000FF406000006060004060600080606000FF606000008060004080
          600080806000FF80600000A0600040A0600080A06000FFA0600000C0600040C0
          600080C06000FFC0600000FF600040FF600080FF6000FFFF6000000080004000
          800080008000FF008000002080004020800080208000FF208000004080004040
          800080408000FF408000006080004060800080608000FF608000008080004080
          800080808000FF80800000A0800040A0800080A08000FFA0800000C0800040C0
          800080C08000FFC0800000FF800040FF800080FF8000FFFF80000000A0004000
          A0008000A000FF00A0000020A0004020A0008020A000FF20A0000040A0004040
          A0008040A000FF40A0000060A0004060A0008060A000FF60A0000080A0004080
          A0008080A000FF80A00000A0A00040A0A00080A0A000FFA0A00000C0A00040C0
          A00080C0A000FFC0A00000FFA00040FFA00080FFA000FFFFA0000000C0004000
          C0008000C000FF00C0000020C0004020C0008020C000FF20C0000040C0004040
          C0008040C000FF40C0000060C0004060C0008060C000FF60C0000080C0004080
          C0008080C000FF80C00000A0C00040A0C00080A0C000FFA0C00000C0C00040C0
          C00080C0C000FFC0C00000FFC00040FFC00080FFC000FFFFC0000000FF004000
          FF008000FF00FF00FF000020FF004020FF008020FF00FF20FF000040FF004040
          FF008040FF00FF40FF000060FF004060FF008060FF00FF60FF000080FF004080
          FF008080FF00FF80FF0000A0FF0040A0FF0080A0FF00FFA0FF0000C0FF0040C0
          FF0080C0FF00FFC0FF0000FFFF0040FFFF0080FFFF00FFFFFF00E3E3E3E3E3E3
          E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3
          E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E300E3E3E3E3E3E3E3E3E3E3E3E3E3
          E3001200E3E3E3E3E3E3E3E3E3E3E3E30012121200E3E3E3E3E3E3E3E3E3E300
          121212121200E3E3E3E3E3E3E3E3001212121212121200E3E3E3E3E3E3000000
          0012121200000000E3E3E3E3E3E3E3E30012121200E3E3E3E3E3E3E3E3E3E3E3
          0012121200E3E3E3E3E3E3E3E3E3E3E30012121200E3E3E3E3E3E3E3E3E3E3E3
          0000000000E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3
          E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3}
        ParentShowHint = False
        ShowHint = True
        OnClick = MoveDownClick
      end
    end
    object plBorder: TPanel
      Left = 2
      Top = 48
      Width = 122
      Height = 255
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      object lbxPanels: TListBox
        Left = 2
        Top = 2
        Width = 118
        Height = 251
        Align = alClient
        DragMode = dmAutomatic
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbxPanelsClick
        OnDragDrop = lbxPanelsDragDrop
        OnDragOver = lbxPanelsDragOver
        OnKeyDown = lbxPanelsKeyDown
      end
    end
  end
  object gbxPanelProperties: TGroupBox
    Left = 143
    Top = 4
    Width = 361
    Height = 305
    Caption = ' Panel Properties '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lbBevel: TLabel
      Left = 8
      Top = 56
      Width = 27
      Height = 13
      Caption = '&Bevel'
      FocusControl = cbxBevel
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbBrushColor: TLabel
      Left = 8
      Top = 96
      Width = 51
      Height = 13
      Caption = 'Brush&Color'
      FocusControl = cbxBrushColor
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbBrushStyle: TLabel
      Left = 144
      Top = 96
      Width = 50
      Height = 13
      Caption = 'Brush&Style'
      FocusControl = cbxBrushStyle
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbPanelStyle: TLabel
      Left = 144
      Top = 56
      Width = 50
      Height = 13
      Caption = '&PanelStyle'
      FocusControl = cbxPanelStyle
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbName: TLabel
      Left = 8
      Top = 16
      Width = 28
      Height = 13
      Caption = '&Name'
      FocusControl = edName
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbWidth: TLabel
      Left = 143
      Top = 16
      Width = 28
      Height = 13
      Caption = '&Width'
      FocusControl = edWidth
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbParentColor: TLabel
      Left = 278
      Top = 96
      Width = 55
      Height = 13
      Caption = 'Pa&rentColor'
      FocusControl = cbxParentColor
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbParentFont: TLabel
      Left = 278
      Top = 56
      Width = 52
      Height = 13
      Caption = 'Par&entFont'
      FocusControl = cbxParentFont
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbPopupMenu: TLabel
      Left = 8
      Top = 136
      Width = 58
      Height = 13
      Caption = 'Popup&Menu'
      FocusControl = cbxPopupMenu
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbHint: TLabel
      Left = 144
      Top = 136
      Width = 19
      Height = 13
      Caption = '&Hint'
      FocusControl = edHint
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object cbxBevel: TComboBox
      Left = 8
      Top = 72
      Width = 120
      Height = 21
      Style = csDropDownList
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 3
      OnChange = PropertiesModified
    end
    object cbxBrushColor: TComboBox
      Left = 8
      Top = 112
      Width = 120
      Height = 21
      Style = csDropDownList
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 6
      OnChange = cbxBrushColorChange
    end
    object cbxBrushStyle: TComboBox
      Left = 144
      Top = 112
      Width = 120
      Height = 21
      Style = csDropDownList
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 7
      OnChange = PropertiesModified
    end
    object cbxPanelStyle: TComboBox
      Left = 144
      Top = 72
      Width = 120
      Height = 21
      Style = csDropDownList
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 4
      OnChange = PropertiesModified
    end
    object pcStyleProperties: TPageControl
      Left = 9
      Top = 183
      Width = 343
      Height = 113
      ActivePage = tsTextStyle
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
      object tsTextStyle: TTabSheet
        Caption = 'Text Style'
        object lbSpacing: TLabel
          Left = 174
          Top = 44
          Width = 39
          Height = 13
          Caption = 'Sp&acing'
          FocusControl = edSpacing
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label6: TLabel
          Left = 239
          Top = 3
          Width = 46
          Height = 13
          Caption = 'A&lignment'
          FocusControl = cbxAlignment
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbGlyphIndex: TLabel
          Left = 111
          Top = 44
          Width = 53
          Height = 13
          Caption = '&GlyphIndex'
          FocusControl = edGlyphIndex
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbLayout: TLabel
          Left = 4
          Top = 44
          Width = 32
          Height = 13
          Caption = 'Layo&ut'
          FocusControl = cbxLayout
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbScrollEffect: TLabel
          Left = 111
          Top = 3
          Width = 54
          Height = 13
          Caption = 'ScrollEffec&t'
          FocusControl = cbxScrollEffect
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbText: TLabel
          Left = 3
          Top = 3
          Width = 21
          Height = 13
          Caption = 'Te&xt'
          FocusControl = edText
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbShowText: TLabel
          Left = 239
          Top = 44
          Width = 48
          Height = 13
          Caption = 'Sho&wText'
          FocusControl = cbxShowText
        end
        object edSpacing: TEdit
          Left = 174
          Top = 60
          Width = 57
          Height = 21
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxLength = 9
          ParentFont = False
          TabOrder = 5
          OnChange = PropertiesModified
        end
        object cbxAlignment: TComboBox
          Left = 239
          Top = 19
          Width = 94
          Height = 21
          Style = csDropDownList
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 2
          OnChange = PropertiesModified
        end
        object edGlyphIndex: TEdit
          Left = 111
          Top = 60
          Width = 56
          Height = 21
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxLength = 9
          ParentFont = False
          TabOrder = 4
          OnChange = PropertiesModified
        end
        object cbxLayout: TComboBox
          Left = 3
          Top = 60
          Width = 100
          Height = 21
          Style = csDropDownList
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 3
          OnChange = PropertiesModified
        end
        object cbxScrollEffect: TComboBox
          Left = 111
          Top = 19
          Width = 121
          Height = 21
          Style = csDropDownList
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 1
          OnChange = PropertiesModified
        end
        object edText: TEdit
          Left = 3
          Top = 19
          Width = 100
          Height = 21
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnChange = PropertiesModified
        end
        object cbxShowText: TComboBox
          Left = 239
          Top = 60
          Width = 94
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 6
          OnChange = PropertiesModified
          Items.Strings = (
            'False'
            'True')
        end
      end
      object tsProgressBarStyle: TTabSheet
        Caption = 'ProgressBar Style'
        object lbShowCaption: TLabel
          Left = 239
          Top = 3
          Width = 63
          Height = 13
          Caption = 'Sho&wCaption'
          FocusControl = cbxShowCaption
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label1: TLabel
          Left = 239
          Top = 44
          Width = 41
          Height = 13
          Caption = 'Pr&ogress'
          FocusControl = edProgress
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbMinValue: TLabel
          Left = 3
          Top = 44
          Width = 44
          Height = 13
          Caption = 'M&inValue'
          FocusControl = edMinValue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label3: TLabel
          Left = 111
          Top = 44
          Width = 47
          Height = 13
          Caption = 'Max&Value'
          FocusControl = edMaxValue
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbProgressCaption: TLabel
          Left = 3
          Top = 3
          Width = 36
          Height = 13
          Caption = 'Capti&on'
          FocusControl = edProgressCaption
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbProgressType: TLabel
          Left = 111
          Top = 3
          Width = 65
          Height = 13
          Caption = 'Progress&Type'
          FocusControl = cbxProgressType
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object cbxShowCaption: TComboBox
          Left = 239
          Top = 19
          Width = 94
          Height = 21
          Style = csDropDownList
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 2
          OnChange = PropertiesModified
          Items.Strings = (
            'False'
            'True')
        end
        object edProgress: TEdit
          Left = 239
          Top = 60
          Width = 94
          Height = 21
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxLength = 9
          ParentFont = False
          TabOrder = 5
          OnChange = PropertiesModified
        end
        object edMinValue: TEdit
          Left = 3
          Top = 60
          Width = 100
          Height = 21
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxLength = 9
          ParentFont = False
          TabOrder = 3
          OnChange = PropertiesModified
        end
        object edMaxValue: TEdit
          Left = 111
          Top = 60
          Width = 121
          Height = 21
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxLength = 9
          ParentFont = False
          TabOrder = 4
          OnChange = PropertiesModified
        end
        object edProgressCaption: TEdit
          Left = 3
          Top = 19
          Width = 100
          Height = 21
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnChange = PropertiesModified
        end
        object cbxProgressType: TComboBox
          Left = 111
          Top = 19
          Width = 121
          Height = 21
          Style = csDropDownList
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 0
          ParentFont = False
          TabOrder = 1
          OnChange = cbxProgressTypeChange
        end
      end
      object tsLockKeyStyle: TTabSheet
        Caption = 'LockKey Style'
        object lbLockKeyCaption: TLabel
          Left = 3
          Top = 3
          Width = 36
          Height = 13
          Caption = 'Capti&on'
          FocusControl = edLockKeyCaption
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbLockKeyType: TLabel
          Left = 111
          Top = 3
          Width = 66
          Height = 13
          Caption = 'LockKey&Type'
          FocusControl = cbxLockKeyType
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbLockKeyColorOn: TLabel
          Left = 3
          Top = 44
          Width = 38
          Height = 13
          Caption = 'ColorOn'
          FocusControl = cbxLockKeyColorOn
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object lbLockKeyColorOff: TLabel
          Left = 111
          Top = 44
          Width = 38
          Height = 13
          Caption = 'ColorOff'
          FocusControl = cbxLockKeyColorOff
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object edLockKeyCaption: TEdit
          Left = 3
          Top = 19
          Width = 100
          Height = 21
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnChange = PropertiesModified
        end
        object cbxLockKeyType: TComboBox
          Left = 111
          Top = 19
          Width = 121
          Height = 21
          Style = csDropDownList
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 0
          ParentFont = False
          TabOrder = 1
          OnChange = cbxLockKeyTypeChange
        end
        object cbxLockKeyColorOn: TComboBox
          Left = 3
          Top = 60
          Width = 100
          Height = 21
          Style = csDropDownList
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 0
          ParentFont = False
          TabOrder = 2
          OnChange = PropertiesModified
        end
        object cbxLockKeyColorOff: TComboBox
          Left = 111
          Top = 60
          Width = 121
          Height = 21
          Style = csDropDownList
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 0
          ParentFont = False
          TabOrder = 3
          OnChange = PropertiesModified
        end
      end
    end
    object edName: TEdit
      Left = 8
      Top = 32
      Width = 120
      Height = 21
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 20
      ParentFont = False
      TabOrder = 0
      OnChange = PropertiesModified
    end
    object edWidth: TEdit
      Left = 143
      Top = 32
      Width = 120
      Height = 21
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 9
      ParentFont = False
      TabOrder = 1
      OnChange = PropertiesModified
    end
    object btnFont: TButton
      Left = 277
      Top = 29
      Width = 75
      Height = 25
      Caption = '&Font'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = btnFontClick
    end
    object cbxParentColor: TComboBox
      Left = 278
      Top = 112
      Width = 75
      Height = 21
      Style = csDropDownList
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 8
      OnChange = cbxParentColorChange
      Items.Strings = (
        'False'
        'True')
    end
    object cbxParentFont: TComboBox
      Left = 278
      Top = 72
      Width = 75
      Height = 21
      Style = csDropDownList
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 5
      OnChange = cbxParentFontChange
      Items.Strings = (
        'False'
        'True')
    end
    object cbxPopupMenu: TComboBox
      Left = 8
      Top = 152
      Width = 120
      Height = 21
      Style = csDropDownList
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      Sorted = True
      TabOrder = 9
      OnChange = PropertiesModified
      OnKeyDown = cbxPopupMenuKeyDown
    end
    object edHint: TEdit
      Left = 144
      Top = 152
      Width = 208
      Height = 21
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
      OnChange = PropertiesModified
    end
  end
end
