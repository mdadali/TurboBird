object FormRawValue: TFormRawValue
  Left = 0
  Top = 0
  Caption = 'Raw value'
  ClientHeight = 300
  ClientWidth = 515
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 16
  object panTop: TPanel
    Left = 0
    Top = 0
    Width = 515
    Height = 53
    Align = alTop
    TabOrder = 0
    object lbTypeText: TLabel
      Left = 16
      Top = 8
      Width = 33
      Height = 16
      Caption = 'Type:'
    end
    object lbType: TLabel
      Left = 55
      Top = 8
      Width = 64
      Height = 16
      Caption = 'Undefined'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lbSize: TLabel
      Left = 55
      Top = 27
      Width = 8
      Height = 16
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lbSizeText: TLabel
      Left = 16
      Top = 27
      Width = 29
      Height = 16
      Caption = 'Size:'
    end
    object lbRawOffsText: TLabel
      Left = 348
      Top = 8
      Width = 60
      Height = 16
      Caption = 'RAW Offs:'
    end
    object lbRawOffs: TLabel
      Left = 412
      Top = 8
      Width = 8
      Height = 16
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object chkFullRaw: TCheckBox
      Left = 348
      Top = 26
      Width = 97
      Height = 17
      Caption = 'Full RAW'
      TabOrder = 0
      OnClick = chkFullRawClick
    end
    object chkValue: TCheckBox
      Left = 236
      Top = 26
      Width = 97
      Height = 17
      Caption = 'Value'
      TabOrder = 1
      OnClick = chkValueClick
    end
  end
  object panDataView: TPanel
    Left = 0
    Top = 53
    Width = 515
    Height = 247
    Align = alClient
    TabOrder = 1
    object memoText: TMemo
      Left = 229
      Top = 1
      Width = 285
      Height = 245
      Align = alClient
      Lines.Strings = (
        'memoText')
      TabOrder = 0
      OnClick = memoTextChange
      OnKeyDown = memoTextKeyDown
    end
    object dgHex: TDrawGrid
      Left = 1
      Top = 1
      Width = 228
      Height = 245
      Align = alLeft
      ColCount = 9
      DefaultColWidth = 20
      DefaultRowHeight = 16
      FixedRows = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect]
      ParentFont = False
      PopupMenu = pmHex
      ScrollBars = ssVertical
      TabOrder = 1
      OnDrawCell = dgHexDrawCell
      ExplicitLeft = 0
      ExplicitTop = 6
    end
  end
  object pmHex: TPopupMenu
    Left = 100
    Top = 68
    object miCopyToClipboard: TMenuItem
      Caption = 'Copy to clipboard'
      OnClick = miCopyToClipboardClick
    end
  end
end
