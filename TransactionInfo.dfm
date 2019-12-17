object FormInfo: TFormInfo
  Left = 0
  Top = 0
  Caption = 'Transaction info'
  ClientHeight = 314
  ClientWidth = 398
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 392
    Height = 278
    Margins.Bottom = 33
    ActivePage = TabGeneral
    Align = alClient
    TabOrder = 0
    object TabGeneral: TTabSheet
      Caption = 'General'
      object lvInfo: TListViewEx
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 378
        Height = 214
        Align = alTop
        BorderStyle = bsNone
        Columns = <
          item
            Width = 120
          end
          item
            AutoSize = True
          end>
        Groups = <
          item
            Header = 'Transaction'
            GroupID = 0
            State = [lgsNormal, lgsCollapsible]
            HeaderAlign = taLeftJustify
            FooterAlign = taLeftJustify
            TitleImage = -1
          end
          item
            Header = 'Handle'
            GroupID = 1
            State = [lgsNormal, lgsCollapsible]
            HeaderAlign = taLeftJustify
            FooterAlign = taLeftJustify
            TitleImage = -1
          end
          item
            Header = 'References'
            GroupID = 2
            State = [lgsNormal, lgsCollapsible]
            HeaderAlign = taLeftJustify
            FooterAlign = taLeftJustify
            TitleImage = -1
          end>
        Items.ItemData = {
          05280100000700000000000000FFFFFFFFFFFFFFFF0000000000000000000000
          0004470055004900440000000000FFFFFFFFFFFFFFFF00000000000000000000
          00000B4400650073006300720069007000740069006F006E0000000000FFFFFF
          FFFFFFFFFF000000000000000000000000074F007500740063006F006D006500
          00000000FFFFFFFFFFFFFFFF00000000010000000000000006480061006E0064
          006C00650000000000FFFFFFFFFFFFFFFF0000000001000000000000000E4700
          720061006E00740065006400200061006300630065007300730000000000FFFF
          FFFFFFFFFFFF0000000002000000000000000850006F0069006E007400650072
          00730000000000FFFFFFFFFFFFFFFF0000000002000000000000000748006100
          6E0064006C0065007300}
        MultiSelect = True
        GroupView = True
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
      end
      object btnCommit: TButton
        Left = 3
        Top = 222
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Commit'
        TabOrder = 1
        OnClick = btnCommitClick
      end
      object btnRollback: TButton
        Left = 84
        Top = 222
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Rollback'
        TabOrder = 2
        OnClick = btnRollbackClick
      end
    end
    object TabConsumers: TTabSheet
      Caption = 'Consumers'
      ImageIndex = 1
      object lblUsedIn: TLabel
        Left = 7
        Top = 7
        Width = 97
        Height = 13
        Caption = 'Transaction used in:'
      end
      object lvConsumers: TListViewEx
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 378
        Height = 190
        Margins.Top = 28
        Margins.Bottom = 32
        Align = alClient
        Columns = <
          item
            Caption = 'Image name'
            Width = 140
          end
          item
            Alignment = taCenter
            Caption = 'PID'
          end
          item
            Alignment = taCenter
            Caption = 'Handle'
          end
          item
            AutoSize = True
            Caption = 'Granted access'
          end>
        GridLines = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = ProcessPopup
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvProcessInspect
        ColoringItems = True
        PopupOnItemsOnly = True
      end
      object btnSendHandle: TButton
        Left = 2
        Top = 223
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Send to'
        TabOrder = 1
        OnClick = btnSendHandleClick
      end
    end
  end
  object btnClose: TButton
    Left = 319
    Top = 284
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 8
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object ProcessPopup: TPopupMenu
    Left = 231
    Top = 99
    object cmInspect: TMenuItem
      Caption = 'Inspect'
      Default = True
      ShortCut = 13
      OnClick = lvProcessInspect
    end
    object cmCloseHandle: TMenuItem
      Caption = 'Close handle'
      ShortCut = 46
      OnClick = cmCloseHandleClick
    end
  end
  object UpdateTimer: TTimer
    OnTimer = UpdateTimerTimer
    Left = 304
    Top = 96
  end
end
