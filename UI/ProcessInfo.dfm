object FormProcessInfo: TFormProcessInfo
  Left = 0
  Top = 0
  Caption = 'Process info'
  ClientHeight = 278
  ClientWidth = 376
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object pageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 370
    Height = 243
    Margins.Bottom = 32
    ActivePage = tabThreads
    Align = alClient
    TabOrder = 0
    object tabThreads: TTabSheet
      Caption = 'Threads'
      object lvThreads: TListViewEx
        Left = 0
        Top = 0
        Width = 362
        Height = 215
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'TID'
            Width = 70
          end
          item
            Caption = 'Created'
            Width = 140
          end
          item
            Alignment = taCenter
            Caption = 'Transaction'
            Width = 80
          end>
        GridLines = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = popupThread
        TabOrder = 0
        ViewStyle = vsReport
        ColoringItems = True
        PopupOnItemsOnly = True
      end
    end
    object tabHandles: TTabSheet
      Caption = 'Transactions'
      ImageIndex = 1
      object lvHandles: TListViewEx
        Left = 0
        Top = 0
        Width = 362
        Height = 215
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Handle'
            Width = 60
          end
          item
            Caption = 'Access'
            Width = 120
          end
          item
            Caption = 'Description'
            Width = 160
          end>
        GridLines = True
        ReadOnly = True
        RowSelect = True
        PopupMenu = popupHandle
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = cmHandleInspect
        ColoringItems = True
        PopupOnItemsOnly = True
      end
    end
  end
  object btnClose: TButton
    Left = 295
    Top = 249
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    ModalResult = 8
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object popupThread: TPopupMenu
    Left = 63
    Top = 91
    object cmAssign: TMenuItem
      Caption = 'Assign a &transaction'
      Default = True
      object cmAssignNone: TMenuItem
        Caption = '&No transaction'
        OnClick = cmAssignClick
      end
    end
    object cmSuspend: TMenuItem
      Caption = '&Suspend'
      OnClick = cmSuspendClick
    end
    object cmResume: TMenuItem
      Caption = '&Resume'
      OnClick = cmResumeClick
    end
  end
  object popupHandle: TPopupMenu
    Left = 143
    Top = 91
    object cmInspect: TMenuItem
      Caption = 'Inspect'
      Default = True
      ShortCut = 13
      OnClick = cmHandleInspect
    end
    object cmCloseHandle: TMenuItem
      Caption = 'Close handle'
      ShortCut = 46
      OnClick = cmCloseHandleClick
    end
  end
end
