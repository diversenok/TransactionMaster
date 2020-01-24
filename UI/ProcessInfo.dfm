object FormProcessInfo: TFormProcessInfo
  Left = 0
  Top = 0
  Caption = 'Process Properties'
  ClientHeight = 342
  ClientWidth = 376
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 350
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
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
    Height = 307
    Margins.Bottom = 32
    ActivePage = tabThreads
    Align = alClient
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object tabThreads: TTabSheet
      Caption = 'Threads'
      object lvThreads: TListViewEx
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 362
        Height = 192
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 6
        Align = alClient
        Columns = <
          item
            Caption = 'TID'
            Width = 70
          end
          item
            Caption = 'Created'
            Width = 180
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
      object gbTracking: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 201
        Width = 356
        Height = 75
        Align = alBottom
        Caption = 'Future threads: '
        TabOrder = 1
        object lblTrackingState: TLabel
          Left = 12
          Top = 51
          Width = 333
          Height = 13
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Unknown: can'#39't open the process.'
          EllipsisPosition = epEndEllipsis
        end
        object cbFutureTmTx: TComboBox
          Left = 12
          Top = 22
          Width = 256
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          Enabled = False
          TabOrder = 0
          Items.Strings = (
            'No transaction (default)')
        end
        object btnSetFuture: TButton
          Left = 274
          Top = 20
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Set'
          DropDownMenu = popupFuture
          Enabled = False
          Style = bsSplitButton
          TabOrder = 1
          OnClick = btnSetFutureClick
        end
      end
    end
    object tabHandles: TTabSheet
      Caption = 'Transactions'
      ImageIndex = 1
      object lvHandles: TListViewEx
        Left = 0
        Top = 0
        Width = 362
        Height = 279
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Handle'
            Width = 60
          end
          item
            AutoSize = True
            Caption = 'Access'
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
    Top = 313
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    ModalResult = 8
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object popupThread: TPopupMenu
    Left = 319
    Top = 83
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
    Left = 319
    Top = 139
    object cmInspect: TMenuItem
      Caption = 'Inspect'
      Default = True
      ShortCut = 13
      OnClick = cmHandleInspect
    end
    object cmSendTo: TMenuItem
      Caption = 'Send to...'
      OnClick = cmSendToClick
    end
    object cmCloseHandle: TMenuItem
      Caption = 'Close handle'
      ShortCut = 46
      OnClick = cmCloseHandleClick
    end
  end
  object popupFuture: TPopupMenu
    Left = 319
    Top = 27
    object cmIncludeExisting: TMenuItem
      AutoCheck = True
      Caption = 'Include existing threads'
      Checked = True
    end
    object cmSeparator: TMenuItem
      Caption = '-'
    end
    object cmRefreshFuture: TMenuItem
      Caption = 'Refresh'
      OnClick = cmRefreshFutureClick
    end
  end
end
