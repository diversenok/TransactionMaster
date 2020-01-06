object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Transaction Master'
  ClientHeight = 392
  ClientWidth = 547
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
  object lblActive: TLabel
    Left = 8
    Top = 13
    Width = 96
    Height = 13
    Caption = 'Active transactions:'
  end
  object lblProcesses: TLabel
    Left = 8
    Top = 204
    Width = 162
    Height = 13
    Caption = 'Processess that use transactions:'
  end
  object lvActiveTmTx: TListViewEx
    Left = 8
    Top = 40
    Width = 530
    Height = 150
    Anchors = [akLeft, akTop, akRight]
    Columns = <
      item
        Caption = 'GUID'
        Width = 260
      end
      item
        Caption = 'Description'
        Width = 180
      end
      item
        Alignment = taCenter
        Caption = 'Handles'
        Width = 60
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lvActiveTmTxDblClick
    ColoringItems = True
  end
  object btnNewTmTx: TButton
    Left = 464
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'New...'
    TabOrder = 1
    OnClick = btnNewTmTxClick
  end
  object lvHandles: TListViewEx
    Left = 8
    Top = 232
    Width = 530
    Height = 152
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Image name'
        Width = 180
      end
      item
        Caption = 'PID'
        Width = 70
      end
      item
        Caption = 'Handle'
        Width = 70
      end
      item
        Caption = 'Access mask'
        Width = 150
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    ViewStyle = vsReport
    OnDblClick = lvHandlesDblClick
    ColoringItems = True
  end
  object btnTransact: TButton
    Left = 408
    Top = 199
    Width = 131
    Height = 25
    Caption = 'Transact a process...'
    TabOrder = 3
  end
  object appEvents: TApplicationEvents
    OnException = appEventsException
    Left = 448
    Top = 136
  end
  object timerUpdate: TTimer
    OnTimer = timerUpdateTick
    Left = 352
    Top = 128
  end
end
