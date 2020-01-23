object FormTransact: TFormTransact
  Left = 0
  Top = 0
  Caption = 'Assign a process a new transaction...'
  ClientHeight = 236
  ClientWidth = 380
  Color = clBtnFace
  Constraints.MinHeight = 270
  Constraints.MinWidth = 260
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblDescription: TLabel
    Left = 8
    Top = 8
    Width = 115
    Height = 13
    Caption = 'Transaction description:'
  end
  object btnContinue: TButton
    Left = 216
    Top = 202
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Continue'
    Default = True
    ModalResult = 11
    TabOrder = 1
    OnClick = btnContinueClick
  end
  object btnCancel: TButton
    Left = 297
    Top = 203
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object Pages: TPageControl
    Left = 8
    Top = 57
    Width = 364
    Height = 134
    ActivePage = tabNewProcess
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    object tabNewProcess: TTabSheet
      Caption = 'New process'
      object lblExecutable: TLabel
        Left = 3
        Top = 5
        Width = 57
        Height = 13
        Caption = 'Executable:'
      end
      object lblParameters: TLabel
        Left = 3
        Top = 53
        Width = 59
        Height = 13
        Caption = 'Parameters:'
      end
      object btnBrowse: TButton
        Left = 278
        Top = 22
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Browse...'
        TabOrder = 1
        OnClick = btnBrowseClick
      end
      object tbExecutable: TEdit
        Left = 3
        Top = 24
        Width = 269
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object tbParameters: TEdit
        Left = 3
        Top = 72
        Width = 350
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
    end
    object tabExistingProcess: TTabSheet
      Caption = 'Existing process'
      ImageIndex = 1
      object lblProcess: TLabel
        Left = 3
        Top = 5
        Width = 41
        Height = 13
        Caption = 'Process:'
      end
      object btnProcessSelect: TButton
        Left = 278
        Top = 22
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Select...'
        TabOrder = 1
        OnClick = btnProcessSelectClick
      end
      object tbProcess: TButtonedEdit
        Left = 3
        Top = 24
        Width = 269
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        DoubleBuffered = False
        Enabled = False
        LeftButton.Visible = True
        ParentDoubleBuffered = False
        ReadOnly = True
        TabOrder = 0
        TextHint = 'Select a process'
      end
      object cbThreadsFuture: TCheckBox
        Left = 3
        Top = 66
        Width = 350
        Height = 17
        Hint = 
          'Load a thread-tracking DLL into the target process that will aut' +
          'omatically assign new threads to the transaction.'
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Include all future threads'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
    end
  end
  object tbDescription: TEdit
    Left = 8
    Top = 24
    Width = 364
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    Text = 'TM Exteriment #42'
  end
  object dlgOpen: TOpenDialog
    Filter = 'Executable files|*.exe; *.com; *.scr|All files (*.*)|*'
    Options = [ofEnableSizing]
    Left = 312
    Top = 8
  end
end
