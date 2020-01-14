unit ProcessList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, VclEx.ListView,
  Vcl.StdCtrls, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls,
  NtUtils.Processes.Snapshots, DelphiUtils.Arrays;

type
  TFormProcessList = class(TForm)
    lvProcesses: TListViewEx;
    btnOk: TButton;
    SearchBox: TButtonedEdit;
    SearchButtons: TImageList;
    btnRefresh: TButton;
    procedure SearchBoxChange(Sender: TObject);
    procedure SearchBoxRightButtonClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    ProcessTree: TArray<TTreeNode<TProcessEntry>>;
    function ComputeIndent(const Node: TTreeNode<TProcessEntry>): Integer;
    procedure AddProcessNode(const Node: TTreeNode<TProcessEntry>;
      Level: Integer);
    procedure SelectFirstVisible;
  public
    class function Pick(AOwner: TComponent): TProcessEntry;
  end;

implementation

uses
  NtUiLib.Icons, NtUtils.Exceptions;

{$R *.dfm}

const
  GROUP_SEARCH_IND = 0;

procedure TFormProcessList.AddProcessNode(const Node: TTreeNode<TProcessEntry>;
  Level: Integer);
var
  i: Integer;
begin
  // Add parent node
  with lvProcesses.Items.Add do
  begin
    Cell[0] := Node.Entry.ImageName;
    Cell[1] := IntToStr(Node.Entry.Process.ProcessId);
    Cell[2] := IntToStr(Node.Entry.Process.SessionId);
    ImageIndex := TProcessIcons.GetIconByPid(Node.Entry.Process.ProcessId);
    Indent := Level;
    Data := @Node;
  end;

  // Add all children
  for i := 0 to High(Node.Children) do
    AddProcessNode(Node.Children[i]^, Level + 1);
end;

procedure TFormProcessList.btnRefreshClick(Sender: TObject);
var
  i: Integer;
begin
  lvProcesses.Items.BeginUpdate;
  lvProcesses.Items.Clear;

  try
    if not NtxEnumerateProcessesEx(ProcessTree).IsSuccess then
      Exit;

    // Add parentless processes and then all other recursively
    for i := 0 to High(ProcessTree) do
      if not Assigned(ProcessTree[i].Parent) then
        AddProcessNode(ProcessTree[i], 0);

  finally
    lvProcesses.Items.EndUpdate;
    SearchBoxChange(Self);
  end;
end;

function TFormProcessList.ComputeIndent(const Node: TTreeNode<TProcessEntry>): Integer;
var
  pNode: ^TTreeNode<TProcessEntry>;
begin
  Result := -1;
  pNode := @Node;

  repeat
    pNode := pNode.Parent;
    Inc(Result);
  until not Assigned(pNode);
end;

procedure TFormProcessList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFormProcessList.FormCreate(Sender: TObject);
begin
  lvProcesses.SmallImages := TProcessIcons.ImageList;
  btnRefreshClick(Self);
end;

procedure TFormProcessList.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // <Ctrl+F> to seacrh
  if (Shift = [ssCtrl]) and (Key = Ord('F')) then
    SearchBox.SetFocus

  // <Up/down/PageUp/PageDown> to navigate
  else if (Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT]) then
    lvProcesses.SetFocus

  // <Esc> to clear search or exit
  else if Key = VK_ESCAPE then
  begin
    if SearchBox.Text <> '' then
      SearchBox.Text := ''
    else
      Close;
  end;
end;

class function TFormProcessList.Pick(AOwner: TComponent): TProcessEntry;
begin
  with TFormProcessList.Create(AOwner) do
  begin
    if ShowModal <> mrOk then
      Abort;

    if not Assigned(lvProcesses.Selected) then
      Abort;

    Assert(Assigned(lvProcesses.Selected.Data));

    Result := TTreeNode<TProcessEntry>(lvProcesses.Selected.Data^).Entry;
  end;
end;

procedure TFormProcessList.SearchBoxChange(Sender: TObject);
var
  i: Integer;
begin
  SearchBox.RightButton.Visible := (SearchBox.Text <> '');
  lvProcesses.GroupView := (SearchBox.Text <> '');

  // Search enables viewing groups in listvew and disables indents.
  // All items that match the query end up in the search-result group.
  // The rest go to the -1 group that is not displayed.

  lvProcesses.Items.BeginUpdate;
  for i := 0 to lvProcesses.Items.Count - 1 do
    with lvProcesses.Items[i] do
      if (SearchBox.Text <> '') and Matches(SearchBox.Text) then
      begin
        Indent := 0;
        GroupID := GROUP_SEARCH_IND;
      end
      else
      begin
        Indent := ComputeIndent(TTreeNode<TProcessEntry>(Data^));
        GroupID := -1;
      end;

  SelectFirstVisible;
  lvProcesses.Items.EndUpdate;
end;

procedure TFormProcessList.SearchBoxRightButtonClick(Sender: TObject);
begin
  SearchBox.Text := '';
end;

procedure TFormProcessList.SelectFirstVisible;
var
  i: Integer;
begin
  for i := 0 to lvProcesses.Items.Count - 1 do
    if (SearchBox.Text = '') or (lvProcesses.Items[i].GroupID =
      GROUP_SEARCH_IND) then
    begin
      lvProcesses.Items[i].Selected := True;
      Break;
    end;
end;

end.
