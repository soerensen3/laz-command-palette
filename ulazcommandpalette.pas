unit uLazCommandPalette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLoggerBase, FileUtil, Forms, Controls, Graphics, Dialogs,
  LCLType, StdCtrls, ComCtrls, IDECommands, IDEWindowIntf, LazIDEIntf, MenuIntf,
  IDEImagesIntf, ToolBarIntf, ListFilterEdit, ListViewFilterEdit, EditBtn;

type

  { TCommandPalette }

  TCommandPalette = class(TForm)
    SearchComEd: TListViewFilterEdit;
    ViewComList: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure SearchComEdAfterFilter(Sender: TObject);
    procedure ViewComListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FCommand: TIDECommand;
    { private declarations }

  public
    procedure UpdateCommandPalette;
    function ShowModal: Integer; override;

    property Command: TIDECommand read FCommand write FCommand;
  end;

var
  CommandPalette: TCommandPalette;
  CommandPaletteCreator: TIDEWindowCreator; // set by Register procedure

procedure ShowCommandPalette(Sender: TObject);
procedure Register; // Check the "Register Unit" of this unit in the package editor.implementation

implementation

{$R *.lfm}

procedure ShowCommandPalette(Sender: TObject);
begin
  if ( CommandPalette.ShowModal() = mrOK ) then
    CommandPalette.Command.Execute( nil );
end;

procedure CreateCommandPalette(Sender: TObject; aFormName: string;
  var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  // sanity check to avoid clashing with another package that has registered a window with the same name
  if CompareText(aFormName, 'CommandPalette')<>0 then begin
    DebugLn(['ERROR: CreateCommandPalette: there is already a form with this name']);
    exit;
  end;
  IDEWindowCreators.CreateForm(AForm, TCommandPalette, DoDisableAutoSizing,
    LazarusIDE.OwningComponent);
  AForm.Name:=aFormName;
  CommandPalette:=AForm as TCommandPalette;
end;

procedure Register;
var
  CmdCatViewMenu: TIDECommandCategory;
  ViewCommandPaletteCommand: TIDECommand;
  MenuItemCaption: String;
begin
  // register shortcut and menu item
  MenuItemCaption:='Show Command Palette'; // <- this caption should be replaced by a resourcestring
  // search shortcut category
  CmdCatViewMenu:=IDECommandList.FindCategoryByName(CommandCategoryViewName);
  // register shortcut
  ViewCommandPaletteCommand:= RegisterIDECommand(CmdCatViewMenu,
    'ViewCommandPalette',
    MenuItemCaption,
    IDEShortCut(VK_P, [ssShift, ssCtrl]), // <- set here your default shortcut
    CleanIDEShortCut, nil, @ShowCommandPalette);
  // register menu item in View menu
  RegisterIDEMenuCommand(itmViewMainWindows,
    'ViewCommandPalette',
    MenuItemCaption, nil, nil, ViewCommandPaletteCommand);

  CommandPalette := TCommandPalette.Create(Application);
end;

{ TCommandPalette }

procedure TCommandPalette.ViewComListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if ( Assigned( Item )) then
    Command:= TIDECommand( Item.Data );
end;

procedure TCommandPalette.FormCreate(Sender: TObject);
begin
  ViewComList.SmallImages:= IDEImages.Images_16;
  ViewComList.LargeImages:= IDEImages.Images_16;
end;

procedure TCommandPalette.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (( Key = VK_RETURN ) and ( Assigned( Command ))) then
    ModalResult:= mrOk;
  if ( Key = VK_ESCAPE ) then
    ModalResult:= mrCancel;
end;

procedure TCommandPalette.FormShow(Sender: TObject);
begin
  SearchComEd.SetFocus;
end;

procedure TCommandPalette.SearchComEdAfterFilter(Sender: TObject);
begin
  if ( ViewComList.Items.Count > 0 ) then
    ViewComList.Items.Item[ 0 ].Selected:= True;
end;

procedure TCommandPalette.UpdateCommandPalette;

  procedure ListAddCommand( Command: TIDECommand );
  var
    btn: TIDEButtonCommand;
  begin
    with ( ViewComList.Items.Add ) do begin
      Caption:= Command.Category.Name + ': ' + Command.Name;
      Data:= Command;
      btn:= IDEToolButtonCategories.FindItemByCommand( Command );
      if ( Assigned( btn )) then
        ImageIndex:= btn.ImageIndex;
    end;
  end;

var
  i, j: Integer;
begin
  SearchComEd.FilteredListview:= nil;
  SearchComEd.Items.Clear;
  ViewComList.Items.Clear;
  for i:= 0 to IDECommandList.CategoryCount - 1 do
    for j:= 0 to IDECommandList.Categories[ i ].Count - 1 do
      ListAddCommand( TIDECommand( IDECommandList.Categories[ i ][ j ]));
  SearchComEd.FilteredListview:= ViewComList;
end;

function TCommandPalette.ShowModal: Integer;
var
  i: Integer;
begin
  SearchComEd.ResetFilter;
  SearchComEd.Clear;

  UpdateCommandPalette;

  Result:= inherited ShowModal;
end;

end.

