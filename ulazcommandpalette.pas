unit uLazCommandPalette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLoggerBase, FileUtil, Forms, Controls, Graphics, Dialogs,
  LCLType, StdCtrls, ComCtrls, IDECommands, IDEWindowIntf, LazIDEIntf, MenuIntf;

type

  { TCommandPalette }

  TCommandPalette = class(TForm)
    SearchComEd: TEdit;
    ViewComList: TListView;
    procedure SearchComEdChange(Sender: TObject);
  private
    { private declarations }
  public
    procedure UpdateCommandPalette;
    function ShowModal: Integer; override;
    { public declarations }
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
  CommandPalette.ShowModal();
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

  // register dockable Window
  {CommandPaletteCreator:=IDEWindowCreators.Add(
    'CommandPalette',
    @CreateCommandPalette, nil,
    '100', '100', '300', '300'  // default place at left=100, top=100, right=300, bottom=300
      // you can also define percentage values of screen or relative positions, see wiki
    );}

end;

{ TCommandPalette }

procedure TCommandPalette.SearchComEdChange(Sender: TObject);
begin

end;

procedure TCommandPalette.UpdateCommandPalette;

  procedure ListAddCommand( Command: TIDECommand );
  begin
    with ( ViewComList.Items.Add ) do begin
      Caption:= Command.Category.Name + ': ' + Command.Name;
      Data:= Command;
    end;
  end;

var
  i, j: Integer;
begin
  ViewComList.Clear;
  for i:= 0 to IDECommandList.CategoryCount - 1 do
    for j:= 0 to IDECommandList.Categories[ i ].Count - 1 do
      ListAddCommand( TIDECommand( IDECommandList.Categories[ i ][ j ]));
end;

function TCommandPalette.ShowModal: Integer;
begin
  UpdateCommandPalette;
  //ViewComList.fi;
  Result:=inherited ShowModal;
end;

end.

