unit SyncDir;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls;

type

  { TSyncDirForm }

  TSyncDirForm = class(TForm)
    ButtonExit: TButton;
    ButtonSynchronize: TButton;
    CheckBoxSkipReadOnlyTargetFiles: TCheckBox;
    CheckBoxDeleteExtraFiles: TCheckBox;
    CheckBoxDeleteExtraDirectories: TCheckBox;
    CheckBoxSkipMissingDirectories: TCheckBox;
    CheckBoxIncludeSubdirectories: TCheckBox;
    CheckBoxMinimizeLogMessages: TCheckBox;
    CheckBoxShowErrorMessages: TCheckBox;
    CheckBoxSynchronizeBothWays: TCheckBox;
    CheckBoxProcessHiddenFiles: TCheckBox;
    CheckBoxCopyOlderFiles: TCheckBox;
    DirectoryEditTarget: TDirectoryEdit;
    DirectoryEditSource: TDirectoryEdit;
    EditOnlyProcessFileTypes: TEdit;
    EditIgnoreFileTypes: TEdit;
    LabelInitializationFileSectionValue: TLabel;
    LabelInitializationFileSection: TLabel;
    LabelOnlyProcessFileTypes: TLabel;
    LabelIgnoreFileTypes: TLabel;
    LabelTargetDirectory: TLabel;
    LabelSourceDirectory: TLabel;
    procedure ButtonExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  SyncDirForm: TSyncDirForm;

implementation

{$R *.lfm}

{ TSyncDirForm }

procedure TSyncDirForm.ButtonExitClick(Sender: TObject);
begin
  { TODO : Close application. }
end;

procedure TSyncDirForm.FormCreate(Sender: TObject);
begin

end;

end.

