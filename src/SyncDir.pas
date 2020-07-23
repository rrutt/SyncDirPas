unit SyncDir;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  SyncDirLog;

type

  { TSyncDirForm }

  TSyncDirForm = class(TForm)
    ButtonShowLog: TButton;
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
    LabelNextSectionValue: TLabel;
    LabelNextSection: TLabel;
    LabelInitializationFileValue: TLabel;
    LabelInitializationFile: TLabel;
    LabelInitializationSectionValue: TLabel;
    LabelInitializationSection: TLabel;
    LabelOnlyProcessFileTypes: TLabel;
    LabelIgnoreFileTypes: TLabel;
    LabelTargetDirectory: TLabel;
    LabelSourceDirectory: TLabel;
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonShowLogClick(Sender: TObject);
    procedure ButtonSynchronizeClick(Sender: TObject);
    procedure CheckBoxProcessHiddenFilesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  SyncDirForm: TSyncDirForm;

implementation

{$R *.lfm}

{ TSyncDirForm }

function FormatMockLogMessage: string;
begin
  result := Format('Simulated Synchronization #%d.', [SyncDirLogForm.MemoLog.Lines.Count]);
end;

procedure AppendLogMessage(message: string);
begin
  SyncDirLogForm.MemoLog.Lines.Add(message);
end;

procedure TSyncDirForm.ButtonExitClick(Sender: TObject);
begin
  Halt(1);
end;

procedure TSyncDirForm.ButtonShowLogClick(Sender: TObject);
begin
  SyncDirLogForm.Show;
end;

procedure TSyncDirForm.ButtonSynchronizeClick(Sender: TObject);
begin
  { TODO : Remove simulated activity of writing to SyncDirLog TMemo. }
  AppendLogMessage(FormatMockLogMessage);

  { TODO : Validate source and target directory selections. }
  { TODO : Perform file synchronization. }
  SyncDirLogForm.Show;
end;

procedure TSyncDirForm.CheckBoxProcessHiddenFilesChange(Sender: TObject);
begin
  DirectoryEditSource.ShowHidden := CheckBoxProcessHiddenFiles.Checked;
  DirectoryEditTarget.ShowHidden := CheckBoxProcessHiddenFiles.Checked;
end;

procedure TSyncDirForm.FormCreate(Sender: TObject);
begin
  { TODO : Process command-line arguments. }
  { TODO : Load initialization file sections and parameters into memory collections. }

  LabelNextSection.Visible := false;
  LabelNextSectionValue.Caption := '';
  { TODO : Update user-interface options based on initialization file primary section. }
end;

end.

