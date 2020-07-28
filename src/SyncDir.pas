unit SyncDir;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  LCLIntf,
  SyncDirLog;

type

  { TSyncDirForm }

  TSyncDirForm = class(TForm)
    ButtonHelp: TButton;
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
    DirectoryEditSource: TDirectoryEdit;
    DirectoryEditTarget: TDirectoryEdit;
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
    function ValidateSourceAndTargetDirectories: Boolean;
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);
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

function EnsureDirectorySeparator(directoryPath: String): String;
var resultPath: String;
begin
  resultPath := directoryPath;
  if (not (AnsiLastChar(directoryPath) = DirectorySeparator)) then begin
    resultPath := resultPath + DirectorySeparator;
  end;

  result := resultPath;
end;

function FormatMockLogMessage: String;
begin
  result := Format('Simulated Synchronization #%d.', [SyncDirLogForm.MemoLog.Lines.Count]);
end;

procedure AppendLogMessage(message: String);
begin
  SyncDirLogForm.MemoLog.Lines.Add(message);
end;

function TSyncDirForm.ValidateSourceAndTargetDirectories: Boolean;
var
  isValid: Boolean = true;
  sourceDirectory: String;
  targetDirectory: String;
begin
  sourceDirectory := Trim(DirectoryEditSource.Text);
  if (sourceDirectory = '') then begin
    isValid := false;
    AppendLogMessage('Error: Source Directory is required');
  end else if (not DirectoryExists(sourceDirectory)) then begin
    isValid := false;
    AppendLogMessage(Format('Error: Invalid Source Directory: %s', [sourceDirectory]));
  end;

  targetDirectory := Trim(DirectoryEditTarget.Text);
  if (targetDirectory = '') then begin
    isValid := false;
    AppendLogMessage('Error: Target Directory is required');
  end else if (not DirectoryExists(targetDirectory)) then begin
    isValid := false;
    AppendLogMessage(Format('Error: Invalid Target Directory: %s', [targetDirectory]));
  end;

  if (isValid) then begin
    sourceDirectory := ExpandFileName(EnsureDirectorySeparator(sourceDirectory));
    targetDirectory := ExpandFileName(EnsureDirectorySeparator(targetDirectory));

    if (AnsiCompareText(sourceDirectory, targetDirectory) = 0) Then begin
      isValid := false;
      AppendLogMessage('Error: Source and Target Directories cannot be the same.');
      AppendLogMessage(Format('  Expanded Source Directory = %s', [sourceDirectory]));
      AppendLogMessage(Format('  Expanded Target Directory = %s', [targetDirectory]));
    end;
  end;

  if (isValid and CheckBoxIncludeSubdirectories.Checked) then begin
    if (Pos(AnsiLowerCase(sourceDirectory), AnsiLowerCase(targetDirectory)) > 0) then begin
      isValid := false;
      AppendLogMessage('Error: Target Directory cannot be a sub-directory of Source Directory when "Include subdirectories" is checked');
      AppendLogMessage(Format('  Expanded Source Directory = %s', [sourceDirectory]));
      AppendLogMessage(Format('  Expanded Target Directory = %s', [targetDirectory]));
    end;

    if (Pos(AnsiLowerCase(targetDirectory), AnsiLowerCase(sourceDirectory)) > 0) then begin
      isValid := false;
      AppendLogMessage('Error: Source Directory cannot be a sub-directory of Target Directory when "Include subdirectories" is checked');
      AppendLogMessage(Format('  Expanded Source Directory = %s', [sourceDirectory]));
      AppendLogMessage(Format('  Expanded Target Directory = %s', [targetDirectory]));
    end;
  end;

  result := isValid;
end;

procedure TSyncDirForm.ButtonExitClick(Sender: TObject);
begin
  Halt(1);
end;

procedure TSyncDirForm.ButtonHelpClick(Sender: TObject);
var
  helpFileURL: string;
begin
  helpFileURL := ExtractFilePath(Application.ExeName) + 'SyncDir.html';
  //ShowMessage('Help File URL = ' + helpFileURL);
  OpenURL(helpFileURL);
end;

procedure TSyncDirForm.ButtonShowLogClick(Sender: TObject);
begin
  SyncDirLogForm.Show;
end;

procedure TSyncDirForm.ButtonSynchronizeClick(Sender: TObject);
var
  optionsAreValid: Boolean = true;
begin
  if (optionsAreValid) then begin
    optionsAreValid := ValidateSourceAndTargetDirectories();
  end;

  { TODO : Validate other option combinations. }

  if (not optionsAreValid) then begin
    AppendLogMessage('Synchronization cancelled due to invalid options.');
  end else begin
    { TODO : Remove simulated activity of writing to SyncDirLog TMemo. }
    AppendLogMessage(FormatMockLogMessage);

    { TODO : Perform file synchronization. }
    { TODO : Function to copy a file:
             https://wiki.freepascal.org/CopyFile }
    { TODO : If NextSection has value,
             iterate file synchronization thru successive section(s).
             (Set user-interface options on main form as each section is processed.) }
  end;

  { TODO : Should we show log form while synchronizing, or only when done? }
  SyncDirLogForm.Show;
end;

procedure TSyncDirForm.CheckBoxProcessHiddenFilesChange(Sender: TObject);
begin
  DirectoryEditSource.ShowHidden := CheckBoxProcessHiddenFiles.Checked;
  DirectoryEditTarget.ShowHidden := CheckBoxProcessHiddenFiles.Checked;
end;

procedure TSyncDirForm.FormCreate(Sender: TObject);
var
  currentWorkingDirectory: String = '';
  initFileName: String;
  initSection: String;
begin
  currentWorkingDirectory := GetCurrentDir;
  //ShowMessage('Current Working Directory = ' + currentWorkingDirectory);

  initFileName := paramStr(1);
  if (initFileName = '') then begin
    initFileName := 'SyncDir.ini';
  end;
  if (ExtractFilePath(initFileName) = '') then begin
    initFileName := currentWorkingDirectory + DirectorySeparator  + initFileName;
  end;
  LabelInitializationFileValue.Caption := initFileName;

  initSection := paramStr(2);
  if (initSection = '') then begin
    initSection := 'SyncDir';
  end;
  LabelInitializationSectionValue.Caption := '[' + initSection + ']';

  { TODO : Load initialization file sections and parameters into memory collections via
           https://wiki.freepascal.org/Using_INI_Files
           https://www.freepascal.org/docs-html/fcl/inifiles/tinifile.html
           https://www.freepascal.org/docs-html/fcl/inifiles/tinifile-3.html}
  { TODO : Set interpretation of True and False ini file strings via:
https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.booltruestrings.html
           and
https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.boolfalsestrings.html}

  LabelNextSection.Visible := false;
  LabelNextSectionValue.Caption := '';
  { TODO : Initialize user-interface options based on initialization file primary section. }
  { TODO : If Automatic option is selected in initialization settings,
           hide forms and start processing primary section,
           unless NotifyUser option is set.
           Halt application when done processing. }
  { TODO : Make LabelNextSection visible if a NextSection is active.
           Make invisible again when last section is being processed. }
end;

end.

