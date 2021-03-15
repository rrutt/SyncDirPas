unit SyncDir;

{ TODO : Check default form dimensions on Acer netbook. }

{$mode objfpc}{$H+}
{$WARN 5044 off : Symbol "$1" is not portable}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  LCLIntf,
  fileutil,
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
  gSourceDirectory: String;
  gTargetDirectory: String;

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
  result := Format(
      'Simulated Synchronization #%d.' + LineEnding +
      'Source Directory: %s' + LineEnding +
      'Target Directory: %s',
      [SyncDirLogForm.MemoLog.Lines.Count, gSourceDirectory, gTargetDirectory]);
end;

procedure AppendLogMessage(message: String);
begin
  SyncDirLogForm.MemoLog.Lines.Add(message);
end;

function SynchronizeSourceFilesToTargetDirectory(fileList: TStringList): Boolean;
var
  isSuccessful: Boolean;
  copySuccessful: Boolean;
  sourceFileFullPath: String;
  targetFileFullPath: String;
  fileIndex: Integer;
begin
  isSuccessful := true;

  // https://wiki.freepascal.org/CopyFile

  fileIndex := 0;
  while (fileIndex < fileList.Count) do begin
    sourceFileFullPath := EnsureDirectorySeparator(gSourceDirectory) + fileList.Strings[fileIndex];
    targetFileFullPath := EnsureDirectorySeparator(gTargetDirectory) + fileList.Strings[fileIndex];

    AppendLogMessage(Format('Synchronizing [%s] to [%s]', [sourceFileFullPath, targetFileFullPath]));
    copySuccessful := CopyFile(sourceFileFullPath, targetFileFullPath, [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]);
    if (copySuccessful) then begin
      AppendLogMessage(Format('Synchronized [%s] to [%s]', [sourceFileFullPath, targetFileFullPath]));
    end else begin
      isSuccessful := false;
      AppendLogMessage(Format('ERROR: Could not synchronize [%s] to [%s]', [sourceFileFullPath, targetFileFullPath]));
    end;

    inc(fileIndex);
  end;

  result := isSuccessful;
end;

function SynchronizeSourceToTarget: Boolean;
var
  isSuccessful: Boolean;
  searchInfo: TSearchRec;
  searchAttr: LongInt;
  dirList: TStringList;
  fileList: TStringList;
  filePrefix: String;
begin
  { TODO : If MinimizeLogMessages options is false,
           write detailed option settings to Log.
           Also show each sub-directory as it is being processed. }

  // https://www.freepascal.org/docs-html/rtl/classes/tstringlist.html
  // https://wiki.freepascal.org/TStringList
  // https://wiki.freepascal.org/TStringList-TStrings_Tutorial

  dirList := TStringList.Create;
  fileList := TStringList.Create;

  // https://www.freepascal.org/docs-html/rtl/sysutils/findfirst.html
  // https://www.freepascal.org/docs-html/rtl/sysutils/findnext.html
  searchAttr := faAnyFile;
  //searchAttr := searchAttr and (not faHidden);
  If FindFirst(EnsureDirectorySeparator(gSourceDirectory) + '*', searchAttr, searchInfo) = 0 then
    begin
    repeat
      with searchInfo do begin
        filePrefix := '';
        if (Attr and faHidden) = faHidden then begin
          filePrefix := filePrefix + 'Hidden ';
        end;
        if (Attr and faReadOnly) = faReadOnly then begin
          filePrefix := filePrefix + 'ReadOnly ';
        end;

        { TODO : If  MinimizeLogMessages is true, do NOT echo directory and file names. }
        if (Attr and faDirectory) = faDirectory then begin
          if ((Name <> '.') and (Name <> '..')) then begin
            AppendLogMessage(Format('%sDirectory: %s  Size: %d', [filePrefix, Name, Size]));
            dirList.Add(Name);
          end;
        end else begin
          AppendLogMessage(Format('%sFile: %s  Size: %d', [filePrefix, Name, Size]));
          fileList.Add(Name);
        end;
      end;
    until FindNext(searchInfo) <> 0;
    FindClose(searchInfo);
  end;

  AppendLogMessage(Format('Finished search. Found %d directories and %d files.', [dirList.Count, fileList.Count]));

  isSuccessful := SynchronizeSourceFilesToTargetDirectory(fileList);

  { TODO : Perform sub-directory synchronization, if option set. }

  dirList.Free;
  fileList.Free;

  if (isSuccessful) then begin
    AppendLogMessage('Synchronization complete.');
  end else begin
    AppendLogMessage('Synchronization failed!');
  end;

  result := isSuccessful;
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
    if (Pos(AnsiLowerCase(sourceDirectory), AnsiLowerCase(targetDirectory)) = 1) then begin
      isValid := false;
      AppendLogMessage('Error: Target Directory cannot be a sub-directory of Source Directory when "Include subdirectories" is checked');
      AppendLogMessage(Format('  Expanded Source Directory = %s', [sourceDirectory]));
      AppendLogMessage(Format('  Expanded Target Directory = %s', [targetDirectory]));
    end;

    if (Pos(AnsiLowerCase(targetDirectory), AnsiLowerCase(sourceDirectory)) = 1) then begin
      isValid := false;
      AppendLogMessage('Error: Source Directory cannot be a sub-directory of Target Directory when "Include subdirectories" is checked');
      AppendLogMessage(Format('  Expanded Source Directory = %s', [sourceDirectory]));
      AppendLogMessage(Format('  Expanded Target Directory = %s', [targetDirectory]));
    end;
  end;

  if (isValid) then begin
    gSourceDirectory := sourceDirectory;
    gTargetDirectory := targetDirectory;
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
  synchronizationSucceeded: Boolean;
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
    synchronizationSucceeded := SynchronizeSourceToTarget;

    { TODO : If NextSection has value,
             and synchronizationSucceeded is true,
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
  { TODO : If Automatic option is enabled,
           write log to SyncDir.log in current directory when complete.
           Add an initialization option for this?
           If so, write file based on that option rather than the Automatic option. }
  { TODO : Make LabelNextSection visible if a NextSection is active.
           Make invisible again when last section is being processed. }
end;

end.

