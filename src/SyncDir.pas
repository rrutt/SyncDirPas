unit SyncDir;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  LCLIntf,
  fileutil,
  inifiles,
  SyncDirLog;

type

  TOptions = record
    AreValid: Boolean;

    SourceDirectory: String;
    TargetDirectory: String;

    CopyOlderFiles: Boolean;
    DeleteExtraFiles: Boolean;
    DeleteExtraDirectories: Boolean;
    IncludeSubdirectories: Boolean;
    MinimizeLogMessages: Boolean;
    ProcessHiddenFiles: Boolean;
    ShowErrorMessages: Boolean;
    SkipMissingDirectories: Boolean;
    SkipReadOnlyTargetFiles: Boolean;
    SynchronizeBothWays: Boolean;

    OnlyProcessFileTypes: String;
    IgnoreFileTypes: String;
    NextSection: String;
  end;

  TProgressContext = record
    SynchronizationSucceeded: Boolean;
    DirList: TStringList;
    DirCount: LongInt;
    SubDirCount: LongInt;
    DeletedSubDirCount: LongInt;
    MissingSubDirCount: LongInt;
    FileList: TStringList;
    FileCount: LongInt;
    DeletedFileCount: LongInt;
    ErrorFileCount: LongInt;
    ReadOnlyFileCount: LongInt;
    SkippedFileCount: LongInt;
    SuccessfulFileCount: LongInt;
  end;

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
    function InitializeProgressContext: TProgressContext;
    procedure LoadInitializationFileSettings(iniFileFullPath: String; initSection: String; var options: TOptions);
    procedure LoadInitialOptionsFromFormControls(var options: TOptions);
    procedure LoadFormControlsFromOptions(const initSection: String; const options: TOptions);
    procedure ValidateSourceAndTargetDirectories(var options: TOptions);
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
  gInitialOptions: TOptions;

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

procedure AppendLogMessage(message: String);
begin
  SyncDirLogForm.MemoLog.Lines.Add(message);
end;

function LoadInitializationFileSettingString(iniFile: TINIFile; sectionName: String; settingName: String; defaultValue: String): String;
var
  settingValue: String;
begin
  settingValue := iniFile.ReadString(sectionName, settingName, defaultValue);
  result := settingValue;
end;

function LoadInitializationFileSettingBoolean(iniFile: TINIFile; sectionName: String; settingName: String; defaultValue: Boolean): Boolean;
var
  settingValue: Boolean;
begin
  // https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.booltruestrings.html
  //https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.boolfalsestrings.html}
  settingValue := iniFile.ReadBool(sectionName, settingName, defaultValue);
  result := settingValue;
end;

procedure TSyncDirForm.LoadInitializationFileSettings(iniFileFullPath: String; initSection: String; var options: TOptions);
var
  iniFile: TINIFile;
begin
  // https://wiki.freepascal.org/Using_INI_Files
  // https://www.freepascal.org/docs-html/fcl/inifiles/tinifile-3.html
  // https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.sectionexists.html
  // https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.booltruestrings.html
  // https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.boolfalsestrings.html}

  iniFile := TINIFile.Create(iniFileFullPath);
  try
    // https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.booltruestrings.html
    // https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.boolfalsestrings.html}
    iniFile.BoolTrueStrings := ['true', 't', 'yes', 'y', '1'];
    iniFile.BoolFalseStrings := ['false', 'f', 'no', 'n', '0'];

    options.SourceDirectory := LoadInitializationFileSettingString(iniFile, initSection, 'SourceDirectory', '.');
    options.TargetDirectory := LoadInitializationFileSettingString(iniFile, initSection, 'TargetDirectory', '.');

    options.CopyOlderFiles := LoadInitializationFileSettingBoolean(iniFile, initSection, 'CopyOlderFiles', false);
    options.DeleteExtraFiles := LoadInitializationFileSettingBoolean(iniFile, initSection, 'DeleteExtraFiles', false);
    options.DeleteExtraDirectories := LoadInitializationFileSettingBoolean(iniFile, initSection, 'DeleteExtraDirectories', false);
    options.IncludeSubdirectories := LoadInitializationFileSettingBoolean(iniFile, initSection, 'IncludeSubdirectories', false);
    options.MinimizeLogMessages := LoadInitializationFileSettingBoolean(iniFile, initSection, 'MinimizeLogMessages', true);
    options.ProcessHiddenFiles := LoadInitializationFileSettingBoolean(iniFile, initSection, 'ProcessHiddenFiles', false);
    options.ShowErrorMessages := LoadInitializationFileSettingBoolean(iniFile, initSection, 'ShowErrorMessages', true);
    options.SkipMissingDirectories := LoadInitializationFileSettingBoolean(iniFile, initSection, 'SkipMissingDirectories', false);
    options.SkipReadOnlyTargetFiles := LoadInitializationFileSettingBoolean(iniFile, initSection, 'SkipReadOnlyTargetFiles', false);
    options.SynchronizeBothWays := LoadInitializationFileSettingBoolean(iniFile, initSection, 'SynchronizeBothWays', false);

    options.OnlyProcessFileTypes := LoadInitializationFileSettingString(iniFile, initSection, 'OnlyProcessFileTypes', '');
    options.IgnoreFileTypes := LoadInitializationFileSettingString(iniFile, initSection, 'IgnoreFileTypes', '');
    options.NextSection := LoadInitializationFileSettingString(iniFile, initSection, 'NextSection', '');

  finally
    // After the INI file was used it must be freed to prevent memory leaks.
    iniFile.Free;
  end;
end;

procedure TSyncDirForm.LoadInitialOptionsFromFormControls(var options: TOptions);
begin
  options.AreValid := true;

  options.SourceDirectory := Trim(DirectoryEditSource.Text);
  options.TargetDirectory := Trim(DirectoryEditTarget.Text);

  options.CopyOlderFiles := CheckBoxCopyOlderFiles.Checked;
  options.DeleteExtraFiles := CheckBoxDeleteExtraFiles.Checked;
  options.DeleteExtraDirectories := CheckBoxDeleteExtraDirectories.Checked;
  options.IncludeSubdirectories := CheckBoxIncludeSubdirectories.Checked;
  options.MinimizeLogMessages := CheckBoxMinimizeLogMessages.Checked;
  options.ProcessHiddenFiles := CheckBoxProcessHiddenFiles.Checked;
  options.ShowErrorMessages := CheckBoxShowErrorMessages.Checked;
  options.SkipMissingDirectories := CheckBoxSkipMissingDirectories.Checked;
  options.SkipReadOnlyTargetFiles := CheckBoxSkipReadOnlyTargetFiles.Checked;
  options.SynchronizeBothWays := CheckBoxSynchronizeBothWays.Checked;

  options.OnlyProcessFileTypes := EditOnlyProcessFileTypes.Text;
  options.IgnoreFileTypes := EditIgnoreFileTypes.Text;
  options.NextSection := LabelNextSectionValue.Caption;
end;

procedure TSyncDirForm.LoadFormControlsFromOptions(const initSection: String; const options: TOptions);
begin
  DirectoryEditSource.Text := options.SourceDirectory;
  DirectoryEditTarget.Text := options.TargetDirectory;

  CheckBoxCopyOlderFiles.Checked := options.CopyOlderFiles;
  CheckBoxDeleteExtraFiles.Checked := options.DeleteExtraFiles;
  CheckBoxDeleteExtraDirectories.Checked := options.DeleteExtraDirectories;
  CheckBoxIncludeSubdirectories.Checked := options.IncludeSubdirectories;
  CheckBoxMinimizeLogMessages.Checked := options.MinimizeLogMessages;
  CheckBoxProcessHiddenFiles.Checked := options.ProcessHiddenFiles;
  CheckBoxShowErrorMessages.Checked := options.ShowErrorMessages;
  CheckBoxSkipMissingDirectories.Checked := options.SkipMissingDirectories;
  CheckBoxSkipReadOnlyTargetFiles.Checked := options.SkipReadOnlyTargetFiles;
  CheckBoxSynchronizeBothWays.Checked := options.SynchronizeBothWays;

  EditOnlyProcessFileTypes.Text := options.OnlyProcessFileTypes;
  EditIgnoreFileTypes.Text := options.IgnoreFileTypes;

  LabelInitializationSectionValue.Caption := '[' + initSection + ']';

  LabelNextSectionValue.Caption := options.NextSection;
  if (options.NextSection <> '') then begin
    LabelNextSection.Visible := true;
  end else begin
    LabelNextSection.Visible := false;
  end;
end;

procedure RecursivelyScanSourceDirectoriesAndFiles(
    var context: TProgressContext;
    var options: TOptions;
    const sourceDirectory: String;
    const {%H-}targetDirectory: String);
var
  searchInfo: TSearchRec;
  searchAttr: LongInt;
  filePrefix: String;
begin
  // https://www.freepascal.org/docs-html/rtl/sysutils/findfirst.html
  // https://www.freepascal.org/docs-html/rtl/sysutils/findnext.html
  searchAttr := faAnyFile;
  if (not options.ProcessHiddenFiles) then begin
    searchAttr := searchAttr and (not faHidden{%H-});
  end;
  If FindFirst(EnsureDirectorySeparator(sourceDirectory) + '*', searchAttr, searchInfo) = 0 then
    begin
    repeat
      with searchInfo do begin
        filePrefix := '';
        if (Attr and faHidden{%H-}) = faHidden{%H-} then begin
          filePrefix := filePrefix + 'Hidden ';
        end;
        if (Attr and faReadOnly) = faReadOnly then begin
          filePrefix := filePrefix + 'ReadOnly ';
        end;

        { TODO : If  MinimizeLogMessages is true, do NOT echo directory and file names. }
        if (Attr and faDirectory) = faDirectory then begin
          if ((Name <> '.') and (Name <> '..')) then begin
            AppendLogMessage(Format('%sDirectory: %s  Size: %d', [filePrefix, Name, Size]));
            context.DirList.Add(Name);
          end;
        end else begin
          { TODO : Filter file list based on IgnoreFileTypes, OnlyProcessFileTypes, and SkipReadOnlyTargetFiles options. }
          AppendLogMessage(Format('%sFile: %s  Size: %d', [filePrefix, Name, Size]));
          context.FileList.Add(Name);
        end;
      end;
    until FindNext(searchInfo) <> 0;
    FindClose(searchInfo);
  end;

  { TODO : Perform sub-directory directory/file scan, if option set. }
  { TODO : If SkipMissingDirectories is true,
           check TargetDir for pre-existence of a matching directory. }
end;

function SynchronizeSourceFilesToTargetDirectory(var context: TProgressContext; var options: TOptions): Boolean;
var
  isSuccessful: Boolean;
  copySuccessful: Boolean;
  sourceFileFullPath: String;
  targetFileFullPath: String;
  sourceFileAge: LongInt;
  targetFileAge: LongInt;
  sourceFileDate: TDateTime;
  targetFileDate: TDateTime;
  fileIndex: LongInt;
begin
  isSuccessful := true;

  // https://wiki.freepascal.org/CopyFile

  { TODO : If NotifyUser option is true, perform two-passes of synchronization.
           One to obtain file and directory counts;
           the second to actual synchronize if user agrees. }

  fileIndex := 0;
  while (fileIndex < context.FileList.Count) do begin
    sourceFileFullPath := EnsureDirectorySeparator(options.SourceDirectory) + context.FileList.Strings[fileIndex];
    targetFileFullPath := EnsureDirectorySeparator(options.TargetDirectory) + context.FileList.Strings[fileIndex];

    sourceFileAge := FileAge(sourceFileFullPath);
    if (sourceFileAge < 0) then begin
      AppendLogMessage(
          Format('Source file age = %d for [%s]',
              [sourceFileAge, sourceFileFullPath]));
    end else begin
      sourceFileDate := FileDateToDateTime(sourceFileAge);
      AppendLogMessage(
          Format('Source file age = %d = %s for [%s]',
              [sourceFileAge, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', sourceFileDate), sourceFileFullPath]));
    end;

    targetFileAge := FileAge(targetFileFullPath);
    if (targetFileAge < 0) then begin
      AppendLogMessage(
          Format('Target file age = %d for [%s]',
              [targetFileAge, targetFileFullPath]));
    end else begin
      targetFileDate := FileDateToDateTime(targetFileAge);
      AppendLogMessage(
        Format('Target file age = %d = %s for [%s]',
            [targetFileAge, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', targetFileDate), targetFileFullPath]));
    end;

    if ((options.CopyOlderFiles and (sourceFileAge <> targetFileAge)) or (sourceFileAge > targetFileAge)) then begin
      { TODO : Check SkipReadOnlyTargetFiles option. }
      // https://www.freepascal.org/docs-html/rtl/sysutils/filegetattr.html
      copySuccessful := CopyFile(sourceFileFullPath, targetFileFullPath, [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]);
      if (copySuccessful) then begin
        Inc(context.SuccessfulFileCount);
        AppendLogMessage(Format('Synchronized [%s] into [%s]', [sourceFileFullPath, targetFileFullPath]));
      end else begin
        isSuccessful := false;
        Inc(context.ErrorFileCount);
        { TODO : Determine impact of ShowErrorMessages option. }
        AppendLogMessage(Format('ERROR: Could not synchronize [%s] into [%s]', [sourceFileFullPath, targetFileFullPath]));
      end;
    end else begin
      Inc(context.SkippedFileCount);
      AppendLogMessage(Format('Skipped copying [%s] to [%s] based on file timestamps.', [sourceFileFullPath, targetFileFullPath]));
    end;

    inc(fileIndex);
  end;

  result := isSuccessful;
end;

procedure SynchronizeSourceToTarget(var context: TProgressContext; var options: TOptions);
var
  isSuccessful: Boolean;
begin
  AppendLogMessage(Format('Synchronizing [%s] into [%s] ...', [options.SourceDirectory, options.TargetDirectory]));

  { TODO : If MinimizeLogMessages options is false,
           write detailed option settings to Log.
           Also show each sub-directory as it is being processed. }

  RecursivelyScanSourceDirectoriesAndFiles(context, options, options.SourceDirectory, options.TargetDirectory);

  context.DirCount := context.DirList.Count;
  context.FileCount := context.FileList.Count;
  AppendLogMessage(Format('Finished search. Found %d directories and %d files.', [context.DirCount, context.FileCount]));

  { TODO : Honor SynchronizeBothWays option. }

  { TODO : Honor DeleteExtraFiles and DeleteExtraDirectories options. }
  // https://www.freepascal.org/docs-html/rtl/sysutils/deletefile.html
  // https://www.freepascal.org/docs-html/rtl/sysutils/removedir.html

  { TODO : Honor SkipMissingDirectories option. }

  isSuccessful := SynchronizeSourceFilesToTargetDirectory(context, options);

  context.DirList.Free;
  context.FileList.Free;

  if (isSuccessful) then begin
    AppendLogMessage(Format('Synchronization of [%s] into [%s] completed.', [options.SourceDirectory, options.TargetDirectory]));
  end else begin
    AppendLogMessage(Format('Synchronization of [%s] into [%s] failed!', [options.SourceDirectory, options.TargetDirectory]));
  end;

  AppendLogMessage(Format('  Successfully copied %d file(s).', [context.SuccessfulFileCount]));
  if (context.ReadOnlyFileCount > 0) then begin
    AppendLogMessage(Format('  Bypassed %d read-only file(s).', [context.ReadOnlyFileCount]));
  end;
  if (context.SkippedFileCount > 0) then begin
    AppendLogMessage(Format('  Skipped copying %d file(s).', [context.SkippedFileCount]));
  end;
  if (context.ErrorFileCount > 0) then begin
    AppendLogMessage(Format('  Encountered error copying %d file(s).', [context.ErrorFileCount]));
  end;

  context.SynchronizationSucceeded := isSuccessful;
end;

function TSyncDirForm.InitializeProgressContext: TProgressContext;
var
  context: TProgressContext;
begin
  context.SynchronizationSucceeded := false;

  // https://www.freepascal.org/docs-html/rtl/classes/tstringlist.html
  // https://wiki.freepascal.org/TStringList
  // https://wiki.freepascal.org/TStringList-TStrings_Tutorial
  context.DirList := TStringList.Create;
  context.FileList := TStringList.Create;

  context.SubDirCount := 0;
  context.DeletedSubDirCount := 0;
  context.MissingSubDirCount := 0;

  context.FileCount := 0;
  context.DeletedFileCount := 0;
  context.ErrorFileCount := 0;
  context.ReadOnlyFileCount := 0;
  context.SkippedFileCount := 0;
  context.SuccessfulFileCount := 0;

  result := context;
end;

procedure TSyncDirForm.ValidateSourceAndTargetDirectories(var options: TOptions);
begin
  options.AreValid := true;

  if (options.SourceDirectory = '') then begin
    options.AreValid := false;
    AppendLogMessage('Error: Source Directory is required');
  end else if (not DirectoryExists(options.SourceDirectory)) then begin
    options.AreValid := false;
    AppendLogMessage(Format('Error: Invalid Source Directory: %s', [options.SourceDirectory]));
  end;

  if (options.TargetDirectory = '') then begin
    options.AreValid := false;
    AppendLogMessage('Error: Target Directory is required');
  end else if (not DirectoryExists(options.TargetDirectory)) then begin
    options.AreValid := false;
    AppendLogMessage(Format('Error: Invalid Target Directory: %s', [options.TargetDirectory]));
  end;

  if (options.AreValid) then begin
    options.SourceDirectory := ExpandFileName(EnsureDirectorySeparator(options.SourceDirectory));
    options.TargetDirectory := ExpandFileName(EnsureDirectorySeparator(options.TargetDirectory));

    if (AnsiCompareText(options.SourceDirectory, options.TargetDirectory) = 0) Then begin
      options.AreValid := false;
      AppendLogMessage('Error: Source and Target Directories cannot be the same.');
      AppendLogMessage(Format('  Expanded Source Directory = %s', [options.SourceDirectory]));
      AppendLogMessage(Format('  Expanded Target Directory = %s', [options.TargetDirectory]));
    end;
  end;

  if (options.AreValid and options.IncludeSubdirectories) then begin
    if (Pos(AnsiLowerCase(options.SourceDirectory), AnsiLowerCase(options.TargetDirectory)) = 1) then begin
      options.AreValid := false;
      AppendLogMessage('Error: Target Directory cannot be a sub-directory of Source Directory when "Include subdirectories" is checked');
      AppendLogMessage(Format('  Expanded Source Directory = %s', [options.SourceDirectory]));
      AppendLogMessage(Format('  Expanded Target Directory = %s', [options.TargetDirectory]));
    end;

    if (Pos(AnsiLowerCase(options.TargetDirectory), AnsiLowerCase(options.SourceDirectory)) = 1) then begin
      options.AreValid := false;
      AppendLogMessage('Error: Source Directory cannot be a sub-directory of Target Directory when "Include subdirectories" is checked');
      AppendLogMessage(Format('  Expanded Source Directory = %s', [options.SourceDirectory]));
      AppendLogMessage(Format('  Expanded Target Directory = %s', [options.TargetDirectory]));
    end;
  end;
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
  context: TProgressContext;
begin
  ButtonSynchronize.Enabled := false;

  LoadInitialOptionsFromFormControls(gInitialOptions);
  if (gInitialOptions.AreValid) then begin
    ValidateSourceAndTargetDirectories(gInitialOptions);
  end;

  { TODO : Validate other option combinations. }

  if (not gInitialOptions.AreValid) then begin
    AppendLogMessage('Synchronization cancelled due to invalid options.');
  end else begin
    AppendLogMessage('Synchronization started ...');
    context := InitializeProgressContext();
    SynchronizeSourceToTarget(context, gInitialOptions);

    { TODO : If NextSection has value,
             and synchronizationSucceeded is true,
             iterate file synchronization thru successive section(s).
             (Set user-interface options on main form as each section is processed.) }
    // https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.sectionexists.html

    { TODO : Define procedure to load NextSection options into a new TOptions record for iterative synchronization call. }
  end;

  { TODO : Should we show log form while synchronizing, or only when done? }
  SyncDirLogForm.Show;

  ButtonSynchronize.Enabled := true;
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

  LoadInitializationFileSettings(initFileName, initSection, gInitialOptions);
  LoadFormControlsFromOptions(initSection, gInitialOptions);

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

END.

