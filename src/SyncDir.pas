unit SyncDir;

{$mode objfpc}{$H+}
{$WARN 5044 off : Symbol "$1" is not portable}
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
    IncludeSubdirectories: Boolean;
    ProcessHiddenFiles: Boolean;
  end;

  { TODO : Add fields to record structure to accumulate synchronization counts:
           files & directories copied, skipped, errors, etc. }
  TProgressContext = record
    SynchronizationSucceeded: Boolean;
    DirCount: LongInt;
    FileCount: LongInt;
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
    function LoadInitialOptionsFromFormControls: TOptions;
    procedure ValidateSourceAndTargetDirectories(var options: TOptions);
    procedure LoadInitializationFileSettings(iniFileFullPath: String; initSection: String; var options: TOptions);
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
  gProgressContext: TProgressContext;

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

function SynchronizeSourceFilesToTargetDirectory(fileList: TStringList; var options: TOptions): Boolean;
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
  while (fileIndex < fileList.Count) do begin
    sourceFileFullPath := EnsureDirectorySeparator(options.SourceDirectory) + fileList.Strings[fileIndex];
    targetFileFullPath := EnsureDirectorySeparator(options.TargetDirectory) + fileList.Strings[fileIndex];

    { TODO : Check file timestamps before copying. }
    sourceFileAge := FileAge(sourceFileFullPath);
    targetFileAge := FileAge(targetFileFullPath);
    sourceFileDate := FileDateToDateTime(sourceFileAge);
    targetFileDate := FileDateToDateTime(targetFileAge);
    AppendLogMessage(
        Format('Source file age = %d = %s for [%s]',
            [sourceFileAge, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', sourceFileDate), sourceFileFullPath]));
    AppendLogMessage(
        Format('Target file age = %d = %s for [%s]',
            [targetFileAge, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', targetFileDate), targetFileFullPath]));

    { TODO : Check CopyOlderFiles option. }
    if (sourceFileAge > targetFileAge) then begin
      { TODO : Check SkipReadOnlyTargetFiles option. }
      // https://www.freepascal.org/docs-html/rtl/sysutils/filegetattr.html
      copySuccessful := CopyFile(sourceFileFullPath, targetFileFullPath, [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]);
      if (copySuccessful) then begin
        AppendLogMessage(Format('Synchronized [%s] into [%s]', [sourceFileFullPath, targetFileFullPath]));
      end else begin
        isSuccessful := false;
        { TODO : Determine impact of ShowErrorMessages option. }
        AppendLogMessage(Format('ERROR: Could not synchronize [%s] into [%s]', [sourceFileFullPath, targetFileFullPath]));
      end;
    end else begin
      AppendLogMessage(Format('Skipped copying [%s] to [%s] based on file timestamps.', [sourceFileFullPath, targetFileFullPath]));
    end;

    inc(fileIndex);
  end;

  result := isSuccessful;
end;

function SynchronizeSourceToTarget(var options: TOptions): Boolean;
var
  isSuccessful: Boolean;
  searchInfo: TSearchRec;
  searchAttr: LongInt;
  dirList: TStringList;
  fileList: TStringList;
  filePrefix: String;
begin
  AppendLogMessage(Format('Synchronizing [%s] into [%s] ...', [options.SourceDirectory, options.TargetDirectory]));

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
  if (not options.ProcessHiddenFiles) then begin
    searchAttr := searchAttr and (not faHidden);
  end;
  If FindFirst(EnsureDirectorySeparator(options.SourceDirectory) + '*', searchAttr, searchInfo) = 0 then
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
            { TODO : If SkipMissingDirectories is true,
                     check TargetDir for pre-existence of a matching directory. }
            AppendLogMessage(Format('%sDirectory: %s  Size: %d', [filePrefix, Name, Size]));
            dirList.Add(Name);
          end;
        end else begin
          { TODO : Filter file list based on IgnoreFileTypes, OnlyProcessFileTypes, and SkipReadOnlyTargetFiles options. }
          AppendLogMessage(Format('%sFile: %s  Size: %d', [filePrefix, Name, Size]));
          fileList.Add(Name);
        end;
      end;
    until FindNext(searchInfo) <> 0;
    FindClose(searchInfo);
  end;

  gProgressContext.DirCount := dirList.Count;
  gProgressContext.FileCount := fileList.Count;
  AppendLogMessage(Format('Finished search. Found %d directories and %d files.', [gProgressContext.DirCount, gProgressContext.FileCount]));

  isSuccessful := SynchronizeSourceFilesToTargetDirectory(fileList, options);

  { TODO : Honor SynchronizeBothWays option. }

  { TODO : Honor DeleteExtraFiles and DeleteExtraDirectories options. }
  // https://www.freepascal.org/docs-html/rtl/sysutils/deletefile.html
  // https://www.freepascal.org/docs-html/rtl/sysutils/removedir.html

  { TODO : Perform sub-directory synchronization, if option set. }
  { TODO : Honor SkipMissingDirectories option. }

  dirList.Free;
  fileList.Free;

  if (isSuccessful) then begin
    AppendLogMessage(Format('Synchronization of [%s] into [%s] completed.', [options.SourceDirectory, options.TargetDirectory]));
  end else begin
    AppendLogMessage(Format('Synchronization of [%s] into [%s] failed!', [options.SourceDirectory, options.TargetDirectory]));
  end;

  result := isSuccessful;
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

    options.IncludeSubdirectories := LoadInitializationFileSettingBoolean(iniFile, initSection, 'IncludeSubdirectories', false);
    options.ProcessHiddenFiles := LoadInitializationFileSettingBoolean(iniFile, initSection, 'TargetDirectory', false);
  finally
    // After the INI file was used it must be freed to prevent memory leaks.
    iniFile.Free;
  end;
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

function TSyncDirForm.LoadInitialOptionsFromFormControls: TOptions;
var
  options: TOptions;
begin
  options.AreValid := true;

  options.SourceDirectory := Trim(DirectoryEditSource.Text);
  options.TargetDirectory := Trim(DirectoryEditTarget.Text);

  options.IncludeSubdirectories := CheckBoxIncludeSubdirectories.Checked;
  options.ProcessHiddenFiles := CheckBoxProcessHiddenFiles.Checked;

  result := options;
end;

procedure TSyncDirForm.ButtonSynchronizeClick(Sender: TObject);
begin
  ButtonSynchronize.Enabled := false;

  gInitialOptions := LoadInitialOptionsFromFormControls();
  if (gInitialOptions.AreValid) then begin
    ValidateSourceAndTargetDirectories(gInitialOptions);
  end;

  { TODO : Validate other option combinations. }

  if (not gInitialOptions.AreValid) then begin
    AppendLogMessage('Synchronization cancelled due to invalid options.');
  end else begin
    AppendLogMessage('Synchronization started ...');
    gProgressContext.SynchronizationSucceeded := SynchronizeSourceToTarget(gInitialOptions);

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

  { TODO : Isolate loading of form controls from INI file section into a procedure to support NextSection iteration. }
  LabelInitializationSectionValue.Caption := '[' + initSection + ']';

  LoadInitializationFileSettings(initFileName, initSection, gInitialOptions);
  DirectoryEditSource.Text := gInitialOptions.SourceDirectory;
  DirectoryEditTarget.Text := gInitialOptions.TargetDirectory;

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

END.

