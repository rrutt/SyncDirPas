unit SyncDir;

{ Copyright © 2021 Rick Rutt }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls,
  LCLIntf,
  fileutil,
  inifiles,
  Process,
  SyncDirLog;

type

  TOptions = record
    AreValid: Boolean;

    SourceDirectory: String;
    TargetDirectory: String;

    Automatic: Boolean;
    CopyOlderFiles: Boolean;
    DeleteExtraFiles: Boolean;
    DeleteExtraDirectories: Boolean;
    IncludeSubdirectories: Boolean;
    MinimizeLogMessages: Boolean;
    NotifyUser: Boolean;
    ProcessHiddenFiles: Boolean;
    ShowErrorMessages: Boolean;
    SkipMissingDirectories: Boolean;
    SkipReadOnlyTargetFiles: Boolean;
    SynchronizeBothWays: Boolean;

    OnlyProcessFileTypes: String;
    IgnoreFileTypes: String;

    RunCommand: String;
    RunDirectory: String;
    AlternateRunCommand: String;
    AlternateRunDirectory: String;

    CurrentSection: String;
    NextSection: String;
  end;

  TProgressContext = record
    SynchronizationSucceeded: Boolean;
    SourceFileList: TStringList;
    TargetFileList: TStringList;
    ExtraDirList: TStringList;
    ExtraFileList: TStringList;
    OnlyProcessFileTypeList: TStringList;
    IgnoreFileTypeList: TStringList;
    SourceDirCount: LongInt;
    SourceFileCount: LongInt;
    DeletedDirCount: LongInt;
    DeletedDirErrorCount: LongInt;
    DeletedFileCount: LongInt;
    DeletedFileErrorCount: LongInt;
    ErrorFileCount: LongInt;
    ReadOnlyFileCount: LongInt;
    SkippedFileCount: LongInt;
    SuccessfulFileCount: LongInt;
    ExtraDirCount: LongInt;
    ExtraFileCount: LongInt;
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
    function InitializeProgressContext(options: TOptions): TProgressContext;
    function PerformSynchronizationPass(var options: TOptions): Boolean;
    function LoadInitializationFileSettings(iniFileFullPath: String; iniSection: String; var options: TOptions): Boolean;
    procedure FinalizeProgressContext(var context: TProgressContext);
    procedure LoadInitialOptionsFromFormControls(var options: TOptions);
    procedure LoadFormControlsFromOptions(const options: TOptions);
    procedure ValidateOptions(var options: TOptions);
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
  gCurrentWorkingDirectory: String;
  gIniFileName: String;
  gLogFileName: String;
  gLogToFile: Boolean;
  gLogFileMessages: TStringList;

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
  if (gLogToFile) then begin
    gLogFileMessages.Add(message);
  end else begin
    SyncDirLogForm.MemoLog.Lines.Add(message);
  end;
end;

procedure AppendVerboseLogMessage(message: String);
begin
  if (not gInitialOptions.MinimizeLogMessages) then begin
    if (gLogToFile) then begin
      gLogFileMessages.Add(message);
    end else begin
      SyncDirLogForm.MemoLog.Lines.Add(Format('{%s}', [message]));
    end;
  end;
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

function TSyncDirForm.LoadInitializationFileSettings(iniFileFullPath: String; iniSection: String; var options: TOptions): Boolean;
var
  iniFile: TINIFile;
  iniSectionExists: Boolean;
begin
  // https://wiki.freepascal.org/Using_INI_Files
  // https://www.freepascal.org/docs-html/fcl/inifiles/tinifile-3.html
  // https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.sectionexists.html
  // https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.booltruestrings.html
  // https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.boolfalsestrings.html}

  iniFile := TINIFile.Create(iniFileFullPath);
  try
    iniSectionExists := iniFile.SectionExists(iniSection);

    // https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.booltruestrings.html
    // https://www.freepascal.org/docs-html/fcl/inifiles/tcustominifile.boolfalsestrings.html}
    iniFile.BoolTrueStrings := ['true', 't', 'yes', 'y', '1'];
    iniFile.BoolFalseStrings := ['false', 'f', 'no', 'n', '0'];

    options.SourceDirectory := LoadInitializationFileSettingString(iniFile, iniSection, 'SourceDirectory', '.');
    options.TargetDirectory := LoadInitializationFileSettingString(iniFile, iniSection, 'TargetDirectory', '.');

    options.Automatic := LoadInitializationFileSettingBoolean(iniFile, iniSection, 'Automatic', false);
    options.CopyOlderFiles := LoadInitializationFileSettingBoolean(iniFile, iniSection, 'CopyOlderFiles', false);
    options.DeleteExtraFiles := LoadInitializationFileSettingBoolean(iniFile, iniSection, 'DeleteExtraFiles', false);
    options.DeleteExtraDirectories := LoadInitializationFileSettingBoolean(iniFile, iniSection, 'DeleteExtraDirectories', false);
    options.IncludeSubdirectories := LoadInitializationFileSettingBoolean(iniFile, iniSection, 'IncludeSubdirectories', false);
    options.MinimizeLogMessages := LoadInitializationFileSettingBoolean(iniFile, iniSection, 'MinimizeLogMessages', true);
    options.NotifyUser := LoadInitializationFileSettingBoolean(iniFile, iniSection, 'NotifyUser', true);
    options.ProcessHiddenFiles := LoadInitializationFileSettingBoolean(iniFile, iniSection, 'ProcessHiddenFiles', false);
    options.ShowErrorMessages := LoadInitializationFileSettingBoolean(iniFile, iniSection, 'ShowErrorMessages', true);
    options.SkipMissingDirectories := LoadInitializationFileSettingBoolean(iniFile, iniSection, 'SkipMissingDirectories', false);
    options.SkipReadOnlyTargetFiles := LoadInitializationFileSettingBoolean(iniFile, iniSection, 'SkipReadOnlyTargetFiles', false);
    options.SynchronizeBothWays := LoadInitializationFileSettingBoolean(iniFile, iniSection, 'SynchronizeBothWays', false);

    options.OnlyProcessFileTypes := LoadInitializationFileSettingString(iniFile, iniSection, 'OnlyProcessFileTypes', '');
    options.IgnoreFileTypes := LoadInitializationFileSettingString(iniFile, iniSection, 'IgnoreFileTypes', '');

    options.RunCommand := LoadInitializationFileSettingString(iniFile, iniSection, 'RunCommand', '');
    options.RunDirectory := LoadInitializationFileSettingString(iniFile, iniSection, 'RunDirectory', gCurrentWorkingDirectory);
    options.AlternateRunCommand := LoadInitializationFileSettingString(iniFile, iniSection, 'AlternateRunCommand', options.RunCommand);
    options.AlternateRunDirectory := LoadInitializationFileSettingString(iniFile, iniSection, 'AlternateRunDirectory', options.RunDirectory);

    options.CurrentSection := iniSection;
    options.NextSection := LoadInitializationFileSettingString(iniFile, iniSection, 'NextSection', '');

  finally
    // After the INI file was used it must be freed to prevent memory leaks.
    iniFile.Free;
  end;

  result := iniSectionExists;
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

procedure TSyncDirForm.LoadFormControlsFromOptions(const options: TOptions);
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

  LabelInitializationSectionValue.Caption := options.CurrentSection;

  if (options.NextSection <> '') then begin
    LabelNextSectionValue.Caption := options.NextSection;
    LabelNextSection.Visible := true;
  end else begin
    LabelNextSectionValue.Caption := '';
    LabelNextSection.Visible := false;
  end;
end;

procedure ClearProgressContextCounts(var context: TProgressContext);
begin
  context.SourceDirCount := 0;
  context.SourceFileCount := 0;
  context.DeletedDirCount := 0;
  context.DeletedDirErrorCount := 0;
  context.DeletedFileCount := 0;
  context.DeletedFileErrorCount := 0;
  context.ErrorFileCount := 0;
  context.SkippedFileCount := 0;
  context.SuccessfulFileCount := 0;
end;

function LoadFileTypeList(var fileTypes: String): TStringList;
var
  fileTypeList: TStringList;
begin
  fileTypeList := TStringList.Create;
  fileTypeList.Delimiter := ',';
  fileTypeList.QuoteChar := '''';
  fileTypeList.DelimitedText := LowerCase(fileTypes);

  result := fileTypeList;
end;

function TSyncDirForm.InitializeProgressContext(options: TOptions): TProgressContext;
var
  context: TProgressContext;
begin
  context.SynchronizationSucceeded := false;

  // https://www.freepascal.org/docs-html/rtl/classes/tstringlist.html
  // https://wiki.freepascal.org/TStringList
  // https://wiki.freepascal.org/TStringList-TStrings_Tutorial
  context.SourceFileList := TStringList.Create;
  context.TargetFileList := TStringList.Create;

  context.ExtraDirList := TStringList.Create;
  context.ExtraFileList := TStringList.Create;

  context.OnlyProcessFileTypeList := LoadFileTypeList(options.OnlyProcessFileTypes);
  context.IgnoreFileTypeList := LoadFileTypeList(options.IgnoreFileTypes);

  ClearProgressContextCounts(context);

  // These counts accumulate during the initial file scan, so they cannot be cleared prior to when the actual synchronization occurs.
  context.ReadOnlyFileCount := 0;
  context.ExtraDirCount := 0;
  context.ExtraFileCount := 0;

  result := context;
end;

procedure TSyncDirForm.FinalizeProgressContext(var context: TProgressContext);
begin
  context.SourceFileList.Free;
  context.TargetFileList.Free;

  context.ExtraDirList.Free;
  context.ExtraFileList.Free;

  context.OnlyProcessFileTypeList.Free;
  context.IgnoreFileTypeList.Free;
end;

function FilterFileType(const fileName: String; const context: TProgressContext): Boolean;
var
  fileType: String;
  n: LongInt;
  isAccepted: Boolean;
begin
  isAccepted := true;

  fileType := LowerCase(ExtractFileExt(fileName));
  if (fileType = '') then begin
    fileType := '.';
  end else begin
    n := Length(fileType);
    if (n > 1) then begin
      // Not an empty file type, so remove the initial '.'
      fileType := RightStr(fileType, n - 1);
    end;
  end;

  if (context.OnlyProcessFileTypeList.Count > 0) then begin
    isAccepted := isAccepted and (context.OnlyProcessFileTypeList.IndexOf(fileType) >= 0);
  end;

  if (context.IgnoreFileTypeList.Count > 0) then begin
    isAccepted := isAccepted and (context.IgnoreFileTypeList.IndexOf(fileType) < 0);
  end;

  result := isAccepted;
end;

function FileIsReadOnly(const fileFullPath: String): Boolean;
var
  isReadOnly: Boolean;
  searchInfo: TSearchRec;
begin
  fileIsReadOnly := false;

  if (FindFirst(fileFullPath, faAnyFile, searchInfo) = 0) then begin
    isReadOnly := ((searchInfo.Attr and faReadOnly) = faReadOnly);
  end;
  FindClose(searchInfo);

  result := isReadOnly;
end;

procedure ScanSourceSubdirectories(
    var context: TProgressContext;
    var options: TOptions;
    const sourceDirectory: String;
    const targetDirectory: String;
    const subdirList: TStringList); forward;

procedure RecursivelyScanSourceDirectoriesAndFiles(
    var context: TProgressContext;
    var options: TOptions;
    const sourceDirectory: String;
    const targetDirectory: String);
var
  subdirList: TStringList;
  searchInfo: TSearchRec;
  searchAttr: LongInt;
  fileTypeIsAccepted: Boolean;
  filePrefix: String;
  sourceFileFullPath: String;
  targetFileFullPath: String;
  sourceSubdirFullPath: String;
  targetSubdirFullPath: String;
begin
  AppendVerboseLogMessage(Format('Scanning Source Directory [%s] ...', [sourceDirectory]));

  subdirList := TStringList.Create;

  // https://www.freepascal.org/docs-html/rtl/sysutils/findfirst.html
  // https://www.freepascal.org/docs-html/rtl/sysutils/findnext.html
  searchAttr := faAnyFile;
  if (not options.ProcessHiddenFiles) then begin
    searchAttr := searchAttr and (not faHidden{%H-} and not (faSysFile{%H-}));
  end;
  if (FindFirst(EnsureDirectorySeparator(sourceDirectory) + '*', searchAttr, searchInfo) = 0) then
    begin
    repeat
      with searchInfo do begin
        filePrefix := '';
        if ((Attr and faHidden{%H-}) = faHidden{%H-}) then begin
          filePrefix := filePrefix + 'Hidden ';
        end;
        if ((Attr and faSysFile{%H-}) = faSysFile{%H-}) then begin
          filePrefix := filePrefix + 'System ';
        end;
        if ((Attr and faReadOnly) = faReadOnly) then begin
          filePrefix := filePrefix + 'ReadOnly ';
        end;

        if (Attr and faDirectory) = faDirectory then begin
          if ((Name <> '.') and (Name <> '..')) then begin
            sourceSubdirFullPath := EnsureDirectorySeparator(sourceDirectory) + Name;
            targetSubdirFullPath := EnsureDirectorySeparator(targetDirectory) + Name;
            if (options.SkipMissingDirectories and (not DirectoryExists(targetSubdirFullPath))) then begin
              AppendLogMessage(
                Format(
                  'Skipped source %ssubdirectory [%s] since target subdirectory [%s] does not exist.',
                  [filePrefix, sourceSubdirFullPath, targetSubdirFullPath]));
            end else begin
              AppendVerboseLogMessage(Format('%sSource subirectory: [%s]', [filePrefix, Name]));
              subdirList.Add(Name);
            end;
          end;
        end else begin
          sourceFileFullPath := EnsureDirectorySeparator(sourceDirectory) + Name;
          targetFileFullPath := EnsureDirectorySeparator(targetDirectory) + Name;

          fileTypeIsAccepted := FilterFileType(Name, context);

          if (fileTypeIsAccepted) then begin
            if (options.SkipReadOnlyTargetFiles and FileIsReadOnly(targetFileFullPath)) then begin
              AppendVerboseLogMessage(
                  Format(
                      'Bypassing synchronization of %sfile [%s] to read-only file [%s].',
                      [filePrefix, sourceFileFullPath, targetFileFullPath]));
              Inc(context.ReadOnlyFileCount);
            end else begin
              context.SourceFileList.Add(sourceFileFullPath);
              context.TargetFileList.Add(targetFileFullPath);

              AppendVerboseLogMessage(Format('%sSource file: [%s]  Size: %d', [filePrefix, sourceFileFullPath, Size]));
            end;
          end else begin
            AppendVerboseLogMessage(Format('Bypassing %ssource file [%s] based on its file type.', [filePrefix, sourceFileFullPath]));
          end;
        end;
      end;
    until FindNext(searchInfo) <> 0;
    FindClose(searchInfo);
  end;

  context.SourceDirCount := context.SourceDirCount + subdirList.Count;

  if (options.IncludeSubdirectories) then begin
    ScanSourceSubdirectories(context, options, sourceDirectory, targetDirectory, subdirList);
  end;

  subdirList.Free;
end;

procedure ScanSourceSubdirectories(
    var context: TProgressContext;
    var options: TOptions;
    const sourceDirectory: String;
    const targetDirectory: String;
    const subdirList: TStringList);
var
  subdirIndex: LongInt;
  subdir: String;
  sourceSubdirFullPath: String;
  targetSubdirFullPath: String;
begin
  subDirIndex := 0;
  while (subDirIndex < subDirList.Count) do begin
    subdir := subDirList.Strings[subDirIndex];

    sourceSubdirFullPath := EnsureDirectorySeparator(SourceDirectory) + subdir;
    targetSubDirFullPath := EnsureDirectorySeparator(TargetDirectory) + subdir;

    RecursivelyScanSourceDirectoriesAndFiles(context, options, sourceSubdirFullPath, targetSubdirFullPath);

    inc(subDirIndex);
  end;
end;

procedure ScanTargetSubdirectories(
    var context: TProgressContext;
    var options: TOptions;
    const sourceDirectory: String;
    const targetDirectory: String;
    const subdirList: TStringList); forward;

procedure RecursivelyScanTargetDirectoriesAndFiles(
    var context: TProgressContext;
    var options: TOptions;
    const sourceDirectory: String;
    const targetDirectory: String);
var
  subdirList: TStringList;
  searchInfo: TSearchRec;
  searchAttr: LongInt;
  fileTypeIsAccepted: Boolean;
  filePrefix: String;
  sourceFileFullPath: String;
  targetFileFullPath: String;
  sourceSubdirFullPath: String;
  targetSubdirFullPath: String;
begin
  AppendVerboseLogMessage(Format('Scanning Target Directory [%s] ...', [targetDirectory]));

  subdirList := TStringList.Create;

  // https://www.freepascal.org/docs-html/rtl/sysutils/findfirst.html
  // https://www.freepascal.org/docs-html/rtl/sysutils/findnext.html
  searchAttr := faAnyFile;
  if (not options.ProcessHiddenFiles) then begin
    searchAttr := searchAttr and (not faHidden{%H-});
  end;
  if (FindFirst(EnsureDirectorySeparator(targetDirectory) + '*', searchAttr, searchInfo) = 0) then
    begin
    repeat
      with searchInfo do begin
        filePrefix := '';
        if ((Attr and faHidden{%H-}) = faHidden{%H-}) then begin
          filePrefix := filePrefix + 'Hidden ';
        end;
        if ((Attr and faReadOnly) = faReadOnly) then begin
          filePrefix := filePrefix + 'ReadOnly ';
        end;

        if (Attr and faDirectory) = faDirectory then begin
          if ((Name <> '.') and (Name <> '..')) then begin
            sourceSubdirFullPath := EnsureDirectorySeparator(sourceDirectory) + Name;
            targetSubdirFullPath := EnsureDirectorySeparator(targetDirectory) + Name;
            if (options.DeleteExtraDirectories and (not DirectoryExists(sourceSubdirFullPath))) then begin
              AppendLogMessage(
                Format(
                  'Will delete extra %starget subdirectory [%s] since source subdirectory [%s] does not exist.',
                  [filePrefix, targetSubdirFullPath, sourceSubdirFullPath]));
              context.ExtraDirList.Add(targetSubdirFullPath);
              subdirList.Add(Name);
            end else begin
              AppendVerboseLogMessage(Format('%sTarget subdirectory: [%s]', [filePrefix, Name]));
              subdirList.Add(Name);
            end;
          end;
        end else begin
          sourceFileFullPath := EnsureDirectorySeparator(sourceDirectory) + Name;
          targetFileFullPath := EnsureDirectorySeparator(targetDirectory) + Name;

          fileTypeIsAccepted := FilterFileType(Name, context);

          if (fileTypeIsAccepted) then begin
            if (options.DeleteExtraFiles and (not FileExists(sourceFileFullPath))) then begin
              if (options.SkipReadOnlyTargetFiles and FileIsReadOnly(targetFileFullPath)) then begin
                AppendVerboseLogMessage(
                    Format(
                        'Bypassing deletion of extra read-only target file [%s].',
                        [targetFileFullPath]));
              end else begin
                AppendLogMessage(
                  Format(
                    'Will delete extra %starget file [%s] since source file [%s] does not exist.',
                    [filePrefix, targetFileFullPath, sourceFileFullPath]));
                context.ExtraFileList.Add(targetFileFullPath);
              end;
            end else begin
              AppendVerboseLogMessage(Format('%sTarget file: [%s]', [filePrefix, Name]));
            end;
          end else begin
            AppendVerboseLogMessage(Format('Bypassing %starget file [%s] based on its file type.', [filePrefix, targetFileFullPath]));
          end;
        end;
      end;
    until FindNext(searchInfo) <> 0;
    FindClose(searchInfo);
  end;

  if (options.IncludeSubdirectories) then begin
    ScanTargetSubdirectories(context, options, sourceDirectory, targetDirectory, subdirList);
  end;

  subdirList.Free;
end;

procedure ScanTargetSubdirectories(
    var context: TProgressContext;
    var options: TOptions;
    const sourceDirectory: String;
    const targetDirectory: String;
    const subdirList: TStringList);
var
  subdirIndex: LongInt;
  subdir: String;
  sourceSubdirFullPath: String;
  targetSubdirFullPath: String;
begin
  subDirIndex := 0;
  while (subDirIndex < subDirList.Count) do begin
    subdir := subDirList.Strings[subDirIndex];

    sourceSubdirFullPath := EnsureDirectorySeparator(SourceDirectory) + subdir;
    targetSubDirFullPath := EnsureDirectorySeparator(TargetDirectory) + subdir;

    RecursivelyScanTargetDirectoriesAndFiles(context, options, sourceSubdirFullPath, targetSubdirFullPath);

    inc(subDirIndex);
  end;
end;

function SynchronizeSourceFilesToTargetDirectory(
    var context: TProgressContext;
    var options: TOptions;
    const actuallySynchronize: Boolean): Boolean;
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
  errorMessage: String;
begin
  isSuccessful := true;

  fileIndex := 0;
  while (isSuccessful and (fileIndex < context.SourceFileList.Count)) do begin
    sourceFileFullPath := context.SourceFileList.Strings[fileIndex];
    targetFileFullPath := context.TargetFileList.Strings[fileIndex];

    sourceFileAge := FileAge(sourceFileFullPath);
    if (sourceFileAge < 0) then begin
      AppendVerboseLogMessage(
          Format('Source file age = %d for [%s]',
              [sourceFileAge, sourceFileFullPath]));
    end else begin
      sourceFileDate := FileDateToDateTime(sourceFileAge);
      AppendVerboseLogMessage(
          Format('Source file age = %d = %s for [%s]',
              [sourceFileAge, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', sourceFileDate), sourceFileFullPath]));
    end;

    targetFileAge := FileAge(targetFileFullPath);
    if (targetFileAge < 0) then begin
      AppendVerboseLogMessage(
          Format('Target file age = %d for [%s]',
              [targetFileAge, targetFileFullPath]));
    end else begin
      targetFileDate := FileDateToDateTime(targetFileAge);
      AppendVerboseLogMessage(
        Format('Target file age = %d = %s for [%s]',
            [targetFileAge, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', targetFileDate), targetFileFullPath]));
    end;

    if ((options.CopyOlderFiles and (sourceFileAge <> targetFileAge)) or (sourceFileAge > targetFileAge)) then begin
      if (actuallySynchronize) then begin
        // https://wiki.freepascal.org/CopyFile
        copySuccessful := CopyFile(sourceFileFullPath, targetFileFullPath, [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]);
        if (copySuccessful) then begin
          Inc(context.SuccessfulFileCount);
          AppendLogMessage(Format('Synchronized [%s] into [%s].', [sourceFileFullPath, targetFileFullPath]));
        end else begin
          Inc(context.ErrorFileCount);
          errorMessage := Format('ERROR: Could not synchronize [%s] into [%s].', [sourceFileFullPath, targetFileFullPath]);
          AppendLogMessage(errorMessage);
          if (options.ShowErrorMessages) then begin
            isSuccessful :=
              (mrOK = MessageDlg('SyncDirPas: Synchronization Error', errorMessage, mtConfirmation, [mbOK, mbCancel], 0));
          end;
        end;
      end else begin
        AppendVerboseLogMessage(Format('Will synchronize [%s] into [%s].', [sourceFileFullPath, targetFileFullPath]));
        Inc(context.SuccessfulFileCount);
      end;
    end else begin
      Inc(context.SkippedFileCount);
      if (actuallySynchronize) then begin
        AppendVerboseLogMessage(Format('Skipped copying [%s] to [%s] based on file timestamps.', [sourceFileFullPath, targetFileFullPath]));
      end else begin
        AppendVerboseLogMessage(Format('Will skip copying [%s] to [%s] based on file timestamps.', [sourceFileFullPath, targetFileFullPath]));
      end;
    end;

    inc(fileIndex);
  end;

  result := isSuccessful;
end;

function PromptUserWhetherToActuallySynchronize(var context: TProgressContext; var options: TOptions): Boolean;
var
  actuallySynchronize: Boolean;
  subdirPhrase: String;
  readOnlyPhrase: String;
  skippedFilePhrase: String;
  extraFilePhrase: String;
  extraDirPhrase: String;
  promptMessage: String;
begin
  actuallySynchronize := false;

  if (options.IncludeSubdirectories) then begin
    subdirPhrase := ' and its subdirectories';
  end else begin
    subdirPhrase := '';
  end;

  if (context.ReadOnlyFileCount > 0) then begin
    readOnlyPhrase := Format(', and %d read-only target file(s) will be bypassed', [context.ReadOnlyFileCount]);
  end else begin
    readOnlyPhrase := '';
  end;

  if (context.SkippedFileCount > 0) then begin
    skippedFilePhrase := Format(', and %d file(s) will be skipped based on file timestamps', [context.SkippedFileCount]);
  end else begin
    skippedFilePhrase := '';
  end;

  if (context.ExtraFileCount > 0) then begin
    extraFilePhrase := Format(', and %d extra target file(s) will be deleted', [context.ExtraFileCount]);
  end else begin
    extraFilePhrase := '';
  end;

  if (context.ExtraDirCount > 0) then begin
    extraDirPhrase := Format(', and %d extra target directory(ies) will be deleted', [context.ExtraDirCount]);
  end else begin
    extraDirPhrase := '';
  end;

  promptMessage :=
    Format(
      'In order to synchronize Source Directory [%s]%s into Target Directory [%s], %d file(s) will be copied%s%s%s%s.',
      [options.SourceDirectory,
       subdirPhrase,
       options.TargetDirectory,
       context.SuccessfulFileCount,
       readOnlyPhrase,
       skippedFilePhrase,
       extraFilePhrase,
       extraDirPhrase]);

  AppendLogMessage(promptMessage);

  actuallySynchronize :=
     (mrYes = MessageDlg('SyncDirPas: Proceed with synchronization?', promptMessage, mtConfirmation, [mbYes, mbNo], 0));

  if (actuallySynchronize) then begin
    AppendLogMessage('User chose to proceed with synchronization.');
  end else begin
    AppendLogMessage('User chose to cancel synchronization.');
  end;

  result := actuallySynchronize;
end;

function DeleteExtraTargetFiles(var context: TProgressContext; var options: TOptions): Boolean;
var
  isSuccessful: Boolean;
  fileDeleted: Boolean;
  fileIndex: LongInt;
  targetFileFullPath: String;
  errorMessage: String;
begin
  isSuccessful := true;

  fileIndex := 0;
  while (isSuccessful and (fileIndex < context.ExtraFileList.Count)) do begin
    targetFileFullPath := context.ExtraFileList.Strings[fileIndex];
    AppendVerboseLogMessage(Format('Deleting extra target file [%s] ...', [targetFileFullPath]));

    // https://www.freepascal.org/docs-html/rtl/sysutils/deletefile.html
    fileDeleted := DeleteFile(targetFileFullPath);
    if (fileDeleted) then begin
      Inc(context.DeletedFileCount);
      AppendLogMessage(Format('Deleted extra target file [%s].', [targetFileFullPath]));
    end else begin
      Inc(context.DeletedFileErrorCount);
      errorMessage := Format('ERROR: Could not delete extra target file [%s].', [targetFileFullPath]);
      AppendLogMessage(errorMessage);
      if (options.ShowErrorMessages) then begin
        isSuccessful :=
          (mrOK = MessageDlg('SyncDirPas: Synchronization Error', errorMessage, mtConfirmation, [mbOK, mbCancel], 0));
      end;
    end;

    inc(fileIndex);
  end;

  result := isSuccessful;
end;

function DeleteExtraTargetDirectories(var context: TProgressContext; var options: TOptions): Boolean;
var
  isSuccessful: Boolean;
  dirRemoved: Boolean;
  dirIndex: LongInt;
  targetDirFullPath: String;
  errorMessage: String;
begin
  isSuccessful := true;

  // Iterate directory list in reverse order so deeper subdirectories are deleted first.
  dirIndex := context.ExtraDirList.Count;
  while (isSuccessful and (dirIndex > 0)) do begin
    Dec(dirIndex);

    targetDirFullPath := context.ExtraDirList.Strings[dirIndex];
    AppendVerboseLogMessage(Format('Deleting extra target directory [%s] ...', [targetDirFullPath]));

    // https://www.freepascal.org/docs-html/rtl/sysutils/removedir.html
    dirRemoved := RemoveDir(targetDirFullPath);
    if (dirRemoved) then begin
      Inc(context.DeletedDirCount);
      AppendLogMessage(Format('Deleted extra target directory [%s].', [targetDirFullPath]));
    end else begin
      Inc(context.DeletedDirErrorCount);
      errorMessage := Format('ERROR: Could not delete extra target directory [%s].', [targetDirFullPath]);
      AppendLogMessage(errorMessage);
      if (options.ShowErrorMessages) then begin
        isSuccessful :=
          (mrOK = MessageDlg('SyncDirPas: Synchronization Error', errorMessage, mtConfirmation, [mbOK, mbCancel], 0));
      end;
    end;
  end;

  result := isSuccessful;
end;

procedure LaunchExternalApplication(const command: String; const directory: String; notify: Boolean);
var
  proc: TProcess;
  message: String;
begin
  proc := TProcess.Create(nil);
  proc.CurrentDirectory := directory;
  proc.{%H-}CommandLine {%H-}:= command;
  proc.Execute;
  proc.Free;

  message := Format('Launched external program [%s] with working directory [%s].', [command, directory]);
  AppendLogMessage(message);
  if (notify) then begin
    Application.MessageBox(PChar(message), 'SyncDirPas', 0);
  end;
end;

procedure SynchronizeSourceToTarget(var context: TProgressContext; var options: TOptions);
var
  isSuccessful: Boolean;
  actuallySynchronize: Boolean;
  synchronizationActuallyOccurred: Boolean;
  message: String;
begin
  AppendLogMessage(Format('Synchronizing [%s] into [%s] ...', [options.SourceDirectory, options.TargetDirectory]));

  RecursivelyScanSourceDirectoriesAndFiles(context, options, options.SourceDirectory, options.TargetDirectory);
  context.SourceFileCount := context.SourceFileList.Count;
  AppendLogMessage(Format('Finished search. Found %d directories and %d files.', [context.SourceDirCount, context.SourceFileCount]));

  if (options.DeleteExtraDirectories or options.DeleteExtraFiles) then begin
    RecursivelyScanTargetDirectoriesAndFiles(context, options, options.SourceDirectory, options.TargetDirectory);
  end;
  context.ExtraDirCount := context.ExtraDirList.Count;
  context.ExtraFileCount := context.ExtraFileList.Count;

  if (options.NotifyUser) then begin
    actuallySynchronize := false;
    isSuccessful := SynchronizeSourceFilesToTargetDirectory(context, options, actuallySynchronize);
    actuallySynchronize := (isSuccessful and PromptUserWhetherToActuallySynchronize(context, options));
  end else begin
    actuallySynchronize := true;
  end;

  if (actuallySynchronize) then begin
    ClearProgressContextCounts(context);
    isSuccessful := SynchronizeSourceFilesToTargetDirectory(context, options, actuallySynchronize);

    // We need to delete extra files before extra directories to help create empty extra directories.
    if (isSuccessful and (context.ExtraFileCount > 0)) then begin
      isSuccessful := DeleteExtraTargetFiles(context, options);
    end;

    if (isSuccessful and (context.ExtraDirCount > 0)) then begin
      isSuccessful := DeleteExtraTargetDirectories(context, options);
    end;

    synchronizationActuallyOccurred := false;

    if (context.SuccessfulFileCount > 0) then begin
      synchronizationActuallyOccurred := true;
      AppendLogMessage(Format('  Successfully copied %d file(s).', [context.SuccessfulFileCount]));
    end;
    if (context.ReadOnlyFileCount > 0) then begin
      AppendLogMessage(Format('  Bypassed %d read-only target file(s).', [context.ReadOnlyFileCount]));
    end;
    if (context.SkippedFileCount > 0) then begin
      AppendLogMessage(Format('  Skipped copying %d file(s).', [context.SkippedFileCount]));
    end;
    if (context.ErrorFileCount > 0) then begin
      message := Format('  Encountered error copying %d file(s).', [context.ErrorFileCount]);
      AppendLogMessage(message);
      if (options.ShowErrorMessages) then begin
        Application.MessageBox(PChar(message), 'SyncDirPas Error', 0);
      end;
    end;
    if (context.DeletedFileCount > 0) then begin
      synchronizationActuallyOccurred := true;
      AppendLogMessage(Format('  Deleted %d extra target file(s).', [context.DeletedFileCount]));
    end;
    if (context.DeletedFileErrorCount > 0) then begin
      AppendLogMessage(Format('  Encountered error deleting %d extra target file(s).', [context.DeletedFileErrorCount]));
    end;
    if (context.DeletedDirCount > 0) then begin
      synchronizationActuallyOccurred := true;
      AppendLogMessage(Format('  Deleted %d extra target directory(ies).', [context.DeletedDirCount]));
    end;
    if (context.DeletedDirErrorCount > 0) then begin
      AppendLogMessage(Format('  Encountered error deleting %d extra target directory(ies).', [context.DeletedDirErrorCount]));
    end;
  end else begin
    isSuccessful := false;
  end;

  if (isSuccessful) then begin
    AppendLogMessage(Format('Synchronization of [%s] into [%s] completed.', [options.SourceDirectory, options.TargetDirectory]));
  end else begin
    AppendLogMessage(Format('Synchronization of [%s] into [%s] failed!', [options.SourceDirectory, options.TargetDirectory]));
  end;

  if (options.Automatic and actuallySynchronize) then begin
    if (synchronizationActuallyOccurred) then begin
      LaunchExternalApplication(options.AlternateRunCommand, options.AlternateRunDirectory, options.NotifyUser);
    end else begin
      LaunchExternalApplication(options.RunCommand, options.RunDirectory, options.NotifyUser);
    end;
  end;

  context.SynchronizationSucceeded := isSuccessful;
end;

procedure TSyncDirForm.ValidateOptions(var options: TOptions);
begin
  options.AreValid := true;

  if (options.SourceDirectory = '') then begin
    options.AreValid := false;
    AppendLogMessage('Error: Source Directory is required.');
  end else if (not DirectoryExists(options.SourceDirectory)) then begin
    options.AreValid := false;
    AppendLogMessage(Format('Error: Invalid Source Directory: [%s].', [options.SourceDirectory]));
  end;

  if (options.TargetDirectory = '') then begin
    options.AreValid := false;
    AppendLogMessage('Error: Target Directory is required');
  end else if (not DirectoryExists(options.TargetDirectory)) then begin
    options.AreValid := false;
    AppendLogMessage(Format('Error: Invalid Target Directory: [%s].', [options.TargetDirectory]));
  end;

  if (options.AreValid) then begin
    options.SourceDirectory := ExpandFileName(EnsureDirectorySeparator(options.SourceDirectory));
    options.TargetDirectory := ExpandFileName(EnsureDirectorySeparator(options.TargetDirectory));

    if (AnsiCompareText(options.SourceDirectory, options.TargetDirectory) = 0) Then begin
      options.AreValid := false;
      AppendLogMessage('Error: Source and Target Directories cannot be the same.');
      AppendLogMessage(Format('  Expanded Source Directory = [%s].', [options.SourceDirectory]));
      AppendLogMessage(Format('  Expanded Target Directory = [%s].', [options.TargetDirectory]));
    end;
  end;

  if (options.AreValid and options.IncludeSubdirectories) then begin
    if (Pos(AnsiLowerCase(options.SourceDirectory), AnsiLowerCase(options.TargetDirectory)) = 1) then begin
      options.AreValid := false;
      AppendLogMessage('Error: Target Directory cannot be a sub-directory of Source Directory when "Include subdirectories" is checked');
      AppendLogMessage(Format('  Expanded Source Directory = [%s].', [options.SourceDirectory]));
      AppendLogMessage(Format('  Expanded Target Directory = [%s].', [options.TargetDirectory]));
    end;

    if (Pos(AnsiLowerCase(options.TargetDirectory), AnsiLowerCase(options.SourceDirectory)) = 1) then begin
      options.AreValid := false;
      AppendLogMessage('Error: Source Directory cannot be a sub-directory of Target Directory when "Include subdirectories" is checked');
      AppendLogMessage(Format('  Expanded Source Directory = [%s].', [options.SourceDirectory]));
      AppendLogMessage(Format('  Expanded Target Directory = [%s].', [options.TargetDirectory]));
    end;
  end;

  if (options.SynchronizeBothWays and (options.DeleteExtraFiles or options.DeleteExtraDirectories)) then begin
    options.AreValid := false;
    AppendLogMessage('Error: When SynchronizeBothWays is enabled, both DeleteExtraFiles and DeleteExtraDirectories must be disabled.');
  end;

  if (options.DeleteExtraDirectories and (not options.IncludeSubdirectories)) then begin
    options.AreValid := false;
    AppendLogMessage('Error: When DeleteExtraDirectories is enabled, IncludeSubdirectories must be enabled.');
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
  helpFileURL := ExtractFilePath(Application.ExeName) + 'SyncDirPas.html';
  //ShowMessage('Help File URL = ' + helpFileURL);
  OpenURL(helpFileURL);
end;

procedure TSyncDirForm.ButtonShowLogClick(Sender: TObject);
var
  message: String;
begin
  if (gLogToFile) then begin
    gLogFileMessages.SaveToFile(gLogFileName);
    message := Format('%d log messages have been written to file [%s].', [gLogFileMessages.Count, gLogFileName]);
    Application.MessageBox(PChar(message), 'SyncDirPas Log', 0);
  end else begin
    SyncDirLogForm.Show;
  end;
end;

function TSyncDirForm.PerformSynchronizationPass(var options: TOptions): Boolean;
var
  context: TProgressContext;
  isSuccessful: Boolean;
  message: String;
begin
  AppendLogMessage(Format('Processing initialization section [%s] in file [%s].', [options.CurrentSection, gIniFileName]));

  if (options.AreValid) then begin
    ValidateOptions(options);
  end;

  if (not options.AreValid) then begin
    isSuccessful := false;;
    message := 'Synchronization cancelled due to invalid options.';
    AppendLogMessage(message);
    Application.MessageBox(PChar(message), 'SyncDirPas Log', 0);
  end else begin
    AppendLogMessage('Synchronization started ...');
    context := InitializeProgressContext(options);
    SynchronizeSourceToTarget(context, options);
    isSuccessful := context.SynchronizationSucceeded;
    FinalizeProgressContext(context);
  end;

  result := isSuccessful;
end;

procedure TSyncDirForm.ButtonSynchronizeClick(Sender: TObject);
var
  currentOptions: TOptions;
  synchronizationSucceeded: Boolean;
  iniSectionExists: Boolean;
  iniSection: String;
  swapDirectory: String;
  errorMessage: String;
begin
  ButtonSynchronize.Enabled := false;

  LoadInitialOptionsFromFormControls(gInitialOptions);
  currentOptions := gInitialOptions;

  iniSectionExists := true;
  while (iniSectionExists) do begin
    synchronizationSucceeded := PerformSynchronizationPass(currentOptions);

    if (synchronizationSucceeded and currentOptions.SynchronizeBothWays) then begin
      AppendLogMessage('SynchronizeBothWays is enabled. Swapping Source Directory and Target Directory.');
      swapDirectory := currentOptions.SourceDirectory;
      currentOptions.SourceDirectory := currentOptions.TargetDirectory;
      currentOptions.TargetDirectory := swapDirectory;

      synchronizationSucceeded := PerformSynchronizationPass(currentOptions);
    end;

    iniSection := currentOptions.NextSection;
    if (currentOptions.Automatic) then begin
      AppendLogMessage(Format('Ignoring NextSection=%s since Automatic option is enabled.', [iniSection]));
      iniSection := '';
    end;

    if (synchronizationSucceeded and (Length(iniSection) > 0)) then begin
      iniSectionExists := LoadInitializationFileSettings(gIniFileName, iniSection, currentOptions);
      if (iniSectionExists) then begin
        LoadFormControlsFromOptions(currentOptions);
      end else begin
        errorMessage := Format('ERROR: Initialization section [%s] does not exist in file [%s].', [iniSection, gIniFileName]);
        AppendLogMessage(errorMessage);
        if (currentOptions.ShowErrorMessages) then begin
          Application.MessageBox(PChar(errorMessage), 'SyncDirPas Error', 0);
        end;
      end;
    end else begin
      iniSectionExists := false;
    end;
  end;

  if (not gInitialOptions.Automatic) then begin
    ButtonShowLogClick(Sender);
  end;

  ButtonSynchronize.Enabled := true;
end;

procedure TSyncDirForm.CheckBoxProcessHiddenFilesChange(Sender: TObject);
begin
  DirectoryEditSource.ShowHidden := CheckBoxProcessHiddenFiles.Checked;
  DirectoryEditTarget.ShowHidden := CheckBoxProcessHiddenFiles.Checked;
end;

procedure TSyncDirForm.FormCreate(Sender: TObject);
var
  errorMessage: String;
  iniSection: String;
  iniSectionExists: Boolean;
  iniFileExists: Boolean;
begin
  LabelInitializationFileValue.Caption := gIniFileName;

  iniSection := paramStr(2);
  if (iniSection = '') then begin
    iniSection := 'SyncDir';
  end;

  iniFileExists := FileExists(gIniFileName);
  iniSectionExists := LoadInitializationFileSettings(gIniFileName, iniSection, gInitialOptions);
  if (not iniFileExists) then begin
    errorMessage := Format('ERROR: Initialization file [%s] does not exist.', [gIniFileName]);
    Application.MessageBox(PChar(errorMessage), 'SyncDirPas Error', 0);
  end else if (not iniSectionExists) then begin
    errorMessage := Format('ERROR: Initialization section [%s] does not exist in file [%s].', [iniSection, gIniFileName]);
    Application.MessageBox(PChar(errorMessage), 'SyncDirPas Error', 0);
  end;
  LoadFormControlsFromOptions(gInitialOptions);

  if (gInitialOptions.Automatic) then begin
    gLogToFile := true;
    ButtonSynchronizeClick(Sender);

    if (gInitialOptions.NotifyUser) then begin
      ButtonShowLogClick(Sender);
    end;
    Halt(1)
  end;
end;

initialization
begin
  gCurrentWorkingDirectory := GetCurrentDir;
  //ShowMessage('Current Working Directory = ' + currentWorkingDirectory);

  gLogFileName := gCurrentWorkingDirectory + DirectorySeparator + 'SyncDirPas.log';
  gLogToFile := false;
  gLogFileMessages := TStringList.Create;

  gIniFileName := paramStr(1);
  if (gIniFileName = '') then begin
    gIniFileName := 'SyncDir.ini';
  end;
  if (ExtractFilePath(gIniFileName) = '') then begin
    gIniFileName := gcurrentWorkingDirectory + DirectorySeparator  + gIniFileName;
  end;
end;

finalization
begin
  if (gLogToFile) then begin
    gLogFileMessages.SaveToFile(gLogFileName);
  end;

  gLogFileMessages.Free;
end;

END.

