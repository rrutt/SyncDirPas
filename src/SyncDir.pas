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
    NotifyUser: Boolean;
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
    procedure FinalizeProgressContext(var context: TProgressContext);
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

procedure AppendVerboseLogMessage(message: String);
begin
  if (not gInitialOptions.MinimizeLogMessages) then begin
    SyncDirLogForm.MemoLog.Lines.Add(Format('{%s}', [message]));
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
    options.NotifyUser := LoadInitializationFileSettingBoolean(iniFile, initSection, 'NotifyUser', true);
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
  context.ExtraDirCount := 0;
  context.ExtraFileCount := 0;
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

  // This count accumulates during the initial file scan, so cannot be cleared prior to when the actual synchronization occurs.
  context.ReadOnlyFileCount := 0;

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
    searchAttr := searchAttr and (not faHidden{%H-});
  end;
  if (FindFirst(EnsureDirectorySeparator(sourceDirectory) + '*', searchAttr, searchInfo) = 0) then
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

    RecursivelyScanSourceDirectoriesAndFiles(context, options, sourceSubdirFullPath, targetSubdirFullPath);

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
      AppendLogMessage(Format('Skipped copying [%s] to [%s] based on file timestamps.', [sourceFileFullPath, targetFileFullPath]));
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
begin
  { TODO : Honor DeleteExtraFiles option. }
  // https://www.freepascal.org/docs-html/rtl/sysutils/deletefile.html
  isSuccessful := true;

  result := isSuccessful;
end;

function DeleteExtraTargetDirectories(var context: TProgressContext; var options: TOptions): Boolean;
var
  isSuccessful: Boolean;
begin
  { TODO : Honor DeleteExtraDirectories option. }
  // https://www.freepascal.org/docs-html/rtl/sysutils/removedir.html
  isSuccessful := true;

  // Iterate directory list in reverse order so deeper subdirectories are deleted first.

  result := isSuccessful;
end;

procedure SynchronizeSourceToTarget(var context: TProgressContext; var options: TOptions);
var
  isSuccessful: Boolean;
  actuallySynchronize: Boolean;
  message: String;
begin
  AppendLogMessage(Format('Synchronizing [%s] into [%s] ...', [options.SourceDirectory, options.TargetDirectory]));

  RecursivelyScanSourceDirectoriesAndFiles(context, options, options.SourceDirectory, options.TargetDirectory);
  context.SourceFileCount := context.SourceFileList.Count;
  AppendLogMessage(Format('Finished search. Found %d directories and %d files.', [context.SourceDirCount, context.SourceFileCount]));

  { TODO : Honor SynchronizeBothWays option. }

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

    AppendLogMessage(Format('  Successfully copied %d file(s).', [context.SuccessfulFileCount]));
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
      AppendLogMessage(Format('  Deleted %d extra target file(s).', [context.DeletedFileCount]));
    end;
    if (context.DeletedFileErrorCount > 0) then begin
      AppendLogMessage(Format('  Encountered error deleting %d extra target file(s).', [context.DeletedFileErrorCount]));
    end;
    if (context.DeletedDirCount > 0) then begin
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

  context.SynchronizationSucceeded := isSuccessful;
end;

procedure TSyncDirForm.ValidateSourceAndTargetDirectories(var options: TOptions);
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
    context := InitializeProgressContext(gInitialOptions);
    SynchronizeSourceToTarget(context, gInitialOptions);
    FinalizeProgressContext(context);

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

