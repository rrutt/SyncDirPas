program SyncDirPas;

{ Copyright © 2021 Rick Rutt }

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SyncDir, SyncDirLog
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TSyncDirForm, SyncDirForm);
  Application.CreateForm(TSyncDirLog, SyncDirLogForm);
  Application.Run;
end.

