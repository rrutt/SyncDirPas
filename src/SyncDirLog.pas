unit SyncDirLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Clipbrd;

type

  { TSyncDirLog }

  TSyncDirLog = class(TForm)
    ButtonClearLog: TButton;
    ButtonClose: TButton;
    ButtonCopyText: TButton;
    MemoLog: TMemo;
    procedure ButtonClearLogClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonCopyTextClick(Sender: TObject);
  private

  public

  end;

var
  SyncDirLogForm: TSyncDirLog;

implementation

{$R *.lfm}

{ TSyncDirLog }

procedure TSyncDirLog.ButtonCloseClick(Sender: TObject);
begin
  SyncDirLogForm.Close;
end;

procedure TSyncDirLog.ButtonClearLogClick(Sender: TObject);
begin
  MemoLog.Lines.Clear;
end;

procedure TSyncDirLog.ButtonCopyTextClick(Sender: TObject);
begin
  Clipboard.AsText := MemoLog.Text;
  Application.MessageBox('The log text has been copied to the clipboard.', 'SyncDirPas Log', 0);
end;

end.

