unit SyncDirLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Clipbrd;

type

  { TSyncDirLog }

  TSyncDirLog = class(TForm)
    ButtonClose: TButton;
    ButtonCopyText: TButton;
    MemoLog: TMemo;
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

procedure TSyncDirLog.ButtonCopyTextClick(Sender: TObject);
begin
  { TODO : Copy log text to Windows clipboard. }
  Clipboard.AsText := MemoLog.Text;
end;

end.

