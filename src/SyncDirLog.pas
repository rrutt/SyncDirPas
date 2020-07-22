unit SyncDirLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TSyncDirLog }

  TSyncDirLog = class(TForm)
    ButtonClose: TButton;
    ButtonCopyText: TButton;
    MemoLog: TMemo;
  private

  public

  end;

var
  SyncDirLog: TSyncDirLog;

implementation

{$R *.lfm}

end.

