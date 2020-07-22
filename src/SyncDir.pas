unit SyncDir;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls;

type

  { TSyncDirForm }

  TSyncDirForm = class(TForm)
    DirectoryEditTarget: TDirectoryEdit;
    DirectoryEditSource: TDirectoryEdit;
    LabelTargetFolder: TLabel;
    LabelSourceFolder: TLabel;
    procedure DirectoryEditSourceChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  SyncDirForm: TSyncDirForm;

implementation

{$R *.lfm}

{ TSyncDirForm }

procedure TSyncDirForm.DirectoryEditSourceChange(Sender: TObject);
begin

end;

procedure TSyncDirForm.FormCreate(Sender: TObject);
begin

end;

end.

