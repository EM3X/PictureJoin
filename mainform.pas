unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BrowsePicture: TOpenDialog;
    OpenLeftPicture: TButton;
    OpenRightPicture: TButton;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

end.

