unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtDlgs, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    BrowsePicture: TOpenDialog;
    OpenLeftPicture: TButton;
    Combine: TButton;
    OpenRightPicture: TButton;
    SavePictureDialog: TSavePictureDialog;
    procedure CombineClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenLeftPictureClick(Sender: TObject);
    procedure OpenRightPictureClick(Sender: TObject);
  private
    LeftPicture: TPicture;
    RightPicture: TPicture;
    IsLoaded: byte;
    CombinedFileName: string;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.OpenLeftPictureClick(Sender: TObject);
var
  Filename: string;
begin
  if BrowsePicture.Execute then
  begin
    if FileExists(BrowsePicture.FileName) then
    begin
      LeftPicture.LoadFromFile(BrowsePicture.FileName);
      IsLoaded := IsLoaded or $0F;
      Filename := ExtractFileName(BrowsePicture.FileName);
      Filename := Copy(Filename, 1, Filename.LastIndexOf('.'));
      CombinedFileName := Filename + '_' + CombinedFileName;
    end;
  end;

  if IsLoaded = $FF then
    Combine.Enabled := True;

end;

procedure TForm1.OpenRightPictureClick(Sender: TObject);
var
  Filename: string;
begin
  if BrowsePicture.Execute then
  begin
    if FileExists(BrowsePicture.FileName) then
    begin
      RightPicture.LoadFromFile(BrowsePicture.FileName);
      IsLoaded := IsLoaded or $F0;
      Filename := ExtractFileName(BrowsePicture.FileName);
      Filename := Copy(Filename, 1, Filename.LastIndexOf('.'));
      CombinedFileName := CombinedFileName + Filename;
    end;

    if IsLoaded = $FF then
      Combine.Enabled := True;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LeftPicture := TPicture.Create;
  RightPicture := TPicture.Create;
  IsLoaded := 0;
  CombinedFileName := '';
end;

procedure TForm1.CombineClick(Sender: TObject);
var
  CombinedResult: TBitmap;
  JPEG: TJPEGImage;
begin
  CombinedResult := TBitmap.Create;
  JPEG := TJPEGImage.Create;
  try
    CombinedResult.Width := LeftPicture.Width + RightPicture.Width;
    CombinedResult.Height := Math.Max(LeftPicture.Height, RightPicture.Height);
    CombinedResult.Canvas.Draw(0, 0, LeftPicture.Graphic);
    CombinedResult.Canvas.Draw(LeftPicture.Width, 0, RightPicture.Graphic);

    JPEG.Assign(CombinedResult);
    JPEG.CompressionQuality := 80;

    SavePictureDialog.FileName := CombinedFileName;

    if SavePictureDialog.Execute then
    begin
      JPEG.SaveToFile(SavePictureDialog.FileName);
      IsLoaded := 0;
      Combine.Enabled := False;
      CombinedFileName := '';
    end;

  finally
    IsLoaded := 0;
    Combine.Enabled := False;
    CombinedFileName := '';
    CombinedResult.Free;
    JPEG.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  LeftPicture.Free;
  RightPicture.Free;
end;

end.
