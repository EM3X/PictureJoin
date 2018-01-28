unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtDlgs, ExtCtrls, Menus, Math;

type

  { TForm1 }

  TForm1 = class(TForm)
    BrowsePicture: TOpenDialog;
    MainMenu1: TMainMenu;
    ResizeHeight: TMenuItem;
    OptionsMenu: TMenuItem;
    StatusLabel: TLabel;
    OpenLeftPicture: TButton;
    Combine: TButton;
    OpenRightPicture: TButton;
    SavePictureDialog: TSavePictureDialog;
    procedure CombineClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure OpenLeftPictureClick(Sender: TObject);
    procedure OpenLeftPictureExec(FilePath: string);
    procedure OpenRightPictureClick(Sender: TObject);
    procedure OpenRightPictureExec(FilePath: string);
  private
    LeftPicture: TPicture;
    RightPicture: TPicture;
    IsLoaded: byte;
    CombinedFileName: string;
    procedure ChangeStatus(msg: string);
    procedure CheckReady;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.OpenLeftPictureClick(Sender: TObject);
begin
  if BrowsePicture.Execute then
  begin
    if FileExists(BrowsePicture.FileName) then
    begin
      OpenLeftPictureExec(BrowsePicture.FileName);
    end;
  end;

  if IsLoaded = $FF then
    Combine.Enabled := True;

end;

procedure TForm1.OpenLeftPictureExec(FilePath: string);
var
  FileName: string;
begin
  ChangeStatus('Loading Left Picture');
  LeftPicture.LoadFromFile(FilePath);
  IsLoaded := IsLoaded or $0F;
  Filename := ExtractFileName(FilePath);
  Filename := Copy(Filename, 1, Filename.LastIndexOf('.'));
  CombinedFileName := Filename + '_' + CombinedFileName;
  ChangeStatus('');
  CheckReady;
end;

procedure TForm1.OpenRightPictureClick(Sender: TObject);
begin
  if BrowsePicture.Execute then
  begin
    if FileExists(BrowsePicture.FileName) then
    begin
      OpenRightPictureExec(BrowsePicture.FileName);
    end;
  end;
end;

procedure TForm1.OpenRightPictureExec(FilePath: string);
var
  Filename: string;
begin
  ChangeStatus('Loading Right Picture');
  RightPicture.LoadFromFile(FilePath);
  IsLoaded := IsLoaded or $F0;
  Filename := ExtractFileName(FilePath);
  Filename := Copy(Filename, 1, Filename.LastIndexOf('.'));
  CombinedFileName := CombinedFileName + Filename;
  ChangeStatus('');
  CheckReady;
end;

procedure TForm1.ChangeStatus(msg: string);
begin
  StatusLabel.Caption := msg;
  Application.ProcessMessages;
end;

procedure TForm1.CheckReady;
begin
  if IsLoaded = $FF then
  begin
    Combine.Enabled := True;
    ChangeStatus('Ready for Combining');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LeftPicture := TPicture.Create;
  RightPicture := TPicture.Create;
  IsLoaded := 0;
  CombinedFileName := '';
  StatusLabel.Caption := '';
end;

procedure TForm1.CombineClick(Sender: TObject);
var
  CombinedResult: TBitmap;
  JPEG: TJPEGImage;
  ResizeScale: double;
begin
  CombinedResult := TBitmap.Create;
  JPEG := TJPEGImage.Create;
  try
    try

      if ResizeHeight.Checked then
      begin

        if LeftPicture.Height < RightPicture.Height then
        begin
          ResizeScale := LeftPicture.Height / RightPicture.Height;
          RightPicture.Bitmap.Canvas.StretchDraw(
            Rect(0, 0, round(RightPicture.Width * ResizeScale),
            round(RightPicture.Height * ResizeScale)), RightPicture.Bitmap);
          RightPicture.Bitmap.SetSize(round(RightPicture.Width * ResizeScale),
            round(RightPicture.Height * ResizeScale));
        end
        else
        begin
          ResizeScale := RightPicture.Height / LeftPicture.Height;
          LeftPicture.Bitmap.Canvas.StretchDraw(
            Rect(0, 0, round(LeftPicture.Width * ResizeScale),
            round(LeftPicture.Height * ResizeScale)), LeftPicture.Bitmap);
          LeftPicture.Bitmap.SetSize(round(LeftPicture.Width * ResizeScale),
            round(LeftPicture.Height * ResizeScale));
        end;

      end;

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
      end;

    except
      IsLoaded := 0;
      Combine.Enabled := False;
      CombinedFileName := '';
      ChangeStatus('');
      CombinedResult.Free;
      JPEG.Free;
    end;

  finally
    IsLoaded := 0;
    Combine.Enabled := False;
    CombinedFileName := '';
    ChangeStatus('');
    CombinedResult.Free;
    JPEG.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  LeftPicture.Free;
  RightPicture.Free;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  FilesCnt: integer;
begin
  FilesCnt := Length(FileNames);
  if FilesCnt > 2 then
  begin
    ShowMessage('Max two files supported!');
    Exit;
  end;

  if FilesCnt = 2 then
  begin
    OpenLeftPictureExec(FileNames[0]);
    OpenRightPictureExec(FileNames[1]);
  end
  else
  begin
    if IsLoaded = $0F then
      OpenRightPictureExec(FileNames[0])
    else
      OpenLeftPictureExec(FileNames[0]);
  end;

end;

end.
