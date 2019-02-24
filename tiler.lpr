program tiler;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, kmodes, extern, sysutils, LCLType, Controls;

{$R *.res}

type

{ TEvtHolder }

TEvtHolder = class
  procedure AppException(Sender : TObject; E : Exception);
end;

procedure TEvtHolder.AppException(Sender: TObject; E: Exception);
begin
  Screen.Cursor := crDefault;
  TApplication(Sender).MessageBox(PChar(Format('Exception %s was thwrown:' + sLineBreak + '%s' + sLineBreak, [e.ClassName, e.Message])), PChar(Application.Title), MB_ICONERROR);
end;

var
  EvtHolder: TEvtHolder;
begin
  EvtHolder := TEvtHolder.Create;
  try
    RequireDerivedFormResource:=True;
    Application.Initialize;
    Application.CreateForm(TMainForm, MainForm);
    Application.Title := MainForm.Caption;
    Application.OnException := @EvtHolder.AppException;
    Application.Run;
  finally
    EvtHolder.Free;
  end;
end.

