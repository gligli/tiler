program tiler;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, kmodes, sysutils, LCLType;

{$R *.res}

type

  { TEvtHolder }

  TEvtHolder = class
    procedure AppException(Sender : TObject; E : Exception);
  end;

{ TDummy }

procedure TEvtHolder.AppException(Sender: TObject; E: Exception);
begin
  TApplication(Sender).MessageBox(PChar(Format('Exception %s was thwrown:' + sLineBreak + '%s' + sLineBreak, [e.ClassName, e.Message])), PChar(Application.Title), MB_ICONERROR);
end;

var
  EvtHolder: TEvtHolder;
begin
  EvtHolder := TEvtHolder.Create;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Title := MainForm.Caption;
  Application.OnException := @EvtHolder.AppException;
  Application.Run;
end.

