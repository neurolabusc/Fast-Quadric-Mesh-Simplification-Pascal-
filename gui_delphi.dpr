program gui_delphi;

uses
  Forms,
  gui_main in 'gui_main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
