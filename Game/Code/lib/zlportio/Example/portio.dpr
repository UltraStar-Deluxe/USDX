{$A-,H+}
program portio;

uses
  Forms,
  mainform in 'mainform.pas' {Main};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
