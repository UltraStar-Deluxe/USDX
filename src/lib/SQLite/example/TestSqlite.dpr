program TestSqlite;

uses
  Forms,
  uTestSqlite in 'uTestSqlite.pas' {Form1},
  SQLiteTable3 in 'SQLiteTable3.pas',
  SQLite3 in 'SQLite3.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
