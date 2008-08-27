program Test_Libraries;

{$mode objfpc}{$H+}

uses
  Classes,
  consoletestrunner,
  TestSQLLite,
  SQLite3 in '../lib/SQLite/SQLite3.pas',

  SQLiteTable3 in '../lib/SQLite/SQLiteTable3.pas';

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
