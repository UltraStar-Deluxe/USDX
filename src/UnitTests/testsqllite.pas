unit TestSQLLite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, SQLiteTable3, unix;

type

  TTest_SqlLite= class(TTestCase)
  private
    fSQLLite  : TSQLiteDatabase;
    fFileName : string;
  protected
    procedure SetUp; override;
    procedure TearDown; override; 
  published
    procedure Test_Random_TableExists;
    procedure Test_Delete_NonExistant_Table;
    procedure Test_TableExists_On_0Length_File;
  end; 

implementation

procedure TTest_SqlLite.Test_Random_TableExists;
begin
  deletefile( fFileName );
  fSQLLite  := TSQLiteDatabase.Create( fFileName );

  // Test if some random table exists
  check( not fSQLLite.TableExists( 'testTable'+floattostr(now()) ) , 'Randomly Named Table Should NOT Exists (In an empty database file)' );
end; 

procedure TTest_SqlLite.Test_Delete_NonExistant_Table;
var
  lSQL : String;
begin
  deletefile( fFileName );
  fSQLLite  := TSQLiteDatabase.Create( fFileName );
  try
    lSQL := 'DROP TABLE testtable';
    fSQLLite.execsql( lSQL );
  except
    exit;
  end;

  Fail('SQLLite did not except when trying to delete a non existant table' );
end;

procedure TTest_SqlLite.Test_TableExists_On_0Length_File;
var
  lSQL : String;
begin
  deletefile( fFileName );
  shell('cat /dev/null > '+fFileName);
  
  if not fileexists( fFileName ) then
    Fail('0 Length file was not created... oops' );
    
  fSQLLite  := TSQLiteDatabase.Create( fFileName );

  check( not fSQLLite.TableExists( 'testTable' ) , 'Randomly Named Table Should NOT Exists' );
end;


procedure TTest_SqlLite.SetUp; 
begin
  fFileName := 'test.db';
//  fSQLLite  := TSQLiteDatabase.Create( fFileName );
end; 


procedure TTest_SqlLite.TearDown; 
begin
  freeandnil( fSQLLite );
end; 

initialization

  RegisterTest(TTest_SqlLite); 
end.

