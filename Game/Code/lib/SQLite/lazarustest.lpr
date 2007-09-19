program lazarustest;

uses
  SQLiteTable3 in 'SQLiteTable3.pas',
  SQLite3      in 'SQLite3.pas',
  sysutils;


procedure DoTest();
var
  slDBpath : string;
  sldb     : TSQLiteDatabase;
  sltb     : TSQLIteTable;
  sSQL     : String;
  Notes    : String;

begin
  // needed for linux build.

  slDBPath := ExtractFilepath( paramstr(0) ) + 'test.db';
  sldb     := TSQLiteDatabase.Create(slDBPath);
  
  try
  
    if sldb.TableExists('testTable') then
    begin
      sSQL := 'DROP TABLE testtable';
      sldb.execsql(sSQL);
    end;

    sSQL := 'CREATE TABLE testtable ([ID] INTEGER PRIMARY KEY,[OtherID] INTEGER NULL,';
    sSQL := sSQL + '[Name] VARCHAR (255),[Number] FLOAT, [notes] BLOB, [picture] BLOB COLLATE NOCASE);';
    sldb.execsql(sSQL);

    sldb.execsql('CREATE INDEX TestTableName ON [testtable]([Name]);');

    //begin a transaction
    sldb.BeginTransaction;

    sSQL := 'INSERT INTO testtable(Name,OtherID,Number,Notes) VALUES ("Some Name",4,587.6594,"Here are some notes");';
    //do the insert
    sldb.ExecSQL(sSQL);

    sSQL := 'INSERT INTO testtable(Name,OtherID,Number,Notes) VALUES ("Another Name",12,4758.3265,"More notes");';
    //do the insert
    sldb.ExecSQL(sSQL);

    //end the transaction
    sldb.Commit;

    //query the data
    sltb := slDb.GetTable('SELECT * FROM testtable');
    try

      if sltb.Count > 0 then
      begin
        //display first row
        writeln( sltb.FieldAsString(sltb.FieldIndex['Name']) );
        writeln( inttostr(sltb.FieldAsInteger(sltb.FieldIndex['ID'])) );
        writeln( floattostr( sltb.FieldAsDouble(sltb.FieldIndex['Number'])) );
      end;

    finally
      sltb.Free;
    end;

  finally
    sldb.Free;
  end;

end;

begin
  try
    DoTest();
    writeln( 'SqlLite3 unit IS lazarus compatible' );
  except
    writeln( 'ERROR : SqlLite3 unit is NOT lazarus compatible' );
  end;
end.

