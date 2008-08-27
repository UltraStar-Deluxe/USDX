unit uTestSqlite;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,SQLiteTable3, ExtCtrls, jpeg;

type
  TForm1 = class(TForm)
    btnTest: TButton;
    memNotes: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    ebName: TEdit;
    Label3: TLabel;
    ebNumber: TEdit;
    Label4: TLabel;
    ebID: TEdit;
    Image1: TImage;
    btnLoadImage: TButton;
    btnDisplayImage: TButton;
    procedure btnTestClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure btnDisplayImageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnTestClick(Sender: TObject);
var
slDBpath: string;
sldb: TSQLiteDatabase;
sltb: TSQLIteTable;
sSQL: String;
Notes: String;

begin

slDBPath := ExtractFilepath(application.exename)
+ 'test.db';

sldb := TSQLiteDatabase.Create(slDBPath);
try

if sldb.TableExists('testTable') then begin
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

ebName.Text := sltb.FieldAsString(sltb.FieldIndex['Name']);
ebID.Text := inttostr(sltb.FieldAsInteger(sltb.FieldIndex['ID']));
ebNumber.Text := floattostr( sltb.FieldAsDouble(sltb.FieldIndex['Number']));
Notes :=  sltb.FieldAsBlobText(sltb.FieldIndex['Notes']);
memNotes.Text := notes;

end;

finally
sltb.Free;
end;

finally
sldb.Free;

end;

end;

procedure TForm1.btnLoadImageClick(Sender: TObject);
var
slDBpath: string;
sldb: TSQLiteDatabase;
sltb: TSQLIteTable;
iID: integer;
fs: TFileStream;

begin

slDBPath := ExtractFilepath(application.exename)
+ 'test.db';

if not FileExists(slDBPath) then begin
MessageDLg('Test.db does not exist. Click Test Sqlite 3 to create it.',mtInformation,[mbOK],0);
exit;
end;

sldb := TSQLiteDatabase.Create(slDBPath);
try

//get an ID
//query the data
sltb := slDb.GetTable('SELECT ID FROM testtable');
try

if sltb.Count = 0 then begin
MessageDLg('There are no rows in the database. Click Test Sqlite 3 to insert a row.',mtInformation,[mbOK],0);
exit;
end;

iID := sltb.FieldAsInteger(sltb.FieldIndex['ID']);

finally
sltb.Free;
end;

//load an image
fs := TFileStream.Create(ExtractFileDir(application.ExeName) + '\sunset.jpg',fmOpenRead);
try

//insert the image into the db
sldb.UpdateBlob('UPDATE testtable set picture = ? WHERE ID = ' + inttostr(iID),fs);

finally
fs.Free;
end;

finally
sldb.Free;

end;

end;

procedure TForm1.btnDisplayImageClick(Sender: TObject);
var
slDBpath: string;
sldb: TSQLiteDatabase;
sltb: TSQLIteTable;
iID: integer;
ms: TMemoryStream;
pic: TJPegImage;

begin

slDBPath := ExtractFilepath(application.exename)
+ 'test.db';

if not FileExists(slDBPath) then begin
MessageDLg('Test.db does not exist. Click Test Sqlite 3 to create it, then Load image to load an image.',mtInformation,[mbOK],0);
exit;
end;

sldb := TSQLiteDatabase.Create(slDBPath);
try

//get an ID
//query the data
sltb := slDb.GetTable('SELECT ID FROM testtable');
try

if not sltb.Count = 0 then begin
MessageDLg('No rows in the test database. Click Test Sqlite 3 to insert a row, then Load image to load an image.',mtInformation,[mbOK],0);
exit;
end;

iID := sltb.FieldAsInteger(sltb.FieldIndex['ID']);

finally
sltb.Free;
end;

sltb := sldb.GetTable('SELECT picture FROM testtable where ID = ' + inttostr(iID));
try

ms := sltb.FieldAsBlob(sltb.FieldIndex['picture']);
//note that the memory stream is freed when the TSqliteTable is destroyed.

if (ms = nil) then begin
MessageDLg('No image in the test database. Click Load image to load an image.',mtInformation,[mbOK],0);
exit;
end;

ms.Position := 0;

pic := TJPEGImage.Create;
pic.LoadFromStream(ms);

self.Image1.Picture.Graphic := pic;

pic.Free;

finally
sltb.Free;
end;

finally
sldb.Free;

end;


end;

end.
