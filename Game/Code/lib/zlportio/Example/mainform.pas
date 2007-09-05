{$A-,H+}
unit mainform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,zlportio, ComCtrls, shellapi,rdtsc;

type
  TMain = class(TForm)
    lb1: TLabel;
    ePort: TEdit;
    lb2: TLabel;
    eData: TEdit;
    eRData: TEdit;
    btnExit: TButton;
    lb3: TLabel;
    coDataType: TComboBox;
    Lb4: TLabel;
    gb1: TGroupBox;
    sbBar: TStatusBar;
    lb5: TLabel;
    btnWrite: TButton;
    btnRead: TButton;
    cbDirect: TCheckBox;
    llbWWW: TLabel;
    procedure btnExitClick(Sender: TObject);
    procedure ePortKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ePortKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure coDataTypeChange(Sender: TObject);
    procedure ePortKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnWriteClick(Sender: TObject);
    procedure eDataKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnReadClick(Sender: TObject);
    procedure cbDirectClick(Sender: TObject);
    procedure llbWWWClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
   procedure setrdata(const Data:dword);
   function str2int(const value:string;const HEX:boolean):integer;
  end;

var
  Main: TMain;
implementation

{$R *.DFM}
{$R-}

function TMain.str2int(const value:string;const HEX:boolean):integer;
begin
  if HEX then
     result := strtoint('$' + value)
    else
     result := strtoint(value);
end;


procedure TMain.btnExitClick(Sender: TObject);
begin
 close;
end;

procedure TMain.ePortKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
try
 Case key of
  38: begin
       key := 0;
      end;
  40: begin
       key := 0;
      end
 end;
except
end;
end;

procedure TMain.ePortKeyPress(Sender: TObject; var Key: Char);
begin
  // filter on hex
 if pos(key,#13#8'0123456789abcdefABCDEF')=0 then
   key := #0;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
 coDataType.itemindex :=0;
 coDataType.Tag := 2;
 if ZLIOStarted then
   sbBar.SimpleText := 'Driver successfully started !'
 else
   sbBar.SimpleText := 'Couldnt start driver. Something wrong !';
end;

procedure TMain.coDataTypeChange(Sender: TObject);
var s:string;
begin
 coDataType.Tag := 2 shl (coDataType.itemindex);
 erdata.MaxLength := coDataType.Tag;
 edata.MaxLength := coDataType.Tag;
 s := edata.Text;
 delete(s,1,length(s)-coDataType.Tag);
 edata.text := s;
end;

procedure TMain.ePortKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var Port,data:dword;
begin
try
      try
       Port := str2int(ePort.Text,true);
      except
       Port := 0;
      end;
 Case key of
  13: begin
        zlioportwrite(port,coDataType.itemindex,str2int(eData.Text, true));
      end;
  38: begin
       inc(Port);
       ePort.Text := inttohex(Port,3);
       key := 0;
      end;
  40: begin
       if port > 0 then
        dec(Port);
       ePort.Text := inttohex(Port,3);
       key := 0;
      end
 end;
 setthreadpriority(GetCurrentThread,THREAD_PRIORITY_TIME_CRITICAL);
 data := zlioportread(Port,coDataType.itemindex);
 setrdata(data);
except
end;

end;

procedure TMain.setrdata(const Data:dword);
begin
 erData.Text := inttohex(Data,coDataType.Tag);
end;

procedure TMain.btnWriteClick(Sender: TObject);
var i:word;
begin
 i := 13;
 ePortKeyDown( self,i,[])
end;



procedure TMain.eDataKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var data:dword;
begin
      try
       Data := str2int(eData.Text,true);
      except Data := 0; end;
 Case key of
  13: begin
        ePortKeyDown( self,key,[])
      end;
  38: begin
       inc(Data);
       eData.Text := inttohex(Data,coDataType.Tag);
       key := 0;
      end;
  40: begin
        dec(Data);
       eData.Text := inttohex(Data,coDataType.Tag);
       key := 0;
      end
 end;

end;

procedure TMain.btnReadClick(Sender: TObject);
var k:word;
begin
 k := 0;
 ePortKeyDown(self, k,[]);
end;

procedure TMain.cbDirectClick(Sender: TObject);
begin
zliosetiopm(cbDirect.Checked);
end;

procedure TMain.llbWWWClick(Sender: TObject);
begin
 shellexecute( 0,'open','http://www.specosoft.com/update/portio_15.htm',nil,nil,SW_SHOW);
end;

end.
