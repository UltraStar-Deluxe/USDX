unit curlform;

{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}

interface


{$IFNDEF FPC}
{$IFDEF LINUX}
uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms,
  QDialogs, QStdCtrls, QComCtrls, QExtCtrls, curlobj;
{$ELSE}
uses
  SysUtils, Classes, Variants, Types, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, curlobj;
{$ENDIF}
{$ELSE}
uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, lazcurl, ComCtrls;
{$ENDIF}


type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    Panel2: TPanel;
    Label2: TLabel;
    ProgressBar1: TProgressBar;

    procedure Form1Create(Sender: TObject);

    procedure Button1Click(Sender: TObject);

    procedure Curl1Debug(Sender: TObject; infotype: curl_infotype;
      data: PChar; len: Cardinal; var bContinue: Boolean);

    procedure Curl1Progress(Sender: TObject; BytesTotal, BytesNow: integer;
      var bContinue: Boolean);

    procedure Curl1Receive(Sender: TObject; data: PChar; len: Cardinal;
      var bContinue: Boolean);

    procedure Curl1Wait(Sender: TObject);

  private
    { private declarations }
    Curl1:TCurl;
    Aborted:Boolean;
    IsText:Boolean;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFNDEF FPC}
  {$IFDEF LINUX}
    {$R *.xfm}
  {$ELSE}
    {$R *.dfm}
  {$ENDIF}
{$ENDIF}


procedure TForm1.Button1Click(Sender: TObject);
var
  SaveDlg:TSaveDialog;
  DownloadStream:TMemoryStream;
  Msg:string;
begin
  if ( Button1.Caption = 'GO' ) then begin
    Button1.Caption:='Stop';
    DownloadStream:=TMemoryStream.Create;
    Curl1.OutputStream:=DownloadStream;
    Curl1.URL:=edit1.text;
    Curl1.Threaded:=CheckBox1.Checked;
    IsText:=True;
    Aborted:=False;
    if Curl1.Perform then begin // Success...
      Label2.Caption:=Format('%d bytes received.', [curl1.SizeDownload]);
      DownloadStream.Position:=0;
      if ( not IsText ) then begin // It's binary data...
        if MessageDlg( 'Save binary download to file?',
                       mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
          SaveDlg:=TSaveDialog.Create(Application);
          SaveDlg.Filename:=ExtractFilename(curl1.EffectiveURL);
          if SaveDlg.Execute then DownloadStream.SaveToFile(SaveDlg.Filename);
          SaveDlg.Free;
        end;
      end else begin // It's plain text...
       // FixMe? this fails on Win9x for files > 32kb.
       Memo1.Lines.LoadFromStream(DownloadStream);
      end;
    end else begin // Failure...
      if ( Aborted ) then Msg:='Transfer cancelled by user.'
      else Msg:=curl1.ErrorString;
      ShowMessage(Msg);
      Label2.Caption:=Format( '%s [%d]', [ Msg, LongInt(curl1.ResultCode) ] );
    end;
    DownloadStream.Free;
    ProgressBar1.Position:=0;
    Button1.Enabled:=True;
    Button1.Caption:='GO';
  end else begin
    Aborted:=True;
    Button1.Enabled:=False;
  end;
end;



procedure TForm1.Curl1Receive(Sender: TObject; data: PChar; len: LongWord;
  var bContinue: Boolean);

var
  i:LongWord;
const
  TextChars=[ #9..#13, #32..#126, #128..#255 ];
begin
  TMemoryStream(TCurl(Sender).OutputStream).Write(data^, len);
  if IsText then for i:=0 to len-1 do begin
    if not ( data[i] in  TextChars ) then begin
      IsText:=False;
      BREAK;
    end;
  end;
  bContinue := not Aborted;
end;



procedure TForm1.Curl1Progress(Sender: TObject; BytesTotal, BytesNow: integer;
  var bContinue: Boolean);
begin
  if ( BytesTotal > 0 ) then begin // Total size is known, show progress...
    ProgressBar1.Max:=BytesTotal;
    ProgressBar1.Position:=BytesNow;
    Label2.Caption:=Format('Receiving: %d / %d', [ BytesNow, BytesTotal ]);
  end else begin // Size unknown, just bounce the prog bar...
    ProgressBar1.Max:=100;
    if ( ProgressBar1.Position+10 < ProgressBar1.Max )
    then ProgressBar1.Position:=ProgressBar1.Position+10
    else ProgressBar1.Position:=0;
    Label2.Caption:='Receiving: ' + IntToStr(BytesNow);
  end;
  bContinue := not Aborted;
end;



procedure TForm1.Curl1Debug(Sender: TObject; infotype: curl_infotype;
  data: PChar; len: LongWord; var bContinue: Boolean);
begin
  if ( infotype = CURLINFO_TEXT )
  then Label2.Caption:=copy(string(data), 1, len);
  bContinue := not Aborted;
end;



procedure TForm1.Curl1Wait(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TForm1.Form1Create(Sender: TObject);
begin
  Curl1:=TCurl.Create(self);
  Curl1.ConnectTimeout:=15;
  Curl1.OnDebug    := {$IFDEF FPC}@{$ENDIF}Curl1Debug;
  Curl1.OnProgress := {$IFDEF FPC}@{$ENDIF}Curl1Progress;
  Curl1.OnReceive  := {$IFDEF FPC}@{$ENDIF}Curl1Receive;
  Curl1.OnWait     := {$IFDEF FPC}@{$ENDIF}Curl1Wait;
end;

// http://www.ietf.org/iesg/1rfc_index.txt
// http://www.ietf.org/rfc/rfc1000.txt
// http://www.google.com/search?q=pascal&num=100

initialization
{$IFDEF FPC} {$I curlform.lrs} {$ENDIF}
end.


