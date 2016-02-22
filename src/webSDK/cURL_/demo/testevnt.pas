program testevnt;

(* Program to test event procedures *)

{$IFDEF FPC}{$MODE DELPHI}{$ELSE}{$INCLUDE curltest.inc}{$ENDIF}


uses Classes, sysutils, curlobj;

(*
 Events cannot be standard procedures, they must be methods of an object.
 So we create a "dummy" class to hold the tCurl object...
*)
type tDummy = class(tObject)
public
  CurlOne:tCurl;
  procedure CurlOneProgress (Sender:tObject; BytesTotal, BytesNow:longint; var bContinue:Boolean);
  procedure CurlOneHeader (Sender:tObject; data:string; var bContinue:Boolean);
  procedure CurlOneReceive (Sender:tObject; data:pChar; len:LongWord; var bContinue:Boolean);
  procedure CurlOnePassword (Sender:tObject; const Prompt:string; var data:string; var bContinue:Boolean);
  procedure CurlOneDebug (Sender:tObject; infotype:curl_infotype; data:pChar; len:LongWord;  var bContinue:boolean);
  procedure CurlOneTransmit (Sender:tObject; data:pChar; var len:LongWord); 
  constructor Create;
  destructor Destroy; override;
end;

procedure tDummy.CurlOneProgress (Sender:tObject; BytesTotal, BytesNow:longint; var bContinue:Boolean);
begin
  WriteLn('Progress: ', BytesNow, ' of ', BytesTotal);
end;

procedure tDummy.CurlOneHeader (Sender:tObject; data:string; var bContinue:Boolean);
begin
  WriteLn('OnHeader: ', data);
end;

procedure tDummy.CurlOneReceive (Sender:tObject; data:pChar; len:LongWord; var bContinue:Boolean);
begin
  WriteLn('OnReceive: ', len, ' bytes');
  with ( Sender as tCurl ) do TFileStream(OutputStream).Write(pChar(data)[0], len);
end;

procedure tDummy.CurlOnePassword (Sender:tObject;  const Prompt:string; var data:string; var bContinue:Boolean);
begin
  WriteLn( 'OnPassword: ');
  Write(Prompt);
  ReadLn(data);
end;

procedure tDummy.CurlOneTransmit (Sender:tObject; data:pChar; var len:LongWord);
begin
  WriteLn('CurlOneTransmit');
end;

procedure tDummy.CurlOneDebug (Sender:tObject; infotype:curl_infotype; data:pChar; len:LongWord;  var bContinue:boolean);
begin
  case InfoType of 
    CURLINFO_TEXT : WriteLn('OnDebug (info): ', copy(data, 1, len));
    CURLINFO_HEADER_OUT: begin
      WriteLn('OnDebug: Sending headers...');
      Write(copy(data, 1, len)) ;
   end;
  end;
end;

constructor tDummy.Create;
begin
  inherited Create;
  CurlOne:=tCurl.Create(nil);
  CurlOne.OnProgress := CurlOneProgress;
  CurlOne.OnReceive  := CurlOneReceive;
  CurlOne.OnHeader   := CurlOneHeader;
  CurlOne.OnDebug    := CurlOneDebug;
  CurlOne.OnTransmit := CurlOneTransmit; // Not used in this example
end;

destructor tDummy.Destroy;
begin
  CurlOne.Free;
  inherited Destroy;
end;

var 
  MyDummy:tDummy;

const
  TEMP_FILE = 'testevnt.tmp';

begin
  MyDummy:=tDummy.Create;
  with MyDummy.CurlOne do begin
    if ( ParamCount > 0 ) then URL:=ParamStr(1) else URL:='http://www.aol.com';
    FollowLocation:=True;
    (* 
      It would be simpler to just say "OutputFile:=TEMP_FILE"
      but I am using a TFileStream here to demonstrate 
      how to stream the received data to a file....
    *)
    OutputStream:=TFileStream.Create(TEMP_FILE, fmOpenWrite or fmCreate);
    if Perform then begin 
      WriteLn('Perform: received ', SizeDownload, ' bytes.') ;
      TFileStream(OutputStream).Free;
      WriteLn('File saved as "', TEMP_FILE, '"');
    end else begin
      TFileStream(OutputStream).Free;
      DeleteFile(TEMP_FILE);
      WriteLn('Perform: ', ErrorString);
    end;
  end;
  MyDummy.Free;
end.
