program testsave; 

(* Program to test downloading to disk *)

{$INCLUDE curltest.inc}

uses curlobj;

var 
  MyCurl:tCurl;

const 
  TMP_FILE='testget.tmp';
  HDR_FILE='headers.txt';
  ERR_FILE='errors.txt';

function Progress( ptr:pointer; dltotal, dlnow, ultotal, ulnow:double ): longint; cdecl;
begin
  Write('Received: ',  dlnow:16:0, #32, dltotal:16:0, #13);
  Result:=0;
end;


begin
  if ( ParamCount = 1 ) then begin
    MyCurl:=tCurl.Create(nil);
    with MyCurl do begin
      URL:=ParamStr(1);
      OutputFile:=TMP_FILE;
      HeaderFile:=HDR_FILE;
      ErrorFile:=ERR_FILE;
      NoProgress:=False;
      ProgressFunction:=@Progress;
      ProgressData:=nil;
      Verbose:=True;
      FollowLocation:=True;
      if not Perform then WriteLn(ErrorString);
      Free;
    end;
  end else begin { usage... }
    WriteLn;
    WriteLn('Download a file ...');
    WriteLn(' File will be saved to disk as "', TMP_FILE, '"');
    WriteLn(' Response headers are saved in "', HDR_FILE, '"');
    WriteLn(' Error messages are saved in "', ERR_FILE, '"');
    WriteLn;
    WriteLn('Usage: testsave <remote-url>');
    WriteLn;
  end;
end.
