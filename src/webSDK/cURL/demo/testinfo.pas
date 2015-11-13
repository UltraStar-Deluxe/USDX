program testinfo; 

(* Program to test transfer information  functions *)

{$INCLUDE curltest.inc}

uses curlobj;

var 
  MyCurl:tCurl;

function ProgressCB(clientp:pointer; dltotal, dlnow, ultotal, ulnow:double ): LongInt; cdecl;
begin
  Write('.');
  Result:=0;
end;

begin
  MyCurl:=tCurl.Create(nil);
  with MyCurl do begin
    if ( ParamCount = 1 ) 
    then URL:=ParamStr(1) 
    else URL:='http://www.netscape.net';

    OutputFile:={$IFDEF WIN32}'NUL'{$ELSE}'/dev/null'{$ENDIF};
    ProgressFunction:=@ProgressCB;
    NoProgress:=False;
    RequestFileTime:=True;
    FollowLocation:=True;

    WriteLn('Requested URL:  ', URL);
    Write('Working...');

    if Perform then begin

      WriteLn(#13, 'Effective URL:  ', EffectiveUrl);
      WriteLn('Redirect count: ', RedirectCount);
      WriteLn('Http code:      ', HttpCode:3);
      WriteLn('Content-Type:   ', ContentType);

      WriteLn('Name lookup time:    ', NameLookupTime:7:2, ' sec');
      WriteLn('Connect time:        ', ConnectTime:7:2, ' sec');
      WriteLn('Pre-transfer time:   ', PreTransferTime:7:2, ' sec');
      WriteLn('Start transfer time: ', StartTransferTime:7:2, ' sec');
      WriteLn('Redirect time:       ', RedirectTime:7:2, ' sec');
      WriteLn('Total time:          ', TotalTime:7:2, ' sec');
      WriteLn('Download speed: ', SpeedDownload:12:2, ' bps');
      WriteLn('Upload speed:   ', SpeedUpload:12:2, ' bps');

      WriteLn('Request  header size:   ', RequestSize:10, ' bytes');
      WriteLn('Response header size:   ', HeaderSize:10, ' bytes');
      WriteLn('Actual download size:   ', SizeDownload:10, ' bytes');
      WriteLn('Content-Length header:  ', ContentLengthDownload:10, ' bytes');
      WriteLn('Actual upload Size:     ', SizeUpload:10, ' bytes');
      WriteLn('Content-Length header:  ', ContentLengthUpload:10, ' bytes');

      WriteLn('Ssl-Verify result: ', SslVerifyResult);
      WriteLn('File time: ', FileTime);

    end else WriteLn(ErrorString);

    Free;

  end;
end.
