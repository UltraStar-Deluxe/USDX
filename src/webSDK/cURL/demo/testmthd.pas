program testmthd;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

uses curlobj;

{$I-} // <<-- Let threads share stdout ( ignore errors when they block )

var
  AddCount:  LongInt = 0;
  DoneCount: LongInt = 0;

const
  NULL_FILE = {$IFDEF WIN32}'NUL'{$ELSE}'/dev/null'{$ENDIF};


function ProgressMeter( clientp: pointer; dltotal, dlnow, ultotal, ulnow: double ): LongInt; cdecl;
begin
  WriteLn( tCurl(clientp).Tag, '. Receiving ', dlnow:0:0, '/', dltotal:0:0, ' from ', tCurl(clientp).Url);
  if (tCurl(clientp).Tag = AddCount) then WriteLn;
  Result:=0;
end;


procedure AddOne(m:tCurlMulti; url:pChar);
var
  c:tCurl;
begin
  inc(AddCount);
  if m.Busy then WriteLn('ADD WHILE RUNNING: ', url);
  c:=tCurl.Create(nil);
  c.URL:=url;
  c.FollowLocation:=True;
  c.OutputFile:=NULL_FILE;
  c.NoProgress:=False;
  c.ProgressFunction:=ProgressMeter;
  c.ProgressData:=c;
  c.Timeout:=60;
  c.Tag:=AddCount;
  m.AddObject(c);
end;


procedure OneDone(which:tCurl; data:pointer); cdecl;
begin
  Write( which.Tag, ': DONE  [ ',which.EffectiveURL,' ]  ');
  if ( which.ResultCode = CURLE_OK ) then WriteLn( which.SizeDownload, ' bytes (OK)')
  else WriteLn(which.ErrorString);
  case which.Tag of
    3: AddOne(tCurlMulti(data) , 'http://news.yahoo.com/');
    4: AddOne(tCurlMulti(data) , 'http://news.google.com/');
  end;
  which.Free;
  inc(DoneCount);
end;


procedure Waiting(p:pointer); cdecl;
begin
   // A GUI app could call ProcessMessages() here...
   WriteLn('**** WAITING ****');
end;

const
  URLs:array[0..7] of pChar = (
   'http://abc.go.com/',
   'http://bbc.co.uk/',
   'http://cbs.com/',
   'http://cnn.com/',
   'http://fox.com/home.htm',
   'http://msnbc.msn.com/',
   'http://nbc.com/',
   'http://www.pbs.org/'
  );

var
  m:tCurlMulti;
  i:LongInt;
begin
  m:=tCurlMulti.Create(nil);
  m.Threaded:=True;
  m.WaitInterval:=2000;
  m.WaitCallback:=Waiting;
  m.WaitData:=m;
  m.DoneData:=m;
  m.SingleDoneCallback:=OneDone;
  for i:=low(URLs) to high(URLs) do AddOne(m, URLs[i]) ;
  m.Perform();
  m.Free;
  WriteLn('Requested: ', AddCount, '    Completed: ', DoneCount);
end.



