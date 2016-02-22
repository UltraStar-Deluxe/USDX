program testcb;

(* Program to test callbacks *)

{$INCLUDE curltest.inc}

uses curlobj;


function MyProgressCallback (
   UserData:pointer; DownloadTotal, DownloadNow, UploadTotal, UploadNow:Double): LongInt; cdecl;
begin
  inc(LongInt(UserData^));
  Write( 'Progress Function: ');
  // If we know the expected size of the file, then show percent complete, 
  // else just show the number of bytes received...
  if ( DownloadTotal > 0 ) 
  then WriteLn ( Trunc( (DownloadNow/DownloadTotal) * 100 ), '%' )
  else WriteLn ( Trunc(DownloadNow), ' bytes.' );

  Result:=0;
end;  



// My   *silly*   write function just counts the number of "<" characters.
// Your *serious* write function will probably want to do something else...
function MyWriteFunction (
    IncomingData: pChar; ItemSize, ItemCount:LongWord; UserData:pointer):LongWord; cdecl;

var I:LongInt;
begin
  Result:= ( ItemSize * ItemCount );
  for I:=0 to Result-1 do if ( IncomingData[I] = '<' ) then inc( LongInt(UserData^) );
  WriteLn('Write Function : ', Result, ' bytes');
end;  


// My header function just tries to get the server info header...
function MyHeaderFunction (
    Hdr: pChar; ItemSize, ItemCount:LongWord; UserData:pointer):LongWord; cdecl;
begin
  Result:= ( ItemSize * ItemCount );
  if ( curl_strnequal(Hdr, 'Server:', 7) <> 0 ) then string(UserData^):=Copy(Hdr, 7, Result-7);
  WriteLn('Header Function: ', Result, ' bytes');
end;  


function MyDebugFunction (
    handle: pCurl; infotype: Curl_InfoType; data:pChar; size:LongWord; UserData:pointer):LongInt; cdecl;
begin
  inc(LongInt(UserData^));
  case InfoType of
    CURLINFO_TEXT:         Write('-> Debug: ', Copy(data, 1, size));
    CURLINFO_HEADER_IN:  WriteLn('-> Debug: GOT RESPONSE HEADER');
    CURLINFO_HEADER_OUT: WriteLn('-> Debug: SENT REQUEST HEADER');
    CURLINFO_DATA_IN:;
    CURLINFO_DATA_OUT:;
    else WriteLn('-> Debug: THIS SHOULD NEVER HAPPEN!!!');
  end;  
  Result:=0;
end;  


var 
  MyCurl:tCurl;
  ProgCount:  LongInt;
  TagCount:   LongInt;
  DebugCount: LongInt;
  ServerType: String;
begin

  ProgCount  := 0;
  TagCount   := 0;
  DebugCount := 0;
  ServerType := '';

  MyCurl:=tCurl.Create(nil);

  if ( ParamCount = 1 )
  then MyCurl.URL:=ParamStr(1)
  else MyCurl.URL:='http://www.aol.com';

  MyCurl.FollowLocation:=True;

  MyCurl.WriteFunction := @MyWriteFunction;  
  MyCurl.OutputStream:=@TagCount;   

  MyCurl.HeaderFunction := @MyHeaderFunction;  
  MyCurl.HeaderStream:=@ServerType;

  MyCurl.Verbose:=True;
  MyCurl.DebugFunction := @MyDebugFunction;  
  MyCurl.DebugData := @DebugCount;

  MyCurl.NoProgress:=False;
  MyCurl.ProgressFunction :=@MyProgressCallback;
  MyCurl.ProgressData:=@ProgCount;
    
  if MyCurl.Perform then begin 
    WriteLn('Received ', MyCurl.SizeDownload, ' bytes') ;
    WriteLn('The ProgressFunction was called ', ProgCount,  ' times.');
    WriteLn('The DebugFunction was called ',    DebugCount, ' times.');
    WriteLn('The web page contains about ',    TagCount,   ' html tags.');
    if ( ServerType <> '' ) 
    then WriteLn('This site is running', ServerType) 
    else WriteLn('Unable to determine server type.');
  end else WriteLn('Transfer failed: ', MyCurl.ErrorString);

  MyCurl.Free;

end.
