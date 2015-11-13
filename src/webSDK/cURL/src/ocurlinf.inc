(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

{ Wrapper functions for "curl_easy_getinfo" ... }

{$IFDEF TCURL_INTF}
public
  function EffectiveUrl: string;
  function HttpCode: LongInt;
  function HeaderSize: LongWord;
  function RequestSize: LongWord;
  function SslVerifyResult: LongInt;
  function FileTime: LongInt;
  function TotalTime: double;
  function NameLookupTime: double;
  function ConnectTime: double;
  function PreTransferTime: double;
  function SizeUpload: LongWord;
  function SizeDownload: LongWord;
  function SpeedDownload: double;
  function SpeedUpload: double;
  function ContentLengthDownload: LongWord;
  function ContentLengthUpload: LongWord;
  function StartTransferTime: double;
  function ContentType: string;
  function RedirectCount: LongInt;
  function RedirectTime: double;

  function ResponseCode:LongInt;
  function HttpConnectCode:LongInt;
  function OsErrno:LongInt;
  function NumConnects:LongInt;

  function HttpAuthAvail:CurlAuthenticationMethods;
  function ProxyAuthAvail:CurlAuthenticationMethods;

  procedure UpdateSslEnginesList;

  procedure UpdateCookieList;

{$ENDIF TCURL_INTF}


{$IFDEF CURLOBJ_IMPLEMENTATION}

function GetStringProp(const field:pChar):string; forward;

function tCurl.EffectiveUrl: string;
var buff:pChar;
begin
  curl_easy_getinfo(fCurl, CURLINFO_EFFECTIVE_URL, buff);
  Result:=GetStringProp(buff);
end;

function tCurl.ContentType: string;
var buff:pChar;
begin
  curl_easy_getinfo(fCurl, CURLINFO_CONTENT_TYPE, buff);
  Result:=GetStringProp(buff);
end;

function tCurl.HttpCode: LongInt;
begin
  curl_easy_getinfo(fCurl, CURLINFO_HTTP_CODE, Result );
end;

function tCurl.HeaderSize: LongWord;
begin
  curl_easy_getinfo(fCurl, CURLINFO_HEADER_SIZE, Result );
end;

function tCurl.RequestSize: LongWord;
begin
  curl_easy_getinfo(fCurl, CURLINFO_REQUEST_SIZE, Result );
end;

function tCurl.SslVerifyResult: LongInt;
begin
  curl_easy_getinfo(fCurl, CURLINFO_SSL_VERIFYRESULT, Result );
end;

function tCurl.FileTime: LongInt;
begin
  curl_easy_getinfo(fCurl, CURLINFO_FILETIME, Result );
end;

function tCurl.TotalTime: double;
begin
  curl_easy_getinfo(fCurl, CURLINFO_TOTAL_TIME, Result );
end;

function tCurl.NameLookuptime: double;
begin
  curl_easy_getinfo(fCurl, CURLINFO_NAMELOOKUP_TIME, Result);
end;

function tCurl.ConnectTime: double;
begin
  curl_easy_getinfo(fCurl, CURLINFO_CONNECT_TIME, Result );
end;

function tCurl.PreTransferTime: double;
begin
  curl_easy_getinfo(fCurl, CURLINFO_PRETRANSFER_TIME, Result );
end;

function tCurl.SizeUpload: LongWord;
var dVal:double;
begin
  curl_easy_getinfo(fCurl, CURLINFO_SIZE_UPLOAD, dVal );
  Result:=LongWord(round(dVal));
end;

function tCurl.SizeDownload: LongWord;
var dVal:double;
begin
  curl_easy_getinfo(fCurl, CURLINFO_SIZE_DOWNLOAD, dVal );
  Result:=LongWord(round(dVal));
end;

function tCurl.SpeedDownload: double;
begin
  curl_easy_getinfo(fCurl, CURLINFO_SPEED_DOWNLOAD, Result );
end;

function tCurl.SpeedUpload: double;
begin
  curl_easy_getinfo(fCurl, CURLINFO_SPEED_UPLOAD, Result );
end;

function tCurl.StartTransferTime: double;
begin
  curl_easy_getinfo(fCurl, CURLINFO_STARTTRANSFER_TIME, Result );
end;

function tCurl.ContentLengthDownload: LongWord;
var dVal:double;
begin
  curl_easy_getinfo(fCurl, CURLINFO_CONTENT_LENGTH_DOWNLOAD, dVal );
  Result:=LongWord(round(dVal));
end;

function tCurl.ContentLengthUpload: LongWord;
var dVal:double;
begin
  curl_easy_getinfo(fCurl, CURLINFO_CONTENT_LENGTH_UPLOAD, dVal );
  Result:=LongWord(round(dVal));
end;

function tCurl.RedirectCount: LongInt;
begin
  curl_easy_getinfo(fCurl, CURLINFO_REDIRECT_COUNT, Result );
end;

function tCurl.RedirectTime: double;
begin
  curl_easy_getinfo(fCurl, CURLINFO_REDIRECT_TIME, Result );
end;


{new===>>>}

function tCurl.ResponseCode:LongInt;
begin
  curl_easy_getinfo(fCurl, CURLINFO_RESPONSE_CODE, Result);
end;

function tCurl.HttpConnectCode:LongInt;
begin
  curl_easy_getinfo(fCurl, CURLINFO_HTTP_CONNECTCODE, Result);
end;


function tCurl.NumConnects:LongInt;
begin
  curl_easy_getinfo(fCurl, CURLINFO_NUM_CONNECTS, Result);
end;


function tCurl.OsErrno:LongInt;
begin
  curl_easy_getinfo(fCurl, CURLINFO_OS_ERRNO, Result);
end;


function IntToAuth(Methods:LongInt):CurlAuthenticationMethods;
begin
  Result:=[];
  if ( ( Methods or CURLAUTH_BASIC ) ) > 0 then Result:=Result+[AUTH_BASIC];
  if ( ( Methods or CURLAUTH_DIGEST ) ) > 0 then Result:=Result+[AUTH_DIGEST];
  if ( ( Methods or CURLAUTH_GSSNEGOTIATE ) ) > 0 then Result:=Result+[AUTH_GSSNEGOTIATE];
  if ( ( Methods or CURLAUTH_NTLM ) ) > 0 then Result:=Result+[AUTH_NTLM];  
end;


function tCurl.HttpAuthAvail:CurlAuthenticationMethods;
var
  tmp:LongInt;
begin
  curl_easy_getinfo(fCurl, CURLINFO_HTTPAUTH_AVAIL, tmp);
  Result:=IntToAuth(tmp);
end;

function tCurl.ProxyAuthAvail:CurlAuthenticationMethods;
var
  tmp:LongInt;
begin
  curl_easy_getinfo(fCurl, CURLINFO_PROXYAUTH_AVAIL, tmp);
  Result:=IntToAuth(tmp);
end;

procedure tCurl.UpdateSslEnginesList;
var
  p:pcurl_slist;
begin
  fSslEnginesList.Clear;
  curl_easy_getinfo(fCurl, CURLINFO_SSL_ENGINES, fSslEnginesList.fList);
  p:=fSslEnginesList.fList;
  while ( p <> nil ) do begin
    inc(fSslEnginesList.fCount);
    p:=p^.next;
  end;
end;



{$ENDIF CURLOBJ_IMPLEMENTATION}