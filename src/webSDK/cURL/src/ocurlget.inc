(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

function GetStringProp(const field:pChar):string;
begin
  if ( field <> nil ) then begin
    Result:=field;
    UniqueString(Result);
  end else Result:='';
end;

function tCurl.GetUrl:string;
begin
  Result:=GetStringProp(fUrl);
end;

function tCurl.GetProxy:string;
begin
  Result:=GetStringProp(fProxy);
end;

function tCurl.GetUserPwd:string;
begin
  Result:=GetStringProp(fUserPwd);
end;

function tCurl.GetProxyUserPwd:string;
begin
  Result:=GetStringProp(fProxyUserPwd);
end;

function tCurl.GetRange:string;
begin
  Result:=GetStringProp(fRange);
end;

function tCurl.GetPostFields:string;
begin
  Result:='';
  if ( fPostFields <> nil ) then begin
    SetLength(Result, fPostFieldSize);
    Move(fPostFields[0], Result[1], fPostFieldSize);
  end;
end;

function tCurl.GetReferer:string;
begin
  Result:=GetStringProp(fReferer);
end;

function tCurl.GetFtpPort:string;
begin
  Result:=GetStringProp(fFtpPort);
end;

function tCurl.GetUserAgent:string;
begin
  Result:=GetStringProp(fUserAgent);
end;

function tCurl.GetCookie:string;
begin
  Result:=GetStringProp(fCookie);
end;

function tCurl.GetSslCert:string;
begin
  Result:=GetStringProp(fSslCert);
end;

function tCurl.GetSslCertPasswd:string;
begin
  Result:=GetStringProp(fSslCertPasswd);
end;

function tCurl.GetCustomRequest:string;
begin
  Result:=GetStringProp(fCustomRequest);
end;

function tCurl.GetInterface:string;
begin
  Result:=GetStringProp(fInterface);
end;

function tCurl.GetKrb4Level:string;
begin
  Result:=GetStringProp(fKrb4Level);
end;

function tCurl.GetCaInfo:string;
begin
  Result:=GetStringProp(fCaInfo);
end;

function tCurl.GetRandomFile:string;
begin
  Result:=GetStringProp(fRandomFile);
end;

function tCurl.GetEgdSocket:string;
begin
  Result:=GetStringProp(fEgdSocket);
end;

function tCurl.GetCookieJar:string;
begin
  Result:=GetStringProp(fCookieJar);
end;

function tCurl.GetCookieFile:string;
begin
  Result:=GetStringProp(fCookieFile);
end;

function tCurl.GetSslCipherList:string;
begin
  Result:=GetStringProp(fSslCipherList);
end;


function tCurl.GetErrorBuffer:string;
begin
  if (fErrorBuffer <> nil) and ( fErrorBuffer[0] <> #0 )
  then GetErrorBuffer:=GetStringProp(fErrorBuffer)
  else case fCurlResult of
    CURLE_OK:  GetErrorBuffer:='success';
    CURLE_WRITE_ERROR: GetErrorBuffer:='Error writing local file.';
    CURLE_READ_ERROR: GetErrorBuffer:='Error reading local file.';
    else GetErrorBuffer:='Unknown error.'; // <- I think this should never happen?
  end;
end;

function tCurl.GetCurlResult:CurlCode;
begin
  Result:=fCurlResult;
  fCurlResult:=CURLE_OK;
end;

function tCurl.GetWriteInfo: string; // Not implemented
begin
  Result:=GetStringProp(fWriteInfo);
end;

function tCurl.GetCaPath:string;
begin
  Result:=GetStringProp(fCaPath);
end;

function tCurl.GetSslEngine:string;
begin
  Result:=GetStringProp(fSslEngine);
end;

function tCurl.GetSslKeyPassword:string;
begin
  Result:=GetStringProp(fSslKeyPassword);
end;


function tCurl.GetSslKey:string;
begin
  Result:=GetStringProp(fSslKey);
end;

function tCurl.GetHeaderFile:string;
begin
  Result:=GetStringProp(fHeaderScheme.filename);
end;

function tCurl.GetOutputFile:string;
begin
  Result:=GetStringProp(fReceiveScheme.filename);
end;

function tCurl.GetInputFile:string;
begin
  Result:=GetStringProp(fTransmitScheme.filename);
end;

function tCurl.GetErrorFile:string;
begin
  Result:=GetStringProp(fErrorFile);
end;


function tCurl.GetReadFunction:curl_read_callback;
begin
Result:=curl_read_callback(fTransmitScheme.callback);
end;

function tCurl.GetNetRcFile:string;
begin
  Result:=GetStringProp(fNetRcFile)
end;


function tCurl.GetFtpAccount:string;
begin
  Result:=GetStringProp(fFtpAccount);
end;

function tCurl.GetSourceUserPwd:string;
begin
  Result:=GetStringProp(fSourceUserPwd);
end;

function tCurl.GetSourceUrl:string;
begin
  Result:=GetStringProp(fSourceUrl);
end;

function RW_LIST_NOT_NIL(var o:tCurlRWList):tCurlRWList;
begin
  if ( o = nil ) then o:=tCurlRWList.Create;
  Result:=o;
end;

function tCurl.GetHttpHeader:tCurlRWList;
begin
  Result:=RW_LIST_NOT_NIL(fHttpHdr);
end;

function tCurl.GetQuote:tCurlRWList;
begin
  Result:=RW_LIST_NOT_NIL(fQuote);
end;

function tCurl.GetPostQuote:tCurlRWList;
begin
  Result:=RW_LIST_NOT_NIL(fPostQuote);
end;

function tCurl.GetPreQuote:tCurlRWList;
begin
  Result:=RW_LIST_NOT_NIL(fPreQuote);
end;

function tCurl.GetTelnetOptions:tCurlRWList;
begin
  Result:=RW_LIST_NOT_NIL(fTelnetOptions);
end;

function tCurl.GetHttp200Aliases:tCurlRWList;
begin
  Result:=RW_LIST_NOT_NIL(fHttp200Aliases);
end;

function tCurl.GetSourcePreQuote:tCurlRWList;
begin
  Result:=RW_LIST_NOT_NIL(fSourcePreQuote);
end;

function tCurl.GetSourceQuote:tCurlRWList;
begin
  Result:=RW_LIST_NOT_NIL(fSourceQuote);
end;

function tCurl.GetSourcePostQuote:tCurlRWList;
begin
  Result:=RW_LIST_NOT_NIL(fSourcePostQuote);
end;
