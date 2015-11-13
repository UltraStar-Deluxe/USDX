(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)


procedure tCurl.SetStringProp(var field:pChar; const value:string);
var 
  old_len, new_len: LongInt;
begin
  if ( field = nil ) then old_len:=0 else old_len:=StrLen(field);
  new_len:=Length(value);
  if ( new_len = old_len ) and ( ( new_len = 0 ) or ( StrLComp(pChar(value), field, new_len) = 0 ) ) then EXIT;
  if ( new_len = 0 ) or ( old_len < new_len ) then begin
    StrDispose(field);
    field:=nil;
  end else FillChar(field[0], old_len, #0);
  if ( new_len > 0 ) then begin
    if ( field = nil ) then field:=StrAlloc(new_len+1);
    FillChar(field[0], new_len+1, #0);
    StrLCopy(field, pChar(value), new_len);
  end;
end;

function tCurl.SetListOption(option: CurlOption; const aList:tCurlRWList):boolean;
begin
  if ( aList <> nil ) then begin
    if ( aList.fList <> nil ) then SetResultCode(curl_easy_setopt(fCurl, option, aList.fList), option);
    Result:= ( fCurlResult = CURLE_OK );
  end else Result:=True;
end;

function tCurl.SetHttpPostOption:boolean;
begin
  if ( fHttpPost <> nil ) then SetResultCode(curl_easy_setopt(fCurl, CURLOPT_HTTPPOST, fHttpPost), CURLOPT_HTTPPOST);
  Result:= ( fCurlResult = CURLE_OK );
end;

procedure tCurl.SetBoolOption(option: CurlOption; out field: boolean; const value: boolean);
begin
  field:=value;
  SetResultCode(curl_easy_setopt(fCurl, option, LongInt(field)), option);
end;

procedure tCurl.SetStrOption(option: CurlOption; var field: pChar; const value: string);
begin
  SetStringProp(field, value);
  SetResultCode(curl_easy_setopt(fCurl, option, pChar(field)), option);
end;

procedure tCurl.SetIntOption(option: CurlOption; out field:longint; const value:longint);
begin
  field:=value;
  SetResultCode(curl_easy_setopt(fCurl, option, field), option);
end;

procedure tCurl.SetPtrOption(option: CurlOption; out field: pointer; const value: pointer);
begin
  field:=value;
  SetResultCode(curl_easy_setopt(fCurl, option, value), option);
end;

procedure tCurl.SetCrLf (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_CRLF, fCrLf, value); 
end;

procedure tCurl.SetVerbose (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_VERBOSE, fVerbose, value); 
end;

procedure tCurl.SetHeader (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_HEADER, fHeader, value); 
end;

procedure tCurl.SetNoProgress (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_NOPROGRESS, fNoProgress, value); 
end;

procedure tCurl.SetNoBody (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_NOBODY, fNoBody, value); 
end;

procedure tCurl.SetFailOnError (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_FAILONERROR, fFailOnError, value); 
end;

procedure tCurl.SetUpload (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_UPLOAD, fUpload, value); 
end;

procedure tCurl.SetFtpListOnly (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_FTPLISTONLY, fFtpListOnly, value); 
end;

procedure tCurl.SetFtpAppend (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_FTPAPPEND, fFtpAppend, value); 
end;

procedure tCurl.SetFollowLocation (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_FOLLOWLOCATION, fFollowLocation, value); 
end;

procedure tCurl.SetTransferText (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_TRANSFERTEXT, fTransferText, value); 
end;

procedure tCurl.SetHttpProxyTunnel (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_HTTPPROXYTUNNEL, fHttpProxyTunnel, value); 
end;

procedure tCurl.SetSslVerifyPeer (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_SSL_VERIFYPEER, fSslVerifyPeer, value); 
end;

procedure tCurl.SetFreshConnect (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_FRESH_CONNECT, fFreshConnect, value); 
end;

procedure tCurl.SetForbidReuse (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_FORBID_REUSE, fForbidReuse, value); 
end;

procedure tCurl.SetFtpUseEpsv (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_FTP_USE_EPSV, fFtpUseEpsv, value); 
end;

procedure tCurl.SetFiletime (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_FILETIME, fFiletime, value); 
end;

procedure tCurl.SetAutoReferer (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_AUTOREFERER, fAutoReferer, value); 
end;

procedure tCurl.SetPort (const value: word);
var
  Tmp:LongInt;
begin
  Tmp:=value;
  SetIntOption(CURLOPT_PORT, Tmp, LongInt(value));
  fPort:=WORD(Tmp);
end;

procedure tCurl.SetTimeout (const value: LongInt);
begin
  SetIntOption(CURLOPT_TIMEOUT, fTimeout, value);
end;

procedure tCurl.SetLowSpeedLimit (const value: LongInt);
begin
  SetIntOption(CURLOPT_LOW_SPEED_LIMIT, fLowSpeedLimit, value);
end;

procedure tCurl.SetLowSpeedTime (const value: LongInt);
begin
  SetIntOption(CURLOPT_LOW_SPEED_TIME, fLowSpeedTime, value);
end;

procedure tCurl.SetRange (const value: string);
begin
  if ( fResumeFrom <> 0 ) then SetResumeFrom(0);
  SetStrOption(CURLOPT_RANGE, fRange, value); 
end;

procedure tCurl.SetResumeFrom (const value: LongInt);
begin
  if ( fRange <> nil ) then SetRange('');
  SetIntOption(CURLOPT_RESUME_FROM, fResumeFrom, value);
end;

procedure tCurl.SetTimeValue (const value: LongInt);
begin
  SetIntOption(CURLOPT_TIMEVALUE, fTimeValue, value);
end;

procedure tCurl.SetProxyPort (const value: LongInt);
begin
  SetIntOption(CURLOPT_PROXYPORT, fProxyPort, value);
end;

procedure tCurl.SetMaxRedirs (const value: LongInt);
begin
  SetIntOption(CURLOPT_MAXREDIRS, fMaxRedirs, value);
end;

procedure tCurl.SetMaxConnects (const value: LongInt);
begin
  SetIntOption(CURLOPT_MAXCONNECTS, fMaxConnects, value);
end;

procedure tCurl.SetConnectTimeout (const value: LongInt);
begin
  SetIntOption(CURLOPT_CONNECTTIMEOUT, fConnectTimeout, value);
end;

procedure tCurl.SetUrl (const value: string);
begin
  SetStrOption(CURLOPT_URL, fUrl, value); 
end;

procedure tCurl.SetProxy (const value: string);
begin
  SetStrOption(CURLOPT_PROXY, fProxy, value); 
end;

procedure tCurl.SetUserPwd (const value: string);
begin
  SetStrOption(CURLOPT_USERPWD, fUserPwd, value); 
end;

procedure tCurl.SetProxyUserPwd (const value: string);
begin
  SetStrOption(CURLOPT_PROXYUSERPWD, fProxyUserPwd, value); 
end;

procedure tCurl.SetReferer (const value: string);
begin
  SetStrOption(CURLOPT_REFERER, fReferer, value); 
end;

procedure tCurl.SetFtpPort (const value: string);
begin
  SetStrOption(CURLOPT_FTPPORT, fFtpPort, value); 
end;

procedure tCurl.SetUserAgent (const value: string);
begin
  SetStrOption(CURLOPT_USERAGENT, fUserAgent, value); 
end;

procedure tCurl.SetCookie (const value: string);
begin
  SetStrOption(CURLOPT_COOKIE, fCookie, value); 
end;

procedure tCurl.SetSslCert (const value: string);
begin
  SetStrOption(CURLOPT_SSLCERT, fSslCert, value); 
end;

procedure tCurl.SetSslCertPasswd (const value: string);
begin
  SetStrOption(CURLOPT_SSLCERTPASSWD, fSslCertPasswd, value); 
end;

procedure tCurl.SetCustomRequest (const value: string);
begin
  SetStrOption(CURLOPT_CUSTOMREQUEST, fCustomRequest, value); 
end;

procedure tCurl.SetInterface (const value: string);
begin
  SetStrOption(CURLOPT_INTERFACE, fInterface, value); 
end;

procedure tCurl.SetKrb4Level (const value: string);
begin
  SetStrOption(CURLOPT_KRB4LEVEL, fKrb4Level, value); 
end;

procedure tCurl.SetCaInfo (const value: string);
begin
  SetStrOption(CURLOPT_CAINFO, fCaInfo, value); 
end;

procedure tCurl.SetRandomFile (const value: string);
begin
  SetStrOption(CURLOPT_RANDOM_FILE, fRandomFile, value); 
end;

procedure tCurl.SetEgdSocket (const value: string);
begin
  SetStrOption(CURLOPT_EGDSOCKET, fEgdSocket, value); 
end;

procedure tCurl.SetCookieJar (const value: string);
begin
  SetStrOption(CURLOPT_COOKIEJAR, fCookieJar, value); 
end;

procedure tCurl.SetCookieFile (const value: string);
begin
  fCookieList.Clear;
  if ( value <> '' ) and FileIsReadable(value) then fCookieList.AddFromFile(value);
  SetStrOption(CURLOPT_COOKIEFILE, fCookieFile, value);
  if ( fCookieJar = nil ) then SetCookieJar(value);
end;

procedure tCurl.SetSslCipherList (const value: string);
begin
  SetStrOption(CURLOPT_SSL_CIPHER_LIST, fSslCipherList, value);
end;

procedure tCurl.SetErrorBuffer (const value: string);
begin
  SetStrOption(CURLOPT_ERRORBUFFER, fErrorBuffer, value); 
end;

procedure tCurl.SetWriteInfo( value: string); // Not impemented in libcurl
begin
  SetStrOption(CURLOPT_WRITEINFO, fWriteInfo, value); 
end;

procedure tCurl.SetCookieSession( value: boolean);
begin
  SetBoolOption(CURLOPT_COOKIESESSION, fCookieSession, value); 
end;

procedure tCurl.SetCaPath( value: string);
begin
  SetStrOption(CURLOPT_CAPATH, fCaPath, value);
end;

procedure tCurl.SetDnsCacheTimeout( value:LongInt);
begin
// Sec's to store name resolves: default=60sec; Zero=disable; -1=forever;
  SetIntOption(CURLOPT_DNS_CACHE_TIMEOUT, fDnsCacheTimeout, value);
end;

procedure tCurl.SetDnsUseGlobalCache( value:Boolean);
begin
  SetBoolOption(CURLOPT_DNS_USE_GLOBAL_CACHE , fDnsUseGlobalCache, value);
end;

procedure tCurl.SetNoSignal( value:boolean);
begin
  SetBoolOption(CURLOPT_NOSIGNAL, fNoSignal, value);
end;

procedure tCurl.SetSslKey( value: string);
begin
  SetStrOption(CURLOPT_SSLKEY, fSslKey, value);
end;

procedure tCurl.SetSslEngine( value: string);
begin
  SetStrOption(CURLOPT_SSLENGINE, fSslEngine, value);
end;

procedure tCurl.SetSslEngineDefault( value: boolean);
begin
  SetBoolOption(CURLOPT_SSLENGINE_DEFAULT, fSslEngineDefault, value);
end;

procedure tCurl.SetSslKeyPassword(value:string);
begin
  SetStrOption(CURLOPT_SSLKEYPASSWD, fSslKeyPassword, value);
end;

procedure tCurl.SetBufferSize(value:LongInt);
begin
  SetIntOption(CURLOPT_BUFFERSIZE, fBufferSize, value)
end;

procedure tCurl.SetHttpVersion( value: curl_http_version);
begin
  fHttpVersion:=value;
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_HTTP_VERSION, LongInt(value)), CURLOPT_HTTP_VERSION);
end;

procedure tCurl.SetClosePolicy( value: curl_closepolicy);
begin
  fClosePolicy:=value;
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_CLOSEPOLICY, LongInt(value)), CURLOPT_CLOSEPOLICY);
end;

procedure tCurl.SetTimeCondition (const value: curl_TimeCond);
begin
  fTimeCondition:=value;
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_TIMECONDITION, LongInt(value)), CURLOPT_TIMECONDITION);
end;

procedure tCurl.SetNetRc (const value: CURL_NETRC_OPTION );
begin 
  fNetrc:=value;
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_NETRC, LongInt(value)), CURLOPT_NETRC); 
end;

procedure tCurl.SetSslVersion (const value: curl_sslversion);
begin
  fSslVersion:=value;
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_SSLVERSION, LongInt(value)), CURLOPT_SSLVERSION);
end;

procedure tCurl.SetProxyType( value: curl_proxytype);
begin
  fProxyType:=value;
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_PROXYTYPE, LongInt(value)), CURLOPT_PROXYTYPE);
end;


const 
 STR_ENC_DEFLATE: pChar = 'deflate';
 STR_ENC_IDENTITY: pChar = 'identity';
 STR_CERT_PEM: pChar = 'PEM';
 STR_CERT_DER: pChar = 'DER';
 STR_CERT_ENG: pChar = 'ENG';

procedure tCurl.SetEncoding( value: CurlEncoding);
begin
  fEncoding:=value;
  case value of 
    CURL_ENCODING_NONE:     SetResultCode(curl_easy_setopt(fCurl, CURLOPT_ENCODING, nil), CURLOPT_ENCODING);
    CURL_ENCODING_IDENTITY: SetResultCode(curl_easy_setopt(fCurl, CURLOPT_ENCODING, @STR_ENC_DEFLATE[0]), CURLOPT_ENCODING);
    CURL_ENCODING_DEFLATE:  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_ENCODING, @STR_ENC_IDENTITY[0]), CURLOPT_ENCODING);
    else SetResultCode(CURLE_BAD_CONTENT_ENCODING, CURLOPT_ENCODING);
  end;
end;

procedure tCurl.SetSslCertType( value: CurlCertType);
begin
  fSslCertType:=value;
  case value of 
    CURL_CERT_NONE: SetResultCode(curl_easy_setopt(fCurl, CURLOPT_SSLCERTTYPE, nil), CURLOPT_SSLCERTTYPE);
    CURL_CERT_PEM:  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_SSLCERTTYPE, @STR_CERT_PEM[0]), CURLOPT_SSLCERTTYPE);
    CURL_CERT_DER:  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_SSLCERTTYPE, @STR_CERT_DER[0]), CURLOPT_SSLCERTTYPE);
    else SetResultCode(CURLE_SSL_CERTPROBLEM, CURLOPT_SSLCERTTYPE);
  end;
end;

procedure tCurl.SetSslKeyType( value: CurlKeyType);
begin
  fSslKeyType:=value;
  case value of 
    CURL_KEY_NONE: SetResultCode(curl_easy_setopt(fCurl, CURLOPT_SSLKEYTYPE, nil), CURLOPT_SSLKEYTYPE);
    CURL_KEY_DER:  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_SSLKEYTYPE, @STR_CERT_DER[0]), CURLOPT_SSLKEYTYPE);
    CURL_KEY_ENG:  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_SSLKEYTYPE, @STR_CERT_ENG[0]), CURLOPT_SSLKEYTYPE);
    else SetResultCode(CURLE_SSL_CERTPROBLEM, CURLOPT_SSLKEYTYPE);
  end;
end;

procedure tCurl.SetSslVerifyHost( value: CurlHostVerify);
begin
  fSslVerifyHost:=value;
  case value of 
    CURL_VERIFY_NONE:  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_SSL_VERIFYHOST , 0), CURLOPT_SSL_VERIFYHOST);
    CURL_VERIFY_EXIST: SetResultCode(curl_easy_setopt(fCurl, CURLOPT_SSL_VERIFYHOST , 1), CURLOPT_SSL_VERIFYHOST);
    CURL_VERIFY_MATCH: SetResultCode(curl_easy_setopt(fCurl, CURLOPT_SSL_VERIFYHOST , 2), CURLOPT_SSL_VERIFYHOST);
    else SetResultCode(CURLE_SSL_CERTPROBLEM, CURLOPT_SSL_VERIFYHOST);
  end;
end;

procedure tCurl.SetPostFields (const value: string); 
begin
  SetBoolOption(CURLOPT_POST, fPost, True);
  if ( fPostFields <> nil ) then begin
    FreeMem(fPostFields);
    fPostFields:=nil;
  end;
  SetIntOption(CURLOPT_POSTFIELDSIZE, fPostFieldSize, Length(value));
  if ( fPostFieldSize > 0 ) then begin
    GetMem(fPostFields, fPostFieldSize+1);
    Move(Value[1], fPostFields[0], fPostFieldSize);
    fPostFields[fPostFieldSize]:=#0;
  end;
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_POSTFIELDS, fPostFields), CURLOPT_POSTFIELDS);
end;

procedure tCurl.SetHttpGet (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_HTTPGET, fHttpGet, value);
end;

procedure tCurl.SetUnrestrictedAuth (const value: boolean); 
begin 
  SetBoolOption(CURLOPT_UNRESTRICTED_AUTH, fUnrestrictedAuth, value);
end;

procedure tCurl.SetFtpUseEPRT ( const value: boolean );
begin 
  SetBoolOption(CURLOPT_FTP_USE_EPRT, fFtpUseEPRT, value);
end;

function AuthenticationsToInteger(Methods:CurlAuthenticationMethods):LongInt;
begin
  Result:=CURLAUTH_NONE;
  if (AUTH_BASIC in Methods) then Result:=Result or CURLAUTH_BASIC;
  if (AUTH_DIGEST in Methods) then Result:=Result or CURLAUTH_DIGEST;
  if (AUTH_GSSNEGOTIATE in Methods) then Result:=Result or CURLAUTH_GSSNEGOTIATE;
  if (AUTH_NTLM in Methods) then Result:=Result or CURLAUTH_NTLM;
end;

procedure tCurl.SetHttpAuth(value: CurlAuthenticationMethods);
var 
  dummy:LongInt;
begin
  fHttpAuth:=value;
  SetIntOption(CURLOPT_HTTPAUTH, dummy, AuthenticationsToInteger(value));
end;

procedure tCurl.SetProxyAuth(value: CurlAuthenticationMethods);
var 
  dummy:LongInt;
begin
  fProxyAuth:=value;
  SetIntOption(CURLOPT_PROXYAUTH, dummy, AuthenticationsToInteger(value));
end;

procedure tCurl.SetIpResolve(value: CurlResolverVersion);
var 
  dummy:LongInt;
begin
  fIPResolve:=value;
  SetIntOption(CURLOPT_IPRESOLVE, dummy, LongInt(value));
end;

procedure tCurl.SetFtpCreateMissingDirs(value: boolean);
begin
  SetBoolOption(CURLOPT_FTP_CREATE_MISSING_DIRS, fFtpCreateMissingDirs, value);
end;

procedure tCurl.SetFtpResponseTimeout(value: LongInt);
begin
  SetIntOption(CURLOPT_FTP_RESPONSE_TIMEOUT, fFtpResponseTimeout, value);
end;

procedure tCurl.SetMaxFileSize(value: LongInt);
begin
  SetIntOption(CURLOPT_MAXFILESIZE, fMaxFileSize, value);
end;

procedure tCurl.SetCookieListOption;
begin
  if ( COOKIES_LIB_STALE in fCookieList.fState ) then fCookieList.SendListToLib;
end;

procedure tCurl.UpdateCookieList;
begin
  if not ( COOKIES_LIB_STALE in fCookieList.fState ) then fCookieList.fState:=fCookieList.fState+[COOKIES_LIB_STALE];
end;

procedure tCurl.SetTcpNoDelay(value:boolean);
begin
  SetBoolOption(CURLOPT_TCP_NODELAY, fTcpNoDelay, value);
end;


procedure tCurl.SetNetRcFile( value: string );
begin
  SetStrOption(CURLOPT_NETRC_FILE, fNetRcFile, value);
end;

procedure tCurl.SetFtpAccount( value:string );
begin
  SetStrOption( CURLOPT_FTP_ACCOUNT, fFtpAccount, value);
end;

procedure tCurl.SetSourceUserPwd( value:string );
begin
  SetStrOption(CURLOPT_SOURCE_USERPWD, fSourceUserPwd, value);
end;

procedure tCurl.SetSourceUrl( value:string );
begin
  SetStrOption(CURLOPT_SOURCE_URL, fSourceUrl, value);
end;


procedure tCurl.SetFtpSsl( value:curl_ftpssl );
var
  dummy:LongInt;
begin
  fFtpSsl:=value;
  SetIntOption(CURLOPT_FTP_SSL, dummy, LongInt(value));
end;

procedure tCurl.SetFtpAuth( value:curl_ftpauth );
var
  dummy:LongInt;
begin
  fFtpAuth:=value;
  SetIntOption(CURLOPT_FTPSSLAUTH, dummy, LongInt(value));
end;

procedure tCurl.SetIgnoreContentLength ( const value: boolean );
begin
  SetBoolOption(CURLOPT_IGNORE_CONTENT_LENGTH, fIgnoreContentLength, value);
end;

procedure tCurl.SetPost (const value: boolean); // OBSOLETE, PostFields sets this, HttpGet unsets it. 
begin 
  SetBoolOption(CURLOPT_POST, fPost, value); 
end;

procedure tCurl.SetPut (const value: boolean); // OBSOLETE, use tCurl.Upload instead.
begin 
  SetBoolOption(CURLOPT_PUT, fPut, value); 
end;

procedure tCurl.SetPostFieldSize (const value: LongInt); // OBSOLETE, PostFields sets this to Length(fPostFields)
begin
  SetIntOption(CURLOPT_POSTFIELDSIZE, fPostFieldSize, value);
end;

procedure tCurl.SetFtpSkipPasvIp (const value: boolean); 
begin
  SetBoolOption(CURLOPT_FTP_SKIP_PASV_IP, fFtpSkipPasvIp, value);
end;