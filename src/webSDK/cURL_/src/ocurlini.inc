(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

procedure tCurl.InitFields();
begin
  fCrLf:=False;
  fVerbose:=False;
  fHeader:=False;
  fNoProgress:=True;
  fNoBody:=False;
  fFailOnError:=False;
  fUpload:=False;
  fPost:=False;
  fFtpListOnly:=False;
  fFtpAppend:=False;

  fTransferText:=False;
  fPut:=False;
  fHttpProxyTunnel:=False;
  fSslVerifyPeer:=True;
  fFreshConnect:=False;
  fForbidReuse:=False;
  fHttpGet:=False;
  fFtpUseEpsv:=True;
  fFiletime:=False;
  fFtpAscii:=False;
  fUpload:=False;
  fPut:=False;
  fCookieSession:=False;
  fDnsUseGlobalCache:=False;
  fNoSignal:=False;
  fSslEngineDefault:=False;
  fUnrestrictedAuth:=False;
  fFtpUseEPRT:=False;

  fPort:=0;
  fTimeout:=0;
  fLowSpeedLimit:=0;
  fLowSpeedTime:=0;
  fResumeFrom:=0;
  fTimeValue:=0;
  fProxyPort:=0;
  fPostFieldSize:=0;
  fMaxConnects:=5;
  fConnectTimeout:=0;
  fResumeFrom:=0;
  fInfileSize:=-1;
  fDnsCacheTimeout:=60;
  fBufferSize:=0; // ???

  fUrl:=nil;
  fProxy:=nil;
  fUserPwd:=nil;
  fProxyUserPwd:=nil;
  fRange:=nil;
  fErrorBuffer:=nil;
  fPostFields:=nil;
  fReferer:=nil;
  fFtpPort:=nil;
  fUserAgent:=nil;
  fCookie:=nil;
  fSslCert:=nil;
  fSslCertPasswd:=nil;
  fCustomRequest:=nil;
  fInterface:=nil;
  fKrb4Level:=nil;

  fRandomFile:=nil;
  fEgdSocket:=nil;

  fSslCipherList:=nil;
  fWriteInfo:=nil;

  fSslEngine:=nil;
  fSslKeyPassword:=nil;
  fErrorFile:=nil;

  fNetrc:=CURL_NETRC_LAST;
  fSslVersion:=CURL_SSLVERSION_DEFAULT;
  fTimeCondition:=CURL_TIMECOND_NONE;
  fHttpVersion:=CURL_HTTP_VERSION_1_1;
  fClosePolicy:=CURLCLOSEPOLICY_NONE;
  fProxyType:=CURLPROXY_HTTP;
  fEncoding:=CURL_ENCODING_NONE;
  fSslCertType:=CURL_CERT_NONE;
  fSslKeyType:=CURL_KEY_NONE;
  fSslVerifyHost:=CURL_VERIFY_NONE;

  fOnProgress:=nil;
  fOnDebug:=nil;
  fOnListCookies:=nil;

  with fHeaderScheme do begin
    hdr_event:=nil;
    stream:=nil;
    filename:=nil;
    cb_type:=CBT_INTERNAL;
    fs_type:=FST_INTERNAL;
  end;

  with fReceiveScheme do begin
    rx_event:=nil;
    stream:=nil;
    filename:=nil;
    cb_type:=CBT_INTERNAL;
    fs_type:=FST_INTERNAL;
  end;

  with fTransmitScheme do begin
    tx_event:=nil;
    stream:=nil;
    filename:=nil;
    cb_type:=CBT_INTERNAL;
    fs_type:=FST_INTERNAL;
  end;


  fCookieJar:=nil;
  fCookieFile:=nil;

  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_COOKIEFILE, pChar(#0#0)), CURLOPT_COOKIEFILE);

  fPrivateData:=nil;
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_PRIVATE, pointer(self)), CURLOPT_PRIVATE);
  
  fHttpAuth:=[AUTH_BASIC];
  fProxyAuth:=[AUTH_BASIC];
  fFtpCreateMissingDirs:=False;
  fFtpResponseTimeout:=0;
  fIpResolve:=CURL_IPRESOLVE_WHATEVER;
  fMaxFileSize:=0;
  fSslCtxData:=nil;
  fSslCtxFunction:=nil;

  fErrorBuffer:=StrAlloc(CURL_ERROR_SIZE);
  FillChar(fErrorBuffer[0], CURL_ERROR_SIZE, #0);
  SetResultCode(curl_easy_setopt(fCurl, CURLOPT_ERRORBUFFER, @fErrorBuffer[0]), CURLOPT_ERRORBUFFER);

  fHttpHdr:=nil;
  fTelnetOptions:=nil;
  fHttp200Aliases:=nil;
  fPreQuote:=nil;
  fQuote:=nil;
  fPostQuote:=nil;
  fSourcePreQuote:=nil;
  fSourceQuote:=nil;
  fSourcePostQuote:=nil;

  fFormData:=tMultiPartFormData.Create;
  fSslEnginesList:=tCurlROList.Create;
  UpdateSslEnginesList;
  fIoCtlCallback:=nil;
  fIoCtlData:=nil;
  fTcpNoDelay:=False;
  fNetRcFile:=nil;
  fFtpAccount:=nil;

  fSourceUserPwd:=nil;
  fSourceUrl:=nil;
  fFtpSsl:=CURLFTPSSL_NONE;
  fFtpAuth:=CURLFTPAUTH_DEFAULT;
  fIgnoreContentLength:=False;

//  fFollowLocation:=False;
  SetBoolOption(CURLOPT_FOLLOWLOCATION, fFollowLocation, True);
//  fAutoReferer:=False;
  SetBoolOption(CURLOPT_AUTOREFERER, fAutoReferer, True);
//  fMaxRedirs:=0;
  SetIntOption(CURLOPT_MAXREDIRS, fMaxRedirs, 25);

{$IFDEF WIN32}

  if ( DEFAULT_WIN32_CA_CERT <> '' ) then begin
    SetStrOption(CURLOPT_CAINFO, fCaInfo, DEFAULT_WIN32_CA_CERT);
  end else fCaInfo:=nil;

  if ( DEFAULT_WIN32_CA_PATH <> '' ) then begin
    SetStrOption(CURLOPT_CAPATH, fCaPath, DEFAULT_WIN32_CA_PATH);
  end else fCaPath:=nil;

{$ELSE}
  fCaPath:=nil;
  fCaInfo:=nil;
{$ENDIF}


  fThdResult:=fCurlResult;
  fFtpSkipPasvIp:=False;
end;


procedure tCurl.Release;
begin
  if ( fHttpHdr   <> nil ) then fHttpHdr.Free;
  if ( fQuote     <> nil ) then fQuote.Free;
  if ( fPostQuote <> nil ) then fPostQuote.Free;
  if ( fPreQuote  <> nil ) then fPreQuote.Free;
  if ( fFormData  <> nil ) then fFormData.Free;
  if ( fTelnetOptions  <> nil ) then fTelnetOptions.Free;
  if ( fHttp200Aliases <> nil ) then fHttp200Aliases.Free;
  with fHeaderScheme   do if ( filename <> nil ) then StrDispose( filename );
  with fReceiveScheme  do if ( filename <> nil ) then StrDispose( filename );
  with fTransmitScheme do if ( filename <> nil ) then StrDispose( filename );
  if ( fUrl            <> nil ) then StrDispose( fUrl );
  if ( fProxy          <> nil ) then StrDispose( fProxy );
  if ( fUserPwd        <> nil ) then StrDispose( fUserPwd );
  if ( fProxyUserPwd   <> nil ) then StrDispose( fProxyUserPwd );
  if ( fRange          <> nil ) then StrDispose( fRange );
  if ( fPostFields     <> nil ) then FreeMem( fPostFields );
  if ( fReferer        <> nil ) then StrDispose( fReferer );
  if ( fFtpPort        <> nil ) then StrDispose( fFtpPort );
  if ( fUserAgent      <> nil ) then StrDispose( fUserAgent );
  if ( fCookie         <> nil ) then StrDispose( fCookie );
  if ( fSslCert        <> nil ) then StrDispose( fSslCert );
  if ( fSslCertPasswd  <> nil ) then StrDispose( fSslCertPasswd );
  if ( fCustomRequest  <> nil ) then StrDispose( fCustomRequest );
  if ( fInterface      <> nil ) then StrDispose( fInterface );
  if ( fKrb4Level      <> nil ) then StrDispose( fKrb4Level );
  if ( fCaInfo         <> nil ) then StrDispose( fCaInfo );
  if ( fRandomFile     <> nil ) then StrDispose( fRandomFile );
  if ( fEgdSocket      <> nil ) then StrDispose( fEgdSocket );
//  if ( fCookieJar      <> nil ) then StrDispose( fCookieJar );
//  if ( fCookieFile     <> nil ) then StrDispose( fCookieFile );
  if ( fSslCipherList  <> nil ) then StrDispose( fSslCipherList );
  if ( fWriteInfo      <> nil ) then StrDispose( fWriteInfo );
  if ( fCaPath         <> nil ) then StrDispose( fCaPath );
  if ( fSslKey         <> nil ) then StrDispose( fSslKey );
  if ( fSslEngine      <> nil ) then StrDispose( fSslEngine );
  if ( fSslKeyPassword <> nil ) then StrDispose( fSslKeyPassword );
  if ( fErrorBuffer    <> nil ) then StrDispose( fErrorBuffer );
  if ( fErrorFile      <> nil ) then StrDispose( fErrorFile );

  if ( fSslEnginesList  <> nil ) then fSslEnginesList.Free;
  if ( fNetRcFile       <> nil ) then StrDispose( fNetRcFile );
  if ( fFtpAccount      <> nil ) then StrDispose( fFtpAccount );

  if ( fSourcePreQuote  <> nil ) then fSourcePreQuote.Free;
  if ( fSourceQuote     <> nil ) then fSourceQuote.Free;
  if ( fSourcePostQuote <> nil ) then fSourcePostQuote.Free;
  if ( fSourceUserPwd   <> nil ) then StrDispose( fSourceUserPwd );
  if ( fSourceUrl       <> nil ) then StrDispose( fSourceUrl );

end;

