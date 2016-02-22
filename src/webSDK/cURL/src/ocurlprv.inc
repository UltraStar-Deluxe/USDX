(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

{$IFDEF TCURL_INTF}
private
  fCurl: pCurl; { The 'handle' to the curl session }
  fCurlResult: CurlCode; { Final return code from all calls to 'curl_easy' functions }

  fCrLf: boolean;
  fVerbose: boolean;
  fHeader: boolean;
  fNoProgress: boolean;
  fNoBody: boolean;
  fFailOnError: boolean;
  fUpload: boolean;
  fPost: boolean;
  fFtpListOnly: boolean;
  fFtpAppend: boolean;
  fNetrc: CURL_NETRC_OPTION;
  fFollowLocation: boolean;
  fTransferText: boolean;
  fPut: boolean;
  fHttpProxyTunnel: boolean;
  fSslVerifyPeer: boolean;
  fFreshConnect: boolean;
  fForbidReuse: boolean;
  fHttpGet: boolean;
  fFtpUseEpsv: boolean;
  fFiletime: boolean;
  fFtpAscii: boolean;

  fAutoReferer: boolean;

  fPort: Word;
  fTimeout: LongInt;
  fLowSpeedLimit: LongInt;
  fLowSpeedTime: LongInt;
  fResumeFrom: LongInt;
  fSslVersion: curl_sslversion;
  fTimeCondition: curl_TimeCond;
  fTimeValue: LongInt;
  fProxyPort: LongInt;
  fPostFieldSize: LongInt;
  fMaxRedirs: LongInt;
  fMaxConnects: LongInt;
  fConnectTimeout: LongInt;
  fBufferSize: LongInt;
  fDnsCacheTimeout: LongInt;
  fInfileSize: LongInt;
  fCookieSession: boolean;
  fSslEngineDefault: boolean;
  fDnsUseGlobalCache: boolean;
  fNoSignal: boolean;
  fUnrestrictedAuth : boolean;
  fFtpUseEPRT : boolean;

  fUrl: pChar;
  fProxy: pChar;
  fUserPwd: pChar;
  fProxyUserPwd: pChar;
  fRange: pChar;
  fPostFields: pChar;
  fReferer: pChar;
  fFtpPort: pChar;
  fUserAgent: pChar;
  fCookie: pChar;
  fSslCert: pChar;
  fSslCertPasswd: pChar;
  fCustomRequest: pChar;
  fInterface: pChar;
  fKrb4Level: pChar;
  fCaInfo: pChar;
  fRandomFile: pChar;
  fEgdSocket: pChar;
  fCookieJar: pChar;
  fCookieFile: pChar;
  fSslCipherList: pChar;
  fWriteInfo: pChar;
  fCaPath: pChar;
  fSslKey: pChar;
  fSslEngine: pChar;
  fSslKeyPassword: pChar;

  fHttpHdr: tCurlRWList;
  fQuote: tCurlRWList;
  fPostQuote: tCurlRWList;
  fPreQuote: tCurlRWList;  
  fTelnetOptions: tCurlRWList;
  fHttp200Aliases: tCurlRWList;

  fFormData: tMultiPartFormData;
  fHttpPost: pCurl_HttpPost;

  fHttpVersion: curl_http_version;
  fClosePolicy: curl_closepolicy;
  fProxyType: curl_proxytype;
  fEncoding: CurlEncoding;
  fSslCertType: CurlCertType;
  fSslKeyType: CurlKeyType;
  fSslVerifyHost: CurlHostVerify;

  fProgressFunction: curl_progress_callback;
  fProgressData: pointer;
  fOnProgress: tCurlProgressEvent;
  
  fDebugFunction: curl_debug_callback;
  fDebugData: pointer;
  fOnDebug: tCurlDebugEvent;
  
  fErrorBuffer: pChar;
  fErrorFile: pChar;
  //fErrorStream: pIOFile;

  fHeaderScheme: tCallbackScheme;
  fReceiveScheme: tCallbackScheme;
  fTransmitScheme: tCallbackScheme;

  fPrivateData: pointer;

  fHttpAuth: CurlAuthenticationMethods;
  fProxyAuth: CurlAuthenticationMethods;
  fFtpCreateMissingDirs: boolean;
  fFtpResponseTimeout: LongInt;
  fIpResolve: CurlResolverVersion;
  fMaxFileSize: LongInt;
  fSslCtxData: pointer;
  fSslCtxFunction: curl_ssl_ctx_callback;

  fIoCtlCallback: curl_ioctl_callback;
  fIoCtlData: pointer;
  fCookieList: tCurlCookieList;
  fOnListCookies: tCurlListCookiesEvent;
  fSslEnginesList: tCurlROList;
  fTcpNoDelay: boolean;
  fNetRcFile: pChar;
  fFtpAccount: pChar;

  fSourcePreQuote: tCurlRWList;
  fSourceQuote: tCurlRWList;
  fSourcePostQuote: tCurlRWList;


  fSourceUserPwd: pChar;
  fSourceUrl: pChar;
  fFtpSsl: curl_ftpssl;
  fFtpAuth: curl_ftpauth;
  fIgnoreContentLength: boolean;

  fThdResult:CurlCode;
  fMultiNotifyDestroying:tNotifyEvent;
  fMulti:TObject;
  fPrev, fNext:tObject;

  fFtpSkipPasvIp: boolean;


{$ENDIF TCURL_INTF}
  
