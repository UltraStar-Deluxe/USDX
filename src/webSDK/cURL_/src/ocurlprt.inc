(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

{$IFDEF TCURL_INTF}
protected

  procedure Release;
  procedure InitFields;
  function InitTransfer:CurlCode;
  procedure DoneTransfer(aCode:CurlCode);
  function DoPerform(aCurl:pCurl):CurlCode;
  procedure MutexLock;
  procedure MutexUnlock;

  procedure SetStringProp(var field:pChar; const value:string);
  procedure SetBoolOption ( option: CurlOption; out field: boolean; const value: boolean ); overload;
  procedure SetIntOption ( option: CurlOption; out field: longint; const value: longint ); overload;
  procedure SetStrOption ( option: CurlOption; var field: pChar;  const value: string ); overload;
  procedure SetPtrOption ( option: CurlOption; out field: pointer; const value: pointer ); overload;

  procedure SetResultCode ( aCode:CurlCode; aOption:CURLoption );

  procedure SetCrLf ( const value: boolean );
  procedure SetVerbose ( const value: boolean );
  procedure SetHeader ( const value: boolean );
  procedure SetNoProgress ( const value: boolean );
  procedure SetNoBody ( const value: boolean );
  procedure SetFailOnError ( const value: boolean );
  procedure SetUpload ( const value: boolean );
  procedure SetPost ( const value: boolean );
  procedure SetFtpListOnly ( const value: boolean );
  procedure SetFtpAppend ( const value: boolean );
  procedure SetNetrc ( const value: CURL_NETRC_OPTION );
  procedure SetFollowLocation ( const value: boolean );
  procedure SetTransferText ( const value: boolean );
  procedure SetPut ( const value: boolean );
  procedure SetHttpProxyTunnel ( const value: boolean );
  procedure SetSslVerifyPeer ( const value: boolean );
  procedure SetFreshConnect ( const value: boolean );
  procedure SetForbidReuse ( const value: boolean );
  procedure SetHttpGet ( const value: boolean );
  procedure SetFtpUseEpsv ( const value: boolean );
  procedure SetFiletime ( const value: boolean );

  procedure SetAutoReferer ( const value: boolean );
  procedure SetPort ( const value: word );
  procedure SetTimeout ( const value: LongInt );
  procedure SetLowSpeedLimit ( const value: LongInt );
  procedure SetLowSpeedTime ( const value: LongInt );
  procedure SetResumeFrom ( const value: LongInt );
  procedure SetSslVersion ( const value: curl_sslversion );
  procedure SetTimeCondition ( const value: curl_TimeCond );
  procedure SetTimeValue ( const value: LongInt );
  procedure SetProxyPort ( const value: LongInt );
  procedure SetPostFieldSize ( const value: LongInt );
  procedure SetMaxRedirs ( const value: LongInt );
  procedure SetMaxConnects ( const value: LongInt );
  procedure SetConnectTimeout ( const value: LongInt );
  procedure SetUrl ( const value: string );
  procedure SetProxy ( const value: string );
  procedure SetUserPwd ( const value: string );
  procedure SetProxyUserPwd ( const value: string );
  procedure SetRange ( const value: string );
  procedure SetPostFields ( const value: string );
  procedure SetReferer ( const value: string );
  procedure SetFtpPort ( const value: string );
  procedure SetUserAgent ( const value: string );
  procedure SetCookie ( const value: string );
  procedure SetSslCert ( const value: string );
  procedure SetSslCertPasswd ( const value: string );
  procedure SetCustomRequest ( const value: string );
  procedure SetInterface ( const value: string );
  procedure SetKrb4Level ( const value: string );
  procedure SetCaInfo ( const value: string );
  procedure SetRandomFile ( const value: string );
  procedure SetEgdSocket ( const value: string );
  procedure SetCookieJar ( const value: string );
  procedure SetCookieFile ( const value: string );
  procedure SetSslCipherList ( const value: string );

  function GetUrl :string;
  function GetProxy :string;
  function GetUserPwd :string;
  function GetProxyUserPwd :string;
  function GetRange :string;
  function GetPostFields :string;
  function GetReferer :string;
  function GetFtpPort :string;
  function GetUserAgent :string;
  function GetCookie :string;
  function GetSslCert :string;
  function GetSslCertPasswd :string;
  function GetCustomRequest :string;
  function GetInterface :string;
  function GetKrb4Level :string;
  function GetCaInfo :string;
  function GetRandomFile :string;
  function GetEgdSocket :string;
  function GetCookieJar :string;
  function GetCookieFile :string;
  function GetSslCipherList :string;

  function SetListOption ( option: CurlOption; const aList:tCurlRWList ):boolean;
  function SetHttpPostOption:boolean;

  procedure SetHttpVersion ( value: curl_http_version );
  procedure SetClosePolicy ( value: curl_closepolicy );
  procedure SetProxyType ( value: curl_proxytype );
  procedure SetWriteInfo ( value: string );
  function  GetWriteInfo:string; // Not implemented
  procedure SetCookieSession ( value: boolean );
  procedure SetCaPath ( value: string );
  function  GetCaPath:string;
  procedure SetDnsCacheTimeout ( value:longint );  
  procedure SetDnsUseGlobalCache ( value:Boolean );
  procedure SetNoSignal ( value: boolean );
  procedure SetEncoding ( value: CurlEncoding );
  procedure SetSslCertType ( value: CurlCertType );
  procedure SetSslKeyType ( value: CurlKeyType );
  procedure SetSslKey ( value: string );
  function  GetSslKey:string;
  procedure SetSslVerifyHost ( value: CurlHostVerify );
  procedure SetSslEngine ( value: string );
  function  GetSslEngine:string;
  procedure SetSslEngineDefault ( value: boolean );
  procedure SetSslKeyPassword ( value:string );
  function  GetSslKeyPassword:string;
  procedure SetBufferSize ( value:LongInt );

  procedure SetHeaderFunction ( aHeaderFunc: curl_write_callback );
  procedure SetHeaderStream ( value: pointer );
  procedure SetOnHeader ( aProc:tCurlHeaderEvent );
  procedure SetHeaderFile ( value:string );
  function  GetHeaderFile:string;
  procedure InitHeaderFile;  

  procedure SetWriteFunction ( aWriteFunc: curl_write_callback );
  procedure SetOutputStream ( value: pointer);
  procedure SetOnReceive ( aProc:tCurlReceiveEvent );
  procedure SetOutputFile ( value:string );
  function  GetOutputFile:string;
  procedure InitOutputFile;

  procedure SetReadFunction ( aReadFunc: curl_read_callback );
  procedure SetInputStream ( value: pointer);
  procedure SetOnTransmit ( aProc:tCurlTransmitEvent );
  procedure SetInputFile ( value:string );
  function  GetInputFile:string;
  procedure InitInputFile;
  
  procedure SetProgressFunction ( const aFunc:curl_progress_callback );
  procedure SetProgressData ( aData:pointer );
  procedure SetOnProgress ( aProc:tCurlProgressEvent );
  
  procedure SetDebugFunction ( aFunc: curl_debug_callback );
  procedure SetDebugData ( aData:pointer );
  procedure SetOnDebug ( aProc:tCurlDebugEvent );

  procedure SetError ( code:CurlCode; const msg:string ); // <- Generate a custom error message
  function  GetErrorFile:string;
  function  GetErrorBuffer :string;
  procedure SetErrorBuffer ( const value: string ); // <- This is not exposed - the buffer is set in 'Create'

  function  GetCurlResult:CurlCode;
  
  procedure SetUnrestrictedAuth ( const value: boolean );
  procedure SetFtpUseEPRT ( const value: boolean );


  procedure SetHttpAuth ( value: CurlAuthenticationMethods);
  procedure SetProxyAuth ( value: CurlAuthenticationMethods);
  procedure SetFtpCreateMissingDirs ( value: boolean );
  procedure SetFtpResponseTimeout ( value: LongInt );
  procedure SetIpResolve ( value: CurlResolverVersion );
  procedure SetMaxFileSize ( value: LongInt );
  procedure SetSslCtxData ( value:pointer );
  procedure SetSslCtxFunction ( value: curl_ssl_ctx_callback );

  function GetReadFunction:curl_read_callback;
  procedure SetCookieListOption;
  procedure SetIoCtlCallback( value:curl_ioctl_callback);
  procedure SetIoCtlData(value: pointer);
  procedure SetTcpNoDelay(value:boolean);
  procedure SetNetRcFile( value: string );
  function GetNetRcFile:string;
  procedure SetFtpAccount(value:string);
  function GetFtpAccount:string;

  procedure SetSourceUserPwd( value:string );
  procedure SetSourceUrl( value:string );

  function GetSourceUserPwd:string;
  function GetSourceUrl:string;

  procedure SetFtpSsl( value:curl_ftpssl );
  procedure SetFtpAuth( value:curl_ftpauth );

  procedure SetIgnoreContentLength ( const value: boolean );
  procedure SetFtpSkipPasvIp ( const value: boolean );

  function GetHttpHeader:tCurlRWList;
  function GetQuote:tCurlRWList;
  function GetPostQuote:tCurlRWList;
  function GetPreQuote:tCurlRWList;
  function GetTelnetOptions:tCurlRWList;
  function GetHttp200Aliases:tCurlRWList;
  function GetSourcePreQuote:tCurlRWList;
  function GetSourceQuote:tCurlRWList;
  function GetSourcePostQuote:tCurlRWList;

{$ENDIF TCURL_INTF}


