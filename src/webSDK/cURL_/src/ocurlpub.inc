(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

public
  property ResultCode: CurlCode read GetCurlResult;
  property Handle: pCurl read fCurl;
  property ErrorString: string read GetErrorBuffer;
  property CrLf: boolean read fCrLf write SetCrLf;
  property NetRc: CURL_NETRC_OPTION read fNetRc write SetNetRc;
  property HttpProxyTunnel: boolean read fHttpProxyTunnel write SetHttpProxyTunnel;
  property SslVerifyPeer: boolean read fSslVerifyPeer write SetSslVerifyPeer;
  property FreshConnect: boolean read fFreshConnect write SetFreshConnect;
  property ForbidReuse: boolean read fForbidReuse write SetForbidReuse;
  property HttpGet: boolean read fHttpGet write SetHttpGet;
  property FtpUseEpsv: boolean read fFtpUseEpsv write SetFtpUseEpsv;
  property RequestFiletime: boolean read fFiletime write SetFiletime;
  property FtpAscii: boolean read fFtpAscii write SetTransferText;
  property AutoReferer: boolean read fAutoReferer write SetAutoReferer;
  property LowSpeedLimit: LongInt read fLowSpeedLimit write SetLowSpeedLimit;
  property LowSpeedTime: LongInt read fLowSpeedTime write SetLowSpeedTime;
  property ResumeFrom: LongInt read fResumeFrom write SetResumeFrom;
  property SslVersion: curl_sslversion read fSslVersion write SetSslVersion;
  property TimeCondition: curl_TimeCond read fTimeCondition write SetTimeCondition;
  property TimeValue: LongInt read fTimeValue write SetTimeValue;
  property ProxyPort: LongInt read fProxyPort write SetProxyPort;
  property PostFieldSize: LongInt read fPostFieldSize write SetPostFieldSize;
  property MaxRedirs: LongInt read fMaxRedirs write SetMaxRedirs;
  property MaxConnects: LongInt read fMaxConnects write SetMaxConnects;
  property ProxyUserPwd: string read GetProxyUserPwd write SetProxyUserPwd;
  property Range: string read GetRange write SetRange;
  property FtpPort: string read GetFtpPort write SetFtpPort;
  property SslCert: string read GetSslCert write SetSslCert;
  property SslCertPasswd: string read GetSslCertPasswd write SetSslCertPasswd;
  property CustomRequest: string read GetCustomRequest write SetCustomRequest;
  property NetInterface: string read GetInterface write SetInterface;
  property Krb4Level: string read GetKrb4Level write SetKrb4Level;
  property CaInfo: string read GetCaInfo write SetCaInfo;
  property RandomFile: string read GetRandomFile write SetRandomFile;
  property EgdSocket: string read GetEgdSocket write SetEgdSocket;
  property FollowLocation: boolean read fFollowLocation write SetFollowLocation;
  property Header: boolean read fHeader write SetHeader;
  property FtpListOnly: boolean read fFtpListOnly write SetFtpListOnly;
  property FtpAppend: boolean read fFtpAppend write SetFtpAppend;
  property TransferText: boolean read fTransferText write SetTransferText;
  property HttpHeader: tCurlRWList read GetHttpHeader;
  property HttpPost: pCurl_HttpPost read fHttpPost write fHttpPost;
  property Quote: tCurlRWList read GetQuote;
  property PostQuote: tCurlRWList read GetPostQuote;
  property PreQuote: tCurlRWList read GetPreQuote;
  property TelnetOptions: tCurlRWList read GetTelnetOptions;
  property FormData: tMultiPartFormData read fFormData write fFormData;
  property Cookie: string read GetCookie write SetCookie;
  property CookieJar: string read GetCookieJar write SetCookieJar;
  property CookieSession: boolean read fCookieSession write SetCookieSession;

  property NoBody: boolean read fNoBody write SetNoBody;
  property Post: boolean read fPost write SetPost; // OBSOLETE, PostFields sets this
  property Port: WORD read fPort write SetPort;
  property Put: boolean read fPut write SetPut;
  property Timeout: LongInt read fTimeout write SetTimeout;
  property HttpVersion: curl_http_version read fHttpVersion write SetHttpVersion;
  property ClosePolicy: curl_closepolicy read fClosePolicy write SetClosePolicy;
  property ProxyType: curl_proxytype read fProxyType write SetProxyType;
  property Encoding: CurlEncoding read fEncoding write SetEncoding;
  property SslCertType: CurlCertType read fSslCertType write SetSslCertType;
  property SslKeyType: CurlKeyType read fSslKeyType write SetSslKeyType;
  property WriteInfo: string read GetWriteInfo write SetWriteInfo; // Not implemented.
  property CaPath: string read GetCaPath write SetCaPath;
  property DnsCacheTimeout: LongInt read fDnsCacheTimeout write SetDnsCacheTimeout;
  property DnsUseGlobalCache: boolean read fDnsUseGlobalCache write SetDnsUseGlobalCache;
  property NoSignal: boolean read fNoSignal write SetNoSignal;
  property Http200Aliases: tCurlRWList read GetHttp200Aliases;
  property SslKey: string read GetSslKey write SetSslKey;
  property SslVerifyHost: CurlHostVerify read fSslVerifyHost write SetSslVerifyHost;
  property SslEngine: string read GetSslEngine write SetSslEngine;
  property SslEngineDefault: boolean read fSslEngineDefault write SetSslEngineDefault;
  property SslCipherList: string read GetSslCipherList write SetSslCipherList;
  property SslKeyPassword: string read GetSslKeyPassword write SetSslKeyPassword;
  property BufferSize: LongInt read fBufferSize write SetBufferSize;
  property NoProgress: boolean read fNoProgress write SetNoProgress;
  property ProgressFunction: curl_progress_callback read fProgressFunction write SetProgressFunction;
  property ProgressData: pointer read fProgressData write SetProgressData;
  property Verbose: boolean read fVerbose write SetVerbose;
  property DebugFunction: curl_debug_callback read fDebugFunction write SetDebugFunction;
  property DebugData: pointer read fDebugData write SetDebugData;

  property HeaderFunction: curl_write_callback read fHeaderScheme.callback write SetHeaderFunction;
  property HeaderStream: pointer read fHeaderScheme.stream write SetHeaderStream;
  property HeaderFile: string read GetHeaderFile write SetHeaderFile;
  property WriteFunction: curl_write_callback read fReceiveScheme.callback write SetWriteFunction;
  property OutputStream: pointer read fReceiveScheme.stream write SetOutputStream;
  property ReadFunction: curl_read_callback read GetReadFunction write SetReadFunction;
  property InputStream: pointer read fTransmitScheme.stream write SetInputStream;
  property PrivateData: pointer read fPrivateData write fPrivateData;
  property UnrestrictedAuth: boolean read fUnrestrictedAuth write SetUnrestrictedAuth;
  property FtpUseEPRT: boolean read fFtpUseEPRT write SetFtpUseEPRT;

  property HttpAuthenticationMethods: CurlAuthenticationMethods read fHttpAuth write SetHttpAuth;
  property ProxyAuthenticationMethods: CurlAuthenticationMethods read fProxyAuth write SetProxyAuth;
  property FtpCreateMissingDirs: boolean read fFtpCreateMissingDirs write SetFtpCreateMissingDirs;
  property FtpResponseTimeout: LongInt read fFtpResponseTimeout write SetFtpResponseTimeout;
  property IpResolverVersion: CurlResolverVersion read fIpResolve write SetIpResolve;
  property MaxFileSize: Longint read fMaxFileSize write SetMaxFileSize;
  property SslCtxFunction: curl_ssl_ctx_callback read fSslCtxFunction write SetSslCtxFunction;
  property SslCtxData: pointer read fSslCtxData write SetSslCtxData;

  property CookieList: tCurlCookieList read fCookieList;
  property SslEnginesList: tCurlROList read fSslEnginesList;
  property IoCtlFunction: curl_ioctl_callback read fIoCtlCallback write fIoCtlCallback;
  property IoCtlData: pointer read fIoCtlData write SetIoCtlData;
  property TcpNoDelay: boolean read fTcpNoDelay write SetTcpNoDelay;
  property NetRcFile: string read GetNetRcFile write SetNetRcFile;
  property FtpAccount: string read GetFtpAccount write SetFtpAccount;

  property SourcePreQuote: tCurlRWList read GetSourcePreQuote;
  property SourceQuote: tCurlRWList read GetSourceQuote;
  property SourcePostQuote: tCurlRWList read GetSourcePostQuote;
  property SourceUserPwd: string read GetSourceUserPwd write SetSourceUserPwd;
  property SourceUrl: string read GetSourceUrl write SetSourceUrl;
  property FtpSsl: curl_ftpssl read fFtpSsl write SetFtpSsl;
  property FtpAuth: curl_ftpauth read fFtpAuth write SetFtpAuth;

  property IgnoreContentLength: boolean read fIgnoreContentLength write SetIgnoreContentLength;
  property FtpSkipPasvIp: boolean read fFtpSkipPasvIp write SetFtpSkipPasvIp;

published
  property ConnectTimeout: LongInt read fConnectTimeout write SetConnectTimeout;
  property CookieFile: string read GetCookieFile write SetCookieFile;
  property FailOnError: boolean read fFailOnError write SetFailOnError;
  property InputFile: string read GetInputFile write SetInputFile;
  property OutputFile: string read GetOutputFile write SetOutputFile; 
  property PostFields: string read GetPostFields write SetPostFields;
  property Proxy: string read GetProxy write SetProxy;
  property Referer: string read GetReferer write SetReferer;
  property URL: string read GetUrl write SetUrl;
  property Upload: boolean read fUpload write SetUpload;
  property UserAgent: string read GetUserAgent write SetUserAgent;
  property UserPwd: string read GetUserPwd write SetUserPwd;

  property OnDebug: tCurlDebugEvent read fOnDebug write SetOnDebug;
  property OnHeader: tCurlHeaderEvent read fHeaderScheme.hdr_event write SetOnHeader;
  property OnProgress: tCurlProgressEvent read fOnProgress write SetOnProgress;
  property OnReceive: tCurlReceiveEvent read fReceiveScheme.rx_event write SetOnReceive;
  property OnTransmit: tCurlTransmitEvent read fTransmitScheme.tx_event write SetOnTransmit;
  property OnListCookies: tCurlListCookiesEvent read fOnListCookies write fOnListCookies;
