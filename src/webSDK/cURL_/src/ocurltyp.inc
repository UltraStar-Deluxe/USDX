(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

{$IFDEF CURLOBJ_INTERFACE}

{$IFDEF WIN32} 
  type 
    pIOFile=pointer;
    tThreadStartProc = procedure(arg:pointer); cdecl;
  const
    MSVCRT_DLL = 'msvcrt.dll';
  var
    DEFAULT_WIN32_CA_CERT: string = '';
    DEFAULT_WIN32_CA_PATH: string = '';

  function fopen ( path, mode: pChar ): pIOFile; cdecl; external MSVCRT_DLL name 'fopen';
  function fclose ( f:pIOFile ): LongInt; cdecl; external MSVCRT_DLL name 'fclose';
  function fread ( ptr:pointer; size:LongInt; nmemb:LongInt; f:pIOFile): size_t; cdecl; external MSVCRT_DLL name 'fread';
  function fwrite ( const ptr:pointer; size:LongInt; nmemb:LongInt; f: pIOFile): size_t; cdecl; external MSVCRT_DLL name 'fwrite';
  function fdopen (fildes:LongInt; mode:pChar ): pIOFile; cdecl; external MSVCRT_DLL name '_fdopen'; 
  function malloc(n:LongInt):pointer; cdecl; external MSVCRT_DLL name 'malloc';
  procedure _free(p:pointer); cdecl; external MSVCRT_DLL name 'free';
  function _beginthread( func:tThreadStartProc; stack:DWORD; arg:pointer ):DWORD; cdecl; external MSVCRT_DLL name '_beginthread';
  procedure _endthread(); cdecl; external MSVCRT_DLL name '_endthread';
  function getenv(varname:pChar):pChar; cdecl; external MSVCRT_DLL name 'getenv';
{$ELSE WIN32}
  procedure _free(p:pointer); cdecl; external 'libc.so' name 'free';
{$ENDIF WIN32}

{$IFNDEF CURL_COMPONENT}
type 
  tComponent = class(tObject) // lightweight "psuedo-component" for console apps
  private
    fOwner:TComponent;
    fTag:LongInt;
  public 
    property Owner:TComponent read fOwner write fOwner;
    constructor Create(aOwner:tComponent);
  published 
    property Tag:LongInt read fTag write fTag;
  end;
  tNotifyEvent = procedure(obj:TObject) of object;
{$ENDIF CURL_COMPONENT}

type 
  tCurlProgressEvent = procedure ( Sender:tObject; BytesTotal, BytesNow:integer; var bContinue:Boolean ) of object;
  tCurlHeaderEvent = procedure ( Sender:tObject; data:string; var bContinue:Boolean ) of object;
  tCurlReceiveEvent = procedure ( Sender:tObject; data:PChar; len:Cardinal; var bContinue:Boolean ) of object;
  tCurlTransmitEvent = procedure ( Sender:tObject; data:PChar; var len:Cardinal ) of object;
  tCurlDebugEvent = procedure ( Sender:tObject; infotype:curl_infotype; data:PChar; len:Cardinal;  var bContinue:Boolean ) of object;
  tCurlWaitCallback = procedure ( UserData:pointer ); cdecl;
  tCurlListCookiesEvent = tCurlHeaderEvent;


  tCurlPostType = ( POST_TYPE_PLAIN, POST_TYPE_ATTACHMENT, POST_TYPE_FILEDATA );
  CurlEncoding = ( CURL_ENCODING_NONE, CURL_ENCODING_IDENTITY, CURL_ENCODING_DEFLATE );
  CurlCertType = ( CURL_CERT_NONE, CURL_CERT_PEM, CURL_CERT_DER );
  CurlKeyType =  ( CURL_KEY_NONE, CURL_KEY_PEM, CURL_KEY_DER, CURL_KEY_ENG );
  CurlHostVerify = ( CURL_VERIFY_NONE, CURL_VERIFY_EXIST, CURL_VERIFY_MATCH );
  CurlAuthenticationMethod = ( AUTH_BASIC, AUTH_DIGEST,  AUTH_GSSNEGOTIATE, AUTH_NTLM );
  CurlAuthenticationMethods = set of CurlAuthenticationMethod;
  CurlResolverVersion = LongInt;


  tCallbackType = (CBT_CALLBACK, CBT_EVENT, CBT_INTERNAL);
  tFileStreamType = (FST_STREAM, FST_FILENAME, FST_INTERNAL);

  tCallbackScheme = record
    cb_type:tCallbackType;
    fs_type:tFilestreamType;
    stream:pointer;
    filename:pChar;
    callback:curl_write_callback;
    case integer of
      0: ( hdr_event: tCurlHeaderEvent   );
      1: ( rx_event:  tCurlReceiveEvent  );
      2: ( tx_event:  tCurlTransmitEvent );
  end;

  tCurlFeatures = record
    Ipv6:         boolean;
    Kerberos4:    boolean;
    Ssl:          boolean;
    Libz:         boolean;
    Ntlm:         boolean;
    GssNegotiate: boolean;
    Debug:        boolean;
    AsynchDns:    boolean;
    Spnego:       boolean;
    LargeFile:    boolean;
    Idn:          boolean;
    Sspi:         boolean;
  end;  



{ "Helper" class to make the multipart/formdata stuff a little easier ... }
  tMultiPartFormData = class
  private 
    fHttpPost:pCurl_HttpPost;
    fErrCODE:CurlCode;
    procedure DoAppend(var aPost:pCurl_HttpPost; const PostInfo:curl_HttpPost);  
    procedure DoClear(var aPost:pCurl_HttpPost);
  public
    property PostPtr:pCurl_HttpPost read fHttpPost;
    constructor Create;
    destructor Destroy; override;
    procedure Append(const PostInfo:Curl_HttpPost);
    procedure Add(const aName:string; const aContents:string; const aContentType:string; aPostType:tCurlPostType);
    procedure Clear;
    property Result:CurlCODE read fErrCode;
  end;



{ A new class to provide a wrapper around the tcurl_slist structure ... }
  tCurlROList = class(tObject) // Read-only list
    private
      fList:pcurl_slist;
      fCount:LongInt;
      fDirty:boolean;
    protected
      function GetText:string;
      procedure SetText(const aStr:string);
      function GetItemPtr(n:LongInt):pcurl_slist;
      function GetItems(n:LongInt):string;
      procedure SetItems(n:LongInt; const s:string);
      procedure Add(const aStr:string);
      procedure Delete(n:LongInt);
      procedure Clear;
    public
      constructor Create;
      destructor Destroy; override;
      property Text:string read GetText;
      property Items[index:integer]:string read GetItems; default;
      property Count:LongInt read fCount;
  end;

  tCurlRWList = class(tCurlROList) // Read-Write list
    public
      procedure Add(const aStr:string);
      procedure Delete(n:LongInt);
      procedure Clear;
      property Text:string read GetText write SetText;
      property Items[index:integer]:string read GetItems write SetItems; default;
  end;

  { Wrapper  around libcurl's cookie list }
  tCurlEnumCookiesFunction = function(Sender:tObject; const Cookie:ansistring; UserData:pointer):boolean;
  tCurlStaleCookieState = set of ( COOKIES_LIB_STALE, COOKIES_OBJ_STALE );

  tCurlCookieList = class(tObject)
  private
    fOwner:tObject;
    fHandle:pCurl;
    fList:pcurl_slist;
    fState:tCurlStaleCookieState;
    function SendListToLib:CurlCode;
    function GetListFromLib:CurlCode;
    procedure RemoveDuplicates;
  public 
    constructor Create(aOwner:tObject);
    destructor Destroy; override;
    function AddFromFile(const filename:ansistring):CurlCode;
    function SaveToFile(const filename:ansistring):CurlCode;
    function Add(const cookie:ansistring):boolean;
    procedure ForEachCookie(callback:tCurlEnumCookiesFunction; user_data:pointer);
    procedure Clear;
  end;


{$ENDIF CURLOBJ_INTERFACE}


