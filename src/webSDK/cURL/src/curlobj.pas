(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

{$IFNDEF LAZARUS}
unit curlobj;
{$ENDIF}

interface

{$DEFINE CURLOBJ_INTERFACE}
  {$INCLUDE ocurluse.inc} // <- USES clause and global IFDEF's 
  {$INCLUDE ocurlacc.inc} // <- Convenience functions for file-handling ( exist / access / size )
  {$INCLUDE ocurltyp.inc} // <- Type declarations ( and Win32 MSVCRT ansi-c functions )
{$UNDEF CURLOBJ_INTERFACE}

type
  TCurlBase = class ( TComponent ) // Base class for TCurl and TCurlMulti
  private
    fBusy:boolean;
    fThreaded:boolean;
    fWaitInterval:LongInt;
    fOnWait:tNotifyEvent;
    fWaitCallback:tCurlWaitCallback;
    fWaitData:pointer;
    fThread:DWORD;
  protected
    procedure SetWaitInterval(ms:Longint);
    procedure SetOnWait(aEvent:tNotifyEvent);
    procedure SetWaitCallback(aCallback:tCurlWaitCallback);
  public
    constructor Create(aOwner:tComponent); {$IFDEF CURL_COMPONENT}override;{$ENDIF}
    property Busy:boolean read fBusy;
    property WaitCallback:tCurlWaitCallback read fWaitCallback write SetWaitCallback;
    property WaitData:pointer read fWaitData write fWaitData;
    property WaitInterval:LongInt read fWaitInterval write SetWaitInterval;
  published
    property OnWait:tNotifyEvent read fOnWait write SetOnWait;
    property Threaded:boolean read fThreaded write fThreaded;
  end;


type 
  TCurl = class ( TCurlBase )
    {$DEFINE TCURL_INTF}
      {$INCLUDE ocurlprv.inc}  //  <- Private fields
      {$INCLUDE ocurlprt.inc}  //  <- Interface to protected methods
      {$INCLUDE ocurlinf.inc}  //  <- Interface to curl_easy_getinfo()
      {$INCLUDE ocurlver.inc}  //  <- Interface to curl_version_info()
      {$INCLUDE ocurlpub.inc}  //  <- Public and published properties
    {$UNDEF TCURL_INTF}
  public
    constructor Create(aOwner:tComponent); {$IFDEF CURL_COMPONENT}override;{$ENDIF}
    destructor Destroy; override;
    function Perform: boolean;
    procedure Clear;
    procedure ListCookies;
    class function Escape(const s:string):string;
    class function UnEscape(const s:string):string;    

  end;


{$DEFINE MULTI_INTERFACE}
  {$INCLUDE ocurlmlt.inc} // <- tCurlMulti interface
{$UNDEF MULTI_INTERFACE}


{$IFNDEF CURL_COMPONENT}
  implementation
{$ELSE}
  procedure Register;
  implementation
  procedure Register;
  begin
    RegisterComponents('Internet', [TCurl]);
  end;
{$ENDIF}


{$DEFINE CURLOBJ_IMPLEMENTATION}
  {$INCLUDE ocurlacc.inc}
  {$INCLUDE ocurlcll.inc}
  {$INCLUDE ocurlmlt.inc}
  {$INCLUDE ocurlver.inc}
  {$INCLUDE ocurlinf.inc} // <- curl_easy_getinfo()
{$UNDEF CURLOBJ_IMPLEMENTATION}

{$INCLUDE ocurlcb.inc} // <- I/O callbacks
{$INCLUDE ocurlcb2.inc} // <- Other callbacks
{$INCLUDE ocurlset.inc} // <- "SET" routines
{$INCLUDE ocurlget.inc} // <- "GET" routines
{$INCLUDE ocurlini.inc} // <- Intitialize/release private fields

{$IFNDEF CURL_COMPONENT}
constructor tComponent.Create(aOwner:tComponent);
begin
  inherited Create;
  fOwner:=aOwner;
  fTag:=0;
end;
{$ENDIF}

constructor tCurl.Create(aOwner:tComponent);
begin
  inherited Create(aOwner);
  fCurl:=curl_easy_init;
  if ( fCurl = nil ) then begin
    fCurlResult:=CURLE_FAILED_INIT;
    Self.Destroy;
    Self:=nil;
    FAIL;
  end else fCurlResult:=CURLE_OK;
  InitFields();
  fCookieList:=tCurlCookieList.Create(self);
  fMultiNotifyDestroying:=nil;
  fMulti:=nil;
  fPrev:=nil;
  fNext:=nil;
end;


destructor tCurl.Destroy;
begin
  if ( {$IFNDEF FPC}@{$ENDIF}fMultiNotifyDestroying <> nil ) then fMultiNotifyDestroying(self);
  if ( fCookieList <> nil ) then fCookieList.Free;
  if ( fCurl <> nil ) then curl_easy_cleanup( fCurl );
  Release;
  inherited Destroy;
end;


function tCurl.Perform:boolean;
begin
  InitTransfer;
  if ( fCurlResult = CURLE_OK ) then begin
    fCurlResult:=DoPerform(fCurl);
  end;
  Result:=( fCurlResult = CURLE_OK );
  DoneTransfer(fCurlResult);
end;


procedure tCurl.Clear;
begin
  if ( COOKIES_OBJ_STALE in fCookieList.fState ) then fCookieList.SendListToLib;
  curl_easy_reset(fCurl);
  Release();
  InitFields();
end;


class function tCurl.Escape(const s:string):string;
var
  tmp:pChar;
begin
  tmp:=curl_escape(pChar(s), Length(s));
  Result:=tmp;
  UniqueString(Result);
  curl_free(tmp);
end;

class function tCurl.Unescape(const s:string):string;    
var
  tmp:pChar;
begin
  tmp:=curl_unescape(pChar(s), Length(s));
  Result:=tmp;
  UniqueString(Result);
  curl_free(tmp);
end;


{$IFDEF WIN32}
procedure InitWin32CACert;
var
  buflen:DWORD;
  buf:pChar;
  p:pChar;
begin
  if tCurl.Features.Ssl then begin
    GetMem(buf, MAX_PATH+1);
    FillChar(buf^, MAX_PATH+1, #0);
    buflen:=SearchPathA(nil, 'curl-ca-bundle.crt', nil, MAX_PATH+2, buf, {$IFDEF FPC}@{$ENDIF}p);
    if ( buflen > 0 ) then begin
      DEFAULT_WIN32_CA_CERT:=buf;
      UniqueString(DEFAULT_WIN32_CA_CERT);
      if ( p <> nil ) then begin
        p[0]:=#0;
        DEFAULT_WIN32_CA_PATH:=buf;
        UniqueString(DEFAULT_WIN32_CA_PATH);
      end;
    end;
    FreeMem(buf);
  end;
end;
{$ENDIF}


{$INCLUDE ocurlthd.inc}


initialization
  {$IFDEF LAZARUS}
    {$I curlpas.lrs}
  {$ENDIF}
  curl_global_init(CURL_GLOBAL_ALL);
  GlobalProtocolList:=InitProtocolList;
  GlobalVersionInfoData:=curl_version_info(CURLVERSION_NOW)^;
  InitFeatures;
  {$IFDEF WIN32}
  InitWin32CACert();
  {$ENDIF}
finalization
  GlobalProtocolList.Free;
  curl_global_cleanup;
end.
