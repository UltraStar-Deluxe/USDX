(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

{$IFDEF TCURL_INTF}
    class function Protocols:tCurlROList;
    class function VersionInfo:curl_version_info_data;
    class function LibraryVersion: string;
    class function VersionNumber:LongWord;
    class function Machine:string;
    class function Features: tCurlFeatures;
    class function SslVersionString: string;
    class function SslVersionNumber: LongInt;
    class function LibzVersion: string;
    class function CurlVersion:string;
    class function AresVersionString:string;
    class function AresVersionNumber:LongInt;
    class function LibIdnVersion:string;
{$ENDIF TCURL_INTF}

{$IFDEF CURLOBJ_IMPLEMENTATION}

var 
  GlobalProtoColList:tCurlROList;
  GlobalVersionInfoData: curl_version_info_data;
  GlobalFeatures:tCurlFeatures;


class function tCurl.LibraryVersion: string;
begin
  Result:=StrPas(GlobalVersionInfoData.version);
end;

class function tCurl.VersionNumber:LongWord;
begin
  Result:=GlobalVersionInfoData.version_num;
end;

class function tCurl.Machine:string;
begin
  Result:=StrPas(GlobalVersionInfoData.host);
end;

class function tCurl.Features: tCurlFeatures;
begin
  Result:=GlobalFeatures;
end;

class function tCurl.SslVersionString: string;
begin
  Result:=StrPas(GlobalVersionInfoData.ssl_version);
  // for some reason this string has a leading blank...
  while ( Result <> '' ) and ( Result[1] = #32 ) do delete(Result, 1, 1);
end;

class function tCurl.SslVersionNumber: LongInt;
begin
  Result:=GlobalVersionInfoData.ssl_version_num;
end;

class function tCurl.LibzVersion: string;
begin
  Result:=StrPas(GlobalVersionInfoData.libz_version);
end;


class function tCurl.Protocols:tCurlROList;
begin
  Result:=GlobalProtocolList;
end;

class function tCurl.VersionInfo:curl_version_info_data;
begin
  Result:=GlobalVersionInfoData;
end;

class function tCurl.CurlVersion:string;
begin
  Result:=StrPas(curl_version);
end;

class function tCurl.AresVersionString:string;
begin
  Result:=StrPas(GlobalVersionInfoData.ares);
end;

class function tCurl.AresVersionNumber:LongInt;
begin
  Result:=GlobalVersionInfoData.ares_num;
end;

class function tCurl.LibIdnVersion:string;
begin
  Result:=StrPas(GlobalVersionInfoData.libidn);
end;


procedure InitFeatures;
begin
  with GlobalFeatures,GlobalVersionInfoData do begin
    Ipv6         := ( Features and CURL_VERSION_IPV6 ) > 0;
    Kerberos4    := ( Features and CURL_VERSION_KERBEROS4 ) > 0;
    Ssl          := ( Features and CURL_VERSION_SSL ) > 0;
    Libz         := ( Features and CURL_VERSION_LIBZ ) > 0;
    Ntlm         := ( Features and CURL_VERSION_NTLM ) > 0;
    GssNegotiate := ( Features and CURL_VERSION_GSSNEGOTIATE ) > 0;
    Debug        := ( Features and CURL_VERSION_DEBUG ) > 0;
    AsynchDns    := ( Features and CURL_VERSION_ASYNCHDNS ) > 0;
    Spnego       := ( Features and CURL_VERSION_SPNEGO ) > 0;
    LargeFile    := ( Features and CURL_VERSION_LARGEFILE ) > 0;
    Idn          := ( Features and CURL_VERSION_IDN ) > 0;
    Sspi         := ( Features and CURL_VERSION_SSPI ) > 0;
  end;
end;

function InitProtocolList:tCurlROList;
var 
  pp:ppChar;
begin
  Result:=tCurlROList.Create;
  pp:=curl_version_info(CURLVERSION_NOW)^.protocols;
  repeat 
    Result.Add(pp^);
    inc(pp);
  until (pp^=nil);
end;

{$ENDIF CURLOBJ_IMPLEMENTATION}
