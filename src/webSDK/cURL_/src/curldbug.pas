(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

unit curldbug; 

// This unit just returns string representations of the enumerated types.
// It is intended for debugging purposes only - NOT for production use!

interface
{$IFDEF FPC}{$MODE OBJFPC}{$ENDIF}

uses curlobj, curl_h; 

(*
    Using the "curl_h"  and "curlobj" units together is NOT recommended. 
    It can cause some strange-looking compile-time errors, like:
    -->>  Type mismatch: got "CurlCode", expected "CurlCode"  <<-- huh???
    This is because the types are defined in both units.
    So I had to jump through a few hoops to get this unit to
    work with either curlobj or curl_h. 
*)



type 
  curl_h_CurlCode =     curl_h.CurlCode;
  curl_h_CurlFormCode = curl_h.CurlFormCode;
  curl_h_CurlInfoType = curl_h.curl_infotype;
  curl_h_CurlInfo =     curl_h.CurlInfo;
  curl_h_CurlOption =   curl_h.CurlOption;
    
  function CurlHdrCodeStr( aCode:curl_h_CurlCode ):string;
  function CurlHdrFormCodeStr( aCode:curl_h_CurlFormCode ):string;
  function CurlHdrInfoTypeStr( info:curl_h_CurlInfoType ):string;
  function CurlHdrInfoStr( info:curl_h_CurlInfo ):string;
  function CurlHdrOptionStr( aOption:curl_h_CurlOption ):string;

type
  curlobj_CurlCode =     curlobj.CurlCode;
  curlobj_CurlFormCode = curlobj.CurlFormCode;
  curlobj_CurlInfoType = curlobj.curl_infotype;
  curlobj_CurlInfo =     curlobj.CurlInfo;
  curlobj_CurlOption =   curlobj.CurlOption;

  function CurlObjCodeStr( aCode:curlobj_CurlCode ):string;
  function CurlObjFormCodeStr( aCode:curlobj_CurlFormCode ):string;
  function CurlObjInfoTypeStr( info:curlobj_CurlInfoType ):string;
  function CurlObjInfoStr( info:curlobj_CurlInfo ):string;
  function CurlObjOptionStr( aOption: curlobj_CurlOption ):string;


  function CurlMCodeStr( aCode:LongInt ):string; overload;


implementation


function CurlHdrCodeStr( aCode:curl_h_CurlCode ):string;
begin
  case aCode of 
    CURLE_OK:                          Result:='CURLE_OK';
    CURLE_UNSUPPORTED_PROTOCOL:        Result:='CURLE_UNSUPPORTED_PROTOCOL';
    CURLE_FAILED_INIT:                 Result:='CURLE_FAILED_INIT';
    CURLE_URL_MALFORMAT:               Result:='CURLE_URL_MALFORMAT';
    CURLE_URL_MALFORMAT_USER:          Result:='CURLE_URL_MALFORMAT_USER';
    CURLE_COULDNT_RESOLVE_PROXY:       Result:='CURLE_COULDNT_RESOLVE_PROXY';
    CURLE_COULDNT_RESOLVE_HOST:        Result:='CURLE_COULDNT_RESOLVE_HOST';
    CURLE_COULDNT_CONNECT:             Result:='CURLE_COULDNT_CONNECT';
    CURLE_FTP_WEIRD_SERVER_REPLY:      Result:='CURLE_FTP_WEIRD_SERVER_REPLY';
    CURLE_FTP_ACCESS_DENIED:           Result:='CURLE_FTP_ACCESS_DENIED';
    CURLE_FTP_USER_PASSWORD_INCORRECT: Result:='CURLE_FTP_USER_PASSWORD_INCORRECT';
    CURLE_FTP_WEIRD_PASS_REPLY:        Result:='CURLE_FTP_WEIRD_PASS_REPLY';
    CURLE_FTP_WEIRD_USER_REPLY:        Result:='CURLE_FTP_WEIRD_USER_REPLY';
    CURLE_FTP_WEIRD_PASV_REPLY:        Result:='CURLE_FTP_WEIRD_PASV_REPLY';
    CURLE_FTP_WEIRD_227_FORMAT:        Result:='CURLE_FTP_WEIRD_227_FORMAT';
    CURLE_FTP_CANT_GET_HOST:           Result:='CURLE_FTP_CANT_GET_HOST';
    CURLE_FTP_CANT_RECONNECT:          Result:='CURLE_FTP_CANT_RECONNECT';
    CURLE_FTP_COULDNT_SET_BINARY:      Result:='CURLE_FTP_COULDNT_SET_BINARY';
    CURLE_PARTIAL_FILE:                Result:='CURLE_PARTIAL_FILE';
    CURLE_FTP_COULDNT_RETR_FILE:       Result:='CURLE_FTP_COULDNT_RETR_FILE';
    CURLE_FTP_WRITE_ERROR:             Result:='CURLE_FTP_WRITE_ERROR';
    CURLE_FTP_QUOTE_ERROR:             Result:='CURLE_FTP_QUOTE_ERROR';
    CURLE_WRITE_ERROR:                 Result:='CURLE_WRITE_ERROR';
    CURLE_MALFORMAT_USER:              Result:='CURLE_MALFORMAT_USER';
    CURLE_FTP_COULDNT_STOR_FILE:       Result:='CURLE_FTP_COULDNT_STOR_FILE';
    CURLE_READ_ERROR:                  Result:='CURLE_READ_ERROR';
    CURLE_OUT_OF_MEMORY:               Result:='CURLE_OUT_OF_MEMORY';
    CURLE_OPERATION_TIMEOUTED:         Result:='CURLE_OPERATION_TIMEOUTED';
    CURLE_FTP_COULDNT_SET_ASCII:       Result:='CURLE_FTP_COULDNT_SET_ASCII';
    CURLE_FTP_PORT_FAILED:             Result:='CURLE_FTP_PORT_FAILED';
    CURLE_FTP_COULDNT_USE_REST:        Result:='CURLE_FTP_COULDNT_USE_REST';
    CURLE_FTP_COULDNT_GET_SIZE:        Result:='CURLE_FTP_COULDNT_GET_SIZE';
    CURLE_HTTP_RANGE_ERROR:            Result:='CURLE_HTTP_RANGE_ERROR';
    CURLE_HTTP_POST_ERROR:             Result:='CURLE_HTTP_POST_ERROR';
    CURLE_SSL_CONNECT_ERROR:           Result:='CURLE_SSL_CONNECT_ERROR';
    CURLE_BAD_DOWNLOAD_RESUME:         Result:='CURLE_BAD_DOWNLOAD_RESUME';
    CURLE_FILE_COULDNT_READ_FILE:      Result:='CURLE_FILE_COULDNT_READ_FILE';
    CURLE_LDAP_CANNOT_BIND:            Result:='CURLE_LDAP_CANNOT_BIND';
    CURLE_LDAP_SEARCH_FAILED:          Result:='CURLE_LDAP_SEARCH_FAILED';
    CURLE_LIBRARY_NOT_FOUND:           Result:='CURLE_LIBRARY_NOT_FOUND';
    CURLE_FUNCTION_NOT_FOUND:          Result:='CURLE_FUNCTION_NOT_FOUND';
    CURLE_ABORTED_BY_CALLBACK:         Result:='CURLE_ABORTED_BY_CALLBACK';
    CURLE_BAD_FUNCTION_ARGUMENT:       Result:='CURLE_BAD_FUNCTION_ARGUMENT';
    CURLE_BAD_CALLING_ORDER:           Result:='CURLE_BAD_CALLING_ORDER';
    CURLE_BAD_PASSWORD_ENTERED:        Result:='CURLE_BAD_PASSWORD_ENTERED';
    CURLE_TOO_MANY_REDIRECTS:          Result:='CURLE_TOO_MANY_REDIRECTS';
    CURLE_UNKNOWN_TELNET_OPTION:       Result:='CURLE_UNKNOWN_TELNET_OPTION';
    CURLE_TELNET_OPTION_SYNTAX:        Result:='CURLE_TELNET_OPTION_SYNTAX';
    CURLE_OBSOLETE:                    Result:='CURLE_OBSOLETE';
    CURLE_SSL_PEER_CERTIFICATE:        Result:='CURLE_SSL_PEER_CERTIFICATE';
    CURLE_GOT_NOTHING:                 Result:='CURLE_GOT_NOTHING';
    CURLE_SSL_ENGINE_NOTFOUND:         Result:='CURLE_SSL_ENGINE_NOTFOUND';
    CURLE_SSL_ENGINE_SETFAILED:        Result:='CURLE_SSL_ENGINE_SETFAILED';
    CURLE_SEND_ERROR:                  Result:='CURLE_SEND_ERROR';
    CURLE_RECV_ERROR:                  Result:='CURLE_RECV_ERROR';
    CURLE_SHARE_IN_USE:                Result:='CURLE_SHARE_IN_USE';
    CURLE_SSL_CERTPROBLEM:             Result:='CURLE_SSL_CERTPROBLEM';
    CURLE_SSL_CIPHER:                  Result:='CURLE_SSL_CIPHER';
    CURLE_SSL_CACERT:                  Result:='CURLE_SSL_CACERT';
    CURLE_BAD_CONTENT_ENCODING:        Result:='CURLE_BAD_CONTENT_ENCODING';
    CURL_LAST:                         Result:='CURL_LAST';
    else Result:='Invalid CurlCode';
  end;
end;

function CurlHdrFormCodeStr( aCode:curl_h_CurlFormCode ):string;
begin
  case aCode of 
    CURL_FORMADD_OK:             Result:='CURL_FORMADD_OK';
    CURL_FORMADD_MEMORY:         Result:='CURL_FORMADD_MEMORY';
    CURL_FORMADD_OPTION_TWICE:   Result:='CURL_FORMADD_OPTION_TWICE';
    CURL_FORMADD_NULL:           Result:='CURL_FORMADD_NULL';
    CURL_FORMADD_UNKNOWN_OPTION: Result:='CURL_FORMADD_UNKNOWN_OPTION';
    CURL_FORMADD_INCOMPLETE:     Result:='CURL_FORMADD_INCOMPLETE';
    CURL_FORMADD_ILLEGAL_ARRAY:  Result:='CURL_FORMADD_ILLEGAL_ARRAY';
    CURL_FORMADD_LAST:           Result:='CURL_FORMADD_LAST';
    else Result:='Invalid CurlFormCode';
  end;
end;

function CurlMCodeStr( aCode:LongInt ):string;
begin
  case aCode of
    CURLM_CALL_MULTI_PERFORM: Result:='CURLM_CALL_MULTI_PERFORM';     
    CURLM_OK:                 Result:='CURLM_OK';                     
    CURLM_BAD_HANDLE:         Result:='CURLM_BAD_HANDLE';             
    CURLM_BAD_EASY_HANDLE:    Result:='CURLM_BAD_EASY_HANDLE';        
    CURLM_OUT_OF_MEMORY:      Result:='CURLM_OUT_OF_MEMORY';          
    CURLM_INTERNAL_ERROR:     Result:='CURLM_INTERNAL_ERROR';         
    CURLM_LAST:               Result:='CURLM_LAST';                   
    else Result:='Invalid CurlMCode';
  end;
end;

function CurlHdrInfoTypeStr( info:curl_h_CurlInfoType ):string;
begin
  case info of
    CURLINFO_TEXT:       Result:='CURLINFO_TEXT';
    CURLINFO_HEADER_IN:  Result:='CURLINFO_HEADER_IN';
    CURLINFO_HEADER_OUT: Result:='CURLINFO_HEADER_OUT';
    CURLINFO_DATA_IN:    Result:='CURLINFO_DATA_IN';
    CURLINFO_DATA_OUT:   Result:='CURLINFO_DATA_OUT';
    CURLINFO_END:        Result:='CURLINFO_END';
    else Result:='Invalid CurlInfoType';
  end;
end;

function CurlHdrOptionStr( aOption:curl_h_CurlOption ):string;
begin
  case aOption of
    CURLOPT_PORT:                 Result:='CURLOPT_PORT';
    CURLOPT_TIMEOUT:              Result:='CURLOPT_TIMEOUT';
    CURLOPT_INFILESIZE:           Result:='CURLOPT_INFILESIZE';
    CURLOPT_LOW_SPEED_LIMIT:      Result:='CURLOPT_LOW_SPEED_LIMIT';
    CURLOPT_LOW_SPEED_TIME:       Result:='CURLOPT_LOW_SPEED_TIME';
    CURLOPT_RESUME_FROM:          Result:='CURLOPT_RESUME_FROM';
    CURLOPT_CRLF:                 Result:='CURLOPT_CRLF';
    CURLOPT_SSLVERSION:           Result:='CURLOPT_SSLVERSION';
    CURLOPT_TIMECONDITION:        Result:='CURLOPT_TIMECONDITION';
    CURLOPT_TIMEVALUE:            Result:='CURLOPT_TIMEVALUE';
    CURLOPT_VERBOSE:              Result:='CURLOPT_VERBOSE';
    CURLOPT_HEADER:               Result:='CURLOPT_HEADER';
    CURLOPT_NOPROGRESS:           Result:='CURLOPT_NOPROGRESS';
    CURLOPT_NOBODY:               Result:='CURLOPT_NOBODY';
    CURLOPT_FAILONERROR:          Result:='CURLOPT_FAILONERROR';
    CURLOPT_UPLOAD:               Result:='CURLOPT_UPLOAD';
    CURLOPT_POST:                 Result:='CURLOPT_POST';
    CURLOPT_FTPLISTONLY:          Result:='CURLOPT_FTPLISTONLY';
    CURLOPT_FTPAPPEND:            Result:='CURLOPT_FTPAPPEND';
    CURLOPT_NETRC:                Result:='CURLOPT_NETRC';
    CURLOPT_FOLLOWLOCATION:       Result:='CURLOPT_FOLLOWLOCATION';
    CURLOPT_TRANSFERTEXT:         Result:='CURLOPT_TRANSFERTEXT';
    CURLOPT_PUT:                  Result:='CURLOPT_PUT';
    CURLOPT_AUTOREFERER:          Result:='CURLOPT_AUTOREFERER';
    CURLOPT_PROXYPORT:            Result:='CURLOPT_PROXYPORT';
    CURLOPT_POSTFIELDSIZE:        Result:='CURLOPT_POSTFIELDSIZE';
    CURLOPT_HTTPPROXYTUNNEL:      Result:='CURLOPT_HTTPPROXYTUNNEL';
    CURLOPT_SSL_VERIFYPEER:       Result:='CURLOPT_SSL_VERIFYPEER';
    CURLOPT_MAXREDIRS:            Result:='CURLOPT_MAXREDIRS';
    CURLOPT_MAXCONNECTS:          Result:='CURLOPT_MAXCONNECTS';
    CURLOPT_CLOSEPOLICY:          Result:='CURLOPT_CLOSEPOLICY';
    CURLOPT_FRESH_CONNECT:        Result:='CURLOPT_FRESH_CONNECT';
    CURLOPT_FORBID_REUSE:         Result:='CURLOPT_FORBID_REUSE';
    CURLOPT_CONNECTTIMEOUT:       Result:='CURLOPT_CONNECTTIMEOUT';
    CURLOPT_HTTPGET:              Result:='CURLOPT_HTTPGET';
    CURLOPT_SSL_VERIFYHOST:       Result:='CURLOPT_SSL_VERIFYHOST';
    CURLOPT_HTTP_VERSION:         Result:='CURLOPT_HTTP_VERSION';
    CURLOPT_FTP_USE_EPSV:         Result:='CURLOPT_FTP_USE_EPSV';
    CURLOPT_SSLENGINE_DEFAULT:    Result:='CURLOPT_SSLENGINE_DEFAULT';
    CURLOPT_DNS_USE_GLOBAL_CACHE: Result:='CURLOPT_DNS_USE_GLOBAL_CACHE';
    CURLOPT_DNS_CACHE_TIMEOUT:    Result:='CURLOPT_DNS_CACHE_TIMEOUT';
    CURLOPT_COOKIESESSION:        Result:='CURLOPT_COOKIESESSION';
    CURLOPT_BUFFERSIZE:           Result:='CURLOPT_BUFFERSIZE';
    CURLOPT_NOSIGNAL:             Result:='CURLOPT_NOSIGNAL';
    CURLOPT_PROXYTYPE:            Result:='CURLOPT_PROXYTYPE';
    CURLOPT_UNRESTRICTED_AUTH:    Result:='CURLOPT_UNRESTRICTED_AUTH';
    CURLOPT_FTP_USE_EPRT:         Result:='CURLOPT_FTP_USE_EPRT';
    CURLOPT_FILE:                 Result:='CURLOPT_FILE';
    CURLOPT_URL:                  Result:='CURLOPT_URL';
    CURLOPT_PROXY:                Result:='CURLOPT_PROXY';
    CURLOPT_USERPWD:              Result:='CURLOPT_USERPWD';
    CURLOPT_PROXYUSERPWD:         Result:='CURLOPT_PROXYUSERPWD';
    CURLOPT_RANGE:                Result:='CURLOPT_RANGE';
    CURLOPT_INFILE:               Result:='CURLOPT_INFILE';
    CURLOPT_ERRORBUFFER:          Result:='CURLOPT_ERRORBUFFER';
    CURLOPT_POSTFIELDS:           Result:='CURLOPT_POSTFIELDS';
    CURLOPT_REFERER:              Result:='CURLOPT_REFERER';
    CURLOPT_FTPPORT:              Result:='CURLOPT_FTPPORT';
    CURLOPT_USERAGENT:            Result:='CURLOPT_USERAGENT';
    CURLOPT_COOKIE:               Result:='CURLOPT_COOKIE';
    CURLOPT_HTTPHEADER:           Result:='CURLOPT_HTTPHEADER';
    CURLOPT_HTTPPOST:             Result:='CURLOPT_HTTPPOST';
    CURLOPT_SSLCERT:              Result:='CURLOPT_SSLCERT';
    CURLOPT_SSLCERTPASSWD:        Result:='CURLOPT_SSLCERTPASSWD';
    CURLOPT_QUOTE:                Result:='CURLOPT_QUOTE';
    CURLOPT_WRITEHEADER:          Result:='CURLOPT_WRITEHEADER';
    CURLOPT_COOKIEFILE:           Result:='CURLOPT_COOKIEFILE';
    CURLOPT_CUSTOMREQUEST:        Result:='CURLOPT_CUSTOMREQUEST';
    CURLOPT_STDERR:               Result:='CURLOPT_STDERR';
    CURLOPT_POSTQUOTE:            Result:='CURLOPT_POSTQUOTE';
    CURLOPT_WRITEINFO:            Result:='CURLOPT_WRITEINFO';
    CURLOPT_PROGRESSDATA:         Result:='CURLOPT_PROGRESSDATA';
    CURLOPT_INTERFACE:            Result:='CURLOPT_INTERFACE';
    CURLOPT_KRB4LEVEL:            Result:='CURLOPT_KRB4LEVEL';
    CURLOPT_CAINFO:               Result:='CURLOPT_CAINFO';
    CURLOPT_FILETIME:             Result:='CURLOPT_FILETIME';
    CURLOPT_TELNETOPTIONS:        Result:='CURLOPT_TELNETOPTIONS';
    CURLOPT_RANDOM_FILE:          Result:='CURLOPT_RANDOM_FILE';
    CURLOPT_EGDSOCKET:            Result:='CURLOPT_EGDSOCKET';
    CURLOPT_COOKIEJAR:            Result:='CURLOPT_COOKIEJAR';
    CURLOPT_SSL_CIPHER_LIST:      Result:='CURLOPT_SSL_CIPHER_LIST';
    CURLOPT_SSLCERTTYPE:          Result:='CURLOPT_SSLCERTTYPE';
    CURLOPT_SSLKEY:               Result:='CURLOPT_SSLKEY';
    CURLOPT_SSLKEYTYPE:           Result:='CURLOPT_SSLKEYTYPE';
    CURLOPT_SSLENGINE:            Result:='CURLOPT_SSLENGINE';
    CURLOPT_PREQUOTE:             Result:='CURLOPT_PREQUOTE';
    CURLOPT_DEBUGDATA:            Result:='CURLOPT_DEBUGDATA';
    CURLOPT_CAPATH:               Result:='CURLOPT_CAPATH';
    CURLOPT_SHARE:                Result:='CURLOPT_SHARE';
    CURLOPT_ENCODING:             Result:='CURLOPT_ENCODING';
    CURLOPT_PRIVATE:              Result:='CURLOPT_PRIVATE';
    CURLOPT_HTTP200ALIASES:       Result:='CURLOPT_HTTP200ALIASES';
    CURLOPT_LASTENTRY:            Result:='CURLOPT_LASTENTRY';
    CURLOPT_WRITEFUNCTION:        Result:='CURLOPT_WRITEFUNCTION';
    CURLOPT_READFUNCTION:         Result:='CURLOPT_READFUNCTION';
    CURLOPT_PROGRESSFUNCTION:     Result:='CURLOPT_PROGRESSFUNCTION';
    CURLOPT_HEADERFUNCTION:       Result:='CURLOPT_HEADERFUNCTION';
    CURLOPT_DEBUGFUNCTION:        Result:='CURLOPT_DEBUGFUNCTION';
    else Result:='Invalid CurlOption';
  end;
end;

function CurlHdrInfoStr( info:curl_h_CurlInfo ):string;
begin
  case info of
    CURLINFO_NONE:                    Result:='CURLINFO_NONE';
    CURLINFO_LASTONE:                 Result:='CURLINFO_LASTONE';
    CURLINFO_EFFECTIVE_URL:           Result:='CURLINFO_EFFECTIVE_URL';
    CURLINFO_CONTENT_TYPE:            Result:='CURLINFO_CONTENT_TYPE';
    CURLINFO_PRIVATE:                 Result:='CURLINFO_PRIVATE';
    CURLINFO_HTTP_CODE:               Result:='CURLINFO_HTTP_CODE';
    CURLINFO_HEADER_SIZE:             Result:='CURLINFO_HEADER_SIZE';
    CURLINFO_REQUEST_SIZE:            Result:='CURLINFO_REQUEST_SIZE';
    CURLINFO_SSL_VERIFYRESULT:        Result:='CURLINFO_SSL_VERIFYRESULT';
    CURLINFO_FILETIME:                Result:='CURLINFO_FILETIME';
    CURLINFO_REDIRECT_COUNT:          Result:='CURLINFO_REDIRECT_COUNT';
    CURLINFO_TOTAL_TIME:              Result:='CURLINFO_TOTAL_TIME';
    CURLINFO_NAMELOOKUP_TIME:         Result:='CURLINFO_NAMELOOKUP_TIME';
    CURLINFO_CONNECT_TIME:            Result:='CURLINFO_CONNECT_TIME';
    CURLINFO_PRETRANSFER_TIME:        Result:='CURLINFO_PRETRANSFER_TIME';
    CURLINFO_SIZE_UPLOAD:             Result:='CURLINFO_SIZE_UPLOAD';
    CURLINFO_SIZE_DOWNLOAD:           Result:='CURLINFO_SIZE_DOWNLOAD';
    CURLINFO_SPEED_DOWNLOAD:          Result:='CURLINFO_SPEED_DOWNLOAD';
    CURLINFO_SPEED_UPLOAD:            Result:='CURLINFO_SPEED_UPLOAD';
    CURLINFO_CONTENT_LENGTH_DOWNLOAD: Result:='CURLINFO_CONTENT_LENGTH_DOWNLOAD';
    CURLINFO_CONTENT_LENGTH_UPLOAD:   Result:='CURLINFO_CONTENT_LENGTH_UPLOAD';
    CURLINFO_STARTTRANSFER_TIME:      Result:='CURLINFO_STARTTRANSFER_TIME';
    CURLINFO_REDIRECT_TIME:           Result:='CURLINFO_REDIRECT_TIME';
    else Result:='Invalid CurlInfo';
  end;
end;



function CurlObjCodeStr( aCode:curlobj_CurlCode ):string;
begin
  Result:=CurlHdrCodeStr(curl_h_CurlCode(aCode));
end;


function CurlObjFormCodeStr( aCode:curlobj_CurlFormCode ):string;
begin
  Result:=CurlHdrFormCodeStr(curl_h_CurlFormCode(aCode));
end;

function CurlObjInfoTypeStr( info:curlobj_CurlInfoType ):string;
begin
  Result:=CurlHdrInfoTypeStr(curl_h_CurlInfoType(info));
end;


function CurlObjInfoStr( info:curlobj_CurlInfo ):string;
begin
  Result:=CurlHdrInfoStr(curl_h_CurlInfo(info));
end;


function CurlObjOptionStr( aOption: curlobj_CurlOption ):string;
begin
  Result:=CurlHdrOptionStr( curl_h_CurlOption(aOption));
end;


end.
