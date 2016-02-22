(***  Copyright (c) 2002-2005, Jeffrey Pohlmeyer, <yetanothergeek@yahoo.com>  ***)
(* Licensed per the file COPYING, which should be included in all distributions *)

{$IFNDEF CURL_H_INCLUDE}
unit curl_h;

{$IFDEF FPC}{$MODE OBJFPC}{$ENDIF}

interface

{$IFDEF WIN32}
uses winsock;
{$ELSE}
uses BaseUnix;
{$ENDIF}
{$ENDIF CURL_H_INCLUDE}

{$MINENUMSIZE 4}

{$IFDEF WIN32}
type
  off_t = longint;
  size_t = longword;
  time_t = longint;
  ptime_t = ^time_t;
{$ENDIF}

const
  LIB_CURL = {$IFDEF LINUX} 'libcurl.so' {$ELSE} 'libcurl-3.dll' {$ENDIF};

{$IFDEF FPC}{$PACKRECORDS C}{$ENDIF}

const
  CURLM_CALL_MULTI_PERFORM = -1;
  CURLM_OK = 0;
  CURLM_BAD_HANDLE = 1;
  CURLM_BAD_EASY_HANDLE = 2;
  CURLM_OUT_OF_MEMORY = 3;
  CURLM_INTERNAL_ERROR = 4;
  CURLM_LAST = 5;

type
  curlioerr = (
    CURLIOE_OK,
    CURLIOE_UNKNOWNCMD,
    CURLIOE_FAILRESTART,
    CURLIOE_LAST
  );

  curliocmd = (
    CURLIOCMD_NOP,
    CURLIOCMD_RESTARTREAD,
    CURLIOCMD_LAST
  );

  curl_infotype = (
    CURLINFO_TEXT = 0,
    CURLINFO_HEADER_IN,
    CURLINFO_HEADER_OUT,
    CURLINFO_DATA_IN,
    CURLINFO_DATA_OUT,
    CURLINFO_SSL_DATA_IN,
    CURLINFO_SSL_DATA_OUT,
    CURLINFO_END
  );

  CURLcode = (
    CURLE_OK = 0,
    CURLE_UNSUPPORTED_PROTOCOL,
    CURLE_FAILED_INIT,
    CURLE_URL_MALFORMAT,
    CURLE_URL_MALFORMAT_USER,
    CURLE_COULDNT_RESOLVE_PROXY,
    CURLE_COULDNT_RESOLVE_HOST,
    CURLE_COULDNT_CONNECT,
    CURLE_FTP_WEIRD_SERVER_REPLY,
    CURLE_FTP_ACCESS_DENIED,
    CURLE_FTP_USER_PASSWORD_INCORRECT,
    CURLE_FTP_WEIRD_PASS_REPLY,
    CURLE_FTP_WEIRD_USER_REPLY,
    CURLE_FTP_WEIRD_PASV_REPLY,
    CURLE_FTP_WEIRD_227_FORMAT,
    CURLE_FTP_CANT_GET_HOST,
    CURLE_FTP_CANT_RECONNECT,
    CURLE_FTP_COULDNT_SET_BINARY,
    CURLE_PARTIAL_FILE,
    CURLE_FTP_COULDNT_RETR_FILE,
    CURLE_FTP_WRITE_ERROR,
    CURLE_FTP_QUOTE_ERROR,
    CURLE_HTTP_RETURNED_ERROR,
    CURLE_WRITE_ERROR,
    CURLE_MALFORMAT_USER,
    CURLE_FTP_COULDNT_STOR_FILE,
    CURLE_READ_ERROR,
    CURLE_OUT_OF_MEMORY,
    CURLE_OPERATION_TIMEOUTED,
    CURLE_FTP_COULDNT_SET_ASCII,
    CURLE_FTP_PORT_FAILED,
    CURLE_FTP_COULDNT_USE_REST,
    CURLE_FTP_COULDNT_GET_SIZE,
    CURLE_HTTP_RANGE_ERROR,
    CURLE_HTTP_POST_ERROR,
    CURLE_SSL_CONNECT_ERROR,
    CURLE_BAD_DOWNLOAD_RESUME,
    CURLE_FILE_COULDNT_READ_FILE,
    CURLE_LDAP_CANNOT_BIND,
    CURLE_LDAP_SEARCH_FAILED,
    CURLE_LIBRARY_NOT_FOUND,
    CURLE_FUNCTION_NOT_FOUND,
    CURLE_ABORTED_BY_CALLBACK,
    CURLE_BAD_FUNCTION_ARGUMENT,
    CURLE_BAD_CALLING_ORDER,
    CURLE_INTERFACE_FAILED,
    CURLE_BAD_PASSWORD_ENTERED,
    CURLE_TOO_MANY_REDIRECTS,
    CURLE_UNKNOWN_TELNET_OPTION,
    CURLE_TELNET_OPTION_SYNTAX,
    CURLE_OBSOLETE,
    CURLE_SSL_PEER_CERTIFICATE,
    CURLE_GOT_NOTHING,
    CURLE_SSL_ENGINE_NOTFOUND,
    CURLE_SSL_ENGINE_SETFAILED,
    CURLE_SEND_ERROR,
    CURLE_RECV_ERROR,
    CURLE_SHARE_IN_USE,
    CURLE_SSL_CERTPROBLEM,
    CURLE_SSL_CIPHER,
    CURLE_SSL_CACERT,
    CURLE_BAD_CONTENT_ENCODING,
    CURLE_LDAP_INVALID_URL,
    CURLE_FILESIZE_EXCEEDED,
    CURLE_FTP_SSL_FAILED,
    CURLE_SEND_FAIL_REWIND,
    CURLE_SSL_ENGINE_INITFAILED,
    CURLE_LOGIN_DENIED,
    CURLE_TFTP_NOTFOUND,
    CURLE_TFTP_PERM,
    CURLE_TFTP_DISKFULL,
    CURLE_TFTP_ILLEGAL,
    CURLE_TFTP_UNKNOWNID,
    CURLE_TFTP_EXISTS,
    CURLE_TFTP_NOSUCHUSER,
    CURL_LAST
  );

  curl_proxytype = (
    CURLPROXY_HTTP = 0,
    CURLPROXY_SOCKS4 = 4,
    CURLPROXY_SOCKS5 = 5
  );

  curl_ftpssl = (
    CURLFTPSSL_NONE,
    CURLFTPSSL_TRY,
    CURLFTPSSL_CONTROL,
    CURLFTPSSL_ALL,
    CURLFTPSSL_LAST
  );

  curl_ftpauth = (
    CURLFTPAUTH_DEFAULT,
    CURLFTPAUTH_SSL,
    CURLFTPAUTH_TLS,
    CURLFTPAUTH_LAST
  );

  CURLoption = (
    CURLOPT_PORT                    = 3,
    CURLOPT_TIMEOUT                 = 13,
    CURLOPT_INFILESIZE              = 14,
    CURLOPT_LOW_SPEED_LIMIT         = 19,
    CURLOPT_LOW_SPEED_TIME          = 20,
    CURLOPT_RESUME_FROM             = 21,
    CURLOPT_CRLF                    = 27,
    CURLOPT_SSLVERSION              = 32,
    CURLOPT_TIMECONDITION           = 33,
    CURLOPT_TIMEVALUE               = 34,
    CURLOPT_VERBOSE                 = 41,
    CURLOPT_HEADER                  = 42,
    CURLOPT_NOPROGRESS              = 43,
    CURLOPT_NOBODY                  = 44,
    CURLOPT_FAILONERROR             = 45,
    CURLOPT_UPLOAD                  = 46,
    CURLOPT_POST                    = 47,
    CURLOPT_FTPLISTONLY             = 48,
    CURLOPT_FTPAPPEND               = 50,
    CURLOPT_NETRC                   = 51,
    CURLOPT_FOLLOWLOCATION          = 52,
    CURLOPT_TRANSFERTEXT            = 53,
    CURLOPT_PUT                     = 54,
    CURLOPT_AUTOREFERER             = 58,
    CURLOPT_PROXYPORT               = 59,
    CURLOPT_POSTFIELDSIZE           = 60,
    CURLOPT_HTTPPROXYTUNNEL         = 61,
    CURLOPT_SSL_VERIFYPEER          = 64,
    CURLOPT_MAXREDIRS               = 68,
    CURLOPT_FILETIME                = 69,
    CURLOPT_MAXCONNECTS             = 71,
    CURLOPT_CLOSEPOLICY             = 72,
    CURLOPT_FRESH_CONNECT           = 74,
    CURLOPT_FORBID_REUSE            = 75,
    CURLOPT_CONNECTTIMEOUT          = 78,
    CURLOPT_HTTPGET                 = 80,
    CURLOPT_SSL_VERIFYHOST          = 81,
    CURLOPT_HTTP_VERSION            = 84,
    CURLOPT_FTP_USE_EPSV            = 85,
    CURLOPT_SSLENGINE_DEFAULT       = 90,
    CURLOPT_DNS_USE_GLOBAL_CACHE    = 91,
    CURLOPT_DNS_CACHE_TIMEOUT       = 92,
    CURLOPT_COOKIESESSION           = 96,
    CURLOPT_BUFFERSIZE              = 98,
    CURLOPT_NOSIGNAL                = 99,
    CURLOPT_PROXYTYPE               = 101,
    CURLOPT_UNRESTRICTED_AUTH       = 105,
    CURLOPT_FTP_USE_EPRT            = 106,
    CURLOPT_HTTPAUTH                = 107,
    CURLOPT_FTP_CREATE_MISSING_DIRS = 110,
    CURLOPT_PROXYAUTH               = 111,
    CURLOPT_FTP_RESPONSE_TIMEOUT    = 112,
    CURLOPT_IPRESOLVE               = 113,
    CURLOPT_MAXFILESIZE             = 114,
    CURLOPT_FTP_SSL                 = 119,
    CURLOPT_TCP_NODELAY             = 121,
    CURLOPT_FTPSSLAUTH              = 129,
    CURLOPT_IGNORE_CONTENT_LENGTH   = 136,
    CURLOPT_FTP_SKIP_PASV_IP        = 137,
    CURLOPT_FILE                    = 10001,
    CURLOPT_URL                     = 10002,
    CURLOPT_PROXY                   = 10004,
    CURLOPT_USERPWD                 = 10005,
    CURLOPT_PROXYUSERPWD            = 10006,
    CURLOPT_RANGE                   = 10007,
    CURLOPT_INFILE                  = 10009,
    CURLOPT_ERRORBUFFER             = 10010,
    CURLOPT_POSTFIELDS              = 10015,
    CURLOPT_REFERER                 = 10016,
    CURLOPT_FTPPORT                 = 10017,
    CURLOPT_USERAGENT               = 10018,
    CURLOPT_COOKIE                  = 10022,
    CURLOPT_HTTPHEADER              = 10023,
    CURLOPT_HTTPPOST                = 10024,
    CURLOPT_SSLCERT                 = 10025,
    CURLOPT_SSLCERTPASSWD           = 10026,
    CURLOPT_QUOTE                   = 10028,
    CURLOPT_WRITEHEADER             = 10029,
    CURLOPT_COOKIEFILE              = 10031,
    CURLOPT_CUSTOMREQUEST           = 10036,
    CURLOPT_STDERR                  = 10037,
    CURLOPT_POSTQUOTE               = 10039,
    CURLOPT_WRITEINFO               = 10040,
    CURLOPT_PROGRESSDATA            = 10057,
    CURLOPT_INTERFACE               = 10062,
    CURLOPT_KRB4LEVEL               = 10063,
    CURLOPT_CAINFO                  = 10065,
    CURLOPT_TELNETOPTIONS           = 10070,
    CURLOPT_RANDOM_FILE             = 10076,
    CURLOPT_EGDSOCKET               = 10077,
    CURLOPT_COOKIEJAR               = 10082,
    CURLOPT_SSL_CIPHER_LIST         = 10083,
    CURLOPT_SSLCERTTYPE             = 10086,
    CURLOPT_SSLKEY                  = 10087,
    CURLOPT_SSLKEYTYPE              = 10088,
    CURLOPT_SSLENGINE               = 10089,
    CURLOPT_PREQUOTE                = 10093,
    CURLOPT_DEBUGDATA               = 10095,
    CURLOPT_CAPATH                  = 10097,
    CURLOPT_SHARE                   = 10100,
    CURLOPT_ENCODING                = 10102,
    CURLOPT_PRIVATE                 = 10103,
    CURLOPT_HTTP200ALIASES          = 10104,
    CURLOPT_SSL_CTX_DATA            = 10109,
    CURLOPT_NETRC_FILE              = 10118,
    CURLOPT_SOURCE_USERPWD          = 10123,
    CURLOPT_SOURCE_PREQUOTE         = 10127,
    CURLOPT_SOURCE_POSTQUOTE        = 10128,
    CURLOPT_IOCTLDATA               = 10131,
    CURLOPT_SOURCE_URL              = 10132,
    CURLOPT_SOURCE_QUOTE            = 10133,
    CURLOPT_FTP_ACCOUNT             = 10134,
    CURLOPT_COOKIELIST              = 10135,
    CURLOPT_WRITEFUNCTION           = 20011,
    CURLOPT_READFUNCTION            = 20012,
    CURLOPT_PROGRESSFUNCTION        = 20056,
    CURLOPT_HEADERFUNCTION          = 20079,
    CURLOPT_DEBUGFUNCTION           = 20094,
    CURLOPT_SSL_CTX_FUNCTION        = 20108,
    CURLOPT_IOCTLFUNCTION           = 20130,
    CURLOPT_INFILESIZE_LARGE        = 30115,
    CURLOPT_RESUME_FROM_LARGE       = 30116,
    CURLOPT_MAXFILESIZE_LARGE       = 30117,
    CURLOPT_POSTFIELDSIZE_LARGE     = 30120,
    CURLOPT_LASTENTRY
  );

  curl_http_version = (
    CURL_HTTP_VERSION_NONE,
    CURL_HTTP_VERSION_1_0,
    CURL_HTTP_VERSION_1_1,
    CURL_HTTP_VERSION_LAST
  );

  CURL_NETRC_OPTION = (
    CURL_NETRC_IGNORED,
    CURL_NETRC_OPTIONAL,
    CURL_NETRC_REQUIRED,
    CURL_NETRC_LAST
  );

  curl_sslversion = (
    CURL_SSLVERSION_DEFAULT,
    CURL_SSLVERSION_TLSv1,
    CURL_SSLVERSION_SSLv2,
    CURL_SSLVERSION_SSLv3,
    CURL_SSLVERSION_LAST
  );

  curl_TimeCond = (
    CURL_TIMECOND_NONE,
    CURL_TIMECOND_IFMODSINCE,
    CURL_TIMECOND_IFUNMODSINCE,
    CURL_TIMECOND_LASTMOD,
    CURL_TIMECOND_LAST
  );

  CURLformoption = (
    CURLFORM_NOTHING,
    CURLFORM_COPYNAME,
    CURLFORM_PTRNAME,
    CURLFORM_NAMELENGTH,
    CURLFORM_COPYCONTENTS,
    CURLFORM_PTRCONTENTS,
    CURLFORM_CONTENTSLENGTH,
    CURLFORM_FILECONTENT,
    CURLFORM_ARRAY,
    CURLFORM_OBSOLETE,
    CURLFORM_FILE,
    CURLFORM_BUFFER,
    CURLFORM_BUFFERPTR,
    CURLFORM_BUFFERLENGTH,
    CURLFORM_CONTENTTYPE,
    CURLFORM_CONTENTHEADER,
    CURLFORM_FILENAME,
    CURLFORM_END,
    CURLFORM_OBSOLETE2,
    CURLFORM_LASTENTRY
  );

  CURLFORMcode = (
    CURL_FORMADD_OK,
    CURL_FORMADD_MEMORY,
    CURL_FORMADD_OPTION_TWICE,
    CURL_FORMADD_NULL,
    CURL_FORMADD_UNKNOWN_OPTION,
    CURL_FORMADD_INCOMPLETE,
    CURL_FORMADD_ILLEGAL_ARRAY,
    CURL_FORMADD_DISABLED,
    CURL_FORMADD_LAST
  );

  CURLINFO = (
    CURLINFO_NONE,
    CURLINFO_LASTONE                 = 28,
    CURLINFO_EFFECTIVE_URL           = 1048577,
    CURLINFO_CONTENT_TYPE            = 1048594,
    CURLINFO_PRIVATE                 = 1048597,
    CURLINFO_RESPONSE_CODE           = 2097154,
    CURLINFO_HEADER_SIZE             = 2097163,
    CURLINFO_REQUEST_SIZE            = 2097164,
    CURLINFO_SSL_VERIFYRESULT        = 2097165,
    CURLINFO_FILETIME                = 2097166,
    CURLINFO_REDIRECT_COUNT          = 2097172,
    CURLINFO_HTTP_CONNECTCODE        = 2097174,
    CURLINFO_HTTPAUTH_AVAIL          = 2097175,
    CURLINFO_PROXYAUTH_AVAIL         = 2097176,
    CURLINFO_OS_ERRNO                = 2097177,
    CURLINFO_NUM_CONNECTS            = 2097178,
    CURLINFO_TOTAL_TIME              = 3145731,
    CURLINFO_NAMELOOKUP_TIME         = 3145732,
    CURLINFO_CONNECT_TIME            = 3145733,
    CURLINFO_PRETRANSFER_TIME        = 3145734,
    CURLINFO_SIZE_UPLOAD             = 3145735,
    CURLINFO_SIZE_DOWNLOAD           = 3145736,
    CURLINFO_SPEED_DOWNLOAD          = 3145737,
    CURLINFO_SPEED_UPLOAD            = 3145738,
    CURLINFO_CONTENT_LENGTH_DOWNLOAD = 3145743,
    CURLINFO_CONTENT_LENGTH_UPLOAD   = 3145744,
    CURLINFO_STARTTRANSFER_TIME      = 3145745,
    CURLINFO_REDIRECT_TIME           = 3145747,
    CURLINFO_SSL_ENGINES             = 4194331,
    CURLINFO_COOKIELIST              = 4194332,
    CURLINFO_VERYLAST
  );

  curl_closepolicy = (
    CURLCLOSEPOLICY_NONE,
    CURLCLOSEPOLICY_OLDEST,
    CURLCLOSEPOLICY_LEAST_RECENTLY_USED,
    CURLCLOSEPOLICY_LEAST_TRAFFIC,
    CURLCLOSEPOLICY_SLOWEST,
    CURLCLOSEPOLICY_CALLBACK,
    CURLCLOSEPOLICY_LAST
  );

  curl_lock_data = (
    CURL_LOCK_DATA_NONE = 0,
    CURL_LOCK_DATA_SHARE,
    CURL_LOCK_DATA_COOKIE,
    CURL_LOCK_DATA_DNS,
    CURL_LOCK_DATA_SSL_SESSION,
    CURL_LOCK_DATA_CONNECT,
    CURL_LOCK_DATA_LAST
  );

  curl_lock_access = (
    CURL_LOCK_ACCESS_NONE = 0,
    CURL_LOCK_ACCESS_SHARED = 1,
    CURL_LOCK_ACCESS_SINGLE = 2,
    CURL_LOCK_ACCESS_LAST
  );

  CURLSHcode = (
    CURLSHE_OK,
    CURLSHE_BAD_OPTION,
    CURLSHE_IN_USE,
    CURLSHE_INVALID,
    CURLSHE_NOMEM,
    CURLSHE_LAST
  );

  CURLSHoption = (
    CURLSHOPT_NONE,
    CURLSHOPT_SHARE,
    CURLSHOPT_UNSHARE,
    CURLSHOPT_LOCKFUNC,
    CURLSHOPT_UNLOCKFUNC,
    CURLSHOPT_USERDATA,
    CURLSHOPT_LAST
  );

  CURLversion = (
    CURLVERSION_FIRST,
    CURLVERSION_SECOND,
    CURLVERSION_THIRD,
    CURLVERSION_LAST
  );

  CURLMSG = (
    CURLMSG_NONE,
    CURLMSG_DONE,
    CURLMSG_LAST
  );


type
  pCURL=^CURL;
  CURL=pointer;
  pcurl_off_t=^curl_off_t;
  curl_off_t=off_t;
  pcurl_httppost=^curl_httppost;
  ppcurl_httppost=^pcurl_httppost;
  pcurl_malloc_callback=^curl_malloc_callback;
  curl_malloc_callback=pointer;
  pcurl_realloc_callback=^curl_realloc_callback;
  curl_realloc_callback=pointer;
  pcurl_strdup_callback=^curl_strdup_callback;
  curl_strdup_callback=char;
  pcurl_calloc_callback=^curl_calloc_callback;
  curl_calloc_callback=pointer;
  pcurl_forms=^curl_forms;
  pcurl_slist=^curl_slist;
  pCURLSH=^CURLSH;
  CURLSH=pointer;
  pcurl_version_info_data=^curl_version_info_data;
  pCURLM=^CURLM;
  CURLM=pointer;
  pCURLMcode=^CURLMcode;
  CURLMcode=longint;
  pCurlMsgRec=^CurlMsgRec;
  curl_progress_callback = function (clientp:pointer; dltotal:double; dlnow:double; ultotal:double; ulnow:double):longint; cdecl;
  curl_write_callback = function (buffer:pchar; size:size_t; nitems:size_t; outstream:pointer):size_t; cdecl;
  curl_read_callback = function (buffer:pchar; size:size_t; nitems:size_t; instream:pointer):size_t; cdecl;
  curl_ioctl_callback = function (handle:pCURL; cmd:longint; clientp:pointer):curlioerr; cdecl;
  curl_free_callback = procedure (ptr:pointer); cdecl;
  curl_debug_callback = function (handle:pCURL; _type:curl_infotype; data:pchar; size:size_t; userptr:pointer):longint; cdecl;
  curl_ssl_ctx_callback = function (curl:pCURL; ssl_ctx:pointer; userptr:pointer):CURLcode; cdecl;
  curl_lock_function = procedure (handle:pCURL; data:curl_lock_data; locktype:curl_lock_access; userptr:pointer); cdecl;
  curl_unlock_function = procedure (handle:pCURL; data:curl_lock_data; userptr:pointer); cdecl;

  curl_httppost = record
    next : pcurl_httppost;
    name : pchar;
    namelength : longint;
    contents : pchar;
    contentslength : longint;
    buffer : pchar;
    bufferlength : longint;
    contenttype : pchar;
    contentheader : pcurl_slist;
    more : pcurl_httppost;
    flags : longint;
    showfilename : pchar;
    userp:    pointer;                      //custom pointer used for HTTPPOST_CALLBACK posts 3:28pm 22 december 2009 Eric de Lange added
  end;


  curl_forms = record
    option : CURLformoption;
    value : pchar;
  end;


  curl_slist = record
    data : pchar;
    next : pcurl_slist;
  end;


  curl_version_info_data = record
    age : CURLversion;
    version : pchar;
    version_num : longword;
    host : pchar;
    features : longint;
    ssl_version : pchar;
    ssl_version_num : longint;
    libz_version : pchar;
    protocols : ppchar;
    ares : pchar;
    ares_num : longint;
    libidn : pchar;
    iconv_ver_num: longint; //3:32pm 22 december 2009 Eric de Lange added
    libssh_version:  pchar; // human readable string  3:33pm 22 december 2009 Eric de Lange added
  end;


  CurlMsgRec = record
    msg : CURLMSG;
    easy_handle : pCURL;
    data : record
      case longint of
        0 : ( whatever : pointer );
        1 : ( result : CURLcode );
    end;
  end;

function  curl_strequal(s1:pchar; s2:pchar):longint; cdecl; external LIB_CURL;
function  curl_strnequal(s1:pchar; s2:pchar; n:size_t):longint; cdecl; external LIB_CURL;
function  curl_formadd(httppost:ppcurl_httppost; last_post:ppcurl_httppost):CURLFORMcode; varargs; cdecl; external LIB_CURL;
procedure curl_formfree(form:pcurl_httppost); cdecl; external LIB_CURL;
function  curl_getenv(variable:pchar):pchar; cdecl; external LIB_CURL;
function  curl_version:pchar; cdecl; external LIB_CURL;
function  curl_escape(s:pchar; len:longint):pchar; cdecl; external LIB_CURL;
function  curl_unescape(s:pchar; len:longint):pchar; cdecl; external LIB_CURL;
procedure curl_free(p:pointer); cdecl; external LIB_CURL;
function  curl_global_init(flags:longint):CURLcode; cdecl; external LIB_CURL;
function  curl_global_init_mem(flags:longint; m:curl_malloc_callback; f:curl_free_callback; r:curl_realloc_callback; s:curl_strdup_callback; c:curl_calloc_callback):CURLcode; cdecl; external LIB_CURL;
procedure curl_global_cleanup; cdecl; external LIB_CURL;
function  curl_slist_append(list:pcurl_slist; s:pchar):pcurl_slist; cdecl; external LIB_CURL;
procedure curl_slist_free_all(list:pcurl_slist); cdecl; external LIB_CURL;
function  curl_getdate(p:pchar; unused:ptime_t):time_t; cdecl; external LIB_CURL;
function  curl_share_init:pCURLSH; cdecl; external LIB_CURL;
function  curl_share_setopt(sh:pCURLSH; option:CURLSHoption):CURLSHcode; varargs; cdecl; external LIB_CURL;
function  curl_share_cleanup(sh:pCURLSH):CURLSHcode; cdecl; external LIB_CURL;
function  curl_version_info(ver:CURLversion):pcurl_version_info_data; cdecl; external LIB_CURL;
function  curl_easy_strerror(code:CURLcode):pchar; cdecl; external LIB_CURL;
function  curl_share_strerror(code:CURLSHcode):pchar; cdecl; external LIB_CURL;
function  curl_easy_init:pCURL; cdecl; external LIB_CURL;
function  curl_easy_setopt(curl:pCURL; option:CURLoption):CURLcode; varargs; cdecl; external LIB_CURL;
function  curl_easy_perform(curl:pCURL):CURLcode; cdecl; external LIB_CURL;
procedure curl_easy_cleanup(curl:pCURL); cdecl; external LIB_CURL;
function  curl_easy_getinfo(curl:pCURL; info:CURLINFO; out value):CURLcode; cdecl; external LIB_CURL;
function  curl_easy_duphandle(curl:pCURL):pCURL; cdecl; external LIB_CURL;
procedure curl_easy_reset(curl:pCURL); cdecl; external LIB_CURL;
function  curl_multi_init:pCURLM; cdecl; external LIB_CURL;
function  curl_multi_add_handle(multi_handle:pCURLM; curl_handle:pCURL):CURLMcode; cdecl; external LIB_CURL;
function  curl_multi_remove_handle(multi_handle:pCURLM; curl_handle:pCURL):CURLMcode; cdecl; external LIB_CURL;
function  curl_multi_fdset(multi_handle:pCURLM; read_fd_set:pfdset; write_fd_set:pfdset; exc_fd_set:pfdset; max_fd:plongint):CURLMcode; cdecl; external LIB_CURL;
function  curl_multi_perform(multi_handle:pCURLM; running_handles:plongint):CURLMcode; cdecl; external LIB_CURL;
function  curl_multi_cleanup(multi_handle:pCURLM):CURLMcode; cdecl; external LIB_CURL;
function  curl_multi_info_read(multi_handle:pCURLM; msgs_in_queue:plongint):pCurlMsgRec; cdecl; external LIB_CURL;
function  curl_multi_strerror(code:CURLMcode):pchar; cdecl; external LIB_CURL;

const
  CURLOPT_SSLKEYPASSWD = CURLOPT_SSLCERTPASSWD;
  
  CURLAUTH_ANY =  not (0);
  CURLAUTH_BASIC = 1 shl 0;
  CURLAUTH_DIGEST = 1 shl 1;
  CURLAUTH_GSSNEGOTIATE = 1 shl 2;
  CURLAUTH_NONE = 0;
  CURLAUTH_NTLM = 1 shl 3;
  CURLE_OPERATION_TIMEDOUT = CURLE_OPERATION_TIMEOUTED;
  CURLINFO_DOUBLE = $300000;
  CURLINFO_HTTP_CODE = CURLINFO_RESPONSE_CODE;
  CURLINFO_LONG = $200000;
  CURLINFO_MASK = $0fffff;
  CURLINFO_SLIST = $400000;
  CURLINFO_STRING = $100000;
  CURLINFO_TYPEMASK = $f00000;
  CURLOPTTYPE_FUNCTIONPOINT = 20000;
  CURLOPTTYPE_LONG = 0;
  CURLOPTTYPE_OBJECTPOINT = 10000;
  CURLOPTTYPE_OFF_T = 30000;
  CURLOPT_HEADERDATA = CURLOPT_WRITEHEADER;
  CURLOPT_READDATA = CURLOPT_INFILE;
  CURLOPT_WRITEDATA = CURLOPT_FILE;
  CURLVERSION_NOW = CURLVERSION_THIRD;
  CURL_ERROR_SIZE = 256;
  CURL_FORMAT_OFF_T = '%ld';
  CURL_GLOBAL_NOTHING = 0;
  CURL_GLOBAL_SSL = 1 shl 0;
  CURL_GLOBAL_WIN32 = 1 shl 1;
  CURL_IPRESOLVE_V4 = 1;
  CURL_IPRESOLVE_V6 = 2;
  CURL_IPRESOLVE_WHATEVER = 0;
  CURL_MAX_WRITE_SIZE = 16384;
  CURL_NO_OLDIES = 1;
  CURL_READFUNC_ABORT = $10000000;
  CURL_VERSION_ASYNCHDNS = 1 shl 7;
  CURL_VERSION_DEBUG = 1 shl 6;
  CURL_VERSION_GSSNEGOTIATE = 1 shl 5;
  CURL_VERSION_IDN = 1 shl 10;
  CURL_VERSION_IPV6 = 1 shl 0;
  CURL_VERSION_KERBEROS4 = 1 shl 1;
  CURL_VERSION_LARGEFILE = 1 shl 9;
  CURL_VERSION_LIBZ = 1 shl 3;
  CURL_VERSION_NTLM = 1 shl 4;
  CURL_VERSION_SPNEGO = 1 shl 8;
  CURL_VERSION_SSL = 1 shl 2;
  CURL_VERSION_SSPI = 1 shl 11;
  HTTPPOST_BUFFER = 1 shl 4;
  HTTPPOST_FILENAME = 1 shl 0;
  HTTPPOST_PTRBUFFER = 1 shl 5;
  HTTPPOST_PTRCONTENTS = 1 shl 3;
  HTTPPOST_PTRNAME = 1 shl 2;
  HTTPPOST_READFILE = 1 shl 1;
  LIBCURL_VERSION = '7.15.1-CVS';
  LIBCURL_VERSION_MAJOR = 7;
  LIBCURL_VERSION_MINOR = 15;
  LIBCURL_VERSION_NUM = $070f01;
  LIBCURL_VERSION_PATCH = 1;
  CURLAUTH_ANYSAFE =  not (CURLAUTH_BASIC);
  CURL_GLOBAL_ALL = CURL_GLOBAL_SSL or CURL_GLOBAL_WIN32;
  CURL_GLOBAL_DEFAULT = CURL_GLOBAL_ALL;

{$IFNDEF CURL_H_INCLUDE}
implementation
end.
{$ENDIF CURL_H_INCLUDE}
