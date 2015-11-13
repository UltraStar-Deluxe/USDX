program testmult; 

(* Example using curl_multi library calls *)

{$INCLUDE curltest.inc}

uses {$IFDEF LINUX}Libc,{$ELSE}winsock,{$ENDIF}curl_h;


{$IFDEF WIN32}
  type 
    pIOFile=pointer;
  function fopen ( path, mode: pChar ): pIOFile; cdecl; external 'msvcrt.dll' name 'fopen';
  function fclose ( f:pIOFile ): LongInt; cdecl; external 'msvcrt.dll' name 'fclose';
{$ENDIF}


function progress_callback( UserData:pChar; 
                            DownloadTotal:double; DownloadNow:double; 
                            UploadTotal:double; UploadNow:double ): LongInt; cdecl;
begin
  WriteLn(UserData, ':', DownloadNow:8:0,' of ' ,DownloadTotal:8:0);
  Result:=0;
end;


var 
  multi_handle:pCurlM;
  http_handle_one:pCurl;
  http_handle_two:pCurl;
  still_running:LongInt;
  timeout: TimeVal;
  rc: LongInt;
  fdread:  tFDSet;
  fdwrite: tFDSet;
  fdexcep: tFDSet;
  maxfd: LongInt;
  file_one, file_two: pIOFile;
  url_one, url_two: pChar;

begin

  url_one:='http://www.cbs.com/';
  url_two:='http://www.cnn.com/';

  http_handle_one := curl_easy_init();
  http_handle_two := curl_easy_init();

  curl_easy_setopt(http_handle_one, CURLOPT_URL, url_one);
  curl_easy_setopt(http_handle_two, CURLOPT_URL, url_two);

  curl_easy_setopt(http_handle_one, CURLOPT_FOLLOWLOCATION, True);
  curl_easy_setopt(http_handle_two, CURLOPT_FOLLOWLOCATION, True);

  file_one:=fopen('temp_one.htm', 'wb');
  file_two:=fopen('temp_two.htm', 'wb');

  curl_easy_setopt(http_handle_one, CURLOPT_FILE, file_one);
  curl_easy_setopt(http_handle_two, CURLOPT_FILE, file_two);

  curl_easy_setopt(http_handle_one, CURLOPT_NOPROGRESS, False);
  curl_easy_setopt(http_handle_two, CURLOPT_NOPROGRESS, False);

  curl_easy_setopt(http_handle_one, CURLOPT_PROGRESSFUNCTION, @progress_callback);
  curl_easy_setopt(http_handle_two, CURLOPT_PROGRESSFUNCTION, @progress_callback);

  curl_easy_setopt(http_handle_one, CURLOPT_PROGRESSDATA, url_one);
  curl_easy_setopt(http_handle_two, CURLOPT_PROGRESSDATA, url_two);

  multi_handle := curl_multi_init();

  curl_multi_add_handle(multi_handle, http_handle_one);
  curl_multi_add_handle(multi_handle, http_handle_two);

  while ( curl_multi_perform(multi_handle, @still_running) = CURLM_CALL_MULTI_PERFORM ) do {nothing};

  while(still_running <> 0 ) do begin
    FD_ZERO(fdread);
    FD_ZERO(fdwrite);
    FD_ZERO(fdexcep);
    timeout.tv_sec := 1;
    timeout.tv_usec := 0;
    curl_multi_fdset(multi_handle, @fdread, @fdwrite, @fdexcep, @maxfd);
    rc := select(maxfd + 1, @fdread, @fdwrite, @fdexcep, @timeout);
    if ( rc <> -1 ) then while ( curl_multi_perform(multi_handle, @still_running) = CURLM_CALL_MULTI_PERFORM ) do {nothing};
  end;

  fclose(file_one);
  fclose(file_two);

  curl_multi_cleanup(multi_handle);
  curl_easy_cleanup(http_handle_one);
  curl_easy_cleanup(http_handle_two);
end.
