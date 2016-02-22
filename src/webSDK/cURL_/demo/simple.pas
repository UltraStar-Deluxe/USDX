program simple;

{ -- Simplest CURL program -- }

{$H+}
uses curl_h;

var 
  hCurl:pCurl;
begin
  hCurl:= curl_easy_init;
  if ( hCurl <> nil ) then begin
    curl_easy_setopt(hCurl, CURLOPT_VERBOSE, True);
    curl_easy_setopt(hCurl, CURLOPT_URL, 'curl.haxx.se');
    curl_easy_perform(hCurl);
    curl_easy_cleanup(hCurl);
  end;
end.

