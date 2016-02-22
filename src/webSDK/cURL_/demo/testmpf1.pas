program test1867;

(*
   Program using curl_formadd() to test rfc1867 multipart/form-data submission 
   
   This program also uses the files "sendme.txt" and "sendme.png" --
    they should be in the current working directory.
*)

{$INCLUDE curltest.inc}

uses curl_h;

var 
  MyCurl:pCurl;
  FirstPost, LastPost:pcurl_httppost;

begin

  FirstPost:=nil;
  LastPost:=nil;
  
  MyCurl:=curl_easy_init();
  curl_easy_setopt(MyCurl, CURLOPT_URL, 'http://curlpas.sourceforge.net/tests/test1867.php');
  curl_easy_setopt(MyCurl, CURLOPT_VERBOSE, True);

  curl_formadd(@FirstPost, @LastPost, 
    CURLFORM_COPYNAME,     'whodunit', 
    CURLFORM_COPYCONTENTS, 'testclient',
    CURLFORM_END
  );

  curl_formadd(@FirstPost, @LastPost, 
    CURLFORM_COPYNAME,    'readthis',  
    CURLFORM_FILECONTENT, 'sendme.txt',
    CURLFORM_CONTENTTYPE, 'text/plain',
    CURLFORM_END
  );

  curl_formadd(@FirstPost, @LastPost, 
    CURLFORM_COPYNAME,    'filethis',  
    CURLFORM_FILE,        'sendme.jpg',
    CURLFORM_CONTENTTYPE, 'image/jpeg',
    CURLFORM_END
  );

  curl_easy_setopt(MyCurl, CURLOPT_HTTPPOST, FirstPost);
  curl_easy_perform(MyCurl);
  curl_formfree(FirstPost);
  curl_easy_cleanup(MyCurl);
end.
