program test1867;

(*
   Program to test rfc1867 multipart/form-data submission 
   This program tries to send the files "sendme.txt" and "sendme.jpg" 
    -- they should be in the current working directory.
*)

{$INCLUDE curltest.inc}
uses curlobj;

var 
  MyCurl:tCurl;
begin
  MyCurl:=tCurl.Create(nil);

  MyCurl.URL:='http://curlpas.sourceforge.net/tests/test1867.php';

  MyCurl.FormData.Add( 'whodunit',  'testclient', '',
      POST_TYPE_PLAIN ); // send a string as contents 

  MyCurl.FormData.Add( 'readthis',  'sendme.txt', 'text/plain',
      POST_TYPE_FILEDATA );  // send contents of file

  MyCurl.FormData.Add( 'filethis',  'sendme.jpg', 'image/jpeg',
      POST_TYPE_ATTACHMENT );  // send the file itself

  if not MyCurl.Perform then WriteLn(MyCurl.ErrorString); 

  MyCurl.Free;

end.
