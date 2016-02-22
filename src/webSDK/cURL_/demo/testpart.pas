program testpart;

(* Test partial downloads - not all servers support this! *)

{$INCLUDE curltest.inc}

uses curlobj;

var 
  MyCurl: tCurl;
  SizeTotal, RangeOffset:LongInt;
  RangeStr:string;

procedure FailTest(msg:string); 
  // This is ugly, but it avoids some nested "IF" statements...
begin
  MyCurl.Free;
  WriteLn(msg);
  HALT(1);
end;


begin  { MAIN }

  MyCurl:=tCurl.Create(nil);
  with MyCurl do begin

    // Use your own URL if you like...
    if ( ParamCount > 0 ) 
    then URL:=ParamStr(1)
    else URL:='http://curlpas.sourceforge.net/tests/rand.dat';
    FollowLocation:=True;
    OutputFile:='testpart.tmp';
//    Verbose:=True;
    NoBody:=True; // Get only response headers for now
    HeaderFile:={$IFDEF WIN32}'NUL'{$ELSE}'/dev/null'{$ENDIF}; // Hide the console output

    WriteLn('Connecting to "', URL, '"');

    // Try to get a total size for the file...
    if ( not Perform ) then FailTest(ErrorString);
    SizeTotal:=ContentLengthDownload;

    // Of course, we need a file that is bigger than one byte!
    if ( SizeTotal < 2 ) then FailTest('Invalid byte range.');
    WriteLn('Stated content length is: ', SizeTotal);

    // Set the range to get the first half of the file...
    RangeOffset:=SizeTotal div 2;
    str(RangeOffset, RangeStr);
    Range:='0-'+RangeStr; // Range is zero-based

    // Ask the server if it supports byte ranges...

    WriteLn('Requesting partial content ...');
    if ( not Perform ) then FailTest(ErrorString);
    WriteLn('HTTP response code is: ', HttpCode);
    if not ( HttpCode in [200, 206] ) then FailTest('Wrong answer, expected 206 or 200');
    WriteLn('(OK)');

    // Now we are ready for some real content...
    NoBody:=False; 
    HttpGet:=True;
    // Get the first part...
    WriteLn('Retrieving first half...');
    if ( not Perform ) then FailTest(ErrorString);

    inc(RangeOffset); // Bump to the next byte
    // Check the result...
    WriteLn('Requested ', RangeOffset , ' bytes,  got ' , GetFileSize(OutputFile), ' bytes.');
    if ( RangeOffset <> GetFileSize(OutputFile) ) then FailTest('Size mismatch.');
    WriteLn('(OK)');

    // Get the second part...
    WriteLn('Retrieving second half...');
    ResumeFrom:=RangeOffset; // Tells LibCurl to append to the local file
    if ( not Perform ) then FailTest(ErrorString);

    // Show me what I got...
    WriteLn('Stated content length was: ', SizeTotal);
    WriteLn('Actual disk file size is:  ', GetFileSize(OutputFile));
    if ( SizeTotal <> GetFileSize(OutputFile) ) then FailTest('Something went wrong!');
    WriteLn('(Passed)');
  end;

  MyCurl.Free; // Clean-up

end.
