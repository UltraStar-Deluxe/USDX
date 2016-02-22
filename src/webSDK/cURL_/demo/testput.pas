program testput;  

(* Program to test ftp uploading *)

{$INCLUDE curltest.inc}

uses curlobj, sysutils;

var 
  MyCurl:tCurl;
  ModeStr, RemoteFile:string;
  i:integer;
begin
  if ( ParamCount in [3..4] ) then begin
    RemoteFile:=ExtractFileName(ParamStr(3));
    if ( ParamCount = 4 ) then begin  // Check for valid 3 digit octal-mode  ... 
       ModeStr:=ParamStr(4);
       for i:=1 to length(ModeStr) do if not ( ModeStr[i] in ['0'..'7']) then begin
         ModeStr:='';
         Break;
       end;
       while ( ModeStr <> '' ) and ( ModeStr[1] = '0' ) do delete(ModeStr,1,1);
       if ( Length(ModeStr) <> 3 ) then ModeStr:='';
       if ( ModeStr <> '' )  
       then ModeStr:='SITE CHMOD ' + ModeStr + ' ' + RemoteFile
       else begin 
         WriteLn('WARNING: invalid octal-mode argument: ', ParamStr(4));
         WriteLn('Mode must be 3 digit octal.');
       end
    end else ModeStr:='';    
    MyCurl:=tCurl.Create(nil);
    MyCurl.Verbose:=True;
    MyCurl.Upload:=True;
    MyCurl.UserPwd:=ParamStr(1);
    MyCurl.URL:=ParamStr(2) + '/' + RemoteFile; // -- The url MUST have a filename!
    MyCurl.InputFile:=ParamStr(3);
    if ( ModeStr <> '' ) then MyCurl.PostQuote.Add(ModeStr);
    if not MyCurl.Perform then WriteLn(MyCurl.ErrorString);
    MyCurl.Free;
  end else begin
    WriteLn('Upload a file using ftp...');
    WriteLn('testput <username>[:password] <remote-site>[/directory] <local-file> [octal-mode]');
    WriteLn('If octal-mode is specified, permissions for the file will be set after upload.');   
  end;
end.
