program testtxcb; 

(* Program to test the ReadFunction callback *)

{$INCLUDE curltest.inc}

uses curlobj, sysutils;

(*
  The "stream" parameter of tCurl.ReadFunction can be a pointer to whatever you want - 
  pChar, tStringList, tFileStream, the handle of fdOpen or fOpen, etc.
  I am using a Pascal file handle just for an example...
*)

type 
  tCharFile = file of char;
  pCharFile = ^tCharFile;

function MyReadFunc(DataOut: pChar; ItemSize, ItemCount:LongWord; UserData:pointer):LongWord; cdecl;
var
  I, N:LongInt;
  C:char;
  F:pCharFile;
begin
  F:=UserData;
  N:=( ItemSize * ItemCount );
  I:=0;
  while ( I < N ) and not EOF(F^) do begin
    Read(F^, C);
    pChar(DataOut)[I]:=C;
    inc(I);
  end;
  if (I > 0) then 
    WriteLn('Sending ->  ', I, ' bytes')
  else WriteLn('Sending ->  EOF');
  Result:=I;
end;

(* Here I am using the HeaderFunc to trap the server's response codes... *)
function MyHeaderFunc( hdr: pChar; ItemSize, ItemCount:LongWord; UserData:pointer ):LongWord; cdecl;
var
  code, err:LongInt;
begin
  code:=0;
  Result:= ( ItemSize * ItemCount );
  if ( Result > 6 ) then begin
    val(copy(hdr, 1, 3), code, err); // If first 3 chars are digits...
    if ( err = 0 ) then LongInt(UserData^):=code; // We have a response code
  end;
  WriteLn(code);
end;  

var 
  MyCurl:tCurl;
  MyLocalFile:tCharFile;
  MyFtpCode:LongInt;
begin
  if ( ParamCount = 2 ) then begin
    if not FileIsReadable(ParamStr(1)) then begin
       WriteLn('Error reading local file: "', ParamStr(1), '"');
       HALT(1);
    end;

    MyCurl:=tCurl.Create(nil);
    with MyCurl do begin
      Upload:=True;
      URL:=ParamStr(2) + '/' + ExtractFileName(ParamStr(1));

      AssignFile(MyLocalFile, ParamStr(1));
      Reset(MyLocalFile);
      InputStream:=@MyLocalFile; // This will be passed to the ReadFunction
      ReadFunction:=@MyReadFunc;

      MyFtpCode:=0;
      HeaderStream:=@MyFtpCode; // This will be passed to the HeaderFunction
      HeaderFunction:=@MyHeaderFunc;

      Perform;
      WriteLn(ErrorString);
      WriteLn('Final response code: ', MyFtpCode);
      Free;
    end;
    CloseFile(MyLocalFile);
  end else begin 
    WriteLn('FTP file upload test program.');
    WriteLn('usage: testtxcb <local-file> <remote-url>');
  end;
end.
