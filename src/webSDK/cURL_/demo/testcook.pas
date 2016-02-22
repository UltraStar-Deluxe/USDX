program testcook;

{$IFDEF FPC}{$H+}{$MODE DELPHI}{$ENDIF}

uses curlobj;

type 
  tMyObj=class(TObject)
    procedure DoListCookies(Sender:TObject; Data:string; var bContinue:boolean);
  end;

procedure tMyObj.DoListCookies(Sender:TObject; Data:string; var bContinue:boolean);
begin
  with Sender as tCurl do Tag:=Tag+1;
  WriteLn(tCurl(Sender).Tag, '. ', Data);
end;


var
  o:tMyObj;
  c:tCurl;
  i:LongInt;
begin
  if ( ParamCount > 0 ) then begin
    o:=tMyObj.Create;
    c:=tCurl.Create(nil);
    c.OnListCookies:=o.DoListCookies;
    c.CookieSession:=True;
    c.OutputFile:={$IFDEF LINUX}'/dev/null'{$ELSE}'NUL'{$ENDIF};
    for i:= 1 to ParamCount do begin
      c.Url:=ParamStr(i);
      WriteLn('Retrieving ',c.URL, '...');
      if not c.Perform then WriteLn(c.ErrorString);
    end;
    c.ListCookies;
    c.Free;
    o.Free;
  end else begin
    WriteLn;
    WriteLn('List cookies returned by server(s)');
    WriteLn('Example:');
    WriteLn('  testcook aol.com msn.com');
    WriteLn;
  end;
end.

