program testver;

(* Display extended version information *)

{$INCLUDE curltest.inc}

uses curlobj;


function YesNo(b:boolean):string;
begin
  if b then Result:='yes' else Result:='no';
end;

var
  i:integer;

begin
  WriteLn('CurlVersion:      ', tCurl.CurlVersion);
  WriteLn('LibraryVersion:   ', tCurl.LibraryVersion);
  WriteLn('VersionNumber:    ', tCurl.VersionNumber);
  WriteLn('Machine:          ', tCurl.Machine);
  if tCurl.Features.SSL then begin
    WriteLn('SslVersionString: ', tCurl.SslVersionString);
    WriteLn('SslVersionNumber: ', tCurl.SslVersionNumber);
  end;
  if tCurl.Features.Libz then WriteLn('LibzVersion:      ', tCurl.LibzVersion);
  if tCurl.Features.AsynchDns then begin
    WriteLn('AresVersionString: ', tCurl.AresVersionString);
    WriteLn('AresVersionNumber: ', tCurl.AresVersionNumber);
  end;
  if tCurl.Features.Idn then WriteLn('LibIdnVersion:      ', tCurl.LibIdnVersion);

  WriteLn('Supported Protocols:');
  for i:= 0 to tCurl.Protocols.Count -1 do Write( ' ', tCurl.Protocols[I]);
  WriteLn;
  WriteLn('Features:');
  WriteLn(' Ipv6         ', YesNo(tCurl.Features.Ipv6));
  WriteLn(' Kerberos4    ', YesNo(tCurl.Features.Kerberos4));
  WriteLn(' Ssl          ', YesNo(tCurl.Features.Ssl));
  WriteLn(' Libz         ', YesNo(tCurl.Features.Libz));
  WriteLn(' Ntlm         ', YesNo(tCurl.Features.Ntlm));
  WriteLn(' GssNegotiate ', YesNo(tCurl.Features.GssNegotiate));
  WriteLn(' Debug        ', YesNo(tCurl.Features.Debug));
  WriteLn(' AsynchDns    ', YesNo(tCurl.Features.AsynchDns));
  WriteLn(' Spnego       ', YesNo(tCurl.Features.Spnego));
  WriteLn(' LargeFile    ', YesNo(tCurl.Features.LargeFile));
  WriteLn(' Idn          ', YesNo(tCurl.Features.Idn));
  WriteLn(' Sspi         ', YesNo(tCurl.Features.Sspi));
end.
