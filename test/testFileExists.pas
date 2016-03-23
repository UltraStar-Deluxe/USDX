program testFileExists;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  SysUtils;

const
  name1 = 'aa.txt';
  name2 = 'aä.txt';
	name3 = 'aa' + char($CC) + char($88) + '.txt';
	
begin
	writeln (name1, ' found: ', SysUtils.FileExists(name1));
	writeln (name2, ' found: ', SysUtils.FileExists(name2));
	writeln (name3, ' found: ', SysUtils.FileExists(name3));
	writeln (    name2[1] :4,     name2[2] :4,     name2[3] :4,     name2[4] :4,     name2[5] :4);
	writeln (ord(name2[1]):4, ord(name2[2]):4, ord(name2[3]):4, ord(name2[4]):4, ord(name2[5]):4);
	writeln (hexstr(ord(name2[1]),2):4, hexstr(ord(name2[2]),2):4, hexstr(ord(name2[3]),2):4, hexstr(ord(name2[4]),2):4, hexstr(ord(name2[5]),2):4);
	writeln (    name3[1] :4,     name3[2] :4,     name3[3] :4,     name3[4] :4,     name3[5] :4);
	writeln (ord(name3[1]):4, ord(name3[2]):4, ord(name3[3]):4, ord(name3[4]):4, ord(name3[5]):4);
	writeln (hexstr(ord(name3[1]),2):4, hexstr(ord(name3[2]),2):4, hexstr(ord(name3[3]),2):4, hexstr(ord(name3[4]),2):4, hexstr(ord(name3[5]),2):4);
end.