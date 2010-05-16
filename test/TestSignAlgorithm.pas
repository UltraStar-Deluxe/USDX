program TestSignAlgorithm;

uses
  sysutils;

const
  a1 = 2;
  a2 = 1;
  a3 = 0;
  a4 = -1;
  a5 = -2;

var
  index, number: longint;

begin
  writeln;
  writeln ('This tests the arithmetic procedure used in libavutil and libavcodec.');
  writeln ('positive numbers should give 1, negative numbers -1.');
  writeln ('-17: ', not((-17 shr 30) and $00000002) + 2);
  writeln ('-16: ', not((-16 shr 30) and $00000002) + 2);
  writeln ('-15: ', not((-15 shr 30) and $00000002) + 2);
  writeln (' -3: ', not(( -3 shr 30) and $00000002) + 2);
  writeln (' -2: ', not(( -2 shr 30) and $00000002) + 2);
  writeln (' -1: ', not(( -1 shr 30) and $00000002) + 2);
  writeln ('  0: ', not((  0 shr 30) and $00000002) + 2);
  writeln ('  1: ', not((  1 shr 30) and $00000002) + 2);
  writeln ('  2: ', not((  2 shr 30) and $00000002) + 2);
  writeln ('  3: ', not((  3 shr 30) and $00000002) + 2);
  writeln ('  8: ', not((  8 shr 30) and $00000002) + 2);
  writeln ('MaxInt:     ', MaxInt:12, ' ', IntToHex(MaxInt,8), ' ', not((MaxInt shr 30) and $00000002) + 2);
  writeln ('MaxLongint: ', MaxLongint:12, ' ', IntToHex(MaxLongint,8), ' ', not((MaxLongint shr 30) and $00000002) + 2);
  writeln ('MinInt:     ', -MaxInt - 1:12, ' ', IntToHex(-MaxInt - 1,8), ' ', not(((-MaxInt - 1) shr 30) and $00000002) + 2);
  writeln ('MinLongInt: ', -MaxLongint - 1:12, ' ', IntToHex(-MaxLongint - 1,8), ' ', not(((-MaxLongint - 1) shr 30) and $00000002) + 2);
  writeln (a1, ' ', IntToHex(a1,8) , ' ', not((a1 shr 30) and $00000002) + 2);
  writeln (a2, ' ', IntToHex(a2,8) , ' ', not((a2 shr 30) and $00000002) + 2);
  writeln (a3, ' ', IntToHex(a3,8) , ' ', not((a3 shr 30) and $00000002) + 2);
  writeln (a4, ' ', IntToHex(a4,8) , ' ', not((a4 shr 30) and $00000002) + 2);
  writeln (a5, ' ', IntToHex(a5,8) , ' ', not((a5 shr 30) and $00000002) + 2);
  writeln;
  writeln ('Hit RETURN for more positive numbers. The last one overflows and becomes negative.');
  readln;
  number := 1;
  for index := 1 to 32 do
  begin
    writeln (index:2, ': ', number:11, ' ', IntToHex(number shr 30 ,8):10, ' ', not((number shr 30) and $00000002) + 2);
    number := number * 2;
  end;
  writeln;
  writeln ('Hit RETURN for more positive numbers.');
  readln;
  number := -1;
  for index := 1 to 32 do
  begin
    writeln (index:2, ': ', number:11, ' ', IntToHex(number shr 30 ,8):10, ' ', not((number shr 30) and $00000002) + 2);
    number := number * 2;
  end;
end.