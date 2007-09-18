program lazarustest;

uses
  pngimage in 'pngimage.pas',
  pnglang  in 'pnglang.pas',
  pngzlib  in 'pngzlib.pas',
  sysutils;

begin
  writeln( 'pngimage is NOT lazarus compatible' );
  writeln( 'It might compile ( not link though ), however the object files are in borland obj format' );
  writeln( 'to use this, it will need to be in GCC object file format format' );
  writeln( 'Or we can use the lazarus / freepascal png unit' );
end.

