program lazarustest;

{$MODE Delphi}

uses

  avcodec      in 'avcodec.pas',
  avformat     in 'avformat.pas',
  avutil       in 'avutil.pas',
  rational     in 'rational.pas',
  opt          in 'opt.pas',
  avio         in 'avio.pas',
  sysutils;

begin
  // This compiles with all units in..
  // but I cant run it, if its compiled with lazarus or delphi
  // I get errors about not finding functions in dll's
  
  // ON LINUX - Ubuntu .. you will probably need to
  // sudo apt-get install libavcodec-dev libavformat-dev

  try
    av_register_all();
    writeln( 'YES - If you see this then ffmpeg is probably lazarus compatible' );
  except
    writeln( 'NO  - ffmpeg is NOT lazarus compatible' );
  end;
end.

