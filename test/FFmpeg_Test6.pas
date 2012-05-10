Program FFmpeg_Test6;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

(**
 * Test of function TFFmpegDecodeStream.Open in media/UAudioDecoder_FFmpeg.pas
 * after configure for selecting the installed FFmpeg lib compile with:
 *
 * fpc -Fi../src -Fu../src/base -Fu../src/lib/FFmpeg-0.... FFmpeg_Test6.pas
 *)

uses
  avcodec,
  avformat;

const
  Filename: PAnsiChar = 'menuswoosh.mp3';

var
  fFormatCtx: PAVFormatContext;
  TestFrame: TAVPacket;

begin
  av_register_all;
  if (av_open_input_file(fFormatCtx, Filename, nil, 0, nil) <> 0) then
    writeln('av_open_input_file failed: File not found: ', Filename);

  if (av_read_frame(fFormatCtx, TestFrame) < 0) then
    writeln('av_read_frame failed');
    
  dump_format(fFormatCtx, 0, argv[1], 0);

  if (fFormatCtx <> nil) then
  begin
    av_close_input_file(fFormatCtx);
    fFormatCtx := nil;
  end;
end.
