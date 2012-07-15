{*****************************************************************
 * Configuration file for UltraStar Deluxe 1.1
 *****************************************************************}

{* Libraries *}

{$DEFINE HaveFFmpeg}

{$IF Defined(HaveFFmpeg)}
  {$IFNDEF FFMPEG_DIR}
    {$DEFINE FFMPEG_DIR := 'ffmpeg-0.10'}
  {$ENDIF}
  {$IFNDEF AVCODEC_PATH}
    {$DEFINE AVCODEC_PATH  := 'lib\ffmpeg-0.10\avcodec.pas'}
  {$ENDIF}
  {$IFNDEF AVFORMAT_PATH}
    {$DEFINE AVFORMAT_PATH := 'lib\ffmpeg-0.10\avformat.pas'}
  {$ENDIF}
  {$IFNDEF AVUTIL_PATH}
    {$DEFINE AVUTIL_PATH   := 'lib\ffmpeg-0.10\avutil.pas'}
  {$ENDIF}
  {$IFNDEF RATIONAL_PATH}
    {$DEFINE RATIONAL_PATH := 'lib\ffmpeg-0.10\rational.pas'}
  {$ENDIF}
  {$IFNDEF AVIO_PATH}
    {$DEFINE AVIO_PATH     := 'lib\ffmpeg-0.10\avio.pas'}
  {$ENDIF}
  {$IFNDEF SWSCALE_PATH}
    {$DEFINE SWSCALE_PATH  := 'lib\ffmpeg-0.10\swscale.pas'}
  {$ENDIF}
  {$IF Defined(IncludeConstants)}
    av__codec = 'avcodec-52';
    LIBAVCODEC_VERSION_MAJOR   = 52;
    LIBAVCODEC_VERSION_MINOR   = 67;
    LIBAVCODEC_VERSION_RELEASE = 2;

    av__format = 'avformat-52';
    LIBAVFORMAT_VERSION_MAJOR   = 52;
    LIBAVFORMAT_VERSION_MINOR   = 62;
    LIBAVFORMAT_VERSION_RELEASE = 0;

    av__util = 'avutil-50';
    LIBAVUTIL_VERSION_MAJOR   = 50;
    LIBAVUTIL_VERSION_MINOR   = 15;
    LIBAVUTIL_VERSION_RELEASE = 2;
  {$IFEND}
{$IFEND}

{$DEFINE HaveSWScale}
{$IF Defined(HaveSWScale) and Defined(IncludeConstants)}
  sw__scale = 'swscale-0';
  LIBSWSCALE_VERSION_MAJOR   = 0;
  LIBSWSCALE_VERSION_MINOR   = 10;
  LIBSWSCALE_VERSION_RELEASE = 0;
{$IFEND}

{$DEFINE HaveProjectM}
{$IF Defined(HaveProjectM) and Defined(IncludeConstants)}
  ProjectM_DataDir = 'Visuals\projectM';
  PROJECTM_VERSION_MAJOR   = 1;
  PROJECTM_VERSION_MINOR   = 10;
  PROJECTM_VERSION_RELEASE = 0;
{$IFEND}

{$UNDEF HavePortaudio}
{$IF Defined(HavePortaudio) and Defined(IncludeConstants)}
  PORTAUDIO_VERSION_MAJOR   = 19;
  PORTAUDIO_VERSION_MINOR   = 0;
  PORTAUDIO_VERSION_RELEASE = 0;
{$IFEND}

{$UNDEF HavePortmixer}

{$UNDEF HaveLibsamplerate}
{$IF Defined(HaveLibsamplerate) and Defined(IncludeConstants)}
  LIBSAMPLERATE_VERSION_MAJOR   = 0;
  LIBSAMPLERATE_VERSION_MINOR   = 1;
  LIBSAMPLERATE_VERSION_RELEASE = 3;
{$IFEND}
