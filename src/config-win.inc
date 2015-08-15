{*****************************************************************
 * Configuration file for UltraStar Deluxe 1.1
 *****************************************************************}

{* Libraries *}

{$IF Defined(IncludeConstants)}
  LUA_LIB_NAME        = 'lua';
  LUA_VERSION_INT     = 5001005;
  LUA_VERSION_RELEASE = '5';
  LUA_VERSION_MINOR   = '1';
  LUA_VERSION_MAJOR   = '5';
  LUA_VERSION         = '5.1.5';
{$IFEND}

{$DEFINE HaveFFmpeg}
//the required DLLs can be downloaded here: https://ffmpeg.zeranoe.com/builds/win32/shared/ffmpeg-20150814-git-affbecb-win32-shared.7z
{$IF Defined(HaveFFmpeg)}
  {$MACRO ON}
  {$IFNDEF FFMPEG_DIR}
    {$IF (4 > 0) or (2 > 0)}
      {$DEFINE FFMPEG_DIR := 'ffmpeg-2.7'}
      {$DEFINE FF_API_OLD_DECODE_AUDIO}
    {$ELSE}
      {$DEFINE FFMPEG_DIR := 'ffmpeg'}
      {$DEFINE useOLD_FFMPEG}
    {$IFEND}
  {$ENDIF}
  {$IF Defined(IncludeConstants)}
    FFMPEG_VERSION_INT = 2007000;
    av__codec = 'avcodec-56';
    LIBAVCODEC_VERSION_MAJOR   = 56;
    LIBAVCODEC_VERSION_MINOR   = 57;
    LIBAVCODEC_VERSION_RELEASE = 100;

    av__format = 'avformat-56';
    LIBAVFORMAT_VERSION_MAJOR   = 56;
    LIBAVFORMAT_VERSION_MINOR   = 40;
    LIBAVFORMAT_VERSION_RELEASE = 101;

    av__util = 'avutil-54';
    LIBAVUTIL_VERSION_MAJOR   = 54;
    LIBAVUTIL_VERSION_MINOR   = 30;
    LIBAVUTIL_VERSION_RELEASE = 100;
  {$IFEND}
{$IFEND}

{$DEFINE HaveSWScale}
{$IF Defined(HaveSWScale) and Defined(IncludeConstants)}
  sw__scale = 'swscale-3';
  LIBSWSCALE_VERSION_MAJOR   = 3;
  LIBSWSCALE_VERSION_MINOR   = 1;
  LIBSWSCALE_VERSION_RELEASE = 101;
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
