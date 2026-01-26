{*****************************************************************
 * Configuration file for UltraStar Deluxe
 *****************************************************************}

{* Libraries *}

{$IF Defined(IncludeConstants)}
  LUA_LIB_NAME        = 'lua';
  LUA_VERSION_INT     = 5004006;
  LUA_VERSION_RELEASE = '6';
  LUA_VERSION_MINOR   = '4';
  LUA_VERSION_MAJOR   = '5';
  LUA_VERSION         = '5.4.6';
  LUA_INTEGER_BITS    = 64;
{$IFEND}

{$DEFINE HaveFFmpeg}
//the required DLLs can be built with MXE
{$IF Defined(HaveFFmpeg)}
  {$MACRO ON}
  {$IFNDEF FFMPEG_DIR}
    {$DEFINE FFMPEG_DIR := 'ffmpeg-7.0'}
  {$ENDIF}
  {$IF Defined(IncludeConstants)}
    FFMPEG_VERSION_INT = 7000000;
    av__codec = 'avcodec-61';
    LIBAVCODEC_VERSION_MAJOR   = 61;
    LIBAVCODEC_VERSION_MINOR   = 3;
    LIBAVCODEC_VERSION_RELEASE = 100;

    av__format = 'avformat-61';
    LIBAVFORMAT_VERSION_MAJOR   = 61;
    LIBAVFORMAT_VERSION_MINOR   = 1;
    LIBAVFORMAT_VERSION_RELEASE = 100;

    av__util = 'avutil-59';
    LIBAVUTIL_VERSION_MAJOR   = 59;
    LIBAVUTIL_VERSION_MINOR   = 8;
    LIBAVUTIL_VERSION_RELEASE = 100;
  {$IFEND}
{$IFEND}

{$DEFINE HaveSWResample}
{$IF Defined(HaveSWScale) and Defined(IncludeConstants)}
  sw__resample = 'swresample-5';
  LIBSWRESAMPLE_VERSION_MAJOR   = 5;
  LIBSWRESAMPLE_VERSION_MINOR   = 1;
  LIBSWRESAMPLE_VERSION_RELEASE = 100;
{$IFEND}

{$DEFINE HaveSWScale}
{$IF Defined(HaveSWScale) and Defined(IncludeConstants)}
  sw__scale = 'swscale-8';
  LIBSWSCALE_VERSION_MAJOR   = 8;
  LIBSWSCALE_VERSION_MINOR   = 1;
  LIBSWSCALE_VERSION_RELEASE = 100;
{$IFEND}

{$DEFINE HaveProjectM}
{$IF Defined(HaveProjectM) and Defined(IncludeConstants)}
  ProjectM_DataDir = 'visuals\projectM';
  PROJECTM_VERSION_MAJOR   = 2;
  PROJECTM_VERSION_MINOR   = 2;
  PROJECTM_VERSION_RELEASE = 1;
{$IFEND}

{$UNDEF HavePortaudio}
{$IF Defined(HavePortaudio) and Defined(IncludeConstants)}
  PORTAUDIO_VERSION_MAJOR   = 19;
  PORTAUDIO_VERSION_MINOR   = 0;
  PORTAUDIO_VERSION_RELEASE = 0;
{$IFEND}

{$UNDEF HavePortmixer}

{$DEFINE UseMIDIPort}
