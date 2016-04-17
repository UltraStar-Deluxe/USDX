{***************************************************************** 
 * Configuration file for ultrastardx 1.1
 * src/config-darwin.inc.  Generated from config.inc.in by configure.
 *****************************************************************}

{* Libraries *}

{$IF Defined(IncludeConstants)}
  lua_lib_name = '';
  LUA_VERSION_INT     = 5002004;
  LUA_VERSION_RELEASE = '4';
  LUA_VERSION_MINOR   = '2';
  LUA_VERSION_MAJOR   = '5';
  LUA_VERSION         = '5.2.4';
{$IFEND}

{$DEFINE HaveFFmpeg}
{$IF Defined(HaveFFmpeg)}
  {$MACRO ON}
  {$IFNDEF FFMPEG_DIR}
    {$IF (8 > 0) or (2 > 0)}
      {$DEFINE FFMPEG_DIR := 'ffmpeg-2.8'}
      {$DEFINE FF_API_OLD_DECODE_AUDIO}
    {$ELSE}
      {$DEFINE FFMPEG_DIR := 'ffmpeg'}
      {$DEFINE useOLD_FFMPEG}
    {$IFEND}
  {$ENDIF}
  {$IF Defined(IncludeConstants)}
    FFMPEG_VERSION_INT = 2008000;

  av__codec = 'libavcodec';
  LIBAVCODEC_VERSION_MAJOR   = 56;
  LIBAVCODEC_VERSION_MINOR   = 60;
  LIBAVCODEC_VERSION_RELEASE = 100;

  av__format = 'libavformat';
  LIBAVFORMAT_VERSION_MAJOR   = 56;
  LIBAVFORMAT_VERSION_MINOR   = 40;
  LIBAVFORMAT_VERSION_RELEASE = 101;

  av__util = 'libavutil';
  LIBAVUTIL_VERSION_MAJOR   = 54;
  LIBAVUTIL_VERSION_MINOR   = 31;
  LIBAVUTIL_VERSION_RELEASE = 100;
  {$IFEND}
{$IFEND}

{$DEFINE HaveSWScale}
{$IF Defined(HaveSWScale) and Defined(IncludeConstants)}
  sw__scale = 'libswscale';
  LIBSWSCALE_VERSION_MAJOR   = 3;
  LIBSWSCALE_VERSION_MINOR   = 1;
  LIBSWSCALE_VERSION_RELEASE = 101;
{$IFEND}

{$UNDEF HaveProjectM}
{$IF Defined(HaveProjectM) and Defined(IncludeConstants)}
  ProjectM_DataDir = '';
  PROJECTM_VERSION_MAJOR   = 0;
  PROJECTM_VERSION_MINOR   = 0;
  PROJECTM_VERSION_RELEASE = 0;
{$IFEND}

{$DEFINE HavePortaudio}
{$IF Defined(HavePortaudio) and Defined(IncludeConstants)}
  PORTAUDIO_VERSION_MAJOR   = 19;
  PORTAUDIO_VERSION_MINOR   = 0;
  PORTAUDIO_VERSION_RELEASE = 0;
{$IFEND}

{$DEFINE HaveLibPcre}
{$IF Defined(HaveLibPcre) and Defined(IncludeConstants)}
  LIBPCRE_LIBNAME  = '';
  LIBPCRE_LIBDIR   = '/usr/local/Cellar/pcre/8.38/lib';
{$IFEND}

{$UNDEF HavePortmixer}
