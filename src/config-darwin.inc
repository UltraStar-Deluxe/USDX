{***************************************************************** 
 * Configuration file for ultrastardx 1.1
 * src/config-darwin.inc.  Generated from config.inc.in by configure.
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
{$IF Defined(HaveFFmpeg)}
  {$MACRO ON}
  {$IFNDEF FFMPEG_DIR}
    {$IF (8 > 0) or (0 > 0)}
      {$DEFINE FFMPEG_DIR := 'ffmpeg-0.8'}
    {$ELSE}
      {$DEFINE FFMPEG_DIR := 'ffmpeg'}
      {$DEFINE useOLD_FFMPEG}
    {$IFEND}
  {$ENDIF}
  {$IF Defined(IncludeConstants)}
    av__codec = 'libavcodec';
    LIBAVCODEC_VERSION_MAJOR   = 53;
    LIBAVCODEC_VERSION_MINOR   = 8;
    LIBAVCODEC_VERSION_RELEASE = 0;

    av__format = 'libavformat';
    LIBAVFORMAT_VERSION_MAJOR   = 53;
    LIBAVFORMAT_VERSION_MINOR   = 5;
    LIBAVFORMAT_VERSION_RELEASE = 0;

    av__util = 'libavutil';
    LIBAVUTIL_VERSION_MAJOR   = 51;
    LIBAVUTIL_VERSION_MINOR   = 9;
    LIBAVUTIL_VERSION_RELEASE = 1;
  {$IFEND}
{$IFEND}

{$DEFINE HaveSWScale}
{$IF Defined(HaveSWScale) and Defined(IncludeConstants)}
  sw__scale = 'libswscale';
  LIBSWSCALE_VERSION_MAJOR   = 2;
  LIBSWSCALE_VERSION_MINOR   = 0;
  LIBSWSCALE_VERSION_RELEASE = 0;
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
  LIBPCRE_LIBDIR   = '/sw/lib/libpcre1';
{$IFEND}

{$UNDEF HavePortmixer}
