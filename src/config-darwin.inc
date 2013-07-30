{***************************************************************** 
 * Configuration file for ultrastardx 1.1
 * src/config-darwin.inc.  Generated from config.inc.in by configure.
 *****************************************************************}

{* Libraries *}

{$IF Defined(IncludeConstants)}
  LUA_LIB_NAME        = 'lua';
  LUA_VERSION_INT     = 5001004;
  LUA_VERSION_RELEASE = '4';
  LUA_VERSION_MINOR   = '1';
  LUA_VERSION_MAJOR   = '5';
  LUA_VERSION         = '5.1.4';
{$IFEND}

{$DEFINE HaveFFmpeg}
{$IF Defined(HaveFFmpeg)}
  {$MACRO ON}
  {$IFNDEF FFMPEG_DIR}
    {$IF (0 > 0) or (1 > 0)}
      {$DEFINE FFMPEG_DIR := 'ffmpeg-1.0'}
    {$ELSE}
      {$DEFINE FFMPEG_DIR := 'ffmpeg'}
      {$DEFINE useOLD_FFMPEG}
    {$IFEND}
  {$ENDIF}
  {$IF Defined(IncludeConstants)}
    av__codec = 'libavcodec';
    LIBAVCODEC_VERSION_MAJOR   = 54;
    LIBAVCODEC_VERSION_MINOR   = 59;
    LIBAVCODEC_VERSION_RELEASE = 100;

    av__format = 'libavformat';
    LIBAVFORMAT_VERSION_MAJOR   = 54;
    LIBAVFORMAT_VERSION_MINOR   = 29;
    LIBAVFORMAT_VERSION_RELEASE = 104;

    av__util = 'libavutil';
    LIBAVUTIL_VERSION_MAJOR   = 51;
    LIBAVUTIL_VERSION_MINOR   = 73;
    LIBAVUTIL_VERSION_RELEASE = 101;
  {$IFEND}
{$IFEND}

{$DEFINE HaveSWScale}
{$IF Defined(HaveSWScale) and Defined(IncludeConstants)}
  sw__scale = 'libswscale';
  LIBSWSCALE_VERSION_MAJOR   = 2;
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
  LIBPCRE_LIBDIR   = '/sw/lib/libpcre1';
{$IFEND}

{$UNDEF HavePortmixer}
