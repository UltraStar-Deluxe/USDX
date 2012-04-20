{***************************************************************** 
 * Configuration file for ultrastardx 1.1
 * src/config-darwin.inc.  Generated from config.inc.in by configure.
 *****************************************************************}

{* Libraries *}

{$IF Defined(IncludeConstants)}
  lua_lib_name = 'lua';
{$IFEND}

{$DEFINE HaveFFmpeg}
{$IF Defined(HaveFFmpeg)}
  {$MACRO ON}
  {$IFNDEF FFMPEG_DIR}
    {$IF 7 > 0}
      {$DEFINE FFMPEG_DIR := 'ffmpeg-0.7'}
    {$ELSE}
      {$DEFINE FFMPEG_DIR := 'ffmpeg'}
    {$IFEND}
  {$IFEND}
  {$IF Defined(IncludeConstants)}
  av__codec = 'libavcodec';
  LIBAVCODEC_VERSION_MAJOR   = 52;
  LIBAVCODEC_VERSION_MINOR   = 123;
  LIBAVCODEC_VERSION_RELEASE = 0;

  av__format = 'libavformat';
  LIBAVFORMAT_VERSION_MAJOR   = 52;
  LIBAVFORMAT_VERSION_MINOR   = 111;
  LIBAVFORMAT_VERSION_RELEASE = 0;

  av__util = 'libavutil';
  LIBAVUTIL_VERSION_MAJOR   = 50;
  LIBAVUTIL_VERSION_MINOR   = 43;
  LIBAVUTIL_VERSION_RELEASE = 0;
  
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
