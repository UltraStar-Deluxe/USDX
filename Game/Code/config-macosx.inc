{***************************************************************** 
 * Configuration file for UltraStar Deluxe 1.1
 *****************************************************************}
 
{* Libraries *}

{$DEFINE HaveFFMpeg}
{$IF Defined(HaveFFMpeg) and Defined(IncludeConstants)}
  av__codec = 'libavcodec.dylib';
  LIBAVCODEC_VERSION_MAJOR   = 51;
  LIBAVCODEC_VERSION_MINOR   = 0;
  LIBAVCODEC_VERSION_RELEASE = 0;

  av__format = 'libavformat.dylib';
  LIBAVFORMAT_VERSION_MAJOR   = 52;
  LIBAVFORMAT_VERSION_MINOR   = 0;
  LIBAVFORMAT_VERSION_RELEASE = 0;

  av__util = 'libavutil.dylib';
  LIBAVUTIL_VERSION_MAJOR   = 49;
  LIBAVUTIL_VERSION_MINOR   = 0;
  LIBAVUTIL_VERSION_RELEASE = 0;
{$IFEND}

{$UNDEF HaveSWScale}
{$IF Defined(HaveSWScale) and Defined(IncludeConstants)}
  sw__scale = 'libswscale.dylib';
  LIBSWSCALE_VERSION_MAJOR   = 0;
  LIBSWSCALE_VERSION_MINOR   = 5;
  LIBSWSCALE_VERSION_RELEASE = 0;
{$IFEND}

{$UNDEF HaveProjectM}
{$IF Defined(HaveProjectM) and Defined(IncludeConstants)}
  ProjectM_DataDir = 'Visuals\projectM';
  PROJECTM_VERSION_MAJOR   = 0;
  PROJECTM_VERSION_MINOR   = 98;
  PROJECTM_VERSION_RELEASE = 0;
{$IFEND}

{$UNDEF HavePortaudio}
{$IF Defined(HavePortaudio) and Defined(IncludeConstants)}
  PORTAUDIO_VERSION_MAJOR   = 19;
  PORTAUDIO_VERSION_MINOR   = 0;
  PORTAUDIO_VERSION_RELEASE = 0;
{$IFEND}

{$UNDEF HavePortmixer}

