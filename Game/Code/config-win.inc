{***************************************************************** 
 * Configuration file for UltraStar Deluxe 1.1
 *****************************************************************}

{* Libraries *}

{$DEFINE HaveFFmpeg}
{$IF Defined(HaveFFmpeg) and Defined(IncludeConstants)}
  av__codec = 'avcodec-51';
  LIBAVCODEC_VERSION_MAJOR   = 51;
  LIBAVCODEC_VERSION_MINOR   = 16;
  LIBAVCODEC_VERSION_RELEASE = 0;

  av__format = 'avformat-50';
  LIBAVFORMAT_VERSION_MAJOR   = 50;
  LIBAVFORMAT_VERSION_MINOR   = 5;
  LIBAVFORMAT_VERSION_RELEASE = 0;

  av__util = 'avutil-49';
  LIBAVUTIL_VERSION_MAJOR   = 49;
  LIBAVUTIL_VERSION_MINOR   = 0;
  LIBAVUTIL_VERSION_RELEASE = 1;
{$IFEND}

{$UNDEF HaveSWScale}
{$IF Defined(HaveSWScale) and Defined(IncludeConstants)}
  sw__scale = 'swscale-0';
  LIBSWSCALE_VERSION_MAJOR   = 0;
  LIBSWSCALE_VERSION_MINOR   = 5;
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

