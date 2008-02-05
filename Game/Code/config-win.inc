{***************************************************************** 
 * Configuration file for UltraStar Deluxe 1.1
 *****************************************************************}

{* Misc options *}

{$DEFINE DEBUG}

{* Libraries *}

{$IF Defined(LAZARUS) and Defined(IncludeConstants)}
  LAZARUS_VERSION_MAJOR   = 0;
  LAZARUS_VERSION_MINOR   = 9;
  LAZARUS_VERSION_RELEASE = 24;
{$IFEND}

{$DEFINE HaveFFMpeg}
{$IF Defined(HaveFFMpeg) and Defined(IncludeConstants)}
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
  LIBAVUTIL_VERSION_RELEASE = 0;
{$IFEND}

{$DEFINE HaveSWScale}
{$IF Defined(HaveSWScale) and Defined(IncludeConstants)}
  sw__scale = 'swscale-0';
  LIBSWSCALE_VERSION_MAJOR   = 0;
  LIBSWSCALE_VERSION_MINOR   = 5;
  LIBSWSCALE_VERSION_RELEASE = 0;
{$IFEND}

// define this for versions of ProjectM <  1.0 (use C-Interface)
{$DEFINE HaveProjectM_0_9}
// define this for versions of ProjectM >= 1.0 (use C++-Interface)
{$UNDEF HaveProjectM_1_0_PLUS}
{$IF Defined(HaveProjectM_0_9) or Defined(HaveProjectM_1_0_PLUS)}
  {$DEFINE HaveProjectM}
{$IFEND}
{$IF Defined(HaveProjectM) and Defined(IncludeConstants)}
  libprojectM = 'libprojectM';
  // Note: Un/Define HaveProjectM_0_9 or HaveProjectM_1_0_PLUS accordingly
  PROJECTM_VERSION_MAJOR   = 0;
  PROJECTM_VERSION_MINOR   = 98;
  PROJECTM_VERSION_RELEASE = 0;
{$IFEND}

{$UNDEF HavePortaudio}
{$IF Defined(HavePortaudio) and Defined(IncludeConstants)}
  libportaudio = 'portaudio_x86';
  PORTAUDIO_VERSION_MAJOR   = 19;
  PORTAUDIO_VERSION_MINOR   = 0;
  PORTAUDIO_VERSION_RELEASE = 0;
{$IFEND}

{$UNDEF HavePortmixer}
{$IF Defined(HavePortmixer) and Defined(IncludeConstants)}
  libportmixer = 'portmixer';
{$IFEND}

