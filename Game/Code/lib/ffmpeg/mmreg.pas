unit mmreg;

interface

uses
  windows, mmsystem;

(*++

Copyright 1991-1998 Microsoft Corporation

Module Name:

    mmreg.h

Abstract:

    Multimedia Registration

Revision History:

  Translated to .pas - Zinetz Victor, Dec. 2005
                       mail@zinetz.info

--*)

// Define the following to skip definitions
//
// NOMMIDS      Multimedia IDs are not defined
// NONEWWAVE    No new waveform types are defined except WAVEFORMATEX
// NONEWRIFF    No new RIFF forms are defined
// NOJPEGDIB    No JPEG DIB definitions
// NONEWIC      No new Image Compressor types are defined
// NOBITMAP     No extended bitmap info header definition

(* manufacturer IDs *)
const
  MM_MICROSOFT                 = 1;          //*  Microsoft Corporation  */
  MM_CREATIVE                  = 2;          //*  Creative Labs, Inc */
  MM_MEDIAVISION               = 3;          (*  Media Vision, Inc. *)
  MM_FUJITSU                   = 4;          (*  Fujitsu Corp. *)
  MM_ARTISOFT                  = 20;         (*  Artisoft, Inc. *)
  MM_TURTLE_BEACH              = 21;         (*  Turtle Beach, Inc. *)
  MM_IBM                       = 22;         (*  IBM Corporation *)
  MM_VOCALTEC                  = 23;         (*  Vocaltec LTD. *)
  MM_ROLAND                    = 24;         (*  Roland *)
  MM_DSP_SOLUTIONS             = 25;         (*  DSP Solutions, Inc. *)
  MM_NEC                       = 26;         (*  NEC *)
  MM_ATI                       = 27;         (*  ATI *)
  MM_WANGLABS                  = 28;         (*  Wang Laboratories, Inc *)
  MM_TANDY                     = 29;         (*  Tandy Corporation *)
  MM_VOYETRA                   = 30;         (*  Voyetra *)
  MM_ANTEX                     = 31;         (*  Antex Electronics Corporation *)
  MM_ICL_PS                    = 32;         (*  ICL Personal Systems *)
  MM_INTEL                     = 33;         (*  Intel Corporation *)
  MM_GRAVIS                    = 34;         (*  Advanced Gravis *)
  MM_VAL                       = 35;         (*  Video Associates Labs, Inc. *)
  MM_INTERACTIVE               = 36;         (*  InterActive Inc *)
  MM_YAMAHA                    = 37;         (*  Yamaha Corporation of America *)
  MM_EVEREX                    = 38;         (*  Everex Systems, Inc *)
  MM_ECHO                      = 39;         (*  Echo Speech Corporation *)
  MM_SIERRA                    = 40;         (*  Sierra Semiconductor Corp *)
  MM_CAT                       = 41;         (*  Computer Aided Technologies *)
  MM_APPS                      = 42;         (*  APPS Software International *)
  MM_DSP_GROUP                 = 43;         (*  DSP Group, Inc *)
  MM_MELABS                    = 44;         (*  microEngineering Labs *)
  MM_COMPUTER_FRIENDS          = 45;         (*  Computer Friends, Inc. *)
  MM_ESS                       = 46;         (*  ESS Technology *)
  MM_AUDIOFILE                 = 47;         (*  Audio, Inc. *)
  MM_MOTOROLA                  = 48;         (*  Motorola, Inc. *)
  MM_CANOPUS                   = 49;         (*  Canopus, co., Ltd. *)
  MM_EPSON                     = 50;         (*  Seiko Epson Corporation *)
  MM_TRUEVISION                = 51;         (*  Truevision *)
  MM_AZTECH                    = 52;         (*  Aztech Labs, Inc. *)
  MM_VIDEOLOGIC                = 53;         (*  Videologic *)
  MM_SCALACS                   = 54;         (*  SCALACS *)
  MM_KORG                      = 55;         (*  Korg Inc. *)
  MM_APT                       = 56;         (*  Audio Processing Technology *)
  MM_ICS                       = 57;         (*  Integrated Circuit Systems, Inc. *)
  MM_ITERATEDSYS               = 58;         (*  Iterated Systems, Inc. *)
  MM_METHEUS                   = 59;         (*  Metheus *)
  MM_LOGITECH                  = 60;         (*  Logitech, Inc. *)
  MM_WINNOV                    = 61;         (*  Winnov, Inc. *)
  MM_NCR                       = 62;         (*  NCR Corporation *)
  MM_EXAN                      = 63;         (*  EXAN *)
  MM_AST                       = 64;         (*  AST Research Inc. *)
  MM_WILLOWPOND                = 65;         (*  Willow Pond Corporation *)
  MM_SONICFOUNDRY              = 66;         (*  Sonic Foundry *)
  MM_VITEC                     = 67;         (*  Vitec Multimedia *)
  MM_MOSCOM                    = 68;         (*  MOSCOM Corporation *)
  MM_SILICONSOFT               = 69;         (*  Silicon Soft, Inc. *)
  MM_SUPERMAC                  = 73;         (*  Supermac *)
  MM_AUDIOPT                   = 74;         (*  Audio Processing Technology *)
  MM_SPEECHCOMP                = 76;         (*  Speech Compression *)
  MM_AHEAD                     = 77;         (*  Ahead, Inc. *)
  MM_DOLBY                     = 78;         (*  Dolby Laboratories *)
  MM_OKI                       = 79;         (*  OKI *)
  MM_AURAVISION                = 80;         (*  AuraVision Corporation *)
  MM_OLIVETTI                  = 81;         (*  Ing C. Olivetti & C., S.p.A. *)
  MM_IOMAGIC                   = 82;         (*  I/O Magic Corporation *)
  MM_MATSUSHITA                = 83;         (*  Matsushita Electric Industrial Co., LTD. *)
  MM_CONTROLRES                = 84;         (*  Control Resources Limited *)
  MM_XEBEC                     = 85;         (*  Xebec Multimedia Solutions Limited *)
  MM_NEWMEDIA                  = 86;         (*  New Media Corporation *)
  MM_NMS                       = 87;         (*  Natural MicroSystems *)
  MM_LYRRUS                    = 88;         (*  Lyrrus Inc. *)
  MM_COMPUSIC                  = 89;         (*  Compusic *)
  MM_OPTI                      = 90;         (*  OPTi Computers Inc. *)
  MM_ADLACC                    = 91;         (*  Adlib Accessories Inc. *)
  MM_COMPAQ                    = 92;         (*  Compaq Computer Corp. *)
  MM_DIALOGIC                  = 93;         (*  Dialogic Corporation *)
  MM_INSOFT                    = 94;         (*  InSoft, Inc. *)
  MM_MPTUS                     = 95;         (*  M.P. Technologies, Inc. *)
  MM_WEITEK                    = 96;         (*  Weitek *)
  MM_LERNOUT_AND_HAUSPIE       = 97;         (*  Lernout & Hauspie *)
  MM_QCIAR                     = 98;         (*  Quanta Computer Inc. *)
  MM_APPLE                     = 99;         (*  Apple Computer, Inc. *)
  MM_DIGITAL                   = 100;        (*  Digital Equipment Corporation *)
  MM_MOTU                      = 101;        (*  Mark of the Unicorn *)
  MM_WORKBIT                   = 102;        (*  Workbit Corporation *)
  MM_OSITECH                   = 103;        (*  Ositech Communications Inc. *)
  MM_MIRO                      = 104;        (*  miro Computer Products AG *)
  MM_CIRRUSLOGIC               = 105;        (*  Cirrus Logic *)
  MM_ISOLUTION                 = 106;        (*  ISOLUTION  B.V. *)
  MM_HORIZONS                  = 107;        (*  Horizons Technology, Inc *)
  MM_CONCEPTS                  = 108;        (*  Computer Concepts Ltd *)
  MM_VTG                       = 109;        (*  Voice Technologies Group, Inc. *)
  MM_RADIUS                    = 110;        (*  Radius *)
  MM_ROCKWELL                  = 111;        (*  Rockwell International *)
  MM_XYz                       = 112;        (*  Co. XYZ for testing *)
  MM_OPCODE                    = 113;        (*  Opcode Systems *)
  MM_VOXWARE                   = 114;        (*  Voxware Inc *)
  MM_NORTHERN_TELECOM          = 115;        (*  Northern Telecom Limited *)
  MM_APICOM                    = 116;        (*  APICOM *)
  MM_GRANDE                    = 117;        (*  Grande Software *)
  MM_ADDX                      = 118;        (*  ADDX *)
  MM_WILDCAT                   = 119;        (*  Wildcat Canyon Software *)
  MM_RHETOREX                  = 120;        (*  Rhetorex Inc *)
  MM_BROOKTREE                 = 121;        (*  Brooktree Corporation *)
  MM_ENSONIQ                   = 125;        (*  ENSONIQ Corporation *)
  MM_FAST                      = 126;        (*  ///FAST Multimedia AG *)
  MM_NVIDIA                    = 127;        (*  NVidia Corporation *)
  MM_OKSORI                    = 128;        (*  OKSORI Co., Ltd. *)
  MM_DIACOUSTICS               = 129;        (*  DiAcoustics, Inc. *)
  MM_GULBRANSEN                = 130;        (*  Gulbransen, Inc. *)
  MM_KAY_ELEMETRICS            = 131;        (*  Kay Elemetrics, Inc. *)
  MM_CRYSTAL                   = 132;        (*  Crystal Semiconductor Corporation *)
  MM_SPLASH_STUDIOS            = 133;        (*  Splash Studios *)
  MM_QUARTERDECK               = 134;        (*  Quarterdeck Corporation *)
  MM_TDK                       = 135;        (*  TDK Corporation *)
  MM_DIGITAL_AUDIO_LABS        = 136;        (*  Digital Audio Labs, Inc. *)
  MM_SEERSYS                   = 137;        (*  Seer Systems, Inc. *)
  MM_PICTURETEL                = 138;        (*  PictureTel Corporation *)
  MM_ATT_MICROELECTRONICS      = 139;        (*  AT&T Microelectronics *)
  MM_OSPREY                    = 140;        (*  Osprey Technologies, Inc. *)
  MM_MEDIATRIX                 = 141;        (*  Mediatrix Peripherals *)
  MM_SOUNDESIGNS               = 142;        (*  SounDesignS M.C.S. Ltd. *)
  MM_ALDIGITAL                 = 143;        (*  A.L. Digital Ltd. *)
  MM_SPECTRUM_SIGNAL_PROCESSING= 144;        (*  Spectrum Signal Processing, Inc. *)
  MM_ECS                       = 145;        (*  Electronic Courseware Systems, Inc. *)
  MM_AMD                       = 146;        (*  AMD *)
  MM_COREDYNAMICS              = 147;        (*  Core Dynamics *)
  MM_CANAM                     = 148;        (*  CANAM Computers *)
  MM_SOFTSOUND                 = 149;        (*  Softsound, Ltd. *)
  MM_NORRIS                    = 150;        (*  Norris Communications, Inc. *)
  MM_DDD                       = 151;        (*  Danka Data Devices *)
  MM_EUPHONICS                 = 152;        (*  EuPhonics *)
  MM_PRECEPT                   = 153;        (*  Precept Software, Inc. *)
  MM_CRYSTAL_NET               = 154;        (*  Crystal Net Corporation *)
  MM_CHROMATIC                 = 155;        (*  Chromatic Research, Inc *)
  MM_VOICEINFO                 = 156;        (*  Voice Information Systems, Inc *)
  MM_VIENNASYS                 = 157;        (*  Vienna Systems *)
  MM_CONNECTIX                 = 158;        (*  Connectix Corporation *)
  MM_GADGETLABS                = 159;        (*  Gadget Labs LLC *)
  MM_FRONTIER                  = 160;        (*  Frontier Design Group LLC *)
  MM_VIONA                     = 161;        (*  Viona Development GmbH *)
  MM_CASIO                     = 162;        (*  Casio Computer Co., LTD *)
  MM_DIAMONDMM                 = 163;        (*  Diamond Multimedia *)
  MM_S3                        = 164;        (*  S3 *)
  MM_FRAUNHOFER_IIS            = 172;        (*  Fraunhofer *)

(* MM_MICROSOFT product IDs *)

  MM_MIDI_MAPPER                    = 1;       (*  Midi Mapper  *)
  MM_WAVE_MAPPER                    = 2;       (*  Wave Mapper  *)
  MM_SNDBLST_MIDIOUT                = 3;       (*  Sound Blaster MIDI output port  *)
  MM_SNDBLST_MIDIIN                 = 4;       (*  Sound Blaster MIDI input port  *)
  MM_SNDBLST_SYNTH                  = 5;       (*  Sound Blaster internal synth  *)
  MM_SNDBLST_WAVEOUT                = 6;       (*  Sound Blaster waveform output  *)
  MM_SNDBLST_WAVEIN                 = 7;       (*  Sound Blaster waveform input  *)
  MM_ADLIB                          = 9;       (*  Ad Lib Compatible synth  *)
  MM_MPU401_MIDIOUT                 = 10;      (*  MPU 401 compatible MIDI output port  *)
  MM_MPU401_MIDIIN                  = 11;      (*  MPU 401 compatible MIDI input port  *)
  MM_PC_JOYSTICK                    = 12;      (*  Joystick adapter  *)

  MM_PCSPEAKER_WAVEOUT              = 13;      (*  PC speaker waveform output  *)
  MM_MSFT_WSS_WAVEIN                = 14;      (*  MS Audio Board waveform input  *)
  MM_MSFT_WSS_WAVEOUT               = 15;      (*  MS Audio Board waveform output  *)
  MM_MSFT_WSS_FMSYNTH_STEREO        = 16;      (*  MS Audio Board  Stereo FM synth  *)
  MM_MSFT_WSS_MIXER                 = 17;      (*  MS Audio Board Mixer Driver  *)
  MM_MSFT_WSS_OEM_WAVEIN            = 18;      (*  MS OEM Audio Board waveform input  *)
  MM_MSFT_WSS_OEM_WAVEOUT           = 19;      (*  MS OEM Audio Board waveform output  *)
  MM_MSFT_WSS_OEM_FMSYNTH_STEREO    = 20;      (*  MS OEM Audio Board Stereo FM Synth  *)
  MM_MSFT_WSS_AUX                   = 21;      (*  MS Audio Board Aux. Port  *)
  MM_MSFT_WSS_OEM_AUX               = 22;      (*  MS OEM Audio Aux Port  *)
  MM_MSFT_GENERIC_WAVEIN            = 23;      (*  MS Vanilla driver waveform input  *)
  MM_MSFT_GENERIC_WAVEOUT           = 24;      (*  MS Vanilla driver wavefrom output  *)
  MM_MSFT_GENERIC_MIDIIN            = 25;      (*  MS Vanilla driver MIDI in  *)
  MM_MSFT_GENERIC_MIDIOUT           = 26;      (*  MS Vanilla driver MIDI  external out  *)
  MM_MSFT_GENERIC_MIDISYNTH         = 27;      (*  MS Vanilla driver MIDI synthesizer  *)
  MM_MSFT_GENERIC_AUX_LINE          = 28;      (*  MS Vanilla driver aux (line in)  *)
  MM_MSFT_GENERIC_AUX_MIC           = 29;      (*  MS Vanilla driver aux (mic)  *)
  MM_MSFT_GENERIC_AUX_CD            = 30;      (*  MS Vanilla driver aux (CD)  *)
  MM_MSFT_WSS_OEM_MIXER             = 31;      (*  MS OEM Audio Board Mixer Driver  *)
  MM_MSFT_MSACM                     = 32;      (*  MS Audio Compression Manager  *)
  MM_MSFT_ACM_MSADPCM               = 33;      (*  MS ADPCM Codec  *)
  MM_MSFT_ACM_IMAADPCM              = 34;      (*  IMA ADPCM Codec  *)
  MM_MSFT_ACM_MSFILTER              = 35;      (*  MS Filter  *)
  MM_MSFT_ACM_GSM610                = 36;      (*  GSM 610 codec  *)
  MM_MSFT_ACM_G711                  = 37;      (*  G.711 codec  *)
  MM_MSFT_ACM_PCM                   = 38;      (*  PCM converter  *)

   // Microsoft Windows Sound System drivers

  MM_WSS_SB16_WAVEIN                = 39;      (*  Sound Blaster 16 waveform input  *)
  MM_WSS_SB16_WAVEOUT               = 40;      (*  Sound Blaster 16  waveform output  *)
  MM_WSS_SB16_MIDIIN                = 41;      (*  Sound Blaster 16 midi-in  *)
  MM_WSS_SB16_MIDIOUT               = 42;      (*  Sound Blaster 16 midi out  *)
  MM_WSS_SB16_SYNTH                 = 43;      (*  Sound Blaster 16 FM Synthesis  *)
  MM_WSS_SB16_AUX_LINE              = 44;      (*  Sound Blaster 16 aux (line in)  *)
  MM_WSS_SB16_AUX_CD                = 45;      (*  Sound Blaster 16 aux (CD)  *)
  MM_WSS_SB16_MIXER                 = 46;      (*  Sound Blaster 16 mixer device  *)
  MM_WSS_SBPRO_WAVEIN               = 47;      (*  Sound Blaster Pro waveform input  *)
  MM_WSS_SBPRO_WAVEOUT              = 48;      (*  Sound Blaster Pro waveform output  *)
  MM_WSS_SBPRO_MIDIIN               = 49;      (*  Sound Blaster Pro midi in  *)
  MM_WSS_SBPRO_MIDIOUT              = 50;      (*  Sound Blaster Pro midi out  *)
  MM_WSS_SBPRO_SYNTH                = 51;      (*  Sound Blaster Pro FM synthesis  *)
  MM_WSS_SBPRO_AUX_LINE             = 52;      (*  Sound Blaster Pro aux (line in )  *)
  MM_WSS_SBPRO_AUX_CD               = 53;      (*  Sound Blaster Pro aux (CD)  *)
  MM_WSS_SBPRO_MIXER                = 54;      (*  Sound Blaster Pro mixer  *)

  MM_MSFT_WSS_NT_WAVEIN             = 55;      (*  WSS NT wave in  *)
  MM_MSFT_WSS_NT_WAVEOUT            = 56;      (*  WSS NT wave out  *)
  MM_MSFT_WSS_NT_FMSYNTH_STEREO     = 57;      (*  WSS NT FM synth  *)
  MM_MSFT_WSS_NT_MIXER              = 58;      (*  WSS NT mixer  *)
  MM_MSFT_WSS_NT_AUX                = 59;      (*  WSS NT aux  *)

  MM_MSFT_SB16_WAVEIN               = 60;      (*  Sound Blaster 16 waveform input  *)
  MM_MSFT_SB16_WAVEOUT              = 61;      (*  Sound Blaster 16  waveform output  *)
  MM_MSFT_SB16_MIDIIN               = 62;      (*  Sound Blaster 16 midi-in  *)
  MM_MSFT_SB16_MIDIOUT              = 63;      (*  Sound Blaster 16 midi out  *)
  MM_MSFT_SB16_SYNTH                = 64;      (*  Sound Blaster 16 FM Synthesis  *)
  MM_MSFT_SB16_AUX_LINE             = 65;      (*  Sound Blaster 16 aux (line in)  *)
  MM_MSFT_SB16_AUX_CD               = 66;      (*  Sound Blaster 16 aux (CD)  *)
  MM_MSFT_SB16_MIXER                = 67;      (*  Sound Blaster 16 mixer device  *)
  MM_MSFT_SBPRO_WAVEIN              = 68;      (*  Sound Blaster Pro waveform input  *)
  MM_MSFT_SBPRO_WAVEOUT             = 69;      (*  Sound Blaster Pro waveform output  *)
  MM_MSFT_SBPRO_MIDIIN              = 70;      (*  Sound Blaster Pro midi in  *)
  MM_MSFT_SBPRO_MIDIOUT             = 71;      (*  Sound Blaster Pro midi out  *)
  MM_MSFT_SBPRO_SYNTH               = 72;      (*  Sound Blaster Pro FM synthesis  *)
  MM_MSFT_SBPRO_AUX_LINE            = 73;      (*  Sound Blaster Pro aux (line in )  *)
  MM_MSFT_SBPRO_AUX_CD              = 74;      (*  Sound Blaster Pro aux (CD)  *)
  MM_MSFT_SBPRO_MIXER               = 75;      (*  Sound Blaster Pro mixer  *)

  MM_MSFT_MSOPL_SYNTH               = 76;      (* Yamaha OPL2/OPL3 compatible FM synthesis *)

  MM_MSFT_VMDMS_LINE_WAVEIN         = 80;     (* Voice Modem Serial Line Wave Input *)
  MM_MSFT_VMDMS_LINE_WAVEOUT        = 81;     (* Voice Modem Serial Line Wave Output *)
  MM_MSFT_VMDMS_HANDSET_WAVEIN      = 82;     (* Voice Modem Serial Handset Wave Input *)
  MM_MSFT_VMDMS_HANDSET_WAVEOUT     = 83;     (* Voice Modem Serial Handset Wave Output *)
  MM_MSFT_VMDMW_LINE_WAVEIN         = 84;     (* Voice Modem Wrapper Line Wave Input *)
  MM_MSFT_VMDMW_LINE_WAVEOUT        = 85;     (* Voice Modem Wrapper Line Wave Output *)
  MM_MSFT_VMDMW_HANDSET_WAVEIN      = 86;     (* Voice Modem Wrapper Handset Wave Input *)
  MM_MSFT_VMDMW_HANDSET_WAVEOUT     = 87;     (* Voice Modem Wrapper Handset Wave Output *)
  MM_MSFT_VMDMW_MIXER               = 88;     (* Voice Modem Wrapper Mixer *)
  MM_MSFT_VMDM_GAME_WAVEOUT         = 89;     (* Voice Modem Game Compatible Wave Device *)
  MM_MSFT_VMDM_GAME_WAVEIN          = 90;     (* Voice Modem Game Compatible Wave Device *)

  MM_MSFT_ACM_MSNAUDIO              = 91;     (* *)
  MM_MSFT_ACM_MSG723                = 92;     (* *)

  MM_MSFT_WDMAUDIO_WAVEOUT          = 100;    (* Generic id for WDM Audio drivers *)
  MM_MSFT_WDMAUDIO_WAVEIN           = 101;    (* Generic id for WDM Audio drivers *)
  MM_MSFT_WDMAUDIO_MIDIOUT          = 102;    (* Generic id for WDM Audio drivers *)
  MM_MSFT_WDMAUDIO_MIDIIN           = 103;    (* Generic id for WDM Audio drivers *)
  MM_MSFT_WDMAUDIO_MIXER            = 104;    (* Generic id for WDM Audio drivers *)


(* MM_CREATIVE product IDs *)
  MM_CREATIVE_SB15_WAVEIN           = 1;       (*  SB (r) 1.5 waveform input  *)
  MM_CREATIVE_SB20_WAVEIN           = 2;
  MM_CREATIVE_SBPRO_WAVEIN          = 3;
  MM_CREATIVE_SBP16_WAVEIN          = 4;
  MM_CREATIVE_PHNBLST_WAVEIN        = 5;
  MM_CREATIVE_SB15_WAVEOUT          = 101;
  MM_CREATIVE_SB20_WAVEOUT          = 102;
  MM_CREATIVE_SBPRO_WAVEOUT         = 103;
  MM_CREATIVE_SBP16_WAVEOUT         = 104;
  MM_CREATIVE_PHNBLST_WAVEOUT       = 105;
  MM_CREATIVE_MIDIOUT               = 201;     (*  SB (r)  *)
  MM_CREATIVE_MIDIIN                = 202;     (*  SB (r)  *)
  MM_CREATIVE_FMSYNTH_MONO          = 301;     (*  SB (r)  *)
  MM_CREATIVE_FMSYNTH_STEREO        = 302;     (*  SB Pro (r) stereo synthesizer  *)
  MM_CREATIVE_MIDI_AWE32            = 303;
  MM_CREATIVE_AUX_CD                = 401;     (*  SB Pro (r) aux (CD)  *)
  MM_CREATIVE_AUX_LINE              = 402;     (*  SB Pro (r) aux (Line in )  *)
  MM_CREATIVE_AUX_MIC               = 403;     (*  SB Pro (r) aux (mic)  *)
  MM_CREATIVE_AUX_MASTER            = 404;
  MM_CREATIVE_AUX_PCSPK             = 405;
  MM_CREATIVE_AUX_WAVE              = 406;
  MM_CREATIVE_AUX_MIDI              = 407;
  MM_CREATIVE_SBPRO_MIXER           = 408;
  MM_CREATIVE_SB16_MIXER            = 409;

(* MM_MEDIAVISION product IDs *)

// Pro Audio Spectrum
  MM_MEDIAVISION_PROAUDIO           = $10;
  MM_PROAUD_MIDIOUT                 = (MM_MEDIAVISION_PROAUDIO+1);
  MM_PROAUD_MIDIIN                  = (MM_MEDIAVISION_PROAUDIO+2);
  MM_PROAUD_SYNTH                   = (MM_MEDIAVISION_PROAUDIO+3);
  MM_PROAUD_WAVEOUT                 = (MM_MEDIAVISION_PROAUDIO+4);
  MM_PROAUD_WAVEIN                  = (MM_MEDIAVISION_PROAUDIO+5);
  MM_PROAUD_MIXER                   = (MM_MEDIAVISION_PROAUDIO+6);
  MM_PROAUD_AUX                     = (MM_MEDIAVISION_PROAUDIO+7);

// Thunder Board
  MM_MEDIAVISION_THUNDER            = $20;
  MM_THUNDER_SYNTH                  = (MM_MEDIAVISION_THUNDER+3);
  MM_THUNDER_WAVEOUT                = (MM_MEDIAVISION_THUNDER+4);
  MM_THUNDER_WAVEIN                 = (MM_MEDIAVISION_THUNDER+5);
  MM_THUNDER_AUX                    = (MM_MEDIAVISION_THUNDER+7);

// Audio Port
  MM_MEDIAVISION_TPORT              = $40;
  MM_TPORT_WAVEOUT                  = (MM_MEDIAVISION_TPORT+1);
  MM_TPORT_WAVEIN                   = (MM_MEDIAVISION_TPORT+2);
  MM_TPORT_SYNTH                    = (MM_MEDIAVISION_TPORT+3);

// Pro Audio Spectrum Plus
  MM_MEDIAVISION_PROAUDIO_PLUS      = $50;
  MM_PROAUD_PLUS_MIDIOUT            = (MM_MEDIAVISION_PROAUDIO_PLUS+1);
  MM_PROAUD_PLUS_MIDIIN             = (MM_MEDIAVISION_PROAUDIO_PLUS+2);
  MM_PROAUD_PLUS_SYNTH              = (MM_MEDIAVISION_PROAUDIO_PLUS+3);
  MM_PROAUD_PLUS_WAVEOUT            = (MM_MEDIAVISION_PROAUDIO_PLUS+4);
  MM_PROAUD_PLUS_WAVEIN             = (MM_MEDIAVISION_PROAUDIO_PLUS+5);
  MM_PROAUD_PLUS_MIXER              = (MM_MEDIAVISION_PROAUDIO_PLUS+6);
  MM_PROAUD_PLUS_AUX                = (MM_MEDIAVISION_PROAUDIO_PLUS+7);

// Pro Audio Spectrum 16
  MM_MEDIAVISION_PROAUDIO_16        = $60;
  MM_PROAUD_16_MIDIOUT              = (MM_MEDIAVISION_PROAUDIO_16+1);
  MM_PROAUD_16_MIDIIN               = (MM_MEDIAVISION_PROAUDIO_16+2);
  MM_PROAUD_16_SYNTH                = (MM_MEDIAVISION_PROAUDIO_16+3);
  MM_PROAUD_16_WAVEOUT              = (MM_MEDIAVISION_PROAUDIO_16+4);
  MM_PROAUD_16_WAVEIN               = (MM_MEDIAVISION_PROAUDIO_16+5);
  MM_PROAUD_16_MIXER                = (MM_MEDIAVISION_PROAUDIO_16+6);
  MM_PROAUD_16_AUX                  = (MM_MEDIAVISION_PROAUDIO_16+7);

// Pro Audio Studio 16
  MM_MEDIAVISION_PROSTUDIO_16       = $60;
  MM_STUDIO_16_MIDIOUT              = (MM_MEDIAVISION_PROSTUDIO_16+1);
  MM_STUDIO_16_MIDIIN               = (MM_MEDIAVISION_PROSTUDIO_16+2);
  MM_STUDIO_16_SYNTH                = (MM_MEDIAVISION_PROSTUDIO_16+3);
  MM_STUDIO_16_WAVEOUT              = (MM_MEDIAVISION_PROSTUDIO_16+4);
  MM_STUDIO_16_WAVEIN               = (MM_MEDIAVISION_PROSTUDIO_16+5);
  MM_STUDIO_16_MIXER                = (MM_MEDIAVISION_PROSTUDIO_16+6);
  MM_STUDIO_16_AUX                  = (MM_MEDIAVISION_PROSTUDIO_16+7);

// CDPC
  MM_MEDIAVISION_CDPC               = $70;
  MM_CDPC_MIDIOUT                   = (MM_MEDIAVISION_CDPC+1);
  MM_CDPC_MIDIIN                    = (MM_MEDIAVISION_CDPC+2);
  MM_CDPC_SYNTH                     = (MM_MEDIAVISION_CDPC+3);
  MM_CDPC_WAVEOUT                   = (MM_MEDIAVISION_CDPC+4);
  MM_CDPC_WAVEIN                    = (MM_MEDIAVISION_CDPC+5);
  MM_CDPC_MIXER                     = (MM_MEDIAVISION_CDPC+6);
  MM_CDPC_AUX                       = (MM_MEDIAVISION_CDPC+7);

// Opus MV 1208 Chipsent
  MM_MEDIAVISION_OPUS1208           = $80;
  MM_OPUS401_MIDIOUT                = (MM_MEDIAVISION_OPUS1208+1);
  MM_OPUS401_MIDIIN                 = (MM_MEDIAVISION_OPUS1208+2);
  MM_OPUS1208_SYNTH                 = (MM_MEDIAVISION_OPUS1208+3);
  MM_OPUS1208_WAVEOUT               = (MM_MEDIAVISION_OPUS1208+4);
  MM_OPUS1208_WAVEIN                = (MM_MEDIAVISION_OPUS1208+5);
  MM_OPUS1208_MIXER                 = (MM_MEDIAVISION_OPUS1208+6);
  MM_OPUS1208_AUX                   = (MM_MEDIAVISION_OPUS1208+7);

// Opus MV 1216 chipset
  MM_MEDIAVISION_OPUS1216           = $90;
  MM_OPUS1216_MIDIOUT               = (MM_MEDIAVISION_OPUS1216+1);
  MM_OPUS1216_MIDIIN                = (MM_MEDIAVISION_OPUS1216+2);
  MM_OPUS1216_SYNTH                 = (MM_MEDIAVISION_OPUS1216+3);
  MM_OPUS1216_WAVEOUT               = (MM_MEDIAVISION_OPUS1216+4);
  MM_OPUS1216_WAVEIN                = (MM_MEDIAVISION_OPUS1216+5);
  MM_OPUS1216_MIXER                 = (MM_MEDIAVISION_OPUS1216+6);
  MM_OPUS1216_AUX                   = (MM_MEDIAVISION_OPUS1216+7);

(* MM_ARTISOFT product IDs *)
  MM_ARTISOFT_SBWAVEIN              = 1;       (*  Artisoft sounding Board waveform input  *)
  MM_ARTISOFT_SBWAVEOUT             = 2;       (*  Artisoft sounding Board waveform output  *)

(* MM_IBM product IDs *)
  MM_MMOTION_WAVEAUX                = 1;       (*  IBM M-Motion Auxiliary Device  *)
  MM_MMOTION_WAVEOUT                = 2;       (*  IBM M-Motion Waveform output  *)
  MM_MMOTION_WAVEIN                 = 3;       (*  IBM M-Motion  Waveform Input  *)
  MM_IBM_PCMCIA_WAVEIN              = 11;      (*  IBM waveform input  *)
  MM_IBM_PCMCIA_WAVEOUT             = 12;      (*  IBM Waveform output  *)
  MM_IBM_PCMCIA_SYNTH               = 13;      (*  IBM Midi Synthesis  *)
  MM_IBM_PCMCIA_MIDIIN              = 14;      (*  IBM external MIDI in  *)
  MM_IBM_PCMCIA_MIDIOUT             = 15;      (*  IBM external MIDI out  *)
  MM_IBM_PCMCIA_AUX                 = 16;      (*  IBM auxiliary control  *)
  MM_IBM_THINKPAD200                = 17;
  MM_IBM_MWAVE_WAVEIN               = 18;
  MM_IBM_MWAVE_WAVEOUT              = 19;
  MM_IBM_MWAVE_MIXER                = 20;
  MM_IBM_MWAVE_MIDIIN               = 21;
  MM_IBM_MWAVE_MIDIOUT              = 22;
  MM_IBM_MWAVE_AUX                  = 23;
  MM_IBM_WC_MIDIOUT                 = 30;
  MM_IBM_WC_WAVEOUT                 = 31;
  MM_IBM_WC_MIXEROUT                = 33;

(* MM_VOCALTEC product IDs *)
  MM_VOCALTEC_WAVEOUT               = 1;
  MM_VOCALTEC_WAVEIN                = 2;

(* MM_ROLAND product IDs *)
  MM_ROLAND_RAP10_MIDIOUT           = 10;      (* MM_ROLAND_RAP10 *)
  MM_ROLAND_RAP10_MIDIIN            = 11;      (* MM_ROLAND_RAP10 *)
  MM_ROLAND_RAP10_SYNTH             = 12;      (* MM_ROLAND_RAP10 *)
  MM_ROLAND_RAP10_WAVEOUT           = 13;      (* MM_ROLAND_RAP10 *)
  MM_ROLAND_RAP10_WAVEIN            = 14;      (* MM_ROLAND_RAP10 *)
  MM_ROLAND_MPU401_MIDIOUT          = 15;
  MM_ROLAND_MPU401_MIDIIN           = 16;
  MM_ROLAND_SMPU_MIDIOUTA           = 17;
  MM_ROLAND_SMPU_MIDIOUTB           = 18;
  MM_ROLAND_SMPU_MIDIINA            = 19;
  MM_ROLAND_SMPU_MIDIINB            = 20;
  MM_ROLAND_SC7_MIDIOUT             = 21;
  MM_ROLAND_SC7_MIDIIN              = 22;
  MM_ROLAND_SERIAL_MIDIOUT          = 23;
  MM_ROLAND_SERIAL_MIDIIN           = 24;
  MM_ROLAND_SCP_MIDIOUT             = 38;
  MM_ROLAND_SCP_MIDIIN              = 39;
  MM_ROLAND_SCP_WAVEOUT             = 40;
  MM_ROLAND_SCP_WAVEIN              = 41;
  MM_ROLAND_SCP_MIXER               = 42;
  MM_ROLAND_SCP_AUX                 = 48;

(* MM_DSP_SOLUTIONS product IDs *)
  MM_DSP_SOLUTIONS_WAVEOUT          = 1;
  MM_DSP_SOLUTIONS_WAVEIN           = 2;
  MM_DSP_SOLUTIONS_SYNTH            = 3;
  MM_DSP_SOLUTIONS_AUX              = 4;

(* MM_WANGLABS product IDs *)
  MM_WANGLABS_WAVEIN1               = 1;       (*  Input audio wave on CPU board models: Exec 4010, 4030, 3450; PC 251/25c, pc 461/25s , pc 461/33c  *)
  MM_WANGLABS_WAVEOUT1              = 2;

(* MM_TANDY product IDs *)
  MM_TANDY_VISWAVEIN                = 1;
  MM_TANDY_VISWAVEOUT               = 2;
  MM_TANDY_VISBIOSSYNTH             = 3;
  MM_TANDY_SENS_MMAWAVEIN           = 4;
  MM_TANDY_SENS_MMAWAVEOUT          = 5;
  MM_TANDY_SENS_MMAMIDIIN           = 6;
  MM_TANDY_SENS_MMAMIDIOUT          = 7;
  MM_TANDY_SENS_VISWAVEOUT          = 8;
  MM_TANDY_PSSJWAVEIN               = 9;
  MM_TANDY_PSSJWAVEOUT              = 10;

(* product IDs *)
  MM_INTELOPD_WAVEIN                = 1;       (*  HID2 WaveAudio Driver  *)
  MM_INTELOPD_WAVEOUT               = 101;     (*  HID2  *)
  MM_INTELOPD_AUX                   = 401;     (*  HID2 for mixing  *)
  MM_INTEL_NSPMODEMLINE             = 501;

(* MM_INTERACTIVE product IDs *)
  MM_INTERACTIVE_WAVEIN             = $45;
  MM_INTERACTIVE_WAVEOUT            = $45;

(* MM_YAMAHA product IDs *)
  MM_YAMAHA_GSS_SYNTH               = $01;
  MM_YAMAHA_GSS_WAVEOUT             = $02;
  MM_YAMAHA_GSS_WAVEIN              = $03;
  MM_YAMAHA_GSS_MIDIOUT             = $04;
  MM_YAMAHA_GSS_MIDIIN              = $05;
  MM_YAMAHA_GSS_AUX                 = $06;
  MM_YAMAHA_SERIAL_MIDIOUT          = $07;
  MM_YAMAHA_SERIAL_MIDIIN           = $08;
  MM_YAMAHA_OPL3SA_WAVEOUT          = $10;
  MM_YAMAHA_OPL3SA_WAVEIN           = $11;
  MM_YAMAHA_OPL3SA_FMSYNTH          = $12;
  MM_YAMAHA_OPL3SA_YSYNTH           = $13;
  MM_YAMAHA_OPL3SA_MIDIOUT          = $14;
  MM_YAMAHA_OPL3SA_MIDIIN           = $15;
  MM_YAMAHA_OPL3SA_MIXER            = $17;
  MM_YAMAHA_OPL3SA_JOYSTICK         = $18;

(* MM_EVEREX product IDs *)
  MM_EVEREX_CARRIER                 = $01;

(* MM_ECHO product IDs *)
  MM_ECHO_SYNTH                     = $01;
  MM_ECHO_WAVEOUT                   = $02;
  MM_ECHO_WAVEIN                    = $03;
  MM_ECHO_MIDIOUT                   = $04;
  MM_ECHO_MIDIIN                    = $05;
  MM_ECHO_AUX                       = $06;

(* MM_SIERRA product IDs *)
  MM_SIERRA_ARIA_MIDIOUT            = $14;
  MM_SIERRA_ARIA_MIDIIN             = $15;
  MM_SIERRA_ARIA_SYNTH              = $16;
  MM_SIERRA_ARIA_WAVEOUT            = $17;
  MM_SIERRA_ARIA_WAVEIN             = $18;
  MM_SIERRA_ARIA_AUX                = $19;
  MM_SIERRA_ARIA_AUX2               = $20;
  MM_SIERRA_QUARTET_WAVEIN          = $50;
  MM_SIERRA_QUARTET_WAVEOUT         = $51;
  MM_SIERRA_QUARTET_MIDIIN          = $52;
  MM_SIERRA_QUARTET_MIDIOUT         = $53;
  MM_SIERRA_QUARTET_SYNTH           = $54;
  MM_SIERRA_QUARTET_AUX_CD          = $55;
  MM_SIERRA_QUARTET_AUX_LINE        = $56;
  MM_SIERRA_QUARTET_AUX_MODEM       = $57;
  MM_SIERRA_QUARTET_MIXER           = $58;

(* MM_CAT product IDs *)
  MM_CAT_WAVEOUT                    = 1;

(* MM_DSP_GROUP product IDs *)
  MM_DSP_GROUP_TRUESPEECH           = $01;

(* MM_MELABS product IDs *)
  MM_MELABS_MIDI2GO                 = $01;

(* MM_ESS product IDs *)
  MM_ESS_AMWAVEOUT                  = $01;
  MM_ESS_AMWAVEIN                   = $02;
  MM_ESS_AMAUX                      = $03;
  MM_ESS_AMSYNTH                    = $04;
  MM_ESS_AMMIDIOUT                  = $05;
  MM_ESS_AMMIDIIN                   = $06;
  MM_ESS_MIXER                      = $07;
  MM_ESS_AUX_CD                     = $08;
  MM_ESS_MPU401_MIDIOUT             = $09;
  MM_ESS_MPU401_MIDIIN              = $0A;
  MM_ESS_ES488_WAVEOUT              = $10;
  MM_ESS_ES488_WAVEIN               = $11;
  MM_ESS_ES488_MIXER                = $12;
  MM_ESS_ES688_WAVEOUT              = $13;
  MM_ESS_ES688_WAVEIN               = $14;
  MM_ESS_ES688_MIXER                = $15;
  MM_ESS_ES1488_WAVEOUT             = $16;
  MM_ESS_ES1488_WAVEIN              = $17;
  MM_ESS_ES1488_MIXER               = $18;
  MM_ESS_ES1688_WAVEOUT             = $19;
  MM_ESS_ES1688_WAVEIN              = $1A;
  MM_ESS_ES1688_MIXER               = $1B;
  MM_ESS_ES1788_WAVEOUT             = $1C;
  MM_ESS_ES1788_WAVEIN              = $1D;
  MM_ESS_ES1788_MIXER               = $1E;
  MM_ESS_ES1888_WAVEOUT             = $1F;
  MM_ESS_ES1888_WAVEIN              = $20;
  MM_ESS_ES1888_MIXER               = $21;
  MM_ESS_ES1868_WAVEOUT             = $22;
  MM_ESS_ES1868_WAVEIN              = $23;
  MM_ESS_ES1868_MIXER               = $24;
  MM_ESS_ES1878_WAVEOUT             = $25;
  MM_ESS_ES1878_WAVEIN              = $26;
  MM_ESS_ES1878_MIXER               = $27;

(* product IDs *)
  MM_EPS_FMSND                      = 1;

(* MM_TRUEVISION product IDs *)
  MM_TRUEVISION_WAVEIN1             = 1;
  MM_TRUEVISION_WAVEOUT1            = 2;

(* MM_AZTECH product IDs *)
  MM_AZTECH_MIDIOUT                 = 3;
  MM_AZTECH_MIDIIN                  = 4;
  MM_AZTECH_WAVEIN                  = 17;
  MM_AZTECH_WAVEOUT                 = 18;
  MM_AZTECH_FMSYNTH                 = 20;
  MM_AZTECH_MIXER                   = 21;
  MM_AZTECH_PRO16_WAVEIN            = 33;
  MM_AZTECH_PRO16_WAVEOUT           = 34;
  MM_AZTECH_PRO16_FMSYNTH           = 38;
  MM_AZTECH_DSP16_WAVEIN            = 65;
  MM_AZTECH_DSP16_WAVEOUT           = 66;
  MM_AZTECH_DSP16_FMSYNTH           = 68;
  MM_AZTECH_DSP16_WAVESYNTH         = 70;
  MM_AZTECH_NOVA16_WAVEIN           = 71;
  MM_AZTECH_NOVA16_WAVEOUT          = 72;
  MM_AZTECH_NOVA16_MIXER            = 73;
  MM_AZTECH_WASH16_WAVEIN           = 74;
  MM_AZTECH_WASH16_WAVEOUT          = 75;
  MM_AZTECH_WASH16_MIXER            = 76;
  MM_AZTECH_AUX_CD                  = 401;
  MM_AZTECH_AUX_LINE                = 402;
  MM_AZTECH_AUX_MIC                 = 403;
  MM_AZTECH_AUX                     = 404;

(* MM_VIDEOLOGIC product IDs *)
  MM_VIDEOLOGIC_MSWAVEIN            = 1;
  MM_VIDEOLOGIC_MSWAVEOUT           = 2;

(* MM_KORG product IDs *)
  MM_KORG_PCIF_MIDIOUT              = 1;
  MM_KORG_PCIF_MIDIIN               = 2;

(* MM_APT product IDs *)
  MM_APT_ACE100CD                   = 1;

(* MM_ICS product IDs *)
  MM_ICS_WAVEDECK_WAVEOUT           = 1;       (*  MS WSS compatible card and driver  *)
  MM_ICS_WAVEDECK_WAVEIN            = 2;
  MM_ICS_WAVEDECK_MIXER             = 3;
  MM_ICS_WAVEDECK_AUX               = 4;
  MM_ICS_WAVEDECK_SYNTH             = 5;
  MM_ICS_WAVEDEC_SB_WAVEOUT         = 6;
  MM_ICS_WAVEDEC_SB_WAVEIN          = 7;
  MM_ICS_WAVEDEC_SB_FM_MIDIOUT      = 8;
  MM_ICS_WAVEDEC_SB_MPU401_MIDIOUT  = 9;
  MM_ICS_WAVEDEC_SB_MPU401_MIDIIN   = 10;
  MM_ICS_WAVEDEC_SB_MIXER           = 11;
  MM_ICS_WAVEDEC_SB_AUX             = 12;
  MM_ICS_2115_LITE_MIDIOUT          = 13;
  MM_ICS_2120_LITE_MIDIOUT          = 14;

(* MM_ITERATEDSYS product IDs *)
  MM_ITERATEDSYS_FUFCODEC           = 1;

(* MM_METHEUS product IDs *)
  MM_METHEUS_ZIPPER                 = 1;

(* MM_WINNOV product IDs *)
  MM_WINNOV_CAVIAR_WAVEIN           = 1;
  MM_WINNOV_CAVIAR_WAVEOUT          = 2;
  MM_WINNOV_CAVIAR_VIDC             = 3;
  MM_WINNOV_CAVIAR_CHAMPAGNE        = 4;       (*  Fourcc is CHAM  *)
  MM_WINNOV_CAVIAR_YUV8             = 5;       (*  Fourcc is YUV8  *)

(* MM_NCR product IDs *)
  MM_NCR_BA_WAVEIN                  = 1;
  MM_NCR_BA_WAVEOUT                 = 2;
  MM_NCR_BA_SYNTH                   = 3;
  MM_NCR_BA_AUX                     = 4;
  MM_NCR_BA_MIXER                   = 5;

(* MM_VITEC product IDs *)
  MM_VITEC_VMAKER                   = 1;
  MM_VITEC_VMPRO                    = 2;

(* MM_MOSCOM product IDs *)
  MM_MOSCOM_VPC2400_IN              = 1;       (*  Four Port Voice Processing / Voice Recognition Board  *)
  MM_MOSCOM_VPC2400_OUT             = 2;       (*  VPC2400 *)

(* MM_SILICONSOFT product IDs *)
  MM_SILICONSOFT_SC1_WAVEIN         = 1;       (*  Waveform in , high sample rate  *)
  MM_SILICONSOFT_SC1_WAVEOUT        = 2;       (*  Waveform out , high sample rate  *)
  MM_SILICONSOFT_SC2_WAVEIN         = 3;       (*  Waveform in 2 channels, high sample rate  *)
  MM_SILICONSOFT_SC2_WAVEOUT        = 4;       (*  Waveform out 2 channels, high sample rate  *)
  MM_SILICONSOFT_SOUNDJR2_WAVEOUT   = 5;       (*  Waveform out, self powered, efficient  *)
  MM_SILICONSOFT_SOUNDJR2PR_WAVEIN  = 6;       (*  Waveform in, self powered, efficient  *)
  MM_SILICONSOFT_SOUNDJR2PR_WAVEOUT = 7;       (*  Waveform out 2 channels, self powered, efficient  *)
  MM_SILICONSOFT_SOUNDJR3_WAVEOUT   = 8;       (*  Waveform in 2 channels, self powered, efficient  *)

(* MM_OLIVETTI product IDs *)
  MM_OLIVETTI_WAVEIN                = 1;
  MM_OLIVETTI_WAVEOUT               = 2;
  MM_OLIVETTI_MIXER                 = 3;
  MM_OLIVETTI_AUX                   = 4;
  MM_OLIVETTI_MIDIIN                = 5;
  MM_OLIVETTI_MIDIOUT               = 6;
  MM_OLIVETTI_SYNTH                 = 7;
  MM_OLIVETTI_JOYSTICK              = 8;
  MM_OLIVETTI_ACM_GSM               = 9;
  MM_OLIVETTI_ACM_ADPCM             = 10;
  MM_OLIVETTI_ACM_CELP              = 11;
  MM_OLIVETTI_ACM_SBC               = 12;
  MM_OLIVETTI_ACM_OPR               = 13;

(* MM_IOMAGIC product IDs *)

(*  The I/O Magic Tempo is a PCMCIA Type 2 audio card featuring wave audio
    record and playback, FM synthesizer, and MIDI output.  The I/O Magic
    Tempo WaveOut device supports mono and stereo PCM playback at rates
    of 7350, 11025, 22050, and  44100 samples *)

  MM_IOMAGIC_TEMPO_WAVEOUT          = 1;
  MM_IOMAGIC_TEMPO_WAVEIN           = 2;
  MM_IOMAGIC_TEMPO_SYNTH            = 3;
  MM_IOMAGIC_TEMPO_MIDIOUT          = 4;
  MM_IOMAGIC_TEMPO_MXDOUT           = 5;
  MM_IOMAGIC_TEMPO_AUXOUT           = 6;

(* MM_MATSUSHITA product IDs *)
  MM_MATSUSHITA_WAVEIN              = 1;
  MM_MATSUSHITA_WAVEOUT             = 2;
  MM_MATSUSHITA_FMSYNTH_STEREO      = 3;
  MM_MATSUSHITA_MIXER               = 4;
  MM_MATSUSHITA_AUX                 = 5;

(* MM_NEWMEDIA product IDs *)
  MM_NEWMEDIA_WAVJAMMER             = 1;       (*  WSS Compatible sound card.  *)

(* MM_LYRRUS product IDs *)

(*  Bridge is a MIDI driver that allows the the Lyrrus G-VOX hardware to
    communicate with Windows base transcription and sequencer applications.
    The driver also provides a mechanism for the user to configure the system
    to their personal playing style. *)

  MM_LYRRUS_BRIDGE_GUITAR           = 1;

(* MM_OPTI product IDs *)
  MM_OPTI_M16_FMSYNTH_STEREO        = $0001;
  MM_OPTI_M16_MIDIIN                = $0002;
  MM_OPTI_M16_MIDIOUT               = $0003;
  MM_OPTI_M16_WAVEIN                = $0004;
  MM_OPTI_M16_WAVEOUT               = $0005;
  MM_OPTI_M16_MIXER                 = $0006;
  MM_OPTI_M16_AUX                   = $0007;
  MM_OPTI_P16_FMSYNTH_STEREO        = $0010;
  MM_OPTI_P16_MIDIIN                = $0011;
  MM_OPTI_P16_MIDIOUT               = $0012;
  MM_OPTI_P16_WAVEIN                = $0013;
  MM_OPTI_P16_WAVEOUT               = $0014;
  MM_OPTI_P16_MIXER                 = $0015;
  MM_OPTI_P16_AUX                   = $0016;
  MM_OPTI_M32_WAVEIN                = $0020;
  MM_OPTI_M32_WAVEOUT               = $0021;
  MM_OPTI_M32_MIDIIN                = $0022;
  MM_OPTI_M32_MIDIOUT               = $0023;
  MM_OPTI_M32_SYNTH_STEREO          = $0024;
  MM_OPTI_M32_MIXER                 = $0025;
  MM_OPTI_M32_AUX                   = $0026;

(*  Product IDs for     MM_ADDX    -  ADDX    *)
  MM_ADDX_PCTV_DIGITALMIX           = 1;       (* MM_ADDX_PCTV_DIGITALMIX *)
  MM_ADDX_PCTV_WAVEIN               = 2;       (* MM_ADDX_PCTV_WAVEIN *)
  MM_ADDX_PCTV_WAVEOUT              = 3;       (* MM_ADDX_PCTV_WAVEOUT *)
  MM_ADDX_PCTV_MIXER                = 4;       (* MM_ADDX_PCTV_MIXER *)
  MM_ADDX_PCTV_AUX_CD               = 5;       (* MM_ADDX_PCTV_AUX_CD *)
  MM_ADDX_PCTV_AUX_LINE             = 6;       (* MM_ADDX_PCTV_AUX_LINE *)

(*  Product IDs for     MM_AHEAD    -  Ahead, Inc.    *)
  MM_AHEAD_MULTISOUND               = 1;
  MM_AHEAD_SOUNDBLASTER             = 2;
  MM_AHEAD_PROAUDIO                 = 3;
  MM_AHEAD_GENERIC                  = 4;

(*  Product IDs for     MM_AMD    -  AMD    *)
  MM_AMD_INTERWAVE_WAVEIN           = 1;
  MM_AMD_INTERWAVE_WAVEOUT          = 2;
  MM_AMD_INTERWAVE_SYNTH            = 3;
  MM_AMD_INTERWAVE_MIXER1           = 4;
  MM_AMD_INTERWAVE_MIXER2           = 5;
  MM_AMD_INTERWAVE_JOYSTICK         = 6;
  MM_AMD_INTERWAVE_EX_CD            = 7;
  MM_AMD_INTERWAVE_MIDIIN           = 8;
  MM_AMD_INTERWAVE_MIDIOUT          = 9;
  MM_AMD_INTERWAVE_AUX1             = 10;
  MM_AMD_INTERWAVE_AUX2             = 11;
  MM_AMD_INTERWAVE_AUX_MIC          = 12;
  MM_AMD_INTERWAVE_AUX_CD           = 13;
  MM_AMD_INTERWAVE_MONO_IN          = 14;
  MM_AMD_INTERWAVE_MONO_OUT         = 15;
  MM_AMD_INTERWAVE_EX_TELEPHONY     = 16;
  MM_AMD_INTERWAVE_WAVEOUT_BASE     = 17;
  MM_AMD_INTERWAVE_WAVEOUT_TREBLE   = 18;
  MM_AMD_INTERWAVE_STEREO_ENHANCED  = 19;

(*  Product IDs for     MM_AST    -  AST Research Inc.    *)
  MM_AST_MODEMWAVE_WAVEIN           = 13;
  MM_AST_MODEMWAVE_WAVEOUT          = 14;

(*  Product IDs for     MM_BROOKTREE    -  Brooktree Corporation    *)
  MM_BTV_WAVEIN                     = 1;       (* Brooktree PCM Wave Audio In *)
  MM_BTV_WAVEOUT                    = 2;       (* Brooktree PCM Wave Audio Out *)
  MM_BTV_MIDIIN                     = 3;       (* Brooktree MIDI In *)
  MM_BTV_MIDIOUT                    = 4;       (* Brooktree MIDI out *)
  MM_BTV_MIDISYNTH                  = 5;       (* Brooktree MIDI FM synth *)
  MM_BTV_AUX_LINE                   = 6;       (* Brooktree Line Input *)
  MM_BTV_AUX_MIC                    = 7;       (* Brooktree Microphone Input *)
  MM_BTV_AUX_CD                     = 8;       (* Brooktree CD Input *)
  MM_BTV_DIGITALIN                  = 9;       (* Brooktree PCM Wave in with subcode information *)
  MM_BTV_DIGITALOUT                 = 10;      (* Brooktree PCM Wave out with subcode information *)
  MM_BTV_MIDIWAVESTREAM             = 11;      (* Brooktree WaveStream *)
  MM_BTV_MIXER                      = 12;      (* Brooktree WSS Mixer driver *)

(*  Product IDs for     MM_CANAM    -  CANAM Computers    *)
  MM_CANAM_CBXWAVEOUT               = 1;
  MM_CANAM_CBXWAVEIN                = 2;

(*  Product IDs for     MM_CASIO    -  Casio Computer Co., LTD    *)
  MM_CASIO_WP150_MIDIOUT            = 1;       (* wp150 *)
  MM_CASIO_WP150_MIDIIN             = 2;

(*  Product IDs for     MM_COMPAQ    -  Compaq Computer Corp.    *)
  MM_COMPAQ_BB_WAVEIN               = 1;
  MM_COMPAQ_BB_WAVEOUT              = 2;
  MM_COMPAQ_BB_WAVEAUX              = 3;

(*  Product IDs for     MM_COREDYNAMICS    -  Core Dynamics    *)
  MM_COREDYNAMICS_DYNAMIXHR         = 1;       (* DynaMax Hi-Rez *)
  MM_COREDYNAMICS_DYNASONIX_SYNTH   = 2;       (* DynaSonix *)
  MM_COREDYNAMICS_DYNASONIX_MIDI_IN = 3;
  MM_COREDYNAMICS_DYNASONIX_MIDI_OUT= 4;
  MM_COREDYNAMICS_DYNASONIX_WAVE_IN = 5;
  MM_COREDYNAMICS_DYNASONIX_WAVE_OUT= 6;
  MM_COREDYNAMICS_DYNASONIX_AUDIO_IN= 7;
  MM_COREDYNAMICS_DYNASONIX_AUDIO_OUT = 8;
  MM_COREDYNAMICS_DYNAGRAFX_VGA     = 9;       (* DynaGrfx *)
  MM_COREDYNAMICS_DYNAGRAFX_WAVE_IN = 10;
  MM_COREDYNAMICS_DYNAGRAFX_WAVE_OUT= 11;

(*  Product IDs for     MM_CRYSTAL    -  Crystal Semiconductor Corporation    *)
  MM_CRYSTAL_CS4232_WAVEIN          = 1;
  MM_CRYSTAL_CS4232_WAVEOUT         = 2;
  MM_CRYSTAL_CS4232_WAVEMIXER       = 3;
  MM_CRYSTAL_CS4232_WAVEAUX_AUX1    = 4;
  MM_CRYSTAL_CS4232_WAVEAUX_AUX2    = 5;
  MM_CRYSTAL_CS4232_WAVEAUX_LINE    = 6;
  MM_CRYSTAL_CS4232_WAVEAUX_MONO    = 7;
  MM_CRYSTAL_CS4232_WAVEAUX_MASTER  = 8;
  MM_CRYSTAL_CS4232_MIDIIN          = 9;
  MM_CRYSTAL_CS4232_MIDIOUT         = 10;
  MM_CRYSTAL_CS4232_INPUTGAIN_AUX1  = 13;
  MM_CRYSTAL_CS4232_INPUTGAIN_LOOP  = 14;

(*  Product IDs for     MM_DDD    -  Danka Data Devices    *)
  MM_DDD_MIDILINK_MIDIIN            = 1;
  MM_DDD_MIDILINK_MIDIOUT           = 2;

(*  Product IDs for     MM_DIACOUSTICS    -  DiAcoustics, Inc.    *)
  MM_DIACOUSTICS_DRUM_ACTION        = 1;       (* Drum Action *)

(*  Product IDs for     MM_DIAMONDMM    -  Diamond Multimedia    *)
  MM_DIMD_PLATFORM                  = 0;       (* Freedom Audio *)
  MM_DIMD_DIRSOUND                  = 1;
  MM_DIMD_VIRTMPU                   = 2;
  MM_DIMD_VIRTSB                    = 3;
  MM_DIMD_VIRTJOY                   = 4;
  MM_DIMD_WAVEIN                    = 5;
  MM_DIMD_WAVEOUT                   = 6;
  MM_DIMD_MIDIIN                    = 7;
  MM_DIMD_MIDIOUT                   = 8;
  MM_DIMD_AUX_LINE                  = 9;
  MM_DIMD_MIXER                     = 10;

(*  Product IDs for     MM_DIGITAL_AUDIO_LABS    -  Digital Audio Labs, Inc.    *)
  MM_DIGITAL_AUDIO_LABS_V8          = $10;
  MM_DIGITAL_AUDIO_LABS_CPRO        = $11;

(*  Product IDs for     MM_DIGITAL    -  Digital Equipment Corporation    *)
  MM_DIGITAL_AV320_WAVEIN           = 1;       (* Digital Audio Video Compression Board *)
  MM_DIGITAL_AV320_WAVEOUT          = 2;       (* Digital Audio Video Compression Board *)

(*  Product IDs for     MM_ECS    -  Electronic Courseware Systems, Inc.    *)
  MM_ECS_AADF_MIDI_IN               = 10;
  MM_ECS_AADF_MIDI_OUT              = 11;
  MM_ECS_AADF_WAVE2MIDI_IN          = 12;

(*  Product IDs for     MM_ENSONIQ    -  ENSONIQ Corporation    *)
  MM_ENSONIQ_SOUNDSCAPE             = $10;    (* ENSONIQ Soundscape *)
  MM_SOUNDSCAPE_WAVEOUT             = MM_ENSONIQ_SOUNDSCAPE+1;
  MM_SOUNDSCAPE_WAVEOUT_AUX         = MM_ENSONIQ_SOUNDSCAPE+2;
  MM_SOUNDSCAPE_WAVEIN              = MM_ENSONIQ_SOUNDSCAPE+3;
  MM_SOUNDSCAPE_MIDIOUT             = MM_ENSONIQ_SOUNDSCAPE+4;
  MM_SOUNDSCAPE_MIDIIN              = MM_ENSONIQ_SOUNDSCAPE+5;
  MM_SOUNDSCAPE_SYNTH               = MM_ENSONIQ_SOUNDSCAPE+6;
  MM_SOUNDSCAPE_MIXER               = MM_ENSONIQ_SOUNDSCAPE+7;
  MM_SOUNDSCAPE_AUX                 = MM_ENSONIQ_SOUNDSCAPE+8;

(*  Product IDs for     MM_FRONTIER    -  Frontier Design Group LLC    *)
  MM_FRONTIER_WAVECENTER_MIDIIN     = 1;       (* WaveCenter *)
  MM_FRONTIER_WAVECENTER_MIDIOUT    = 2;
  MM_FRONTIER_WAVECENTER_WAVEIN     = 3;
  MM_FRONTIER_WAVECENTER_WAVEOUT    = 4;

(*  Product IDs for     MM_GADGETLABS    -  Gadget Labs LLC    *)
  MM_GADGETLABS_WAVE44_WAVEIN       = 1;
  MM_GADGETLABS_WAVE44_WAVEOUT      = 2;
  MM_GADGETLABS_WAVE42_WAVEIN       = 3;
  MM_GADGETLABS_WAVE42_WAVEOUT      = 4;
  MM_GADGETLABS_WAVE4_MIDIIN        = 5;
  MM_GADGETLABS_WAVE4_MIDIOUT       = 6;

(*  Product IDs for     MM_KAY_ELEMETRICS    -  Kay Elemetrics, Inc.    *)
  MM_KAY_ELEMETRICS_CSL             = $4300;
  MM_KAY_ELEMETRICS_CSL_DAT         = $4308;
  MM_KAY_ELEMETRICS_CSL_4CHANNEL    = $4309;

(*  Product IDs for     MM_LERNOUT_AND_HAUSPIE    -  Lernout & Hauspie    *)
  MM_LERNOUT_ANDHAUSPIE_LHCODECACM  = 1;

(*  Product IDs for     MM_MPTUS    -  M.P. Technologies, Inc.    *)
  MM_MPTUS_SPWAVEOUT                = 1;       (* Sound Pallette *)

(*  Product IDs for     MM_MOTU    -  Mark of the Unicorn    *)
  MM_MOTU_MTP_MIDIOUT_ALL           = 100;
  MM_MOTU_MTP_MIDIIN_1              = 101;
  MM_MOTU_MTP_MIDIOUT_1             = 101;
  MM_MOTU_MTP_MIDIIN_2              = 102;
  MM_MOTU_MTP_MIDIOUT_2             = 102;
  MM_MOTU_MTP_MIDIIN_3              = 103;
  MM_MOTU_MTP_MIDIOUT_3             = 103;
  MM_MOTU_MTP_MIDIIN_4              = 104;
  MM_MOTU_MTP_MIDIOUT_4             = 104;
  MM_MOTU_MTP_MIDIIN_5              = 105;
  MM_MOTU_MTP_MIDIOUT_5             = 105;
  MM_MOTU_MTP_MIDIIN_6              = 106;
  MM_MOTU_MTP_MIDIOUT_6             = 106;
  MM_MOTU_MTP_MIDIIN_7              = 107;
  MM_MOTU_MTP_MIDIOUT_7             = 107;
  MM_MOTU_MTP_MIDIIN_8              = 108;
  MM_MOTU_MTP_MIDIOUT_8             = 108;

  MM_MOTU_MTPII_MIDIOUT_ALL         = 200;
  MM_MOTU_MTPII_MIDIIN_SYNC         = 200;
  MM_MOTU_MTPII_MIDIIN_1            = 201;
  MM_MOTU_MTPII_MIDIOUT_1           = 201;
  MM_MOTU_MTPII_MIDIIN_2            = 202;
  MM_MOTU_MTPII_MIDIOUT_2           = 202;
  MM_MOTU_MTPII_MIDIIN_3            = 203;
  MM_MOTU_MTPII_MIDIOUT_3           = 203;
  MM_MOTU_MTPII_MIDIIN_4            = 204;
  MM_MOTU_MTPII_MIDIOUT_4           = 204;
  MM_MOTU_MTPII_MIDIIN_5            = 205;
  MM_MOTU_MTPII_MIDIOUT_5           = 205;
  MM_MOTU_MTPII_MIDIIN_6            = 206;
  MM_MOTU_MTPII_MIDIOUT_6           = 206;
  MM_MOTU_MTPII_MIDIIN_7            = 207;
  MM_MOTU_MTPII_MIDIOUT_7           = 207;
  MM_MOTU_MTPII_MIDIIN_8            = 208;
  MM_MOTU_MTPII_MIDIOUT_8           = 208;
  MM_MOTU_MTPII_NET_MIDIIN_1        = 209;
  MM_MOTU_MTPII_NET_MIDIOUT_1       = 209;
  MM_MOTU_MTPII_NET_MIDIIN_2        = 210;
  MM_MOTU_MTPII_NET_MIDIOUT_2       = 210;
  MM_MOTU_MTPII_NET_MIDIIN_3        = 211;
  MM_MOTU_MTPII_NET_MIDIOUT_3       = 211;
  MM_MOTU_MTPII_NET_MIDIIN_4        = 212;
  MM_MOTU_MTPII_NET_MIDIOUT_4       = 212;
  MM_MOTU_MTPII_NET_MIDIIN_5        = 213;
  MM_MOTU_MTPII_NET_MIDIOUT_5       = 213;
  MM_MOTU_MTPII_NET_MIDIIN_6        = 214;
  MM_MOTU_MTPII_NET_MIDIOUT_6       = 214;
  MM_MOTU_MTPII_NET_MIDIIN_7        = 215;
  MM_MOTU_MTPII_NET_MIDIOUT_7       = 215;
  MM_MOTU_MTPII_NET_MIDIIN_8        = 216;
  MM_MOTU_MTPII_NET_MIDIOUT_8       = 216;

  MM_MOTU_MXP_MIDIIN_MIDIOUT_ALL    = 300;
  MM_MOTU_MXP_MIDIIN_SYNC           = 300;
  MM_MOTU_MXP_MIDIIN_MIDIIN_1       = 301;
  MM_MOTU_MXP_MIDIIN_MIDIOUT_1      = 301;
  MM_MOTU_MXP_MIDIIN_MIDIIN_2       = 302;
  MM_MOTU_MXP_MIDIIN_MIDIOUT_2      = 302;
  MM_MOTU_MXP_MIDIIN_MIDIIN_3       = 303;
  MM_MOTU_MXP_MIDIIN_MIDIOUT_3      = 303;
  MM_MOTU_MXP_MIDIIN_MIDIIN_4       = 304;
  MM_MOTU_MXP_MIDIIN_MIDIOUT_4      = 304;
  MM_MOTU_MXP_MIDIIN_MIDIIN_5       = 305;
  MM_MOTU_MXP_MIDIIN_MIDIOUT_5      = 305;
  MM_MOTU_MXP_MIDIIN_MIDIIN_6       = 306;
  MM_MOTU_MXP_MIDIIN_MIDIOUT_6      = 306;

  MM_MOTU_MXPMPU_MIDIOUT_ALL        = 400;
  MM_MOTU_MXPMPU_MIDIIN_SYNC        = 400;
  MM_MOTU_MXPMPU_MIDIIN_1           = 401;
  MM_MOTU_MXPMPU_MIDIOUT_1          = 401;
  MM_MOTU_MXPMPU_MIDIIN_2           = 402;
  MM_MOTU_MXPMPU_MIDIOUT_2          = 402;
  MM_MOTU_MXPMPU_MIDIIN_3           = 403;
  MM_MOTU_MXPMPU_MIDIOUT_3          = 403;
  MM_MOTU_MXPMPU_MIDIIN_4           = 404;
  MM_MOTU_MXPMPU_MIDIOUT_4          = 404;
  MM_MOTU_MXPMPU_MIDIIN_5           = 405;
  MM_MOTU_MXPMPU_MIDIOUT_5          = 405;
  MM_MOTU_MXPMPU_MIDIIN_6           = 406;
  MM_MOTU_MXPMPU_MIDIOUT_6          = 406;

  MM_MOTU_MXN_MIDIOUT_ALL           = 500;
  MM_MOTU_MXN_MIDIIN_SYNC           = 500;
  MM_MOTU_MXN_MIDIIN_1              = 501;
  MM_MOTU_MXN_MIDIOUT_1             = 501;
  MM_MOTU_MXN_MIDIIN_2              = 502;
  MM_MOTU_MXN_MIDIOUT_2             = 502;
  MM_MOTU_MXN_MIDIIN_3              = 503;
  MM_MOTU_MXN_MIDIOUT_3             = 503;
  MM_MOTU_MXN_MIDIIN_4              = 504;
  MM_MOTU_MXN_MIDIOUT_4             = 504;

  MM_MOTU_FLYER_MIDI_IN_SYNC        = 600;
  MM_MOTU_FLYER_MIDI_IN_A           = 601;
  MM_MOTU_FLYER_MIDI_OUT_A          = 601;
  MM_MOTU_FLYER_MIDI_IN_B           = 602;
  MM_MOTU_FLYER_MIDI_OUT_B          = 602;

  MM_MOTU_PKX_MIDI_IN_SYNC          = 700;
  MM_MOTU_PKX_MIDI_IN_A             = 701;
  MM_MOTU_PKX_MIDI_OUT_A            = 701;
  MM_MOTU_PKX_MIDI_IN_B             = 702;
  MM_MOTU_PKX_MIDI_OUT_B            = 702;

  MM_MOTU_DTX_MIDI_IN_SYNC          = 800;
  MM_MOTU_DTX_MIDI_IN_A             = 801;
  MM_MOTU_DTX_MIDI_OUT_A            = 801;
  MM_MOTU_DTX_MIDI_IN_B             = 802;
  MM_MOTU_DTX_MIDI_OUT_B            = 802;

  MM_MOTU_MTPAV_MIDIOUT_ALL         = 900;
  MM_MOTU_MTPAV_MIDIIN_SYNC         = 900;
  MM_MOTU_MTPAV_MIDIIN_1            = 901;
  MM_MOTU_MTPAV_MIDIOUT_1           = 901;
  MM_MOTU_MTPAV_MIDIIN_2            = 902;
  MM_MOTU_MTPAV_MIDIOUT_2           = 902;
  MM_MOTU_MTPAV_MIDIIN_3            = 903;
  MM_MOTU_MTPAV_MIDIOUT_3           = 903;
  MM_MOTU_MTPAV_MIDIIN_4            = 904;
  MM_MOTU_MTPAV_MIDIOUT_4           = 904;
  MM_MOTU_MTPAV_MIDIIN_5            = 905;
  MM_MOTU_MTPAV_MIDIOUT_5           = 905;
  MM_MOTU_MTPAV_MIDIIN_6            = 906;
  MM_MOTU_MTPAV_MIDIOUT_6           = 906;
  MM_MOTU_MTPAV_MIDIIN_7            = 907;
  MM_MOTU_MTPAV_MIDIOUT_7           = 907;
  MM_MOTU_MTPAV_MIDIIN_8            = 908;
  MM_MOTU_MTPAV_MIDIOUT_8           = 908;
  MM_MOTU_MTPAV_NET_MIDIIN_1        = 909;
  MM_MOTU_MTPAV_NET_MIDIOUT_1       = 909;
  MM_MOTU_MTPAV_NET_MIDIIN_2        = 910;
  MM_MOTU_MTPAV_NET_MIDIOUT_2       = 910;
  MM_MOTU_MTPAV_NET_MIDIIN_3        = 911;
  MM_MOTU_MTPAV_NET_MIDIOUT_3       = 911;
  MM_MOTU_MTPAV_NET_MIDIIN_4        = 912;
  MM_MOTU_MTPAV_NET_MIDIOUT_4       = 912;
  MM_MOTU_MTPAV_NET_MIDIIN_5        = 913;
  MM_MOTU_MTPAV_NET_MIDIOUT_5       = 913;
  MM_MOTU_MTPAV_NET_MIDIIN_6        = 914;
  MM_MOTU_MTPAV_NET_MIDIOUT_6       = 914;
  MM_MOTU_MTPAV_NET_MIDIIN_7        = 915;
  MM_MOTU_MTPAV_NET_MIDIOUT_7       = 915;
  MM_MOTU_MTPAV_NET_MIDIIN_8        = 916;
  MM_MOTU_MTPAV_NET_MIDIOUT_8       = 916;
  MM_MOTU_MTPAV_MIDIIN_ADAT         = 917;
  MM_MOTU_MTPAV_MIDIOUT_ADAT        = 917;


(*  Product IDs for     MM_MIRO    -  miro Computer Products AG    *)
  MM_MIRO_MOVIEPRO                  = 1;       (* miroMOVIE pro *)
  MM_MIRO_VIDEOD1                   = 2;       (* miroVIDEO D1 *)
  MM_MIRO_VIDEODC1TV                = 3;       (* miroVIDEO DC1 tv *)
  MM_MIRO_VIDEOTD                   = 4;       (* miroVIDEO 10/20 TD *)
  MM_MIRO_DC30_WAVEOUT              = 5;
  MM_MIRO_DC30_WAVEIN               = 6;
  MM_MIRO_DC30_MIX                  = 7;

(*  Product IDs for     MM_NEC    -  NEC    *)
  MM_NEC_73_86_SYNTH                = 5;
  MM_NEC_73_86_WAVEOUT              = 6;
  MM_NEC_73_86_WAVEIN               = 7;
  MM_NEC_26_SYNTH                   = 9;
  MM_NEC_MPU401_MIDIOUT             = 10;
  MM_NEC_MPU401_MIDIIN              = 11;
  MM_NEC_JOYSTICK                   = 12;

(*  Product IDs for     MM_NORRIS    -  Norris Communications, Inc.    *)
  MM_NORRIS_VOICELINK               = 1;

(*  Product IDs for     MM_NORTHERN_TELECOM    -  Northern Telecom Limited    *)
  MM_NORTEL_MPXAC_WAVEIN            = 1;       (*  MPX Audio Card Wave Input Device *)
  MM_NORTEL_MPXAC_WAVEOUT           = 2;       (* MPX Audio Card Wave Output Device *)

(*  Product IDs for     MM_NVIDIA    -  NVidia Corporation    *)
  MM_NVIDIA_WAVEOUT                 = 1;
  MM_NVIDIA_WAVEIN                  = 2;
  MM_NVIDIA_MIDIOUT                 = 3;
  MM_NVIDIA_MIDIIN                  = 4;
  MM_NVIDIA_GAMEPORT                = 5;
  MM_NVIDIA_MIXER                   = 6;
  MM_NVIDIA_AUX                     = 7;

(*  Product IDs for     MM_OKSORI    -  OKSORI Co., Ltd.    *)
  MM_OKSORI_BASE                    = 0;                      (* Oksori Base *)
  MM_OKSORI_OSR8_WAVEOUT            = MM_OKSORI_BASE+1;       (* Oksori 8bit Wave out *)
  MM_OKSORI_OSR8_WAVEIN             = MM_OKSORI_BASE+2;       (* Oksori 8bit Wave in *)
  MM_OKSORI_OSR16_WAVEOUT           = MM_OKSORI_BASE+3;       (* Oksori 16 bit Wave out *)
  MM_OKSORI_OSR16_WAVEIN            = MM_OKSORI_BASE+4;       (* Oksori 16 bit Wave in *)
  MM_OKSORI_FM_OPL4                 = MM_OKSORI_BASE+5;       (* Oksori FM Synth Yamaha OPL4 *)
  MM_OKSORI_MIX_MASTER              = MM_OKSORI_BASE+6;       (* Oksori DSP Mixer - Master Volume *)
  MM_OKSORI_MIX_WAVE                = MM_OKSORI_BASE+7;       (* Oksori DSP Mixer - Wave Volume *)
  MM_OKSORI_MIX_FM                  = MM_OKSORI_BASE+8;       (* Oksori DSP Mixer - FM Volume *)
  MM_OKSORI_MIX_LINE                = MM_OKSORI_BASE+9;       (* Oksori DSP Mixer - Line Volume *)
  MM_OKSORI_MIX_CD                  = MM_OKSORI_BASE+10;      (* Oksori DSP Mixer - CD Volume *)
  MM_OKSORI_MIX_MIC                 = MM_OKSORI_BASE+11;      (* Oksori DSP Mixer - MIC Volume *)
  MM_OKSORI_MIX_ECHO                = MM_OKSORI_BASE+12;      (* Oksori DSP Mixer - Echo Volume *)
  MM_OKSORI_MIX_AUX1                = MM_OKSORI_BASE+13;      (* Oksori AD1848 - AUX1 Volume *)
  MM_OKSORI_MIX_LINE1               = MM_OKSORI_BASE+14;      (* Oksori AD1848 - LINE1 Volume *)
  MM_OKSORI_EXT_MIC1                = MM_OKSORI_BASE+15;      (* Oksori External - One Mic Connect *)
  MM_OKSORI_EXT_MIC2                = MM_OKSORI_BASE+16;      (* Oksori External - Two Mic Connect *)
  MM_OKSORI_MIDIOUT                 = MM_OKSORI_BASE+17;      (* Oksori MIDI Out Device *)
  MM_OKSORI_MIDIIN                  = MM_OKSORI_BASE+18;      (* Oksori MIDI In Device *)
  MM_OKSORI_MPEG_CDVISION           = MM_OKSORI_BASE+19;      (* Oksori CD-Vision MPEG Decoder *)

(*  Product IDs for     MM_OSITECH    -  Ositech Communications Inc.    *)
  MM_OSITECH_TRUMPCARD              = 1;       (* Trumpcard *)

(*  Product IDs for     MM_OSPREY    -  Osprey Technologies, Inc.    *)
  MM_OSPREY_1000WAVEIN              = 1;
  MM_OSPREY_1000WAVEOUT             = 2;

(*  Product IDs for     MM_QUARTERDECK    -  Quarterdeck Corporation    *)
  MM_QUARTERDECK_LHWAVEIN           = 0;      (* Quarterdeck L&H Codec Wave In *)
  MM_QUARTERDECK_LHWAVEOUT          = 1;      (* Quarterdeck L&H Codec Wave Out *)

(*  Product IDs for     MM_RHETOREX    -  Rhetorex Inc    *)
  MM_RHETOREX_WAVEIN                = 1;
  MM_RHETOREX_WAVEOUT               = 2;

(*  Product IDs for     MM_ROCKWELL    -  Rockwell International    *)
  MM_VOICEMIXER                     = 1;
  ROCKWELL_WA1_WAVEIN               = 100;
  ROCKWELL_WA1_WAVEOUT              = 101;
  ROCKWELL_WA1_SYNTH                = 102;
  ROCKWELL_WA1_MIXER                = 103;
  ROCKWELL_WA1_MPU401_IN            = 104;
  ROCKWELL_WA1_MPU401_OUT           = 105;
  ROCKWELL_WA2_WAVEIN               = 200;
  ROCKWELL_WA2_WAVEOUT              = 201;
  ROCKWELL_WA2_SYNTH                = 202;
  ROCKWELL_WA2_MIXER                = 203;
  ROCKWELL_WA2_MPU401_IN            = 204;
  ROCKWELL_WA2_MPU401_OUT           = 205;

(*  Product IDs for     MM_S3    -  S3    *)
  MM_S3_WAVEOUT                     = $1;
  MM_S3_WAVEIN                      = $2;
  MM_S3_MIDIOUT                     = $3;
  MM_S3_MIDIIN                      = $4;
  MM_S3_FMSYNTH                     = $5;
  MM_S3_MIXER                       = $6;
  MM_S3_AUX                         = $7;

(*  Product IDs for     MM_SEERSYS    -  Seer Systems, Inc.    *)
  MM_SEERSYS_SEERSYNTH              = 1;
  MM_SEERSYS_SEERWAVE               = 2;
  MM_SEERSYS_SEERMIX                = 3;

(*  Product IDs for     MM_SOFTSOUND    -  Softsound, Ltd.    *)
  MM_SOFTSOUND_CODEC                = 1;

(*  Product IDs for     MM_SOUNDESIGNS    -  SounDesignS M.C.S. Ltd.    *)
  MM_SOUNDESIGNS_WAVEIN             = 1;
  MM_SOUNDESIGNS_WAVEOUT            = 2;

(*  Product IDs for     MM_SPECTRUM_SIGNAL_PROCESSING    -  Spectrum Signal Processing, Inc.    *)
  MM_SSP_SNDFESWAVEIN               = 1;       (* Sound Festa Wave In Device *)
  MM_SSP_SNDFESWAVEOUT              = 2;       (* Sound Festa Wave Out Device *)
  MM_SSP_SNDFESMIDIIN               = 3;       (* Sound Festa MIDI In Device *)
  MM_SSP_SNDFESMIDIOUT              = 4;       (* Sound Festa MIDI Out Device *)
  MM_SSP_SNDFESSYNTH                = 5;       (* Sound Festa MIDI Synth Device *)
  MM_SSP_SNDFESMIX                  = 6;       (* Sound Festa Mixer Device *)
  MM_SSP_SNDFESAUX                  = 7;       (* Sound Festa Auxilliary Device *)

(*  Product IDs for     MM_TDK    -  TDK Corporation    *)
  MM_TDK_MW_MIDI_SYNTH              = 1;
  MM_TDK_MW_MIDI_IN                 = 2;
  MM_TDK_MW_MIDI_OUT                = 3;
  MM_TDK_MW_WAVE_IN                 = 4;
  MM_TDK_MW_WAVE_OUT                = 5;
  MM_TDK_MW_AUX                     = 6;
  MM_TDK_MW_MIXER                   = 10;
  MM_TDK_MW_AUX_MASTER              = 100;
  MM_TDK_MW_AUX_BASS                = 101;
  MM_TDK_MW_AUX_TREBLE              = 102;
  MM_TDK_MW_AUX_MIDI_VOL            = 103;
  MM_TDK_MW_AUX_WAVE_VOL            = 104;
  MM_TDK_MW_AUX_WAVE_RVB            = 105;
  MM_TDK_MW_AUX_WAVE_CHR            = 106;
  MM_TDK_MW_AUX_VOL                 = 107;
  MM_TDK_MW_AUX_RVB                 = 108;
  MM_TDK_MW_AUX_CHR                 = 109;

(*  Product IDs for     MM_TURTLE_BEACH    -  Turtle Beach, Inc.    *)
  MM_TBS_TROPEZ_WAVEIN              = 37;
  MM_TBS_TROPEZ_WAVEOUT             = 38;
  MM_TBS_TROPEZ_AUX1                = 39;
  MM_TBS_TROPEZ_AUX2                = 40;
  MM_TBS_TROPEZ_LINE                = 41;

(*  Product IDs for     MM_VIENNASYS    -  Vienna Systems    *)
  MM_VIENNASYS_TSP_WAVE_DRIVER      = 1;

(*  Product IDs for     MM_VIONA    -  Viona Development GmbH    *)
  MM_VIONA_QVINPCI_MIXER            = 1;       (* Q-Motion PCI II/Bravado 2000 *)
  MM_VIONA_QVINPCI_WAVEIN           = 2;
  MM_VIONAQVINPCI_WAVEOUT           = 3;
  MM_VIONA_BUSTER_MIXER             = 4;       (* Buster *)
  MM_VIONA_CINEMASTER_MIXER         = 5;       (* Cinemaster *)
  MM_VIONA_CONCERTO_MIXER           = 6;       (* Concerto *)

(*  Product IDs for     MM_WILDCAT    -  Wildcat Canyon Software    *)
  MM_WILDCAT_AUTOSCOREMIDIIN        = 1;       (* Autoscore *)

(*  Product IDs for     MM_WILLOWPOND    -  Willow Pond Corporation    *)
  MM_WILLOWPOND_FMSYNTH_STEREO      = 20;
  MM_WILLOWPOND_SNDPORT_WAVEIN      = 100;
  MM_WILLOWPOND_SNDPORT_WAVEOUT     = 101;
  MM_WILLOWPOND_SNDPORT_MIXER       = 102;
  MM_WILLOWPOND_SNDPORT_AUX         = 103;
  MM_WILLOWPOND_PH_WAVEIN           = 104;
  MM_WILLOWPOND_PH_WAVEOUT          = 105;
  MM_WILLOWPOND_PH_MIXER            = 106;
  MM_WILLOWPOND_PH_AUX              = 107;

(*  Product IDs for     MM_WORKBIT    -  Workbit Corporation    *)
  MM_WORKBIT_MIXER                  = 1;      (* Harmony Mixer *)
  MM_WORKBIT_WAVEOUT                = 2;      (* Harmony Mixer *)
  MM_WORKBIT_WAVEIN                 = 3;      (* Harmony Mixer *)
  MM_WORKBIT_MIDIIN                 = 4;      (* Harmony Mixer *)
  MM_WORKBIT_MIDIOUT                = 5;      (* Harmony Mixer *)
  MM_WORKBIT_FMSYNTH                = 6;      (* Harmony Mixer *)
  MM_WORKBIT_AUX                    = 7;      (* Harmony Mixer *)
  MM_WORKBIT_JOYSTICK               = 8;

(*  Product IDs for     MM_FRAUNHOFER_IIS -  Fraunhofer *)
  MM_FHGIIS_MPEGLAYER3              = 10;

{(*)///////////////////////////////////////////////////////////////////////// *)

(*              INFO LIST CHUNKS (from the Multimedia Programmer's Reference
                                        plus new ones)                       *)
  RIFFINFO_IARL      = mmioFOURCC ('I', 'A', 'R', 'L');     (*Archival location  *)
#define RIFFINFO_IART      mmioFOURCC ('I', 'A', 'R', 'T')     (*Artist  *)
#define RIFFINFO_ICMS      mmioFOURCC ('I', 'C', 'M', 'S')     (*Commissioned  *)
#define RIFFINFO_ICMT      mmioFOURCC ('I', 'C', 'M', 'T')     (*Comments  *)
#define RIFFINFO_ICOP      mmioFOURCC ('I', 'C', 'O', 'P')     (*Copyright  *)
#define RIFFINFO_ICRD      mmioFOURCC ('I', 'C', 'R', 'D')     (*Creation date of subject  *)
#define RIFFINFO_ICRP      mmioFOURCC ('I', 'C', 'R', 'P')     (*Cropped  *)
#define RIFFINFO_IDIM      mmioFOURCC ('I', 'D', 'I', 'M')     (*Dimensions  *)
#define RIFFINFO_IDPI      mmioFOURCC ('I', 'D', 'P', 'I')     (*Dots per inch  *)
#define RIFFINFO_IENG      mmioFOURCC ('I', 'E', 'N', 'G')     (*Engineer  *)
#define RIFFINFO_IGNR      mmioFOURCC ('I', 'G', 'N', 'R')     (*Genre  *)
#define RIFFINFO_IKEY      mmioFOURCC ('I', 'K', 'E', 'Y')     (*Keywords  *)
#define RIFFINFO_ILGT      mmioFOURCC ('I', 'L', 'G', 'T')     (*Lightness settings  *)
#define RIFFINFO_IMED      mmioFOURCC ('I', 'M', 'E', 'D')     (*Medium  *)
#define RIFFINFO_INAM      mmioFOURCC ('I', 'N', 'A', 'M')     (*Name of subject  *)
#define RIFFINFO_IPLT      mmioFOURCC ('I', 'P', 'L', 'T')     (*Palette Settings. No. of colors requested.   *)
#define RIFFINFO_IPRD      mmioFOURCC ('I', 'P', 'R', 'D')     (*Product  *)
#define RIFFINFO_ISBJ      mmioFOURCC ('I', 'S', 'B', 'J')     (*Subject description  *)
#define RIFFINFO_ISFT      mmioFOURCC ('I', 'S', 'F', 'T')     (*Software. Name of package used to create file.  *)
#define RIFFINFO_ISHP      mmioFOURCC ('I', 'S', 'H', 'P')     (*Sharpness.  *)
#define RIFFINFO_ISRC      mmioFOURCC ('I', 'S', 'R', 'C')     (*Source.   *)
#define RIFFINFO_ISRF      mmioFOURCC ('I', 'S', 'R', 'F')     (*Source Form. ie slide, paper  *)
#define RIFFINFO_ITCH      mmioFOURCC ('I', 'T', 'C', 'H')     (*Technician who digitized the subject.  *)

(* New INFO Chunks as of August 30, 1993: *)
#define RIFFINFO_ISMP      mmioFOURCC ('I', 'S', 'M', 'P')     (*SMPTE time code  *)
(* ISMP: SMPTE time code of digitization start point expressed as a NULL terminated
                text string "HH:MM:SS:FF". If performing MCI capture in AVICAP, this
                chunk will be automatically set based on the MCI start time.
*)
#define RIFFINFO_IDIT      mmioFOURCC ('I', 'D', 'I', 'T')     (*Digitization Time  *)
(* IDIT: "Digitization Time" Specifies the time and date that the digitization commenced.
                The digitization time is contained in an ASCII string which
                contains exactly 26 characters and is in the format
                "Wed Jan 02 02:03:55 1990\n\0".
                The ctime(), asctime(), functions can be used to create strings
                in this format. This chunk is automatically added to the capture
                file based on the current system time at the moment capture is initiated.
*)

(*Template line for new additions*)
(*#define RIFFINFO_I      mmioFOURCC ('I', '', '', '')        *) }


(* WAVE form wFormatTag IDs *)
  WAVE_FORMAT_UNKNOWN    =$0000;  (*  Microsoft Corporation  *)
  WAVE_FORMAT_ADPCM      =$0002;  (*  Microsoft Corporation  *)
  WAVE_FORMAT_IEEE_FLOAT =$0003;  (*  Microsoft Corporation  *)
                                        (*  IEEE754: range (+1, -1]  *)
                                        (*  32-bit/64-bit format as defined by *)
                                        (*  MSVC++ float/double type *)
  WAVE_FORMAT_IBM_CVSD   =$0005;  (*  IBM Corporation  *)
  WAVE_FORMAT_ALAW       =$0006;  (*  Microsoft Corporation  *)
  WAVE_FORMAT_MULAW      =$0007;  (*  Microsoft Corporation  *)
  WAVE_FORMAT_OKI_ADPCM  =$0010;  (*  OKI  *)
  WAVE_FORMAT_DVI_ADPCM  =$0011;  (*  Intel Corporation  *)
  WAVE_FORMAT_IMA_ADPCM  =(WAVE_FORMAT_DVI_ADPCM); (*  Intel Corporation  *)
  WAVE_FORMAT_MEDIASPACE_ADPCM   =$0012;  (*  Videologic  *)
  WAVE_FORMAT_SIERRA_ADPCM       =$0013;  (*  Sierra Semiconductor Corp  *)
  WAVE_FORMAT_G723_ADPCM =$0014;  (*  Antex Electronics Corporation  *)
  WAVE_FORMAT_DIGISTD    =$0015;  (*  DSP Solutions, Inc.  *)
  WAVE_FORMAT_DIGIFIX    =$0016;  (*  DSP Solutions, Inc.  *)
  WAVE_FORMAT_DIALOGIC_OKI_ADPCM =$0017;  (*  Dialogic Corporation  *)
  WAVE_FORMAT_MEDIAVISION_ADPCM  =$0018;  (*  Media Vision, Inc. *)
  WAVE_FORMAT_YAMAHA_ADPCM       =$0020;  (*  Yamaha Corporation of America  *)
  WAVE_FORMAT_SONARC     =$0021;  (*  Speech Compression  *)
  WAVE_FORMAT_DSPGROUP_TRUESPEECH        =$0022;  (*  DSP Group, Inc  *)
  WAVE_FORMAT_ECHOSC1    =$0023;  (*  Echo Speech Corporation  *)
  WAVE_FORMAT_AUDIOFILE_AF36     =$0024;  (*    *)
  WAVE_FORMAT_APTX       =$0025;  (*  Audio Processing Technology  *)
  WAVE_FORMAT_AUDIOFILE_AF10     =$0026;  (*    *)
  WAVE_FORMAT_DOLBY_AC2  =$0030;  (*  Dolby Laboratories  *)
  WAVE_FORMAT_GSM610     =$0031;  (*  Microsoft Corporation  *)
  WAVE_FORMAT_MSNAUDIO   =$0032;  (*  Microsoft Corporation  *)
  WAVE_FORMAT_ANTEX_ADPCME       =$0033;  (*  Antex Electronics Corporation  *)
  WAVE_FORMAT_CONTROL_RES_VQLPC  =$0034;  (*  Control Resources Limited  *)
  WAVE_FORMAT_DIGIREAL   =$0035;  (*  DSP Solutions, Inc.  *)
  WAVE_FORMAT_DIGIADPCM  =$0036;  (*  DSP Solutions, Inc.  *)
  WAVE_FORMAT_CONTROL_RES_CR10   =$0037;  (*  Control Resources Limited  *)
  WAVE_FORMAT_NMS_VBXADPCM       =$0038;  (*  Natural MicroSystems  *)
  WAVE_FORMAT_CS_IMAADPCM =$0039; (* Crystal Semiconductor IMA ADPCM *)
  WAVE_FORMAT_ECHOSC3     =$003A; (* Echo Speech Corporation *)
  WAVE_FORMAT_ROCKWELL_ADPCM     =$003B;  (* Rockwell International *)
  WAVE_FORMAT_ROCKWELL_DIGITALK  =$003C;  (* Rockwell International *)
  WAVE_FORMAT_XEBEC      =$003D;  (* Xebec Multimedia Solutions Limited *)
  WAVE_FORMAT_G721_ADPCM =$0040;  (*  Antex Electronics Corporation  *)
  WAVE_FORMAT_G728_CELP  =$0041;  (*  Antex Electronics Corporation  *)
  WAVE_FORMAT_MPEG       =$0050;  (*  Microsoft Corporation  *)
  WAVE_FORMAT_MPEGLAYER3 =$0055;  (*  ISO/MPEG Layer3 Format Tag *)
  WAVE_FORMAT_CIRRUS     =$0060;  (*  Cirrus Logic  *)
  WAVE_FORMAT_ESPCM      =$0061;  (*  ESS Technology  *)
  WAVE_FORMAT_VOXWARE    =$0062;  (*  Voxware Inc  *)
  WAVEFORMAT_CANOPUS_ATRAC       =$0063;  (*  Canopus, co., Ltd.  *)
  WAVE_FORMAT_G726_ADPCM =$0064;  (*  APICOM  *)
  WAVE_FORMAT_G722_ADPCM =$0065;  (*  APICOM      *)
  WAVE_FORMAT_DSAT       =$0066;  (*  Microsoft Corporation  *)
  WAVE_FORMAT_DSAT_DISPLAY       =$0067;  (*  Microsoft Corporation  *)
  WAVE_FORMAT_SOFTSOUND  =$0080;  (*  Softsound, Ltd.      *)
  WAVE_FORMAT_RHETOREX_ADPCM     =$0100;  (*  Rhetorex Inc  *)
  WAVE_FORMAT_CREATIVE_ADPCM     =$0200;  (*  Creative Labs, Inc  *)
  WAVE_FORMAT_CREATIVE_FASTSPEECH8       =$0202;  (*  Creative Labs, Inc  *)
  WAVE_FORMAT_CREATIVE_FASTSPEECH10      =$0203;  (*  Creative Labs, Inc  *)
  WAVE_FORMAT_QUARTERDECK =$0220; (*  Quarterdeck Corporation  *)
  WAVE_FORMAT_FM_TOWNS_SND       =$0300;  (*  Fujitsu Corp.  *)
  WAVE_FORMAT_BTV_DIGITAL        =$0400;  (*  Brooktree Corporation  *)
  WAVE_FORMAT_OLIGSM     =$1000;  (*  Ing C. Olivetti & C., S.p.A.  *)
  WAVE_FORMAT_OLIADPCM   =$1001;  (*  Ing C. Olivetti & C., S.p.A.  *)
  WAVE_FORMAT_OLICELP    =$1002;  (*  Ing C. Olivetti & C., S.p.A.  *)
  WAVE_FORMAT_OLISBC     =$1003;  (*  Ing C. Olivetti & C., S.p.A.  *)
  WAVE_FORMAT_OLIOPR     =$1004;  (*  Ing C. Olivetti & C., S.p.A.  *)
  WAVE_FORMAT_LH_CODEC   =$1100;  (*  Lernout & Hauspie  *)
  WAVE_FORMAT_NORRIS     =$1400;  (*  Norris Communications, Inc.  *)

//
//  the WAVE_FORMAT_DEVELOPMENT format tag can be used during the
//  development phase of a new wave format.  Before shipping, you MUST
//  acquire an official format tag from Microsoft.
//
  WAVE_FORMAT_DEVELOPMENT         = ($FFFF);


  ACM_MPEG_LAYER1             =($0001);
  ACM_MPEG_LAYER2             =($0002);
  ACM_MPEG_LAYER3             =($0004);
  ACM_MPEG_STEREO             =($0001);
  ACM_MPEG_JOINTSTEREO        =($0002);
  ACM_MPEG_DUALCHANNEL        =($0004);
  ACM_MPEG_SINGLECHANNEL      =($0008);
  ACM_MPEG_PRIVATEBIT         =($0001);
  ACM_MPEG_COPYRIGHT          =($0002);
  ACM_MPEG_ORIGINALHOME       =($0004);
  ACM_MPEG_PROTECTIONBIT      =($0008);
  ACM_MPEG_ID_MPEG1           =($0010);

//
// MPEG Layer3 WAVEFORMATEX structure
// for WAVE_FORMAT_MPEGLAYER3 ($0055)
//
  MPEGLAYER3_WFX_EXTRA_BYTES  = 12;

  MPEGLAYER3_ID_UNKNOWN           = 0;
  MPEGLAYER3_ID_MPEG              = 1;
  MPEGLAYER3_ID_CONSTANTFRAMESIZE = 2;

  MPEGLAYER3_FLAG_PADDING_ISO     = $00000000;
  MPEGLAYER3_FLAG_PADDING_ON      = $00000001;
  MPEGLAYER3_FLAG_PADDING_OFF     = $00000002;

  WAVE_FILTER_UNKNOWN        = $0000;
  WAVE_FILTER_DEVELOPMENT    =($FFFF);

  WAVE_FILTER_VOLUME      = $0001;
  WAVE_FILTER_ECHO        = $0002;

  BI_BITFIELDS    = 3;

  QUERYDIBSUPPORT = 3073;
  QDI_SETDIBITS   = $0001;
  QDI_GETDIBITS   = $0002;
  QDI_DIBTOSCREEN = $0004;
  QDI_STRETCHDIB  = $0008;

  JPEG_PROCESS_BASELINE         = 0;       (* Baseline DCT *)


(*
//
// Microsoft MPEG audio WAV definition
//
  MPEG-1 audio wave format (audio layer only).   (0x0050)   *)
type
  PMPEG1WAVEFORMAT = ^MPEG1WAVEFORMAT;
  mpeg1waveformat_tag = record
    wfx: tWAVEFORMATEX;
    fwHeadLayer: word;
    dwHeadBitrate: dword;
    fwHeadMode: word;
    fwHeadModeExt: word;
    wHeadEmphasis: word;
    fwHeadFlags: word;
    dwPTSLow: dword;
    dwPTSHigh: dword;
  end;
  MPEG1WAVEFORMAT = mpeg1waveformat_tag;

(*
// MPEG Layer3 WAVEFORMATEX structure
// for WAVE_FORMAT_MPEGLAYER3 (0x0055)
//
#define MPEGLAYER3_WFX_EXTRA_BYTES   12

// WAVE_FORMAT_MPEGLAYER3 format sructure *)
  PMPEGLAYER3WAVEFORMAT = ^MPEGLAYER3WAVEFORMAT;
  mpeglayer3waveformat_tag = record
    wfx: tWAVEFORMATEX;
    wID: word;
    fdwFlags: dword;
    nBlockSize: word;
    nFramesPerBlock: word;
    nCodecDelay: word;
  end;
  MPEGLAYER3WAVEFORMAT = mpeglayer3waveformat_tag;

implementation

end.
