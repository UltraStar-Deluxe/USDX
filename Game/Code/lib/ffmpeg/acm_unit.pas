unit acm_unit;

interface

uses
  windows, mmsystem;
                                                                              (*
  ******************************************************************************
  *     интерфейс к MS Audio Compression Manager
  ******************************************************************************
                                                                              *)
type
  PWaveFilter = ^TWaveFilter;
  // Defined in mmreg.h
  WAVEFILTER = packed record
    cbStruct: DWORD;                    // Size of the filter in bytes
    dwFilterTag: DWORD;                 // filter type
    fdwFilter: DWORD;                   // Flags for the filter (Universal Dfns)
    dwReserved: array [0..4] of DWORD;  // Reserved for system use
  end;
  TWaveFilter = WAVEFILTER;

  HACMDRIVERID__ = record
    Unused: Integer;
  end;
  {$EXTERNALSYM HACMDRIVERID__}
  HACMDRIVERID = ^HACMDRIVERID__;
  {$EXTERNALSYM HACMDRIVERID}
  PHACMDRIVERID = ^HACMDRIVERID;
  {$EXTERNALSYM PHACMDRIVERID}
  LPHACMDRIVERID = ^HACMDRIVERID;
  {$EXTERNALSYM LPHACMDRIVERID}

  HACMDRIVER__ = record
    Unused: Integer;
  end;

  {$EXTERNALSYM HACMDRIVER__}
  HACMDRIVER = ^HACMDRIVER__;
  {$EXTERNALSYM HACMDRIVER}
  PHACMDRIVER = ^HACMDRIVER;
  {$EXTERNALSYM PHACMDRIVER}
  LPHACMDRIVER = ^HACMDRIVER;
  {$EXTERNALSYM LPHACMDRIVER}

  HACMSTREAM__ = record
    Unused: Integer;
  end;

  {$EXTERNALSYM HACMSTREAM__}
  HACMSTREAM = ^HACMSTREAM__;
  {$EXTERNALSYM HACMSTREAM}
  PHACMSTREAM = ^HACMSTREAM;
  {$EXTERNALSYM PHACMSTREAM}
  LPHACMSTREAM = ^HACMSTREAM;
  {$EXTERNALSYM LPHACMSTREAM}

  PAcmStreamHeader = ^TAcmStreamHeader;
  ACMSTREAMHEADER = packed record
    cbStruct: DWORD;
    fdwStatus: DWORD;
    dwUser: DWORD;
    pbSrc: PBYTE;
    cbSrcLength: DWORD;
    cbSrcLengthUsed: DWORD;
    dwSrcUser: DWORD;
    pbDst: PBYTE;
    cbDstLength: DWORD;
    cbDstLengthUsed: DWORD;
    dwDstUser: DWORD;
    dwReservedDriver: array [0..10 - 1] of DWORD;
  end;
  {$EXTERNALSYM tACMSTREAMHEADER}
  TAcmStreamHeader = ACMSTREAMHEADER;

const
  ACMSTREAMHEADER_STATUSF_DONE     = $00010000;
  {$EXTERNALSYM ACMSTREAMHEADER_STATUSF_DONE}
  ACMSTREAMHEADER_STATUSF_PREPARED = $00020000;
  {$EXTERNALSYM ACMSTREAMHEADER_STATUSF_PREPARED}
  ACMSTREAMHEADER_STATUSF_INQUEUE  = $00100000;
  {$EXTERNALSYM ACMSTREAMHEADER_STATUSF_INQUEUE}

function acmStreamOpen(var phas: HACMSTREAM; had: HACMDRIVER; var pwfxSrc: TWAVEFORMATEX;
  var pwfxDst: TWAVEFORMATEX; pwfltr: PWAVEFILTER; dwCallback: DWORD; dwInstance: DWORD;
  fdwOpen: DWORD): MMRESULT; stdcall;
{$EXTERNALSYM acmStreamOpen}

const
  ACM_STREAMOPENF_QUERY       = $00000001;
  {$EXTERNALSYM ACM_STREAMOPENF_QUERY}
  ACM_STREAMOPENF_ASYNC       = $00000002;
  {$EXTERNALSYM ACM_STREAMOPENF_ASYNC}
  ACM_STREAMOPENF_NONREALTIME = $00000004;
  {$EXTERNALSYM ACM_STREAMOPENF_NONREALTIME}

function acmStreamSize(has: HACMSTREAM; cbInput: DWORD; var pdwOutputBytes: DWORD;
  fdwSize: DWORD): MMRESULT; stdcall;
{$EXTERNALSYM acmStreamSize}

const
  ACM_STREAMSIZEF_SOURCE      = $00000000;
  {$EXTERNALSYM ACM_STREAMSIZEF_SOURCE}
  ACM_STREAMSIZEF_DESTINATION = $00000001;
  {$EXTERNALSYM ACM_STREAMSIZEF_DESTINATION}
  ACM_STREAMSIZEF_QUERYMASK   = $0000000F;
  {$EXTERNALSYM ACM_STREAMSIZEF_QUERYMASK}

function acmStreamConvert(has: HACMSTREAM; var pash: TAcmStreamHeader;
  fdwConvert: DWORD): MMRESULT; stdcall;
{$EXTERNALSYM acmStreamConvert}

const
  ACM_STREAMCONVERTF_BLOCKALIGN = $00000004;
  {$EXTERNALSYM ACM_STREAMCONVERTF_BLOCKALIGN}
  ACM_STREAMCONVERTF_START      = $00000010;
  {$EXTERNALSYM ACM_STREAMCONVERTF_START}
  ACM_STREAMCONVERTF_END        = $00000020;
  {$EXTERNALSYM ACM_STREAMCONVERTF_END}

function acmStreamPrepareHeader(has: HACMSTREAM; var pash: TAcmStreamHeader;
  fdwPrepare: DWORD): MMRESULT; stdcall;
{$EXTERNALSYM acmStreamPrepareHeader}

function acmStreamUnprepareHeader(has: HACMSTREAM; var pash: TAcmStreamHeader;
  fdwUnprepare: DWORD): MMRESULT; stdcall;
{$EXTERNALSYM acmStreamUnprepareHeader}

function acmStreamClose(has: HACMSTREAM; fdwClose: DWORD): MMRESULT; stdcall;
{$EXTERNALSYM acmStreamClose}
                                                                              (*
  ******************************************************************************
  *     интерфейс к MS Audio Compression Manager
  ******************************************************************************
                                                                              *)

implementation

const
  msacm32 = 'msacm32.dll';

(*function acmGetVersion;            external msacm32 name 'acmGetVersion';
function acmMetrics;               external msacm32 name 'acmMetrics';
function acmDriverEnum;            external msacm32 name 'acmDriverEnum';
function acmDriverID;              external msacm32 name 'acmDriverID';
function acmDriverAddA;          external msacm32 name 'acmDriverAddA';
function acmDriverAddW;          external msacm32 name 'acmDriverAddW';
function acmDriverAdd;          external msacm32 name 'acmDriverAddA';
function acmDriverRemove;          external msacm32 name 'acmDriverRemove';
function acmDriverOpen;            external msacm32 name 'acmDriverOpen';
function acmDriverClose;           external msacm32 name 'acmDriverClose';
function acmDriverMessage;         external msacm32 name 'acmDriverMessage';
function acmDriverPriority;        external msacm32 name 'acmDriverPriority';
function acmDriverDetailsA;      external msacm32 name 'acmDriverDetailsA';
function acmDriverDetailsW;      external msacm32 name 'acmDriverDetailsW';
function acmDriverDetails;      external msacm32 name 'acmDriverDetailsA';
function acmFormatTagDetailsA;   external msacm32 name 'acmFormatTagDetailsA';
function acmFormatTagDetailsW;   external msacm32 name 'acmFormatTagDetailsW';
function acmFormatTagDetails;   external msacm32 name 'acmFormatTagDetailsA';
function acmFormatDetailsA;      external msacm32 name 'acmFormatDetailsA';
function acmFormatDetailsW;      external msacm32 name 'acmFormatDetailsW';
function acmFormatDetails;      external msacm32 name 'acmFormatDetailsA';
function acmFormatChooseA;       external msacm32 name 'acmFormatChooseA';
function acmFormatChooseW;       external msacm32 name 'acmFormatChooseW';
function acmFormatChoose;       external msacm32 name 'acmFormatChooseA';
function acmFormatEnumA;         external msacm32 name 'acmFormatEnumA';
function acmFormatEnumW;         external msacm32 name 'acmFormatEnumW';
function acmFormatEnum;         external msacm32 name 'acmFormatEnumA';
function acmFormatTagEnumA;      external msacm32 name 'acmFormatTagEnumA';
function acmFormatTagEnumW;      external msacm32 name 'acmFormatTagEnumW';
function acmFormatTagEnum;      external msacm32 name 'acmFormatTagEnumA';
function acmFormatSuggest;         external msacm32 name 'acmFormatSuggest';
function acmFilterTagDetailsA;   external msacm32 name 'acmFilterTagDetailsA';
function acmFilterTagDetailsW;   external msacm32 name 'acmFilterTagDetailsW';
function acmFilterTagDetails;   external msacm32 name 'acmFilterTagDetailsA';
function acmFilterTagEnumA;      external msacm32 name 'acmFilterTagEnumA';
function acmFilterTagEnumW;      external msacm32 name 'acmFilterTagEnumW';
function acmFilterTagEnum;      external msacm32 name 'acmFilterTagEnumA';
function acmFilterDetailsA;      external msacm32 name 'acmFilterDetailsA';
function acmFilterDetailsW;      external msacm32 name 'acmFilterDetailsW';
function acmFilterDetails;      external msacm32 name 'acmFilterDetailsA';
function acmFilterEnumA;         external msacm32 name 'acmFilterEnumA';
function acmFilterEnumW;         external msacm32 name 'acmFilterEnumW';
function acmFilterEnum;         external msacm32 name 'acmFilterEnumA';
function acmFilterChooseA;       external msacm32 name 'acmFilterChooseA';
function acmFilterChooseW;       external msacm32 name 'acmFilterChooseW';
function acmFilterChoose;       external msacm32 name 'acmFilterChooseA'; *)
function acmStreamOpen;            external msacm32 name 'acmStreamOpen';
function acmStreamClose;           external msacm32 name 'acmStreamClose';
function acmStreamSize;            external msacm32 name 'acmStreamSize';
//function acmStreamReset;           external msacm32 name 'acmStreamReset';
//function acmStreamMessage;         external msacm32 name 'acmStreamMessage';
function acmStreamConvert;         external msacm32 name 'acmStreamConvert';
function acmStreamPrepareHeader;   external msacm32 name 'acmStreamPrepareHeader';
function acmStreamUnprepareHeader; external msacm32 name 'acmStreamUnprepareHeader';

end.
