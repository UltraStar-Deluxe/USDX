{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/base/UMusic.pas $
 * $Id: UMusic.pas 3103 2014-11-22 23:21:19Z k-m_schindler $
 *}

unit UMusic;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  Classes,
  UTime,
  UBeatTimer,
  UPath,
  UWebcam;

type
  TNoteType = (ntFreestyle, ntNormal, ntGolden, ntRap, ntRapGolden);

  TPos = record // Tracks[track].Lines[line].Notes[note]
    track: integer;
    line:  integer;
    note:  integer;
  end;

  {**
   * acoCrop: Stretch to screen width or height, crop the other dimension
   *   + keeps aspect
   *   + no borders
   *   - frame borders are cropped (image data loss)
   * acoLetterBox: Stretch to screen width, add bars at or crop top and bottom
   *   + keeps aspect
   *   - borders at top and bottom
   *   o top/bottom is cropped if width < height (unusual)
   * acoHalfway: a compromise between crop and letterbox
   *}
  TAspectCorrection = (acoLetterBox, acoHalfway, acoCrop);

  TRectCoords = record
    Left, Right:  double;
    Upper, Lower: double;
  end;

const
  // ScoreFactor defines how a notehit of a specified notetype is
  // measured in comparison to the other types
  // 0 means this notetype is not rated at all
  // 2 means a hit of this notetype will be rated w/ twice as much
  // points as a hit of a notetype w/ ScoreFactor 1
  ScoreFactor:         array[TNoteType] of integer = (0, 1, 2, 1, 2);

type
  (**
   * TLineFragment represents a fragment of a lyrics line.
   * This is a text-fragment (e.g. a syllable) assigned to a note pitch,
   * represented by a bar in the sing-screen.
   *)
  PLineFragment = ^TLineFragment;
  TLineFragment = record
    Color:          integer;
    StartBeat:      integer;    // beat the fragment starts at
    Duration:       integer;    // duration in beats
    Tone:           integer;    // full range tone
    Text:           UTF8String; // text assigned to this fragment (a syllable, word, etc.)
    NoteType:       TNoteType;  // note-type: golden-note/freestyle etc.

    IsMedley:       boolean; // just for editor
    IsStartPreview: boolean; // just for editor

    private
    function GetEnd:integer;

    public
    property EndBeat:integer read GetEnd;


  end;

  (**
   * TLine represents one lyrics line and consists of multiple
   * notes.
   *)
  PLine = ^TLine;
  TLine = record
    StartBeat:  integer; // the start beat of this line (<> start beat of the first note of this line)
    Lyric:      UTF8String;
    //LyricWidth: real;    // @deprecated: width of the line in pixels.
                         // Do not use this as the width is not correct.
                         // Use TLyricsEngine.GetUpperLine().Width instead.
    EndBeat:    integer;
    BaseNote:   integer;
    HighNote:   integer; // index of last note in line (= High(Note)?)
    ScoreValue: integer; // value of all notes in the line
    LastLine:   boolean;
    Notes:      array of TLineFragment;

    private
    function GetLength(): integer;

    public
    { Returns whether the line has a valid length. }
    function HasLength(): boolean; overload;
    { Returns whether the line has a valid length and passes length. }
    function HasLength(out Len: Integer): boolean; overload;
    { Returns whether the line has a valid length and passes length. Output converted to Real }
    function HasLength(out Len: real): boolean; overload;
    { Returns whether the line has a valid length and passes length. Output converted to Double }
    function HasLength(out Len: double): boolean; overload;

    property Length_: integer read GetLength;

  end;

  (**
   * TLines stores sets of lyric lines and information on them.
   * Normally just one set is defined but in duet mode it might for example
   * contain two sets.
   *)
  TLines = record
    CurrentLine: integer;  // for drawing of current line
    High:        integer;  // = High(Line)!
    Number:      integer;
    Resolution:  integer;
    NotesGAP:    integer;
    ScoreValue:  integer;
    Lines:       array of TLine;
  end;

const
  FFTSize = 512; // size of FFT data (output: FFTSize/2 values)
type
  TFFTData  = array[0..(FFTSize div 2)-1] of Single;

type
  PPCMStereoSample = ^TPCMStereoSample;
  TPCMStereoSample = array[0..1] of SmallInt;
  TPCMData  = array[0..511] of TPCMStereoSample;

type
  TStreamStatus = (ssStopped, ssPlaying, ssPaused);
const
  StreamStatusStr:  array[TStreamStatus] of string =
    ('Stopped', 'Playing', 'Paused');

type
  TAudioSampleFormat = (
    asfU8, asfS8,         // unsigned/signed  8 bits
    asfU16LSB, asfS16LSB, // unsigned/signed 16 bits (endianness: LSB)
    asfU16MSB, asfS16MSB, // unsigned/signed 16 bits (endianness: MSB)
    asfU16, asfS16,       // unsigned/signed 16 bits (endianness: System)
    asfS32,               // signed 32 bits (endianness: System)
    asfFloat,             // float
    asfDouble             // double
  );

const
  // Size of one sample (one channel only) in bytes
  AudioSampleSize: array[TAudioSampleFormat] of integer = (
    1, 1,     // asfU8, asfS8
    2, 2,     // asfU16LSB, asfS16LSB
    2, 2,     // asfU16MSB, asfS16MSB
    2, 2,     // asfU16,    asfS16
    4,        // asfS32
    4,        // asfFloat
    8         // asfDouble
  );

const
  CHANNELMAP_LEFT  = 1;
  CHANNELMAP_RIGHT = 2;
  CHANNELMAP_FRONT = CHANNELMAP_LEFT or CHANNELMAP_RIGHT;

type
  TAudioFormatInfo = class
    private
      fSampleRate : double;
      fChannels   : byte;
      fFormat     : TAudioSampleFormat;
      fFrameSize  : integer;

      procedure SetChannels(Channels: byte);
      procedure SetFormat(Format: TAudioSampleFormat);
      procedure UpdateFrameSize();
      function GetBytesPerSec(): double;
      function GetSampleSize(): integer;
    public
      constructor Create(Channels: byte; SampleRate: double; Format: TAudioSampleFormat);
      function Copy(): TAudioFormatInfo;

      (**
       * Returns the inverse ratio of the size of data in this format to its
       * size in a given target format.
       * Example: SrcSize*SrcInfo.GetRatio(TgtInfo) = TgtSize
       *)
      function GetRatio(TargetInfo: TAudioFormatInfo): double;

      property SampleRate: double read fSampleRate write fSampleRate;
      property Channels: byte read fChannels write SetChannels;
      property Format: TAudioSampleFormat read fFormat write SetFormat;
      property FrameSize: integer read fFrameSize;
      property SampleSize: integer read GetSampleSize;
      property BytesPerSec: double read GetBytesPerSec;
  end;

type
  TAudioProcessingStream = class;
  TOnCloseHandler = procedure(Stream: TAudioProcessingStream);

  TAudioProcessingStream = class
    protected
      OnCloseHandlers: array of TOnCloseHandler;

      function GetLength(): real;           virtual; abstract;
      function GetPosition(): real;         virtual; abstract;
      procedure SetPosition(Time: real);    virtual; abstract;
      function GetLoop(): boolean;          virtual; abstract;
      procedure SetLoop(Enabled: boolean);  virtual; abstract;

      procedure PerformOnClose();
    public
      function GetAudioFormatInfo(): TAudioFormatInfo; virtual; abstract;
      procedure Close(); virtual; abstract;

      (**
       * Adds a new OnClose action handler.
       * The handlers are performed in the order they were added.
       * If not stated explicitely, member-variables might have been invalidated
       * already. So do not use any member (variable/method/...) if you are not
       * sure it is valid.
       *)
      procedure AddOnCloseHandler(Handler: TOnCloseHandler);

      property Length: real read GetLength;
      property Position: real read GetPosition write SetPosition;
      property Loop: boolean read GetLoop write SetLoop;
  end;

  TAudioSourceStream = class(TAudioProcessingStream)
    private
      RG: single;

    protected
      function IsEOF(): boolean;            virtual; abstract;
      function IsError(): boolean;          virtual; abstract;
      procedure SetReplayGain(const GainTag: AnsiString; const PeakTag: AnsiString);
      procedure SetReplayGainR128(const R128Tag: AnsiString);
    public
      constructor Create();
      function ReadData(Buffer: PByte; BufferSize: integer): integer; virtual; abstract;
      function GetReplayGain(): single;

      property EOF: boolean read IsEOF;
      property Error: boolean read IsError;
  end;

  (*
   * State-Chart for playback-stream state transitions
   * []: Transition, (): State
   *
   *               /---[Play/FadeIn]--->-\  /-------[Pause]----->-\
   * -[Create]->(Stop)                  (Play)                 (Pause)
   *              \\-<-[Stop/EOF*/Error]-/  \-<---[Play/FadeIn]--//
   *               \-<------------[Stop/EOF*/Error]--------------/
   *
   * *: if not looped, otherwise stream is repeated
   * Note: SetPosition() does not change the state.
   *)

  TAudioPlaybackStream = class(TAudioProcessingStream)
    protected
      AvgSyncDiff: double;  //** average difference between stream and sync clock
      SyncSource: TSyncSource;
      SourceStream: TAudioSourceStream;
      RG: single;
      RGEnabled: boolean;

      function GetLatency(): double; virtual; abstract;
      function GetStatus(): TStreamStatus;  virtual; abstract;
      function GetVolume(): single;         virtual; abstract;
      procedure SetVolume(Volume: single);  virtual; abstract;
      function Synchronize(BufferSize: integer; FormatInfo: TAudioFormatInfo): integer;
      procedure FillBufferWithFrame(Buffer: PByteArray; BufferSize: integer; Frame: PByteArray; FrameSize: integer);
    public
      constructor Create();

      (**
       * Opens a SourceStream for playback.
       * Note that the caller (not the TAudioPlaybackStream) is responsible to
       * free the SourceStream after the Playback-Stream is closed.
       * You may use an OnClose-handler to achieve this. GetSourceStream()
       * guarantees to deliver this method's SourceStream parameter to
       * the OnClose-handler. Freeing SourceStream at OnClose is allowed.
       *)
      function Open(SourceStream: TAudioSourceStream): boolean; virtual;

      procedure Play();                     virtual; abstract;
      procedure Pause();                    virtual; abstract;
      procedure Stop();                     virtual; abstract;
      procedure FadeIn(Time: real; TargetVolume: single);  virtual; abstract;
      procedure Fade(Time: real; TargetVolume: single);  virtual; abstract;

      procedure GetFFTData(var data: TFFTData);          virtual; abstract;
      function GetPCMData(var data: TPCMData): Cardinal; virtual; abstract;

      procedure SetSyncSource(SyncSource: TSyncSource);
      function GetSourceStream(): TAudioSourceStream;

      function GetReplayGainAdjustment(): single;
      procedure SetReplayGainEnabled(RGEnabled: boolean); virtual;
      function GetReplayGainEnabled(): boolean;

      property Status: TStreamStatus read GetStatus;
      property Volume: single read GetVolume write SetVolume;
      property ReplayGainAdjustment: single read GetReplayGainAdjustment;
      property ReplayGainEnabled: boolean read GetReplayGainEnabled write SetReplayGainEnabled;
  end;

  TAudioDecodeStream = class(TAudioSourceStream)
  end;

  TAudioVoiceStream = class(TAudioSourceStream)
    protected
      FormatInfo: TAudioFormatInfo;
      ChannelMap: integer;
    public
      constructor Create;
      destructor Destroy; override;

      function Open(ChannelMap: integer; FormatInfo: TAudioFormatInfo): boolean; virtual;
      procedure Close(); override;

      procedure WriteData(Buffer: PByteArray; BufferSize: integer); virtual; abstract;
      function GetAudioFormatInfo(): TAudioFormatInfo; override;

      function GetLength(): real;           override;
      function GetPosition(): real;         override;
      procedure SetPosition(Time: real);    override;
      function GetLoop(): boolean;          override;
      procedure SetLoop(Enabled: boolean);  override;
  end;

type
  // soundcard output-devices information
  TAudioOutputDevice = class
    public
      Name: UTF8String; // soundcard name
  end;
  TAudioOutputDeviceList = array of TAudioOutputDevice;

type
  IGenericPlayback = Interface
  ['{63A5EBC3-3F4D-4F23-8DFB-B5165FCE33DD}']
      function GetName: String;
  end;

  IVideo = interface
  ['{58DFC674-9168-41EA-B59D-A61307242B80}']
      procedure Play;
      procedure Pause;
      procedure Stop;

      procedure SetLoop(Enable: boolean);
      function GetLoop(): boolean;

      procedure SetPosition(Time: real);
      function GetPosition: real;

      procedure SetScreen(Screen: integer);
      function GetScreen(): integer;

      procedure SetScreenPosition(X, Y: double; Z: double = 0.0);
      procedure GetScreenPosition(var X, Y, Z: double);

      procedure  SetWidth(Width: double);
       function GetWidth(): double;

      procedure  SetHeight(Height: double);
       function GetHeight(): double;

      {**
       * Sub-image of the video frame to draw.
       * This can be used for zooming or similar purposes.
       *}
      procedure SetFrameRange(Range: TRectCoords);
      function GetFrameRange(): TRectCoords;

      function GetFrameAspect(): real;

      procedure SetAspectCorrection(AspectCorrection: TAspectCorrection);
      function GetAspectCorrection(): TAspectCorrection;


      procedure SetAlpha(Alpha: double);
      function GetAlpha(): double;

      procedure SetReflectionSpacing(Spacing: double);
      function GetReflectionSpacing(): double;

      procedure GetFrame(Time: Extended);
      procedure Draw();
      procedure DrawReflection();


      property Screen: integer read GetScreen;
      property Width: double read GetWidth write SetWidth;
      property Height: double read GetHeight write SetHeight;
      property Alpha: double read GetAlpha write SetAlpha;
      property ReflectionSpacing: double read GetReflectionSpacing write SetReflectionSpacing;
      property FrameAspect: real read GetFrameAspect;
      property AspectCorrection: TAspectCorrection read GetAspectCorrection write SetAspectCorrection;
      property Loop: boolean read GetLoop write SetLoop;
      property Position: real read GetPosition write SetPosition;
  end;

  IVideoPlayback = Interface( IGenericPlayback )
  ['{3574C40C-28AE-4201-B3D1-3D1F0759B131}']
      function Init(): boolean;
      function Finalize: boolean;

      function Open(const FileName : IPath): IVideo;
  end;

  IVideoVisualization = Interface( IVideoPlayback )
  ['{5AC17D60-B34D-478D-B632-EB00D4078017}']
  end;

  IAudioPlayback = Interface( IGenericPlayback )
  ['{E4AE0B40-3C21-4DC5-847C-20A87E0DFB96}']
      function InitializePlayback: boolean;
      function FinalizePlayback: boolean;

      function GetOutputDeviceList(): TAudioOutputDeviceList;

      procedure SetAppVolume(Volume: single);
      procedure SetVolume(Volume: single);
      procedure SetLoop(Enabled: boolean);

      procedure FadeIn(Time: real; TargetVolume: single);
      procedure Fade(Time: real; TargetVolume: single);
      procedure SetSyncSource(SyncSource: TSyncSource);

      procedure Rewind;
      function  Finished: boolean;
      function  Length: real;

      function Open(const Filename: IPath; const FilenameKaraoke: IPath): boolean; // true if succeed
      procedure Close;

      procedure Play;
      procedure ToggleKaraoke;
      procedure Pause;
      procedure Stop;

      procedure SetPosition(Time: real);
      function GetPosition: real;

      property Position: real read GetPosition write SetPosition;

      // Sounds
      // TODO:
      // add a TMediaDummyPlaybackStream implementation that will
      // be used by the TSoundLib whenever OpenSound() fails, so checking for
      // nil-pointers is not neccessary anymore.
      // PlaySound/StopSound will be removed then, OpenSound will be renamed to
      // CreateSound.
      function OpenSound(const Filename: IPath): TAudioPlaybackStream;
      function OpenSoundBuffer(Buffer: TStream; Format: TAudioFormatInfo): TAudioPlaybackStream;
      procedure PlaySound(Stream: TAudioPlaybackStream);
      procedure StopSound(Stream: TAudioPlaybackStream);

      // Equalizer
      procedure GetFFTData(var Data: TFFTData);

      // Interface for Visualizer
      function GetPCMData(var Data: TPCMData): Cardinal;

      function CreateVoiceStream(ChannelMap: integer; FormatInfo: TAudioFormatInfo): TAudioVoiceStream;
  end;

  IGenericDecoder = Interface
  ['{557B0E9A-604D-47E4-B826-13769F3E10B7}']
      function GetName(): string;
      function InitializeDecoder(): boolean;
      function FinalizeDecoder(): boolean;
      //function IsSupported(const Filename: string): boolean;
  end;

  (*
  IVideoDecoder = Interface( IGenericDecoder )
  ['{2F184B2B-FE69-44D5-9031-0A2462391DCA}']
       function Open(const Filename: IPath): TVideoDecodeStream;

       procedure SetPosition(Time: real);
       function GetPosition:  real;

       procedure UpdateTexture(Texture: glUint);

       property Loop: boolean read GetLoop write SetLoop;
       property Position: real read GetPosition write SetPosition;
  end;
  *)

  IAudioDecoder = Interface( IGenericDecoder )
  ['{AB47B1B6-2AA9-4410-BF8C-EC79561B5478}']
      function Open(const Filename: IPath): TAudioDecodeStream;
  end;

  IAudioInput = Interface
  ['{A5C8DA92-2A0C-4AB2-849B-2F7448C6003A}']
      function GetName: String;
      function InitializeRecord: boolean;
      function FinalizeRecord(): boolean;

      procedure CaptureStart;
      procedure CaptureStop;
  end;

type
  TAudioConverter = class
    protected
      fSrcFormatInfo: TAudioFormatInfo;
      fDstFormatInfo: TAudioFormatInfo;
    public
      function Init(SrcFormatInfo: TAudioFormatInfo; DstFormatInfo: TAudioFormatInfo): boolean; virtual;
      destructor Destroy(); override;

      (**
       * Converts the InputBuffer and stores the result in OutputBuffer.
       * If the result is not -1, InputSize will be set to the actual number of
       * input-buffer bytes used.
       * Returns the number of bytes written to the output-buffer or -1 if an error occured.
       *)
      function Convert(InputBuffer: PByteArray; OutputBuffer: PByteArray; var InputSize: integer): integer; virtual; abstract;

      (**
       * Destination/Source size ratio
       *)
      function GetRatio(): double; virtual; abstract;

      function GetOutputBufferSize(InputSize: integer): integer; virtual; abstract;
      property SrcFormatInfo: TAudioFormatInfo read fSrcFormatInfo;
      property DstFormatInfo: TAudioFormatInfo read fDstFormatInfo;
  end;

(* TODO
const
  SOUNDID_START    = 0;
  SOUNDID_BACK     = 1;
  SOUNDID_SWOOSH   = 2;
  SOUNDID_CHANGE   = 3;
  SOUNDID_OPTION   = 4;
  SOUNDID_CLICK    = 5;
  LAST_SOUNDID = SOUNDID_CLICK;

  BaseSoundFilenames: array[0..LAST_SOUNDID] of IPath = (
    '%SOUNDPATH%/Common start.mp3',                 // Start
    '%SOUNDPATH%/Common back.mp3',                  // Back
    '%SOUNDPATH%/menu swoosh.mp3',                  // Swoosh
    '%SOUNDPATH%/select music change music 50.mp3', // Change
    '%SOUNDPATH%/option change col.mp3',            // Option
    '%SOUNDPATH%/rimshot022b.mp3'                   // Click
    {
    '%SOUNDPATH%/bassdrumhard076b.mp3',             // Drum (unused)
    '%SOUNDPATH%/hihatclosed068b.mp3',              // Hihat (unused)
    '%SOUNDPATH%/claps050b.mp3',                    // Clap (unused)
    '%SOUNDPATH%/Shuffle.mp3'                       // Shuffle (unused)
    }
  );
*)

type
  TSoundLibrary = class
    private
      // TODO
      //Sounds: array of TAudioPlaybackStream;
    public
      // TODO: move sounds to the private section
      // and provide IDs instead.
      Start:   TAudioPlaybackStream;
      Back:    TAudioPlaybackStream;
      Swoosh:  TAudioPlaybackStream;
      Change:  TAudioPlaybackStream;
      Option:  TAudioPlaybackStream;
      Click:   TAudioPlaybackStream;
      Ping:   TAudioPlaybackStream;
      Applause:TAudioPlaybackStream;
      BGMusic: TAudioPlaybackStream;

      constructor Create();
      destructor Destroy(); override;

      procedure LoadSounds();
      procedure UnloadSounds();

      procedure StartBgMusic();
      procedure PauseBgMusic();
      // TODO
      //function AddSound(Filename: IPath): integer;
      //procedure RemoveSound(ID: integer);
      //function GetSound(ID: integer): TAudioPlaybackStream;
      //property Sound[ID: integer]: TAudioPlaybackStream read GetSound; default;
  end;

var
  // TODO: JB --- THESE SHOULD NOT BE GLOBAL
  Tracks: array of TLines;
  LyricsState: TLyricsState;
  SoundLib: TSoundLibrary;


procedure InitializeSound;
procedure InitializeVideo;
procedure FinalizeMedia;

function  Visualization(): IVideoPlayback;
function  VideoPlayback(): IVideoPlayback;
function  AudioPlayback(): IAudioPlayback;
function  AudioInput(): IAudioInput;
function  AudioDecoders(): TInterfaceList;

function  MediaManager: TInterfaceList;

procedure DumpMediaInterfaces();

function FindNote(beat: integer): TPos;

implementation

uses
  math,
  UIni,
  UNote,
  UCommandLine,
  UCommon,
  URecord,
  ULog,
  UPathUtils;

var
  DefaultVideoPlayback : IVideoPlayback;
  DefaultVisualization : IVideoPlayback;
  DefaultAudioPlayback : IAudioPlayback;
  DefaultAudioInput    : IAudioInput;
  AudioDecoderList     : TInterfaceList;
  MediaInterfaceList   : TInterfaceList;


constructor TAudioFormatInfo.Create(Channels: byte; SampleRate: double; Format: TAudioSampleFormat);
begin
  inherited Create();
  fChannels := Channels;
  fSampleRate := SampleRate;
  fFormat := Format;
  UpdateFrameSize();
end;

procedure TAudioFormatInfo.SetChannels(Channels: byte);
begin
  fChannels := Channels;
  UpdateFrameSize();
end;

procedure TAudioFormatInfo.SetFormat(Format: TAudioSampleFormat);
begin
  fFormat := Format;
  UpdateFrameSize();
end;

function TAudioFormatInfo.GetBytesPerSec(): double;
begin
  Result := FrameSize * SampleRate;
end;

function TAudioFormatInfo.GetSampleSize(): integer;
begin
  Result := AudioSampleSize[fFormat];
end;

procedure TAudioFormatInfo.UpdateFrameSize();
begin
  fFrameSize := AudioSampleSize[fFormat] * fChannels;
end;

function TAudioFormatInfo.Copy(): TAudioFormatInfo;
begin
  Result := TAudioFormatInfo.Create(Self.Channels, Self.SampleRate, Self.Format);
end;

function TAudioFormatInfo.GetRatio(TargetInfo: TAudioFormatInfo): double;
begin
  Result := (TargetInfo.FrameSize / Self.FrameSize) *
            (TargetInfo.SampleRate / Self.SampleRate)
end;


function MediaManager: TInterfaceList;
begin
  if (not assigned(MediaInterfaceList)) then
    MediaInterfaceList := TInterfaceList.Create();
  Result := MediaInterfaceList;
end;

function  VideoPlayback(): IVideoPlayback;
begin
  Result := DefaultVideoPlayback;
end;

function  Visualization(): IVideoPlayback;
begin
  Result := DefaultVisualization;
end;

function AudioPlayback(): IAudioPlayback;
begin
  Result := DefaultAudioPlayback;
end;

function AudioInput(): IAudioInput;
begin
  Result := DefaultAudioInput;
end;

function AudioDecoders(): TInterfaceList;
begin
  Result := AudioDecoderList;
end;

procedure FilterInterfaceList(const IID: TGUID; InList, OutList: TInterfaceList);
var
  i: integer;
  obj: IInterface;
begin
  if (not assigned(OutList)) then
    Exit;

  OutList.Clear;
  for i := 0 to InList.Count-1 do
  begin
    if assigned(InList[i]) then
    begin
      // add object to list if it implements the interface searched for
      if (InList[i].QueryInterface(IID, obj) = 0) then
        OutList.Add(obj);
    end;
  end;
end;

procedure InitializeSound;
var
  i: integer;
  InterfaceList: TInterfaceList;
  CurrentAudioDecoder: IAudioDecoder;
  CurrentAudioPlayback: IAudioPlayback;
  CurrentAudioInput: IAudioInput;
begin
  // create a temporary list for interface enumeration
  InterfaceList := TInterfaceList.Create();

  // initialize all audio-decoders first
  FilterInterfaceList(IAudioDecoder, MediaManager, InterfaceList);
  for i := 0 to InterfaceList.Count-1 do
  begin
    CurrentAudioDecoder := InterfaceList[i] as IAudioDecoder;
    if (not CurrentAudioDecoder.InitializeDecoder()) then
    begin
      Log.LogError('Initialize failed, Removing decoder: '+ CurrentAudioDecoder.GetName);
      MediaManager.Remove(CurrentAudioDecoder);
    end;
  end;

  // create and setup decoder-list (see AudioDecoders())
  AudioDecoderList := TInterfaceList.Create;
  FilterInterfaceList(IAudioDecoder, MediaManager, AudioDecoders);

  // find and initialize playback interface
  DefaultAudioPlayback := nil;
  FilterInterfaceList(IAudioPlayback, MediaManager, InterfaceList);
  for i := 0 to InterfaceList.Count-1 do
  begin
    CurrentAudioPlayback := InterfaceList[i] as IAudioPlayback;
    if (CurrentAudioPlayback.InitializePlayback()) then
    begin
      DefaultAudioPlayback := CurrentAudioPlayback;
      break;
    end;
    Log.LogError('Initialize failed, Removing playback: '+ CurrentAudioPlayback.GetName);
    MediaManager.Remove(CurrentAudioPlayback);
  end;

  // find and initialize input interface
  DefaultAudioInput := nil;
  FilterInterfaceList(IAudioInput, MediaManager, InterfaceList);
  for i := 0 to InterfaceList.Count-1 do
  begin
    CurrentAudioInput := InterfaceList[i] as IAudioInput;
    if (CurrentAudioInput.InitializeRecord()) then
    begin
      DefaultAudioInput := CurrentAudioInput;
      break;
    end;
    Log.LogError('Initialize failed, Removing input: '+ CurrentAudioInput.GetName);
    MediaManager.Remove(CurrentAudioInput);
  end;

  InterfaceList.Free;

  // Update input-device list with registered devices
  AudioInputProcessor.UpdateInputDeviceConfig();

  // Load in-game sounds
  SoundLib := TSoundLibrary.Create;
end;

procedure InitializeVideo();
var
  i: integer;
  InterfaceList: TInterfaceList;
  VideoInterface: IVideoPlayback;
  VisualInterface: IVideoVisualization;
begin
  InterfaceList := TInterfaceList.Create;

  // initialize and set video-playback singleton
  DefaultVideoPlayback := nil;
  FilterInterfaceList(IVideoPlayback, MediaManager, InterfaceList);
  for i := 0 to InterfaceList.Count-1 do
  begin
    VideoInterface := InterfaceList[i] as IVideoPlayback;
    if (VideoInterface.Init()) then
    begin
      DefaultVideoPlayback := VideoInterface;
      break;
    end;
    Log.LogError('Initialize failed, Removing - '+ VideoInterface.GetName);
    MediaManager.Remove(VideoInterface);
  end;

  // initialize and set visualization singleton
  DefaultVisualization := nil;
  FilterInterfaceList(IVideoVisualization, MediaManager, InterfaceList);
  for i := 0 to InterfaceList.Count-1 do
  begin
    VisualInterface := InterfaceList[i] as IVideoVisualization;
    if (VisualInterface.Init()) then
    begin
      DefaultVisualization := VisualInterface;
      break;
    end;
    Log.LogError('Initialize failed, Removing - '+ VisualInterface.GetName);
    MediaManager.Remove(VisualInterface);
  end;

  InterfaceList.Free;

  // now that we have all interfaces, we can dump them
  // TODO: move this to another place
  if FindCmdLineSwitch(cMediaInterfaces) then
  begin
    DumpMediaInterfaces();
    halt;
  end;
end;

procedure UnloadMediaModules;
var
  i: integer;
  InterfaceList: TInterfaceList;
begin
  FreeAndNil(AudioDecoderList);
  DefaultAudioPlayback := nil;
  DefaultAudioInput := nil;
  DefaultVideoPlayback := nil;
  DefaultVisualization := nil;

  // create temporary interface list
  InterfaceList := TInterfaceList.Create();

  // finalize audio playback interfaces (should be done before the decoders)
  FilterInterfaceList(IAudioPlayback, MediaManager, InterfaceList);
  for i := 0 to InterfaceList.Count-1 do
    (InterfaceList[i] as IAudioPlayback).FinalizePlayback();

  // finalize audio input interfaces
  FilterInterfaceList(IAudioInput, MediaManager, InterfaceList);
  for i := 0 to InterfaceList.Count-1 do
    (InterfaceList[i] as IAudioInput).FinalizeRecord();

  // finalize audio decoder interfaces
  FilterInterfaceList(IAudioDecoder, MediaManager, InterfaceList);
  for i := 0 to InterfaceList.Count-1 do
    (InterfaceList[i] as IAudioDecoder).FinalizeDecoder();

  // finalize video interfaces
  FilterInterfaceList(IVideoPlayback, MediaManager, InterfaceList);
  for i := 0 to InterfaceList.Count-1 do
    (InterfaceList[i] as IVideoPlayback).Finalize();

  // finalize audio decoder interfaces
  FilterInterfaceList(IVideoVisualization, MediaManager, InterfaceList);
  for i := 0 to InterfaceList.Count-1 do
    (InterfaceList[i] as IVideoVisualization).Finalize();

  InterfaceList.Free;

  // finally free interfaces (by removing all references to them)
  FreeAndNil(MediaInterfaceList);
end;

procedure FinalizeMedia;
begin
  // stop, close and free sounds
  SoundLib.Free;

  // release webcam
  Webcam.Free;

  // stop and close music stream
  if (AudioPlayback <> nil) then
    AudioPlayback.Close;

  // stop any active captures
  if (AudioInput <> nil) then
    AudioInput.CaptureStop;

  UnloadMediaModules();
end;

procedure DumpMediaInterfaces();
begin
  writeln( '' );
  writeln( '--------------------------------------------------------------' );
  writeln( '  In-use Media Interfaces                                     ' );
  writeln( '--------------------------------------------------------------' );
  writeln( 'Registered Audio Playback Interface : ' + AudioPlayback.GetName );
  writeln( 'Registered Audio Input    Interface : ' + AudioInput.GetName    );
  writeln( 'Registered Video Playback Interface : ' + VideoPlayback.GetName );
  writeln( 'Registered Visualization  Interface : ' + Visualization.GetName );
  writeln( '--------------------------------------------------------------' );
  writeln( '' );
end;


{ TSoundLibrary }

constructor TSoundLibrary.Create();
begin
  inherited;
  LoadSounds();
end;

destructor TSoundLibrary.Destroy();
begin
  UnloadSounds();
  inherited;
end;

procedure TSoundLibrary.LoadSounds();
begin
  UnloadSounds();

  Start   := AudioPlayback.OpenSound(SoundPath.Append('Common start.mp3'));
  Back    := AudioPlayback.OpenSound(SoundPath.Append('Common back.mp3'));
  Swoosh  := AudioPlayback.OpenSound(SoundPath.Append('menu swoosh.mp3'));
  Change  := AudioPlayback.OpenSound(SoundPath.Append('select music change music 50.mp3'));
  Option  := AudioPlayback.OpenSound(SoundPath.Append('option change col.mp3'));
  Click   := AudioPlayback.OpenSound(SoundPath.Append('rimshot022b.mp3'));
  Ping    := AudioPlayback.OpenSound(SoundPath.Append('ping.wav'));
  Applause:= AudioPlayback.OpenSound(SoundPath.Append('Applause.mp3'));

  BGMusic := AudioPlayback.OpenSound(SoundPath.Append('background track.mp3'));

  if (BGMusic <> nil) then
    BGMusic.Loop := True;
end;

procedure TSoundLibrary.UnloadSounds();
begin
  FreeAndNil(Start);
  FreeAndNil(Back);
  FreeAndNil(Swoosh);
  FreeAndNil(Change);
  FreeAndNil(Option);
  FreeAndNil(Click);
  FreeAndNil(Ping);
  FreeAndNil(Applause);
  FreeAndNil(BGMusic);
end;

(* TODO
function TSoundLibrary.GetSound(ID: integer): TAudioPlaybackStream;
begin
  if ((ID >= 0) and (ID < Length(Sounds))) then
    Result := Sounds[ID]
  else
    Result := nil;
end;
*)

procedure TSoundLibrary.StartBgMusic();
begin
  if (TBackgroundMusicOption(Ini.BackgroundMusicOption) = bmoOn) and
    (Soundlib.BGMusic <> nil) and not (Soundlib.BGMusic.Status = ssPlaying) then
  begin
    SoundLib.BGMusic.SetVolume(IPreviewVolumeVals[Ini.PreviewVolume]);
    AudioPlayback.PlaySound(Soundlib.BGMusic);
  end;
end;

procedure TSoundLibrary.PauseBgMusic();
begin
  If (Soundlib.BGMusic <> nil) then
  begin
    Soundlib.BGMusic.Pause;
  end;
end;

{ TAudioConverter }

function TAudioConverter.Init(SrcFormatInfo: TAudioFormatInfo; DstFormatInfo: TAudioFormatInfo): boolean;
begin
  fSrcFormatInfo := SrcFormatInfo.Copy();
  fDstFormatInfo := DstFormatInfo.Copy();
  Result := true;
end;

destructor TAudioConverter.Destroy();
begin
  FreeAndNil(fSrcFormatInfo);
  FreeAndNil(fDstFormatInfo);
end;


{ TAudioProcessingStream }

procedure TAudioProcessingStream.AddOnCloseHandler(Handler: TOnCloseHandler);
begin
  if (@Handler <> nil) then
  begin
    SetLength(OnCloseHandlers, System.Length(OnCloseHandlers)+1);
    OnCloseHandlers[High(OnCloseHandlers)] := @Handler;
  end;
end;

procedure TAudioProcessingStream.PerformOnClose();
var i: integer;
begin
  for i := 0 to High(OnCloseHandlers) do
  begin
    OnCloseHandlers[i](Self);
  end;
end;

{ TAudioSourceStream }

constructor TAudioSourceStream.Create();
begin
  RG := 1.0;
end;

procedure TAudioSourceStream.SetReplayGain(const GainTag: AnsiString; const PeakTag: AnsiString);
var
  GainStr: AnsiString;
  GainValDb: single;
  PeakVal: single;
  PeakValDb: single;
  GainTagTokens: TStringDynArray;
begin
    GainTagTokens := SplitString(GainTag, 0); //Split string by space to remove 'dB'
    if (System.Length(GainTagTokens) > 0) then
    begin
      try
        GainStr := GainTagTokens[0];

        // Some ReplayGain scanners prefix positive gains with a + character,
        // even though this violates the ReplayGain standard. Let's handle this
        // case anyways
        if (GainStr.StartsWith('+')) then
          GainStr := GainStr.Substring(1);
        GainValDb := StrToFloat(GainStr);

        // If the file has a replaygain peak tag, use it for clipping protection
        if (not PeakTag.IsEmpty()) then
        begin
          PeakVal := StrToFloat(PeakTag);
          if (PeakVal > 0.0) then
          begin
            PeakValDb := 20.0 * log10(PeakVal);
            GainValDb := Min(GainValDb, 0.0 - PeakValDb);
          end;
        end;

        RG := power(10.0, (GainValDb / 20.0));
        Log.LogInfo('Using ReplayGain adjustment: ' + FormatFloat('0.00', GainValDb)
                    + ' dB (' + FormatFloat('0.000', RG) + ')', 'TAudioSourceStream.SetReplayGain');
      except
        Log.LogError('Failed to parse ReplayGain tag');
      end;
    end;
end;


(*  Opus files support another loudness normalization scheme that is completely
 *  separate from ReplayGain. Values are stored in the 'R128_TRACK_GAIN' tag,
 *  encoded as a Q7.8 fixed-point number, and there is no peak information.
 *  To obtain the gain in decibels, divide by 256. See RFC 7845 for more information:
 *
 *  https://datatracker.ietf.org/doc/html/rfc7845
 *)
procedure TAudioSourceStream.SetReplayGainR128(const R128Tag: AnsiString);
var
  GainValDb: single;
begin
  try

    // The R128_TRACK_GAIN tags are referenced to -23 LUFS, but ReplayGain tags are
    // referenced to -18 LUFS. This means that Opus files will play back 5 dB quieter
    // than non-Opus files we apply the gain directly. So we add a 5 dB preamp value to
    // the R128 tag to ensure that Opus files will play back at the same volume as non-Opus files
    GainValDb := (StrToFloat(R128Tag) / 256.0) + 5.0;
    RG := power(10.0, (GainValDb / 20.0));
    Log.LogInfo('Using ReplayGain adjustment: ' + FormatFloat('0.00', GainValDb)
                + ' dB (' + FormatFloat('0.000', RG) + ')', 'TAudioSourceStream.SetReplayGainR128');
  except
    Log.LogError('Failed to parse R128 tag');
  end;
end;

function TAudioSourceStream.GetReplayGain(): single;
begin
  Result := RG;
end;

{ TAudioPlaybackStream }

constructor TAudioPlaybackStream.Create();
begin
  RG := 1.0;
  RGEnabled := false;
  inherited;
end;

function TAudioPlaybackStream.Open(SourceStream: TAudioSourceStream): boolean;
begin
  if (Ini.ReplayGain = 1) then
    RG := SourceStream.GetReplayGain();
  Result := true;
end;

function TAudioPlaybackStream.GetSourceStream(): TAudioSourceStream;
begin
  Result := SourceStream;
end;

procedure TAudioPlaybackStream.SetSyncSource(SyncSource: TSyncSource);
begin
  Self.SyncSource := SyncSource;
  AvgSyncDiff := -1;
end;

{.$DEFINE LOG_SYNC}

(*
 * Results an adjusted size of the input buffer size to keep the stream in sync
 * with the SyncSource. If no SyncSource was assigned to this stream, the
 * input buffer size will be returned, so this method will have no effect.
 *
 * These are the possible cases:
 *   - Result > BufferSize: stream is behind the sync-source (stream is too slow),
 *                          (Result-BufferSize) bytes of the buffer must be skipped.
 *   - Result = BufferSize: stream is in sync,
 *                          there is nothing to do.
 *   - Result < BufferSize: stream is ahead of the sync-source (stream is too fast),
 *                          (BufferSize-Result) bytes of the buffer must be padded.
 *)
function TAudioPlaybackStream.Synchronize(BufferSize: integer; FormatInfo: TAudioFormatInfo): integer;
var
  TimeDiff: double;
  FrameDiff: double;
  FrameSkip: integer;
  ReqFrames: integer;
  MasterClock: real;
  CurPosition: real;
const
  AVG_HISTORY_FACTOR = 0.7;
  SYNC_REPOS_THRESHOLD = 5.000;
  SYNC_SOFT_THRESHOLD  = 0.010;
begin
  Result := BufferSize;

  if (not assigned(SyncSource)) then
    Exit;

  if (BufferSize <= 0) then
    Exit;

  CurPosition := Position;
  MasterClock := SyncSource.GetClock();

  // difference between sync-source and stream position
  // (negative if the music-stream's position is ahead of the master clock)
  TimeDiff := MasterClock - CurPosition;

  // calculate average time difference (some sort of weighted mean).
  // The bigger AVG_HISTORY_FACTOR is, the smoother is the average diff.
  // This means that older diffs are weighted more with a higher history factor
  // than with a lower. Do not use a too low history factor. FFmpeg produces
  // very instable timestamps (pts) for ogg due to some bugs. They may differ
  // +-50ms from the real stream position. Without filtering those glitches we
  // would synch without any need, resulting in ugly plopping sounds.
  if (AvgSyncDiff = -1) then
    AvgSyncDiff := TimeDiff
  else
    AvgSyncDiff := TimeDiff * (1-AVG_HISTORY_FACTOR) +
                   AvgSyncDiff * AVG_HISTORY_FACTOR;

  {$IFDEF LOG_SYNC}
  //Log.LogError(Format('c:%.3f | p:%.3f | d:%.3f | a:%.3f',
  //    [MasterClock, CurPosition, TimeDiff, AvgSyncDiff]), 'Synch');
  {$ENDIF}

  // check if we are out of sync
  if (Abs(AvgSyncDiff) >= SYNC_REPOS_THRESHOLD) then
  begin
    {$IFDEF LOG_SYNC}
    Log.LogError(Format('ReposSynch: %.3f > %.3f',
        [Abs(AvgSyncDiff), SYNC_REPOS_THRESHOLD]), 'Synch');
    {$ENDIF}

    // diff far is too large -> reposition stream
    // (resulting position might still be out of sync)
    SetPosition(CurPosition + AvgSyncDiff);

    // reset sync info
    AvgSyncDiff := -1;
  end
  else if (Abs(AvgSyncDiff) >= SYNC_SOFT_THRESHOLD) then
  begin
    {$IFDEF LOG_SYNC}
    Log.LogError(Format('SoftSynch: %.3f > %.3f',
        [Abs(AvgSyncDiff), SYNC_SOFT_THRESHOLD]), 'Synch');
    {$ENDIF}

    // hard sync: directly jump to the current position
    FrameSkip := Round(AvgSyncDiff * FormatInfo.SampleRate);
    Result := BufferSize + FrameSkip * FormatInfo.FrameSize;
    if (Result < 0) then
      Result := 0;

    // reset sync info
    AvgSyncDiff := -1;
  end;
end;

(*
 * Fills a buffer with copies of the given Frame or with 0 if Frame is nil.
 *)
procedure TAudioPlaybackStream.FillBufferWithFrame(Buffer: PByteArray; BufferSize: integer; Frame: PByteArray; FrameSize: integer);
var
  i: integer;
  FrameCopyCount: integer;
begin
  // the buffer must at least contain place for one copy of the frame.
  if ((Buffer = nil) or (BufferSize <= 0) or (BufferSize < FrameSize)) then
    Exit;

  // no valid frame -> fill with 0
  if ((Frame = nil) or (FrameSize <= 0)) then
  begin
    FillChar(Buffer[0], BufferSize, 0);
    Exit;
  end;

  // number of frames to copy
  FrameCopyCount := BufferSize div FrameSize;
  // insert as many copies of frame into the buffer as possible
  for i := 0 to FrameCopyCount-1 do
    Move(Frame[0], Buffer[i*FrameSize], FrameSize);
end;

function TAudioPlaybackStream.GetReplayGainAdjustment(): single;
begin
  if (RGEnabled) then
    Result := RG
  else
    Result := 1.0;
end;

function TAudioPlaybackStream.GetReplayGainEnabled(): boolean;
begin
  Result := RGEnabled;
end;

procedure TAudioPlaybackStream.SetReplayGainEnabled(RGEnabled: boolean);
begin
  self.RGEnabled := RGEnabled;
end;

{ TAudioVoiceStream }

constructor TAudioVoiceStream.Create();
begin
  inherited;
end;

function TAudioVoiceStream.Open(ChannelMap: integer; FormatInfo: TAudioFormatInfo): boolean;
begin
  Self.ChannelMap := ChannelMap;
  Self.FormatInfo := FormatInfo.Copy();
  // a voice stream is always mono, reassure the the format is correct
  Self.FormatInfo.Channels := 1;
  Result := true;
end;

destructor TAudioVoiceStream.Destroy;
begin
  Close();
  inherited;
end;

procedure TAudioVoiceStream.Close();
begin
  PerformOnClose();
  FreeAndNil(FormatInfo);
end;

function TAudioVoiceStream.GetAudioFormatInfo(): TAudioFormatInfo;
begin
  Result := FormatInfo;
end;

function TAudioVoiceStream.GetLength(): real;
begin
  Result := -1;
end;

function TAudioVoiceStream.GetPosition(): real;
begin
  Result := -1;
end;

procedure TAudioVoiceStream.SetPosition(Time: real);
begin
end;

function TAudioVoiceStream.GetLoop(): boolean;
begin
  Result := false;
end;

procedure TAudioVoiceStream.SetLoop(Enabled: boolean);
begin
end;

{ TLineFragment }

function TLineFragment.GetEnd(): integer;
begin
  Result := StartBeat + Duration;
end;

{ TLine }

function TLine.HasLength(): boolean;
var tempi: integer;
begin
  Result := HasLength(tempi);
end;

function TLine.HasLength(out Len: integer): boolean;
begin
  Result := false;
  if Length(Notes) >= 0 then
  begin
    Len := EndBeat - Notes[0].StartBeat;
    Result := (Len > 0);
  end;
end;

function TLine.HasLength(out Len: real): boolean;
var tempi: integer;
begin
  Result := HasLength(tempi);
  Len := tempi;
end;

function TLine.HasLength(out Len: double): boolean;
var tempi: integer;
begin
  Result := HasLength(tempi);
  Len := tempi;
end;

function TLine.GetLength(): integer;
begin
  Result := ifthen(Length(Notes) < 0, 0, EndBeat - Notes[0].StartBeat);
end;

function FindNote(beat: integer): TPos;
var
  LineIndex: integer;
  NoteIndex: integer;
  found:     boolean;
  min:       integer;
  diff:      integer;

begin
  found := false;

  for LineIndex := 0 to High(Tracks[0].Lines) do
  begin
    for NoteIndex := 0 to High(Tracks[0].Lines[LineIndex].Notes) do
    begin
      if (beat >= Tracks[0].Lines[LineIndex].Notes[NoteIndex].StartBeat) and
         (beat <= Tracks[0].Lines[LineIndex].Notes[NoteIndex].StartBeat + Tracks[0].Lines[LineIndex].Notes[NoteIndex].Duration) then
      begin
        Result.track := 0;
        Result.line := LineIndex;
        Result.note := NoteIndex;
        found := true;
        break;
      end;
    end;
  end;

  if found then //found exactly
    exit;

  if CurrentSong.isDuet and (PlayersPlay <> 1) then
  begin
    for LineIndex := 0 to High(Tracks[1].Lines) do
    begin
      for NoteIndex := 0 to High(Tracks[1].Lines[LineIndex].Notes) do
      begin
        if (beat >= Tracks[1].Lines[LineIndex].Notes[NoteIndex].StartBeat) and
          (beat <= Tracks[1].Lines[LineIndex].Notes[NoteIndex].StartBeat + Tracks[1].Lines[LineIndex].Notes[NoteIndex].Duration) then
        begin
          Result.track := 1;
          Result.line := LineIndex;
          Result.note := NoteIndex;
          found := true;
          break;
        end;
      end;
    end;
  end;

  if found then //found exactly
    exit;

  min := high(integer);
  //second try (approximating)
  for LineIndex := 0 to High(Tracks[0].Lines) do
  begin
    for NoteIndex := 0 to High(Tracks[0].Lines[LineIndex].Notes) do
    begin
      diff := abs(Tracks[0].Lines[LineIndex].Notes[NoteIndex].StartBeat - beat);
      if diff < min then
      begin
        Result.track := 0;
        Result.line := LineIndex;
        Result.note := NoteIndex;
        min := diff;
      end;
    end;
  end;

  if CurrentSong.isDuet and (PlayersPlay <> 1) then
  begin
    for LineIndex := 0 to High(Tracks[1].Lines) do
    begin
      for NoteIndex := 0 to High(Tracks[1].Lines[LineIndex].Notes) do
      begin
        diff := abs(Tracks[1].Lines[LineIndex].Notes[NoteIndex].StartBeat - beat);
        if diff < min then
        begin
          Result.track := 1;
          Result.line := LineIndex;
          Result.note := NoteIndex;
          min := diff;
        end;
      end;
    end;
  end;
end;

end.
