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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenEditSub.pas $
 * $Id: UScreenEditSub.pas 3103 2014-11-22 23:21:19Z k-m_schindler $
 *}

unit UScreenEditSub;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$I switches.inc}

uses
  Classes,
  UCommon,
  UEditorLyrics,
  UFiles,
  UFilesystem,
  UGraphicClasses,
  UIni,
  UMain,
  UMenu,
  UMenuText,
  UMusic,
  UPath,
  URecord,
  USong,
  USongs,
  UTexture,
  UThemes,
  UTime,
  dglOpenGL,
  Math,
  {$IFDEF UseMIDIPort}
    MidiOut,
    MidiCons,
    {$IFDEF MSWINDOWS}
    UMidiInput,
    {$ENDIF}
  {$ENDIF}
  sdl2,
  strutils,
  SysUtils;

type

  TMedleyNotes = record
    start:        TPos;
    end_:         TPos;
    Preview:      TPos;
    isStart:      boolean;   //start beat is declared
    isEnd:        boolean;   //end beat is declared
    isCustom:     boolean;
  end;

  TVisibleHeaders = record
    Title:        UTF8String;
    Artist:       UTF8String;
    Language:     UTF8String;
    Edition:      UTF8String;
    Genre:        UTF8String;
    Year:         Integer;
    Creator:      UTF8String;
    Mp3:          IPath;
    Mp3Id:        Integer;
    Cover:        IPath;
    CoverId:      Integer;
    Background:   IPath;
    BackgroundId: Integer;
    Video:        IPath;
    VideoId:      Integer;
    VideoGAP:     Real;
    BPM:          array of TBPM;
    GAP:          Real;
    StartTag:     Real;
    EndTag:       longint;
    MedleyNotes:  TMedleyNotes;
    PreviewStart: Real;
    Relative:     boolean;
  end;

  TScreenEditSub = class(TMenu)
    private

      CurrentBeat:             Integer;
      //Variable is True if no Song is loaded
      Error:                   boolean;

      TextNote:                Integer;
      TextSentence:            Integer;
      TextInfo:                Integer;

      CurrentNote:             array[0..1] of Integer;
      PlaySentence:            boolean;
      PlayOne:                 boolean;
      PlayOneMidi:             boolean;
      PlaySentenceMidi:        boolean;
      midinotefound:           boolean; // if one note was found
      PlayVideo:               boolean;
      PlayStopTime:            real;
      LastClick:               Integer;
      LastClickValid:          boolean;
      LastMidiBeat:            Integer;
      LastMidiBeatValid:       boolean;
      Click:                   boolean;
      CopySrc:                 TPos;
      CopySrcValid:            Boolean;
      {$IFDEF UseMIDIPort}
      MidiOut:                 TMidiOutput;
      MidiStart:               real;
      MidiStop:                real;
      MidiAnchorPos:           real;
      MidiPos:                 real;
      MidiLastNote:            Integer;
      MidiActive:              boolean;
      MidiTone:                Integer;
      MidiStopBeat:            Integer;
      {$ENDIF}

      //for mouse move
      LastPressedMouseButton:  boolean;
      LastPressedMouseType:    Integer;
      PressedNoteId:           Integer;

      RepeatCounter:           Integer;

      TextPosition:            Integer;
      TextEditMode:            boolean;
      TitleEditMode:           boolean;
      ArtistEditMode:          boolean;
      LanguageEditMode:        boolean;
      EditionEditMode:         boolean;
      GenreEditMode:           boolean;
      YearEditMode:            boolean;
      CreatorEditMode:         boolean;
      P1EditMode:              boolean;
      P2EditMode:              boolean;
      BPMEditMode:             boolean;
      PianoEditMode:           boolean;

      PianoKeysLow: TPianoKeyArray;
      PianoKeysHigh: TPianoKeyArray;

      // to interactive divide note
      LastClickTime:           Integer;

      BackupEditText:          UTF8String; //backup of current text in text-edit-mode
      CurrentEditText:         UTF8String; // current edit text
      editLengthText:          Integer;
      CurrentSlideId:          Integer;
      //title header
      TitleSlideId:            Integer;
      TitleData:               Integer;
      TitleVal:                array of UTF8String;
      SlideTitleIndex:         Integer;
      // artist header
      ArtistSlideId:           Integer;
      ArtistData:              Integer;
      ArtistVal:               array of UTF8String;
      SlideArtistIndex:        Integer;
      // language header
      LanguageSlideId:         Integer;
      LanguageData:            Integer;
      LanguageVal:             array of UTF8String;
      SlideLanguageIndex:      Integer;
      // edition header
      EditionSlideId:          Integer;
      EditionData:             Integer;
      EditionVal:              array of UTF8String;
      SlideEditionIndex:       Integer;
      // genre header
      GenreSlideId:            Integer;
      GenreData:               Integer;
      GenreVal:                array of UTF8String;
      SlideGenreIndex:         Integer;
      // year header
      YearSlideId:             Integer;
      YearData:                Integer;
      YearVal:                 array of UTF8String;
      SlideYearIndex:          Integer;
      // creator header
      CreatorSlideId:          Integer;
      CreatorData:             Integer;
      CreatorVal:              array of UTF8String;
      SlideCreatorIndex:       Integer;
      // mp3 header
      MP3SlideId:              Integer;
      MP3Data:                 Integer;
      MP3Val:                  array of UTF8String;
      SlideMP3Index:           Integer;
      // Cover header
      CoverSlideId:            Integer;
      CoverData:               Integer;
      CoverVal:                array of UTF8String;
      SlideCoverIndex:         Integer;
      // Background header
      BackgroundSlideId:       Integer;
      BackgroundData:          Integer;
      BackgroundVal:           array of UTF8String;
      SlideBackgroundIndex:    Integer;
      // Video header
      VideoSlideId:            Integer;
      VideoData:               Integer;
      VideoVal:                array of UTF8String;
      SlideVideoIndex:         Integer;
      // VideoGap header
      VideoGapSlideId:         Integer;
      VideoGapData:            Integer;
      VideoGapVal:             array of UTF8String;
      SlideVideoGapIndex:      Integer;
      // BPM header
      BPMSlideId:              Integer;
      BPMData:                 Integer;
      BPMVal:                  array of UTF8String;
      SlideBPMIndex:           Integer;
      // GAP header
      GAPSlideId:              Integer;
      GAPData:                 Integer;
      GAPVal:                  array of UTF8String;
      SlideGAPIndex:           Integer;
      // StartTag header
      StartTagSlideId:         Integer;
      StartTagData:            Integer;
      StartTagVal:             array of UTF8String;
      SlideStartTagIndex:      Integer;
      // EndTag header
      EndTagSlideId:           Integer;
      EndTagData:              Integer;
      EndTagVal:               array of UTF8String;
      SlideEndTagIndex:        Integer;
      // MedleyStart header
      MedleyStartSlideId:      Integer;
      MedleyStartData:         Integer;
      MedleyStartVal:          array of UTF8String;
      SlideMedleyStartIndex:   Integer;
      // MedleyEnd header
      MedleyEndSlideId:        Integer;
      MedleyEndData:           Integer;
      MedleyEndVal:            array of UTF8String;
      SlideMedleyEndIndex:     Integer;
      // PreviewStart header
      PreviewStartSlideId:     Integer;
      PreviewStartData:        Integer;
      PreviewStartVal:         array of UTF8String;
      SlidePreviewStartIndex:  Integer;
      // Relative header
      RelativeSlideId:         Integer;
      RelativeData:            Integer;
      RelativeVal:             array of UTF8String;
      SlideRelativeIndex:      Integer;
      // Start header
      StartSlideId:            Integer;
      StartData:               Integer;
      StartVal:                array of UTF8String;
      SlideStartIndex:         Integer;
      // Duration header
      DurationSlideId:         Integer;
      DurationData:            Integer;
      DurationVal:             array of UTF8String;
      SlideDurationIndex:      Integer;
      // Tone header
      ToneSlideId:             Integer;
      ToneData:                Integer;
      ToneVal:                 array of UTF8String;
      SlideToneIndex:          Integer;
      // Text header
      LyricSlideId:            Integer;
      LyricData:               Integer;
      LyricVal:                array of UTF8String;
      SlideLyricIndex:         Integer;
      // Volume Slide
      VolumeAudioSlideId:      Integer;
      VolumeMidiSlideId:       Integer;
      VolumeClickSlideId:      Integer;
      PrerollAudioSlideId:     Integer;
      PrerollMidiSlideId:      Integer;
      VolumeAudioIndex:        Integer;
      VolumeMidiIndex:         Integer;
      VolumeClickIndex:        Integer; //for update slide
      VolumeDragSlideId:       Integer;
      PrerollAudioIndex:       Integer;
      PrerollMidiIndex:        Integer;

      VolumeAudio:             array of UTF8String;
      VolumeMidi:              array of UTF8String;
      VolumeClick:             array of UTF8String;
      PrerollAudio:            array of UTF8String;
      PrerollMidi:             array of UTF8String;
      // control buttons
      UndoButtonId:            Integer;
      PreviousSeqButtonID:     Integer;
      NextSeqButtonID:         Integer;
      FreestyleButtonID:       Integer;
      GoldButtonID:            Integer;
      PlayOnlyButtonID:        Integer;
      PlayWithNoteButtonID:    Integer;
      PlayNoteButtonID:        Integer;
      // background image & video preview
      BackgroundImageId:       Integer;
      //NotesBackgroundId:       Integer;
      // player static picture
      playerIconId:            array[1..2] of Integer;
      //  currentX, CurrentY
      CurrentX:                Integer;
      CurrentY:                Integer;
      LastX:                   Integer;
      LastY:                   Integer;
      Xmouse:                  Integer;
      ResizeNoteLeft:          boolean;
      ResizeNoteRight:         boolean;
      MoveNote:                boolean;

      CurrentTrack:            Integer; // 0 or 1 (for duets)
      EditorLyrics:            array[0..1] of TEditorLyrics;

      //undo declaration
      UndoLines:               array of array of TLines;
      UndoStateNote:           array of array of Integer; //UNDO: note's position
      CurrentUndoLines:        Integer;
      UndoHeader:              array of TVisibleHeaders;

      //video view
      fCurrentVideo: IVideo;

      //singtrack
      CurrentSound:            TCaptureBuffer;
      // Interactive note
      InteractiveNoteId:       array of Integer;
      TransparentNoteButtonId: array of Integer;
      // Interactive Line bar
      InteractiveLineId:       array of Integer;
      TransparentLineButtonId: array of Integer;

      {$IFDEF UseMIDIPort}
      PlayMidi:                boolean;
      {$ENDIF}// Midi
      BackupVolume:            single;
      BackupVolumeSet:         boolean;

      // medley
      MedleyNotes:             TMedleyNotes;
      PendingSaveRelative:     boolean;
      TimingErrorTrack:        Integer;
      TimingErrorLine:         Integer;
      TimingErrorBeat:         Integer;
      TimingErrorValid:        boolean;
      FVocalsPitchContour:     array of Single;
      FVocalsPitchStepSec:     Single;
      FVocalsPitchStartSec:    Single;
      FPreviewScoreTrack:      Integer;
      FPreviewLineNormalScoreByTrack: array[0..1] of array of Double;
      FPreviewLineGoldenScoreByTrack: array[0..1] of array of Double;
      FPreviewLineBonusScoreByTrack:  array[0..1] of array of Double;
      FPreviewLineScoreReadyByTrack:  array[0..1] of array of Boolean;
      FPreviewLineNormalScore: array of Double;
      FPreviewLineGoldenScore: array of Double;
      FPreviewLineBonusScore:  array of Double;
      FPreviewLineScoreReady:  array of Boolean;
      FVocalsPreviewSamples:   array of SmallInt;
      FVocalsPreviewReadySamples: Int64;
      FVocalsPreviewSourceStream: TAudioSourceStream;
      FVocalsPreviewPlaybackStream: TAudioPlaybackStream;
      FPreviewBuildComplete:   Boolean;
      FPreviewBuildStarted:    Boolean;
      FPreviewRescorePending:  Boolean;
      FUsingVocalsPreviewPlayback: Boolean;
      FAnalysisThread:         TThread;
      FAnalysisThreadRunning:  Boolean;
      FAnalysisThreadBuildPreview: Boolean;
      FAnalysisThreadTrack:    Integer;
      FAnalysisCancelRequested: Boolean;
      FPreserveAnalysisOnShow: Boolean;
      FAnalysisProgressPct:    Integer;
      FAnalysisFailed:         Boolean;
      FAnalysisError:          UTF8String;
      FAnalysisDecodeLock:     TRTLCriticalSection;
      FAnalysisDataLock:       TRTLCriticalSection;

      procedure ChangeBPM(newBPM: real);
      procedure ResetBeatTracking;
      procedure ClearVocalsWaveformCache(const ClearPreview: Boolean = true);
      procedure InvalidateVocalsAnalysis(const KeepPreview: Boolean = true);
      function  OpenDecodeStream(const Filename: IPath): TAudioDecodeStream;
      procedure RunIncrementalVocalsAnalysis(const BuildPreview: Boolean; const Track: Integer);
      procedure EnsureCurrentLineWaveformCache;
      procedure EnsurePreviewScoreCache(const Track: Integer);
      procedure RefreshPreviewScoreMetadata(const Track: Integer);
      procedure RecomputeAllPreviewScores;
      procedure TriggerGlobalPreviewRescore;
      function  GetEditorScoreDifficulty: Integer;
      function  HasVocalsPreviewDirectSource: Boolean;
      function  GetVocalsPreviewSourcePath: IPath;
      function  HasVocalsPreviewSource: Boolean;
      function  HasVocalsPreviewPlayback: Boolean;
      function  HasVocalsPreviewPlaybackAtTime(const Time: Real): Boolean;
      function  HasCopySource: Boolean;
      procedure StartAnalysisThread(const BuildPreview: Boolean; const Track: Integer);
      procedure StopAnalysisThread;
      function  EnsureVocalsPreviewPlaybackStream: Boolean;
      function  GetVocalsPreviewByteSize: Int64;
      function  ReadVocalsPreviewData(const PositionBytes: Int64; Buffer: PByte; const BufferSize: Integer): Integer;
      procedure SetVocalsPreviewPlayback(const Enable: Boolean; const KeepPosition: Boolean = false);
      procedure RestoreStandardPlayback(const KeepPosition: Boolean = true);
      function  GetPlaybackLength: real;
      function  GetPlaybackPosition: real;
      procedure SetPlaybackPosition(const Time: real);
      procedure StopPlayback;
      procedure PlayPlayback;
      procedure SetPlaybackVolume(const Volume: single);
      procedure DrawVocalsWaveform(const X, Y, W, H: real; const Track: Integer);
      procedure ConfigureMidiPlayback(const StartTime, StopTime: real);
      {$IFDEF UseMIDIPort}
      procedure PlayMidiTone(const Tone: Integer);
      procedure StopMidi;
      function GetMidiVolume(): Integer;
      {$ENDIF}
      procedure HandleSaveSong(SDL_ModState: word);
      procedure SetPreviewStart(SDL_ModState: word);
      procedure SetMedleyTags(SDL_ModState: word);
      procedure Jump(SDL_ModState: word);
      procedure ReloadSong(SDL_ModState: word);
      procedure HandleDivideBPM(SDL_ModState: word);
      procedure HandleMultiplyBPM(SDL_ModState: word);
      procedure HandleLinePlayback(SDL_ModState: word; const WithVideo: Boolean;
        const AllowVocalsPreview: Boolean);
      procedure HandleVideo(SDL_ModState: word);
      procedure HandlePaste(SDL_ModState: word);
      procedure HandleFixTimings(SDL_ModState: word);
      procedure HandleBPMIncrease(SDL_ModState: word);
      procedure HandleBPMDecrease(SDL_ModState: word);
      procedure HandleExtendedCopyPaste(SDL_ModState: word; PressedKey: Integer);
      procedure HandleMoveTextRight(SDL_ModState: word);
      procedure HandleMoveRight(SDL_ModState: word);
      procedure HandleMoveLeft(SDL_ModState: word);
      procedure HandleDownKey(SDL_ModState: word);
      procedure HandleUpKey(SDL_ModState: word);
      procedure EnterTextEditMode(SDL_ModState: word);
      procedure EnterBPMEditMode(SDL_ModState: word);
      procedure EnterPianoEditMode(SDL_ModState: word);
      procedure ToggleTextEditMode(SDL_ModState: word);
      procedure HandlePlaySentence(SDL_ModState: word);
      procedure PlayNote(SDL_ModState: word);
      procedure IncreaseNoteLength(SDL_ModState: word);
      procedure CapitalizeLyrics(SDL_ModState: word);
      procedure Undo(SDL_ModState: word);
      procedure LeaveScope(SDL_ModState: word);
      procedure IncreaseAllNoteTones(SDL_ModState: word);
      procedure DecreaseAllNoteTones(SDL_ModState: word);
      procedure DivideOrJoinNotes(SDL_ModState: word);
      procedure DeleteNotes(SDL_ModState: word);
      procedure IncreaseGAP(SDL_ModState: word);
      procedure DecreaseGAP(SDL_ModState: word);
      procedure DecreaseVideoGap(SDL_ModState: word);
      procedure IncreaseVideoGap(SDL_ModState: word);
      procedure SetFreestyleNote(SDL_ModState: word);
      procedure SetGoldenNote(SDL_ModState: word);
      procedure ShowPopupHelp(SDL_ModState: word);
      procedure ToggleDuet(SDL_ModState: word);
      procedure DivideBPM;
      procedure MultiplyBPM;
      procedure LyricsCapitalize;
      procedure LyricsCorrectSpaces;
      procedure FixTimings;
      procedure DivideSentence;
      procedure JoinSentence;
      procedure NextSentence;
      procedure PreviousSentence;
      procedure DivideNote(doubleclick: boolean);
      procedure DeleteNote;
      procedure OnMidiNote(Note: Byte);
      procedure DeleteSentence;
      procedure TransposeNote(Transpose: Integer);
      procedure UpdateLineBaseNote(const TrackIndex, LineIndex: Integer);
      procedure ChangeWholeTone(Tone: Integer);
      procedure MoveAllToEnd(Move: Integer);
      procedure MoveTextToRight;
      procedure MarkCopySrc;
      procedure CopySentence(SrcTrack, SrcLine, DstTrack, DstLine: Integer; CopyText: boolean = true; CopyNotes: boolean = true; EnforceSrcLength: boolean = false);
      procedure CopySentences(SrcTrack, SrcLine, DstTrack, DstLine, Num: Integer);
      procedure MakeSolo;
      procedure MakeDuet;
      function  DuetCopyLine: boolean;
      function  DuetMoveLine: boolean;
      procedure CopyLine(SrcTrack, SrcLine, DstTrack, DstLine: Integer);
      function  CheckTimingSyntaxErrors(out ErrorMessages: UTF8String): boolean;
      procedure GoToLineAndBeat(const TrackIndex, LineIndex, Beat: Integer);
      procedure GoToFirstTimingError;
      function  SaveSongToFile(const SaveRelative: boolean): boolean;
      procedure Refresh;
      procedure CopyToUndo; //copy current lines, mouse position and headers
      procedure CopyFromUndo; //undo last lines, mouse position and headers
      procedure DrawPlayerTrack(CurrentTone: Integer; Count: Integer; CurrentNote: Integer);
      procedure DrawInfoBar(X, Y, W, H: Integer; ColR, ColG, ColB, Alpha: real; Track: Integer);
      procedure DrawMaxScoreInfo(X, Y, W, H: Integer; Track: Integer);
      procedure DrawText(X, Y, W, H: real; Track: Integer; NumLines: Integer = 10);
      //video view
      procedure StartVideoPreview();
      procedure StopVideoPreview();
      procedure UpdateVideoPosition(NewPosition: real);
      //Note Name Mod
      function GetNoteName(Note: Integer): string;
      // show transparent background note for interactions
      procedure ShowInteractiveBackground;
      function  GetMedleyLength: real; //if available returns the length of the medley in seconds, otherwise 0
      procedure SyncVolumeSlidersFromIni;
      function  GetSentenceLeadSeconds(const UseMidi: boolean): real;

    public
      Tex_PrevBackground:      TTexture;
      FadeOut:                 boolean;

      constructor Create; override;
      destructor Destroy; override;
      procedure OnShow; override;
      function  ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean; override;
      function  ParseInputEditText(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean;
      function  ParseInputEditBPM(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean;
      function  ParseInputEditPiano(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean;
      procedure ApplyTone(NewTone: Integer);
      function  ParseMouse(MouseButton: Integer; BtnDown: boolean; X, Y: Integer): boolean; override;
      function  Draw: boolean; override;
      procedure OnHide; override;
  end;

const
  ID='ID_064';   //for help system

implementation

uses
  UDisplay,
  UAudioConverter,
  UDraw,
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UMenuInteract,
  UNote,
  USkins,
  UTextEncoding,
  UUnicodeUtils,
  TextGL;

type
  TEditSubAnalysisThread = class(TThread)
  private
    FOwner: TScreenEditSub;
    FBuildPreview: Boolean;
    FTrack: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TScreenEditSub; const BuildPreview: Boolean; const Track: Integer);
  end;

  TEditSubPreviewSourceStream = class(TAudioSourceStream)
  private
    FOwner: TScreenEditSub;
    FFormat: TAudioFormatInfo;
    FLoop: Boolean;
    FPositionBytes: Int64;
  protected
    function IsEOF(): boolean; override;
    function IsError(): boolean; override;
    function GetLength(): real; override;
    function GetPosition(): real; override;
    procedure SetPosition(Time: real); override;
    function GetLoop(): boolean; override;
    procedure SetLoop(Enabled: boolean); override;
  public
    constructor Create(AOwner: TScreenEditSub);
    destructor Destroy; override;
    function ReadData(Buffer: PByte; BufferSize: integer): integer; override;
    function GetAudioFormatInfo(): TAudioFormatInfo; override;
    procedure Close(); override;
  end;

const
  DEFAULT_FADE_IN_TIME  = 8;    //TODO in INI
  DEFAULT_FADE_OUT_TIME = 2;
  MIN_EDITOR_BPM = 1.0;
  NOT_SET = '-';
  // Line-local waveform rendering can afford a denser cache.
  VOCALS_WAVEFORM_SAMPLE_RATE = 44100;
  VOCALS_WAVEFORM_ENVELOPE_HZ = 44100;
  VOCALS_WAVEFORM_DECODE_BYTES = 32768;
  VOCALS_PITCH_ANALYSIS_WINDOW = 4096;
  VOCALS_PITCH_ANALYSIS_HOP = 256;
  VOCALS_PITCH_INVALID = -1000.0;
  NotesSkipX:  Integer = 20;
  LineSpacing: Integer = 15;

constructor TEditSubAnalysisThread.Create(AOwner: TScreenEditSub; const BuildPreview: Boolean; const Track: Integer);
begin
  inherited Create(false);
  FreeOnTerminate := false;
  FOwner := AOwner;
  FBuildPreview := BuildPreview;
  FTrack := Track;
end;

procedure TEditSubAnalysisThread.Execute;
begin
  try
    try
      if Terminated or FOwner.FAnalysisCancelRequested then
        Exit;
      if FBuildPreview then
        FOwner.RunIncrementalVocalsAnalysis(true, FTrack)
      else
        FOwner.RunIncrementalVocalsAnalysis(false, FTrack);
      if not Terminated and not FOwner.FAnalysisCancelRequested then
      begin
        FOwner.FAnalysisFailed := false;
        FOwner.FAnalysisError := '';
      end;
    except
      on E: Exception do
      begin
        if not Terminated and not FOwner.FAnalysisCancelRequested then
        begin
          FOwner.FAnalysisFailed := true;
          if FBuildPreview then
            FOwner.FAnalysisError := 'preview: ' + E.ClassName + ': ' + E.Message
          else
            FOwner.FAnalysisError := 'score: ' + E.ClassName + ': ' + E.Message;
        end;
      end;
    end;
  finally
    FOwner.FAnalysisThreadRunning := false;
    FOwner.FAnalysisCancelRequested := false;
  end;
end;

constructor TEditSubPreviewSourceStream.Create(AOwner: TScreenEditSub);
begin
  inherited Create;
  FOwner := AOwner;
  FFormat := TAudioFormatInfo.Create(1, VOCALS_WAVEFORM_SAMPLE_RATE, asfS16);
  FLoop := false;
  FPositionBytes := 0;
end;

destructor TEditSubPreviewSourceStream.Destroy;
begin
  FreeAndNil(FFormat);
  inherited;
end;

function TEditSubPreviewSourceStream.IsEOF(): boolean;
begin
  Result := (not FLoop) and (FPositionBytes >= FOwner.GetVocalsPreviewByteSize);
end;

function TEditSubPreviewSourceStream.IsError(): boolean;
begin
  Result := false;
end;

function TEditSubPreviewSourceStream.GetLength(): real;
begin
  if not Assigned(FFormat) or (FFormat.BytesPerSec <= 0) then
    Result := 0
  else
    Result := FOwner.GetVocalsPreviewByteSize / FFormat.BytesPerSec;
end;

function TEditSubPreviewSourceStream.GetPosition(): real;
begin
  if not Assigned(FFormat) or (FFormat.BytesPerSec <= 0) then
    Result := 0
  else
    Result := FPositionBytes / FFormat.BytesPerSec;
end;

procedure TEditSubPreviewSourceStream.SetPosition(Time: real);
begin
  if not Assigned(FFormat) or (FFormat.BytesPerSec <= 0) then
    FPositionBytes := 0
  else
  begin
    FPositionBytes := Trunc(Time * FFormat.BytesPerSec);
    FPositionBytes := FPositionBytes - (FPositionBytes mod SizeOf(SmallInt));
    if FPositionBytes < 0 then
      FPositionBytes := 0
    else if FPositionBytes > FOwner.GetVocalsPreviewByteSize then
      FPositionBytes := FOwner.GetVocalsPreviewByteSize;
  end;
end;

function TEditSubPreviewSourceStream.GetLoop(): boolean;
begin
  Result := FLoop;
end;

procedure TEditSubPreviewSourceStream.SetLoop(Enabled: boolean);
begin
  FLoop := Enabled;
end;

function TEditSubPreviewSourceStream.ReadData(Buffer: PByte; BufferSize: integer): integer;
var
  BytesLeft: Integer;
  BytesRead: Integer;
begin
  Result := 0;
  if not Assigned(FOwner) or (BufferSize <= 0) then
    Exit;

  BytesRead := FOwner.ReadVocalsPreviewData(FPositionBytes, Buffer, BufferSize);
  Inc(FPositionBytes, BytesRead);
  Result := BytesRead;

  if FLoop and (Result < BufferSize) then
  begin
    BytesLeft := BufferSize - Result;
    FPositionBytes := 0;
    while BytesLeft > 0 do
    begin
      BytesRead := FOwner.ReadVocalsPreviewData(FPositionBytes, @Buffer[Result], BytesLeft);
      if BytesRead <= 0 then
        Break;
      Inc(FPositionBytes, BytesRead);
      Inc(Result, BytesRead);
      Dec(BytesLeft, BytesRead);
      if FPositionBytes >= FOwner.GetVocalsPreviewByteSize then
        FPositionBytes := 0;
    end;
  end;
end;

function TEditSubPreviewSourceStream.GetAudioFormatInfo(): TAudioFormatInfo;
begin
  Result := FFormat;
end;

procedure TEditSubPreviewSourceStream.Close();
begin
  FOwner := nil;
end;

procedure OnSaveEncodingError(Value: boolean; Data: Pointer);
var
  SResult: TSaveSongResult;
  FilePath: IPath;
  Success: boolean;
begin
  Success := false;
  if (Value) then
  begin
    CurrentSong.Encoding := encUTF8;
    FilePath := CurrentSong.Path.Append(CurrentSong.FileName);
    // create backup file
    FilePath.CopyFile(Path(FilePath.ToUTF8 + '.ansi.bak'), false);
    // store in UTF-8 encoding
    SResult := SaveSong(CurrentSong, CurrentSong.Tracks, FilePath, boolean(Data));
    Success := (SResult = ssrOK);
  end;

  if (Success) then
    ScreenPopupInfo.ShowPopup(Language.Translate('INFO_FILE_SAVED'))
  else
    ScreenPopupError.ShowPopup(Language.Translate('ERROR_SAVE_FILE_FAILED'));
end;

procedure OnSaveTimingErrorChoice(Value: integer; Data: Pointer);
var
  EditScreen: TScreenEditSub;
begin
  if Data = nil then
    Exit;

  EditScreen := TScreenEditSub(Data);
  case Value of
    0: EditScreen.SaveSongToFile(EditScreen.PendingSaveRelative);
    1: EditScreen.GoToFirstTimingError;
  end;
end;

procedure OnExit(Value: boolean; Data: Pointer);
begin
  Display.CheckOK := Value;
  if (Value) then
  begin
    Display.CheckOK := false;
    AudioPlayback.Stop;
    AudioPlayback.SetVolume(1.0);
    Display.FadeTo(@ScreenSong);
  end;
end;

// Method for input parsing. If false is returned, GetNextWindow
// should be checked to know the next window to load;
function TScreenEditSub.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean;
var
  SDL_ModState: word;
begin
  Result := true;

  if not Repeated then
    RepeatCounter := 1
  else
    Inc(RepeatCounter);

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT + KMOD_RALT {+ KMOD_CAPS});

  if PianoEditMode then
  begin
    Result := ParseInputEditPiano(PressedKey, CharCode, PressedDown);
    if (Result = true) then
    begin
      Exit;
    end;
    if (PressedKey = SDLK_RETURN) then
    begin
      PressedKey := SDLK_P;
    end;
      Result := true;
  end;

  if TextEditMode or
     TitleEditMode or
     ArtistEditMode or
     LanguageEditMode or
     EditionEditMode or
     GenreEditMode or
     YearEditMode or
     CreatorEditMode or
     P1EditMode or
     P2EditMode then
  begin
    Result := ParseInputEditText(PressedKey, CharCode, PressedDown);
  end
  else if BPMEditMode then
  begin
    Result := ParseInputEditBPM(PressedKey, CharCode, PressedDown)
  end
  else
  begin

  if (PressedDown) then  // Key Down
  begin
    // check normal keys
    case PressedKey of
      SDLK_Q: Result := false;
      SDLK_S: HandleSaveSong(SDL_ModState);
      SDLK_I: SetPreviewStart(SDL_ModState);
      SDLK_A: SetMedleyTags(SDL_ModState);
      SDLK_J: Jump(SDL_ModState);
      SDLK_R: ReloadSong(SDL_ModState);
      SDLK_D:
        begin
          if (SDL_ModState = KMOD_LSHIFT) then HandleDivideBPM(SDL_ModState);
          if (SDL_ModState = KMOD_LCTRL or KMOD_LSHIFT) then ToggleDuet(SDL_ModState);
        end;
      SDLK_M: HandleMultiplyBPM(SDL_ModState);
      SDLK_C: CapitalizeLyrics(SDL_ModState);
      SDLK_V:
        if (SDL_ModState and KMOD_CTRL) <> 0 then
          HandlePaste(SDL_ModState)
        else
          HandleVideo(SDL_ModState);
      SDLK_T: HandleFixTimings(SDL_ModState);
      SDLK_P: HandlePlaySentence(SDL_ModState);
      SDLK_G: SetGoldenNote(SDL_ModState);
      SDLK_F: SetFreestyleNote(SDL_ModState);
      SDLK_Z: Undo(SDL_ModState);
      SDLK_ESCAPE: LeaveScope(SDL_ModState);
      SDLK_TAB: ShowPopupHelp(SDL_ModState);
      SDLK_BACKQUOTE: IncreaseNoteLength(SDL_ModState);
      SDLK_EQUALS, // for keyboards which produce the EQUALS symbol without SHIFT modifier
        SDLK_PLUS: HandleBPMIncrease(SDL_ModState); // for keyboards which produce the PLUS symbol without SHIFT modifier
      SDLK_MINUS: HandleBPMDecrease(SDL_ModState);
      SDLK_2, SDLK_3, SDLK_4, SDLK_5, SDLK_6: HandleExtendedCopyPaste(SDL_ModState, PressedKey);
      SDLK_7: DecreaseVideoGap(SDL_ModState);
      SDLK_8: IncreaseVideoGap(SDL_ModState);
      SDLK_9: DecreaseGAP(SDL_ModState);
      SDLK_0: IncreaseGAP(SDL_ModState);
      SDLK_KP_PLUS: IncreaseAllNoteTones(SDL_ModState);
      SDLK_KP_MINUS: DecreaseAllNoteTones(SDL_ModState);
      SDLK_SLASH, SDLK_HASH, SDLK_KP_DIVIDE: DivideOrJoinNotes(SDL_ModState);
      SDLK_F4: EnterTextEditMode(SDL_ModState);
      SDLK_F5: EnterBPMEditMode(SDL_ModState);
      SDLK_F6: EnterPianoEditMode(SDL_ModState);
      SDLK_SPACE: PlayNote(SDL_ModState);
      SDLK_W:
        if (SDL_ModState and (KMOD_LALT or KMOD_RALT)) <> 0 then
          Text[TextInfo].Text := 'Pitch contour is always shown';
      SDLK_RETURN: ToggleTextEditMode(SDL_ModState);
      SDLK_DELETE: DeleteNotes(SDL_ModState);
      SDLK_PERIOD: HandleMoveTextRight(SDL_ModState);
      SDLK_RIGHT: HandleMoveRight(SDL_ModState);
      SDLK_LEFT: HandleMoveLeft(SDL_ModState);
      SDLK_DOWN: HandleDownKey(SDL_ModState);
      SDLK_UP: HandleUpKey(SDL_ModState);
      end; // case
    end;
  end; // if
end;

      // SDLK_S: HandleSaveSong
procedure TScreenEditSub.HandleSaveSong(SDL_ModState: word);
var
  TimingErrors: UTF8String;
  SaveRelative: boolean;
begin
  SaveRelative := (SDL_ModState = KMOD_LSHIFT);
  PendingSaveRelative := SaveRelative;

  if CheckTimingSyntaxErrors(TimingErrors) then
  begin
    if TimingErrors <> '' then
      ScreenPopupError.ShowPopup(
        TimingErrors,
        ['Save anyways', 'Go to Error', 'Cancel'],
        OnSaveTimingErrorChoice,
        Self,
        1,
        2);
    Exit;
  end;

  SaveSongToFile(SaveRelative);
end;


      // SDLK_I: SetPreviewStart
procedure TScreenEditSub.SetPreviewStart(SDL_ModState: word);
// set PreviewStart tag
var
  LineIndex: Integer;
begin
  CopyToUndo;
  if SDL_ModState and KMOD_SHIFT <> 0 then
  begin
    if (CurrentSong.HasPreview) and
        (CurrentTrack = MedleyNotes.Preview.track) and
        (CurrentSong.Tracks[CurrentTrack].CurrentLine = MedleyNotes.Preview.line) and
        (CurrentNote[CurrentTrack] = MedleyNotes.Preview.note) and
        (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsStartPreview) then
    begin
      CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsStartPreview := false;
      CurrentSong.PreviewStart := 0;
      CurrentSong.HasPreview := false;
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_PREVIEW_CLEARED');
    end
    else
    begin
      // clear old IsStartPreview flag
      CurrentSong.Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].IsStartPreview := false;

      // set preview start
      CurrentSong.PreviewStart := Round(GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat) * 100) / 100;
      CurrentSong.HasPreview := CurrentSong.PreviewStart >= 0.0;
      if (CurrentSong.HasPreview) then
      begin
        MedleyNotes.Preview.track := CurrentTrack;
        MedleyNotes.Preview.line := CurrentSong.Tracks[CurrentTrack].CurrentLine;
        MedleyNotes.Preview.note := CurrentNote[CurrentTrack];
        CurrentSong.Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].IsStartPreview := true;
        Text[TextInfo].Text := Format(Language.Translate('EDIT_INFO_PREVIEW_SET'), [CurrentSong.PreviewStart]);
      end
      else
      begin
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsStartPreview := false;

      end;
    end;
  end
  else if InRange(CurrentSong.PreviewStart, 0.0, AudioPlayback.Length) then
  begin
    if SDL_ModState = KMOD_LALT then
    begin // jump and play
      // simulate sentence switch to clear props
      PreviousSentence;

      CurrentSong.Tracks[CurrentTrack].CurrentLine := 0; // update lyric

      Text[TextInfo].Text := Language.Translate('EDIT_INFO_JUMPTO_PREVIEW_AND_PLAY') + ' (' + FloatToStr(CurrentSong.PreviewStart) + ' s)';
      PlayStopTime := AudioPlayback.Length;
      PlaySentence := true;
      Click := false;
      AudioPlayback.Position := CurrentSong.PreviewStart;
      AudioPlayback.SetVolume(VolumeAudioIndex / 100);
      AudioPlayback.Play;

      // play video in sync if visible
      if (fCurrentVideo <> nil) then UpdateVideoPosition(AudioPlayback.Position);
    end
    else if SDL_ModState = 0 then
    begin // jump to preview start
      // simulate sentence switch to clear props
      PreviousSentence;

      CurrentBeat := Floor(GetMidBeat(CurrentSong.PreviewStart - (CurrentSong.GAP) / 1000));
      LineIndex := 0;
      while (LineIndex <= CurrentSong.Tracks[CurrentTrack].High) and (CurrentBeat > CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].EndBeat) do
        Inc(LineIndex);

      if LineIndex <= High(CurrentSong.Tracks[CurrentTrack].Lines) then
      begin
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
        CurrentSong.Tracks[CurrentTrack].CurrentLine := LineIndex;

        // finding the right note
        CurrentNote[CurrentTrack] := 0;
        while (CurrentNote[CurrentTrack] <= CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote) and (CurrentBeat > CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[CurrentNote[CurrentTrack]].EndBeat) do
          Inc(CurrentNote[CurrentTrack]);

        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
        EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
        EditorLyrics[CurrentTrack].Selected := 0;

        Text[TextInfo].Text := Language.Translate('EDIT_INFO_JUMPTO_PREVIEW') + ' (' + FloatToStr(CurrentSong.PreviewStart) + ' s)';
      end;
    end;
  end
  else Text[TextInfo].Text := Language.Translate('EDIT_INFO_NO_PREVIEW');
  Exit;
end;

      // SDLK_A: SetMedleyTags
procedure TScreenEditSub.SetMedleyTags(SDL_ModState: word);
// set Medley tags
begin
  CopyToUndo;
  if CurrentSong.Relative then
  begin
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_RELATIVE_UNSUPPORTED');
    Exit;
  end;

  if CurrentSong.isDuet then
  begin
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_DUET_UNSUPPORTED');
    Exit;
  end;

  MedleyNotes.isCustom := true;
  CurrentSong.Medley.Source := msTag;
  if SDL_ModState = KMOD_LSHIFT then // medley end note
  begin
    if MedleyNotes.isEnd then // if end is already set
    begin
      if (CurrentSong.Tracks[CurrentTrack].CurrentLine = MedleyNotes.end_.line) and (CurrentNote[CurrentTrack] = MedleyNotes.end_.note) then
      begin // current note is medley end, so clear it
        MedleyNotes.isEnd := false;
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := false;
        Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_END_CLEARED');
      end else // otherwise, clear old medley end flag and set medley end flag to current note
      begin
        if (MedleyNotes.isStart) and
           (MedleyNotes.start.line < CurrentSong.Tracks[CurrentTrack].CurrentLine) then // ensure that medley start is set and is earlier than medley end
        begin
          CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := true;
          if (Length(CurrentSong.Tracks[CurrentTrack].Lines) > MedleyNotes.end_.line) and
            (Length(CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes) > MedleyNotes.end_.note) then
            CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].IsMedley := false;
          MedleyNotes.end_.line := CurrentSong.Tracks[CurrentTrack].CurrentLine;
          MedleyNotes.end_.note := CurrentNote[CurrentTrack];
          Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_END_SET') + ' (' + FloatToStr(GetMedleyLength) + ' s)';
        end
        else
        begin
          if (MedleyNotes.isStart) then
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_END_AFTER_MEDLEY_START')
          else
            Text[TextInfo].Text := Language.Translate('EDIT_INFO_SET_MEDLEY_START_FIRST');
        end;
      end;
    end else // end is not set yet
    begin
    if (MedleyNotes.isStart) and
      (MedleyNotes.start.line < CurrentSong.Tracks[CurrentTrack].CurrentLine) then // ensure that medley start is set and is earlier than medley end
      begin
        MedleyNotes.isEnd := true;
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := true;
        MedleyNotes.end_.line := CurrentSong.Tracks[CurrentTrack].CurrentLine;
        MedleyNotes.end_.note := CurrentNote[CurrentTrack];
        Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_END_SET') + ' (' + FloatToStr(GetMedleyLength) + ' s)';
      end
      else
      begin
        if (MedleyNotes.isStart) then
          Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_END_AFTER_MEDLEY_START')
        else
          Text[TextInfo].Text := Language.Translate('EDIT_INFO_SET_MEDLEY_START_FIRST');
      end;
    end;
  end else
  begin // medley start note
    if MedleyNotes.isStart then
    begin
      if (CurrentSong.Tracks[CurrentTrack].CurrentLine = MedleyNotes.start.line) and (CurrentNote[CurrentTrack] = MedleyNotes.start.note) then
      begin
        MedleyNotes.isStart := false;
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := false;
        Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_START_CLEARED');
      end else
      begin
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := true;
        if (Length(CurrentSong.Tracks[CurrentTrack].Lines) > MedleyNotes.start.line) and
          (Length(CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes) > MedleyNotes.start.note) then
          CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes[MedleyNotes.start.note].IsMedley := false;
        MedleyNotes.start.line := CurrentSong.Tracks[CurrentTrack].CurrentLine;
        MedleyNotes.start.note := CurrentNote[CurrentTrack];
        Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_START_SET') + ifthen(MedleyNotes.isEnd, '(' + FloatToStr(GetMedleyLength) + ' s)', '');
      end;
    end else
    begin
      CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].IsMedley := true;
      MedleyNotes.isStart := true;
      MedleyNotes.start.line := CurrentSong.Tracks[CurrentTrack].CurrentLine;
      MedleyNotes.start.note := CurrentNote[CurrentTrack];
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_START_SET');
    end;
  end;

  Exit;
end;

      // SDLK_J: Jump
procedure TScreenEditSub.Jump(SDL_ModState: word);
// jump to Medley tags
var
  R: real;
begin
  if CurrentSong.Relative then
  begin
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_RELATIVE_UNSUPPORTED');
    Exit;
  end;

  if CurrentSong.isDuet then
  begin
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_MEDLEY_DUET_UNSUPPORTED');
    Exit;
  end;

  //Medley End Note
  if (SDL_ModState = KMOD_LSHIFT) then
  begin
    if (MedleyNotes.IsEnd) then
    begin
      // simulate sentence switch to clear props
      PreviousSentence;

      if (Length(CurrentSong.Tracks[CurrentTrack].Lines) > MedleyNotes.end_.line) and
            (Length(CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes) > MedleyNotes.end_.note) then
      begin
        CurrentSong.Tracks[CurrentTrack].CurrentLine := MedleyNotes.end_.line;
        CurrentNote[CurrentTrack] := MedleyNotes.end_.note;
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;

        EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
        EditorLyrics[CurrentTrack].Selected := 0;
        Text[TextInfo].Text := Language.Translate('EDIT_INFO_JUMPTO_MEDLEY_END') + ' (' + Language.Translate('EDIT_DURATION') + ' ' + FloatToStr(GetMedleyLength) + ' s)';
      end;
    end
    else
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_NO_MEDLEY_END');
  end;

  if (SDL_ModState = 0) then
  begin
    if (MedleyNotes.IsStart) then
    begin
      // simulate sentence switch to clear props
      PreviousSentence;

      if (Length(CurrentSong.Tracks[CurrentTrack].Lines)> MedleyNotes.start.line) and
          (Length(CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes) > MedleyNotes.start.note) then
      begin
        CurrentSong.Tracks[CurrentTrack].CurrentLine := MedleyNotes.start.line;
        CurrentNote[CurrentTrack] := MedleyNotes.start.note;
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;

        EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
        EditorLyrics[CurrentTrack].Selected := 0;
        Text[TextInfo].Text := Language.Translate('EDIT_INFO_JUMPTO_MEDLEY_START') + ' (' + Language.Translate('EDIT_DURATION') + ' ' + FloatToStr(GetMedleyLength) + ' s)';
      end;
    end
    else
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_NO_MEDLEY_START');
  end;

  if (SDL_ModState = KMOD_LALT) then
  begin
    Writeln('ALT+J');
    // simulate sentence switch to clear props
    PreviousSentence;

    if (MedleyNotes.isStart and MedleyNotes.isEnd) and
      (MedleyNotes.start.line < MedleyNotes.end_.line) and
      (Length(CurrentSong.Tracks[CurrentTrack].Lines) > MedleyNotes.end_.line) and
      (Length(CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes) > MedleyNotes.end_.note) and
      (Length(CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes) > MedleyNotes.start.note) then
    begin
      R := GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes[MedleyNotes.start.note].StartBeat);
      if InRange(R, 0.0, AudioPlayback.Length) then
      begin
        AudioPlayback.Position:= R;
        PlayStopTime := GetTimeFromBeat(
          CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].StartBeat +
          CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].Duration);
        PlaySentence := true;
        Click := false;
        AudioPlayback.SetVolume(VolumeAudioIndex / 100);
        AudioPlayback.Play;

        // play video in sync if visible
        if (fCurrentVideo <> nil) then UpdateVideoPosition(AudioPlayback.Position);
        Text[TextInfo].Text := Language.Translate('EDIT_INFO_JUMPTO_MEDLEY_AND_PLAY');
      end;
    end
    else
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_NO_MEDLEY_SECTION');
  end;

  Exit;
end;

      // SDLK_R: ReloadSong
procedure TScreenEditSub.ReloadSong(SDL_ModState: word);
//reload
begin
  AudioPlayback.Stop;
  {$IFDEF UseMIDIPort}
  MidiOut.Close;
  MidiOut.Free;
  {$ENDIF}

  FPreserveAnalysisOnShow := true;
  try
    OnShow;
  finally
    FPreserveAnalysisOnShow := false;
  end;
  if HasVocalsPreviewPlayback and (not FAnalysisThreadRunning) then
    RecomputeAllPreviewScores;
  Text[TextInfo].Text := Language.Translate('EDIT_INFO_SONG_RELOADED');
end;

      // SDLK_D: HandleDivideBPM; or ToggleDuet;
procedure TScreenEditSub.HandleDivideBPM(SDL_ModState: word);
begin
  // Divide lengths by 2
  if (SDL_ModState = KMOD_LSHIFT) then
  begin
    CopyToUndo;
    DivideBPM;
    ShowInteractiveBackground;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_DIVIDED_BPM');
    Exit;
  end;
end;

procedure TScreenEditSub.ToggleDuet(SDL_ModState: word);
begin
  // create duet or convert duet to normal song
  if (SDL_ModState = KMOD_LCTRL or KMOD_LSHIFT) then
  begin
    if (CurrentSong.isDuet) then
    begin
      MakeSolo;
      FixTimings;
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_CONVERTED_TO_SOLO');
    end
    else
    begin
      MakeDuet;
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_CONVERTED_TO_DUET');
    end;
  end;
end;

      // SDLK_M: HandleMultiplyBPM;
procedure TScreenEditSub.HandleMultiplyBPM(SDL_ModState: word);
begin
  // Multiply lengths by 2
  if (SDL_ModState = KMOD_LSHIFT) then
  begin
    CopyToUndo;
    MultiplyBPM;
    ShowInteractiveBackground;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_MULTIPLIED_BPM');
    Exit;
  end;
end;

      // SDLK_C: CapitalizeLyrics;
procedure TScreenEditSub.CapitalizeLyrics(SDL_ModState: word);
begin
  // Capitalize letter at the beginning of line
  if SDL_ModState = 0 then
    begin
    CopyToUndo;
    LyricsCapitalize;
    EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
    EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_CAPITALIZATION_CORRECTED');
    end;

  // Correct spaces
  if SDL_ModState = KMOD_LSHIFT then
    begin
      CopyToUndo;
      LyricsCorrectSpaces;
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_SPACES_CORRECTED');
    end;

  // Copy sentence
  if SDL_ModState = KMOD_LCTRL then
  begin
    MarkCopySrc;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_MARKED_FOR_COPY');
  end;

  Exit;
end;

procedure TScreenEditSub.HandleLinePlayback(SDL_ModState: word; const WithVideo: Boolean;
  const AllowVocalsPreview: Boolean);
var
  R: real;
  PlaybackStart: real;
  PlaybackStop: real;
  SentenceEndTime: real;
  SentencePreroll: real;
  RequestedVocalsPreview: boolean;
  UseVocalsPreview: boolean;
  UseMidiPreview: boolean;
  ContinuePlayback: boolean;
  UseClicks: boolean;
begin
  ContinuePlayback := (SDL_ModState and KMOD_ALT) <> 0;
  RequestedVocalsPreview := AllowVocalsPreview and ((SDL_ModState and KMOD_CTRL) <> 0);
  UseVocalsPreview := RequestedVocalsPreview;
  UseMidiPreview := (SDL_ModState and KMOD_SHIFT) <> 0;
  UseClicks := (not WithVideo) and (SDL_ModState = 0);

  if UseVocalsPreview then
  begin
    if HasVocalsPreviewSource and (not FPreviewBuildStarted) and (not FAnalysisFailed) then
      StartAnalysisThread(true, CurrentTrack);
  end
  else
    RestoreStandardPlayback(false);

  Click := false;
  StopPlayback;
  PlayVideo := WithVideo;
  StopVideoPreview;
  CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
  CurrentNote[CurrentTrack] := 0;

  R := GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
  SentenceEndTime := GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat);
  PlaybackStart := R;
  SentencePreroll := 3 * GetSentenceLeadSeconds(UseMidiPreview);
  if SentencePreroll > 0 then
  begin
    PlaybackStart := R - SentencePreroll;
    if PlaybackStart < 0 then
      PlaybackStart := 0;
  end;

  if ContinuePlayback then
    PlaybackStop := GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].High].EndBeat)
  else
    PlaybackStop := SentenceEndTime;

  if UseVocalsPreview then
  begin
    if HasVocalsPreviewPlaybackAtTime(PlaybackStart) then
      SetVocalsPreviewPlayback(true, false)
    else
    begin
      // fall back silently to standard playback with the same no-click behavior
      UseVocalsPreview := false;
      RestoreStandardPlayback(false);
    end;
  end;

  PlaySentenceMidi := UseMidiPreview;
  {$IFDEF UseMIDIPort}
  StopMidi;
  {$ENDIF}
  if PlaySentenceMidi then
  begin
    PlayOneMidi := false;
    midinotefound := false;
  end;

  if UseVocalsPreview or RequestedVocalsPreview or (not UseMidiPreview) then
    SetPlaybackVolume(VolumeAudioIndex / 100)
  else
    SetPlaybackVolume(0);

  if PlaybackStart <= GetPlaybackLength then
  begin
    SetPlaybackPosition(PlaybackStart);
    {$IFDEF UseMIDIPort}
    if PlaySentenceMidi then
      ConfigureMidiPlayback(PlaybackStart, PlaybackStop);
    {$ELSE}
      PlaySentenceMidi := false;
    {$ENDIF}
    PlayStopTime := PlaybackStop;
    PlaySentence := true;
    Click := UseClicks;
    PlayPlayback;
    LastClick := -100;
    ResetBeatTracking;
    if WithVideo then
      StartVideoPreview();
  end;

  if WithVideo then
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_SONG')
  else if UseVocalsPreview then
    Text[TextInfo].Text := 'Play sentence vocals'
  else if UseMidiPreview then
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE_AUDIO_AND_MIDI')
  else
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE_AUDIO');
end;

      // SDLK_V: HandleVideo; HandlePaste;
procedure TScreenEditSub.HandleVideo(SDL_ModState: word);
begin
  HandleLinePlayback(SDL_ModState, true, false);
end;

procedure TScreenEditSub.HandlePaste(SDL_ModState: word);
begin
  if not HasCopySource then
  begin
    Text[TextInfo].Text := 'Nothing copied yet';
    Exit;
  end;

  // paste notes + text (enforce length of src line)
  if SDL_ModState = KMOD_LCTRL then
  begin
    CopyToUndo;
    CopySentence(CopySrc.track, CopySrc.line, CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine, true, true, true);
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_PASTE_SENTENCE');
  end;

  // paste text only (use minimum of src and dst length)
  if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
  begin
    CopyToUndo;
    CopySentence(CopySrc.track, CopySrc.line, CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine, true, false, false);
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_PASTE_TEXT');
  end;

  // paste notes only (use minimum of src and dst length)
  if SDL_ModState = KMOD_LCTRL + KMOD_LALT then
  begin
    CopyToUndo;
    CopySentence(CopySrc.track, CopySrc.line, CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine, false, true, false);
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_PASTE_NOTES');
  end;

  // paste notes + text (use minimum of src and dst length)
  if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
  begin
    CopyToUndo;
    CopySentence(CopySrc.track, CopySrc.line, CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine, true, true, false);
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_PASTE_TEXT_NOTES');
  end;

  GoldenRec.KillAll;
  ShowInteractiveBackground;
end;

      // SDLK_T: HandleFixTimings;
procedure TScreenEditSub.HandleFixTimings(SDL_ModState: word);
begin
  // Fixes timings between sentences
  CopyToUndo;
  FixTimings;
  TriggerGlobalPreviewRescore;
  Text[TextInfo].Text := Language.Translate('EDIT_INFO_FIX_TIMINGS');
  Exit;
end;

      // SDLK_P: PlaySentence;
procedure TScreenEditSub.HandlePlaySentence(SDL_ModState: word);
begin
  HandleLinePlayback(SDL_ModState, false, true);
end;
      // SDLK_G: SetGoldenNote;
procedure TScreenEditSub.SetGoldenNote(SDL_ModState: word);
// Golden Note
begin
  CopyToUndo;
  if (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntGolden) then
  begin
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntRapGolden;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_RAPGOLDEN');
  end
  else if (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntRapGolden) then
  begin
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntNormal;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_NORMAL');
  end
  else
  begin
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntGolden;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_GOLDEN');
  end;
  TriggerGlobalPreviewRescore;
  GoldenRec.KillAll;
  Exit;
end;

      // SDLK_F: SetFreestyleNote;
procedure TScreenEditSub.SetFreestyleNote(SDL_ModState: word);
// Freestyle Note
begin
  CopyToUndo;
  if (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntFreestyle) then
  begin
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntRap;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_RAP');
  end
  else if (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntRap) then
  begin
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntNormal;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_NORMAL');
  end
  else
  begin
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntFreestyle;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SET_TO_FREESTYLE');
  end;
  TriggerGlobalPreviewRescore;
  GoldenRec.KillAll;

  // update lyrics
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
  Exit;
end;

      // SDLK_Z: Undo;
procedure TScreenEditSub.Undo(SDL_ModState: word);
// undo
begin
  if SDL_ModState = KMOD_LCTRL then
  begin
      CopyFromUndo;
      GoldenRec.KillAll;
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_UNDO');
  end;
  ShowInteractiveBackground;
end;

    // SDLK_ESCAPE: LeaveScope
procedure TScreenEditSub.LeaveScope(SDL_ModState: word);
begin
  if length(UndoLines) > 0 then
    ScreenPopupcheck.ShowPopup(Language.Translate('EDIT_INFO_EXIT'), OnExit, nil, false)
  else
    FadeTo(@ScreenSong);
end;

      // SDLK_TAB: ShowPopupHelp;
procedure TScreenEditSub.ShowPopupHelp(SDL_ModState: word);
begin
  ScreenPopupHelp.ShowPopup();
end;

      // SDLK_BACKQUOTE: IncreaseNoteLength;
procedure TScreenEditSub.IncreaseNoteLength(SDL_ModState: word);
begin
  // Increase Note Length (same as Alt + Right)
  CopyToUndo;
  Inc(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
  if CurrentNote[CurrentTrack] = CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote then
  begin
    Inc(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat);
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_LENGTH_INCREASED');
  end;
  GoldenRec.KillAll;
  ShowInteractiveBackground;
end;

      // SDLK_EQUALS, HandleBPMIncrease; // for keyboards which produce the EQUALS symbol without SHIFT modifier
      // SDLK_PLUS: HandleBPMIncrease; // for keyboards which produce the PLUS symbol without SHIFT modifier
procedure TScreenEditSub.HandleBPMIncrease(SDL_ModState: word);
begin
  // Increase BPM
  CopyToUndo;
  if SDL_ModState = 0 then
  begin
    CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 5) + 1) / 5; // (1/20)
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_BPM_INCREASED_BY') + ' 0.05';
  end;
  if SDL_ModState = KMOD_LSHIFT then
  begin
    CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM + 4; // (1/1)
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_BPM_INCREASED_BY') + ' 1.0';
  end;
  if SDL_ModState = KMOD_LCTRL then
  begin
    CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 25) + 1) / 25; // (1/100)
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_BPM_INCREASED_BY') + ' 0.01';
  end;
end;

      // SDLK_MINUS: HandleBPMDecrease;
procedure TScreenEditSub.HandleBPMDecrease(SDL_ModState: word);
begin
  // Decrease BPM
  CopyToUndo;
  if SDL_ModState = 0 then
  begin
    CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 5) - 1) / 5;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_BPM_DECREASED_BY') + ' 0.05';
  end;
  if SDL_ModState = KMOD_LSHIFT then
  begin
    CurrentSong.BPM[0].BPM := CurrentSong.BPM[0].BPM - 4;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_BPM_DECREASED_BY') + ' 1.0';
  end;
  if SDL_ModState = KMOD_LCTRL then
  begin
    CurrentSong.BPM[0].BPM := Round((CurrentSong.BPM[0].BPM * 25) - 1) / 25;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_BPM_DECREASED_BY') + ' 0.01';
  end;

  if CurrentSong.BPM <= 0 then
    CurrentSong.BPM := MIN_EDITOR_BPM;
end;

      // SDLK_2, SDLK_3, SDLK_4, SDLK_5, SDLK_6: HandleExtendedCopyPaste;
procedure TScreenEditSub.HandleExtendedCopyPaste(SDL_ModState: word; PressedKey: Integer);
var
  DstTrack:     Integer;
  DstLine:      Integer;
  NumLines:     Integer;
  LineCount:    Integer;
begin
  if not HasCopySource then
  begin
    Text[TextInfo].Text := 'Nothing copied yet';
    Exit;
  end;

  DstTrack := CurrentTrack;
  DstLine := CurrentSong.Tracks[CurrentTrack].CurrentLine;
  NumLines := PressedKey - SDLK_0;
  // copy less lines if there are not enough src lines available
  NumLines := Min(NumLines, High(CurrentSong.Tracks[CopySrc.track].Lines) - CopySrc.line + 1);
  // copy less lines if there are not enough dst lines available
  NumLines := Min(NumLines, High(CurrentSong.Tracks[CurrentTrack].Lines) - DstLine + 1);

  for LineCount := 0 to NumLines - 1 do
  begin
    if (SDL_ModState = KMOD_LCTRL) then
    begin
      CopyToUndo;
      // paste notes + text (use src length, ignore dst length)
      CopySentence(CopySrc.track, CopySrc.line + LineCount, DstTrack, DstLine + LineCount, true, true, true);
      Text[TextInfo].Text := Format(Language.Translate('EDIT_INFO_PASTE_SENTENCE_N'), [NumLines]);
    end;
    if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT then
    begin
      CopyToUndo;
      // paste text only (use minimum of src and dst length)
      CopySentence(CopySrc.track, CopySrc.line + LineCount, DstTrack, DstLine + LineCount, true, false, false);
      Text[TextInfo].Text := Format(Language.Translate('EDIT_INFO_PASTE_TEXT_N'), [NumLines]);
    end;
    if SDL_ModState = KMOD_LCTRL + KMOD_LALT then
    begin
      CopyToUndo;
      // paste notes only (use minimum of src and dst length)
      CopySentence(CopySrc.track, CopySrc.line + LineCount, DstTrack, DstLine + LineCount, false, true, false);
      Text[TextInfo].Text := Format(Language.Translate('EDIT_INFO_PASTE_NOTES_N'), [NumLines]);
    end;
    if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
    begin
      CopyToUndo;
      // paste notes + text (use minimum of src and dst length)
      CopySentence(CopySrc.track, CopySrc.line + LineCount, DstTrack, DstLine + LineCount, true, true, false);
      Text[TextInfo].Text := Format(Language.Translate('EDIT_INFO_PASTE_TEXT_NOTES_N'), [NumLines]);
    end;
  end;
  GoldenRec.KillAll;
  ShowInteractiveBackground;

  // does this insert additional 4 lines before the current line?
  {if SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT + KMOD_LALT then
  begin
    CopyToUndo;
    CopySentences(CopySrc.track, CopySrc.line, CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine, 4);
    GoldenRec.KillAll;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_COPY_4_SENTENCES');
  end;}
end;

      // SDLK_7: DecreaseVideoGap
procedure TScreenEditSub.DecreaseVideoGap(SDL_ModState: word);
begin
  // Decrease VideoGap
  CopyToUndo;
  if SDL_ModState = 0 then
  begin
    CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) - 1) / 100;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_VIDEOGAP_DECREASED_BY') + ' 0.01';
  end;
  if SDL_ModState = KMOD_LSHIFT then
  begin
    CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) - 10) / 100;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_VIDEOGAP_DECREASED_BY') + ' 0.1';
  end;
  if SDL_ModState = KMOD_LCTRL then
  begin
    CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) - 100) / 100;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_VIDEOGAP_DECREASED_BY') + ' 1.0';
  end;
end;

      // SDLK_8: IncreaseVideoGap;
procedure TScreenEditSub.IncreaseVideoGap(SDL_ModState: word);
begin
  // Increase VideoGap
  CopyToUndo;
  if SDL_ModState = 0 then
  begin
    CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) + 1) / 100;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_VIDEOGAP_INCREASED_BY') + ' 0.01 s';
  end;
  if SDL_ModState = KMOD_LSHIFT then
  begin
    CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) + 10) / 100;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_VIDEOGAP_INCREASED_BY') + ' 0.1 s';
  end;
  if SDL_ModState = KMOD_LCTRL then
  begin
    CurrentSong.VideoGAP := (round(CurrentSong.VideoGAP*100) + 100) / 100;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_VIDEOGAP_INCREASED_BY') + ' 1.0 s';
  end;
end;

      // SDLK_9: DecreaseGAP
procedure TScreenEditSub.DecreaseGAP(SDL_ModState: word);
begin
  // Decrease GAP
  CopyToUndo;
  if SDL_ModState = 0 then
  begin
    CurrentSong.GAP := CurrentSong.GAP - 10;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_GAP_DECREASED_BY') + ' 10 ms';
  end;
  if SDL_ModState = KMOD_LSHIFT then
  begin
    CurrentSong.GAP := CurrentSong.GAP - 1000;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_GAP_DECREASED_BY') + ' 1000 ms';
  end;
  TriggerGlobalPreviewRescore;
end;

      // SDLK_0: IncreaseGAP
procedure TScreenEditSub.IncreaseGAP(SDL_ModState: word);
begin
  // Increase GAP
  CopyToUndo;
  if SDL_ModState = 0 then
  begin
    CurrentSong.GAP := CurrentSong.GAP + 10;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_GAP_INCREASED_BY') + ' 10 ms';
  end;
  if SDL_ModState = KMOD_LSHIFT then
  begin
    CurrentSong.GAP := CurrentSong.GAP + 1000;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_GAP_INCREASED_BY') + ' 1000 ms';
  end;
  TriggerGlobalPreviewRescore;
end;

      // SDLK_KP_PLUS: IncreaseAllNoteTones
procedure TScreenEditSub.IncreaseAllNoteTones(SDL_ModState: word);
begin
  // Increase tone of all notes
  CopyToUndo;
  if SDL_ModState = 0 then
  begin
    ChangeWholeTone(1);
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_ALL_TONES_INCREASED_BY_SEMITONE');
  end;
  if SDL_ModState = KMOD_LSHIFT then
  begin
    ChangeWholeTone(12);
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_ALL_TONES_INCREASED_BY_OCTAVE');
  end;
  GoldenRec.KillAll;
  ShowInteractiveBackground;
end;

      // SDLK_KP_MINUS: DecreaseAllNoteTones
procedure TScreenEditSub.DecreaseAllNoteTones(SDL_ModState: word);
begin
  // Decrease tone of all notes
  CopyToUndo;
  if SDL_ModState = 0 then
  begin
    ChangeWholeTone(-1);
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_ALL_TONES_DECREASED_BY_SEMITONE');
  end;
  if SDL_ModState = KMOD_LSHIFT then
  begin
    ChangeWholeTone(-12);
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_ALL_TONES_DECREASED_BY_OCTAVE');
  end;
  GoldenRec.KillAll;
  ShowInteractiveBackground;
end;

      // SDLK_KP_DIVIDE is a temporary workaround for German keyboards
      //SDLK_SLASH, SDLK_HASH, SDLK_KP_DIVIDE: DivideOrJoinNotes;
procedure TScreenEditSub.DivideOrJoinNotes(SDL_ModState: word);
begin
  CopyToUndo;
  if SDL_ModState = 0 then
  begin
    // Start a new sentence with currently selected note
    if CurrentNote[CurrentTrack] > 0 then
    begin
      DivideSentence;
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_DIVIDED');
    end;
    GoldenRec.KillAll;
  end;

  if SDL_ModState = KMOD_LSHIFT then
  begin
    // Join current with subsequent sentence
    if CurrentSong.Tracks[CurrentTrack].CurrentLine < CurrentSong.Tracks[CurrentTrack].High then
    begin
      JoinSentence;
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINES_JOINED');
    end;
    GoldenRec.KillAll;
  end;

  if SDL_ModState = KMOD_LCTRL then
  begin
    // divide note
    DivideNote(false);
    EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
    EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_DIVIDED');
    GoldenRec.KillAll;
  end;
ShowInteractiveBackground;
end;

      // SDLK_F4: EnterTextEditMode
procedure TScreenEditSub.EnterTextEditMode(SDL_ModState: word);
begin
  // Enter Text Edit Mode
  BackupEditText := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
  CurrentEditText := BackupEditText;
  CurrentSlideId := LyricSlideId;
  TextPosition := LengthUTF8(BackupEditText);
  editLengthText := LengthUTF8(BackupEditText);
  TextEditMode := true;
  StartTextInput;
end;

      // SDLK_F5: EnterBPMEditMode
procedure TScreenEditSub.EnterBPMEditMode(SDL_ModState: word);
begin
  // Enter BPM Edit Mode
  BackupEditText := FloatToStr(CurrentSong.BPM[0].BPM / 4);
  CurrentEditText := BackupEditText;
  CurrentSlideId := BPMSlideId;
  TextPosition := LengthUTF8(BackupEditText);
  editLengthText := LengthUTF8(BackupEditText);
  BPMEditMode := true;
  StartTextInput;
end;

      // SDLK_F6: EnterPianoEditMode
procedure TScreenEditSub.EnterPianoEditMode(SDL_ModState: word);
begin
  // Enter Piano Edit Mode
  PianoEditMode := true;
end;

      // SDLK_SPACE: PlayNote
procedure TScreenEditSub.PlayNote(SDL_ModState: word);
var
  PlayAudio: boolean;
  NoteStart: real;
  NoteStop: real;
  NoteEndBeat: Integer;
  NoteTone: Integer;
  ModWithoutAlt: word;
  UseVocalsPreview: boolean;
begin
  ModWithoutAlt := SDL_ModState and not (KMOD_LALT + KMOD_RALT);
  UseVocalsPreview := (SDL_ModState and (KMOD_LALT + KMOD_RALT)) <> 0;
  if UseVocalsPreview then
  begin
    if HasVocalsPreviewSource and (not FPreviewBuildStarted) and (not FAnalysisFailed) then
      StartAnalysisThread(true, CurrentTrack);
  end
  else
    RestoreStandardPlayback(false);

  NoteTone := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone;
  NoteStart := GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
  NoteStop := GetTimeFromBeat(
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat +
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
  NoteEndBeat := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat +
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration;

  if UseVocalsPreview then
  begin
    if HasVocalsPreviewPlaybackAtTime(NoteStart) then
      SetVocalsPreviewPlayback(true, false)
    else
    begin
      UseVocalsPreview := false;
      RestoreStandardPlayback(false);
    end;
  end;

  // Play current note
  PlaySentenceMidi := false; // stop midi
  {$IFDEF UseMIDIPort}
  StopMidi;
  {$ENDIF}
  PlaySentence := false;
  midinotefound := false;
  PlayOne := true;
  PlayAudio := (ModWithoutAlt = 0) or (ModWithoutAlt and KMOD_LCTRL <> 0);
  if not PlayAudio then
    SetPlaybackVolume(0)
  else
    SetPlaybackVolume(VolumeAudioIndex / 100);
  PlayOneMidi := ModWithoutAlt and KMOD_LSHIFT <> 0;
  if PlayOneMidi then
  begin
    ConfigureMidiPlayback(NoteStart, NoteStop);
    {$IFDEF UseMIDIPort}
    MidiStopBeat := NoteEndBeat;
    PlayMidiTone(NoteTone);
    {$ENDIF}
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_NOTE_MIDI');
    midinotefound := true;
  end
  else
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_NOTE_AUDIO');
  PlayVideo := false;
  StopVideoPreview;
  Click := false;
  StopPlayback;
  SetPlaybackPosition(NoteStart);
  PlayStopTime := NoteStop;
  PlayPlayback;
  LastClick := -100;
end;

      // SDLK_RETURN: ToggleTextEditMode
procedure TScreenEditSub.ToggleTextEditMode(SDL_ModState: word);
var
  R: real;
  LineIndex:    Integer;
  NoteIndex:    Integer;
begin
    if Interaction =  TitleSlideId then
    begin
      BackupEditText := CurrentSong.Title;
      CurrentEditText := BackupEditText;
      editLengthText := LengthUTF8(BackupEditText);
      CurrentSlideId := TitleSlideId;
      TextPosition := LengthUTF8(BackupEditText);
      TitleEditMode := true;
      StartTextInput;
    end;

    if Interaction = ArtistSlideId then
    begin
      BackupEditText := CurrentSong.Artist;
      CurrentEditText := BackupEditText;
      editLengthText := LengthUTF8(BackupEditText);
      CurrentSlideId := ArtistSlideId;
      TextPosition := LengthUTF8(BackupEditText);
      ArtistEditMode := true;
      StartTextInput;
    end;

    if Interaction = LanguageSlideId then
    begin
      BackupEditText := ifthen(CurrentSong.Language <> 'Unknown', CurrentSong.Language, NOT_SET);
      CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
      editLengthText := LengthUTF8(BackupEditText);
      CurrentSlideId := LanguageSlideId;
      TextPosition := LengthUTF8(BackupEditText);
      LanguageEditMode := true;
      StartTextInput;
    end;

    if Interaction = EditionSlideId then
    begin
      BackupEditText := ifthen(CurrentSong.Edition <> 'Unknown', CurrentSong.Edition, NOT_SET);
      CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
      editLengthText := LengthUTF8(BackupEditText);
      CurrentSlideId := EditionSlideId;
      TextPosition := LengthUTF8(BackupEditText);
      EditionEditMode := true;
      StartTextInput;
    end;

    if Interaction = GenreSlideId then
    begin
      BackupEditText := ifthen(CurrentSong.Genre <> 'Unknown', CurrentSong.Genre, NOT_SET);
      CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
      editLengthText := LengthUTF8(BackupEditText);
      CurrentSlideId := GenreSlideId;
      TextPosition := LengthUTF8(BackupEditText);
      GenreEditMode := true;
      StartTextInput;
    end;

    if Interaction = YearSlideId then
    begin
      BackupEditText := ifthen(CurrentSong.Year <> 0, IntToStr(CurrentSong.Year), NOT_SET);
      CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
      editLengthText := LengthUTF8(BackupEditText);
      CurrentSlideId := YearSlideId;
      TextPosition := LengthUTF8(BackupEditText);
      YearEditMode := true;
      StartTextInput;
    end;

    if Interaction = CreatorSlideId then
    begin
      BackupEditText := ifthen(CurrentSong.Creator <> '', CurrentSong.Creator, NOT_SET);
      CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
      editLengthText := LengthUTF8(BackupEditText);
      CurrentSlideId := CreatorSlideId;
      TextPosition := LengthUTF8(BackupEditText);
      CreatorEditMode := true;
      StartTextInput;
    end;

    // Interaction = 7 // Mp3SlideId
    // Interaction = 8 // CoverSlideId
    // Interaction = 9 // BackgroundSlideId
    // Interaction = 10 // VideoSlideId
    // Interaction = 11 // VideoGapSlideId

    if Interaction = BPMSlideId then
    begin
      BackupEditText := FloatToStr(CurrentSong.BPM[0].BPM / 4);
      CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
      editLengthText := LengthUTF8(BackupEditText);
      CurrentSlideId := BPMSlideId;
      TextPosition := LengthUTF8(BackupEditText);
      BPMEditMode := true;
      StartTextInput;
    end;

    // Interaction = 13 // GAPSlideId
    // Interaction = 14 // StartTagSlideId
    // Interaction = 15 // EndTagSlideId
    // Interaction = 16 // PreviewSlideId
    // Interaction = 17 // RelativeSlideId

    if Interaction = MedleyStartSlideId then
    begin
      BackupEditText := ifthen(CurrentSong.DuetNames[0] <> '', CurrentSong.DuetNames[0], NOT_SET);
      CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
      editLengthText := LengthUTF8(BackupEditText);
      CurrentSlideId := MedleyStartSlideId;
      TextPosition := LengthUTF8(BackupEditText);
      P1EditMode := true;
      StartTextInput;
    end;

    if Interaction = MedleyEndSlideId then
    begin
      BackupEditText := ifthen(CurrentSong.DuetNames[1] <> '', CurrentSong.DuetNames[1], NOT_SET);
      CurrentEditText := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
      editLengthText := LengthUTF8(BackupEditText);
      CurrentSlideId := MedleyEndSlideId;
      TextPosition := LengthUTF8(BackupEditText);
      P2EditMode := true;
      StartTextInput;
    end;

    // Interaction = 20 // StartSlideId
    // Interaction = 21 // DurationSlideId
    // Interaction = 22 // ToneSlideId

    if Interaction = LyricSlideId then
    begin
      BackupEditText := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
      CurrentEditText := BackupEditText;
      editLengthText := LengthUTF8(BackupEditText);
      CurrentSlideId := LyricSlideId;
      TextPosition := LengthUTF8(BackupEditText);
      TextEditMode := true;
      StartTextInput;
    end;

    if Interaction = 24 then // UndoButtonId
    begin
      CopyFromUndo;
      GoldenRec.KillAll;
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_UNDO');
      ShowInteractiveBackground;
    end;

    if Interaction = 25 then // PreviousSeqButtonID
    begin
      PreviousSentence;
    end;

    if Interaction = 26 then // NextSeqButtonID
    begin
      NextSentence;
    end;

    if Interaction = 27 then // FreestyleButtonID
    begin
      CopyToUndo;
      if (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntFreestyle) then
      begin
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntRap;
      end
      else if (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntRap) then
      begin
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntNormal;
      end
      else
      begin
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntFreestyle;
      end;
      GoldenRec.KillAll;

      // update lyrics
      EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
      EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
      Exit;
    end;

    if Interaction = 28 then // GoldButtonID
    begin
      CopyToUndo;
      if (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntGolden) then
      begin
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntRapGolden;
      end
      else if (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType = ntRapGolden) then
      begin
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntNormal;
      end
      else
      begin
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].NoteType := ntGolden;
      end;
      GoldenRec.KillAll;
      Exit;
    end;

    if Interaction = 29 then // PlayOnlyButtonID
    begin
      // Play Sentence
      Click := true;
      AudioPlayback.Stop;
      PlayVideo := false;
      StopVideoPreview;
      CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
      CurrentNote[CurrentTrack] := 0;
      R := GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
      if R <= AudioPlayback.Length then
      begin
        AudioPlayback.Position := R;
        PlayStopTime := GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat);
        PlaySentence := true;
        AudioPlayback.SetVolume(VolumeAudioIndex / 100);
        AudioPlayback.Play;
        LastClick := -100;
      end;
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE');
    end;

    if Interaction = 30 then // PlayWithNoteButtonID
    begin
      CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
      CurrentNote[CurrentTrack] := 0;
      {$IFDEF UseMIDIPort}
      PlaySentenceMidi := true;
      {$ELSE}
      PlaySentenceMidi := false;
      {$ENDIF}
      PlayVideo := false;
      StopVideoPreview;
      LastClick := -100;

      PlaySentence := true;
      Click := true;
      AudioPlayback.Stop;
      AudioPlayback.Position := GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat)+0{-0.10};
      PlayStopTime := GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat)+0;
      ConfigureMidiPlayback(AudioPlayback.Position, PlayStopTime);
      AudioPlayback.SetVolume(VolumeAudioIndex / 100);
      AudioPlayback.Play;
      LastClick := -100;
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE');
    end;

    if Interaction = 31 then // PlayNoteButtonID
    begin
      CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
      CurrentNote[CurrentTrack] := 0;
      {$IFDEF UseMIDIPort}
      PlaySentenceMidi := true;
      {$ELSE}
      PlaySentenceMidi := false;
      {$ENDIF}
      PlayVideo := false;
      StopVideoPreview;
      PlaySentence := true;
      Click := true;
      AudioPlayback.Stop;
      AudioPlayback.Position := GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
      PlayStopTime := GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat);
      ConfigureMidiPlayback(AudioPlayback.Position, PlayStopTime);
      AudioPlayback.SetVolume(VolumeAudioIndex / 100);
      AudioPlayback.Play;
      LastClick := -100;
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_PLAY_SENTENCE');
    end;

    for LineIndex := 0 to CurrentSong.Tracks[CurrentTrack].High do
    begin
      if Interaction = InteractiveLineId[LineIndex] then
      begin
        CopyToUndo;
        GoldenRec.KillAll;
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
        CurrentSong.Tracks[CurrentTrack].CurrentLine := LineIndex;
        ShowInteractiveBackground;
        CurrentNote[CurrentTrack] := 0;
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
        EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
        EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
      end;
    end;

    if high(InteractiveNoteId) >= CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote then
    for NoteIndex := 0 to CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote do
    begin
      if Interaction = InteractiveNoteId[NoteIndex] then
      begin
        if (SDL_GetTicks() - LastClickTime < 350) then
        begin
            CopyToUndo;
            GoldenRec.KillAll;
            DivideNote(true);
            ShowInteractiveBackground;
        end;

        // to check last click for divide note
        LastClickTime := SDL_GetTicks();

        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
        CurrentNote[CurrentTrack] := NoteIndex;
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
        EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
        //play current note playonewithmidi
        PlaySentenceMidi := false;
        {$IFDEF UseMIDIPort}
        StopMidi;
        {$ENDIF}
        midinotefound := false;
        PlayOne := true;
        PlayOneMidi := true;

        // playone
        PlayVideo := false;
        StopVideoPreview;
        Click := false;
        AudioPlayback.Stop;
        AudioPlayback.Position := GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
        PlayStopTime := GetTimeFromBeat(
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat +
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
        ConfigureMidiPlayback(AudioPlayback.Position, PlayStopTime);
        AudioPlayback.SetVolume(VolumeAudioIndex / 100);
        AudioPlayback.Play;

        LastClick := -100;
      end;
    end;
end;

      // SDLK_DELETE: DeleteNotes
procedure TScreenEditSub.DeleteNotes(SDL_ModState: word);
begin
  if SDL_ModState = KMOD_LCTRL then
  begin
    // deletes current note
    CopyToUndo;
    DeleteNote;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_DELETE_NOTE');
    GoldenRec.KillAll;
    ShowInteractiveBackground;
  end;

  if (SDL_ModState = KMOD_LCTRL or KMOD_LSHIFT) then
  begin
    // deletes current sentence
    CopyToUndo;
    DeleteSentence;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_DELETE_SENTENCE');
    GoldenRec.KillAll;
    ShowInteractiveBackground;
  end;
end;

      // SDLK_PERIOD: HandleMoveTextRight
procedure TScreenEditSub.HandleMoveTextRight(SDL_ModState: word);
begin
  // moves text to right in current sentence
  CopyToUndo;
  MoveTextToRight;
  Text[TextInfo].Text := Language.Translate('EDIT_INFO_MOVE_TEXT_RIGHT');
end;

      // SDLK_RIGHT: HandleMoveRight
procedure TScreenEditSub.HandleMoveRight(SDL_ModState: word);
begin
  // right
  if SDL_ModState = 0 then
  begin
    // clear debug text
    Text[TextInfo].Text := '';
    StopPlayback;
    PlaySentence := false;
    PlayOne := false;
    PlayVideo := false;
    {$IFDEF UseMIDIPort}
    //MidiOut.PutShort($B1, $7, Floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
    //MidiOut.PutShort($81, CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].Current].Notes[MidiLastNote].Tone + 60, 127);
    PlaySentenceMidi := false;
    StopMidi;
    {$endif}
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
    Inc(CurrentNote[CurrentTrack]);
    if CurrentNote[CurrentTrack] > CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote then
      CurrentNote[CurrentTrack] := 0;
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
    EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
  end;

  // ctrl + right
  if SDL_ModState = KMOD_LCTRL then
  begin
    CopyToUndo;
    if CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration > 1 then
    begin
      Dec(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
      Inc(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
      if CurrentNote[CurrentTrack] = 0 then
      begin
        Inc(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].StartBeat);
      end;
    end;
    TriggerGlobalPreviewRescore;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SHORTENED_AT_START');
    GoldenRec.KillAll;
  end;

  // shift + right
  if SDL_ModState = KMOD_LSHIFT then
  begin
    CopyToUndo;
    Inc(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
    if CurrentNote[CurrentTrack] = 0 then
    begin
      Inc(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].StartBeat);
    end;
    if CurrentNote[CurrentTrack] = CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote then
      Inc(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat);
    TriggerGlobalPreviewRescore;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SHIFTED_RIGHT');
    GoldenRec.KillAll;
  end;

  // alt + right
  if SDL_ModState = KMOD_LALT then
  begin
    CopyToUndo;
    Inc(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
    if CurrentNote[CurrentTrack] = CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote then
    begin
      Inc(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat);
    end;
    TriggerGlobalPreviewRescore;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_LENGTHENED_AT_END');
    GoldenRec.KillAll;
  end;

  // alt + ctrl + shift + right = move all from cursor to right
  if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then
  begin
    CopyToUndo;
    MoveAllToEnd(Floor(1+(RepeatCounter/20)));
    TriggerGlobalPreviewRescore;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTES_SHIFTED_RIGHT');
    GoldenRec.KillAll;
  end;
  ShowInteractiveBackground;
end;

      // SDLK_LEFT: HandleMoveLeft
procedure TScreenEditSub.HandleMoveLeft(SDL_ModState: word);
begin
  // left
  if SDL_ModState = 0 then
  begin
    // clear debug text
    Text[TextInfo].Text := '';
    StopPlayback;
    PlaySentence := false;
    PlayOne := false;
    PlayVideo := false;
    {$IFDEF UseMIDIPort}
    //MidiOut.PutShort($B1, $7, Floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
    //MidiOut.PutShort($81, CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].Current].Notes[MidiLastNote].Tone + 60, 127);
    PlaySentenceMidi := false;
    {$endif}

    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
    Dec(CurrentNote[CurrentTrack]);
    if CurrentNote[CurrentTrack] = -1 then
      CurrentNote[CurrentTrack] := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote;
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
    EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
  end;

  // ctrl + left
  if SDL_ModState = KMOD_LCTRL then
  begin
    CopyToUndo;
    Dec(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
    Inc(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
    if CurrentNote[CurrentTrack] = 0 then
      Dec(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].StartBeat);
    TriggerGlobalPreviewRescore;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_LENGTHENED_AT_START');
    GoldenRec.KillAll;
  end;

  // shift + left
  if SDL_ModState = KMOD_LSHIFT then
  begin
    CopyToUndo;
    Dec(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);

    // resizing sentences
    if CurrentNote[CurrentTrack] = 0 then
    begin
      Dec(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].StartBeat);
    end;

    if CurrentNote[CurrentTrack] = CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote then
      Dec(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat);
    TriggerGlobalPreviewRescore;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SHIFTED_LEFT');
    GoldenRec.KillAll;
  end;

  // alt + left
  if SDL_ModState = KMOD_LALT then
  begin
    CopyToUndo;
    if CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration > 1 then
    begin
      Dec(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
      if CurrentNote[CurrentTrack] = CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote then
      begin
        Dec(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat);
      end;
      TriggerGlobalPreviewRescore;
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_SHORTENED_AT_END');
    end;
    GoldenRec.KillAll;
  end;

  // alt + ctrl + shift + right = move all from cursor to left
  if SDL_ModState = KMOD_LALT + KMOD_LCTRL + KMOD_LSHIFT then
  begin
    CopyToUndo;
    MoveAllToEnd(-Floor(1+(RepeatCounter/20)));
    TriggerGlobalPreviewRescore;
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTES_SHIFTED_LEFT');
    GoldenRec.KillAll;
  end;
  ShowInteractiveBackground;
end;

      // SDLK_DOWN: HandleDownKey
procedure TScreenEditSub.HandleDownKey(SDL_ModState: word);
begin
  // skip to next sentence
  if SDL_ModState = 0 then
  begin
    // clear debug text
    Text[TextInfo].Text := '';
    NextSentence;
  end;

  // decrease tone
  if (SDL_ModState = KMOD_LSHIFT) or
     (not CurrentSong.isDuet and
      ((SDL_ModState = KMOD_LCTRL) or (SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT))) then
  begin
    CopyToUndo;
    TransposeNote(-1);
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_PITCH_DECREASED');
    GoldenRec.KillAll;
  end;

  // switch to second track, if possible
  if ((SDL_ModState = KMOD_LCTRL) or (SDL_ModState = KMOD_LALT)) and (CurrentSong.isDuet) then
  begin
    if (Length(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes) > 0) then
    begin
      CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 0;
      CurrentTrack := 1;
      EnsurePreviewScoreCache(CurrentTrack);
      if FPreviewBuildComplete and
         (not FAnalysisThreadRunning) and
         HasVocalsPreviewPlayback then
        StartAnalysisThread(false, CurrentTrack);
      GoldenRec.KillAll;
      CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
      EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
      EditorLyrics[CurrentTrack].Selected := 0;
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_SWITCHED_TO_TRACK') + ' 2 (' + CurrentSong.DuetNames[CurrentTrack] + ')';
    end;
  end;

  // copy line from first to second track
  if (CurrentSong.isDuet) and (CurrentTrack = 0) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) then
  begin
    CopyToUndo;
    if (DuetCopyLine) then
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_COPIED_TO_TRACK') + ' 2'
    else
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_NOT_COPIED');
  end;

  // move line from first to second track
  if (CurrentSong.isDuet) and (CurrentTrack = 0) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL or KMOD_LALT) then
  begin
    CopyToUndo;
    if (DuetMoveLine) then
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_MOVED_TO_TRACK') + ' 2'
    else
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_NOT_MOVED');
  end;

  ShowInteractiveBackground;
end;

      // SDLK_UP: HandleUpKey
procedure TScreenEditSub.HandleUpKey(SDL_ModState: word);
begin
  // skip to previous sentence
  if SDL_ModState = 0 then
  begin
    // clear debug text
    Text[TextInfo].Text := '';
    PreviousSentence;
  end;

  // increase tone
  if (SDL_ModState = KMOD_LSHIFT) or
     (not CurrentSong.isDuet and
      ((SDL_ModState = KMOD_LCTRL) or (SDL_ModState = KMOD_LCTRL + KMOD_LSHIFT))) then
  begin
    CopyToUndo;
    TransposeNote(1);
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_NOTE_PITCH_INCREASED');
    GoldenRec.KillAll;
  end;

  // switch to first track, if possible
  if ((SDL_ModState = KMOD_LCTRL) or (SDL_ModState = KMOD_LALT)) and (CurrentSong.isDuet) then
  begin
    if (Length(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes) > 0) then
    begin
      CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 0;
      CurrentTrack := 0;
      EnsurePreviewScoreCache(CurrentTrack);
      if FPreviewBuildComplete and
         (not FAnalysisThreadRunning) and
         HasVocalsPreviewPlayback then
        StartAnalysisThread(false, CurrentTrack);
      GoldenRec.KillAll;
      CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
      EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
      EditorLyrics[CurrentTrack].Selected := 0;
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_SWITCHED_TO_TRACK') + ' 1 (' + CurrentSong.DuetNames[CurrentTrack] + ')';
    end;
  end;

  // copy line from second to first track
  if (CurrentSong.isDuet) and (CurrentTrack = 1) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL) then
  begin
    CopyToUndo;
    if (DuetCopyLine) then
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_COPIED_TO_TRACK') + ' 1'
    else
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_NOT_COPIED');
  end;

  // move line from second to first track
  if (CurrentSong.isDuet) and (CurrentTrack = 1) and (SDL_ModState = KMOD_LSHIFT or KMOD_LCTRL or KMOD_LALT) then
  begin
    CopyToUndo;
    if (DuetMoveLine) then
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_MOVED_TO_TRACK') + ' 1'
    else
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_LINE_NOT_MOVED');
  end;

  ShowInteractiveBackground;
end;

function TScreenEditSub.ParseInputEditText(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean;
var
  SDL_ModState:  word;
begin
  // used when in Text Edit Mode
  Result := true;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT {+ KMOD_CAPS});

  if (PressedDown) then
  begin
    // check normal keys
    if (IsPrintableChar(CharCode)) then
    begin
      CurrentEditText :=
      UTF8Copy(CurrentEditText, 1,TextPosition) + UCS4ToUTF8String(CharCode) +
      UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText));
      Inc(editLengthText);
      Inc(TextPosition);

      if TextEditMode then
        begin
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text := CurrentEditText;
        EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
        EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
        end;
      Exit;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE:
        begin
          if TextEditMode then CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text := BackupEditText;
          if TitleEditMode then
          begin
            CurrentSong.Title := BackupEditText;
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if ArtistEditMode then
          begin
            CurrentSong.Artist := BackupEditText;
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if LanguageEditMode then
          begin
            CurrentSong.Language := ifthen(BackupEditText <> NOT_SET, BackupEditText, 'Unknown');
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if EditionEditMode then
          begin
            CurrentSong.Edition := ifthen(BackupEditText <> NOT_SET, BackupEditText, 'Unknown');
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if GenreEditMode then
          begin
            CurrentSong.Genre := ifthen(BackupEditText <> NOT_SET, BackupEditText, 'Unknown');
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if YearEditMode then
          begin
            if (TryStrToInt(BackupEditText, CurrentSong.Year)) then
              SelectsS[CurrentSlideId].TextOpt[0].Text := IntToStr(CurrentSong.Year)
            else
              SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if CreatorEditMode then
          begin
            CurrentSong.Creator := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if P1EditMode then
          begin
            CurrentSong.DuetNames[0] := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          if P2EditMode then
          begin
            CurrentSong.DuetNames[1] := ifthen(BackupEditText <> NOT_SET, BackupEditText, '');
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;
          EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
          EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
          TextEditMode := false;
          TitleEditMode := false;
          ArtistEditMode := false;
          LanguageEditMode := false;
          EditionEditMode := false;
          GenreEditMode := false;
          YearEditMode := false;
          CreatorEditMode := false;
          P1EditMode := false;
          P2EditMode := false;
          StopTextInput;
          editLengthText := 0;
          TextPosition := -1;
        end;
      SDLK_F4, SDLK_RETURN:
        begin
          // Exit Text Edit Mode
          CopyToUndo;
          if TitleEditMode then
          begin
            CurrentSong.Title := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            TitleVal[0] := CurrentSong.Title;
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(TitleSlideId,TitleVal,SlideTitleIndex);
            SelectsS[TitleSlideId].TextOpt[0].Align := 0;
            SelectsS[TitleSlideId].TextOpt[0].X := SelectsS[TitleSlideId].TextureSBG.X + 5;
          end;
          if ArtistEditMode then
          begin
            CurrentSong.Artist := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            ArtistVal[0] := CurrentSong.Artist;
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(ArtistSlideId,ArtistVal,SlideArtistIndex);
            SelectsS[ArtistSlideId].TextOpt[0].Align := 0;
            SelectsS[ArtistSlideId].TextOpt[0].X := SelectsS[ArtistSlideId].TextureSBG.X + 5;
          end;
          if LanguageEditMode then
          begin
            CurrentSong.Language := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            CurrentSong.Language := ifthen(CurrentSong.Language <> '', CurrentSong.Language, 'Unknown');
            LanguageVal[0] := ifthen(CurrentSong.Language <> 'Unknown', CurrentSong.Language, NOT_SET);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(LanguageSlideId,LanguageVal,SlideLanguageIndex);
            SelectsS[LanguageSlideId].TextOpt[0].Align := 0;
            SelectsS[LanguageSlideId].TextOpt[0].X := SelectsS[LanguageSlideId].TextureSBG.X + 5;
          end;
          if EditionEditMode then
          begin
            CurrentSong.Edition := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            CurrentSong.Edition := ifthen(CurrentSong.Edition <> '', CurrentSong.Edition, 'Unknown');
            EditionVal[0] := ifthen(CurrentSong.Edition <> 'Unknown', CurrentSong.Edition, NOT_SET);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(EditionSlideId,EditionVal,SlideEditionIndex);
            SelectsS[EditionSlideId].TextOpt[0].Align := 0;
            SelectsS[EditionSlideId].TextOpt[0].X := SelectsS[EditionSlideId].TextureSBG.X + 5;
          end;
          if GenreEditMode then
          begin
            CurrentSong.Genre := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            CurrentSong.Genre := ifthen(CurrentSong.Genre <> '', CurrentSong.Genre, 'Unknown');
            GenreVal[0] := ifthen(CurrentSong.Genre <> 'Unknown', CurrentSong.Genre, NOT_SET);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(GenreSlideId,GenreVal,SlideGenreIndex);
            SelectsS[GenreSlideId].TextOpt[0].Align := 0;
            SelectsS[GenreSlideId].TextOpt[0].X := SelectsS[GenreSlideId].TextureSBG.X + 5;
          end;
          if YearEditMode then
          begin
            if (TryStrToInt(UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition), CurrentSong.Year)) and (CurrentSong.Year <= 2100) and (CurrentSong.Year >= 1900) then
            begin
              YearVal[0] := IntToStr(CurrentSong.Year);
              SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
              UpdateSelectSlideOptions(YearSlideId,YearVal,SlideYearIndex);
              SelectsS[YearSlideId].TextOpt[0].Align := 0;
              SelectsS[YearSlideId].TextOpt[0].X := SelectsS[YearSlideId].TextureSBG.X + 5;
            end
            else
            begin
              CurrentSong.Year := 0;
              SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
            end;
          end;
          if CreatorEditMode then
          begin
            CurrentSong.Creator := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            CreatorVal[0] := ifthen(CurrentSong.Creator <> '', CurrentSong.Creator, NOT_SET);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(CreatorSlideId,CreatorVal,SlideCreatorIndex);
            SelectsS[CreatorSlideId].TextOpt[0].Align := 0;
            SelectsS[CreatorSlideId].TextOpt[0].X := SelectsS[CreatorSlideId].TextureSBG.X + 5;
          end;
          if P1EditMode then
          begin
            CurrentSong.DuetNames[0] := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            MedleyStartVal[0] := ifthen(CurrentSong.DuetNames[0] <> '', CurrentSong.DuetNames[0], NOT_SET);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(MedleyStartSlideId,MedleyStartVal,SlideMedleyStartIndex);
            SelectsS[MedleyStartSlideId].TextOpt[0].Align := 0;
            SelectsS[MedleyStartSlideId].TextOpt[0].X := SelectsS[MedleyStartSlideId].TextureSBG.X + 5;
          end;
          if P2EditMode then
          begin
            CurrentSong.DuetNames[1] := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            MedleyEndVal[0] := ifthen(CurrentSong.DuetNames[1] <> '', CurrentSong.DuetNames[1], NOT_SET);
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(MedleyEndSlideId,MedleyEndVal,SlideMedleyEndIndex);
            SelectsS[MedleyEndSlideId].TextOpt[0].Align := 0;
            SelectsS[MedleyEndSlideId].TextOpt[0].X := SelectsS[MedleyEndSlideId].TextureSBG.X + 5;
          end;
          if TextEditMode then
          begin
            CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
            LyricVal[0] := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
            SelectsS[CurrentSlideId].TextOpt[0].Text := CurrentEditText;
            UpdateSelectSlideOptions(LyricSlideId,LyricVal,SlideLyricIndex);
            SelectsS[LyricSlideId].TextOpt[0].Align := 0;
            SelectsS[LyricSlideId].TextOpt[0].X := SelectsS[LyricSlideId].TextureSBG.X + 5;
            EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
          end;
          EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
          TitleEditMode := false;
          ArtistEditMode := false;
          LanguageEditMode := false;
          EditionEditMode := false;
          GenreEditMode := false;
          YearEditMode := false;
          CreatorEditMode := false;
          P1EditMode := false;
          P2EditMode := false;
          TextEditMode := false;
          StopTextInput;
          editLengthText := 0;
          TextPosition := -1;
          CurrentSlideId := -1;
        end;
      SDLK_BACKSPACE:
        begin
          if (TextPosition > 0) then
          begin
            UTF8Delete(CurrentEditText, TextPosition, 1);
            dec(TextPosition);
            dec(editLengthText);
            if TextEditMode then
            begin
              CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
              EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
            end;
          end;
        end;
      SDLK_DELETE:
        begin
          if (TextPosition >= 0) and (TextPosition < editLengthText) then
          begin
            UTF8Delete(CurrentEditText, TextPosition+1, 1);
            dec(editLengthText);
            if TextEditMode then
            begin
              CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text := UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
              EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
              EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
            end;
          end;
        end;

      SDLK_RIGHT:
        begin
          // right
          if SDL_ModState = 0 then
          begin
            if (TextPosition >= 0) and (TextPosition < editLengthText-1) then
                TextPosition := TextPosition + 1
            else
            begin
              if TextEditMode then
              begin
                CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
                Inc(CurrentNote[CurrentTrack]);
                if CurrentNote[CurrentTrack] > CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote then
                  CurrentNote[CurrentTrack] := 0;
                CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
                CurrentEditText := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
                BackupEditText := CurrentEditText;
                editLengthText := LengthUTF8(CurrentEditText);
                TextPosition := 0;
                EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
                EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
                ShowInteractiveBackground;
              end
              else
                TextPosition := 0;
            end;
          end;
        end;
      SDLK_LEFT:
        begin
          // left
          if SDL_ModState = 0 then
          begin
            if TextPosition > 0 then
                TextPosition := TextPosition - 1
            else
            begin
              if TextEditMode then
              begin
                CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
                Dec(CurrentNote[CurrentTrack]);
                if CurrentNote[CurrentTrack] < 0 then
                  CurrentNote[CurrentTrack] := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote;
                CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
                CurrentEditText := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
                BackupEditText := CurrentEditText;
                editLengthText := LengthUTF8(CurrentEditText);
                TextPosition := editLengthText;
                EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
                EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
                ShowInteractiveBackground;
              end
              else
                TextPosition := editLengthText-1;
            end;
          end;
        end;
      // SDLK_KP_DIVIDE is a temporary workaround for German keyboards
      SDLK_SLASH, SDLK_KP_DIVIDE:
        begin
          CopyToUndo;
          if SDL_ModState = KMOD_LCTRL then
          begin
            // divide note
            DivideNote(false);
            TextEditMode := false;
            StopTextInput;
            EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
            EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
            GoldenRec.KillAll;
          end;
          ShowInteractiveBackground;
        end;

    end; //case
  end; //if (PressedDown)
end;

function TScreenEditSub.ParseInputEditBPM(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean;
var
  SDL_ModState:  word;
  qBPM:          real;
  NewBPM:        real;
begin
  // used when in Text Edit Mode
  Result := true;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT {+ KMOD_CAPS});

  if (PressedDown) then
  begin
    // only allow digits and decimal separators
    if (CharCode in [48..57, 46]) then    // 48..57 = 0-9, 44 = decimal comma, 46 = decimal point
    begin
      CurrentEditText := UTF8Copy(CurrentEditText, 1,TextPosition) + UCS4ToUTF8String(CharCode) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText));
      Inc(editLengthText);
      Inc(TextPosition);

      Exit;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE:
        begin
          // exit BPM edit mode, restore previous BPM value
          SelectsS[CurrentSlideId].TextOpt[0].Text := FloatToStr(CurrentSong.BPM[0].BPM / 4);
          BPMEditMode := false;
          StopTextInput;
          editLengthText := 0;
          TextPosition := -1;
        end;
      SDLK_F5, SDLK_RETURN:
        begin
          // Exit BPM Edit Mode
          //CopyToUndo;
          if (TryStrToFloat(UTF8Copy(CurrentEditText, 1, TextPosition) + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition), qBPM)) then
          begin
            NewBPM := qBPM * 4;
            if NewBPM <= 0 then
              NewBPM := MIN_EDITOR_BPM;

            BPMVal[0] := FloatToStr(NewBPM / 4);
            ChangeBPM(NewBPM);
            SelectsS[CurrentSlideId].TextOpt[0].Text := BPMVal[0];
            UpdateSelectSlideOptions(BPMSlideId,BPMVal,SlideBPMIndex);
            SelectsS[BPMSlideId].TextOpt[0].Align := 0;
            SelectsS[BPMSlideId].TextOpt[0].X := SelectsS[BPMSlideId].TextureSBG.X + 5;
          end
          else
          begin
            CurrentSong.BPM[0].BPM := 0;
            SelectsS[CurrentSlideId].TextOpt[0].Text := BackupEditText;
          end;

          BPMEditMode := false;
          StopTextInput;
          editLengthText := 0;
          TextPosition := -1;
          CurrentSlideId := -1;
        end;

      SDLK_BACKSPACE:
        begin
          if (TextPosition > 0) then
          begin
            UTF8Delete(CurrentEditText, TextPosition, 1);
            dec(TextPosition);
          end;
        end;

      SDLK_DELETE:
        begin
            UTF8Delete(CurrentEditText, TextPosition+1, 1);
        end;

      SDLK_RIGHT:
        begin
          // right
          if SDL_ModState = 0 then
          begin
            if (TextPosition >= 0) and (TextPosition < editLengthText-1) then
                TextPosition := TextPosition + 1
            else
              begin
              // todo change to next note
              TextPosition := 0;
              end;
          end;
        end;

      SDLK_LEFT:
        begin
          // left
          if SDL_ModState = 0 then
          begin
            if TextPosition > 0 then
                TextPosition := TextPosition - 1
            else
              begin
                TextPosition := editLengthText-1;
              end;
          end;
        end;

    end; //case
  end; //if (PressedDown)
end;

procedure TScreenEditSub.ApplyTone(NewTone: Integer);
begin
  CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone := NewTone;

  PlaySentenceMidi := False;
  PlayVideo        := False;
  midinotefound    := False;
  PlayOne          := True;
  PlayOneMidi      := True;
  Click            := False;
  AudioPlayback.Stop;
  StopVideoPreview;
  AudioPlayback.Position := GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
  PlayStopTime := (GetTimeFromBeat(
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat +
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration));
  {$IFDEF UseMIDIPort}
  ConfigureMidiPlayback(AudioPlayback.Position, PlayStopTime);
  {$ENDIF}
  AudioPlayback.SetVolume(VolumeAudioIndex / 100);
  AudioPlayback.Play;
  LastClick := -100;
end;

procedure TScreenEditSub.OnMidiNote(Note: Byte);
begin
  ApplyTone(Note - 48);
  // Play current note
end;

function TScreenEditSub.ParseInputEditPiano(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean;
var
  SDL_ModState:  word;
  Shift: Integer;
  NewNote: Integer;
  i: Integer;
begin
  // used when in Piano Edit Mode
  Result := true;
  NewNote := -1000;
  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT {+ KMOD_CAPS});

  Shift := 0;
  if SDL_ModState = KMOD_LSHIFT then
  begin
    Shift := 12;
  end;


  if PressedDown then
  begin
    // check special keys
    case PressedKey of
      SDLK_ESCAPE, SDLK_F6:
        begin
          PianoEditMode := false;
        end;
    end;
    if PianoEditMode = true then
    begin
      for i := Low(PianoKeysLow) to High(PianoKeysLow) do
      begin
        if PressedKey = PianoKeysLow[i] then
        begin
          NewNote := i - 7 + Shift;
          Break;
        end;
      end;
      if NewNote = -1000 then // If not found in PianoKeysLow, check PianoKeysHigh
      begin
        for i := Low(PianoKeysHigh) to High(PianoKeysHigh) do
        begin
          if PressedKey = PianoKeysHigh[i] then
          begin
            NewNote := i + 6 + Shift;
            Break;
          end;
        end;
      end;

      if NewNote <> -1000 then
        ApplyTone(NewNote)
      else
        Result := False;
    end; //if (PianoEditMode)
  end; //if (PressedDown)
end;

function TScreenEditSub.ParseMouse(MouseButton: Integer; BtnDown: boolean; X, Y: Integer): boolean;
var
  nBut:   Integer;
  Action: TMouseClickAction;
  TempR:  real;
  i:      Integer;
  ClickedNoteIndex: Integer;
  HandledNoteClick: Boolean;
  PrevAudioIndex: Integer;
  PrevClickIndex: Integer;
  CursorWordIndex: Integer;
  CursorCharIndex: Integer;
  procedure HandleNoteClick(NoteIndex: Integer);
  begin
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
    CurrentNote[CurrentTrack] := NoteIndex;
    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
    EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];

    PlaySentenceMidi := false;
    {$IFDEF UseMIDIPort}
    StopMidi;
    {$ENDIF}
    midinotefound := false;
    PlayOne := true;
    PlayOneMidi := true;

    PlayVideo := false;
    StopVideoPreview;
    Click := false;
    AudioPlayback.Stop;
    AudioPlayback.Position := GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
    PlayStopTime := GetTimeFromBeat(
      CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat +
      CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
    ConfigureMidiPlayback(AudioPlayback.Position, PlayStopTime);
    AudioPlayback.SetVolume(VolumeAudioIndex / 100);
    AudioPlayback.Play;
    LastClick := -100;
  end;
  function FindInteractiveNoteAt(MouseX, MouseY: Integer): Integer;
  var
    NoteIdx: Integer;
    InteractionIdx: Integer;
    ButtonIdx: Integer;
    ButtonX: real;
    ButtonY: real;
    ButtonW: real;
    ButtonH: real;
  begin
    Result := -1;
    for NoteIdx := High(InteractiveNoteId) downto 0 do
    begin
      InteractionIdx := InteractiveNoteId[NoteIdx];
      ButtonIdx := -1;
      if (InteractionIdx >= Low(Interactions)) and (InteractionIdx <= High(Interactions)) and
         (Interactions[InteractionIdx].Typ = iButton) then
      begin
        ButtonIdx := Interactions[InteractionIdx].Num;
        ButtonX := Button[ButtonIdx].X;
        ButtonY := Button[ButtonIdx].Y;
        ButtonW := Button[ButtonIdx].W;
        ButtonH := Button[ButtonIdx].H;
        if Button[ButtonIdx].Visible and
           Button[ButtonIdx].Selectable and
           (ButtonW > 0) and (ButtonH > 0) and
           (MouseX >= ButtonX) and (MouseX <= ButtonX + ButtonW) and
           (MouseY >= ButtonY) and (MouseY <= ButtonY + ButtonH) and
           ((not Button[ButtonIdx].Scrollable) or InRegion(MouseX, MouseY, ScrollArea)) then
        begin
          Result := NoteIdx;
          Exit;
        end;
      end;
    end;
  end;
begin
  // transfer mousecords to the 800x600 raster we use to draw
  X := Round((X / (ScreenW / Screens)) * RenderW);
  if (X > RenderW) then
    X := X - RenderW;
  Y := Round((Y / ScreenH) * RenderH);

  CurrentX := X;
  CurrentY := Y;

  PrevAudioIndex := VolumeAudioIndex;
  PrevClickIndex := VolumeClickIndex;

  Result := true;
  ClickedNoteIndex := -1;
  HandledNoteClick := false;
  ClickedNoteIndex := FindInteractiveNoteAt(X, Y);
  if ClickedNoteIndex >= 0 then
    nBut := InteractiveNoteId[ClickedNoteIndex]
  else
    nBut := InteractAt(X, Y);
  Action := maNone;

  if BtnDown and (MouseButton = SDL_BUTTON_LEFT) and (ClickedNoteIndex < 0) and (nBut = -1) then
  begin
    if EditorLyrics[CurrentTrack].GetCursorFromPoint(CurrentX, CurrentY, CursorWordIndex, CursorCharIndex) then
    begin
      CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
      CurrentNote[CurrentTrack] := CursorWordIndex;
      CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
      EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];

      TitleEditMode := false;
      ArtistEditMode := false;
      LanguageEditMode := false;
      EditionEditMode := false;
      GenreEditMode := false;
      YearEditMode := false;
      CreatorEditMode := false;
      P1EditMode := false;
      P2EditMode := false;
      BPMEditMode := false;

      BackupEditText := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
      CurrentEditText := BackupEditText;
      editLengthText := LengthUTF8(CurrentEditText);
      TextPosition := EnsureRange(CursorCharIndex, 0, editLengthText);
      CurrentSlideId := LyricSlideId;
      TextEditMode := true;
      StartTextInput;
      ShowInteractiveBackground;
      Exit;
    end;
  end;

  if nBut >= 0 then
  begin
    //select on mouse-over
    if nBut <> Interaction then
      SetInteraction(nBut);

  end;
  if not BtnDown then
  begin
    PressedNoteId := -1;
    Xmouse := 0;
  end;

  if (nBut > -1) then
  begin
  if (BtnDown) then
  begin
    if (MouseButton = SDL_BUTTON_LEFT) and (ClickedNoteIndex >= 0) then
    begin
      if (ClickedNoteIndex = CurrentNote[CurrentTrack]) and (SDL_GetTicks() - LastClickTime < 350) then
      begin
        CopyToUndo;
        GoldenRec.KillAll;
        DivideNote(true);
        ShowInteractiveBackground;
        LastClickTime := 0;
        Exit;
      end;

      HandleNoteClick(ClickedNoteIndex);
      LastClickTime := SDL_GetTicks();
      HandledNoteClick := true;
    end;

    if (MouseButton = SDL_BUTTON_RIGHT) or (MouseButton = SDL_BUTTON_LEFT) then
    begin
      LastPressedMouseType := MouseButton;
      LastX := CurrentX;
      LastY := CurrentY;

      MoveNote := true;
      ResizeNoteLeft := false;
      ResizeNoteRight := false;
      // check current mouse position to resize note - 20% of left or right note to resize
      if (Interactions[nBut].Typ = iButton) then
      begin
        if CurrentX < Button[Interactions[nBut].Num].X + Button[Interactions[nBut].Num].W * 0.2 then
        begin
          // selected left side note - 20%
          ResizeNoteLeft := true;
          ResizeNoteRight := false;
          MoveNote := false;
        end;

        if CurrentX > Button[Interactions[nBut].Num].X + Button[Interactions[nBut].Num].W - Button[Interactions[nBut].Num].W * 0.2 then
        begin
          // selected right side note - 20%
          ResizeNoteLeft := false;
          ResizeNoteRight := true;
          MoveNote := false;
        end;
        PressedNoteId :=  Interactions[nBut].Num;
      end;
    end;

    if (MouseButton = SDL_BUTTON_LEFT) then
    begin
      //click button or SelectS
      if (Interactions[nBut].Typ = iSelectS) then
      begin
        Action := SelectsS[Interactions[nBut].Num].OnClick(X, Y);
      end
      else if HandledNoteClick then
        Action := maNone
      else
        Action := maReturn;
    end;

    // move notes by mouse move (left-right)
    TempR := 720 / (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat - CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
    if (MouseButton = 0) and (LastPressedMouseType = SDL_BUTTON_RIGHT) and (PressedNoteId >=0) then
    begin
      // left & right
      if (Floor((CurrentX - 40) / TempR) > Floor((LastX - 40) / TempR)) or  (Floor((CurrentX - 40) / TempR) < Floor((LastX - 40) / TempR)) then
      begin
        CopyToUndo;
        i := Floor((currentx-40) / Floor(TempR)) - Floor((lastx - 40) / Floor(TempR));
        if MoveNote then
          MoveAllToEnd(i);
        if (ResizeNoteRight) and (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration + i > 0) then
        begin
          MoveAllToEnd(i);
          CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat - i;
          CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration + i;
        end;
        if (ResizeNoteLeft) and (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration - i > 0) then
        begin
          CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat + i;
          CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration - i;
          if CurrentNote[CurrentTrack] = 0 then
            CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].StartBeat := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].StartBeat - i;
        end;
        LastX := CurrentX;
        if MoveNote or ResizeNoteRight then
          TriggerGlobalPreviewRescore
        else
          TriggerGlobalPreviewRescore;
        GoldenRec.KillAll;
        ShowInteractiveBackground;
      end;
      // up & down
      if (CurrentY - LastY < -6) or (CurrentY - LastY > 6) then
      begin
        CopyToUndo;
        TransposeNote(-1* Floor((CurrentY-LastY) div (6)));
        LastY := CurrentY;
        GoldenRec.KillAll;
        ShowInteractiveBackground;
      end;
    end;

    //move one note by mouse move
    if (MouseButton = 0) and (LastPressedMouseType = SDL_BUTTON_LEFT) and (PressedNoteId >= 0) then
    begin
      if (Floor((CurrentX - 40) / TempR) > Floor((LastX - 40) / TempR)) or (Floor((CurrentX - 40) / TempR) < Floor((LastX - 40) / TempR)) then
      begin
        CopyToUndo;
        // move left & right
        i := Floor((currentx - 40) / Floor(TempR)) - Floor((lastx - 40) / Floor(TempR));
        if MoveNote then
        begin
            CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat +i;
            if CurrentNote[CurrentTrack] = 0 then
              CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].StartBeat := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].StartBeat - i;
            if CurrentNote[CurrentTrack] = CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote then
              CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat + i;
        end;
        // resize note
        if (ResizeNoteRight) and (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration + i > 0) then
        begin
          CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration + i;
          if CurrentNote[CurrentTrack] = CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote then
              CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat + i;
        end;
        if (ResizeNoteLeft) and (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration - i > 0) then
        begin
          CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat + i;
          CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration - i;
          if CurrentNote[CurrentTrack] = 0 then
              CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].StartBeat := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].StartBeat + i;
        end;

        LastX := CurrentX;
        TriggerGlobalPreviewRescore;
        GoldenRec.KillAll;
        ShowInteractiveBackground;
      end;

      // up & down
      if (CurrentY - LastY < -6) or (CurrentY - LastY > 6) then
      begin
        CopyToUndo;
        TransposeNote(-1* Floor((CurrentY-LastY) div (6)));
        LastY := CurrentY;
        GoldenRec.KillAll;
        ShowInteractiveBackground;
      end;
    end;

    // change to next sequence
    if (MouseButton = 0) and (LastPressedMouseType = SDL_BUTTON_LEFT) and (Interactions[nBut].Typ <> iButton) and (Interactions[nBut].Typ <> iText) and (Interactions[nBut].Typ <>  iSelectS) and (Interactions[nBut].Typ <> iBCollectionChild) and (PressedNoteId = -1) then
    begin
      if CurrentX - LastX > 120 then
      begin
        PreviousSentence;
        LastX := CurrentX;
        showInteractiveBackground;
      end;
      if CurrentX - LastX < -120 then
      begin
        NextSentence;
        LastX := CurrentX;
        showInteractiveBackground;
      end;
      if (CurrentX - LastX < 120) and (CurrentX - LastX > -120) then
        Xmouse := CurrentX - LastX;
    end;
  end;

  // changed cover
  if ((CoverSlideId = Interactions[nBut].Num) and (Action = maLeft) and (SelectsS[Interactions[nBut].Num].SelectedOption > 0)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption -1;
    CurrentSong.Cover := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
  end;

  if ((CoverSlideId = Interactions[nBut].Num) and (Action = maRight) and (SelectsS[Interactions[nBut].Num].SelectedOption < Length(SelectsS[Interactions[nBut].Num].TextOptT)-1)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption +1;
    CurrentSong.Cover := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
  end;

  // changed background picture
  if ((BackgroundSlideId = Interactions[nBut].Num) and (Action = maLeft) and (SelectsS[Interactions[nBut].Num].SelectedOption > 0)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption -1;
    CurrentSong.Background := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
    Tex_PrevBackground := Texture.LoadTexture(CurrentSong.Path.Append(CurrentSong.Background));
    Texture.AddTexture(Tex_PrevBackground, TEXTURE_TYPE_PLAIN, false);
    Statics[BackgroundImageId].Texture := Tex_PrevBackground;
    Statics[BackgroundImageId].Texture.X := theme.EditSub.BackgroundImage.X;
    Statics[BackgroundImageId].Texture.Y := theme.EditSub.BackgroundImage.Y;
    Statics[BackgroundImageId].Texture.W := theme.EditSub.BackgroundImage.W;
    Statics[BackgroundImageId].Texture.H := theme.EditSub.BackgroundImage.H;
    Statics[BackgroundImageId].Visible := false;
  end;

  if ((BackgroundSlideId = Interactions[nBut].Num) and (Action = maRight) and (SelectsS[Interactions[nBut].Num].SelectedOption < Length(SelectsS[Interactions[nBut].Num].TextOptT)-1)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption +1;
    CurrentSong.Background := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
    Tex_PrevBackground := Texture.LoadTexture(CurrentSong.Path.Append(CurrentSong.Background));
    Texture.AddTexture(Tex_PrevBackground, TEXTURE_TYPE_PLAIN, false);
    Statics[BackgroundImageId].Texture := Tex_PrevBackground;
    Statics[BackgroundImageId].Texture.X := theme.EditSub.BackgroundImage.X;
    Statics[BackgroundImageId].Texture.Y := theme.EditSub.BackgroundImage.Y;
    Statics[BackgroundImageId].Texture.W := theme.EditSub.BackgroundImage.W;
    Statics[BackgroundImageId].Texture.H := theme.EditSub.BackgroundImage.H;
    Statics[BackgroundImageId].Visible := false;
  end;

  // changed video
  if ((VideoSlideId = Interactions[nBut].Num) and (Action = maLeft) and (SelectsS[Interactions[nBut].Num].SelectedOption > 0)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption -1;
    CurrentSong.Video := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
  end;

  if ((VideoSlideId = Interactions[nBut].Num) and (Action = maRight) and (SelectsS[Interactions[nBut].Num].SelectedOption < Length(SelectsS[Interactions[nBut].Num].TextOptT)-1)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption +1;
    CurrentSong.Video := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
  end;

  if ((Mp3SlideId = Interactions[nBut].Num) and (Action = maLeft) and (SelectsS[Interactions[nBut].Num].SelectedOption > 0)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption -1;
    CurrentSong.Audio := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
    AudioPlayback.Close;
    AudioPlayback.Open(CurrentSong.Path.Append(CurrentSong.Audio),nil);
  end;

  if ((Mp3SlideId = Interactions[nBut].Num) and (Action = maRight) and (SelectsS[Interactions[nBut].Num].SelectedOption < Length(SelectsS[Interactions[nBut].Num].TextOptT)-1)) then
  begin
    CopyToUndo;
    SelectsS[Interactions[nBut].Num].SelectedOption := SelectsS[Interactions[nBut].Num].SelectedOption +1;
    CurrentSong.Audio := Path(SelectsS[Interactions[nBut].Num].TextOptT[SelectsS[Interactions[nBut].Num].SelectedOption]);
    AudioPlayback.Close();
    AudioPlayback.Open(CurrentSong.Path.Append(CurrentSong.Audio),nil);
  end;

  if (((VolumeAudioSlideId = Interactions[nBut].Num) or (VolumeMidiSlideId = Interactions[nBut].Num) or (VolumeClickSlideId = Interactions[nBut].Num)
    or (PrerollAudioSlideId = Interactions[nBut].Num) or (PrerollMidiSlideId = Interactions[nBut].Num))
    and (Action = maLeft) and (SelectsS[Interactions[nBut].Num].SelectedOption > 0)) then
  begin
    SelectsS[Interactions[nBut].Num].SelectedOption := ((SelectsS[Interactions[nBut].Num].SelectedOption div 10) - 1) * 10;
  end;

  if (((VolumeAudioSlideId = Interactions[nBut].Num) or (VolumeMidiSlideId = Interactions[nBut].Num) or (VolumeClickSlideId = Interactions[nBut].Num)
    or (PrerollAudioSlideId = Interactions[nBut].Num) or (PrerollMidiSlideId = Interactions[nBut].Num))
    and (Action = maRight) and (SelectsS[Interactions[nBut].Num].SelectedOption < Length(SelectsS[Interactions[nBut].Num].TextOptT)-1)) then
  begin
    SelectsS[Interactions[nBut].Num].SelectedOption := ((SelectsS[Interactions[nBut].Num].SelectedOption div 10) + 1) * 10;
  end;

  if (VolumeAudioSlideId = Interactions[nBut].Num) and (VolumeAudioIndex <> PrevAudioIndex) then
    SetAudioVolumePercent(EnsureRange(VolumeAudioIndex, 0, 100));

  if (VolumeClickSlideId = Interactions[nBut].Num) and (VolumeClickIndex <> PrevClickIndex) then
    SetSfxVolumePercent(EnsureRange(VolumeClickIndex, 0, 100));
  end
  else if (MouseButton = SDL_BUTTON_RIGHT) then
  begin
    if length(UndoLines) > 0 then
    begin
      ScreenPopupcheck.ShowPopup(Language.Translate('EDIT_INFO_EXIT'), OnExit, nil, false);
    end
    else
    begin
      FadeTo(@ScreenSong);
    end;
  end;

  case Action of
    maReturn: Result := ParseInput(SDLK_RETURN, 0, true);
    //maLeft:   Result := ParseInput(SDLK_LEFT, 0, true);
    //maRight:  Result := ParseInput(SDLK_RIGHT, 0, true);
  end;
end;

{
procedure TScreenEditSub.NewBeat;
begin
  // click
  for NoteIndex := 0 to CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].Current].HighNut do
    if (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].Current].Notes[NoteIndex].Start = Czas.CurrentBeat) then
      Music.PlayClick;
end;
}

procedure TScreenEditSub.ChangeBPM(newBPM: real);
var
  TrackIndex: Integer;
  LineIndex:  Integer;
  NoteIndex:  Integer;
  factor:     real;

begin
  if CurrentSong.BPM <= 0 then
    CurrentSong.BPM := MIN_EDITOR_BPM;

  if newBPM <= 0 then
    newBPM := MIN_EDITOR_BPM;

  factor := newBPM / CurrentSong.BPM;    // e.g. new/old => 1/2 = 0.5 => * 0.5
  CurrentSong.BPM := newBPM;

  for TrackIndex := 0 to High(CurrentSong.Tracks) do
  begin
    for LineIndex := 0 to CurrentSong.Tracks[TrackIndex].High do
    begin
      CurrentSong.Tracks[TrackIndex].Lines[LineIndex].StartBeat := ceil(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].StartBeat * factor);
      //CurrentSong.Tracks[TrackIndex].Lines[LineIndex].EndBeat   := ceil(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].EndBeat * factor);
      if (Length(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes) > 0) then
      begin
        for NoteIndex := 0 to CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote do
        begin
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat := ceil(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat * factor);
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration  := floor(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration * factor);
          if (CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration = 0) then
            CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration := 1;
        end; // NoteIndex
        CurrentSong.Tracks[TrackIndex].Lines[LineIndex].EndBeat := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote].StartBeat + CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote].Duration;
      end else
        CurrentSong.Tracks[TrackIndex].Lines[LineIndex].EndBeat := round(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].EndBeat * factor)
    end; // LineIndex
  end; // TrackIndex

  TriggerGlobalPreviewRescore;
end;

procedure TScreenEditSub.ResetBeatTracking;
begin
  LastClickValid := false;
  LastClick := 0;
  {$IFDEF UseMIDIPort}
  LastMidiBeatValid := false;
  LastMidiBeat := 0;
  MidiLastNote := -1;
  MidiStopBeat := Low(Integer);
  {$ENDIF}
end;

procedure TScreenEditSub.ConfigureMidiPlayback(const StartTime, StopTime: real);
begin
  {$IFDEF UseMIDIPort}
  MidiStart := StartTime;
  MidiStop  := StopTime;
  MidiAnchorPos := GetPlaybackPosition;
  LastMidiBeatValid := false;
  MidiLastNote := -1;
  {$ENDIF}
end;

{$IFDEF UseMIDIPort}
function TScreenEditSub.GetMidiVolume(): Integer;
begin
  if (VolumeMidiSlideId >= 0) and (VolumeMidiSlideId < Length(SelectsS)) then
    Result := Floor(1.27 * SelectsS[VolumeMidiSlideId].SelectedOption)
  else
    Result := 0;
end;

procedure TScreenEditSub.PlayMidiTone(const Tone: Integer);
var
  MidiNote: Integer;
begin
  MidiNote := EnsureRange(Tone + 60, 0, 127);

  if MidiActive then
    MidiOut.PutShort(MIDI_NOTEOFF or 1, MidiTone + 60, 0);

  MidiOut.PutShort($B1, $7, GetMidiVolume());
  MidiOut.PutShort($91, MidiNote, 127);
  MidiTone := MidiNote - 60;
  MidiActive := true;
end;

procedure TScreenEditSub.StopMidi;
begin
  if MidiActive then
  begin
    MidiOut.PutShort(MIDI_NOTEOFF or 1, MidiTone + 60, 0);
    MidiOut.PutShort(MIDI_STOP, 0, 0);
  end;

  MidiActive := false;
  MidiTone := -1;
  MidiStopBeat := Low(Integer);
end;
{$ENDIF}

function TScreenEditSub.GetSentenceLeadSeconds(const UseMidi: boolean): real;
begin
  if UseMidi then
    Result := Ini.EditorMidiLeadMs / 1000
  else
    Result := Ini.EditorClickLeadMs / 1000;
end;

procedure TScreenEditSub.DivideBPM;
begin
  ChangeBPM(CurrentSong.BPM[0].BPM / 2);
end;

procedure TScreenEditSub.MultiplyBPM;
begin
  ChangeBPM(CurrentSong.BPM[0].BPM * 2);
end;

procedure TScreenEditSub.LyricsCapitalize;
var
  TrackIndex:   Integer;
  LineIndex:    Integer;
  Str:          UTF8String;
begin
  for TrackIndex := 0 to High(CurrentSong.Tracks) do
  begin
    for LineIndex := 0 to CurrentSong.Tracks[TrackIndex].High do
    begin
      Str := UTF8UpperCase(UTF8Copy(TrimLeft(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[0].Text), 1, 1));
      Str := Str + UTF8Copy(TrimLeft(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[0].Text), 2, Length(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[0].Text)-1);
      CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[0].Text := Str;
    end; // LineIndex
  end; // TrackIndex
end;

procedure TScreenEditSub.LyricsCorrectSpaces;
var
  TrackIndex:   Integer;
  LineIndex:    Integer;
  NoteIndex:    Integer;
begin
  for TrackIndex := 0 to High(CurrentSong.Tracks) do
  begin
    for LineIndex := 0 to CurrentSong.Tracks[TrackIndex].High do
    begin
      // correct starting spaces in the first word
      while Copy(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[0].Text, 1, 1) = ' ' do
        CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[0].Text := Copy(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[0].Text, 2, 100);

      // move spaces on the start to the end of the previous note
      for NoteIndex := 1 to CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote do
      begin
        while (Copy(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text, 1, 1) = ' ') do
        begin
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text := Copy(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text, 2, 100);
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex-1].Text := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex-1].Text + ' ';
        end;
      end; // NoteIndex

      // correct '-'  to '- '
      for NoteIndex := 0 to CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote do
      begin
        if CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text = '-' then
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text := '- ';
      end; // NoteIndex

      // add space to the previous note when the current word is '- '
      for NoteIndex := 1 to CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote do
      begin
        if CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text  = '- ' then
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex-1].Text := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex-1].Text + ' ';
      end; // NoteIndex

      // correct too many spaces at the end of note
      for NoteIndex := 0 to CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote do
      begin
        while Copy(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text, Length(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text)-1, 2) = '  ' do
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text := Copy(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text, 1, Length(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text)-1);
      end; // NoteIndex

      // and correct if there is no space at the end of sentence
      NoteIndex := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote;
      if Copy(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text, Length(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text), 1) <> ' ' then
        CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text + ' ';
    end; // LineIndex
  end; // TrackIndex
end;

procedure TScreenEditSub.FixTimings;
var
  TrackIndex:   Integer;
  LineIndex:    Integer;
  NoteIndex:    Integer;
  LineStart:    Integer;
  FirstBeat:    Integer;
  LastLineIndex:Integer;
  EndBeat:      Integer;
  GapBeats:     Integer;
  GapSeconds:   Double;
begin
  FirstBeat := High(Integer);

  for TrackIndex := 0 to High(CurrentSong.Tracks) do
    if (CurrentSong.Tracks[TrackIndex].Lines[0].Notes[0].StartBeat < FirstBeat) then
    FirstBeat := CurrentSong.Tracks[TrackIndex].Lines[0].Notes[0].StartBeat;

  // set first note to start at beat 0 (common practice)
  if (FirstBeat <> 0) then
  begin
    for TrackIndex := 0 to High(CurrentSong.Tracks) do
      for LineIndex := 0 to CurrentSong.Tracks[TrackIndex].High do
        for NoteIndex := 0 to CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote do
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat - FirstBeat;

    for TrackIndex := 0 to High(CurrentSong.Tracks) do
      for LineIndex := 0 to CurrentSong.Tracks[TrackIndex].High do
      begin
        CurrentSong.Tracks[TrackIndex].Lines[LineIndex].StartBeat := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].StartBeat - FirstBeat;
        CurrentSong.Tracks[TrackIndex].Lines[LineIndex].EndBeat   := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].EndBeat   - FirstBeat;
      end;

    // adjust GAP accordingly, round to nearest integer value (fractional GAPs make no sense)
    CurrentSong.GAP := round((CurrentSong.GAP + (FirstBeat * 15000) / (CurrentSong.BPM[0].BPM / 4)));

    // adjust medley tags accordingly
    if (MedleyNotes.isStart) then
      CurrentSong.Medley.StartBeat := CurrentSong.Medley.StartBeat - FirstBeat;
    if (MedleyNotes.isEnd) then
      CurrentSong.Medley.EndBeat := CurrentSong.Medley.EndBeat - FirstBeat;
  end;

  // adjust line break timings
  for TrackIndex := 0 to High(CurrentSong.Tracks) do
  begin
    LastLineIndex := -1;
    for LineIndex := 0 to CurrentSong.Tracks[TrackIndex].High do
    begin
      if (LastLineIndex <> -1) then
      begin
        EndBeat := CurrentSong.Tracks[TrackIndex].Lines[LastLineIndex].Notes[CurrentSong.Tracks[TrackIndex].Lines[LastLineIndex].HighNote].StartBeat +
                   CurrentSong.Tracks[TrackIndex].Lines[LastLineIndex].Notes[CurrentSong.Tracks[TrackIndex].Lines[LastLineIndex].HighNote].Duration;
        FirstBeat := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[0].StartBeat;
        GapBeats := FirstBeat - EndBeat;
        GapSeconds := GetTimeFromBeat(FirstBeat) - GetTimeFromBeat(EndBeat);

        if GapSeconds >= 4.0 then
          LineStart := EndBeat + Trunc(2.0 * CurrentSong.BPM[0].BPM / 60.0)
        else if GapSeconds >= 2.0 then
          LineStart := EndBeat + Trunc(1.0 * CurrentSong.BPM[0].BPM / 60.0)
        else if (GapBeats >= 0) and (GapBeats <= 1) then
          LineStart := EndBeat
        else if (GapBeats >= 2) and (GapBeats <= 8) then
          LineStart := FirstBeat - 2
        else if (GapBeats >= 9) and (GapBeats <= 12) then
          LineStart := FirstBeat - 3
        else if (GapBeats >= 13) and (GapBeats <= 16) then
          LineStart := FirstBeat - 4
        else if (GapBeats > 16) then
          LineStart := EndBeat + 10
        else
          LineStart := FirstBeat;

        CurrentSong.Tracks[TrackIndex].Lines[LineIndex].StartBeat := LineStart;
      end;

      LastLineIndex := LineIndex;
    end;
  end;
end;

procedure TScreenEditSub.DivideSentence;
var
  LineIndex:  Integer;
  LineStart:  Integer;
  LineNew:    Integer;
  LineLength: Integer;
  NoteIndex:  Integer;
  NoteStart:  Integer;
  NoteHigh:   Integer;
begin
  // increase sentence length by 1
  LineLength := Length(CurrentSong.Tracks[CurrentTrack].Lines);
  SetLength(CurrentSong.Tracks[CurrentTrack].Lines, LineLength + 1);
  Inc(CurrentSong.Tracks[CurrentTrack].Number);
  Inc(CurrentSong.Tracks[CurrentTrack].High);

  // move needed sentences to one forward. newly has the copy of divided sentence
  LineStart := CurrentSong.Tracks[CurrentTrack].CurrentLine;
  for LineIndex := LineLength-1 downto LineStart do
    CopyLine(CurrentTrack, LineIndex, CurrentTrack, LineIndex+1);
    //CurrentSong.Tracks[CurrentTrack].Lines[LineIndex+1] := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex];

  // clear and set new sentence
  LineNew := LineStart + 1;
  NoteStart := CurrentNote[CurrentTrack];
  CurrentSong.Tracks[CurrentTrack].Lines[LineNew].StartBeat := CurrentSong.Tracks[CurrentTrack].Lines[LineStart].Notes[NoteStart].StartBeat;
  CurrentSong.Tracks[CurrentTrack].Lines[LineNew].Lyric := '';
  CurrentSong.Tracks[CurrentTrack].Lines[LineNew].EndBeat := 0;
  CurrentSong.Tracks[CurrentTrack].Lines[LineNew].BaseNote := 0;//High(Integer); // TODO: High (Integer) will causes a memory exception later in this procedure. Weird!
  CurrentSong.Tracks[CurrentTrack].Lines[LineNew].HighNote := -1;
  SetLength(CurrentSong.Tracks[CurrentTrack].Lines[LineNew].Notes, 0);

  // move right notes to new sentences
  NoteHigh := CurrentSong.Tracks[CurrentTrack].Lines[LineStart].HighNote;
  for NoteIndex := NoteStart to NoteHigh do
  begin
    // increase sentence counters
    with CurrentSong.Tracks[CurrentTrack].Lines[LineNew] do
    begin
      Inc(HighNote);
      SetLength(Notes, HighNote + 1);
      Notes[HighNote] := CurrentSong.Tracks[CurrentTrack].Lines[LineStart].Notes[NoteIndex];
      EndBeat := Notes[HighNote].StartBeat + Notes[HighNote].Duration;
      
      if Notes[HighNote].Tone < BaseNote then
        BaseNote := Notes[HighNote].Tone;
    end;
  end;

  // clear old notes and set sentence counters
  CurrentSong.Tracks[CurrentTrack].Lines[LineStart].HighNote := NoteStart - 1;
  CurrentSong.Tracks[CurrentTrack].Lines[LineStart].EndBeat := CurrentSong.Tracks[CurrentTrack].Lines[LineStart].Notes[NoteStart-1].StartBeat +
    CurrentSong.Tracks[CurrentTrack].Lines[LineStart].Notes[NoteStart-1].Duration;
  SetLength(CurrentSong.Tracks[CurrentTrack].Lines[LineStart].Notes, CurrentSong.Tracks[CurrentTrack].Lines[LineStart].HighNote + 1);

  //cleanup of first note of new sentence: trim leading white space and capitalize
  CurrentSong.Tracks[CurrentTrack].Lines[LineNew].Notes[0].Text := TrimLeft(CurrentSong.Tracks[CurrentTrack].Lines[LineNew].Notes[0].Text);
  CurrentSong.Tracks[CurrentTrack].Lines[LineNew].Notes[0].Text := UTF8UpperCase(UTF8Copy(CurrentSong.Tracks[CurrentTrack].Lines[LineNew].Notes[0].Text, 1, 1)) + UTF8Copy(CurrentSong.Tracks[CurrentTrack].Lines[LineNew].Notes[0].Text, 2, Length(CurrentSong.Tracks[CurrentTrack].Lines[LineNew].Notes[0].Text) - 1);

  CurrentSong.Tracks[CurrentTrack].CurrentLine := CurrentSong.Tracks[CurrentTrack].CurrentLine + 1;
  CurrentNote[CurrentTrack] := 0;

  // adjust medley tags
  if (MedleyNotes.isStart) then
  begin
    if (MedleyNotes.start.line = LineStart) then
    begin
      if (MedleyNotes.start.note >= NoteStart) then
      begin
        Inc(MedleyNotes.start.line);
        MedleyNotes.start.note := MedleyNotes.start.note - NoteStart;
      end;
    end
    else if (MedleyNotes.start.line > LineStart) then
    begin
      Inc(MedleyNotes.start.line);
    end;
  end;

  if (MedleyNotes.isEnd) then
  begin
    if (MedleyNotes.end_.line = LineStart) then
    begin
      if (MedleyNotes.end_.note >= NoteStart) then
      begin
        Inc(MedleyNotes.end_.line);
        MedleyNotes.end_.note := MedleyNotes.end_.note - NoteStart;
      end;
    end
    else if (MedleyNotes.end_.line > LineStart) then
    begin
      Inc(MedleyNotes.end_.line);
    end;
  end;

  Refresh;
  CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end;

procedure TScreenEditSub.JoinSentence;
var
  LineIndex: Integer;
  NoteIndex: Integer;
  StartNote: Integer;
  DstNote:   Integer;
begin
  LineIndex := CurrentSong.Tracks[CurrentTrack].CurrentLine;

  // add space to last note's syllable
  CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote].Text := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote].Text + ' ';

  // increase TotalNotes, HighNote and number of Notes in current sentence
  StartNote := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote + 1;
  CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote + Length(CurrentSong.Tracks[CurrentTrack].Lines[LineIndex+1].Notes);
  SetLength(CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes, CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote + 1);

  // copy notes of subsequent sentence to the end of the current sentence
  for NoteIndex := 0 to CurrentSong.Tracks[CurrentTrack].Lines[LineIndex+1].HighNote do
  begin
    DstNote := StartNote + NoteIndex;
    CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[DstNote] := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex+1].Notes[NoteIndex];
  end;

  // adjust end beat of
  DstNote := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote;
  CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].EndBeat := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[DstNote].StartBeat +
    CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[DstNote].Duration;

  // move needed sentences to one backward.
  for LineIndex := CurrentSong.Tracks[CurrentTrack].CurrentLine + 1 to CurrentSong.Tracks[CurrentTrack].High - 1 do
    CopyLine(CurrentTrack, LineIndex+1, CurrentTrack, LineIndex);
    //CurrentSong.Tracks[CurrentTrack].Lines[LineIndex] := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex+1];

  // decrease sentence length by 1
  SetLength(CurrentSong.Tracks[CurrentTrack].Lines, Length(CurrentSong.Tracks[CurrentTrack].Lines) - 1);
  Dec(CurrentSong.Tracks[CurrentTrack].Number);
  Dec(CurrentSong.Tracks[CurrentTrack].High);

  // adjust medley tags
  if (MedleyNotes.isStart) then
  begin
    if (MedleyNotes.start.line = CurrentSong.Tracks[CurrentTrack].CurrentLine + 1) then
    begin
      Dec(MedleyNotes.start.line);
      MedleyNotes.start.note := StartNote + MedleyNotes.start.note;
    end
    else if (MedleyNotes.start.line > CurrentSong.Tracks[CurrentTrack].CurrentLine + 1) then
    begin
      Dec(MedleyNotes.start.line);
    end;
  end;

  if (MedleyNotes.isEnd) then
  begin
    if (MedleyNotes.end_.line = CurrentSong.Tracks[CurrentTrack].CurrentLine + 1) then
    begin
      Dec(MedleyNotes.end_.line);
      MedleyNotes.end_.note := StartNote + MedleyNotes.end_.note;
    end
    else if (MedleyNotes.end_.line > CurrentSong.Tracks[CurrentTrack].CurrentLine + 1) then
    begin
      Dec(MedleyNotes.end_.line);
    end;
  end;

  Refresh;
  CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
end;

procedure TScreenEditSub.NextSentence;
begin
  {$IFDEF UseMIDIPort}
  //MidiOut.PutShort($B1, $7, Floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
  //MidiOut.PutShort(MIDI_NOTEOFF or 1, CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].Current].Notes[MidiLastNote].Tone + 60, 127);
  PlaySentenceMidi := false;
  PlayOne := false;
  {$ENDIF}
  CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
  Inc(CurrentSong.Tracks[CurrentTrack].CurrentLine);
  CurrentNote[CurrentTrack] := 0;
  if CurrentSong.Tracks[CurrentTrack].CurrentLine > CurrentSong.Tracks[CurrentTrack].High then
    CurrentSong.Tracks[CurrentTrack].CurrentLine := 0;
  UpdateLineBaseNote(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
  CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;

  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := 0;
  StopPlayback;
  PlaySentence := false;
  PlayVideo := false;
  GoldenRec.KillAll;
end;

procedure TScreenEditSub.PreviousSentence;
begin
  StopPlayback;
  PlayVideo := false;
  PlaySentence := false;
  PlayOne := false;
  {$IFDEF UseMIDIPort}
  //MidiOut.PutShort(MIDI_NOTEOFF or 1, $7, Floor(1.27*SelectsS[VolumeMidiSlideId].SelectedOption));
  //MidiOut.PutShort($81, CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].Current].Notes[MidiLastNote].Tone + 60, 127);
  PlaySentenceMidi := false;
  {$endif}

  CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
  Dec(CurrentSong.Tracks[CurrentTrack].CurrentLine);
  CurrentNote[CurrentTrack] := 0;
  if CurrentSong.Tracks[CurrentTrack].CurrentLine = -1 then
    CurrentSong.Tracks[CurrentTrack].CurrentLine := CurrentSong.Tracks[CurrentTrack].High;
  UpdateLineBaseNote(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
  CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;

  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := 0;
  GoldenRec.KillAll;
end;

procedure TScreenEditSub.DivideNote(doubleclick: boolean);
var
  LineIndex:     Integer;
  NoteIndex:     Integer;
  CutPosition:   Integer;
  SpacePosition: Integer;
  FirstSpacePos: Integer;
  LeftWordCount: Integer;
  TotalWordCount: Integer;
  TempR:         real;
  NoteDuration:  Integer;
  SourceText:    UTF8String;
  SourceTextLen: Integer;
  SourceNoteText: UTF8String;
  ShouldInsertContinuation: Boolean;
  LeftPart:      UTF8String;
  RightPart:     UTF8String;

  function CountWords(const S: UTF8String): Integer;
  var
    i: Integer;
    InWord: Boolean;
  begin
    Result := 0;
    InWord := false;
    for i := 1 to Length(S) do
    begin
      if S[i] <> ' ' then
      begin
        if not InWord then
        begin
          Inc(Result);
          InWord := true;
        end;
      end
      else
        InWord := false;
    end;
  end;

  function IsVowel(const Ch: UTF8String): Boolean;
  begin
    Result :=
      (Ch = 'a') or (Ch = 'e') or (Ch = 'i') or (Ch = 'o') or (Ch = 'u') or (Ch = 'y') or
      (Ch = 'A') or (Ch = 'E') or (Ch = 'I') or (Ch = 'O') or (Ch = 'U') or (Ch = 'Y');
  end;

  function HasVowel(const S: UTF8String): Boolean;
  var
    Chars: UCS4String;
    i: Integer;
  begin
    Result := false;
    if S = '' then
      Exit;

    Chars := UTF8ToUCS4String(S);
    for i := 0 to High(Chars) do
    begin
      if IsVowel(UCS4ToUTF8String(Chars[i])) then
      begin
        Result := true;
        Exit;
      end;
    end;
  end;

  // Returns the first split position at a real word boundary:
  // previous char is non-space and there is at least one non-space char after.
  // Result uses the same 0-based position convention expected by UTF8Copy calls below.
  function FindFirstSplitSpacePos(const S: UTF8String): Integer;
  var
    Chars: UCS4String;
    i, j: Integer;
  begin
    Result := -1;
    if S = '' then
      Exit;

    Chars := UTF8ToUCS4String(S);
    for i := 0 to High(Chars) do
    begin
      if (UCS4ToUTF8String(Chars[i]) = ' ') and
         (i > 0) and
         (UCS4ToUTF8String(Chars[i - 1]) <> ' ') then
      begin
        for j := i + 1 to High(Chars) do
        begin
          if UCS4ToUTF8String(Chars[j]) <> ' ' then
          begin
            Result := i;
            Exit;
          end;
        end;
      end;
    end;
  end;
begin
  LineIndex := CurrentSong.Tracks[CurrentTrack].CurrentLine;
  NoteDuration := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[CurrentNote[CurrentTrack]].Duration;
  TempR := 720 / (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat - CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);

  if (doubleclick) and
     (CurrentNote[CurrentTrack] >= 0) and
     (CurrentNote[CurrentTrack] <= High(TransparentNoteButtonId)) then
      CutPosition := Round((currentX - Button[TransparentNoteButtonId[CurrentNote[CurrentTrack]]].X) / TempR)
  else
      CutPosition := NoteDuration div 2;

  if CutPosition < 1 then
    CutPosition := 1;
  if CutPosition >= NoteDuration then
    CutPosition := NoteDuration - 1;

  // Auto-splitting text at spaces should only adjust timing for non-mouse splits.
  if (not doubleclick) and (TextPosition < 0) then
  begin
    SourceNoteText := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[CurrentNote[CurrentTrack]].Text;
    FirstSpacePos := FindFirstSplitSpacePos(SourceNoteText);
    if (FirstSpacePos >= 0) then
    begin
      LeftWordCount := CountWords(UTF8Copy(SourceNoteText, 1, FirstSpacePos));
      TotalWordCount := CountWords(SourceNoteText);
      if (LeftWordCount > 0) and (TotalWordCount > LeftWordCount) then
        CutPosition := EnsureRange(Round(NoteDuration * LeftWordCount / TotalWordCount), 1, NoteDuration - 1);
    end;
  end;

  with CurrentSong.Tracks[CurrentTrack].Lines[LineIndex] do
  begin
    Inc(HighNote);
    SetLength(Notes, HighNote + 1);

    // we copy all notes including selected one
    for NoteIndex := HighNote downto CurrentNote[CurrentTrack]+1 do
    begin
      Notes[NoteIndex] := Notes[NoteIndex-1];
    end;

    // Notes[CurrentNote[CurrentTrack]] and Notes[CurrentNote[CurrentTrack] + 1] is identical at this point
    // modify first note
    Notes[CurrentNote[CurrentTrack]].Duration := CutPosition;

    // 2nd note
    Notes[CurrentNote[CurrentTrack]+1].StartBeat := Notes[CurrentNote[CurrentTrack]].StartBeat + Notes[CurrentNote[CurrentTrack]].Duration;
    Notes[CurrentNote[CurrentTrack]+1].Duration := Notes[CurrentNote[CurrentTrack]+1].Duration - Notes[CurrentNote[CurrentTrack]].Duration;

    // find first real word boundary (ignores leading spaces)
    SpacePosition := FindFirstSplitSpacePos(Notes[CurrentNote[CurrentTrack]].Text);
    if (TextPosition < 0) and (SpacePosition >= 0) then
    begin
      LeftPart := UTF8Copy(Notes[CurrentNote[CurrentTrack]].Text, 1, SpacePosition);
      RightPart := ' ' + UTF8Copy(Notes[CurrentNote[CurrentTrack]].Text, SpacePosition + 2, LengthUTF8(Notes[CurrentNote[CurrentTrack]].Text));
      ShouldInsertContinuation := not (HasVowel(LeftPart) and HasVowel(RightPart));
      Notes[CurrentNote[CurrentTrack]].Text := LeftPart;
      if ShouldInsertContinuation then
        Notes[CurrentNote[CurrentTrack]+1].Text := '~' + RightPart
      else
        Notes[CurrentNote[CurrentTrack]+1].Text := RightPart;
    end
    else
    if (TextPosition >= 0) and (TextPosition <= LengthUTF8(Notes[CurrentNote[CurrentTrack]].Text)) then
    begin
      SourceText := SelectsS[LyricSlideId].TextOpt[0].Text;
      SourceTextLen := LengthUTF8(SourceText);
      Notes[CurrentNote[CurrentTrack]+1].Text := UTF8Copy(SourceText, TextPosition + 2, SourceTextLen);
      Notes[CurrentNote[CurrentTrack]].Text := UTF8Copy(SourceText, 1, TextPosition);
      ShouldInsertContinuation := not (HasVowel(Notes[CurrentNote[CurrentTrack]].Text) and HasVowel(Notes[CurrentNote[CurrentTrack]+1].Text));

      // If splitting at/around a space, keep a continuation marker on its own space note.
      if (TextPosition < SourceTextLen) and
         (UTF8Copy(SourceText, TextPosition + 1, 1) = ' ') then
      begin
        if ShouldInsertContinuation then
          Notes[CurrentNote[CurrentTrack]+1].Text := '~ ' + Notes[CurrentNote[CurrentTrack]+1].Text
        else
          Notes[CurrentNote[CurrentTrack]+1].Text := ' ' + Notes[CurrentNote[CurrentTrack]+1].Text;
      end
      else if (TextPosition > 0) and
              (UTF8Copy(SourceText, TextPosition, 1) = ' ') then
      begin
        if (LengthUTF8(Notes[CurrentNote[CurrentTrack]].Text) > 0) then
          UTF8Delete(Notes[CurrentNote[CurrentTrack]].Text, LengthUTF8(Notes[CurrentNote[CurrentTrack]].Text), 1);
        if ShouldInsertContinuation then
          Notes[CurrentNote[CurrentTrack]+1].Text := '~ ' + UTF8Copy(SourceText, TextPosition + 1, SourceTextLen)
        else
          Notes[CurrentNote[CurrentTrack]+1].Text := ' ' + UTF8Copy(SourceText, TextPosition + 1, SourceTextLen);
      end
      else
      begin
        if ShouldInsertContinuation then
          Notes[CurrentNote[CurrentTrack]+1].Text := '~' + Notes[CurrentNote[CurrentTrack]+1].Text;
      end;

      SelectsS[LyricSlideId].TextOpt[0].Text := Notes[CurrentNote[CurrentTrack]].Text;
      TextPosition := -1;
    end
    else
      Notes[CurrentNote[CurrentTrack]+1].Text := '~';
    Notes[CurrentNote[CurrentTrack]+1].Color := 1;
  end;

  // update lyric display
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
  TriggerGlobalPreviewRescore;
end;

procedure TScreenEditSub.DeleteNote;
var
  CurrentLine: Integer;
  LineIndex:   Integer;
  NoteIndex:   Integer;
  DeletedText: UTF8String;
  PrevText:    UTF8String;
  NextText:    UTF8String;
begin
  CurrentLine := CurrentSong.Tracks[CurrentTrack].CurrentLine;

  //Do Not delete Last Note
  if (CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].HighNote > 0) then
  begin
    if (CurrentNote[CurrentTrack] > 0) and (CurrentNote[CurrentTrack] < CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].HighNote) then
    begin
      DeletedText := CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
      if Pos(' ', DeletedText) > 0 then
      begin
        PrevText := CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].Notes[CurrentNote[CurrentTrack]-1].Text;
        NextText := CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].Notes[CurrentNote[CurrentTrack]+1].Text;
        if not ((Length(PrevText) > 0) and (PrevText[Length(PrevText)] = ' ') or (Length(NextText) > 0) and (NextText[1] = ' ')) then
          CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].Notes[CurrentNote[CurrentTrack]-1].Text := PrevText + ' ';
      end;
    end;

    // we copy all notes from the next to the selected one
    for NoteIndex := CurrentNote[CurrentTrack]+1 to CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].HighNote do
    begin
      CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].Notes[NoteIndex-1] := CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].Notes[NoteIndex];
    end;
    
    Dec(CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].HighNote);

    SetLength(CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].Notes, CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].HighNote + 1);

    // last note was deleted
    if (CurrentNote[CurrentTrack] > CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].HighNote) then
    begin
      // select new last note
      CurrentNote[CurrentTrack] := CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].HighNote;

      // correct Line ending
      with CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine] do
        EndBeat := Notes[HighNote].StartBeat + Notes[HighNote].Duration;
    end;

    CurrentSong.Tracks[CurrentTrack].Lines[CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
  end
  // Last Note of current Sentence Deleted - > Delete Sentence
  // if there are more than two left
  else if (CurrentSong.Tracks[CurrentTrack].High > 1) then
  begin
    //Move all Sentences after the current to the Left
    for LineIndex := CurrentLine+1 to CurrentSong.Tracks[CurrentTrack].High do
      CopyLine(CurrentTrack, LineIndex, CurrentTrack, LineIndex-1);
      //CurrentSong.Tracks[CurrentTrack].Lines[LineIndex-1] := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex];

    //Delete Last Sentence
    SetLength(CurrentSong.Tracks[CurrentTrack].Lines, CurrentSong.Tracks[CurrentTrack].High);
    CurrentSong.Tracks[CurrentTrack].High := High(CurrentSong.Tracks[CurrentTrack].Lines);
    CurrentSong.Tracks[CurrentTrack].Number := Length(CurrentSong.Tracks[CurrentTrack].Lines);

    CurrentNote[CurrentTrack] := 0;
    if (CurrentLine > 0) then
      CurrentSong.Tracks[CurrentTrack].CurrentLine := CurrentLine - 1
    else
      CurrentSong.Tracks[CurrentTrack].CurrentLine := 0;

    CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
  end;

  // update lyric display
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
  TriggerGlobalPreviewRescore;
end;

procedure TScreenEditSub.DeleteSentence;
var
  CurrentLine: Integer;
  LineIndex:   Integer;

begin
  CurrentLine := CurrentSong.Tracks[CurrentTrack].CurrentLine;

  // move all sentences after the current to the Left
  for LineIndex := CurrentLine+1 to CurrentSong.Tracks[CurrentTrack].High do
    CopyLine(CurrentTrack, LineIndex, CurrentTrack, LineIndex-1);

  // delete last sentence
  SetLength(CurrentSong.Tracks[CurrentTrack].Lines, CurrentSong.Tracks[CurrentTrack].High);
  CurrentSong.Tracks[CurrentTrack].High := High(CurrentSong.Tracks[CurrentTrack].Lines);
  CurrentSong.Tracks[CurrentTrack].Number := Length(CurrentSong.Tracks[CurrentTrack].Lines);

  CurrentNote[CurrentTrack] := 0;
  if (CurrentLine > 0) then
    CurrentSong.Tracks[CurrentTrack].CurrentLine := CurrentLine - 1
  else
    CurrentSong.Tracks[CurrentTrack].CurrentLine := 0;

  Refresh;
  //SelectPrevNote();
  //SelectNextNote();
  CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
end;

procedure TScreenEditSub.TransposeNote(Transpose: Integer);
begin
  Inc(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone, Transpose);
  TriggerGlobalPreviewRescore;
end;

procedure TScreenEditSub.UpdateLineBaseNote(const TrackIndex, LineIndex: Integer);
var
  NoteIndex: Integer;
  MinTone: Integer;
  MaxTone: Integer;
begin
  if (TrackIndex < 0) or (TrackIndex > High(CurrentSong.Tracks)) then
    Exit;
  if (LineIndex < 0) or (LineIndex > High(CurrentSong.Tracks[TrackIndex].Lines)) then
    Exit;

  with CurrentSong.Tracks[TrackIndex].Lines[LineIndex] do
  begin
    if HighNote < 0 then
    begin
      BaseNote := 0;
      Exit;
    end;

    MinTone := Notes[0].Tone;
    MaxTone := Notes[0].Tone;
    for NoteIndex := 1 to HighNote do
    begin
      if Notes[NoteIndex].Tone < MinTone then
        MinTone := Notes[NoteIndex].Tone;
      if Notes[NoteIndex].Tone > MaxTone then
        MaxTone := Notes[NoteIndex].Tone;
    end;

    BaseNote := Round((MinTone + MaxTone) / 2) - 5;
  end;
end;

procedure TScreenEditSub.ChangeWholeTone(Tone: Integer);
var
  LineIndex: Integer;
  NoteIndex: Integer;
begin
  for LineIndex := 0 to CurrentSong.Tracks[CurrentTrack].High do
  begin
    CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].BaseNote := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].BaseNote + Tone;
    for NoteIndex := 0 to CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote do
      CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteIndex].Tone := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteIndex].Tone + Tone;
  end;
  TriggerGlobalPreviewRescore;
end;

procedure TScreenEditSub.MoveAllToEnd(Move: Integer);
var
  LineIndex: Integer;
  NoteIndex: Integer;
  NoteStart: Integer;
begin
  for LineIndex := CurrentSong.Tracks[CurrentTrack].CurrentLine to CurrentSong.Tracks[CurrentTrack].High do
  begin
    NoteStart := 0;
    if LineIndex = CurrentSong.Tracks[CurrentTrack].CurrentLine then
      NoteStart := CurrentNote[CurrentTrack];
    for NoteIndex := NoteStart to CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote do
    begin
      Inc(CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteIndex].StartBeat, Move); // move note start

      if NoteIndex = 0 then
      begin // fix beginning
        Inc(CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].StartBeat, Move);
      end;

      if NoteIndex = CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote then // fix ending
        Inc(CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].EndBeat, Move);

    end; // for NoteIndex
  end; // for LineIndex
end;

procedure TScreenEditSub.MoveTextToRight;
var
  LineIndex: Integer;
  NoteIndex: Integer;
  NoteHigh:  Integer;
begin
  LineIndex := CurrentSong.Tracks[CurrentTrack].CurrentLine;
  NoteHigh := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote;

  // last word
  CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteHigh].Text := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteHigh-1].Text + CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteHigh].Text;

  // other words
  for NoteIndex := NoteHigh - 1 downto CurrentNote[CurrentTrack] + 1 do
  begin
    CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteIndex].Text := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteIndex-1].Text;
  end; // for
  CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[CurrentNote[CurrentTrack]].Text := '- ';
end;

procedure TScreenEditSub.MarkCopySrc;
begin
  CopySrc.track := CurrentTrack;
  CopySrc.line  := CurrentSong.Tracks[CurrentTrack].CurrentLine;
  CopySrc.note  := CurrentNote[CurrentTrack];
  CopySrcValid  := true;
end;

function TScreenEditSub.HasCopySource: Boolean;
begin
  Result := CopySrcValid and
    (CopySrc.track >= 0) and (CopySrc.track <= High(CurrentSong.Tracks)) and
    (CopySrc.line >= 0) and (CopySrc.line <= High(CurrentSong.Tracks[CopySrc.track].Lines));
end;

procedure TScreenEditSub.CopySentence(SrcTrack, SrcLine, DstTrack, DstLine: Integer; CopyText, CopyNotes, EnforceSrcLength: boolean);
var
  SrcStartBeat: Integer;
  DstStartBeat: Integer;
  BeatDiff:     Integer;
  NoteIndex:    Integer;
begin
  SrcStartBeat := CurrentSong.Tracks[SrcTrack].Lines[SrcLine].Notes[0].StartBeat;
  DstStartBeat := CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes[0].StartBeat;
  BeatDiff := DstStartBeat - SrcStartBeat;

  if (EnforceSrcLength) then
  begin
    // copy src line as is (even if length of src and dst lines don't match)
    SetLength(CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes, Length(CurrentSong.Tracks[SrcTrack].Lines[SrcLine].Notes));
    CurrentSong.Tracks[DstTrack].Lines[DstLine].HighNote := CurrentSong.Tracks[SrcTrack].Lines[SrcLine].HighNote;
  end;

  // if length(src) and length(dst) differ, only copy as many notes as possible (ignoring the rest)
  for NoteIndex := 0 to Min(CurrentSong.Tracks[SrcTrack].Lines[SrcLine].HighNote, CurrentSong.Tracks[DstTrack].Lines[DstLine].HighNote) do
  begin
    if (CopyText) then // copy text
    begin
      CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].Text      := CurrentSong.Tracks[SrcTrack].Lines[SrcLine].Notes[NoteIndex].Text;
    end;
    if (CopyNotes) then // copy duration, tone, note type and (shifted) start beat
    begin
      CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].Duration  := CurrentSong.Tracks[SrcTrack].Lines[SrcLine].Notes[NoteIndex].Duration;
      CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].Tone      := CurrentSong.Tracks[SrcTrack].Lines[SrcLine].Notes[NoteIndex].Tone;
      CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].NoteType  := CurrentSong.Tracks[SrcTrack].Lines[SrcLine].Notes[NoteIndex].NoteType;
      CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].StartBeat := CurrentSong.Tracks[SrcTrack].Lines[SrcLine].Notes[NoteIndex].StartBeat + BeatDiff;
      CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].Color     := CurrentSong.Tracks[SrcTrack].Lines[SrcLine].Notes[NoteIndex].Color;
    end;
  end;

  NoteIndex := CurrentSong.Tracks[DstTrack].Lines[DstLine].HighNote;
  CurrentSong.Tracks[DstTrack].Lines[DstLine].EndBeat := CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].StartBeat + CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes[NoteIndex].Duration;

  Refresh;
  CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes[CurrentNote[DstTrack]].Color := P1_INVERTED;
  EditorLyrics[DstTrack].AddLine(DstTrack, CurrentSong.Tracks[DstTrack].CurrentLine);
end;

procedure TScreenEditSub.CopySentences(SrcTrack, SrcLine, DstTrack, DstLine, Num: Integer);
var
  LineIndex: Integer;
begin
  // create place for new sentences
  SetLength(CurrentSong.Tracks[CurrentTrack].Lines, CurrentSong.Tracks[CurrentTrack].Number + Num - 1);

  // moves sentences next to the destination
  for LineIndex := CurrentSong.Tracks[CurrentTrack].High downto DstLine + 1 do
  begin
    CurrentSong.Tracks[CurrentTrack].Lines[LineIndex + Num - 1] := CurrentSong.Tracks[CurrentTrack].Lines[LineIndex];
  end;

  // prepares new sentences: sets sentence start and create first note
  for LineIndex := 1 to Num-1 do
  begin
    CurrentSong.Tracks[CurrentTrack].Lines[DstLine + LineIndex].StartBeat := CurrentSong.Tracks[CurrentTrack].Lines[DstLine + LineIndex - 1].Notes[0].StartBeat +
      (CurrentSong.Tracks[CurrentTrack].Lines[SrcLine + LineIndex].Notes[0].StartBeat - CurrentSong.Tracks[CurrentTrack].Lines[SrcLine + LineIndex - 1].Notes[0].StartBeat);
    SetLength(CurrentSong.Tracks[CurrentTrack].Lines[DstLine + LineIndex].Notes, 1);
    CurrentSong.Tracks[CurrentTrack].Lines[DstLine + LineIndex].HighNote := 0;
    CurrentSong.Tracks[CurrentTrack].Lines[DstLine + LineIndex].Notes[0].StartBeat := CurrentSong.Tracks[CurrentTrack].Lines[DstLine + LineIndex].StartBeat;
    CurrentSong.Tracks[CurrentTrack].Lines[DstLine + LineIndex].Notes[0].Duration := 1;
    CurrentSong.Tracks[CurrentTrack].Lines[DstLine + LineIndex].EndBeat := CurrentSong.Tracks[CurrentTrack].Lines[DstLine + LineIndex].StartBeat + 1;
  end;

  // increase counters
  CurrentSong.Tracks[CurrentTrack].Number := CurrentSong.Tracks[CurrentTrack].Number + Num - 1;
  CurrentSong.Tracks[CurrentTrack].High := CurrentSong.Tracks[CurrentTrack].High + Num - 1;

  for LineIndex := 0 to Num - 1 do
    CopySentence(SrcTrack, SrcLine + LineIndex, DstTrack, DstLine + LineIndex);
end;

procedure TScreenEditSub.MakeSolo;
begin
  if not (CurrentSong.isDuet) then
    Exit;

  // use current track to make solo
  if (CurrentTrack <> 0) then
    CurrentSong.Tracks[0] := CurrentSong.Tracks[CurrentTrack];

  SetLength(CurrentSong.Tracks, 1);
  CurrentSong.isDuet := false;

  CurrentTrack := 0;
  Refresh;
  CurrentNote[CurrentTrack] := 0;
  CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;

  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
end;

procedure TScreenEditSub.MakeDuet;
var
  LineIndex:  Integer;

begin
  if (CurrentSong.isDuet) then
    Exit;

  SetLength(CurrentSong.Tracks, 2);

  CurrentSong.Tracks[CurrentTrack+1].CurrentLine := CurrentSong.Tracks[CurrentTrack].CurrentLine;
  CurrentSong.Tracks[CurrentTrack+1].High := CurrentSong.Tracks[CurrentTrack].High;
  CurrentSong.Tracks[CurrentTrack+1].Number := CurrentSong.Tracks[CurrentTrack].Number;
  CurrentSong.Tracks[CurrentTrack+1].NotesGAP := CurrentSong.Tracks[CurrentTrack].NotesGAP;
  CurrentSong.Tracks[CurrentTrack+1].ScoreValue := 0;
  SetLength(CurrentSong.Tracks[CurrentTrack+1].Lines, Length(CurrentSong.Tracks[CurrentTrack].Lines));

  for LineIndex := 0 to High(CurrentSong.Tracks[CurrentTrack].Lines) do
    CopyLine(0, LineIndex, 1, LineIndex);

  CurrentSong.isDuet := true;

  CurrentNote[CurrentTrack+1] := 0;
  CurrentSong.Tracks[CurrentTrack+1].CurrentLine := 0;

  EditorLyrics[CurrentTrack+1] := EditorLyrics[0];

  //delete medley
  MedleyNotes.isStart := false;
  MedleyNotes.isEnd := false;
  CurrentSong.Medley.Source := msNone;
end;

function TScreenEditSub.DuetCopyLine: boolean;
var
  SrcLine:      Integer;
  DstLine:      Integer;
  SrcTrack:     Integer;
  DstTrack:     Integer;

  SrcStartBeat: Integer;
  SrcEndBeat:   Integer;

  DstStartBeat: Integer;
  DstEndBeat:   Integer;

  SrcNumN:      Integer;
  DstNumN:      Integer;

  LineIndex1:   Integer;
  LineIndex2:   Integer;

  LineLength:   Integer;
begin
  Result := false;

  SrcTrack := CurrentTrack;
  DstTrack := (CurrentTrack + 1) mod 2;
  SrcLine := CurrentSong.Tracks[SrcTrack].CurrentLine;
  DstLine := -1;

  SrcStartBeat := CurrentSong.Tracks[SrcTrack].Lines[SrcLine].Notes[0].StartBeat;
  SrcNumN := Length(CurrentSong.Tracks[SrcTrack].Lines[SrcLine].Notes);
  SrcEndBeat := CurrentSong.Tracks[SrcTrack].Lines[SrcLine].Notes[SrcNumN-1].StartBeat + CurrentSong.Tracks[SrcTrack].Lines[SrcLine].Notes[SrcNumN-1].Duration;

  for LineIndex1 := 0 to High(CurrentSong.Tracks[DstTrack].Lines) do
  begin
    DstStartBeat := CurrentSong.Tracks[DstTrack].Lines[LineIndex1].Notes[0].StartBeat;
    DstNumN := Length(CurrentSong.Tracks[DstTrack].Lines[LineIndex1].Notes);
    DstEndBeat := CurrentSong.Tracks[DstTrack].Lines[LineIndex1].Notes[DstNumN-1].StartBeat + CurrentSong.Tracks[DstTrack].Lines[LineIndex1].Notes[DstNumN-1].Duration;
    if (DstStartBeat <= SrcStartBeat) and (SrcEndBeat <= DstEndBeat) then // SrcLine fits into existing line DstLine --> replace DstLine by SrcLine
    begin
      DstLine := LineIndex1;
      break;
    end;

    if (DstLine = -1) then // SrcLine does not fit into any of the existing lines --> insert SrcLine
    begin
      if (LineIndex1 < Length(CurrentSong.Tracks[DstTrack].Lines)-1) then // insert somewhere in the middle
      begin
        DstStartBeat := DstEndBeat;
        DstEndBeat := CurrentSong.Tracks[DstTrack].Lines[LineIndex1+1].Notes[0].StartBeat;
        if (DstStartBeat < SrcStartBeat) and (SrcEndBeat < DstEndBeat) then
        begin
          LineLength := Length(CurrentSong.Tracks[DstTrack].Lines);
          SetLength(CurrentSong.Tracks[DstTrack].Lines, LineLength + 1);
          Inc(CurrentSong.Tracks[DstTrack].Number);
          Inc(CurrentSong.Tracks[DstTrack].High);

          // make room for new line
          for LineIndex2 := LineLength-1 downto LineIndex1 do
            CopyLine(DstTrack, LineIndex2, DstTrack, LineIndex2+1);

          DstLine := LineIndex1 + 1;
          SetLength(CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes, 0);
          break;
        end;
      end
      else
      begin
        if (SrcEndBeat <= CurrentSong.Tracks[DstTrack].Lines[0].Notes[0].StartBeat) then // insert at beginning
        begin
          LineLength := Length(CurrentSong.Tracks[DstTrack].Lines);
          SetLength(CurrentSong.Tracks[DstTrack].Lines, LineLength + 1);
          Inc(CurrentSong.Tracks[DstTrack].Number);
          Inc(CurrentSong.Tracks[DstTrack].High);

          // make room for new line
          for LineIndex2 := LineLength-1 downto 0 do
            CopyLine(DstTrack, LineIndex2, DstTrack, LineIndex2+1);

          DstLine := 0;
          SetLength(CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes, 0);
          break;
        end
        else
        if (SrcStartBeat >= CurrentSong.Tracks[DstTrack].Lines[High(CurrentSong.Tracks[DstTrack].Lines)].EndBeat) then // insert at end
        begin
          LineLength := Length(CurrentSong.Tracks[DstTrack].Lines);
          SetLength(CurrentSong.Tracks[DstTrack].Lines, LineLength + 1);
          Inc(CurrentSong.Tracks[DstTrack].Number);
          Inc(CurrentSong.Tracks[DstTrack].High);

          DstLine := High(CurrentSong.Tracks[DstTrack].Lines);
          SetLength(CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes, 0);
          break;
        end;
      end;
    end;
  end;

  if (DstLine = -1) then
    Exit;

  CopyLine(SrcTrack, SrcLine, DstTrack, DstLine);

  Refresh;
  EditorLyrics[DstTrack].AddLine(DstTrack, CurrentSong.Tracks[DstTrack].CurrentLine);
  EditorLyrics[DstTrack].Selected := 0;
  CurrentNote[DstTrack] := 0;
  CurrentSong.Tracks[SrcTrack].Lines[SrcLine].Notes[CurrentNote[SrcTrack]].Color := P1_INVERTED;
  Result := true;
end;

procedure TScreenEditSub.CopyLine(SrcTrack, SrcLine, DstTrack, DstLine: Integer);
begin
  CurrentSong.Tracks[DstTrack].Lines[DstLine] := CurrentSong.Tracks[SrcTrack].Lines[SrcLine];
  CurrentSong.Tracks[DstTrack].Lines[DstLine].Notes := Copy(CurrentSong.Tracks[SrcTrack].Lines[SrcLine].Notes);
end;

function TScreenEditSub.DuetMoveLine: boolean;
begin
  Result := DuetCopyLine;
  if (Result) then
    DeleteSentence;
end;

procedure TScreenEditSub.CopyToUndo;
var
  BPMIndex:   Integer;
  TrackIndex: Integer;
  LineIndex:  Integer;
  NoteIndex:  Integer;
begin
  FPreviewRescorePending := true;
  SetLength(UndoLines, Length(UndoLines)+1, Length(CurrentSong.Tracks));
  CurrentUndoLines := high(UndoLines);
  SetLength(UndoStateNote, CurrentUndoLines+1, Length(CurrentSong.Tracks));
  SetLength(UndoHeader, CurrentUndoLines+1);

  UndoHeader[CurrentUndoLines].Title := CurrentSong.Title;
  UndoHeader[CurrentUndoLines].Artist := CurrentSong.Artist;
  UndoHeader[CurrentUndoLines].Language := CurrentSong.Language;
  UndoHeader[CurrentUndoLines].Edition := CurrentSong.Edition;
  UndoHeader[CurrentUndoLines].Genre := CurrentSong.Genre;
  UndoHeader[CurrentUndoLines].Year := CurrentSong.Year;
  UndoHeader[CurrentUndoLines].Creator := CurrentSong.Creator;
  UndoHeader[CurrentUndoLines].Mp3 := CurrentSong.Audio;
  UndoHeader[CurrentUndoLines].Mp3Id := SelectsS[Mp3SlideId].SelectedOption;
  UndoHeader[CurrentUndoLines].Cover := CurrentSong.Cover;
  UndoHeader[CurrentUndoLines].CoverId := SelectsS[CoverSlideId].SelectedOption;
  UndoHeader[CurrentUndoLines].Background := CurrentSong.Background;
  UndoHeader[CurrentUndoLines].BackgroundId := SelectsS[BackgroundSlideId].SelectedOption;
  UndoHeader[CurrentUndoLines].Video := CurrentSong.Video;
  UndoHeader[CurrentUndoLines].VideoId := SelectsS[VideoSlideId].SelectedOption;
  UndoHeader[CurrentUndoLines].VideoGAP := CurrentSong.VideoGAP;
  SetLength(UndoHeader[CurrentUndoLines].BPM, length(CurrentSong.BPM));
  for BPMIndex := 0 to High(CurrentSong.BPM) do
  begin
    UndoHeader[CurrentUndoLines].BPM[BPMIndex].BPM := CurrentSong.BPM[BPMIndex].BPM;
    UndoHeader[CurrentUndoLines].BPM[BPMIndex].StartBeat := CurrentSong.BPM[BPMIndex].StartBeat;
  end;
  UndoHeader[CurrentUndoLines].GAP  := CurrentSong.GAP;
  UndoHeader[CurrentUndoLines].StartTag := CurrentSong.Start;
  UndoHeader[CurrentUndoLines].EndTag := CurrentSong.Finish;
  if not (CurrentSong.isDuet) then
  begin
    UndoHeader[CurrentUndoLines].MedleyNotes := MedleyNotes;
  end;
  UndoHeader[CurrentUndoLines].PreviewStart := CurrentSong.PreviewStart;
  UndoHeader[CurrentUndoLines].Relative := CurrentSong.Relative;

  for TrackIndex := 0 to High(CurrentSong.Tracks) do
  begin
    UndoStateNote[CurrentUndoLines, TrackIndex] := CurrentNote[TrackIndex];

    UndoLines[CurrentUndoLines, TrackIndex].CurrentLine := CurrentSong.Tracks[TrackIndex].CurrentLine;
    UndoLines[CurrentUndoLines, TrackIndex].High := CurrentSong.Tracks[TrackIndex].High;
    UndoLines[CurrentUndoLines, TrackIndex].Number := CurrentSong.Tracks[TrackIndex].Number;
    UndoLines[CurrentUndoLines, TrackIndex].NotesGAP := CurrentSong.Tracks[TrackIndex].NotesGAP;
    UndoLines[CurrentUndoLines, TrackIndex].ScoreValue := CurrentSong.Tracks[TrackIndex].ScoreValue;
    SetLength(UndoLines[CurrentUndoLines, TrackIndex].Lines, Length(CurrentSong.Tracks[TrackIndex].Lines));

    for LineIndex := 0 to High(CurrentSong.Tracks[TrackIndex].Lines) do
    begin
      UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].StartBeat  := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].StartBeat;
      UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Lyric      := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Lyric;
      UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].EndBeat    := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].EndBeat;
      UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].BaseNote   := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].BaseNote;
      UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].HighNote   := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote;
      UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].ScoreValue := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].ScoreValue;
      UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].LastLine   := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].LastLine;

      SetLength(UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes, length(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes));
      for NoteIndex := 0 to High(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes) do
      begin
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Color          := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Color;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat      := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration       := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Tone           := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Tone;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text           := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].NoteType       := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].NoteType;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsMedley       := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsMedley;
        UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsStartPreview := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsStartPreview;
      end; //for NoteIndex
    end; //for LineIndex
  end; // TrackIndex
end;

procedure TScreenEditSub.Refresh;
var
  TrackIndex: Integer;
  LineIndex:  Integer;
  NoteIndex:  Integer;

begin
  FixTimings;
  LyricsCorrectSpaces;

  if not (CurrentSong.isDuet) then
  begin
    if MedleyNotes.isStart and
      ((High(CurrentSong.Tracks[CurrentTrack].Lines) < MedleyNotes.start.line) or
       (High(CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes) < MedleyNotes.start.note)) then
      MedleyNotes.isStart := false;

    if MedleyNotes.isEnd and
      ((High(CurrentSong.Tracks[CurrentTrack].Lines) < MedleyNotes.end_.line) or
       (High(CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes) < MedleyNotes.end_.note)) then
      MedleyNotes.isEnd := false;
  end;


  for TrackIndex := 0 to High(CurrentSong.Tracks) do
  begin
    CurrentSong.Tracks[TrackIndex].Number := Length(CurrentSong.Tracks[TrackIndex].Lines);
    CurrentSong.Tracks[TrackIndex].High := CurrentSong.Tracks[TrackIndex].Number - 1;
    CurrentSong.Tracks[TrackIndex].ScoreValue := 0;

    for LineIndex := 0 to High(CurrentSong.Tracks[TrackIndex].Lines) do
    begin
      with CurrentSong.Tracks[TrackIndex].Lines[LineIndex] do
      begin
        HighNote := Length(Notes) - 1;
        ScoreValue := 0;

        if (Length(Notes) > 0) then
        begin
          //StartBeat := Notes[0].StartBeat; // FIXME: is this really true? Isn't StartBeat of a Line corresponding to the line break time? See TLine in UMusic
          for NoteIndex := 0 to High(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes) do
          begin
            //Notes[NoteIndex].Color := 0;

            if not (CurrentSong.isDuet) then
            begin
              if (MedleyNotes.isStart and
                 (MedleyNotes.start.track = TrackIndex) and
                 (MedleyNotes.start.line = LineIndex) and
                 (MedleyNotes.start.note = NoteIndex)) or
                 (MedleyNotes.isEnd and (MedleyNotes.end_.track = TrackIndex) and
                 (MedleyNotes.end_.line = LineIndex) and
                 (MedleyNotes.end_.note = NoteIndex)) then
                Notes[NoteIndex].IsMedley := true
              else
                Notes[NoteIndex].IsMedley := false;
            end;

            Notes[NoteIndex].IsStartPreview := false;

            CurrentSong.Tracks[TrackIndex].ScoreValue := CurrentSong.Tracks[TrackIndex].ScoreValue + Notes[NoteIndex].Duration * ScoreFactor[Notes[NoteIndex].NoteType];
            ScoreValue := ScoreValue + Notes[NoteIndex].Duration * ScoreFactor[Notes[NoteIndex].NoteType];
          end;
        end;

        UpdateLineBaseNote(TrackIndex, LineIndex);
      end;
    end;
  end;

  // set Preview Start
  MedleyNotes.Preview := FindNote(round(GetMidBeat(CurrentSong.PreviewStart - CurrentSong.Gap / 1000)));
  CurrentSong.Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].IsStartPreview := true;
  CurrentSong.PreviewStart := Round(GetTimeFromBeat(CurrentSong.Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].StartBeat) * 100) / 100;
end;

procedure TScreenEditSub.CopyFromUndo;
var
  BPMIndex:   Integer;
  TrackIndex: Integer;
  LineIndex:  Integer;
  NoteIndex:  Integer;

begin
CurrentUndoLines := high(UndoLines);

if CurrentUndoLines >= 0 then
begin
  CurrentSong.Title        := Undoheader[CurrentUndoLines].Title;
  CurrentSong.Artist       := Undoheader[CurrentUndoLines].Artist;
  CurrentSong.Language     := Undoheader[CurrentUndoLines].Language;
  CurrentSong.Edition      := Undoheader[CurrentUndoLines].Edition;
  CurrentSong.Genre        := Undoheader[CurrentUndoLines].Genre;
  CurrentSong.Year         := Undoheader[CurrentUndoLines].Year;
  CurrentSong.Creator      := Undoheader[CurrentUndoLines].Creator;
  CurrentSong.Audio          := Undoheader[CurrentUndoLines].Mp3;
  SelectsS[Mp3SlideId].SelectedOption := Undoheader[CurrentUndoLines].Mp3Id;
  CurrentSong.Cover        := Undoheader[CurrentUndoLines].Cover;
  SelectsS[CoverSlideId].SelectedOption := Undoheader[CurrentUndoLines].CoverId;
  CurrentSong.Background   := Undoheader[CurrentUndoLines].Background;
  SelectsS[BackgroundSlideId].SelectedOption := Undoheader[CurrentUndoLines].BackgroundId;
  CurrentSong.Video        := Undoheader[CurrentUndoLines].Video;
  SelectsS[VideoSlideId].SelectedOption := Undoheader[CurrentUndoLines].VideoId;
  CurrentSong.VideoGAP     := Undoheader[CurrentUndoLines].VideoGAP;
  SetLength(CurrentSong.BPM, length(Undoheader[CurrentUndoLines].BPM));
  for BPMIndex := 0 to High(Undoheader[CurrentUndoLines].BPM) do
  begin
    CurrentSong.BPM[BPMIndex].BPM := Undoheader[CurrentUndoLines].BPM[BPMIndex].BPM;
    CurrentSong.BPM[BPMIndex].StartBeat := Undoheader[CurrentUndoLines].BPM[BPMIndex].StartBeat;
  end;
  CurrentSong.GAP          := Undoheader[CurrentUndoLines].GAP;
  CurrentSong.Start        := Undoheader[CurrentUndoLines].StartTag;
  CurrentSong.Finish       := Undoheader[CurrentUndoLines].EndTag;
  MedleyNotes := Undoheader[CurrentUndoLines].MedleyNotes;
  CurrentSong.PreviewStart := Undoheader[CurrentUndoLines].PreviewStart;
  CurrentSong.Relative     := Undoheader[CurrentUndoLines].Relative;

  for TrackIndex := 0 to High(CurrentSong.Tracks) do
  begin
    CurrentNote[TrackIndex] := UndoStateNote[High(UndoStateNote), TrackIndex];

    CurrentSong.Tracks[TrackIndex].CurrentLine := UndoLines[CurrentUndoLines, TrackIndex].CurrentLine;
    CurrentSong.Tracks[TrackIndex].High        := UndoLines[CurrentUndoLines, TrackIndex].High;
    CurrentSong.Tracks[TrackIndex].Number      := UndoLines[CurrentUndoLines, TrackIndex].Number;
    CurrentSong.Tracks[TrackIndex].NotesGAP    := UndoLines[CurrentUndoLines, TrackIndex].NotesGAP;
    CurrentSong.Tracks[TrackIndex].ScoreValue  := UndoLines[CurrentUndoLines, TrackIndex].ScoreValue;
    SetLength(CurrentSong.Tracks[TrackIndex].Lines, Length(UndoLines[CurrentUndoLines, TrackIndex].Lines));
    for LineIndex := 0 to High(UndoLines[CurrentUndoLines, TrackIndex].Lines) do
    begin
        CurrentSong.Tracks[TrackIndex].Lines[LineIndex].StartBeat  := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].StartBeat;
        CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Lyric      := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Lyric;
        CurrentSong.Tracks[TrackIndex].Lines[LineIndex].EndBeat    := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].EndBeat;
        CurrentSong.Tracks[TrackIndex].Lines[LineIndex].BaseNote   := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].BaseNote;
        CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote   := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].HighNote;
        CurrentSong.Tracks[TrackIndex].Lines[LineIndex].ScoreValue := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].ScoreValue;
        CurrentSong.Tracks[TrackIndex].Lines[LineIndex].LastLine   := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].LastLine;

        SetLength(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes, Length(UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes));
        for  NoteIndex := 0 to High(UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes) do
        begin
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Color          := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Color;
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat      := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat;
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration       := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration;
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Tone           := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Tone;
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text           := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].Text;
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].NoteType       := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].NoteType;
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsMedley       := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsMedley;
          CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsStartPreview := UndoLines[CurrentUndoLines, TrackIndex].Lines[LineIndex].Notes[NoteIndex].IsStartPreview;
        end; //for NoteIndex
    end; //for LineIndex
  end; //for TrackIndex
  SetLength(UndoStateNote, high(UndoStateNote));
  SetLength(UndoHeader, high(UndoLines));
  SetLength(UndoLines, high(UndoLines));
  Text[TextInfo].Text := Language.Translate('EDIT_INFO_UNDO');

  // to refresh all headers
  SelectsS[TitleSlideId].TextOpt[0].Text    := CurrentSong.Title;
  SelectsS[ArtistSlideId].TextOpt[0].Text   := CurrentSong.Artist;
  SelectsS[LanguageSlideId].TextOpt[0].Text := CurrentSong.Language;
  SelectsS[EditionSlideId].TextOpt[0].Text  := CurrentSong.Edition;
  SelectsS[GenreSlideId].TextOpt[0].Text    := CurrentSong.Genre;
  SelectsS[YearSlideId].TextOpt[0].Text     := IntToStr(CurrentSong.Year);
  SelectsS[CreatorSlideId].TextOpt[0].Text  := CurrentSong.Creator;
  SelectsS[LyricSlideId].TextOpt[0].Text    := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
  FPreviewRescorePending := false;
  InvalidateVocalsAnalysis(true);
end; //if CurrentUndoLines
end;

procedure TScreenEditSub.DrawPlayerTrack(CurrentTone: Integer; Count: Integer; CurrentNote: Integer);
var
  Rec:     TRecR;
  scale:   Integer;
  HalfToneHeight: real;
  BarWidth: real;
  BarHeight: real;
begin
  glColor3f(1, 1, 1);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  BarWidth := 16;
  BarHeight := 16;
  HalfToneHeight := 7.5;
  Rec.Right := 40 + 0.5 + Count;
  Rec.Left  := Rec.Right - BarWidth;

  scale := Round((CurrentTone - CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote].Tone) / 12);

  Rec.Top := 429 - (CurrentTone - 12*scale - CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].BaseNote) * HalfToneHeight - 0.5 * BarHeight;
  Rec.Bottom := Rec.Top + BarHeight;

  glColor3f(1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, Tex_Lyric_Help_Bar.TexNum);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(Rec.Left,  Rec.Top);
    glTexCoord2f(0, 1); glVertex2f(Rec.Left,  Rec.Bottom);
    glTexCoord2f(1, 1); glVertex2f(Rec.Right, Rec.Bottom);
    glTexCoord2f(1, 0); glVertex2f(Rec.Right, Rec.Top);
  glEnd;

  // draw currently recorded tone
  SetFontPos(Theme.EditSub.TextCurrentTone.X, Theme.EditSub.TextCurrentTone.Y);
  SetFontSize(Theme.EditSub.TextCurrentTone.Size);
  glColor4f(Theme.EditSub.TextCurrentTone.ColR, Theme.EditSub.TextCurrentTone.ColG, Theme.EditSub.TextCurrentTone.ColB, 1);
  glPrint(GetNoteName(CurrentTone));
end;

procedure TScreenEditSub.ClearVocalsWaveformCache(const ClearPreview: Boolean = true);
begin
  if Assigned(FVocalsPreviewPlaybackStream) then
  begin
    FVocalsPreviewPlaybackStream.Stop;
    FreeAndNil(FVocalsPreviewPlaybackStream);
  end;
  if Assigned(FVocalsPreviewSourceStream) then
    FreeAndNil(FVocalsPreviewSourceStream);

  EnterCriticalSection(FAnalysisDataLock);
  try
  SetLength(FVocalsPitchContour, 0);
  FVocalsPitchStepSec := 0;
  FVocalsPitchStartSec := 0;
  SetLength(FVocalsPreviewSamples, 0);
  FVocalsPreviewReadySamples := 0;
  if ClearPreview then
  begin
    FPreviewScoreTrack := -1;
    SetLength(FPreviewLineNormalScoreByTrack[0], 0);
    SetLength(FPreviewLineNormalScoreByTrack[1], 0);
    SetLength(FPreviewLineGoldenScoreByTrack[0], 0);
    SetLength(FPreviewLineGoldenScoreByTrack[1], 0);
    SetLength(FPreviewLineBonusScoreByTrack[0], 0);
    SetLength(FPreviewLineBonusScoreByTrack[1], 0);
    SetLength(FPreviewLineScoreReadyByTrack[0], 0);
    SetLength(FPreviewLineScoreReadyByTrack[1], 0);
    SetLength(FPreviewLineNormalScore, 0);
    SetLength(FPreviewLineGoldenScore, 0);
    SetLength(FPreviewLineBonusScore, 0);
    SetLength(FPreviewLineScoreReady, 0);
    FPreviewBuildComplete := false;
    FPreviewBuildStarted := false;
    FUsingVocalsPreviewPlayback := false;
    FAnalysisFailed := false;
    FAnalysisError := '';
  end;
  finally
    LeaveCriticalSection(FAnalysisDataLock);
  end;
end;

procedure TScreenEditSub.InvalidateVocalsAnalysis(const KeepPreview: Boolean = true);
var
  LineIndex: Integer;
begin
  if not KeepPreview then
    ClearVocalsWaveformCache(true);
  if KeepPreview and HasVocalsPreviewPlayback then
  begin
    RefreshPreviewScoreMetadata(CurrentTrack);
    EnsurePreviewScoreCache(CurrentTrack);
    EnterCriticalSection(FAnalysisDataLock);
    try
      if (CurrentTrack >= 0) and (CurrentTrack <= High(FPreviewLineScoreReadyByTrack)) then
      begin
        for LineIndex := 0 to High(FPreviewLineScoreReadyByTrack[CurrentTrack]) do
        begin
          FPreviewLineNormalScoreByTrack[CurrentTrack][LineIndex] := 0;
          FPreviewLineGoldenScoreByTrack[CurrentTrack][LineIndex] := 0;
          FPreviewLineBonusScoreByTrack[CurrentTrack][LineIndex] := 0;
          FPreviewLineScoreReadyByTrack[CurrentTrack][LineIndex] := false;
        end;
      end;

      if FPreviewScoreTrack = CurrentTrack then
      begin
        for LineIndex := 0 to High(FPreviewLineScoreReady) do
        begin
          FPreviewLineNormalScore[LineIndex] := 0;
          FPreviewLineGoldenScore[LineIndex] := 0;
          FPreviewLineBonusScore[LineIndex] := 0;
          FPreviewLineScoreReady[LineIndex] := false;
        end;
      end;
    finally
      LeaveCriticalSection(FAnalysisDataLock);
    end;
    StartAnalysisThread(false, CurrentTrack);
  end;
end;

procedure TScreenEditSub.EnsurePreviewScoreCache(const Track: Integer);
var
  LineCount: Integer;
begin
  if (Track < 0) or (Track > High(CurrentSong.Tracks)) then
    Exit;

  if (FPreviewScoreTrack >= 0) and (FPreviewScoreTrack <= High(FPreviewLineNormalScoreByTrack)) and
     (FPreviewScoreTrack <> Track) then
  begin
    FPreviewLineNormalScoreByTrack[FPreviewScoreTrack] := FPreviewLineNormalScore;
    FPreviewLineGoldenScoreByTrack[FPreviewScoreTrack] := FPreviewLineGoldenScore;
    FPreviewLineBonusScoreByTrack[FPreviewScoreTrack] := FPreviewLineBonusScore;
    FPreviewLineScoreReadyByTrack[FPreviewScoreTrack] := FPreviewLineScoreReady;
  end;

  if Track <= High(FPreviewLineNormalScoreByTrack) then
  begin
    FPreviewLineNormalScore := FPreviewLineNormalScoreByTrack[Track];
    FPreviewLineGoldenScore := FPreviewLineGoldenScoreByTrack[Track];
    FPreviewLineBonusScore := FPreviewLineBonusScoreByTrack[Track];
    FPreviewLineScoreReady := FPreviewLineScoreReadyByTrack[Track];
  end;

  LineCount := Length(CurrentSong.Tracks[Track].Lines);
  if (Length(FPreviewLineNormalScore) = LineCount) and
     (Length(FPreviewLineGoldenScore) = LineCount) and
     (Length(FPreviewLineBonusScore) = LineCount) and
     (Length(FPreviewLineScoreReady) = LineCount) then
  begin
    FPreviewScoreTrack := Track;
    Exit;
  end;

  FPreviewScoreTrack := Track;
  SetLength(FPreviewLineNormalScore, LineCount);
  SetLength(FPreviewLineGoldenScore, LineCount);
  SetLength(FPreviewLineBonusScore, LineCount);
  SetLength(FPreviewLineScoreReady, LineCount);
  if LineCount > 0 then
  begin
    FillChar(FPreviewLineNormalScore[0], LineCount * SizeOf(FPreviewLineNormalScore[0]), 0);
    FillChar(FPreviewLineGoldenScore[0], LineCount * SizeOf(FPreviewLineGoldenScore[0]), 0);
    FillChar(FPreviewLineBonusScore[0], LineCount * SizeOf(FPreviewLineBonusScore[0]), 0);
    FillChar(FPreviewLineScoreReady[0], LineCount * SizeOf(FPreviewLineScoreReady[0]), 0);
  end;

  if Track <= High(FPreviewLineNormalScoreByTrack) then
  begin
    FPreviewLineNormalScoreByTrack[Track] := FPreviewLineNormalScore;
    FPreviewLineGoldenScoreByTrack[Track] := FPreviewLineGoldenScore;
    FPreviewLineBonusScoreByTrack[Track] := FPreviewLineBonusScore;
    FPreviewLineScoreReadyByTrack[Track] := FPreviewLineScoreReady;
  end;
end;

procedure TScreenEditSub.RefreshPreviewScoreMetadata(const Track: Integer);
var
  LineIndex: Integer;
  NoteIndex: Integer;
  LineScoreValue: Integer;
begin
  if (Track < 0) or (Track > High(CurrentSong.Tracks)) then
    Exit;

  CurrentSong.Tracks[Track].Number := Length(CurrentSong.Tracks[Track].Lines);
  CurrentSong.Tracks[Track].High := CurrentSong.Tracks[Track].Number - 1;
  CurrentSong.Tracks[Track].ScoreValue := 0;

  for LineIndex := 0 to High(CurrentSong.Tracks[Track].Lines) do
  begin
    CurrentSong.Tracks[Track].Lines[LineIndex].HighNote :=
      Length(CurrentSong.Tracks[Track].Lines[LineIndex].Notes) - 1;
    LineScoreValue := 0;
    for NoteIndex := 0 to High(CurrentSong.Tracks[Track].Lines[LineIndex].Notes) do
      Inc(LineScoreValue,
        CurrentSong.Tracks[Track].Lines[LineIndex].Notes[NoteIndex].Duration *
        ScoreFactor[CurrentSong.Tracks[Track].Lines[LineIndex].Notes[NoteIndex].NoteType]);

    CurrentSong.Tracks[Track].Lines[LineIndex].ScoreValue := LineScoreValue;
    Inc(CurrentSong.Tracks[Track].ScoreValue, LineScoreValue);
    UpdateLineBaseNote(Track, LineIndex);
  end;
end;

procedure TScreenEditSub.RecomputeAllPreviewScores;
var
  TrackIndex: Integer;
begin
  if not HasVocalsPreviewPlayback then
    Exit;

  for TrackIndex := 0 to High(CurrentSong.Tracks) do
  begin
    RefreshPreviewScoreMetadata(TrackIndex);
    StartAnalysisThread(false, TrackIndex);
  end;
end;

procedure TScreenEditSub.TriggerGlobalPreviewRescore;
begin
  RefreshPreviewScoreMetadata(CurrentTrack);
  FPreviewRescorePending := false;
  InvalidateVocalsAnalysis(true);
end;

function TScreenEditSub.GetEditorScoreDifficulty: Integer;
begin
  Result := Ini.PlayerLevel[0];
  if Result < 0 then
    Result := 0
  else if Result > 2 then
    Result := 2;
end;

function TScreenEditSub.HasVocalsPreviewDirectSource: Boolean;
begin
  Result := Assigned(CurrentSong.Vocals) and
    (CurrentSong.Vocals <> PATH_NONE) and
    CurrentSong.Vocals.IsSet and
    CurrentSong.Path.Append(CurrentSong.Vocals).Exists;
end;

function TScreenEditSub.GetVocalsPreviewSourcePath: IPath;
begin
  if HasVocalsPreviewDirectSource then
    Result := CurrentSong.Vocals
  else if Assigned(CurrentSong.Karaoke) and
          (CurrentSong.Karaoke <> PATH_NONE) and
          CurrentSong.Karaoke.IsSet and
          CurrentSong.Path.Append(CurrentSong.Karaoke).Exists then
    Result := CurrentSong.Karaoke
  else
    Result := PATH_NONE;
end;

function TScreenEditSub.HasVocalsPreviewSource: Boolean;
begin
  Result := HasVocalsPreviewDirectSource or
    (Assigned(CurrentSong.Karaoke) and
     (CurrentSong.Karaoke <> PATH_NONE) and
     CurrentSong.Karaoke.IsSet and
     CurrentSong.Path.Append(CurrentSong.Karaoke).Exists);
end;

function TScreenEditSub.HasVocalsPreviewPlayback: Boolean;
begin
  Result := GetVocalsPreviewByteSize > 0;
end;

function TScreenEditSub.HasVocalsPreviewPlaybackAtTime(const Time: Real): Boolean;
var
  ReadySamples: Int64;
begin
  ReadySamples := 0;
  EnterCriticalSection(FAnalysisDataLock);
  try
    ReadySamples := FVocalsPreviewReadySamples;
  finally
    LeaveCriticalSection(FAnalysisDataLock);
  end;
  Result := (Time >= 0) and
    (Floor(Time * VOCALS_WAVEFORM_SAMPLE_RATE) < ReadySamples);
end;

function TScreenEditSub.GetVocalsPreviewByteSize: Int64;
begin
  Result := 0;
  EnterCriticalSection(FAnalysisDataLock);
  try
    Result := FVocalsPreviewReadySamples * SizeOf(SmallInt);
  finally
    LeaveCriticalSection(FAnalysisDataLock);
  end;
end;

function TScreenEditSub.ReadVocalsPreviewData(const PositionBytes: Int64; Buffer: PByte; const BufferSize: Integer): Integer;
var
  SafePosition: Int64;
  BytesAvailable: Int64;
begin
  Result := 0;
  if (BufferSize <= 0) or not Assigned(Buffer) then
    Exit;

  EnterCriticalSection(FAnalysisDataLock);
  try
    SafePosition := PositionBytes;
    if SafePosition < 0 then
      SafePosition := 0;
    SafePosition := SafePosition - (SafePosition mod SizeOf(SmallInt));
    BytesAvailable := FVocalsPreviewReadySamples * SizeOf(SmallInt) - SafePosition;
    if BytesAvailable <= 0 then
      Exit;
    Result := BufferSize;
    if Result > BytesAvailable then
      Result := BytesAvailable;
    Move(FVocalsPreviewSamples[SafePosition div SizeOf(SmallInt)], Buffer^, Result);
  finally
    LeaveCriticalSection(FAnalysisDataLock);
  end;
end;

procedure TScreenEditSub.StartAnalysisThread(const BuildPreview: Boolean; const Track: Integer);
begin
  if BuildPreview and not HasVocalsPreviewSource then
    Exit;

  if not BuildPreview then
  begin
    FAnalysisThreadBuildPreview := false;
    FAnalysisThreadTrack := Track;
    FAnalysisCancelRequested := false;
    FAnalysisProgressPct := 0;
    FAnalysisFailed := false;
    FAnalysisError := '';
    RunIncrementalVocalsAnalysis(false, Track);
    Exit;
  end;

  if Assigned(FAnalysisThread) and (not FAnalysisThreadRunning) then
    FreeAndNil(FAnalysisThread);
  if FAnalysisThreadRunning then
    Exit;

  FAnalysisThreadBuildPreview := BuildPreview;
  FAnalysisThreadTrack := Track;
  FAnalysisCancelRequested := false;
  FAnalysisProgressPct := 0;
  FAnalysisFailed := false;
  FAnalysisError := '';
  if BuildPreview then
  begin
    FPreviewBuildComplete := false;
    FPreviewBuildStarted := true;
  end;
  FAnalysisThreadRunning := true;
  FAnalysisThread := TEditSubAnalysisThread.Create(Self, BuildPreview, Track);
end;

procedure TScreenEditSub.StopAnalysisThread;
begin
  if Assigned(FAnalysisThread) then
  begin
    FAnalysisCancelRequested := true;
    FAnalysisThread.Terminate;
    FAnalysisThread.WaitFor;
    FreeAndNil(FAnalysisThread);
  end;
  FAnalysisThreadRunning := false;
end;

procedure TScreenEditSub.EnsureCurrentLineWaveformCache;
begin
  if Error or
     (CurrentTrack < 0) or
     (CurrentTrack > High(CurrentSong.Tracks)) or
     (CurrentSong.Tracks[CurrentTrack].CurrentLine < 0) or
     (CurrentSong.Tracks[CurrentTrack].CurrentLine > High(CurrentSong.Tracks[CurrentTrack].Lines)) then
    Exit;

  if HasVocalsPreviewSource and
     not FPreviewBuildStarted and not FAnalysisThreadRunning and not FAnalysisFailed then
    StartAnalysisThread(true, CurrentTrack);
end;

function TScreenEditSub.EnsureVocalsPreviewPlaybackStream: Boolean;
begin
  Result := HasVocalsPreviewPlayback;
  if not Result then
    Exit;

  if Assigned(FVocalsPreviewPlaybackStream) then
    Exit(true);

  if not Assigned(AudioPlayback) then
    Exit(false);

  FVocalsPreviewSourceStream := TEditSubPreviewSourceStream.Create(Self);
  FVocalsPreviewPlaybackStream := AudioPlayback.CreatePlaybackStreamForSource(FVocalsPreviewSourceStream);
  Result := Assigned(FVocalsPreviewPlaybackStream);
  if not Result then
    FreeAndNil(FVocalsPreviewSourceStream);
end;

procedure TScreenEditSub.SetVocalsPreviewPlayback(const Enable: Boolean; const KeepPosition: Boolean = false);
var
  RestorePos: real;
begin
  if Enable = FUsingVocalsPreviewPlayback then
    Exit;

  RestorePos := 0;
  if KeepPosition then
    RestorePos := GetPlaybackPosition;

  StopPlayback;
  if Enable and EnsureVocalsPreviewPlaybackStream then
  begin
    FUsingVocalsPreviewPlayback := true;
  end
  else
    FUsingVocalsPreviewPlayback := false;

  if KeepPosition and (RestorePos >= 0) then
    SetPlaybackPosition(EnsureRange(RestorePos, 0.0, GetPlaybackLength));
end;

procedure TScreenEditSub.RestoreStandardPlayback(const KeepPosition: Boolean = true);
begin
  if FUsingVocalsPreviewPlayback then
    SetVocalsPreviewPlayback(false, KeepPosition);
end;

function TScreenEditSub.GetPlaybackLength: real;
begin
  if FUsingVocalsPreviewPlayback and Assigned(FVocalsPreviewPlaybackStream) then
    Result := FVocalsPreviewPlaybackStream.Length
  else
    Result := AudioPlayback.Length;
end;

function TScreenEditSub.GetPlaybackPosition: real;
begin
  if FUsingVocalsPreviewPlayback and Assigned(FVocalsPreviewPlaybackStream) then
    Result := FVocalsPreviewPlaybackStream.Position
  else
    Result := AudioPlayback.Position;
end;

procedure TScreenEditSub.SetPlaybackPosition(const Time: real);
begin
  if FUsingVocalsPreviewPlayback and Assigned(FVocalsPreviewPlaybackStream) then
    FVocalsPreviewPlaybackStream.Position := Time
  else
    AudioPlayback.Position := Time;
end;

procedure TScreenEditSub.StopPlayback;
begin
  if FUsingVocalsPreviewPlayback and Assigned(FVocalsPreviewPlaybackStream) then
    FVocalsPreviewPlaybackStream.Stop
  else
    AudioPlayback.Stop;
end;

procedure TScreenEditSub.PlayPlayback;
begin
  if FUsingVocalsPreviewPlayback and Assigned(FVocalsPreviewPlaybackStream) then
    FVocalsPreviewPlaybackStream.Play
  else
    AudioPlayback.Play;
end;

procedure TScreenEditSub.SetPlaybackVolume(const Volume: single);
begin
  if FUsingVocalsPreviewPlayback and Assigned(FVocalsPreviewPlaybackStream) then
    FVocalsPreviewPlaybackStream.Volume := Volume
  else
    AudioPlayback.SetVolume(Volume);
end;

function TScreenEditSub.OpenDecodeStream(const Filename: IPath): TAudioDecodeStream;
var
  DecoderIndex: Integer;
begin
  Result := nil;
  if not Assigned(AudioDecoders) then
    Exit;
  if (not Assigned(Filename)) or
     (Filename = PATH_NONE) or
     (not Filename.IsSet) or
     (not Filename.Exists) then
    Exit;

  for DecoderIndex := 0 to AudioDecoders.Count - 1 do
  begin
    EnterCriticalSection(FAnalysisDecodeLock);
    try
      Result := IAudioDecoder(AudioDecoders[DecoderIndex]).Open(Filename);
    finally
      LeaveCriticalSection(FAnalysisDecodeLock);
    end;
    if Assigned(Result) then
      Exit;
  end;
end;

procedure TScreenEditSub.RunIncrementalVocalsAnalysis(const BuildPreview: Boolean; const Track: Integer);
const
  ANALYSIS_PUBLISH_HOPS = 128;
  PREVIEW_BOUNDARY_SMOOTH_SAMPLES = 1024;
type
  TSingleDynArray = array of Single;
var
  OriginalStream: TAudioDecodeStream;
  KaraokeStream: TAudioDecodeStream;
  OriginalConverter: TAudioConverter_SWResample;
  KaraokeConverter: TAudioConverter_SWResample;
  TargetFormat: TAudioFormatInfo;
  LineSnapshots: array of TLine;
  RefinedLines: array of Boolean;
  WorkingContour: TSingleDynArray;
  PendingSamples: TSingleDynArray;
  OriginalSamples: TSingleDynArray;
  KaraokeSamples: TSingleDynArray;
  CarryOriginal: TSingleDynArray;
  CarryKaraoke: TSingleDynArray;
  DiffSamples: TSingleDynArray;
  AnalysisSamples: TSingleDynArray;
  InputBytesOriginal: array of Byte;
  InputBytesKaraoke: array of Byte;
  OutputBytesOriginal: array of Byte;
  OutputBytesKaraoke: array of Byte;
  ReadBytesOriginal: Integer;
  ReadBytesKaraoke: Integer;
  InputSizeOriginal: Integer;
  InputSizeKaraoke: Integer;
  OutputSizeOriginal: Integer;
  OutputSizeKaraoke: Integer;
  FramesDecodedOriginal: Integer;
  FramesDecodedKaraoke: Integer;
  SampleCount: Integer;
  SampleIndex: Integer;
  BuildLine: Integer;
  PendingStartSample: Int64;
  NextPitchStartSample: Int64;
  PreviewNextSample: Int64;
  LocalStartIndex: Integer;
  TrimCount: Integer;
  HopsSinceScoreUpdate: Integer;
  SongTotalSamples: Int64;
  PitchTone: Single;
  PreviewSample16: SmallInt;
  PreviewWriteIndex: Integer;
  WrittenSamples: Integer;
  AvailableUntilSec: Double;
  BoundaryDelta: Single;
  SmoothCount: Integer;
  SmoothFactor: Single;
  LastPreviewSample: Single;
  HaveLastPreviewSample: Boolean;
  ScoreTrackValueSnapshot: Integer;
  NonEmptyLineCountSnapshot: Integer;
  Difficulty: Integer;
  Range: Integer;
  SnapshotLineIndex: Integer;
  NoteIndex: Integer;
  BeatIndex: Integer;
  NotePointsPerBeat: Double;
  DetectedNormal: Double;
  DetectedGolden: Double;
  DetectedLineBonus: Double;
  LineDetectedScore: Double;
  LineMaxScore: Double;
  LineBonus: Double;
  LinePerfection: Double;
  TimeSec: Double;
  RefineLineIndex: Integer;
  RefineStartSec: Double;
  RefineEndSec: Double;
  RefineStartSample: Int64;
  RefineEndSample: Int64;
  RefineFrameStart: Integer;
  RefineFrameEnd: Integer;
  RefineSampleCount: Integer;
  RefineLocalStart: Integer;
  RefineSamples: TSingleDynArray;
  RefineTone: Single;
  SongLengthSec: Double;
  OriginalEOF: Boolean;
  KaraokeEOF: Boolean;
  HasKaraokePreview: Boolean;
  UseDirectVocalsSource: Boolean;
  RescoreTrackIndex: Integer;

  function AnalysisCancelled: Boolean;
  begin
    Result := FAnalysisCancelRequested;
  end;

  procedure UpdateAnalysisProgress(const Percent: Integer);
  var
    ClampedPercent: Integer;
  begin
    if AnalysisCancelled then
      Exit;
    ClampedPercent := EnsureRange(Percent, 0, 100);
    if ClampedPercent > FAnalysisProgressPct then
      FAnalysisProgressPct := ClampedPercent;
  end;

  procedure AppendSamples(var Dest: TSingleDynArray; const Src: TSingleDynArray; const Count: Integer);
  var
    OldLength: Integer;
  begin
    if Count <= 0 then
      Exit;
    OldLength := Length(Dest);
    SetLength(Dest, OldLength + Count);
    Move(Src[0], Dest[OldLength], Count * SizeOf(Single));
  end;

  procedure ConsumeLeadingSamples(var Samples: TSingleDynArray; const Count: Integer);
  begin
    if Count <= 0 then
      Exit;
    if Count >= Length(Samples) then
    begin
      SetLength(Samples, 0);
      Exit;
    end;
    Move(Samples[Count], Samples[0], (Length(Samples) - Count) * SizeOf(Single));
    SetLength(Samples, Length(Samples) - Count);
  end;

  function DetectPitchTone(const Samples: TSingleDynArray; const StartIndex: Integer; out ToneValue: Single): Boolean;
  var
    Threshold: Single;
    MaxVolume: Single;
    Delay: Integer;
    ToneIndex: Integer;
    SampleWindowIndex: Integer;
    MaxCheckSamples: Integer;
    Correlation: Double;
    BestCorrelation: Double;
    BestToneIndex: Integer;
  begin
    Result := false;
    ToneValue := VOCALS_PITCH_INVALID;
    if (StartIndex < 0) or (StartIndex + VOCALS_PITCH_ANALYSIS_WINDOW > Length(Samples)) then
      Exit;

    Threshold := 0.01;
    MaxVolume := 0;
    MaxCheckSamples := Min(1024, VOCALS_PITCH_ANALYSIS_WINDOW);
    for SampleWindowIndex := 0 to MaxCheckSamples - 1 do
      MaxVolume := Max(MaxVolume, Abs(Samples[StartIndex + SampleWindowIndex]));
    if MaxVolume < Threshold then
      Exit;

    BestCorrelation := 1.0E300;
    BestToneIndex := -1;
    for ToneIndex := 0 to NumHalftones - 1 do
    begin
      Delay := Round(VOCALS_WAVEFORM_SAMPLE_RATE / (BaseToneFreq * Power(2, (ToneIndex - 33) / 12)));
      Correlation := 0;
      for SampleWindowIndex := 0 to VOCALS_PITCH_ANALYSIS_WINDOW - 1 do
        Correlation := Correlation + Abs(
          Samples[StartIndex + ((SampleWindowIndex + Delay) and (VOCALS_PITCH_ANALYSIS_WINDOW - 1))] -
          Samples[StartIndex + SampleWindowIndex]);
      Correlation := Correlation / VOCALS_PITCH_ANALYSIS_WINDOW;
      if Correlation < BestCorrelation then
      begin
        BestCorrelation := Correlation;
        BestToneIndex := ToneIndex;
      end;
    end;

    if BestToneIndex < 0 then
      Exit;

    ToneValue := BestToneIndex - 24;
    Result := true;
  end;

  function GetContourToneAtTime(const Contour: TSingleDynArray; const TimePosition: Double; out ToneValue: Single): Boolean;
  var
    FrameIndex: Integer;
  begin
    Result := false;
    ToneValue := VOCALS_PITCH_INVALID;
    if Length(Contour) <= 0 then
      Exit;
    FrameIndex := Round((TimePosition - FVocalsPitchStartSec) / FVocalsPitchStepSec);
    if (FrameIndex < 0) or (FrameIndex > High(Contour)) then
      Exit;
    ToneValue := Contour[FrameIndex];
    Result := ToneValue <> VOCALS_PITCH_INVALID;
  end;

  procedure RefineCompletedLines(const MaxTimeSec: Double; const ForceAll: Boolean);
  var
    LocalSampleIndex: Integer;
    RefineLineIndex: Integer;
  begin
    for RefineLineIndex := 0 to High(LineSnapshots) do
    begin
      if AnalysisCancelled then
        Exit;
      if (RefineLineIndex < 0) or (RefineLineIndex > High(RefinedLines)) then
        Exit;
      if RefinedLines[RefineLineIndex] then
        Continue;
      if (Length(LineSnapshots[RefineLineIndex].Notes) = 0) or
         (LineSnapshots[RefineLineIndex].HighNote < 0) then
      begin
        RefinedLines[RefineLineIndex] := true;
        Continue;
      end;

      if not ForceAll then
      begin
        if GetTimeFromBeat(LineSnapshots[RefineLineIndex].EndBeat) +
           (VOCALS_PITCH_ANALYSIS_WINDOW * 0.5) / VOCALS_WAVEFORM_SAMPLE_RATE > MaxTimeSec then
          Continue;
      end;

      RefineStartSec := GetTimeFromBeat(LineSnapshots[RefineLineIndex].Notes[0].StartBeat) -
        (VOCALS_PITCH_ANALYSIS_WINDOW * 0.5) / VOCALS_WAVEFORM_SAMPLE_RATE;
      if RefineStartSec < 0 then
        RefineStartSec := 0;
      RefineEndSec := GetTimeFromBeat(LineSnapshots[RefineLineIndex].EndBeat) +
        (VOCALS_PITCH_ANALYSIS_WINDOW * 0.5) / VOCALS_WAVEFORM_SAMPLE_RATE;
      RefineStartSample := Floor(RefineStartSec * VOCALS_WAVEFORM_SAMPLE_RATE);
      RefineEndSample := Ceil(RefineEndSec * VOCALS_WAVEFORM_SAMPLE_RATE);

      if RefineEndSample <= RefineStartSample then
      begin
        RefinedLines[RefineLineIndex] := true;
        Continue;
      end;

      if not ForceAll and (RefineEndSample > PreviewNextSample) then
        Continue;

      RefineSampleCount := RefineEndSample - RefineStartSample;
      if RefineSampleCount < VOCALS_PITCH_ANALYSIS_WINDOW then
      begin
        RefinedLines[RefineLineIndex] := true;
        Continue;
      end;

      SetLength(RefineSamples, RefineSampleCount);
      EnterCriticalSection(FAnalysisDataLock);
      try
        if RefineEndSample > Length(FVocalsPreviewSamples) then
          RefineSampleCount := Length(FVocalsPreviewSamples) - RefineStartSample;
        if RefineSampleCount < VOCALS_PITCH_ANALYSIS_WINDOW then
          Continue;
        SetLength(RefineSamples, RefineSampleCount);
        for LocalSampleIndex := 0 to RefineSampleCount - 1 do
          RefineSamples[LocalSampleIndex] := FVocalsPreviewSamples[RefineStartSample + LocalSampleIndex] / 32767.0;
      finally
        LeaveCriticalSection(FAnalysisDataLock);
      end;

      RefineFrameStart := RefineStartSample div VOCALS_PITCH_ANALYSIS_HOP;
      RefineFrameEnd := (RefineEndSample - VOCALS_PITCH_ANALYSIS_WINDOW) div VOCALS_PITCH_ANALYSIS_HOP;
      if RefineFrameEnd > High(WorkingContour) then
        RefineFrameEnd := High(WorkingContour);

      for LocalSampleIndex := RefineFrameStart to RefineFrameEnd do
      begin
        if AnalysisCancelled then
          Exit;
        RefineLocalStart := LocalSampleIndex * VOCALS_PITCH_ANALYSIS_HOP - RefineStartSample;
        if (RefineLocalStart < 0) or
           (RefineLocalStart + VOCALS_PITCH_ANALYSIS_WINDOW > Length(RefineSamples)) then
          Continue;
        if DetectPitchTone(RefineSamples, RefineLocalStart, RefineTone) then
          WorkingContour[LocalSampleIndex] := RefineTone
        else
          WorkingContour[LocalSampleIndex] := VOCALS_PITCH_INVALID;
      end;

      RefinedLines[RefineLineIndex] := true;
    end;
  end;

  procedure RecomputeScoresFromContour(const Contour: TSingleDynArray; const MaxTimeSec: Double;
    const MarkComplete: Boolean);
  var
    ScoreLineIndex: Integer;
    ScoreNoteIndex: Integer;
    ScoreBeatIndex: Integer;
    LineReady: Boolean;
  begin
    EnsurePreviewScoreCache(Track);
    Difficulty := GetEditorScoreDifficulty;
    Range := 2 - Difficulty;

    EnterCriticalSection(FAnalysisDataLock);
    try
      for ScoreLineIndex := 0 to High(LineSnapshots) do
      begin
        if AnalysisCancelled then
          Exit;
        if (ScoreTrackValueSnapshot <= 0) or
           (LineSnapshots[ScoreLineIndex].ScoreValue <= 0) or
           (Length(LineSnapshots[ScoreLineIndex].Notes) = 0) or
           (LineSnapshots[ScoreLineIndex].HighNote < 0) then
        begin
          if Track <= High(FPreviewLineScoreReadyByTrack) then
            FPreviewLineScoreReadyByTrack[Track][ScoreLineIndex] := true;
          if FPreviewScoreTrack = Track then
            FPreviewLineScoreReady[ScoreLineIndex] := true;
          Continue;
        end;

        LineReady := MarkComplete or
          (GetTimeFromBeat(LineSnapshots[ScoreLineIndex].EndBeat) +
           (VOCALS_PITCH_ANALYSIS_WINDOW * 0.5) / VOCALS_WAVEFORM_SAMPLE_RATE <= MaxTimeSec);
        if not LineReady then
        begin
          if Track <= High(FPreviewLineScoreReadyByTrack) then
            FPreviewLineScoreReadyByTrack[Track][ScoreLineIndex] := false;
          if FPreviewScoreTrack = Track then
            FPreviewLineScoreReady[ScoreLineIndex] := false;
          Continue;
        end;

        if Track <= High(FPreviewLineNormalScoreByTrack) then
        begin
          FPreviewLineNormalScoreByTrack[Track][ScoreLineIndex] := 0;
          FPreviewLineGoldenScoreByTrack[Track][ScoreLineIndex] := 0;
          FPreviewLineBonusScoreByTrack[Track][ScoreLineIndex] := 0;
        end;
        if FPreviewScoreTrack = Track then
        begin
          FPreviewLineNormalScore[ScoreLineIndex] := 0;
          FPreviewLineGoldenScore[ScoreLineIndex] := 0;
          FPreviewLineBonusScore[ScoreLineIndex] := 0;
        end;

        DetectedNormal := 0;
        DetectedGolden := 0;
        LineDetectedScore := 0;
        LineMaxScore := (MAX_SONG_SCORE - MAX_SONG_LINE_BONUS) *
          (LineSnapshots[ScoreLineIndex].ScoreValue / ScoreTrackValueSnapshot);

        for ScoreNoteIndex := 0 to High(LineSnapshots[ScoreLineIndex].Notes) do
        begin
          if AnalysisCancelled then
            Exit;

          NotePointsPerBeat := (MAX_SONG_SCORE - MAX_SONG_LINE_BONUS) /
            ScoreTrackValueSnapshot * ScoreFactor[LineSnapshots[ScoreLineIndex].Notes[ScoreNoteIndex].NoteType];

          for ScoreBeatIndex := LineSnapshots[ScoreLineIndex].Notes[ScoreNoteIndex].StartBeat to
                                LineSnapshots[ScoreLineIndex].Notes[ScoreNoteIndex].StartBeat +
                                LineSnapshots[ScoreLineIndex].Notes[ScoreNoteIndex].Duration - 1 do
          begin
            if AnalysisCancelled then
              Exit;

            TimeSec := (GetTimeFromBeat(ScoreBeatIndex) + GetTimeFromBeat(ScoreBeatIndex + 1)) * 0.5;
            if TimeSec > MaxTimeSec then
              Continue;
            if GetContourToneAtTime(Contour, TimeSec, PitchTone) then
            begin
              while PitchTone - LineSnapshots[ScoreLineIndex].Notes[ScoreNoteIndex].Tone > 6 do
                PitchTone := PitchTone - 12;
              while PitchTone - LineSnapshots[ScoreLineIndex].Notes[ScoreNoteIndex].Tone < -6 do
                PitchTone := PitchTone + 12;

              if (LineSnapshots[ScoreLineIndex].Notes[ScoreNoteIndex].NoteType in [ntRap, ntRapGolden]) or
                 (Abs(LineSnapshots[ScoreLineIndex].Notes[ScoreNoteIndex].Tone - PitchTone) <= Range) then
              begin
                case LineSnapshots[ScoreLineIndex].Notes[ScoreNoteIndex].NoteType of
                  ntNormal, ntRap:
                    DetectedNormal := DetectedNormal + NotePointsPerBeat;
                  ntGolden, ntRapGolden:
                    DetectedGolden := DetectedGolden + NotePointsPerBeat;
                end;
                LineDetectedScore := LineDetectedScore + NotePointsPerBeat;
              end;
            end;
          end;
        end;

        if LineMaxScore <= 2 then
          LinePerfection := 1
        else
          LinePerfection := LineDetectedScore / (LineMaxScore - 2);
        if LinePerfection < 0 then
          LinePerfection := 0
        else if LinePerfection > 1 then
          LinePerfection := 1;

        if NonEmptyLineCountSnapshot > 0 then
          LineBonus := MAX_SONG_LINE_BONUS / NonEmptyLineCountSnapshot
        else
          LineBonus := 0;
        DetectedLineBonus := LineBonus * LinePerfection;

        if Track <= High(FPreviewLineNormalScoreByTrack) then
        begin
          FPreviewLineNormalScoreByTrack[Track][ScoreLineIndex] := DetectedNormal;
          FPreviewLineGoldenScoreByTrack[Track][ScoreLineIndex] := DetectedGolden;
          FPreviewLineBonusScoreByTrack[Track][ScoreLineIndex] := DetectedLineBonus;
          FPreviewLineScoreReadyByTrack[Track][ScoreLineIndex] := true;
        end;
        if FPreviewScoreTrack = Track then
        begin
          FPreviewLineNormalScore[ScoreLineIndex] := DetectedNormal;
          FPreviewLineGoldenScore[ScoreLineIndex] := DetectedGolden;
          FPreviewLineBonusScore[ScoreLineIndex] := DetectedLineBonus;
          FPreviewLineScoreReady[ScoreLineIndex] := true;
        end;
      end;
    finally
      LeaveCriticalSection(FAnalysisDataLock);
    end;
  end;

begin
  if (Track < 0) or (Track > High(CurrentSong.Tracks)) or Error then
    Exit;
  if AnalysisCancelled then
    Exit;

  if not BuildPreview then
  begin
    EnterCriticalSection(FAnalysisDataLock);
    try
      WorkingContour := Copy(FVocalsPitchContour);
    finally
      LeaveCriticalSection(FAnalysisDataLock);
    end;
    if Length(WorkingContour) > 0 then
    begin
      LineSnapshots := Copy(CurrentSong.Tracks[Track].Lines);
      SetLength(RefinedLines, Length(LineSnapshots));
      ScoreTrackValueSnapshot := CurrentSong.Tracks[Track].ScoreValue;
      NonEmptyLineCountSnapshot := 0;
      for SnapshotLineIndex := 0 to High(LineSnapshots) do
      begin
        if AnalysisCancelled then
          Exit;
        if LineSnapshots[SnapshotLineIndex].ScoreValue > 0 then
          Inc(NonEmptyLineCountSnapshot);
      end;
      RecomputeScoresFromContour(WorkingContour,
        FVocalsPitchStartSec + High(WorkingContour) * FVocalsPitchStepSec,
        FPreviewBuildComplete);
    end;
    FAnalysisProgressPct := 100;
    Exit;
  end;

  ClearVocalsWaveformCache(false);
  FUsingVocalsPreviewPlayback := false;
  OriginalStream := nil;
  KaraokeStream := nil;
  OriginalConverter := nil;
  KaraokeConverter := nil;
  TargetFormat := nil;
  PendingStartSample := 0;
  NextPitchStartSample := 0;
  PreviewNextSample := 0;
  OriginalEOF := false;
  KaraokeEOF := false;
  HasKaraokePreview := false;
  UseDirectVocalsSource := false;
  HopsSinceScoreUpdate := 0;
  HaveLastPreviewSample := false;
  SetLength(PendingSamples, 0);
  SetLength(WorkingContour, 0);

  try
    UseDirectVocalsSource := HasVocalsPreviewDirectSource;
    if UseDirectVocalsSource then
    begin
      KaraokeStream := OpenDecodeStream(CurrentSong.Path.Append(CurrentSong.Vocals));
      if not Assigned(KaraokeStream) then
        Exit;
    end
    else
    begin
      OriginalStream := OpenDecodeStream(CurrentSong.Path.Append(CurrentSong.Audio));
      if not Assigned(OriginalStream) then
        Exit;
      if HasVocalsPreviewSource then
        KaraokeStream := OpenDecodeStream(CurrentSong.Path.Append(GetVocalsPreviewSourcePath));
    end;
    HasKaraokePreview := Assigned(KaraokeStream);

    TargetFormat := TAudioFormatInfo.Create(1, VOCALS_WAVEFORM_SAMPLE_RATE, asfFloat);
    if Assigned(OriginalStream) then
    begin
      OriginalConverter := TAudioConverter_SWResample.Create();
      if not OriginalConverter.Init(OriginalStream.GetAudioFormatInfo(), TargetFormat) then
        Exit;
    end;
    if HasKaraokePreview then
    begin
      KaraokeConverter := TAudioConverter_SWResample.Create();
      if not KaraokeConverter.Init(KaraokeStream.GetAudioFormatInfo(), TargetFormat) then
        Exit;
    end;

    LineSnapshots := Copy(CurrentSong.Tracks[Track].Lines);
    SetLength(RefinedLines, Length(LineSnapshots));
    ScoreTrackValueSnapshot := CurrentSong.Tracks[Track].ScoreValue;
    NonEmptyLineCountSnapshot := 0;
    for SnapshotLineIndex := 0 to High(LineSnapshots) do
    begin
      if AnalysisCancelled then
        Exit;
      if LineSnapshots[SnapshotLineIndex].ScoreValue > 0 then
        Inc(NonEmptyLineCountSnapshot);
    end;
    EnsurePreviewScoreCache(Track);
    EnterCriticalSection(FAnalysisDataLock);
    try
      for SnapshotLineIndex := 0 to High(LineSnapshots) do
      begin
        if Track <= High(FPreviewLineNormalScoreByTrack) then
        begin
          FPreviewLineNormalScoreByTrack[Track][SnapshotLineIndex] := 0;
          FPreviewLineGoldenScoreByTrack[Track][SnapshotLineIndex] := 0;
          FPreviewLineBonusScoreByTrack[Track][SnapshotLineIndex] := 0;
          FPreviewLineScoreReadyByTrack[Track][SnapshotLineIndex] := false;
        end;

        if FPreviewScoreTrack = Track then
        begin
          FPreviewLineNormalScore[SnapshotLineIndex] := 0;
          FPreviewLineGoldenScore[SnapshotLineIndex] := 0;
          FPreviewLineBonusScore[SnapshotLineIndex] := 0;
          FPreviewLineScoreReady[SnapshotLineIndex] := false;
        end;
      end;
    finally
      LeaveCriticalSection(FAnalysisDataLock);
    end;

    if Assigned(OriginalStream) then
      SongLengthSec := OriginalStream.Length
    else if HasKaraokePreview then
      SongLengthSec := KaraokeStream.Length
    else
      SongLengthSec := 0;
    if HasKaraokePreview and (KaraokeStream.Length > SongLengthSec) then
      SongLengthSec := KaraokeStream.Length;
    if Assigned(AudioPlayback) and (AudioPlayback.Length > SongLengthSec) then
      SongLengthSec := AudioPlayback.Length;
    SongTotalSamples := Ceil(SongLengthSec * VOCALS_WAVEFORM_SAMPLE_RATE);
    if HasKaraokePreview then
    begin
      EnterCriticalSection(FAnalysisDataLock);
      try
        if SongTotalSamples > High(Integer) then
          SetLength(FVocalsPreviewSamples, High(Integer))
        else
          SetLength(FVocalsPreviewSamples, SongTotalSamples);
      finally
        LeaveCriticalSection(FAnalysisDataLock);
      end;
    end;
    if Assigned(OriginalConverter) then
    begin
      SetLength(InputBytesOriginal, VOCALS_WAVEFORM_DECODE_BYTES);
      SetLength(OutputBytesOriginal, OriginalConverter.GetOutputBufferSize(Length(InputBytesOriginal)));
    end;
    if HasKaraokePreview then
    begin
      SetLength(InputBytesKaraoke, VOCALS_WAVEFORM_DECODE_BYTES);
      SetLength(OutputBytesKaraoke, KaraokeConverter.GetOutputBufferSize(Length(InputBytesKaraoke)));
    end;

    EnterCriticalSection(FAnalysisDataLock);
    try
      SetLength(FVocalsPitchContour, 0);
      FVocalsPitchStepSec := VOCALS_PITCH_ANALYSIS_HOP / 1.0 / VOCALS_WAVEFORM_SAMPLE_RATE;
      FVocalsPitchStartSec := (VOCALS_PITCH_ANALYSIS_WINDOW * 0.5) / 1.0 / VOCALS_WAVEFORM_SAMPLE_RATE;
      FVocalsPreviewReadySamples := 0;
    finally
      LeaveCriticalSection(FAnalysisDataLock);
    end;

    if UseDirectVocalsSource then
      OriginalEOF := true;

    while true do
    begin
      if AnalysisCancelled then
        Exit;

      ReadBytesOriginal := 0;
      ReadBytesKaraoke := 0;
      EnterCriticalSection(FAnalysisDecodeLock);
      try
        if not OriginalEOF then
          ReadBytesOriginal := OriginalStream.ReadData(@InputBytesOriginal[0], Length(InputBytesOriginal));
        if HasKaraokePreview and (not KaraokeEOF) then
          ReadBytesKaraoke := KaraokeStream.ReadData(@InputBytesKaraoke[0], Length(InputBytesKaraoke));
      finally
        LeaveCriticalSection(FAnalysisDecodeLock);
      end;

      if ReadBytesOriginal <= 0 then
        OriginalEOF := true;
      if HasKaraokePreview and (ReadBytesKaraoke <= 0) then
        KaraokeEOF := true;
      if not HasKaraokePreview then
        KaraokeEOF := true;

      if OriginalEOF and KaraokeEOF and
         (UseDirectVocalsSource or (Length(CarryOriginal) = 0)) and
         ((not HasKaraokePreview) or (Length(CarryKaraoke) = 0)) then
        Break;

      InputSizeOriginal := ReadBytesOriginal;
      InputSizeKaraoke := ReadBytesKaraoke;
      if InputSizeOriginal > 0 then
        OutputSizeOriginal := OriginalConverter.Convert(@InputBytesOriginal[0], @OutputBytesOriginal[0], InputSizeOriginal)
      else
        OutputSizeOriginal := 0;
      if HasKaraokePreview and (InputSizeKaraoke > 0) then
        OutputSizeKaraoke := KaraokeConverter.Convert(@InputBytesKaraoke[0], @OutputBytesKaraoke[0], InputSizeKaraoke)
      else
        OutputSizeKaraoke := 0;
      FramesDecodedOriginal := OutputSizeOriginal div SizeOf(Single);
      FramesDecodedKaraoke := OutputSizeKaraoke div SizeOf(Single);
      if (FramesDecodedOriginal <= 0) and (FramesDecodedKaraoke <= 0) and
         ((not OriginalEOF) or (not KaraokeEOF) or
          ((not UseDirectVocalsSource) and (Length(CarryOriginal) = 0)) or
          (HasKaraokePreview and (Length(CarryKaraoke) = 0))) then
      begin
        Sleep(1);
        Continue;
      end;

      if (FramesDecodedOriginal > 0) and not UseDirectVocalsSource then
      begin
        SetLength(OriginalSamples, FramesDecodedOriginal);
        Move(OutputBytesOriginal[0], OriginalSamples[0], FramesDecodedOriginal * SizeOf(Single));
        AppendSamples(CarryOriginal, OriginalSamples, FramesDecodedOriginal);
      end;

      if HasKaraokePreview and (FramesDecodedKaraoke > 0) then
      begin
        SetLength(KaraokeSamples, FramesDecodedKaraoke);
        Move(OutputBytesKaraoke[0], KaraokeSamples[0], FramesDecodedKaraoke * SizeOf(Single));
        AppendSamples(CarryKaraoke, KaraokeSamples, FramesDecodedKaraoke);
      end;

      if UseDirectVocalsSource then
        SampleCount := Length(CarryKaraoke)
      else if HasKaraokePreview then
        SampleCount := Min(Length(CarryOriginal), Length(CarryKaraoke))
      else
        SampleCount := Length(CarryOriginal);
      if SampleCount <= 0 then
      begin
        if OriginalEOF and KaraokeEOF then
          Break;
        Sleep(1);
        Continue;
      end;

      if HasKaraokePreview and not UseDirectVocalsSource then
      begin
        SetLength(DiffSamples, SampleCount);
        for SampleIndex := 0 to SampleCount - 1 do
          DiffSamples[SampleIndex] := CarryOriginal[SampleIndex] - CarryKaraoke[SampleIndex];
        AnalysisSamples := DiffSamples;
      end
      else
      begin
        SetLength(AnalysisSamples, SampleCount);
        if UseDirectVocalsSource then
          Move(CarryKaraoke[0], AnalysisSamples[0], SampleCount * SizeOf(Single))
        else
          Move(CarryOriginal[0], AnalysisSamples[0], SampleCount * SizeOf(Single));
      end;
      if not UseDirectVocalsSource then
        ConsumeLeadingSamples(CarryOriginal, SampleCount);
      if HasKaraokePreview then
        ConsumeLeadingSamples(CarryKaraoke, SampleCount);

      if HasKaraokePreview and HaveLastPreviewSample and (SampleCount > 0) then
      begin
        BoundaryDelta := AnalysisSamples[0] - LastPreviewSample;
        SmoothCount := Min(PREVIEW_BOUNDARY_SMOOTH_SAMPLES, SampleCount);
        if SmoothCount > 0 then
          for SampleIndex := 0 to SmoothCount - 1 do
          begin
            SmoothFactor := (SmoothCount - SampleIndex) / SmoothCount;
            AnalysisSamples[SampleIndex] := AnalysisSamples[SampleIndex] - BoundaryDelta * SmoothFactor;
          end;
      end;

      if HasKaraokePreview then
      begin
        EnterCriticalSection(FAnalysisDataLock);
        try
          WrittenSamples := 0;
          for SampleIndex := 0 to SampleCount - 1 do
          begin
            if PreviewNextSample + SampleIndex > High(FVocalsPreviewSamples) then
              Break;
            PreviewWriteIndex := PreviewNextSample + SampleIndex;
            PreviewSample16 := Round(EnsureRange(AnalysisSamples[SampleIndex], -1.0, 1.0) * 32767);
            FVocalsPreviewSamples[PreviewWriteIndex] := PreviewSample16;
            Inc(WrittenSamples);
          end;
          if PreviewNextSample + WrittenSamples > FVocalsPreviewReadySamples then
            FVocalsPreviewReadySamples := PreviewNextSample + WrittenSamples;
        finally
          LeaveCriticalSection(FAnalysisDataLock);
        end;

        if SampleCount > 0 then
        begin
          LastPreviewSample := AnalysisSamples[SampleCount - 1];
          HaveLastPreviewSample := true;
          Inc(PreviewNextSample, SampleCount);
        end;
      end;

      AppendSamples(PendingSamples, AnalysisSamples, SampleCount);

      while NextPitchStartSample + VOCALS_PITCH_ANALYSIS_WINDOW <= PendingStartSample + Length(PendingSamples) do
      begin
        if AnalysisCancelled then
          Exit;
        LocalStartIndex := NextPitchStartSample - PendingStartSample;
        SetLength(WorkingContour, Length(WorkingContour) + 1);
        if DetectPitchTone(PendingSamples, LocalStartIndex, PitchTone) then
          WorkingContour[High(WorkingContour)] := PitchTone
        else
          WorkingContour[High(WorkingContour)] := VOCALS_PITCH_INVALID;

        Inc(NextPitchStartSample, VOCALS_PITCH_ANALYSIS_HOP);
        Inc(HopsSinceScoreUpdate);

        if HopsSinceScoreUpdate >= ANALYSIS_PUBLISH_HOPS then
        begin
          if Length(WorkingContour) > 0 then
            AvailableUntilSec := FVocalsPitchStartSec + High(WorkingContour) * FVocalsPitchStepSec
          else
            AvailableUntilSec := 0;

          if HasKaraokePreview then
            RefineCompletedLines(AvailableUntilSec, false);

          EnterCriticalSection(FAnalysisDataLock);
          try
            FVocalsPitchContour := Copy(WorkingContour);
          finally
            LeaveCriticalSection(FAnalysisDataLock);
          end;

          RecomputeScoresFromContour(WorkingContour, AvailableUntilSec, false);
          if SongTotalSamples > 0 then
            UpdateAnalysisProgress(Round((NextPitchStartSample * 100.0) / SongTotalSamples));
          HopsSinceScoreUpdate := 0;
          Sleep(1);
          if AnalysisCancelled then
            Exit;
        end;
      end;

      TrimCount := NextPitchStartSample - PendingStartSample;
      if TrimCount > 0 then
      begin
        if TrimCount > Length(PendingSamples) then
          TrimCount := Length(PendingSamples);
        if TrimCount < Length(PendingSamples) then
          Move(PendingSamples[TrimCount], PendingSamples[0], (Length(PendingSamples) - TrimCount) * SizeOf(Single));
        SetLength(PendingSamples, Length(PendingSamples) - TrimCount);
        PendingStartSample := NextPitchStartSample;
      end;
    end;

    EnterCriticalSection(FAnalysisDataLock);
    try
      FVocalsPitchContour := Copy(WorkingContour);
    finally
      LeaveCriticalSection(FAnalysisDataLock);
    end;

    if Length(WorkingContour) > 0 then
      AvailableUntilSec := FVocalsPitchStartSec + High(WorkingContour) * FVocalsPitchStepSec
    else
      AvailableUntilSec := 0;
    if AnalysisCancelled then
      Exit;
    if HasKaraokePreview then
      RefineCompletedLines(AvailableUntilSec, true);
    FPreviewBuildComplete := true;
    RecomputeScoresFromContour(WorkingContour, AvailableUntilSec, true);
    for RescoreTrackIndex := 0 to High(CurrentSong.Tracks) do
    begin
      if AnalysisCancelled then
        Exit;
      if RescoreTrackIndex = Track then
        Continue;
      RefreshPreviewScoreMetadata(RescoreTrackIndex);
      RunIncrementalVocalsAnalysis(false, RescoreTrackIndex);
    end;
    UpdateAnalysisProgress(100);
  finally
    if Assigned(OriginalStream) then
      OriginalStream.Free;
    if Assigned(KaraokeStream) then
      KaraokeStream.Free;
    FreeAndNil(OriginalConverter);
    FreeAndNil(KaraokeConverter);
    FreeAndNil(TargetFormat);
  end;
end;

procedure TScreenEditSub.DrawVocalsWaveform(const X, Y, W, H: real; const Track: Integer);
var
  Contour: array of Single;
  PitchStepSec: Single;
  PitchStartSec: Single;
  LineIndex: Integer;
  PitchIndex: Integer;
  TimeStart: real;
  TimeEnd: real;
  XPos: real;
  InSegment: Boolean;
  NoteSpace: real;
  BaseNoteY: real;
  PitchY: real;
  PitchTime: real;
  PitchTone: Single;
  PitchTop: real;
  PitchBottom: real;
begin
  EnsureCurrentLineWaveformCache;
  if (Track < 0) or (Track > High(CurrentSong.Tracks)) then
    Exit;
  if (W <= 2) or (H <= 2) then
    Exit;

  EnterCriticalSection(FAnalysisDataLock);
  try
    Contour := Copy(FVocalsPitchContour);
    PitchStepSec := FVocalsPitchStepSec;
    PitchStartSec := FVocalsPitchStartSec;
  finally
    LeaveCriticalSection(FAnalysisDataLock);
  end;
  if Length(Contour) <= 0 then
    Exit;

  LineIndex := CurrentSong.Tracks[Track].CurrentLine;
  if (LineIndex < 0) or (LineIndex > High(CurrentSong.Tracks[Track].Lines)) or
     (CurrentSong.Tracks[Track].Lines[LineIndex].HighNote < 0) then
    Exit;

  TimeStart := GetTimeFromBeat(CurrentSong.Tracks[Track].Lines[LineIndex].Notes[0].StartBeat);
  TimeEnd := GetTimeFromBeat(CurrentSong.Tracks[Track].Lines[LineIndex].EndBeat);
  if TimeStart < 0 then
    TimeStart := 0;
  if TimeEnd < 0 then
    TimeEnd := 0;
  if TimeEnd <= TimeStart then
    Exit;

  glPushAttrib(GL_ENABLE_BIT or GL_COLOR_BUFFER_BIT or GL_LINE_BIT);
  try
    glDisable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    NoteSpace := H / 9;
    BaseNoteY := Y + 7 * NoteSpace;
    PitchTop := Y;
    PitchBottom := Y + H;

    glLineWidth(5.0);
    glColor4f(0.55, 0.0, 0.0, 0.10);
    InSegment := false;
    for PitchIndex := 0 to High(Contour) do
    begin
      PitchTone := Contour[PitchIndex];
      PitchTime := PitchStartSec + PitchIndex * PitchStepSec;
      if (PitchTone = VOCALS_PITCH_INVALID) or (PitchTime < TimeStart) or (PitchTime > TimeEnd) then
      begin
        if InSegment then
        begin
          glEnd;
          InSegment := false;
        end;
        Continue;
      end;

      XPos := X + ((PitchTime - TimeStart) / (TimeEnd - TimeStart)) * W;
      PitchY := BaseNoteY - (PitchTone - CurrentSong.Tracks[Track].Lines[LineIndex].BaseNote) * NoteSpace / 2;
      if (PitchY < PitchTop) or (PitchY > PitchBottom) then
      begin
        if InSegment then
        begin
          glEnd;
          InSegment := false;
        end;
        Continue;
      end;
      if not InSegment then
      begin
        glBegin(GL_LINE_STRIP);
        InSegment := true;
      end;
      glVertex2f(XPos, PitchY);
    end;
    if InSegment then
      glEnd;

    glLineWidth(2.5);
    glColor4f(1.0, 0.0, 0.0, 0.35);
    InSegment := false;
    for PitchIndex := 0 to High(Contour) do
    begin
      PitchTone := Contour[PitchIndex];
      PitchTime := PitchStartSec + PitchIndex * PitchStepSec;
      if (PitchTone = VOCALS_PITCH_INVALID) or (PitchTime < TimeStart) or (PitchTime > TimeEnd) then
      begin
        if InSegment then
        begin
          glEnd;
          InSegment := false;
        end;
        Continue;
      end;

      XPos := X + ((PitchTime - TimeStart) / (TimeEnd - TimeStart)) * W;
      PitchY := BaseNoteY - (PitchTone - CurrentSong.Tracks[Track].Lines[LineIndex].BaseNote) * NoteSpace / 2;
      if (PitchY < PitchTop) or (PitchY > PitchBottom) then
      begin
        if InSegment then
        begin
          glEnd;
          InSegment := false;
        end;
        Continue;
      end;
      if not InSegment then
      begin
        glBegin(GL_LINE_STRIP);
        InSegment := true;
      end;
      glVertex2f(XPos, PitchY);
    end;
    if InSegment then
      glEnd;
  finally
    glPopAttrib;
  end;
end;

procedure TScreenEditSub.DrawInfoBar(X, Y, W, H: Integer; ColR, ColG, ColB, Alpha: real; Track: Integer);
var
  StartBeat:    Integer;
  EndBeat:      Integer;
  SongStart:    Integer;
  SongEnd:      Integer;
  SongDuration: Integer;
  i:            Integer;
  Color:        TRGB;

  CurrentPos: real;
  Width:      real;

  LineIndex: Integer;
  numLines:  Integer;

  function FindStartBeat(): Integer;
  var
    TrackIndex: Integer;
    LineIndex:  Integer;
  begin
    Result := High(Integer);

    for TrackIndex := 0 to High(CurrentSong.Tracks) do
      for LineIndex := 0 to High(CurrentSong.Tracks[TrackIndex].Lines) do
      begin
        if (Length(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes) > 0) then
        begin
          if(Result > CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[0].StartBeat) then
            Result := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[0].StartBeat;
        end;
      end;
  end;

  function FindEndBeat(): Integer;
  var
    TrackIndex: Integer;
    LineIndex:  Integer;
    NoteIndex:  Integer;
  begin
    Result := Low(Integer);

    for TrackIndex := 0 to High(CurrentSong.Tracks) do
      for LineIndex := 0 to High(CurrentSong.Tracks[TrackIndex].Lines) do
      begin
        if (Length(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes)>0) then
        begin
          NoteIndex := Length(CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes) - 1;
          if(Result < CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat + CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration) then
            Result := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat + CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration;
        end;
      end;
  end;

begin
  numLines := Length(CurrentSong.Tracks[Track].Lines);

  if (Track = CurrentTrack) then
    EditDrawBorderedBox(X, Y, W, H, ColR, ColG, ColB, Alpha)
  else
    EditDrawBorderedBox(X, Y, W, H, ColR, ColG, ColB, Alpha);

  if(numLines < 1) then
    Exit;

  SongStart := FindStartBeat;
  SongEnd := FindEndBeat;
  SongDuration := SongEnd - SongStart;

  Statics[playerIconId[Track+1]].Visible := true;

  for i := 0 to High(TransparentLineButtonId) do
  begin
    Button[TransparentLineButtonId[i]].SetX(0);
    Button[TransparentLineButtonId[i]].SetY(0);
    Button[TransparentLineButtonId[i]].SetW(0);
    Button[TransparentLineButtonId[i]].SetH(0);
  end;

  while (Length(TransparentLineButtonId) < numLines) do
  begin
    SetLength(InteractiveLineId, Length(InteractiveLineId) + 1);
    SetLength(TransparentLineButtonId, Length(TransparentLineButtonId) + 1);
    TransparentLineButtonId[Length(TransparentLineButtonId) - 1] := AddButton(0, 0, 0, 0,PATH_NONE);
    //TransparentLineButtonId[Length(TransparentLineButtonId) - 1] := AddButton(0, 0, 0, 0, Skin.GetTextureFileName(Theme.Main.Buttonsolo.Tex));
    InteractiveLineId[Length(InteractiveLineId) - 1] := Length(Interactions) - 1;
  end;

  for LineIndex := 0 to numLines - 1 do
  begin
    if (LineIndex = CurrentSong.Tracks[Track].CurrentLine) and not (PlaySentence or PlaySentenceMidi or PlayOne) then
      glColor4f(1, 0.6, 0, 1) // currently selected line in orange
    else
      if (CurrentSong.Medley.Source <> msNone) and
         (MedleyNotes.isStart) and (MedleyNotes.isEnd) and
         (LineIndex >= MedleyNotes.start.line) and (LineIndex <= MedleyNotes.end_.line) then
        // medley section in green
        glColor4f(0.15, 0.75, 0.15, 1)
      else
      begin
        // all other lines in orange (current track) and gray (other track)
        if (Track = CurrentTrack) then
        begin
          Color := GetPlayerColor(Ini.SingColor[CurrentTrack]);
          glColor4f(Color.R, Color.G, Color.B, 1)
        end
        else
          glColor4f(0.7, 0.7, 0.7, 1);
      end;

    StartBeat := CurrentSong.Tracks[Track].Lines[LineIndex].Notes[0].StartBeat;
    EndBeat := CurrentSong.Tracks[Track].Lines[LineIndex].Notes[CurrentSong.Tracks[Track].Lines[LineIndex].HighNote].StartBeat + CurrentSong.Tracks[Track].Lines[LineIndex].Notes[CurrentSong.Tracks[Track].Lines[LineIndex].HighNote].Duration;

    CurrentPos := (StartBeat - SongStart) / SongDuration * W;
    Width := (EndBeat - StartBeat) / SongDuration * W;

    // todo: add transparent active button to change current line
    Button[TransparentLineButtonId[LineIndex]].SetX(X + CurrentPos);
    Button[TransparentLineButtonId[LineIndex]].SetY(Y);
    Button[TransparentLineButtonId[LineIndex]].SetW(Width);
    Button[TransparentLineButtonId[LineIndex]].SetH(H);

    glbegin(gl_quads);
      glVertex2f(X + CurrentPos, Y);
      glVertex2f(X + CurrentPos, Y + H);
      glVertex2f(X + CurrentPos + Width, Y + H);
      glVertex2f(X + CurrentPos + Width, Y);
    glEnd;
  end;

  if(PlaySentence or PlaySentenceMidi or PlayOne) then
  begin
    glColor4f(0, 0, 0, 0.5);
    CurrentPos := 0;
    Width := (CurrentBeat - SongStart) / SongDuration * W;
    if (Width > W) then
      Width := W;
  end else
  begin
    glColor4f(1, 0, 0, 1);
    CurrentPos := (CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[CurrentNote[Track]].StartBeat - SongStart) / SongDuration * W;
    Width := CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[CurrentNote[Track]].Duration / SongDuration * W;
    if (Width < 1) then
      Width := 1;
  end;

  glEnable(GL_BLEND);
  glbegin(gl_quads);
    glVertex2f(X + CurrentPos, Y);
    glVertex2f(X + CurrentPos, Y + H);
    glVertex2f(X + CurrentPos + Width, Y + H);
    glVertex2f(X + CurrentPos + Width, Y);
  glEnd;
  glDisable(GL_BLEND);

  glLineWidth(1);
end;

procedure TScreenEditSub.DrawMaxScoreInfo(X, Y, W, H: Integer; Track: Integer);
var
  LineIndex: Integer;
  NoteIndex: Integer;
  HasReadyPreviewLine: Boolean;
  NonEmptyLines: Integer;
  LineWeight: Integer;
  TotalWeight: Integer;
  NormalWeight: Integer;
  GoldenWeight: Integer;
  MaxSongPoints: Integer;
  NormalPoints: Integer;
  GoldenPoints: Integer;
  LineBonusPoints: Integer;
  TotalPoints: Integer;
  PreviewNormalPoints: Integer;
  PreviewGoldenPoints: Integer;
  PreviewLineBonusPoints: Integer;
  PreviewTotalPoints: Integer;
  PreviewNormalScore: Double;
  PreviewGoldenScore: Double;
  PreviewLineBonusScore: Double;
  PreviewAvailable: Boolean;
  Difficulty: Integer;
  ReadyLines: Integer;
  AnalysisPercent: Integer;
  Lines: array[0..4] of UTF8String;
  OrgFont: TFont;
begin
  if (Track < Low(CurrentSong.Tracks)) or (Track > High(CurrentSong.Tracks)) then
    Exit;

  Difficulty := GetEditorScoreDifficulty;

  NonEmptyLines := 0;
  TotalWeight := 0;
  NormalWeight := 0;
  GoldenWeight := 0;

  for LineIndex := 0 to High(CurrentSong.Tracks[Track].Lines) do
  begin
    LineWeight := 0;

    for NoteIndex := 0 to High(CurrentSong.Tracks[Track].Lines[LineIndex].Notes) do
    begin
      Inc(
        LineWeight,
        CurrentSong.Tracks[Track].Lines[LineIndex].Notes[NoteIndex].Duration *
        ScoreFactor[CurrentSong.Tracks[Track].Lines[LineIndex].Notes[NoteIndex].NoteType]);

      case CurrentSong.Tracks[Track].Lines[LineIndex].Notes[NoteIndex].NoteType of
        ntNormal, ntRap:
          Inc(
            NormalWeight,
            CurrentSong.Tracks[Track].Lines[LineIndex].Notes[NoteIndex].Duration *
            ScoreFactor[CurrentSong.Tracks[Track].Lines[LineIndex].Notes[NoteIndex].NoteType]);
        ntGolden, ntRapGolden:
          Inc(
            GoldenWeight,
            CurrentSong.Tracks[Track].Lines[LineIndex].Notes[NoteIndex].Duration *
            ScoreFactor[CurrentSong.Tracks[Track].Lines[LineIndex].Notes[NoteIndex].NoteType]);
      end;
    end;

    if LineWeight > 0 then
      Inc(NonEmptyLines);

    Inc(TotalWeight, LineWeight);
  end;

  if TotalWeight > 0 then
  begin
    MaxSongPoints := MAX_SONG_SCORE - MAX_SONG_LINE_BONUS;
    GoldenPoints := Round(MaxSongPoints * GoldenWeight / TotalWeight);
    NormalPoints := MaxSongPoints - GoldenPoints;
  end
  else
  begin
    NormalPoints := 0;
    GoldenPoints := 0;
  end;

  if NonEmptyLines > 0 then
    LineBonusPoints := MAX_SONG_LINE_BONUS
  else
    LineBonusPoints := 0;
  TotalPoints := NormalPoints + GoldenPoints + LineBonusPoints;

  PreviewAvailable := false;
  AnalysisPercent := 0;
  if HasVocalsPreviewPlayback then
  begin
    EnsurePreviewScoreCache(Track);

    HasReadyPreviewLine := false;
    EnterCriticalSection(FAnalysisDataLock);
    try
      if Track <= High(FPreviewLineScoreReadyByTrack) then
        for LineIndex := 0 to High(FPreviewLineScoreReadyByTrack[Track]) do
          if FPreviewLineScoreReadyByTrack[Track][LineIndex] then
          begin
            HasReadyPreviewLine := true;
            Break;
          end;
    finally
      LeaveCriticalSection(FAnalysisDataLock);
    end;

    if FPreviewBuildComplete and
       (not HasReadyPreviewLine) and
       (not FAnalysisThreadRunning) and
       (not FAnalysisFailed) then
      StartAnalysisThread(false, Track);

    PreviewNormalScore := 0;
    PreviewGoldenScore := 0;
    PreviewLineBonusScore := 0;
    EnterCriticalSection(FAnalysisDataLock);
    try
      for LineIndex := 0 to High(CurrentSong.Tracks[Track].Lines) do
      begin
        if Track <= High(FPreviewLineNormalScoreByTrack) then
        begin
          PreviewNormalScore := PreviewNormalScore + FPreviewLineNormalScoreByTrack[Track][LineIndex];
          PreviewGoldenScore := PreviewGoldenScore + FPreviewLineGoldenScoreByTrack[Track][LineIndex];
          PreviewLineBonusScore := PreviewLineBonusScore + FPreviewLineBonusScoreByTrack[Track][LineIndex];
        end
        else
        begin
          PreviewNormalScore := PreviewNormalScore + FPreviewLineNormalScore[LineIndex];
          PreviewGoldenScore := PreviewGoldenScore + FPreviewLineGoldenScore[LineIndex];
          PreviewLineBonusScore := PreviewLineBonusScore + FPreviewLineBonusScore[LineIndex];
        end;
      end;
    finally
      LeaveCriticalSection(FAnalysisDataLock);
    end;

    if FAnalysisThreadRunning and FAnalysisThreadBuildPreview and (Track = FAnalysisThreadTrack) then
      AnalysisPercent := FAnalysisProgressPct
    else if FPreviewBuildComplete then
      AnalysisPercent := 100;

    PreviewNormalPoints := Round(PreviewNormalScore);
    PreviewGoldenPoints := Round(PreviewGoldenScore);
    PreviewLineBonusPoints := Round(PreviewLineBonusScore);
    PreviewTotalPoints := PreviewNormalPoints + PreviewGoldenPoints + PreviewLineBonusPoints;
    PreviewAvailable := true;
  end;

  if PreviewAvailable then
  begin
    if FAnalysisThreadRunning and FAnalysisThreadBuildPreview and (Track = FAnalysisThreadTrack) then
      Lines[0] := Language.Translate('SING_OPTIONS_GAME_DIFFICULTY') + ': ' + IDifficultyTranslated[Difficulty] +
        ' (' + IntToStr(AnalysisPercent) + '%)'
    else
      Lines[0] := Language.Translate('SING_OPTIONS_GAME_DIFFICULTY') + ': ' + IDifficultyTranslated[Difficulty];
    Lines[1] := Language.Translate('SING_NOTES') + ': ' + IntToStr(PreviewNormalPoints) + ' / ' + IntToStr(NormalPoints);
    Lines[2] := Language.Translate('SING_PHRASE_BONUS') + ': ' + IntToStr(PreviewLineBonusPoints) + ' / ' + IntToStr(LineBonusPoints);
    Lines[3] := Language.Translate('SING_GOLDEN_NOTES') + ': ' + IntToStr(PreviewGoldenPoints) + ' / ' + IntToStr(GoldenPoints);
    Lines[4] := Language.Translate('SING_TOTAL') + ': ' + IntToStr(PreviewTotalPoints) + ' / ' + IntToStr(TotalPoints);
  end
  else
  begin
    if FAnalysisThreadRunning and FAnalysisThreadBuildPreview and (Track = FAnalysisThreadTrack) then
      Lines[0] := Language.Translate('SING_OPTIONS_GAME_DIFFICULTY') + ': ' + IDifficultyTranslated[Difficulty] +
        ' (' + IntToStr(FAnalysisProgressPct) + '%)'
    else
      Lines[0] := Language.Translate('SING_OPTIONS_GAME_DIFFICULTY') + ': ' + IDifficultyTranslated[Difficulty];
    Lines[1] := Language.Translate('SING_NOTES') + ': ' + IntToStr(NormalPoints);
    Lines[2] := Language.Translate('SING_PHRASE_BONUS') + ': ' + IntToStr(LineBonusPoints);
    Lines[3] := Language.Translate('SING_GOLDEN_NOTES') + ': ' + IntToStr(GoldenPoints);
    Lines[4] := Language.Translate('SING_TOTAL') + ': ' + IntToStr(TotalPoints);
  end;

  OrgFont := CurrentFont;
  SetFontFamily(0);
  SetFontStyle(ftBold);
  SetFontItalic(False);
  SetFontReflection(False, 0);
  SetFontSize(14);
  glColor4f(0, 0, 0, 1);

  for LineIndex := 0 to High(Lines) do
  begin
    SetFontPos(X + 10, Y + 12 + LineIndex * 18);
    glPrint(Lines[LineIndex]);
  end;

  SetFont(OrgFont);
end;

procedure TScreenEditSub.DrawText(X, Y, W, H: real; Track: Integer; NumLines: Integer);
var
  Rec:   TRecR;
  Count: Integer;
  TempR: real;
  Space: real;
  Str:   UTF8String;

  //PlayerNumber:  Integer;
  OrgFont:  TFont;

begin
  if ((1 shl Track) <> 0) then
  begin
    Space := H / (NumLines - 1);
    //PlayerNumber := Track + 1; // Player 1 is 0
    glColor3f(1, 1, 1);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    if not CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].HasLength(TempR) then TempR := 0
    else TempR := W / TempR;

    with CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine] do
    begin

      OrgFont := CurrentFont;
      glColor4f(0, 0, 0, 1);
      SetFontFamily(0);
      SetFontStyle(ftBold);
      SetFontItalic(False);
      SetFontSize(14);

      for Count := 0 to HighNote do
      begin
        with Notes[Count] do
        begin
          // left part
          Rec.Left  := 0;
          Rec.Right := 0;
          BaseNote := CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].BaseNote;
          Rec.Top := Y - (Tone-BaseNote)*Space/2 - NotesH[0];
          Rec.Bottom := Rec.Top + 2 * NotesH[0];
          // middle part
          Rec.Left := (StartBeat - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + X + 0.5 + NotesW[0];
          Rec.Right := (StartBeat + Duration - CurrentSong.Tracks[Track].Lines[CurrentSong.Tracks[Track].CurrentLine].Notes[0].StartBeat) * TempR + X - NotesW[0] - 0.5;
          SetFontPos(Rec.Left, Rec.Top);
          glPrint(Text);
          // add info if current note is medley start
          if (CurrentSong.Medley.Source <> msNone) and (MedleyNotes.isStart) and (CurrentSong.Tracks[Track].CurrentLine = MedleyNotes.start.line) and (Count = MedleyNotes.start.note) then
          begin
            Str := Language.Translate('EDIT_MEDLEYSTART');
            Str := '| ' + Copy(Str, 1, Length(Str) - 1) + ' (' + Language.Translate('EDIT_DURATION') + ' ' + FloatToStr(GetMedleyLength) + ' s)';
            SetFontPos(Rec.Left - 0.5 - NotesW[0], Rec.Top + Space);
            glColor4f(0.15, 0.75, 0.15, 1);
            glPrint(Str);
            glColor4f(0, 0, 0, 1);
          end;
          // add info if current note is medley end
          if (CurrentSong.Medley.Source <> msNone) and (MedleyNotes.isEnd) and (CurrentSong.Tracks[Track].CurrentLine = MedleyNotes.end_.line) and (Count = MedleyNotes.end_.note) then
          begin
            Str := Language.Translate('EDIT_MEDLEYEND');
            Str := '(' + Language.Translate('EDIT_DURATION') + ' ' + FloatToStr(GetMedleyLength) + ' s) ' + Copy(Str, 1, Length(Str) - 1) + ' |';
            SetFontPos(Rec.Right + 0.5 + NotesW[0] - glTextWidth(Str), Rec.Top + Space);
            glColor4f(0.15, 0.75, 0.15, 1);
            glPrint(Str);
            glColor4f(0, 0, 0, 1);
          end;
          if (CurrentSong.HasPreview) and (IsStartPreview) then
          begin
            Str := Language.Translate('EDIT_PREVIEWSTART');
            Str := '| ' + Copy(Str, 1, Length(Str) - 1 ) + ' (' + FloatToStr(CurrentSong.PreviewStart) + ' s)';
            SetFontPos(Rec.Left - 0.5 - NotesW[0], Rec.Top - Space);
            //glColor4f(0.15, 0.75, 0.15, 1);
            glPrint(Str);
            //glColor4f(0, 0, 0, 1);
          end;
        end; // with
      end; // for
    end; // with

    // revert the font to prevent conflicts within drawing the editor lyric line
    SetFont(OrgFont);

    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  end;
end;

// show transparent background for interactive note

procedure TScreenEditSub.ShowInteractiveBackground;
var
  TempR:     real;
  NoteIndex: Integer;
begin

  for NoteIndex := 0 to High(TransparentNoteButtonId) do
  begin
    Button[TransparentNoteButtonId[NoteIndex]].SetX(0);
    Button[TransparentNoteButtonId[NoteIndex]].SetY(0);
    Button[TransparentNoteButtonId[NoteIndex]].SetW(0);
    Button[TransparentNoteButtonId[NoteIndex]].SetH(0);
  end;

// adding transparent buttons
  while (Length(TransparentNoteButtonId) - 1 < CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote) do
  begin
    SetLength(InteractiveNoteId, Length(InteractiveNoteId) + 1);
    SetLength(TransparentNoteButtonId, Length(TransparentNoteButtonId) + 1);
    TransparentNoteButtonId[Length(TransparentNoteButtonId) - 1] := AddButton(0, 0, 0, 0, PATH_NONE);
    // for debug purposes: use some button texture instead of a transparent button (comment out line above, uncomment line below)
    //TransparentNoteButtonId[Length(TransparentNoteButtonId)-1] := AddButton(0, 0, 0, 0, Skin.GetTextureFileName(Theme.Main.Buttonsolo.Tex));
    InteractiveNoteId[Length(InteractiveNoteId) - 1] := Length(Interactions) - 1;
  end;
  TempR := 720 / (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat - CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat);
  for NoteIndex := 0 to CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote do
  begin
    Button[TransparentNoteButtonId[NoteIndex]].SetX(Theme.EditSub.NotesBackground.X + NotesSkipX + (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].StartBeat - CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat) * TempR + 0.5);
    Button[TransparentNoteButtonId[NoteIndex]].SetY(
      Theme.EditSub.NotesBackground.Y + 7 * LineSpacing -
      (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].Tone -
       CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].BaseNote) * LineSpacing / 2 -
      NotesH[0]);
    Button[TransparentNoteButtonId[NoteIndex]].SetW((CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].Duration) * TempR - 0.5);
    Button[TransparentNoteButtonId[NoteIndex]].SetH(2 * NotesH[0]);
  end;
end;

// from revision 2475
procedure TScreenEditSub.StartVideoPreview;
var
  VideoFile: IPath;

begin
  if Assigned(fCurrentVideo) then
  begin
    fCurrentVideo.Stop();
    fCurrentVideo := nil;
  end;

  VideoFile := CurrentSong.Path.Append(CurrentSong.Video);
  if (CurrentSong.Video.IsSet) and VideoFile.IsFile then
  begin
    fCurrentVideo := VideoPlayback.Open(VideoFile);
    if (fCurrentVideo <> nil) then
    begin
      fCurrentVideo.Position := CurrentSong.VideoGAP + GetPlaybackPosition;
      fCurrentVideo.Play;
    end;
  end;

  Statics[BackgroundImageId].Visible := (not Assigned(fCurrentVideo)) and (Statics[BackgroundImageId].Texture.TexNum > 0);
end;

procedure TScreenEditSub.StopVideoPreview;
begin
  // Stop video preview of previous song
  Statics[BackgroundImageId].Visible := false;
  if Assigned(fCurrentVideo) then
  begin
    fCurrentVideo.Stop();
    fCurrentVideo := nil;
  end;
end;

procedure TScreenEditSub.UpdateVideoPosition(NewPosition: real);
begin
  if (fCurrentVideo <> nil) then
  begin
    fCurrentVideo.Position := CurrentSong.VideoGAP + NewPosition;
    fCurrentVideo.Play;
  end;
end;

function TScreenEditSub.CheckTimingSyntaxErrors(out ErrorMessages: UTF8String): boolean;
var
  TrackIndex, LineIndex, NoteIndex: Integer;
  LineEndBeat: Integer;
  NoteStart, NoteEnd: Integer;
  PrevNoteEnd: Integer;
  Messages: UTF8String;
  OtherLine: Integer;


  procedure AppendMsg(const Msg: UTF8String);
  begin
    if Messages <> '' then
      Messages := Messages + '\n' + Msg
    else
      Messages := Msg;
  end;

  procedure RememberFirstError(const TrackIndex, LineIndex, Beat: Integer);
  begin
    if TimingErrorValid then
      Exit;

    TimingErrorValid := true;
    TimingErrorTrack := TrackIndex;
    TimingErrorLine := LineIndex;
    TimingErrorBeat := Beat;
  end;
begin
  Messages := '';
  TimingErrorValid := false;
  TimingErrorTrack := 0;
  TimingErrorLine := 0;
  TimingErrorBeat := 0;

  for TrackIndex := 0 to High(CurrentSong.Tracks) do
  begin
    if Length(CurrentSong.Tracks[TrackIndex].Lines) = 0 then
      Continue;

    // compute linebreak positions and note overlaps for this track
    for LineIndex := 0 to CurrentSong.Tracks[TrackIndex].High do
    begin
      if (CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote < 0) then
        Continue;

      // line end beat based on last note in the line
      NoteIndex := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote;
      LineEndBeat := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat +
                     CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration;

      // 1) note vs note overlaps on this track (global, not limited to the line)
      // we still walk line by line but carry PrevNoteEnd across lines
      if LineIndex = 0 then
        PrevNoteEnd := -MaxInt;

      for NoteIndex := 0 to CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote do
      begin
        NoteStart := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat;
        NoteEnd   := NoteStart + CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration;

        // any later note starting before previous note ended is an overlap
        if NoteStart < PrevNoteEnd then
        begin
          RememberFirstError(TrackIndex, LineIndex, NoteStart);
          AppendMsg(Format(Language.Translate('EDIT_INFO_TIMING_NOTE_OVERLAP'),
                           [TrackIndex + 1, LineIndex + 1, NoteIndex + 1,
                            NoteStart, NoteEnd - 1, PrevNoteEnd - 1]));
        end;

        if NoteEnd > PrevNoteEnd then
          PrevNoteEnd := NoteEnd;
      end;

      // 2) linebreak vs note overlaps: no note on this track may cross this linebreak
      for NoteIndex := 0 to CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote do
      begin
        NoteStart := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat;
        NoteEnd   := NoteStart + CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration;

        // allowed: linebreak at note start (LineEndBeat = NoteStart)
        // forbidden: NoteStart < LineEndBeat < NoteEnd
        if (NoteStart < LineEndBeat) and (LineEndBeat < NoteEnd) then
        begin
          RememberFirstError(TrackIndex, LineIndex, LineEndBeat);
          AppendMsg(Format(Language.Translate('EDIT_INFO_TIMING_LINEBREAK_OVERLAP'),
                           [TrackIndex + 1, LineIndex + 1, LineEndBeat,
                            NoteIndex + 1, NoteStart, NoteEnd - 1]));
        end;
      end;
    end;

    // additionally check each linebreak against notes in OTHER lines of the same track
    for LineIndex := 0 to CurrentSong.Tracks[TrackIndex].High do
    begin
      if (CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote < 0) then
        Continue;

      NoteIndex := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote;
      LineEndBeat := CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].StartBeat +
                     CurrentSong.Tracks[TrackIndex].Lines[LineIndex].Notes[NoteIndex].Duration;

      // check all notes on this track, all lines
      for OtherLine := 0 to CurrentSong.Tracks[TrackIndex].High do
      begin
        if CurrentSong.Tracks[TrackIndex].Lines[OtherLine].HighNote < 0 then
          Continue;

        for NoteIndex := 0 to CurrentSong.Tracks[TrackIndex].Lines[OtherLine].HighNote do
        begin
          NoteStart := CurrentSong.Tracks[TrackIndex].Lines[OtherLine].Notes[NoteIndex].StartBeat;
          NoteEnd   := NoteStart + CurrentSong.Tracks[TrackIndex].Lines[OtherLine].Notes[NoteIndex].Duration;

          if (NoteStart < LineEndBeat) and (LineEndBeat < NoteEnd) then
          begin
            RememberFirstError(TrackIndex, LineIndex, LineEndBeat);
            AppendMsg(Format(Language.Translate('EDIT_INFO_TIMING_LINEBREAK_OVERLAP'),
                             [TrackIndex + 1, LineIndex + 1, LineEndBeat,
                              NoteIndex + 1, NoteStart, NoteEnd - 1]));
          end;
        end;
      end;
    end;
  end;

  if Messages <> '' then
  begin
    ErrorMessages := Language.Translate('EDIT_INFO_SYNTAX_ERRORS_TIMING_HEADER') + '\n\n' + Messages;
    Result := true;
  end
  else
  begin
    ErrorMessages := '';
    Result := false;
  end;
end;

procedure TScreenEditSub.GoToLineAndBeat(const TrackIndex, LineIndex, Beat: Integer);
var
  NoteIndex: Integer;
begin
  if (TrackIndex < 0) or (TrackIndex > High(CurrentSong.Tracks)) then
    Exit;
  if (LineIndex < 0) or (LineIndex > CurrentSong.Tracks[TrackIndex].High) then
    Exit;
  if CurrentSong.Tracks[TrackIndex].Lines[LineIndex].HighNote < 0 then
    Exit;

  AudioPlayback.Stop;
  PlaySentence := false;
  PlaySentenceMidi := false;
  PlayOne := false;
  PlayVideo := false;
  StopVideoPreview;
  {$IFDEF UseMIDIPort}
  StopMidi;
  {$ENDIF}

  CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
  CurrentTrack := TrackIndex;
  CurrentSong.Tracks[CurrentTrack].CurrentLine := LineIndex;
  CurrentNote[CurrentTrack] := 0;

  for NoteIndex := 0 to CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].HighNote do
  begin
    CurrentNote[CurrentTrack] := NoteIndex;
    if CurrentSong.Tracks[CurrentTrack].Lines[LineIndex].Notes[NoteIndex].EndBeat >= Beat then
      Break;
  end;

  UpdateLineBaseNote(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
  CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
  EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
  EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
  ShowInteractiveBackground;
  GoldenRec.KillAll;
  Text[TextInfo].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(Beat);
end;

procedure TScreenEditSub.GoToFirstTimingError;
begin
  if TimingErrorValid then
    GoToLineAndBeat(TimingErrorTrack, TimingErrorLine, TimingErrorBeat);
end;

function TScreenEditSub.SaveSongToFile(const SaveRelative: boolean): boolean;
var
  SResult: TSaveSongResult;
begin
  if CurrentSong.isDuet then
  begin
    CurrentSong.Medley.Source := msNone;
  end
  else if (MedleyNotes.isStart and MedleyNotes.isEnd and MedleyNotes.isCustom) and
          (MedleyNotes.start.line < MedleyNotes.end_.line) and
          (Length(CurrentSong.Tracks[CurrentTrack].Lines) > MedleyNotes.end_.line) and
          (Length(CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes) > MedleyNotes.end_.note) and
          (Length(CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes) > MedleyNotes.start.note) then
  begin
    CurrentSong.Medley.Source := msTag;
    CurrentSong.Medley.StartBeat := CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes[MedleyNotes.start.note].StartBeat;
    CurrentSong.Medley.EndBeat := CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].StartBeat +
      CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].Duration;
    CurrentSong.Medley.FadeIn_time := DEFAULT_FADE_IN_TIME;
    CurrentSong.Medley.FadeOut_time := DEFAULT_FADE_OUT_TIME;
  end
  else if MedleyNotes.isCustom then
  begin
    CurrentSong.Medley.Source := msNone;
    CurrentSong.Medley.StartBeat := 0;
    CurrentSong.Medley.EndBeat := 0;
  end;

  if SaveRelative then
  begin
    if CurrentSong.isDuet then
    begin
      ScreenPopupError.ShowPopup(Language.Translate('EDIT_INFO_DUET_RELATIVE_UNSUPPORTED'));
      Exit(false);
    end;

    if CurrentSong.Medley.Source = msTag then
      ScreenPopupError.ShowPopup(Language.Translate('EDIT_INFO_MEDLEY_RELATIVE_UNSUPPORTED') + ' ' + Language.Translate('EDIT_INFO_MEDLEY_DELETED'));

    CurrentSong.Medley.Source := msNone;
    CurrentSong.Relative := true;
  end
  else
    CurrentSong.Relative := false;

  SResult := SaveSong(CurrentSong, CurrentSong.Tracks, CurrentSong.Path.Append(CurrentSong.FileName), CurrentSong.Relative);
  Result := (SResult = ssrOK);

  if Result then
  begin
    Text[TextInfo].Text := Language.Translate('INFO_FILE_SAVED');
    SetLength(UndoLines, 0, High(CurrentSong.Tracks));
    SetLength(UndoStateNote, 0, Length(CurrentSong.Tracks));
    SetLength(Undoheader, 0);
    CurrentUndoLines := 0;
  end;
  if not Result then
    ScreenPopupError.ShowPopup(Language.Translate('ERROR_SAVE_FILE_FAILED'));
end;

constructor TScreenEditSub.Create;
begin
  inherited Create;
  LoadFromTheme(Theme.EditSub);
  InitCriticalSection(FAnalysisDecodeLock);
  InitCriticalSection(FAnalysisDataLock);

  //video
  fCurrentVideo := nil;
  ClearVocalsWaveformCache;

  ResetBeatTracking;
  PendingSaveRelative := false;
  TimingErrorValid := false;

  EditorLyrics[0] := TEditorLyrics.Create;
  EditorLyrics[1] := TEditorLyrics.Create;

  SetLength(Player, 1);
  SetLength(TitleVal, 0);
  SetLength(ArtistVal, 0);
  SetLength(LanguageVal, 0);
  SetLength(EditionVal, 0);
  SetLength(GenreVal, 0);
  SetLength(YearVal, 0);
  SetLength(CreatorVal, 0);
  SetLength(MP3Val, 0);
  SetLength(CoverVal, 0);
  SetLength(BackgroundVal, 0);
  SetLength(VideoVal, 0);
  SetLength(VideoGapVal, 0);
  SetLength(BPMVal, 0);
  SetLength(GAPVal, 0);
  SetLength(StartTagVal, 0);
  SetLength(EndTagVal, 0);
  SetLength(MedleyStartVal, 0);
  SetLength(MedleyEndVal, 0);
  SetLength(PreviewStartVal, 0);
  SetLength(RelativeVal, 0);
  SetLength(StartVal, 0);
  SetLength(ToneVal, 0);
  SetLength(DurationVal, 0);
  SetLength(LyricVal, 0);

  //volume
  SetLength(VolumeAudio,0);
  SetLength(VolumeMidi,0);
  SetLength(VolumeClick,0);
  VolumeDragSlideId := -1;

  // interactive
  SetLength(InteractiveNoteId, 0);
  SetLength(TransparentNoteButtonId, 0);
  SetLength(InteractiveLineId, 0);
  SetLength(TransparentLineButtonId, 0);

  // file info
  // title header
  TitleSlideId := AddSelectSlide(Theme.EditSub.SlideTitle, TitleData, TitleVal);
  SelectsS[TitleSlideId].Text.Align := 0;
  SelectsS[TitleSlideId].Text.X := SelectsS[TitleSlideId].Texture.X + 3;

  // artist header
  ArtistSlideId := AddSelectSlide(Theme.EditSub.SlideArtist, ArtistData, ArtistVal);
  SelectsS[ArtistSlideId].Text.Align := 0;
  SelectsS[ArtistSlideId].Text.X := SelectsS[ArtistSlideId].Texture.X + 3;

  // language header
  LanguageSlideId := AddSelectSlide(Theme.EditSub.SlideLanguage, LanguageData, LanguageVal);
  SelectsS[LanguageSlideId].Text.Align := 0;
  SelectsS[LanguageSlideId].Text.X := SelectsS[LanguageSlideId].Texture.X + 3;

  // edition header
  EditionSlideId := AddSelectSlide(Theme.EditSub.SlideEdition, EditionData, EditionVal);
  SelectsS[EditionSlideId].Text.Align := 0;
  SelectsS[EditionSlideId].Text.X := SelectsS[EditionSlideId].Texture.X + 3;

  // genre header
  GenreSlideId := AddSelectSlide(Theme.EditSub.SlideGenre, GenreData, GenreVal);
  SelectsS[GenreSlideId].Text.Align := 0;
  SelectsS[GenreSlideId].Text.X := SelectsS[GenreSlideId].Texture.X + 3;

  // year header
  YearSlideId := AddSelectSlide(Theme.EditSub.SlideYear, YearData, YearVal);
  SelectsS[YearSlideId].Text.Align := 0;
  SelectsS[YearSlideId].Text.X := SelectsS[YearSlideId].Texture.X + 3;

  // creator header
  CreatorSlideId := AddSelectSlide(Theme.EditSub.SlideCreator, CreatorData, CreatorVal);
  SelectsS[CreatorSlideId].Text.Align := 0;
  SelectsS[CreatorSlideId].Text.X := SelectsS[CreatorSlideId].Texture.X + 3;

  // MP3 header
  MP3SlideId := AddSelectSlide(Theme.EditSub.SlideMP3, MP3Data, MP3Val);
  SelectsS[MP3SlideId].Text.Align := 0;
  SelectsS[MP3SlideId].Text.X := SelectsS[MP3SlideId].Texture.X + 3;

  // cover header
  CoverSlideId := AddSelectSlide(Theme.EditSub.SlideCover, CoverData, CoverVal);
  SelectsS[CoverSlideId].Text.Align := 0;
  SelectsS[CoverSlideId].Text.X := SelectsS[CoverSlideId].Texture.X + 3;

  // background header
  BackgroundSlideId := AddSelectSlide(Theme.EditSub.SlideBackground, BackgroundData, BackgroundVal);
  SelectsS[BackgroundSlideId].Text.Align := 0;
  SelectsS[BackgroundSlideId].Text.X := SelectsS[BackgroundSlideId].Texture.X + 3;

  // video header
  VideoSlideId := AddSelectSlide(Theme.EditSub.SlideVideo, VideoData, VideoVal);
  SelectsS[VideoSlideId].Text.Align := 0;
  SelectsS[VideoSlideId].Text.X := SelectsS[VideoSlideId].Texture.X + 3;

  // videogap header
  VideoGapSlideId := AddSelectSlide(Theme.EditSub.SlideVideoGap, VideoGapData, VideoGapVal);
  SelectsS[VideoGapSlideId].Text.Align := 0;
  SelectsS[VideoGapSlideId].Text.X := SelectsS[VideoGapSlideId].Texture.X + 3;

  // BPM header
  BPMSlideId := AddSelectSlide(Theme.EditSub.SlideBPM, BPMData, BPMVal);
  SelectsS[BPMSlideId].Text.Align := 0;
  SelectsS[BPMSlideId].Text.X := SelectsS[BPMSlideId].Texture.X + 3;

  // GAP header
  GAPSlideId := AddSelectSlide(Theme.EditSub.SlideGAP, GAPData, GAPVal);
  SelectsS[GAPSlideId].Text.Align := 0;
  SelectsS[GAPSlideId].Text.X := SelectsS[GAPSlideId].Texture.X + 3;

  // StartTag header
  StartTagSlideId := AddSelectSlide(Theme.EditSub.SlideStartTag, StartTagData, StartTagVal);
  SelectsS[StartTagSlideId].Text.Align := 0;
  SelectsS[StartTagSlideId].Text.X := SelectsS[StartTagSlideId].Texture.X + 3;

  // EndTag header
  EndTagSlideId := AddSelectSlide(Theme.EditSub.SlideEndTag, EndTagData, EndTagVal);
  SelectsS[EndTagSlideId].Text.Align := 0;
  SelectsS[EndTagSlideId].Text.X := SelectsS[EndTagSlideId].Texture.X + 3;

  // MedleyStart header
  MedleyStartSlideId := AddSelectSlide(Theme.EditSub.SlideMedleyStart, MedleyStartData, MedleyStartVal);
  SelectsS[MedleyStartSlideId].Text.Align := 0;
  SelectsS[MedleyStartSlideId].Text.X := SelectsS[MedleyStartSlideId].Texture.X + 3;

  // MedleyEnd header
  MedleyEndSlideId := AddSelectSlide(Theme.EditSub.SlideMedleyEnd, MedleyEndData, MedleyEndVal);
  SelectsS[MedleyEndSlideId].Text.Align := 0;
  SelectsS[MedleyEndSlideId].Text.X := SelectsS[MedleyEndSlideId].Texture.X + 3;

  // PreviewStart header
  PreviewStartSlideId := AddSelectSlide(Theme.EditSub.SlidePreviewStart, PreviewStartData, PreviewStartVal);
  SelectsS[PreviewStartSlideId].Text.Align := 0;
  SelectsS[PreviewStartSlideId].Text.X := SelectsS[PreviewStartSlideId].Texture.X + 3;

  // Relative header
  RelativeSlideId := AddSelectSlide(Theme.EditSub.SlideRelative, RelativeData, RelativeVal);
  SelectsS[RelativeSlideId].Text.Align := 0;
  SelectsS[RelativeSlideId].Text.X := SelectsS[RelativeSlideId].Texture.X + 3;

  // background image & preview
  BackgroundImageId := AddStatic(Theme.EditSub.BackgroundImage);
  Statics[BackgroundImageId].Visible := false;

  // note info
  // start header
  StartSlideId := AddSelectSlide(Theme.EditSub.SlideStart, StartData, StartVal);
  SelectsS[StartSlideId].Text.Align := 0;
  SelectsS[StartSlideId].Text.X := SelectsS[StartSlideId].Texture.X + 3;

  // duration header
  DurationSlideId := AddSelectSlide(Theme.EditSub.SlideDuration, DurationData, DurationVal);
  SelectsS[DurationSlideId].Text.Align := 0;
  SelectsS[DurationSlideId].Text.X := SelectsS[DurationSlideId].Texture.X + 3;

  // tone header
  ToneSlideId := AddSelectSlide(Theme.EditSub.SlideTone, ToneData, ToneVal);
  SelectsS[ToneSlideId].Text.Align := 0;
  SelectsS[ToneSlideId].Text.X := SelectsS[ToneSlideId].Texture.X + 3;

  // lyrics header
  LyricSlideId := AddSelectSlide(Theme.EditSub.SlideLyric, LyricData, LyricVal);
  SelectsS[LyricSlideId].Text.Align := 0;
  SelectsS[LyricSlideId].Text.X := SelectsS[LyricSlideId].Texture.X + 3;

  // control buttons
  UndoButtonId         := AddButton(Theme.EditSub.undo);
  PreviousSeqButtonID  := AddButton(Theme.EditSub.previousseq);
  NextSeqButtonID      := AddButton(Theme.EditSub.nextseq);
  FreestyleButtonID    := AddButton(Theme.EditSub.freestyle);
  GoldButtonID         := AddButton(Theme.EditSub.gold);
  PlayOnlyButtonID     := AddButton(Theme.EditSub.PlayOnly);
  PlayWithNoteButtonID := AddButton(Theme.EditSub.PlayWithNote);
  PlayNoteButtonID     := AddButton(Theme.EditSub.PlayNote);

  // current line
  TextSentence := AddButton(Theme.EditSub.ButtonCurrentLine);

  // current note
  TextNote := AddButton(Theme.EditSub.ButtonCurrentNote);

  // Audio Volume
  VolumeAudioSlideId := AddSelectSlide(Theme.EditSub.SelectVolAudio, VolumeAudioIndex, VolumeAudio);
  SelectsS[VolumeAudioSlideId].ClickSelectsPosition := true;

  // Midi Volume
  VolumeMidiSlideId := AddSelectSlide(Theme.EditSub.SelectVolMidi, VolumeMidiIndex, VolumeMidi);
  SelectsS[VolumeMidiSlideId].ClickSelectsPosition := true;

  // Click Volume
  VolumeClickSlideId := AddSelectSlide(Theme.EditSub.SelectVolClick, VolumeClickIndex, VolumeClick);
  SelectsS[VolumeClickSlideId].ClickSelectsPosition := true;

  // Audio Preroll
  PrerollAudioSlideId := AddSelectSlide(Theme.EditSub.SelectPrerollAudio, PrerollAudioIndex, PrerollAudio);

  // Midi Preroll
  PrerollMidiSlideId := AddSelectSlide(Theme.EditSub.SelectPrerollMidi, PrerollMidiIndex, PrerollMidi);

  {
  playerIconId[1] := AddStatic(Theme.Score.StaticPlayerIdBox[1]);
  Statics[playerIconId[1]].Texture.X := 2;
  Statics[playerIconId[1]].Texture.Y := 460;
  Statics[playerIconId[1]].Texture.W := 14;
  Statics[playerIconId[1]].Texture.H := 15;
  Statics[playerIconId[1]].Reflection := false;
  Statics[playerIconId[1]].Visible := false;

  playerIconId[2] := AddStatic(Theme.Score.StaticPlayerIdBox[3]);
  Statics[playerIconId[2]].Texture.X := 2;
  Statics[playerIconId[2]].Texture.Y := 480;
  Statics[playerIconId[2]].Texture.W := 14;
  Statics[playerIconId[2]].Texture.H := 15;
  Statics[playerIconId[2]].Reflection := false;
  Statics[playerIconId[2]].Visible := false;
  }

  // info text
  TextInfo := AddText(Theme.EditSub.TextInfo);

  // in notes place -> for move notes by mouse
  //NotesBackgroundId := AddSelectSlide(Theme.EditSub.NotesBackground, i, Empty);

  // Initialize Piano Keys to default values
  PianoKeysLow := Ini.PianoKeysLow;
  PianoKeysHigh := Ini.PianoKeysHigh;

  {$IFDEF UseMIDIPort}
    {$IFDEF MSWINDOWS}
    OpenMidiIn(OnMidiNote);
    {$ENDIF}
  {$ENDIF}
end;

destructor TScreenEditSub.Destroy;
begin
  StopAnalysisThread;
  StopPlayback;
  RestoreStandardPlayback(false);
  ClearVocalsWaveformCache;
  DoneCriticalSection(FAnalysisDataLock);
  DoneCriticalSection(FAnalysisDecodeLock);
  {$IFDEF UseMIDIPort}
    {$IFDEF MSWINDOWS}
    CloseMidiIn;
    {$ENDIF}
  {$ENDIF}
  inherited;
end;

procedure TScreenEditSub.OnShow;
const
  SUPPORTED_EXTS_AUDIO: array[0..4]  of string = ('.mp3', '.flac', '.wav', '.ogg', '.m4a');
  SUPPORTED_EXTS_IMAGE: array[0..1]  of string = ('.jpg', '.png');
  SUPPORTED_EXTS_VIDEO: array[0..11] of string = ('.avi', '.mov', '.divx', '.mkv', '.mpeg', '.mpg', '.mp4', '.mpeg', '.m2v', '.ts', '.webm', '.wmv');
var
  Files:       TPathDynArray;
  FileIndex:   Integer;
  TrackIndex:  Integer;
  VolumeIndex: Integer;
  PrerollIndex: Integer;
  Ext:         string;
  TimingErrors: UTF8String;

  function IsBeatMatchingNote(beat: Integer; Note: TLineFragment): boolean;
  begin
    Result := InRange(beat, Note.StartBeat, Note.StartBeat + Note.Duration);
  end;
begin
  inherited;
  // reset video playback engine
  CurrentTrack := 0;
  fCurrentVideo := nil;
  AudioPlayback.Stop;
  PlaySentence := false;
  PlayOne := false;
  PlaySentenceMidi := false;
  Text[TextInfo].Text := '';
  Log.LogStatus('Initializing', 'TEditScreen.OnShow');
  Xmouse := 0;
  VolumeDragSlideId := -1;

  ResetSingTemp;
  GoldenRec.KillAll;
//  SetLength(UndoSong, 0);
  SetLength(UndoLines, 0);
  SetLength(UndoStateNote, 0);
  SetLength(Undoheader, 0);

  try
    // reread header with custom tags
    Error := not CurrentSong.Analyse(true, false, false, false, 0, true);

    // with the duet/medley code, TSong.Analyse is already loading the song
    //if not Error then
    //  Error := not CurrentSong.LoadSong(false);
  except
    Error := true;
  end;

  if Error then
  begin
    //Error Loading Song -> Go back to Song Screen and Show some Error Message
    FadeTo(@ScreenSong);
    ScreenPopupError.ShowPopup (Language.Translate('ERROR_CORRUPT_SONG'));
    Exit;
  end
  else
  begin
    // run additional timing checks (note/linebreak overlaps)
    // if there are problems, show them in a popup but still allow editing
    // the function returns false if no errors were found
    if CheckTimingSyntaxErrors(TimingErrors) then
    begin
      if TimingErrors <> '' then
        ScreenPopupError.ShowPopup(TimingErrors);
    end;
  {$IFDEF UseMIDIPort}
    MidiOut := TMidiOutput.Create(nil);
      // run additional timing checks (note/linebreak overlaps)
      // if there are problems, show them in a popup but still allow editing
      if CheckTimingSyntaxErrors(TimingErrors) then
      begin
        if TimingErrors <> '' then
          ScreenPopupError.ShowPopup(TimingErrors);
      end;
    MidiOut.Open;
    MidiOut.PutShort(MIDI_PROGRAMCHANGE or 1, 0, 0);
    MidiActive := false;
    MidiTone := -1;
    MidiStopBeat := Low(Integer);
  {$ENDIF}

    if not Help.SetHelpID(ID) then
      Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenEditSub');

    //    Text[TextTitle].Text :=   CurrentSong.Title;

    // Header Title
    SetLength(TitleVal, 1);
    TitleVal[0] := CurrentSong.Title;
    SlideTitleIndex := 1;
    UpdateSelectSlideOptions(TitleSlideId,TitleVal,SlideTitleIndex);
    SelectsS[TitleSlideId].TextOpt[0].Align := 0;
    SelectsS[TitleSlideId].TextOpt[0].X := SelectsS[TitleSlideId].TextureSBG.X + 5;

    // Header Artist
    SetLength(ArtistVal, 1);
    ArtistVal[0] := CurrentSong.Artist;
    SlideArtistIndex := 1;
    UpdateSelectSlideOptions(ArtistSlideId,ArtistVal,SlideArtistIndex);
    SelectsS[ArtistSlideId].TextOpt[0].Align := 0;
    SelectsS[ArtistSlideId].TextOpt[0].X := SelectsS[ArtistSlideId].TextureSBG.X + 5;

    // Header Language
    SetLength(LanguageVal, 1);
    LanguageVal[0] := ifthen(CurrentSong.Language <> 'Unknown', CurrentSong.Language, NOT_SET);
    SlideLanguageIndex := 1;
    UpdateSelectSlideOptions(LanguageSlideId,LanguageVal,SlideLanguageIndex);
    SelectsS[LanguageSlideId].TextOpt[0].Align := 0;
    SelectsS[LanguageSlideId].TextOpt[0].X := SelectsS[LanguageSlideId].TextureSBG.X + 5;

    // Header Edition
    SetLength(EditionVal, 1);
    EditionVal[0] := ifthen(CurrentSong.Edition <> 'Unknown', CurrentSong.Edition, NOT_SET);
    SlideEditionIndex := 1;
    UpdateSelectSlideOptions(EditionSlideId,EditionVal,SlideEditionIndex);
    SelectsS[EditionSlideId].TextOpt[0].Align := 0;
    SelectsS[EditionSlideId].TextOpt[0].X := SelectsS[EditionSlideId].TextureSBG.X + 5;

    // Header Genre
    SetLength(GenreVal, 1);
    GenreVal[0] := ifthen(CurrentSong.Genre <> 'Unknown', CurrentSong.Genre, NOT_SET);
    SlideGenreIndex := 1;
    UpdateSelectSlideOptions(GenreSlideId,GenreVal,SlideGenreIndex);
    SelectsS[GenreSlideId].TextOpt[0].Align := 0;
    SelectsS[GenreSlideId].TextOpt[0].X := SelectsS[GenreSlideId].TextureSBG.X + 5;

    // Header Year
    SetLength(YearVal, 1);
    YearVal[0] := ifthen(CurrentSong.Year <> 0, IntToStr(CurrentSong.Year), NOT_SET);
    SlideYearIndex := 1;
    UpdateSelectSlideOptions(YearSlideId,YearVal,SlideYearIndex);
    SelectsS[YearSlideId].TextOpt[0].Align := 0;
    SelectsS[YearSlideId].TextOpt[0].X := SelectsS[YearSlideId].TextureSBG.X + 5;

    // Header Creator
    SetLength(CreatorVal, 1);
    CreatorVal[0] := ifthen(CurrentSong.Creator <> '', CurrentSong.Creator, NOT_SET);
    SlideCreatorIndex := 1;
    UpdateSelectSlideOptions(CreatorSlideId,CreatorVal,SlideCreatorIndex);
    SelectsS[CreatorSlideId].TextOpt[0].Align := 0;
    SelectsS[CreatorSlideId].TextOpt[0].X := SelectsS[CreatorSlideId].TextureSBG.X + 5;

    // Header MP3 / AUDIO
    SetLength(MP3Val, 0);
    SetLength(Files, 0);
    SlideMP3Index := -1;

    for Ext in SUPPORTED_EXTS_AUDIO do
      Songs.FindFilesByExtension(Path(includeTrailingPathDelimiter(CurrentSong.Path.ToNative)), Path(Ext), true, Files);

    for FileIndex := 0 to High(Files) do
    begin
      SetLength(MP3Val, High(MP3Val) + 2);
      MP3Val[FileIndex] := filesystem.ExtractFileName(Files[FileIndex]).ToUTF8;
      if (UTF8CompareText(MP3Val[FileIndex],CurrentSong.Audio.ToUTF8) = 0) then
            SlideMP3Index := FileIndex;
    end;
    UpdateSelectSlideOptions(MP3SlideId,MP3Val,SlideMP3Index);

    // Header Cover
    SetLength(Files, 0);
    SetLength(CoverVal, 0);
    SlideCoverIndex := -1;

    for Ext in SUPPORTED_EXTS_IMAGE do
      Songs.FindFilesByExtension(Path(includeTrailingPathDelimiter(CurrentSong.Path.ToNative)), Path(Ext), true, Files);

    for FileIndex := 0 to High(Files) do
    begin
      SetLength(CoverVal, High(CoverVal) + 2);
      CoverVal[FileIndex] := ExtractFileName(Files[FileIndex].ToUTF8());
      if UTF8CompareText(CoverVal[FileIndex], CurrentSong.Cover.ToUTF8) = 0 then
            SlideCoverIndex := FileIndex;
    end;

    if High(Files) < 0 then
    begin
      SetLength(CoverVal, 1);
      CoverVal[0] := CurrentSong.Cover.ToUTF8;
      SlideCoverIndex := 0;
    end;
    UpdateSelectSlideOptions(CoverSlideId,CoverVal,SlideCoverIndex);

    // Header Background
    SetLength(Files, 0);
    SetLength(BackgroundVal, 0);
    SlideBackgroundIndex := -1;

    for Ext in SUPPORTED_EXTS_IMAGE do
      Songs.FindFilesByExtension(Path(includeTrailingPathDelimiter(CurrentSong.Path.ToNative)), Path(Ext), true, Files);

    for FileIndex := 0 to High(Files) do
    begin
      SetLength(BackgroundVal, High(BackgroundVal) + 2);
      BackgroundVal[FileIndex] := ExtractFileName(Files[FileIndex].ToUTF8());
      if UTF8CompareText(BackgroundVal[FileIndex], CurrentSong.Background.ToUTF8) = 0 then
            SlideBackgroundIndex := FileIndex;
    end;

    if High(Files) < 0 then
    begin
      SetLength(BackgroundVal, 1);
      BackgroundVal[0] := CurrentSong.Background.ToUTF8;
      SlideBackgroundIndex := 0;
    end;
    UpdateSelectSlideOptions(BackgroundSlideId,BackgroundVal,SlideBackgroundIndex);

    // Header Video
    SetLength(Files, 0);
    SetLength(VideoVal, 0);
    SlideVideoIndex := -1;
    for Ext in SUPPORTED_EXTS_VIDEO do
      Songs.FindFilesByExtension(Path(includeTrailingPathDelimiter(CurrentSong.Path.ToNative)), Path(Ext), true, Files);

    for FileIndex := 0 to High(Files) do
    begin
      SetLength(VideoVal, High(VideoVal) + 2);
      VideoVal[FileIndex] := ExtractFileName(Files[FileIndex].ToUTF8());
      if UTF8CompareText(VideoVal[FileIndex], CurrentSong.Video.ToUTF8) = 0 then
            SlideVideoIndex := FileIndex;
    end;

    if High(Files) < 0 then
    begin
      SetLength(VideoVal, 1);
      VideoVal[0] := CurrentSong.Video.ToUTF8;
      SlideVideoIndex := 0;
    end;
    UpdateSelectSlideOptions(VideoSlideId,VideoVal,SlideVideoIndex);

    // Header VideoGap
    SetLength(VideoGapVal, 1);
    VideoGapVal[0] := '';
    SlideVideoGapIndex := 1;
    UpdateSelectSlideOptions(VideoGapSlideId,VideoGapVal,SlideVideoGapIndex);
    SelectsS[VideoGapSlideId].TextOpt[0].Align := 0;
    SelectsS[VideoGapSlideId].TextOpt[0].X := SelectsS[VideoGapSlideId].TextureSBG.X + 5;

    // Header BPM
    SetLength(BPMVal, 1);
    BPMVal[0] := '';
    SlideBPMIndex := 1;
    UpdateSelectSlideOptions(BPMSlideId,BPMVal,SlideBPMIndex);
    SelectsS[BPMSlideId].TextOpt[0].Align := 0;
    SelectsS[BPMSlideId].TextOpt[0].X := SelectsS[BPMSlideId].TextureSBG.X + 5;

    // Header GAP
    SetLength(GAPVal, 1);
    GAPVal[0] := '';
    SlideGAPIndex := 1;
    UpdateSelectSlideOptions(GAPSlideId,GAPVal,SlideGAPIndex);
    SelectsS[GAPSlideId].TextOpt[0].Align := 0;
    SelectsS[GAPSlideId].TextOpt[0].X := SelectsS[GAPSlideId].TextureSBG.X + 5;

    // Header StartTag
    SetLength(StartTagVal, 1);
    StartTagVal[0] := '';
    SlideStartTagIndex := 1;
    UpdateSelectSlideOptions(StartTagSlideId,StartTagVal,SlideStartTagIndex);
    SelectsS[StartTagSlideId].TextOpt[0].Align := 0;
    SelectsS[StartTagSlideId].TextOpt[0].X := SelectsS[StartTagSlideId].TextureSBG.X + 5;

    // Header EndTag
    SetLength(EndTagVal, 1);
    EndTagVal[0] := '';
    SlideEndTagIndex := 1;
    UpdateSelectSlideOptions(EndTagSlideId,EndTagVal,SlideEndTagIndex);
    SelectsS[EndTagSlideId].TextOpt[0].Align := 0;
    SelectsS[EndTagSlideId].TextOpt[0].X := SelectsS[EndTagSlideId].TextureSBG.X + 5;

    // Header MedleyStart
    SetLength(MedleyStartVal, 1);
    MedleyStartVal[0] := '';
    SlideMedleyStartIndex := 1;
    UpdateSelectSlideOptions(MedleyStartSlideId,MedleyStartVal,SlideMedleyStartIndex);
    SelectsS[MedleyStartSlideId].TextOpt[0].Align := 0;
    SelectsS[MedleyStartSlideId].TextOpt[0].X := SelectsS[MedleyStartSlideId].TextureSBG.X + 5;

    // Header MedleyEnd
    SetLength(MedleyEndVal, 1);
    MedleyEndVal[0] := '';
    SlideMedleyEndIndex := 1;
    UpdateSelectSlideOptions(MedleyEndSlideId,MedleyEndVal,SlideMedleyEndIndex);
    SelectsS[MedleyEndSlideId].TextOpt[0].Align := 0;
    SelectsS[MedleyEndSlideId].TextOpt[0].X := SelectsS[MedleyEndSlideId].TextureSBG.X + 5;

    // Header PreviewStart
    SetLength(PreviewStartVal, 1);
    PreviewStartVal[0] := '';
    SlidePreviewStartIndex := 1;
    UpdateSelectSlideOptions(PreviewStartSlideId,PreviewStartVal,SlidePreviewStartIndex);
    SelectsS[PreviewStartSlideId].TextOpt[0].Align := 0;
    SelectsS[PreviewStartSlideId].TextOpt[0].X := SelectsS[PreviewStartSlideId].TextureSBG.X + 5;

    // Header Relative
    SetLength(RelativeVal, 1);
    RelativeVal[0] := '';
    UpdateSelectSlideOptions(RelativeSlideId,RelativeVal,SlideRelativeIndex);
    SelectsS[RelativeSlideId].TextOpt[0].Align := 0;
    SelectsS[RelativeSlideId].TextOpt[0].X := SelectsS[RelativeSlideId].TextureSBG.X + 5;

    // Header Start
    SetLength(StartVal, 1);
    StartVal[0] := '';
    SlideStartIndex := 1;
    UpdateSelectSlideOptions(StartSlideId,StartVal,SlideStartIndex);
    SelectsS[StartSlideId].TextOpt[0].Align := 0;
    SelectsS[StartSlideId].TextOpt[0].X := SelectsS[StartSlideId].TextureSBG.X + 5;

    // Header Duration
    SetLength(DurationVal, 1);
    DurationVal[0] := '';
    SlideDurationIndex := 1;
    UpdateSelectSlideOptions(DurationSlideId,DurationVal,SlideDurationIndex);
    SelectsS[DurationSlideId].TextOpt[0].Align := 0;
    SelectsS[DurationSlideId].TextOpt[0].X := SelectsS[DurationSlideId].TextureSBG.X + 5;

    // Header Tone
    SetLength(ToneVal, 1);
    ToneVal[0] := '';
    SlideDurationIndex := 1;
    UpdateSelectSlideOptions(ToneSlideId,ToneVal,SlideToneIndex);
    SelectsS[ToneSlideId].TextOpt[0].Align := 0;
    SelectsS[ToneSlideId].TextOpt[0].X := SelectsS[ToneSlideId].TextureSBG.X + 5;

    // Header Lyric
    SetLength(LyricVal, 1);
    LyricVal[0] := '';
    SlideLyricIndex := 1;
    UpdateSelectSlideOptions(LyricSlideId,LyricVal,SlideLyricIndex);
    SelectsS[LyricSlideId].TextOpt[0].Align := 0;
    SelectsS[LyricSlideId].TextOpt[0].X := SelectsS[LyricSlideId].TextureSBG.X + 5;

    // volume slides
    SetLength(VolumeAudio, 0);
    SetLength(VolumeMidi, 0);
    SetLength(VolumeClick, 0);
    for VolumeIndex := 0 to 100 do
    begin
      SetLength(VolumeAudio, High(VolumeAudio) + 2);
      SetLength(VolumeMidi,  High(VolumeMidi)  + 2);
      SetLength(VolumeClick, High(VolumeClick) + 2);
      VolumeAudio[VolumeIndex] := IntToStr(VolumeIndex);
      VolumeMidi[VolumeIndex]  := IntToStr(VolumeIndex);
      VolumeClick[VolumeIndex] := IntToStr(VolumeIndex);
    end;
    VolumeAudioIndex := EnsureRange(Ini.AudioVolume, 0, 100);
    VolumeMidiIndex  := 100;
    VolumeClickIndex := EnsureRange(Ini.SfxVolume, 0, 100);
    UpdateSelectSlideOptions(VolumeAudioSlideId, VolumeAudio, VolumeAudioIndex);
    UpdateSelectSlideOptions(VolumeMidiSlideId,  VolumeMidi,  VolumeMidiIndex);
    UpdateSelectSlideOptions(VolumeClickSlideId, VolumeClick, VolumeClickIndex);

    // preroll slides (-500..500 ms)
    SetLength(PrerollAudio, 1001);
    SetLength(PrerollMidi, 1001);
    for PrerollIndex := -500 to 500 do
    begin
      PrerollAudio[PrerollIndex + 500] := IntToStr(PrerollIndex);
      PrerollMidi[PrerollIndex + 500] := IntToStr(PrerollIndex);
    end;
    PrerollAudioIndex := EnsureRange(Ini.EditorClickLeadMs, -500, 500) + 500;
    PrerollMidiIndex := EnsureRange(Ini.EditorMidiLeadMs, -500, 500) + 500;
    UpdateSelectSlideOptions(PrerollAudioSlideId, PrerollAudio, PrerollAudioIndex);
    UpdateSelectSlideOptions(PrerollMidiSlideId,  PrerollMidi,  PrerollMidiIndex);

    for TrackIndex := 0 to High(CurrentSong.Tracks) do
    begin
      CurrentSong.Tracks[TrackIndex].CurrentLine := 0;
      CurrentNote[TrackIndex] := 0;
      CurrentSong.Tracks[TrackIndex].Lines[0].Notes[0].Color := P1_INVERTED;
      RefreshPreviewScoreMetadata(TrackIndex);
    end;

    AudioPlayBack.Open(CurrentSong.Path.Append(CurrentSong.Audio),nil);
    if not FPreserveAnalysisOnShow then
    begin
      ClearVocalsWaveformCache(false);
      if HasVocalsPreviewSource then
      begin
        StartAnalysisThread(true, CurrentTrack);
        Text[TextInfo].Text := 'Vocals analysis is building in the background';
      end;
    end
    else if HasVocalsPreviewSource and (not FPreviewBuildComplete) and (not FAnalysisThreadRunning) and (not FAnalysisFailed) then
      StartAnalysisThread(true, CurrentTrack);
    //Set Down Music Volume for Better hearability of Midi Sounds
    //Music.SetVolume(0.4);

    // finding the note for the Medley section
    MedleyNotes := Default(TMedleyNotes);
    if (CurrentSong.Medley.Source = msTag) then
    begin
      if (CurrentSong.Medley.EndBeat > 0) then MedleyNotes.end_ := FindNote(CurrentSong.Medley.EndBeat);
      if (CurrentSong.Medley.EndBeat > CurrentSong.Medley.StartBeat) then MedleyNotes.start := FindNote(CurrentSong.Medley.StartBeat);

      MedleyNotes.isEnd   := (MedleyNotes.end_.line > 0)  or (MedleyNotes.end_.note  >= 0);
      MedleyNotes.isStart := (MedleyNotes.start.line > 0) or (MedleyNotes.start.note >= 0);
    end;

    // set preview start
    MedleyNotes.Preview := FindNote(round(GetMidBeat(CurrentSong.PreviewStart - CurrentSong.GAP/1000)));
    CurrentSong.Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].IsStartPreview := true;
    CurrentSong.PreviewStart := round(GetTimeFromBeat(CurrentSong.Tracks[MedleyNotes.Preview.track].Lines[MedleyNotes.Preview.line].Notes[MedleyNotes.Preview.note].StartBeat) * 1000) / 1000;

    for TrackIndex := 0 to High(CurrentSong.Tracks) do
    begin
      EditorLyrics[TrackIndex].Clear;

      EditorLyrics[TrackIndex].X     := Theme.EditSub.TextSentence.X;
      EditorLyrics[TrackIndex].Y     := Theme.EditSub.TextSentence.Y;
      EditorLyrics[TrackIndex].Align := Theme.EditSub.TextSentence.Align;
      EditorLyrics[TrackIndex].Size  := Theme.EditSub.TextSentence.Size;
      EditorLyrics[TrackIndex].ColR  := Theme.EditSub.TextSentence.ColR;
      EditorLyrics[TrackIndex].ColG  := Theme.EditSub.TextSentence.ColG;
      EditorLyrics[TrackIndex].ColB  := Theme.EditSub.TextSentence.ColB;
      EditorLyrics[TrackIndex].DColR := Theme.EditSub.TextSentence.DColR;
      EditorLyrics[TrackIndex].DColG := Theme.EditSub.TextSentence.DColG;
      EditorLyrics[TrackIndex].DColB := Theme.EditSub.TextSentence.DColB;

      EditorLyrics[TrackIndex].AddLine(TrackIndex, 0);
      EditorLyrics[TrackIndex].Selected := 0;
    end;

    NotesH[0] := 7;
    NotesW[0] := 4;
    ResizeNoteLeft := false;
    ResizeNoteRight := false;
    MoveNote := false;
    //show transparent background for notes
    ShowInteractiveBackground;
    // user input tracking
    AudioInput.CaptureStart;
  end;

  // background picture
  try
    if (not (CurrentSong.Background = PATH_NONE) and CurrentSong.Path.Append(CurrentSong.Background).Exists) then
    begin
      Tex_PrevBackground := Texture.LoadTexture(CurrentSong.Path.Append(CurrentSong.Background));
      Texture.AddTexture(Tex_PrevBackground, TEXTURE_TYPE_PLAIN, true);
      Statics[BackgroundImageId].Texture   := Tex_PrevBackground;
      Statics[BackgroundImageId].Texture.X := Theme.EditSub.BackgroundImage.X;
      Statics[BackgroundImageId].Texture.Y := Theme.EditSub.BackgroundImage.Y;
      Statics[BackgroundImageId].Texture.W := Theme.EditSub.BackgroundImage.W;
      Statics[BackgroundImageId].Texture.H := Theme.EditSub.BackgroundImage.H;
      Statics[BackgroundImageId].Visible := false;
    end;
  except
    Log.LogError('Background could not be loaded: ' + CurrentSong.Background.ToNative);
    Tex_PrevBackground.TexNum := 0;
  end;

  //Interaction := 0;
  TextEditMode := false;
  TitleEditMode := false;
  ArtistEditMode := false;
  LanguageEditMode := false;
  EditionEditMode := false;
  GenreEditMode := false;
  YearEditMode := false;
  CreatorEditMode := false;
  P1EditMode := false;
  P2EditMode := false;
  BPMEditMode := false;
  StopTextInput;

  editLengthText := 0;
  TextPosition := -1;
end;

procedure TScreenEditSub.SyncVolumeSlidersFromIni;
var
  NewAudio: Integer;
  NewClick: Integer;
begin
  NewAudio := EnsureRange(Ini.AudioVolume, 0, 100);
  if (VolumeAudioIndex <> NewAudio) and
     (VolumeAudioSlideId >= 0) and (VolumeAudioSlideId <= High(SelectsS)) then
  begin
    VolumeAudioIndex := NewAudio;
    SelectsS[VolumeAudioSlideId].SelectedOption := VolumeAudioIndex;
  end;

  NewClick := EnsureRange(Ini.SfxVolume, 0, 100);
  if (VolumeClickIndex <> NewClick) and
     (VolumeClickSlideId >= 0) and (VolumeClickSlideId <= High(SelectsS)) then
  begin
    VolumeClickIndex := NewClick;
    SelectsS[VolumeClickSlideId].SelectedOption := VolumeClickIndex;
  end;
end;

function TScreenEditSub.Draw: boolean;
var
  LastLine:  Integer;
  NoteIndex: Integer;
  Count:     Integer;
  ProjectedBeat: Integer;
begin
  SyncVolumeSlidersFromIni;

  if FPreviewRescorePending and HasVocalsPreviewPlayback then
  begin
    FPreviewRescorePending := false;
    InvalidateVocalsAnalysis(true);
  end;

  Ini.EditorClickLeadMs := PrerollAudioIndex - 500;
  Ini.EditorMidiLeadMs := PrerollMidiIndex - 500;

  {$IFDEF UseMIDIPort} // midi music
  if PlaySentenceMidi and Not (PlayOneMidi) then
  begin
    MidiPos := GetPlaybackPosition;

    // stop the music
    if (MidiPos > MidiStop) then
    begin
      StopMidi;
      MidiLastNote := -1;
      PlaySentenceMidi := false;
      LastMidiBeatValid := false;
    end
    else
    begin
      CurrentBeat := Floor(GetMidBeat(MidiPos - CurrentSong.GAP / 1000));
      ProjectedBeat := Floor(GetMidBeat((MidiPos + GetSentenceLeadSeconds(PlaySentenceMidi)) - CurrentSong.GAP / 1000));
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(CurrentBeat);

      if (not LastMidiBeatValid) or (ProjectedBeat < LastMidiBeat) then
      begin
        LastMidiBeat := ProjectedBeat - 1;
        LastMidiBeatValid := true;
      end;
      while LastMidiBeat < ProjectedBeat do
      begin
        Inc(LastMidiBeat);
        for NoteIndex := 0 to CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote do
          if (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].StartBeat = LastMidiBeat) then
          begin
            PlayMidiTone(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].Tone);
            MidiStopBeat := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].StartBeat +
              CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].Duration;
            MidiLastNote := NoteIndex;
            Break;
          end;
      end;

      if MidiActive and (CurrentBeat >= MidiStopBeat) then
        StopMidi;
    end;
  end; // if PlaySentenceMidi
  {$ENDIF}

  // move "cursor"
  if (PlaySentence or PlaySentenceMidi or PlayVideo) and not (PlayOne) then //and Not (PlayNote) then
  begin
    if (PlaySentence or PlayVideo) then
    begin
      CurrentBeat := Floor(GetMidBeat(GetPlaybackPosition - (CurrentSong.GAP) / 1000));
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(CurrentBeat);
    end;

    // store current line, find next line to given beat
    LastLine := CurrentSong.Tracks[CurrentTrack].CurrentLine;
    while (CurrentSong.Tracks[CurrentTrack].CurrentLine < High(CurrentSong.Tracks[CurrentTrack].Lines)) and (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat < CurrentBeat) do
      inc(CurrentSong.Tracks[CurrentTrack].CurrentLine);

    // only update lyric if line changes
    if CurrentSong.Tracks[CurrentTrack].CurrentLine <> LastLine then
    begin
      CurrentSong.Tracks[CurrentTrack].Lines[LastLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
      EditorLyrics[CurrentTrack].AddLine(CurrentTrack, CurrentSong.Tracks[CurrentTrack].CurrentLine);
      EditorLyrics[CurrentTrack].Selected := 0;
      CurrentNote[CurrentTrack] := 0;
      ShowInteractiveBackground;
      GoldenRec.KillAll;
    end;

    for NoteIndex := CurrentNote[CurrentTrack] to High(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes) do
    begin
      //note change
      if CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].StartBeat < CurrentBeat then
      begin
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := 1;
        CurrentNote[CurrentTrack] := NoteIndex;
        EditorLyrics[CurrentTrack].Selected := CurrentNote[CurrentTrack];
        CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Color := P1_INVERTED;
      end; //if
    end; //for NoteIndex}
  end; //end move cursor

  // music
  if (PlaySentence or PlayVideo or PlayOne) then
  begin
    // stop the music
    if (GetPlaybackPosition > PlayStopTime) then
    begin
      StopPlayback;
      RestoreStandardPlayback(true);
      PlaySentence := false;
      PlayOne := false;
      PlayVideo := false;
      StopVideoPreview;
      ResetBeatTracking;
    end;

    // click
    if (Click) and (PlaySentence) then
    begin
      //CurrentBeat := Floor(CurrentSong.BPM * (Music.Position - CurrentSong.GAP / 1000) / 60);
      CurrentBeat := Floor(GetMidBeat(AudioPlayback.Position - CurrentSong.GAP / 1000));
      ProjectedBeat := Floor(GetMidBeat((AudioPlayback.Position + GetSentenceLeadSeconds(PlaySentenceMidi)) - CurrentSong.GAP / 1000));
      Text[TextInfo].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(CurrentBeat);
      if (not LastClickValid) or (ProjectedBeat < LastClick) then
      begin
        LastClick := ProjectedBeat - 1;
        LastClickValid := true;
      end;

      while LastClick < ProjectedBeat do
      begin
        Inc(LastClick);
        for NoteIndex := 0 to CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote do
          if (CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[NoteIndex].StartBeat = LastClick) then
          begin
            SoundLib.Click.Volume := SelectsS[VolumeClickSlideId].SelectedOption / 100;
            ForcePlaySound(SoundLib.Click);
            Break;
          end;
      end;
    end; // click
  end; // if PlaySentence

  {$IFDEF UseMIDIPort} if PlayOneMidi then
  begin
    MidiPos := GetPlaybackPosition - MidiAnchorPos + MidiStart;
    // stop the music
    if ((MidiPos >= MidiStop))  then // and (midinotefound)
    begin
      StopMidi;
      PlayOneMidi := false;
      midinotefound := false;
    end;

    // click
    CurrentBeat := Floor(GetMidBeat(MidiPos - CurrentSong.GAP / 1000));
    ProjectedBeat := Floor(GetMidBeat((MidiPos + GetSentenceLeadSeconds(PlaySentenceMidi)) - CurrentSong.GAP / 1000));
    Text[TextInfo].Text := Language.Translate('EDIT_INFO_CURRENT_BEAT') + ' ' + IntToStr(CurrentBeat);

    if ((ProjectedBeat <> LastClick) and Not (midinotefound)) then
    begin
//      for NoteIndex := 0 to CurrentSong.Tracks[CurrentTrack].Lines[Lines[CurrentTrack].Current].HighNote do
//      begin
        if ((CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat <= ProjectedBeat) and
        ((CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat + CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration) > ProjectedBeat)) then
        begin
          LastClick := ProjectedBeat;
          midinotefound := true;
          PlayMidiTone(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone);
          MidiStopBeat := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat +
            CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration;
          MidiLastNote := CurrentNote[CurrentTrack];
        end;
//      end;
    end;

    if MidiActive and (CurrentBeat >= MidiStopBeat) then
      StopMidi;
  end; // if PlayOneNoteMidi
  {$ENDIF}

  Button[TextSentence].Text[0].Text := Language.Translate('EDIT_INFO_CURRENT_LINE') + ' ' + IntToStr(CurrentSong.Tracks[CurrentTrack].CurrentLine + 1) + ' / ' + IntToStr(CurrentSong.Tracks[CurrentTrack].Number);
  Button[TextNote].Text[0].Text :=  Language.Translate('EDIT_INFO_CURRENT_NOTE') + ' ' + IntToStr(CurrentNote[CurrentTrack] + 1) + ' / ' + IntToStr(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].HighNote + 1);

  if FAnalysisFailed and not (PlaySentence or PlayVideo or PlayOne) then
    Text[TextInfo].Text := 'Analysis failed: ' + FAnalysisError;

  // Song info
  // VideoGap
  VideoGapVal[0] := ifthen(CurrentSong.VideoGAP <> 0, FloatToStr(CurrentSong.VideoGAP) + ' s', NOT_SET);
  SelectsS[VideoGapSlideId].TextOpt[0].Text := VideoGapVal[0];
  // BPM
  if not BPMEditMode then
  begin
    BPMVal[0] := FloatToStr(CurrentSong.BPM[0].BPM / 4);
    SelectsS[BPMSlideId].TextOpt[0].Text := BPMVal[0];
  end;
  // GAP
  GAPVal[0] := FloatToStr(CurrentSong.GAP) + ' ms';
  SelectsS[GAPSlideId].TextOpt[0].Text := GAPVal[0];
  // StartTag
  StartTagVal[0] := ifthen(CurrentSong.Start <> 0.0, FloatToStr(CurrentSong.Start) + ' s', NOT_SET);
  SelectsS[StartTagSlideId].TextOpt[0].Text := StartTagVal[0];
  // EndTag
  EndTagVal[0] := ifthen(CurrentSong.Finish > 0.0, FloatToStr(CurrentSong.Finish) + ' ms', NOT_SET);
  SelectsS[EndTagSlideId].TextOpt[0].Text := EndTagVal[0];
  // MedleyStart
  if not (CurrentSong.isDuet) then
  begin
    SelectsS[MedleyStartSlideId].Text.Text := Theme.EditSub.SlideMedleyStart.Text;
    SelectsS[MedleyStartSlideId].TextOpt[0].Writable := false;
    SelectsS[MedleyEndSlideId].Text.Text := Theme.EditSub.SlideMedleyEnd.Text;
    SelectsS[MedleyEndSlideId].TextOpt[0].Writable := false;
    if (MedleyNotes.isStart) then
      MedleyStartVal[0] := IntToStr(CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes[MedleyNotes.start.note].StartBeat)
    else
      MedleyStartVal[0] := NOT_SET;
    SelectsS[MedleyStartSlideId].TextOpt[0].Text := MedleyStartVal[0];
    // MedleyEnd
    if (MedleyNotes.isEnd) then
      MedleyEndVal[0] := IntToStr(CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].StartBeat + CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].Duration)
    else
      MedleyEndVal[0] := NOT_SET;
    SelectsS[MedleyEndSlideId].TextOpt[0].Text := MedleyEndVal[0];
  end
  else // reuse for P1/P2 tag
  begin
    SelectsS[MedleyStartSlideId].Text.Text := 'P1:';
    MedleyStartVal[0] := CurrentSong.DuetNames[0];
    SelectsS[MedleyStartSlideId].TextOpt[0].Text := MedleyStartVal[0];
    SelectsS[MedleyStartSlideId].TextOpt[0].Writable := true;
    SelectsS[MedleyEndSlideId].Text.Text := 'P2:';
    MedleyEndVal[0] := CurrentSong.DuetNames[1];
    SelectsS[MedleyEndSlideId].TextOpt[0].Text := MedleyEndVal[0];
    SelectsS[MedleyEndSlideId].TextOpt[0].Writable := true;
  end;
  // PreviewStart
  if (CurrentSong.HasPreview) then
    PreviewStartVal[0] := FloatToStr(CurrentSong.PreviewStart) + ' s'
  else
    PreviewStartVal[0] := NOT_SET;
  SelectsS[PreviewStartSlideId].TextOpt[0].Text := PreviewStartVal[0];
  if (CurrentSong.Relative) then
    RelativeVal[0] := Language.Translate('SONG_MENU_YES')
  else
    RelativeVal[0] := NOT_SET + ' (' + Language.Translate('SONG_MENU_NO') + ')';
  SelectsS[RelativeSlideId].TextOpt[0].Text := RelativeVal[0];

  //Error reading variables when no song is loaded
  if not (Error or TitleEditMode or TextEditMode) then
  begin
    // Note info
    StartVal[0] := IntToStr(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].StartBeat);
    SelectsS[StartSlideId].TextOpt[0].Text := StartVal[0];
    DurationVal[0] := IntToStr(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Duration);
    SelectsS[DurationSlideId].TextOpt[0].Text := DurationVal[0];
    ToneVal[0] := IntToStr(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone) + ' (' + GetNoteName(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Tone) + ')';
    SelectsS[ToneSlideId].TextOpt[0].Text := ToneVal[0];
    LyricVal[0] := CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[CurrentNote[CurrentTrack]].Text;
    SelectsS[LyricSlideId].TextOpt[0].Text := LyricVal[0];
  end;

  // Text Edit Mode
  if TextEditMode or
     TitleEditMode or
     ArtistEditMode or
     LanguageEditMode or
     EditionEditMode or
     GenreEditMode or
     YearEditMode or
     CreatorEditMode or
     P1EditMode or
     P2EditMode or
     BPMEditMode then
  begin
    if TextPosition >= 0 then
    SelectsS[CurrentSlideId].TextOpt[0].Text :=
      UTF8Copy(CurrentEditText, 1, TextPosition) + '|' + UTF8Copy(CurrentEditText, TextPosition+1, LengthUTF8(CurrentEditText)-TextPosition);
    editLengthText := LengthUTF8(SelectsS[CurrentSlideId].TextOpt[0].Text);
  end;

  // draw static menu
  DrawBG;

  // header background
  EditDrawBorderedBox(Theme.EditSub.HeaderBackground.X, Theme.EditSub.HeaderBackground.Y, Theme.EditSub.HeaderBackground.W, Theme.EditSub.HeaderBackground.H, Theme.EditSub.HeaderBackground.ColR, Theme.EditSub.HeaderBackground.ColG, Theme.EditSub.HeaderBackground.ColB, Theme.EditSub.HeaderBackground.Alpha);

  // currently selected note info background
  EditDrawBorderedBox(Theme.EditSub.CurrentNoteInfoBackground.X, Theme.EditSub.CurrentNoteInfoBackground.Y, Theme.EditSub.CurrentNoteInfoBackground.W, Theme.EditSub.CurrentNoteInfoBackground.H, Theme.EditSub.CurrentNoteInfoBackground.ColR, Theme.EditSub.CurrentNoteInfoBackground.ColG, Theme.EditSub.CurrentNoteInfoBackground.ColB, Theme.EditSub.CurrentNoteInfoBackground.Alpha);

  // volume slider background
  EditDrawBorderedBox(Theme.EditSub.VolumeSliderBackground.X, Theme.EditSub.VolumeSliderBackground.Y, Theme.EditSub.VolumeSliderBackground.W, Theme.EditSub.VolumeSliderBackground.H, Theme.EditSub.VolumeSliderBackground.ColR, Theme.EditSub.VolumeSliderBackground.ColG, Theme.EditSub.VolumeSliderBackground.ColB, Theme.EditSub.VolumeSliderBackground.Alpha);

  // P1 info bar
  DrawInfoBar(Theme.EditSub.P1InfoBarBackground.X, Theme.EditSub.P1InfoBarBackground.Y, Theme.EditSub.P1InfoBarBackground.W, Theme.EditSub.P1InfoBarBackground.H, Theme.EditSub.P1InfoBarBackground.ColR, Theme.EditSub.P1InfoBarBackground.ColG, Theme.EditSub.P1InfoBarBackground.ColB, Theme.EditSub.P1InfoBarBackground.Alpha, 0);

  // P2 info bar - for duet mode
  if (CurrentSong.isDuet) then
    DrawInfoBar(Theme.EditSub.P2InfoBarBackground.X, Theme.EditSub.P2InfoBarBackground.Y, Theme.EditSub.P2InfoBarBackground.W, Theme.EditSub.P2InfoBarBackground.H, Theme.EditSub.P2InfoBarBackground.ColR, Theme.EditSub.P2InfoBarBackground.ColG, Theme.EditSub.P2InfoBarBackground.ColB, Theme.EditSub.P2InfoBarBackground.Alpha, 1);

  // sentence background
  EditDrawBorderedBox(Theme.EditSub.SentenceBackground.X, Theme.EditSub.SentenceBackground.Y, Theme.EditSub.SentenceBackground.W, Theme.EditSub.SentenceBackground.H, Theme.EditSub.SentenceBackground.ColR, Theme.EditSub.SentenceBackground.ColG, Theme.EditSub.SentenceBackground.ColB, Theme.EditSub.SentenceBackground.Alpha);

  //inherited Draw;
  DrawFG;
  // draw notes

  //Error Drawing when no Song is loaded
  if not Error then
  begin
    if Xmouse < 0 then
    begin
      // notes background
      EditDrawBorderedBox(Theme.EditSub.NotesBackground.X, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.W + Xmouse, Theme.EditSub.NotesBackground.H, Theme.EditSub.NotesBackground.ColR, Theme.EditSub.NotesBackground.ColG, Theme.EditSub.NotesBackground.ColB, Theme.EditSub.NotesBackground.Alpha);
      // horizontal lines
      SingDrawNoteLines(Theme.EditSub.NotesBackground.X, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.X + Theme.EditSub.NotesBackground.W + Xmouse);
      // vertical lines
      EditDrawBeatDelimiters(Theme.EditSub.NotesBackground.X + NotesSkipX, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.W - 2 * NotesSkipX + Xmouse, Theme.EditSub.NotesBackground.H, CurrentTrack);
      // draw notes
      EditDrawLine(Theme.EditSub.NotesBackground.X + NotesSkipX, Theme.EditSub.NotesBackground.Y + 7 * LineSpacing, Theme.EditSub.NotesBackground.W - 2 * NotesSkipX + Xmouse, Theme.EditSub.NotesBackground.W, CurrentTrack);
      // draw text on notes
      DrawText(Theme.EditSub.NotesBackground.X + NotesSkipX, Theme.EditSub.NotesBackground.Y + 7 * LineSpacing, Theme.EditSub.NotesBackground.W - 2 * NotesSkipX + Xmouse, Theme.EditSub.NotesBackground.H, CurrentTrack);
      // pitch contour in front
      DrawVocalsWaveform(Theme.EditSub.NotesBackground.X + NotesSkipX, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.W - 2 * NotesSkipX + Xmouse, Theme.EditSub.NotesBackground.H, CurrentTrack);
    end
    else
    begin
      // notes background
      EditDrawBorderedBox(Theme.EditSub.NotesBackground.X + Xmouse, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.W - Xmouse, Theme.EditSub.NotesBackground.H, Theme.EditSub.NotesBackground.ColR, Theme.EditSub.NotesBackground.ColG, Theme.EditSub.NotesBackground.ColB, Theme.EditSub.NotesBackground.Alpha);
      // horizontal lines
      SingDrawNoteLines(Theme.EditSub.NotesBackground.X + Xmouse, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.X + Theme.EditSub.NotesBackground.W);
      // vertical lines
      EditDrawBeatDelimiters(Theme.EditSub.NotesBackground.X + NotesSkipX + Xmouse, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.W - 2 * NotesSkipX, Theme.EditSub.NotesBackground.H, CurrentTrack);
      // draw notes
      EditDrawLine(Theme.EditSub.NotesBackground.X + NotesSkipX + Xmouse, Theme.EditSub.NotesBackground.Y + (7/9) * Theme.EditSub.NotesBackground.H, Theme.EditSub.NotesBackground.W - 2 * NotesSkipX, Theme.EditSub.NotesBackground.H, CurrentTrack);
      // draw text on notes
      DrawText(Theme.EditSub.NotesBackground.X + NotesSkipX + Xmouse, Theme.EditSub.NotesBackground.Y + (7/9) * Theme.EditSub.NotesBackground.H, Theme.EditSub.NotesBackground.W - 2 * NotesSkipX, Theme.EditSub.NotesBackground.H, CurrentTrack);
      // pitch contour in front
      DrawVocalsWaveform(Theme.EditSub.NotesBackground.X + NotesSkipX + Xmouse, Theme.EditSub.NotesBackground.Y, Theme.EditSub.NotesBackground.W - 2 * NotesSkipX, Theme.EditSub.NotesBackground.H, CurrentTrack);
    end;

    if Xmouse <> 0 then
       GoldenRec.KillAll;
  end;

  CurrentSound := AudioInputProcessor.Sound[0];
  CurrentSound.AnalyzeBuffer;
  if (CurrentSound.ToneString <> '-') then
  begin
    Count := trunc((720 / (GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].EndBeat) - GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat)))*(GetPlaybackPosition - GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[CurrentSong.Tracks[CurrentTrack].CurrentLine].Notes[0].StartBeat)));
    DrawPlayerTrack(CurrentSound.Tone, Count, CurrentNote[CurrentTrack]);
  end;

  GoldenRec.SpawnRec;

  // draw text
  if TextEditMode then
    EditorLyrics[CurrentTrack].SetCursor(CurrentNote[CurrentTrack], TextPosition)
  else
    EditorLyrics[CurrentTrack].ClearCursor;
  EditorLyrics[CurrentTrack].Draw;

  DrawMaxScoreInfo(
    Theme.EditSub.BackgroundImage.X,
    Theme.EditSub.BackgroundImage.Y,
    Theme.EditSub.BackgroundImage.W,
    Theme.EditSub.BackgroundImage.H,
    CurrentTrack);

  //video
  if Assigned(fCurrentVideo) then
  begin
    fCurrentVideo.GetFrame(CurrentSong.VideoGAP + GetPlaybackPosition);
    fCurrentVideo.SetScreen(1);
    fCurrentVideo.Alpha := 1;
    fCurrentVideo.SetScreenPosition(theme.EditSub.BackgroundImage.X, theme.EditSub.BackgroundImage.Y, 1);
    fCurrentVideo.Width := theme.EditSub.BackgroundImage.W;
    fCurrentVideo.Height := theme.EditSub.BackgroundImage.H;
    fCurrentVideo.ReflectionSpacing := 1;
    fCurrentVideo.AspectCorrection := acoLetterBox;
    fCurrentVideo.Draw;
  end;

  if HasVocalsPreviewSource and FAnalysisThreadRunning and (FAnalysisProgressPct < 100) and
     not (PlaySentence or PlayVideo or PlayOne) then
  begin
    if FAnalysisThreadBuildPreview then
      Text[TextInfo].Text := 'Vocals preview building: ' + IntToStr(FAnalysisProgressPct) + '%'
    else
      Text[TextInfo].Text := 'Pitch contour building: ' + IntToStr(FAnalysisProgressPct) + '%';
  end;

  if (not FAnalysisThreadRunning) and (FAnalysisProgressPct >= 100) and
     ((Text[TextInfo].Text = 'Vocals preview building: 100%') or
      (Text[TextInfo].Text = 'Pitch contour building: 100%')) then
    Text[TextInfo].Text := '';

  Result := true;
end;

procedure TScreenEditSub.OnHide;
begin
  Ini.EditorClickLeadMs := PrerollAudioIndex - 500;
  Ini.EditorMidiLeadMs := PrerollMidiIndex - 500;
  Ini.Save;

  StopAnalysisThread;
  {$IFDEF UseMIDIPort}
  StopMidi;
  MidiOut.Close;
  MidiOut.Free;
  {$ENDIF}

  //Music.SetVolume(1.0);
  AudioInput.CaptureStop;
  RestoreStandardPlayback(false);
  ClearVocalsWaveformCache;
end;

function TScreenEditSub.GetNoteName(Note: Integer): string;
var
  NormNote: Integer;
  Octave:   Integer;
const
  SUB_MINUS = #$E2#$82#$8B;
  SUB_0     = #$E2#$82#$80;
  SUB_1     = #$E2#$82#$81;
  SUB_2     = #$E2#$82#$82;
  SUB_3     = #$E2#$82#$83;
  SUB_4     = #$E2#$82#$84;
  SUB_5     = #$E2#$82#$85;
  SUB_6     = #$E2#$82#$86;
  SUB_7     = #$E2#$82#$87;
  SUB_8     = #$E2#$82#$88;
  SUB_9     = #$E2#$82#$89;
begin
  if (Note >= 0) then
  begin
    NormNote := Note mod 12;
    Octave := Note div 12;
  end
  else
  begin
    NormNote := (Note + (-Trunc(Note/12)+1)*12) mod 12;
    Octave := (Note + 1) div 12 - 1;
  end;

  case NormNote of
     0: Result := Language.Translate('EDIT_NOTENAME_C');
     1: Result := Language.Translate('EDIT_NOTENAME_C_SHARP');
     2: Result := Language.Translate('EDIT_NOTENAME_D');
     3: Result := Language.Translate('EDIT_NOTENAME_D_SHARP');
     4: Result := Language.Translate('EDIT_NOTENAME_E');
     5: Result := Language.Translate('EDIT_NOTENAME_F');
     6: Result := Language.Translate('EDIT_NOTENAME_F_SHARP');
     7: Result := Language.Translate('EDIT_NOTENAME_G');
     8: Result := Language.Translate('EDIT_NOTENAME_G_SHARP');
     9: Result := Language.Translate('EDIT_NOTENAME_A');
    10: Result := Language.Translate('EDIT_NOTENAME_A_SHARP');
    11: Result := Language.Translate('EDIT_NOTENAME_B');
  end;

  case Octave of
    -3: Result := Result + SUB_MINUS + SUB_1; // subsub-contra: '''C - '''B
    -2: Result := Result + SUB_0;             // sub-contra:     ''C - ''B
    -1: Result := Result + SUB_1;             // contra:          'C - 'B
     0: Result := Result + SUB_2;             // great:            C - B
     1: Result := Result + SUB_3;             // small:            c - b
     2: Result := Result + SUB_4;             // one-lined:       c' - b'
     3: Result := Result + SUB_5;             // two-lined:      c'' - b''
     4: Result := Result + SUB_6;             // three-lined:   c''' - b'''
     5: Result := Result + SUB_7;             // four-lined:   c'''' - b''''
     6: Result := Result + SUB_8;             // five-lined:  c''''' - b'''''
     7: Result := Result + SUB_9;             // six-lined:  c'''''' - b''''''
  end;
end;

function TScreenEditSub.GetMedleyLength: real;
begin
  if MedleyNotes.isStart and MedleyNotes.isEnd then
  begin
    Result := GetTimeFromBeat(
      CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].StartBeat +
      CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.end_.line].Notes[MedleyNotes.end_.note].Duration) -
      GetTimeFromBeat(CurrentSong.Tracks[CurrentTrack].Lines[MedleyNotes.start.line].Notes[MedleyNotes.start.note].StartBeat);
    Result := Round(Result * 100) / 100;
  end else
    Result := 0;
end;

end.
