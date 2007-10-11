unit UMusic;

interface

{$I switches.inc}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses Classes // UCommon
     ;

type
  TMuzyka = record
    Path:   string;
    Start:  integer;        // start of song in ms
    IlNut:  integer;
    DlugoscNut:   integer;
  end;

  PLine = ^TLine;
  TLine = record
    Start:    integer;
    StartNote:  integer;
    Lyric:      string;
    LyricWidth: real;
    Koniec:   integer;
    BaseNote: integer;
    HighNut:  integer;
    IlNut:    integer;
    TotalNotes: integer;
    Nuta:     array of record
      Color:      integer;
      Start:      integer;
      Dlugosc:    integer;
      Ton:        integer;
      TonGamy:    integer;
      Tekst:      string;
      FreeStyle:  boolean;
      Wartosc:    integer;    // zwykla nuta x1, zlota nuta x2
    end;
  end;
  ALine = array of TLine;

  TCzesci = record
    Akt:      integer;      // aktualna czesc utworu do rysowania
    High:     integer;
    Ilosc:    integer;
    Resolution: integer;
    NotesGAP: integer;
    Wartosc:  integer;
    Czesc:    ALine;
  end;

  TCzas = record              // wszystko, co dotyczy aktualnej klatki
    OldBeat:      integer;    // poprzednio wykryty beat w utworze
    AktBeat:      integer;    // aktualny beat w utworze
    MidBeat:      real;       // dokladny AktBeat

    // now we use this for super synchronization!
    // only used when analyzing voice
    OldBeatD:     integer;    // poprzednio wykryty beat w utworze
    AktBeatD:     integer;    // aktualny beat w utworze
    MidBeatD:     real;       // dokladny AktBeatD
    FracBeatD:    real;       // fractional part of MidBeatD

    // we use this for audiable clicks
    OldBeatC:     integer;    // poprzednio wykryty beat w utworze
    AktBeatC:     integer;    // aktualny beat w utworze
    MidBeatC:     real;       // dokladny AktBeatC
    FracBeatC:    real;       // fractional part of MidBeatC


    OldCzesc:     integer;    // poprzednio wyswietlana czesc
                              // akt jest w czesci.akt

    Teraz:        real;       // aktualny czas w utworze
    Razem:        real;       // caly czas utworu
  end;

  TSoundCard = record
    Name:     string;
    Source:   array of string;
  end;

  TFFTData  = array [0..256] of Single;

  hStream = Cardinal;

  TCustomSoundEntry = record
    Filename : String;
    Handle   : hStream;
  end;

type
  IAudioPlayback = Interface
      procedure InitializePlayback;
      procedure SetVolume(Volume: integer);
      procedure SetMusicVolume(Volume: integer);
      procedure SetLoop(Enabled: boolean);
      function Open(Name: string): boolean; // true if succeed
      procedure Rewind;
      procedure MoveTo(Time: real);
      procedure Play;
      procedure Pause; //Pause Mod
      procedure Stop;
      procedure Close;
      function Finished: boolean;
      function Length: real;
      function Position: real;
      procedure PlayStart;
      procedure PlayBack;
      procedure PlaySwoosh;
      procedure PlayChange;
      procedure PlayOption;
      procedure PlayClick;
      procedure PlayDrum;
      procedure PlayHihat;
      procedure PlayClap;
      procedure PlayShuffle;
      procedure StopShuffle;

      function LoadSoundFromFile(var hStream: hStream; Name: string): boolean;

      //Custom Sounds
      function LoadCustomSound(const Filename: String): Cardinal;
      procedure PlayCustomSound(const Index: Cardinal );
  end;

  IAudioInput = Interface
      procedure InitializeRecord;

      procedure CaptureStart;
      procedure CaptureStop;

      procedure CaptureCard(RecordI, PlayerLeft, PlayerRight: byte);
      procedure StopCard(Card: byte);

      //Equalizer
      function GetFFTData: TFFTData;
  end;


var // TODO : JB --- THESE SHOULD NOT BE GLOBAL
  // muzyka
  Muzyka:   TMuzyka;

  // czesci z nutami;
  Czesci:   array of TCzesci;

  // czas
  Czas:     TCzas;
  

procedure InitializeSound;
 
function  AudioPlayback(): IAudioPlayback;
function  AudioInput(): IAudioInput;


implementation

uses
  uLog,
  UMusic_BASS;

var
  singleton_AudioPlayback : IAudioPlayback;
  singleton_AudioInput    : IAudioInput;

function AudioPlayback(): IAudioPlayback;
begin
  if singleton_AudioPlayback = nil then
  begin
    writeln( 'Created AudioPlayback' );
    singleton_AudioPlayback := TMusic_bass.create(); // Yes we could do this with one instance of TMusic_Bass... but I cant be bothered at this point:P
  end;

  result := singleton_AudioPlayback;
end;

function AudioInput(): IAudioInput;
begin
  if singleton_AudioInput = nil then
  begin
    writeln( 'Created AudioInput' );
    singleton_AudioInput := TMusic_bass.create();   // Yes we could do this with one instance of TMusic_Bass... but I cant be bothered at this point:P
  end;

  result := singleton_AudioInput;
end;

procedure InitializeSound;
begin
    Log.LogStatus('Initializing Playback', 'InitializeSound');
    AudioPlayback.InitializePlayback;

    Log.LogStatus('Initializing Record', 'InitializeSound');
    AudioInput.InitializeRecord;
end;

end.
