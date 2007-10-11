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
      procedure InitializeRecord;
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
      procedure CaptureStart;
      procedure CaptureStop;
      procedure CaptureCard(RecordI, PlayerLeft, PlayerRight: byte);
      procedure StopCard(Card: byte);
      function LoadSoundFromFile(var hStream: hStream; Name: string): boolean;

      //Equalizer
      function GetFFTData: TFFTData;

      //Custom Sounds
      function LoadCustomSound(const Filename: String): Cardinal;
      procedure PlayCustomSound(const Index: Cardinal );
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

implementation

uses
  uLog,
  UMusic_BASS;

var
  singleton_AudioPlayback : IAudioPlayback;

function AudioPlayback(): IAudioPlayback;
begin
  if singleton_AudioPlayback = nil then
  begin
    writeln( 'Created AudioPlayback' );
    singleton_AudioPlayback := TMusic_bass.create();
  end;

  result := singleton_AudioPlayback;
end;

procedure InitializeSound;
begin
    Log.LogStatus('Initializing Playback', 'InitializeSound');
    AudioPlayback.InitializePlayback;

    Log.LogStatus('Initializing Record', 'InitializeSound');
    AudioPlayback.InitializeRecord;
end;

end.
