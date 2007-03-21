unit ModiSDK;

interface

type  //PluginInfo, for Init
  TPluginInfo = record
    //Info
    Name: Array [0..32] of Char;         //Modi to Register for the Plugin
    Creator: Array [0..32] of Char;      //Name of the Author
    PluginDesc: Array [0..64] of Char;   //Plugin Description

    //Options
    LoadSong:      boolean; //Whether or not a Song should be Loaded
    //Only When Song is Loaded:
    ShowNotes:     boolean; //Whether the Note Lines should be displayed
    LoadVideo:     boolean; //Should the Video be loaded ?
    LoadBack:      boolean; //Should the Background be loaded ?

    ShowRateBar:   boolean; //Whether the Bar that shows how good the player was sould be displayed
    ShowRateBar_O: boolean; //Load from Ini whether the Bar should be Displayed

    EnLineBonus:   boolean; //Whether LineBonus Should be enabled
    EnLineBonus_O: boolean; //Load from Ini whether LineBonus Should be enabled

    BGShowFull:    boolean; //Whether the Background or the Video should be shown Fullsize
    BGShowFull_O:  boolean; //Whether the Background or the Video should be shown Fullsize

    //Options -> everytime
    ShowScore:     boolean; //Whether or not the Score should be shown
    ShowBars:      boolean; //Whether the White Bars on Top and Bottom should be Drawn
    TeamModeOnly:  boolean; //If True the Plugin can only be Played in Team Mode
    GetSoundData:  boolean; //If True the RData Procedure is called when new SoundData is available
    Dummy:         boolean; //Should be Set to False... for Updateing Plugin Interface

    NumPlayers: Byte;   //Number of Available Players for Modi
    //Set different Bits
    //1 -> One Player
    //2 -> Two Players
    //4 -> Three Players
    //8 -> Four Players
    //16-> Six Players
    //e.g. : 10 -> Playable with 2 and 4 Players

  end;

  TPlayerInfo = record
    NumPlayers: Byte;
    Playerinfo: array[0..5] of record
      Name: PChar; //Name of the Player
      Score:Word;  //Players Score
      Bar:  Byte;  //Percentage of the SingBar filled
      PosX: Real;  //PosX of Players SingBar
      PosY: Real;  //PosY "
      Enabled: Boolean; //Whether the Player could get Points
      Percentage: Byte; //Percentage Shown on the Score Screen
    end;
  end;

  TTeamInfo = record
    NumTeams: Byte;
    Teaminfo: array[0..5] of record
      Name:  PChar;
      Score: Word;
      Joker: Byte;
      CurPlayer: Byte;
      NumPlayers: Byte;
      Playerinfo: array[0..3] of record
        Name: PChar;
        TimesPlayed: Byte;
        
      end;
    end;
  end;

  TsmallTexture = record
    TexNum:   integer;
    W:        real;
    H:        real;
    ScaleW:   real; // for dynamic scalling while leaving width constant
    ScaleH:   real; // for dynamic scalling while leaving height constant
  end;

  TSentences = record
    Akt:      integer;      // aktualna czesc utworu do rysowania
    High:     integer;
    Ilosc:    integer;
    Resolution: integer;
    NotesGAP: integer;
    TotalLength:  integer;
    Sentence:    array of record
      Start:    integer;
      StartNote:  integer;
      Lyric:      string;
      LyricWidth: real;
      Koniec:   integer;
      BaseNote: integer;
      HighNote:  integer;
      IlNut:    integer;
      TotalNotes: integer;
      Note:     array of record
        Color:      integer;
        Start:      integer;
        Length:    integer;
        Ton:        integer;
        TonGamy:    integer;
        //Text:      string;
        FreeStyle:  boolean;
        Typ:    integer;    // zwykla nuta x1, zlota nuta x2
      end;
    end;
  end;

  //AChar = array [0..254] of Char;

  DWORD = Longword;
  HSTREAM = DWORD;

  //Routines to gave to the Plugin
  fModi_LoadTex = function (const Name, Typ: PChar): TsmallTexture; stdcall; //Pointer to Texture Loader
  //fModi_Translate = function (const Name, Translation: AChar): Integer; stdcall;       //Pointer to Translator
  fModi_Print = procedure (const Style, Size: Byte; const X, Y: Real; const Text: PChar); stdcall;       //Procedure to Print Text   //Now translated automatically
  fModi_LoadSound = function (const Name: PChar): Cardinal; stdcall;       //Procedure that loads a Custom Sound
  fModi_PlaySound = procedure (const Index: Cardinal); stdcall;       //Plays a Custom Sound

  //DLL Funktionen
  //Gave the Plugins Info
  pModi_PluginInfo = procedure (var Info: TPluginInfo); stdcall;
  //Executed on Game Start //If True Game begins, else Failure
  fModi_Init = function (const TeamInfo: TTeamInfo; var Playerinfo: TPlayerinfo; const Sentences: TSentences; const LoadTex: fModi_LoadTex; const Print: fModi_Print; LoadSound: fModi_LoadSound; PlaySound: fModi_PlaySound): boolean; stdcall;
  //Executed everytime the Screen is Drawed //If False The Game finishes
  fModi_Draw = function (var Playerinfo: TPlayerinfo; const CurSentence: Cardinal): boolean; stdcall;
  //Is Executed on Finish, Returns the Playernum of the Winner
  fModi_Finish = function (var Playerinfo: TPlayerinfo): byte; stdcall;
  //Procedure called when new Sound Data is available
  pModi_RData = procedure (handle: HSTREAM; buffer: Pointer; len: DWORD; user: DWORD); stdcall;

implementation

end.
