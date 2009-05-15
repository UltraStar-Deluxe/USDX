unit ModiSDK;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

type  // PluginInfo, for init
  TPluginInfo = record
    // Info
    Name:       array [0..32] of char;   // modus to register for the plugin
    Creator:    array [0..32] of char;   // name of the author
    PluginDesc: array [0..64] of char;   // plugin description

    // plugin type, atm: 8 only for partymode modus
    Case Typ: byte of
      8: (
        // Options
        LoadSong:      boolean; // Whether or not a song should be loaded
        // Only when song is loaded:
        ShowNotes:     boolean; // Whether the note lines should be displayed
        LoadVideo:     boolean; // Should the video be loaded?
        LoadBack:      boolean; // Should the background be loaded?

        ShowRateBar:   boolean; // Whether the bar that shows how good the player was sould be displayed
        ShowRateBar_O: boolean; // Load from ini whether the bar should be displayed

        EnLineBonus:   boolean; // Whether line bonus should be enabled
        EnLineBonus_O: boolean; // Load from ini whether line bonus should be enabled

        BGShowFull:    boolean; // Whether the background or the video should be shown full size
        BGShowFull_O:  boolean; // Whether the background or the video should be shown full size

        // Options -> everytime
        ShowScore:     boolean; // Whether or not the score should be shown
        ShowBars:      boolean; // Whether the white bars on top and bottom should be drawn
        TeamModeOnly:  boolean; // If true the plugin can only be played in team mode
        GetSoundData:  boolean; // If true the rdata procedure is called when new sound data is available
        Dummy:         boolean; // Should be set to false... for updating plugin interface

        NumPlayers: byte   // Number of available players for modus
        // Set different bits
        // 1 -> one player
        // 2 -> two players
        // 4 -> three players
        // 8 -> four players
        // 16-> six players
        // e.g. : 10 -> playable with 2 and 4 players
        );

  end;

  TPlayerInfo = record
    NumPlayers: byte;
    Playerinfo: array[0..5] of record
      Name:       PChar;   // Name of the player
      Score:      word;    // Player's score
      Bar:        byte;    // Percentage of the singbar filled
      PosX:       real;    // PosX of player's singbar
      PosY:       real;    // PosY "
      Enabled:    boolean; // Whether the player could get points
      Percentage: byte;    // Percentage shown on the score screen
    end;
  end;

  TTeamInfo = record
    NumTeams: byte;
    Teaminfo: array[0..5] of record
      Name:       PChar;
      Score:      word;
      Joker:      byte;
      CurPlayer:  byte;
      NumPlayers: byte;
      Playerinfo: array[0..3] of record
        Name:        PChar;
        TimesPlayed: byte;
      end;
    end;
  end;

  TsmallTexture = record
    TexNum: integer;
    W:      real;
    H:      real;
  end;

  TSentences = record
    Current:     integer;      // current part of a line
    High:        integer;
    Number:      integer;
    Resolution:  integer;
    NotesGAP:    integer;
    TotalLength: integer;
    Sentence:    array of record
      Start:      integer;
      StartNote:  integer;
      Lyric:      string;
      LyricWidth: real;
      End_:       integer;
      BaseNote:   integer;
      HighNote:   integer;
      IlNut:      integer;
      TotalNotes: integer;
      Note:       array of record
        Color:     integer;
        Start:     integer;
        Length:    integer;
        Tone:      integer;
        //Text:      string;
        FreeStyle: boolean;
        Typ:      integer;    // normal note x1, golden note x2
      end;
    end;
  end;

  dword   = longword;
  hstream = dword;

  TTextureType = (
    TEXTURE_TYPE_PLAIN,        // Plain (alpha = 1)
    TEXTURE_TYPE_TRANSPARENT,  // Alpha is used
    TEXTURE_TYPE_COLORIZED     // Alpha is used; Hue of the HSV color-model will be replaced by a new value
  );

  // Routines to give to the plugin
  fModi_LoadTex = function (const Name: PChar; Typ: TTextureType): TsmallTexture; // Pointer to texture loader
    {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  //fModi_Translate = function (const Name, Translation: AChar): integer; // Pointer to translator
  //  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  fModi_Print = procedure (const Style, Size: byte; const X, Y: real; const Text: PChar); // Procedure to print text   // Now translated automatically
    {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  fModi_LoadSound = function (const Name: PChar): cardinal; // Procedure that loads a custom sound
    {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  pModi_PlaySound = procedure (const Index: cardinal); // Plays a custom sound
    {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  TMethodRec = record
    LoadTex:    fModi_LoadTex;
    Print:      fModi_Print;
    LoadSound:  fModi_LoadSound;
    PlaySound:  pModi_PlaySound;
  end;
  // DLL functions
  // Give the plugins info
  pModi_PluginInfo = procedure (var Info: TPluginInfo);
    {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  // Executed on game start // if true game begins, else failure
  fModi_Init = function (const TeamInfo: TTeamInfo; var Playerinfo: TPlayerinfo; const Sentences: TSentences; const Methods: TMethodRec): boolean;
    {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  // Executed everytime the screen is drawn // if false the game finishes
  fModi_Draw = function (var Playerinfo: TPlayerinfo; const CurSentence: cardinal): boolean;
    {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  // Is executed on finish, returns the player num of the winner
  fModi_Finish = function (var Playerinfo: TPlayerinfo): byte;
    {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  // Procedure called when new sound data is available
  pModi_RData = procedure (handle: hstream; buffer: pointer; len: dword; user: dword);
    {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

implementation

end.
