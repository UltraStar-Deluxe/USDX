unit UPartyDefs;
{*********************
  uPluginDefs
  Some Basic Structures and Functions used to communicate with Plugins
  Usable as Delphi Plugin SDK
*********************}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses UPluginDefs;

type
  //----------------
  // TUS_Party_Proc_Init - Structure of the Party Init Proc
  // This Function is called on SingScreen Init Everytime this Modi should be sung
  // Return Non Zero to Abort Party Modi Loading... In this Case another Plugin will be loaded
  //----------------
  TUS_Party_Proc_Init     = Function (ID: Integer): integer; stdcall;

  //----------------
  // TUS_Party_Proc_Draw - Structure of the Party Draw Proc
  // This Function is called on SingScreen Draw (Not when Paused). You should draw in this Proc
  // Return Non Zero to Finish Song... In this Case Score Screen is loaded
  //----------------
  TUS_Party_Proc_Draw     = Function (ID: Integer): integer; stdcall;

  //----------------
  // TUS_Party_Proc_DeInit - Structure of the Party DeInit Proc
  // This Function is called on SingScreen DeInit When Plugin abort Song or Song finishes
  // Return Winner
  //----------------
  TUS_Party_Proc_DeInit   = Function (ID: Integer): integer; stdcall;

  //----------------
  // TUS_ModiInfo - Some Infos from Plugin to Partymode.
  // Used to register party modi to Party manager
  // ---
  // Version Structure:
  // First  Byte: Head Revison
  // Second Byte: Sub Revison
  // Third  Byte: Sub Revision 2
  // Fourth Byte: Letter (For Bug Fix releases. 0 or 'a' .. 'z')
  //----------------
  TModiInfo_Name = Array [0..31] of Char;
  TModiInfo_Desc = Array [0..63] of Char;
  
  PUS_ModiInfo = ^TUS_ModiInfo;
  TUS_ModiInfo = record
    //Size of this record (usefull if record will be extended in the future)
    cbSize:       Integer; //Don't forget to set this as Plugin!

    //Infos about the Modi
    Name       : TModiInfo_Name;   //Modiname to Register for the Plugin
    Description: TModiInfo_Desc;   //Plugin Description

    //------------
    // Loading Settings
    // ---
    // Bit to Set | Triggered Option
    // 1          | Song should be loaded
    // 2          | Song has to be Non Duett
    // 4          | Song has to be Duett  (If 2 and 4 is set, both will be ignored)
    // 8          | Only Playable with 2 and more players
    // 16         | Restrict Background Loading
    // 32         | Restrict Video Loading
    // 64         | Increase TimesPlayed for Cur. Player
    // 128        | Not in Use, Don't set it!
    LoadingSettings: Byte;

    // SingScreen Settings
    // ---
    // Bit to Set | Triggered Option
    // 1          | ShowNotes
    // 2          | ShowScores
    // 4          | ShowTime
    // 8          | Start Audio Playback automaticaly
    // 16         | Not in Use, Don't set it!
    // 32         | Not in Use, Don't set it!
    // 64         | Not in Use, Don't set it!
    // 128        | Not in Use, Don't set it!
    SingScreenSettings: Byte;

    // With which count of players can this modi be played
    // ---
    //Set different Bits
    //1 -> One Player
    //2 -> Two Players
    //4 -> Three Players
    //8 -> Four Players
    //16-> Six Players
    //e.g. : 10 -> Playable with 2 and 4 Players
    NumPlayers: Byte;

    // ID that is given to the Party Procs when they are called
    // If this Modi is running
    // (e.g. to register Until 2000 and Until 5000 with the same Procs
    //  ID is the Max Point Count in this example)
    ID: Integer;

    // Party Procs called on Party
    // ---
    // Set to nil(C: NULL) if u don't want to use this method
    ModiInit:   TUS_Party_Proc_Init;
    ModiDraw:   TUS_Party_Proc_Draw;
    ModiDeInit: TUS_Party_Proc_DeInit;
  end;

  //--------------
  // Team Info Record. Used by "Party/GetTeamInfo" and "Party/SetTeamInfo"
  //--------------
  TTeamInfo = record
    NumTeams: Byte;
    Teaminfo: array[0..5] of record
      Name:  PChar;     //Teamname
      Score: Word;      //TeamScore
      Joker: Byte;      //Team Jokers available
      CurPlayer: Byte;  //Id of Cur. Playing Player
      NumPlayers: Byte;
      Playerinfo: array[0..3] of record
        Name: PChar;        //Playername
        TimesPlayed: Byte;  //How often this Player has Sung
      end;
    end;
  end;

//----------------
// Some Default Constants
//----------------
const
  // to use for TUS_ModiInfo.LoadingSettings
  MLS_LoadSong    = 1;  //Song should be loaded
  MLS_NotDuett    = 2;  //Song has to be Non Duett
  MLS_ForceDuett  = 4;  //Song has to be Duett  (If 2 and 4 is set, both will be ignored)
  MLS_TeamOnly    = 8;  //Only Playable with 2 and more players
  MLS_RestrictBG  = 16; //Restrict Background Loading
  MLS_RestrictVid = 32; //Restrict Video Loading
  MLS_IncTP       = 64; //Increase TimesPlayed for Cur. Player

  // to use with TUS_ModiInfo.SingScreenSettings
  MSS_ShowNotes   = 1;  //ShowNotes
  MSS_ShowScores  = 2;  //ShowScores
  MSS_ShowTime    = 4;  //ShowTime
  MSS_AutoPlayback= 8;  //Start Audio Playback automaticaly

  //Standard (Duell) for TUS_ModiInfo.LoadingSettings and TUS_ModiInfo.SingScreenSettings
  MLS_Standard    = MLS_LoadSong or MLS_IncTP;
  MSS_Standard    = MSS_ShowNotes or MSS_ShowScores or MSS_ShowTime or MSS_AutoPlayback;

//-------------
// Some helper functions to register Party Modi
//-------------
Function RegisterModi(const PluginInterface: PUS_PluginInterface; const Name: TModiInfo_Name; const Description: TModiInfo_Desc; const LoadingSettings, SingScreenSettings, NumPlayers: Byte; const ID: Integer; const ModiInit: TUS_Party_Proc_Init = nil; const ModiDeInit: TUS_Party_Proc_DeInit = nil; const ModiDraw: TUS_Party_Proc_Draw = nil): THandle;



implementation

//-------------
// Function that Prepares the ModiInfo Record and Calls Party/RegisterModi
//-------------
Function RegisterModi(const PluginInterface: PUS_PluginInterface; const Name: TModiInfo_Name; const Description: TModiInfo_Desc; const LoadingSettings, SingScreenSettings, NumPlayers: Byte; const ID: Integer; const ModiInit: TUS_Party_Proc_Init; const ModiDeInit: TUS_Party_Proc_DeInit; const ModiDraw: TUS_Party_Proc_Draw): THandle;
var
  ModiInfo: TUS_ModiInfo;
begin
  //Init Record
  ModiInfo.cbSize             := SizeOf(TUS_ModiInfo);

  ModiInfo.Name               := Name;
  ModiInfo.Description        := Description;
  ModiInfo.LoadingSettings    := LoadingSettings;
  ModiInfo.SingScreenSettings := SingScreenSettings;
  ModiInfo.NumPlayers         := NumPlayers;

  ModiInfo.ID                 := ID;
  ModiInfo.ModiInit           := ModiInit;
  ModiInfo.ModiDraw           := ModiDraw;
  ModiInfo.ModiDeInit         := ModiDeInit;

  //Call Service
  Result := PluginInterface.CallService('Party/RegisterModi', Integer(@ModiInfo), nil);
end;

end.