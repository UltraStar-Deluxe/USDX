program UltraStar;

{$DEFINE TRANSLATE}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ELSE}
  {$R 'UltraStar.res' 'UltraStar.rc'}
{$ENDIF}

{$I switches.inc}

uses

  // ***************************************************************************
  //
  //                         Developers PLEASE NOTE !!!!!!!
  //
  //  As of september 2007, I am working towards porting Ultrastar-DX to run
  //  on Linux.  I will be modifiying the source to make it compile in lazarus
  //  on windows & linux and I will make sure that it compiles in delphi still
  //  To help me in this endevour, please can you make a point of remembering
  //  that linux is CASE SENSATIVE, and file / unit names must be as per
  //  the filename exactly.
  //
  //  EG :  opengl12.pas  must not be OpenGL in the uses cluase.
  //
  //  thanks for your help...
  //
  // ***************************************************************************

  // Interesting stuff... :)
  // http://burningsmell.org/sdl_audioin/


  //------------------------------
  //Includes - 3rd Party Libraries
  //------------------------------
  
  // SDL / OpenGL
  moduleloader           in 'lib\JEDI-SDLv1.0\SDL\Pas\moduleloader.pas',
  opengl12               in 'lib\JEDI-SDLv1.0\OpenGL\Pas\opengl12.pas',
  sdl                    in 'lib\JEDI-SDLv1.0\SDL\Pas\sdl.pas',
  sdl_image              in 'lib\JEDI-SDLv1.0\SDL_Image\Pas\sdl_image.pas',
  sdl_ttf                in 'lib\JEDI-SDLv1.0\SDL_ttf\Pas\sdl_ttf.pas',
  sdlutils               in 'lib\JEDI-SDLv1.0\SDL\Pas\sdlutils.pas',


  // Bass
  {$IFDEF UseBASS}
  bass                   in 'lib\bass\delphi\bass.pas',
  {$ENDIF}

  // Midi Units
  {$IFDEF UseMIDIPort}
  Circbuf                in 'lib\midi\CIRCBUF.PAS',
  Delphmcb               in 'lib\midi\Delphmcb.PAS',
  MidiCons               in 'lib\midi\MidiCons.PAS',
  MidiDefs               in 'lib\midi\MidiDefs.PAS',
  MidiFile               in 'lib\midi\MidiFile.PAS',
  midiin                 in 'lib\midi\midiin.pas',
  midiout                in 'lib\midi\midiout.pas',
  MidiType               in 'lib\midi\MidiType.PAS',
  {$ENDIF}

  // FFMpeg units
  avcodec                in 'lib\ffmpeg\avcodec.pas',
  avformat               in 'lib\ffmpeg\avformat.pas',
  avio                   in 'lib\ffmpeg\avio.pas',
  avutil                 in 'lib\ffmpeg\avutil.pas',
  opt                    in 'lib\ffmpeg\opt.pas',
  rational               in 'lib\ffmpeg\rational.pas',


  // Sql Lite
  SQLiteTable3           in 'lib\SQLite\SQLiteTable3.pas',
  SQLite3                in 'lib\SQLite\SQLite3.pas',


  //------------------------------
  //Includes - Menu System
  //------------------------------
  
  UDisplay               in 'Menu\UDisplay.pas',
  UDrawTexture           in 'Menu\UDrawTexture.pas',
  UMenu                  in 'Menu\UMenu.pas',
  UMenuButton            in 'Menu\UMenuButton.pas',
  UMenuButtonCollection  in 'Menu\UMenuButtonCollection.pas',
  UMenuInteract          in 'Menu\UMenuInteract.pas',
  UMenuSelect            in 'Menu\UMenuSelect.pas',
  UMenuSelectSlide       in 'Menu\UMenuSelectSlide.pas',
  UMenuStatic            in 'Menu\UMenuStatic.pas',
  UMenuText              in 'Menu\UMenuText.pas',

  //------------------------------
  //Includes - Classes
  //------------------------------
  
  {$IFDEF FPC}
  ulazjpeg               in 'Classes\Ulazjpeg.pas',
  {$ENDIF}

  TextGL                 in 'Classes\TextGL.pas',
  UCatCovers             in 'Classes\UCatCovers.pas',
  UCommandLine           in 'Classes\UCommandLine.pas',
  UCommon                in 'Classes\UCommon.pas',
  UCovers                in 'Classes\UCovers.pas',
  UDataBase              in 'Classes\UDataBase.pas',
  UDLLManager            in 'Classes\UDLLManager.pas',
  UDraw                  in 'Classes\UDraw.pas',
  UFiles                 in 'Classes\UFiles.pas',
  UGraphic               in 'Classes\UGraphic.pas',
  UGraphicClasses        in 'Classes\UGraphicClasses.pas',
  UIni                   in 'Classes\UIni.pas',
  UJoystick              in 'Classes\UJoystick.pas',
  ULanguage              in 'Classes\ULanguage.pas',
  ULCD                   in 'Classes\ULCD.pas',
  ULight                 in 'Classes\ULight.pas',
  ULog                   in 'Classes\ULog.pas',
  ULyrics                in 'Classes\ULyrics.pas',
  ULyrics_bak            in 'Classes\ULyrics_bak.pas',
  UMain                  in 'Classes\UMain.pas',


  UMusic                 in 'Classes\UMusic.pas',

  UMedia_dummy           in 'Classes\UMedia_dummy.pas',  
  UVideo                 in 'Classes\UVideo.pas',
  UAudio_FFMpeg          in 'Classes\UAudio_FFMpeg.pas',
{$ifdef win32}
  UAudio_bass            in 'Classes\UAudio_bass.pas',
{$endif}

//   UAudio_fmod            in 'Classes\UAudio_fmod.pas', // this has not yet been developed.. :(

  UParty                 in 'Classes\UParty.pas',
  UPlaylist              in 'Classes\UPlaylist.pas',
  URecord                in 'Classes\URecord.pas',
  USkins                 in 'Classes\USkins.pas',
  USingScores            in 'Classes\USingScores.pas',
  USongs                 in 'Classes\USongs.pas',
  UTexture               in 'Classes\UTexture.pas',
  UThemes                in 'Classes\UThemes.pas',
  UTime                  in 'Classes\UTime.pas',
  USingNotes 		         in 'Classes\USingNotes.pas',
  
  uPluginLoader          in 'Classes\uPluginLoader.pas',
  UCoreModule            in 'Classes\UCoreModule.pas',
  UServices              in 'Classes\UServices.pas',
  UCore                  in 'Classes\UCore.pas',
  UHooks                 in 'Classes\UHooks.pas',
  


  //------------------------------
  //Includes - Screens
  //------------------------------
  UScreenCredits         in 'Screens\UScreenCredits.pas',
  UScreenEdit            in 'Screens\UScreenEdit.pas',
  UScreenEditConvert     in 'Screens\UScreenEditConvert.pas',
  UScreenEditHeader      in 'Screens\UScreenEditHeader.pas',
  UScreenEditSub         in 'Screens\UScreenEditSub.pas',
  UScreenLevel           in 'Screens\UScreenLevel.pas',
  UScreenLoading         in 'Screens\UScreenLoading.pas',
  UScreenMain            in 'Screens\UScreenMain.pas',
  UScreenName            in 'Screens\UScreenName.pas',
  UScreenOpen            in 'Screens\UScreenOpen.pas',
  UScreenOptions         in 'Screens\UScreenOptions.pas',
  UScreenOptionsAdvanced in 'Screens\UScreenOptionsAdvanced.pas',
  UScreenOptionsGame     in 'Screens\UScreenOptionsGame.pas',
  UScreenOptionsGraphics in 'Screens\UScreenOptionsGraphics.pas',
  UScreenOptionsLyrics   in 'Screens\UScreenOptionsLyrics.pas',
  UScreenOptionsRecord   in 'Screens\UScreenOptionsRecord.pas',
  UScreenOptionsSound    in 'Screens\UScreenOptionsSound.pas',
  UScreenOptionsThemes   in 'Screens\UScreenOptionsThemes.pas',
  UScreenPopup           in 'Screens\UScreenPopup.pas',
  UScreenScore           in 'Screens\UScreenScore.pas',
  UScreenSing            in 'Screens\UScreenSing.pas',
  UScreenSong            in 'Screens\UScreenSong.pas',
  UScreenSongJumpto      in 'Screens\UScreenSongJumpto.pas',
  UScreenSongMenu        in 'Screens\UScreenSongMenu.pas',
  UScreenStatDetail      in 'Screens\UScreenStatDetail.pas',
  UScreenStatMain        in 'Screens\UScreenStatMain.pas',
  UScreenTop5            in 'Screens\UScreenTop5.pas',
  UScreenWelcome         in 'Screens\UScreenWelcome.pas',

  //------------------------------
  //Includes - Screens PartyMode
  //------------------------------
  UScreenPartyNewRound   in 'Screens\UScreenPartyNewRound.pas',
  UScreenPartyOptions    in 'Screens\UScreenPartyOptions.pas',
  UScreenPartyPlayer     in 'Screens\UScreenPartyPlayer.pas',
  UScreenPartyScore      in 'Screens\UScreenPartyScore.pas',
  UScreenPartyWin        in 'Screens\UScreenPartyWin.pas',
  UScreenSingModi        in 'Screens\UScreenSingModi.pas',

  //------------------------------
  //Includes - Modi SDK
  //------------------------------
  ModiSDK                in '..\..\Modis\SDK\ModiSDK.pas',
  UPluginDefs            in '..\..\Modis\SDK\UPluginDefs.pas',

  //------------------------------
  //Includes - Delphi
  //------------------------------
  {$IFDEF win32}
  Windows,
  {$ENDIF}
  SysUtils;

const
  Version = 'UltraStar Deluxe V 1.10 Alpha Build';

begin

  main();


end.
