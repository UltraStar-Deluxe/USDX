{$IFNDEF FPC}  // This is here, so linux & MacOS X Versions can simply include the uses
               // from the dpr.  Saves dupicating the uses clause.
program UltraStar;

  {$R 'UltraStar.res' 'UltraStar.rc'}
  {$I switches.inc}

uses
{$ENDIF}

  //------------------------------
  //Includes - 3rd Party Libraries
  //------------------------------
  SDL           in 'lib\JEDI-SDLv1.0\SDL\Pas\SDL.pas',
  moduleloader  in 'lib\JEDI-SDLv1.0\SDL\Pas\moduleloader.pas',
  sdlutils      in 'lib\JEDI-SDLv1.0\SDL\Pas\sdlutils.pas',
  sdl_image     in 'lib\JEDI-SDLv1.0\SDL_Image\Pas\sdl_image.pas',
  OpenGL12      in 'lib\JEDI-SDLv1.0\OpenGL\Pas\OpenGL12.pas',
  sdl_ttf       in 'lib\JEDI-SDLv1.0\SDL_ttf\Pas\sdl_ttf.pas',

  bass          in 'lib\bass\delphi\bass.pas',

  {$ifdef delphi}
  midiout       in 'lib\midi\midiout.pas',
  midiin        in 'lib\midi\midiin.pas',
  CIRCBUF       in 'lib\midi\CIRCBUF.PAS',
  MidiType      in 'lib\midi\MidiType.PAS',
  MidiDefs      in 'lib\midi\MidiDefs.PAS',
  MidiCons      in 'lib\midi\MidiCons.PAS',
  MidiFile      in 'lib\midi\MidiFile.PAS',
  Delphmcb      in 'lib\midi\Delphmcb.PAS',

  DirWatch      in 'lib\other\DirWatch.pas',
  {$endif}

  avcodec       in 'lib\ffmpeg\avcodec.pas',
  avformat      in 'lib\ffmpeg\avformat.pas',
  avutil        in 'lib\ffmpeg\avutil.pas',
  rational      in 'lib\ffmpeg\rational.pas',
  opt           in 'lib\ffmpeg\opt.pas',
  avio          in 'lib\ffmpeg\avio.pas',


  SQLiteTable3  in 'lib\SQLite\SQLiteTable3.pas',
  SQLite3       in 'lib\SQLite\SQLite3.pas',

  //------------------------------
  //Includes - Menu System
  //------------------------------
  UDisplay               in 'Menu\UDisplay.pas',
  UMenu                  in 'Menu\UMenu.pas',
  UMenuStatic            in 'Menu\UMenuStatic.pas',
  UMenuText              in 'Menu\UMenuText.pas',
  UMenuButton            in 'Menu\UMenuButton.pas',
  UMenuInteract          in 'Menu\UMenuInteract.pas',
  UMenuSelect            in 'Menu\UMenuSelect.pas',
  UMenuSelectSlide       in 'Menu\UMenuSelectSlide.pas',
  UDrawTexture           in 'Menu\UDrawTexture.pas',
  UMenuButtonCollection  in 'Menu\UMenuButtonCollection.pas',

  //------------------------------
  //Includes - Classes
  //------------------------------  
  UCommon           in 'Classes\UCommon.pas',
  UGraphic          in 'Classes\UGraphic.pas',
  UTexture          in 'Classes\UTexture.pas',
  ULanguage         in 'Classes\ULanguage.pas',
  UMain             in 'Classes\UMain.pas',
  UDraw             in 'Classes\UDraw.pas',
  URecord           in 'Classes\URecord.pas',
  UTime             in 'Classes\UTime.pas',
  TextGL            in 'Classes\TextGL.pas',
  USongs            in 'Classes\USongs.pas',
  UIni              in 'Classes\UIni.pas',
  ULyrics           in 'Classes\ULyrics.pas',
  ULyrics_bak       in 'Classes\ULyrics_bak.pas',
  USkins            in 'Classes\USkins.pas',
  UThemes           in 'Classes\UThemes.pas',
  ULog              in 'Classes\ULog.pas',
  UJoystick         in 'Classes\UJoystick.pas',
  ULCD              in 'Classes\ULCD.pas',
  ULight            in 'Classes\ULight.pas',
  UDataBase         in 'Classes\UDataBase.pas',
  UCovers           in 'Classes\UCovers.pas',
  UCatCovers        in 'Classes\UCatCovers.pas',
  UFiles            in 'Classes\UFiles.pas',
  UGraphicClasses   in 'Classes\UGraphicClasses.pas',
  UDLLManager       in 'Classes\UDLLManager.pas',
  UPlaylist         in 'Classes\UPlaylist.pas',
  UCommandLine      in 'Classes\UCommandLine.pas',
  UTextClasses      in 'Classes\UTextClasses.pas',
  USingScores       in 'Classes\USingScores.pas',
  USingNotes        in 'Classes\USingNotes.pas',

  UModules          in 'Classes\UModules.pas',          //List of Modules to Load
  UHooks            in 'Classes\UHooks.pas',            //Hook Managing
  UServices         in 'Classes\UServices.pas',         //Service Managing
  UCore             in 'Classes\UCore.pas',             //Core, Maybe remove this
  UCoreModule       in 'Classes\UCoreModule.pas',       //^
  UPluginInterface  in 'Classes\UPluginInterface.pas',  //Interface offered by Core to Plugins
  UPluginLoader     in 'Classes\UPluginLoader.pas',     //New Plugin Loader Module

  UParty            in 'Classes\UParty.pas',            // to - do : rewrite Party Manager as Module, reomplent ability to offer party Mody by Plugin

{$IFDEF FPC}
  ulazjpeg               in 'Classes\Ulazjpeg.pas',
{$ENDIF}


  //------------------------------
  //Includes - Media support classes....
  //           Make sure UMedia always first, then UMedia_dummy
  //------------------------------
  UMusic          in 'Classes\UMusic.pas',
  UMedia_dummy    in 'Classes\UMedia_dummy.pas',
  UVideo          in 'Classes\UVideo.pas',
{$ifdef linux}
  UAudio_FFMpeg   in 'Classes\UAudio_FFMpeg.pas',
{$endif}
{$ifdef win32}
  UAudio_bass     in 'Classes\UAudio_bass.pas',
{$endif}

  //------------------------------
  //Includes - Screens
  //------------------------------  
  UScreenLoading          in 'Screens\UScreenLoading.pas',
  UScreenWelcome          in 'Screens\UScreenWelcome.pas',
  UScreenMain             in 'Screens\UScreenMain.pas',
  UScreenName             in 'Screens\UScreenName.pas',
  UScreenLevel            in 'Screens\UScreenLevel.pas',
  UScreenSong             in 'Screens\UScreenSong.pas',
  UScreenSing             in 'Screens\UScreenSing.pas',
  UScreenScore            in 'Screens\UScreenScore.pas',
  UScreenOptions          in 'Screens\UScreenOptions.pas',
  UScreenOptionsGame      in 'Screens\UScreenOptionsGame.pas',
  UScreenOptionsGraphics  in 'Screens\UScreenOptionsGraphics.pas',
  UScreenOptionsSound     in 'Screens\UScreenOptionsSound.pas',
  UScreenOptionsLyrics    in 'Screens\UScreenOptionsLyrics.pas',
  UScreenOptionsThemes    in 'Screens\UScreenOptionsThemes.pas',
  UScreenOptionsRecord    in 'Screens\UScreenOptionsRecord.pas',
  UScreenOptionsAdvanced  in 'Screens\UScreenOptionsAdvanced.pas',
  UScreenEditSub          in 'Screens\UScreenEditSub.pas',
  UScreenEdit             in 'Screens\UScreenEdit.pas',
  UScreenEditConvert      in 'Screens\UScreenEditConvert.pas',
  UScreenEditHeader       in 'Screens\UScreenEditHeader.pas',
  UScreenOpen             in 'Screens\UScreenOpen.pas',
  UScreenTop5             in 'Screens\UScreenTop5.pas',
  UScreenSongMenu         in 'Screens\UScreenSongMenu.pas',
  UScreenSongJumpto       in 'Screens\UScreenSongJumpto.pas',
  UScreenStatMain         in 'Screens\UScreenStatMain.pas',
  UScreenStatDetail       in 'Screens\UScreenStatDetail.pas',
  UScreenCredits          in 'Screens\UScreenCredits.pas',
  UScreenPopup            in 'Screens\UScreenPopup.pas',

  //Includes - Screens PartyMode
  UScreenSingModi         in 'Screens\UScreenSingModi.pas',
  UScreenPartyNewRound    in 'Screens\UScreenPartyNewRound.pas',
  UScreenPartyScore       in 'Screens\UScreenPartyScore.pas',
  UScreenPartyPlayer      in 'Screens\UScreenPartyPlayer.pas',
  UScreenPartyOptions     in 'Screens\UScreenPartyOptions.pas',
  UScreenPartyWin         in 'Screens\UScreenPartyWin.pas',


  //------------------------------ 	 
  //Includes - Modi SDK
  //------------------------------
  ModiSDK       in '..\..\Modis\SDK\ModiSDK.pas', //Old SDK, will be deleted soon
  UPluginDefs   in '..\..\Modis\SDK\UPluginDefs.pas', //New SDK, not only Modis
  UPartyDefs    in '..\..\Modis\SDK\UPartyDefs.pas', //Headers to register Party Modes

  Windows,
  SysUtils;

const
  Version = 'UltraStar Deluxe V 1.10 Alpha Build';  

{$IFNDEF FPC}
begin
  Main;
end.
{$ENDIF}