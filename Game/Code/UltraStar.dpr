program UltraStar;

{$IFDEF MSWINDOWS}
  {$R 'UltraStar.res' 'UltraStar.rc'}
{$ENDIF}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

// TODO: check if this is needed for MacOSX too
{$IFDEF MSWINDOWS}
  // Set global application-type (GUI/CONSOLE) switch for Windows.
  // CONSOLE is the default for FPC, GUI for Delphi, so we have
  // to specify one of the two in any case.
  {$IFDEF CONSOLE}
    {$APPTYPE CONSOLE}
  {$ELSE}
    {$APPTYPE GUI}
  {$ENDIF}
{$ENDIF}

uses
  {$IFDEF Unix}
  cthreads,            // THIS MUST be the first used unit in FPC if Threads are used!!
                       // (see http://wiki.lazarus.freepascal.org/Multithreaded_Application_Tutorial)
  {$IFNDEF DARWIN}
  cwstring,            // Enable Unicode support. MacOSX misses some references to iconv.
  {$ENDIF}
  {$ENDIF}

  //------------------------------
  //Includes - 3rd Party Libraries
  //------------------------------
  moduleloader           in 'lib\JEDI-SDL\SDL\Pas\moduleloader.pas',
  gl                     in 'lib\JEDI-SDL\OpenGL\Pas\gl.pas',
  glu                    in 'lib\JEDI-SDL\OpenGL\Pas\glu.pas',
  glext                  in 'lib\JEDI-SDL\OpenGL\Pas\glext.pas',
  sdl                    in 'lib\JEDI-SDL\SDL\Pas\sdl.pas',
  sdl_image              in 'lib\JEDI-SDL\SDL_Image\Pas\sdl_image.pas',
  sdl_ttf                in 'lib\JEDI-SDL\SDL_ttf\Pas\sdl_ttf.pas',
  sdlutils               in 'lib\JEDI-SDL\SDL\Pas\sdlutils.pas',

  zlib                   in 'lib\zlib\zlib.pas',
  png                    in 'lib\libpng\png.pas',

  {$IFDEF UseBass}
  bass                   in 'lib\bass\delphi\bass.pas',
  UAudioCore_Bass        in 'Classes\UAudioCore_Bass.pas',
  {$ENDIF}
  {$IFDEF DARWIN}
  PseudoThread           in 'MacOSX/Wrapper/PseudoThread.pas',
  {$ENDIF}
  {$IFDEF UsePortaudio}
  portaudio              in 'lib\portaudio\delphi\portaudio.pas',
  UAudioCore_Portaudio   in 'Classes\UAudioCore_Portaudio.pas',
  {$ENDIF}
  {$IFDEF UsePortmixer}
  portmixer              in 'lib\portmixer\delphi\portmixer.pas',
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  midiout       in 'lib\midi\midiout.pas',
  CIRCBUF       in 'lib\midi\CIRCBUF.PAS',
  MidiType      in 'lib\midi\MidiType.PAS',
  MidiDefs      in 'lib\midi\MidiDefs.PAS',
  MidiCons      in 'lib\midi\MidiCons.PAS',
  MidiFile      in 'lib\midi\MidiFile.PAS',
  Delphmcb      in 'lib\midi\Delphmcb.PAS',
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  DirWatch      in 'lib\other\DirWatch.pas',
  {$ENDIF}

  {$IFDEF UseFFMpeg}
  avcodec       in 'lib\ffmpeg\avcodec.pas',
  avformat      in 'lib\ffmpeg\avformat.pas',
  avutil        in 'lib\ffmpeg\avutil.pas',
  rational      in 'lib\ffmpeg\rational.pas',
  opt           in 'lib\ffmpeg\opt.pas',
  avio          in 'lib\ffmpeg\avio.pas',
  mathematics   in 'lib\ffmpeg\mathematics.pas',
  {$IFDEF UseSWScale}
  swscale       in 'lib\ffmpeg\swscale.pas',
  {$ENDIF}
  {$ENDIF}

  {$IFDEF UseProjectM}
  projectM      in 'lib\projectM\projectM.pas',
  {$ENDIF}

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
  UConfig           in 'Classes\UConfig.pas',
  
  UCommon           in 'Classes\UCommon.pas',
  UGraphic          in 'Classes\UGraphic.pas',
  UTexture          in 'Classes\UTexture.pas',
  ULanguage         in 'Classes\ULanguage.pas',
  UMain             in 'Classes\UMain.pas',
  UDraw             in 'Classes\UDraw.pas',
  URecord           in 'Classes\URecord.pas',
  UTime             in 'Classes\UTime.pas',
  TextGL            in 'Classes\TextGL.pas',
  USong             in 'Classes\USong.pas',
  UXMLSong          in 'Classes\UXMLSong.pas',
  USongs            in 'Classes\USongs.pas',
  UIni              in 'Classes\UIni.pas',
  UImage            in 'Classes\UImage.pas',
  ULyrics           in 'Classes\ULyrics.pas',
  ULyrics_bak       in 'Classes\ULyrics_bak.pas',
  USkins            in 'Classes\USkins.pas',
  UThemes           in 'Classes\UThemes.pas',
  ULog              in 'Classes\ULog.pas',
  UJoystick         in 'Classes\UJoystick.pas',
  //ULCD              in 'Classes\ULCD.pas',
  //ULight            in 'Classes\ULight.pas',
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
  uPluginLoader     in 'Classes\uPluginLoader.pas',     //New Plugin Loader Module

  UParty            in 'Classes\UParty.pas',            // TODO: rewrite Party Manager as Module, reomplent ability to offer party Mody by Plugin
  UPlatform         in 'Classes\UPlatform.pas',
{$IFDEF MSWINDOWS}
  UPlatformWindows  in 'Classes\UPlatformWindows.pas',
{$ENDIF}
{$IFDEF LINUX}
  UPlatformLinux    in 'Classes\UPlatformLinux.pas',
{$ENDIF}
{$IFDEF DARWIN}
  UPlatformMacOSX   in 'Classes/UPlatformMacOSX.pas',
{$ENDIF}

  //------------------------------
  //Includes - Media support classes....
  //           Make sure UMedia always first, then UMedia_dummy
  //------------------------------

  // TODO :  these all need to be renamed like UMedia_********   for consistency
  UMusic          in 'Classes\UMusic.pas',
  UAudioPlaybackBase in 'Classes\UAudioPlaybackBase.pas',
  UMedia_dummy    in 'Classes\UMedia_dummy.pas',  // Must be first UMedia Unit, all others will override available interfaces
{$IFDEF UseProjectM}
  UVisualizer     in 'Classes\UVisualizer.pas',   // MUST be before Video... so video can override...
{$ENDIF}
{$IFDEF UseFFMpegVideo}
  UVideo          in 'Classes\UVideo.pas',
{$ENDIF}
{$IFDEF UseFFMpegDecoder}
  UAudioDecoder_FFMpeg   in 'Classes\UAudioDecoder_FFMpeg.pas',  // MUST be before Playback-classes
{$ENDIF}
{$IFDEF UseBASSInput}
  UAudioInput_Bass       in 'Classes\UAudioInput_Bass.pas',
{$ENDIF}
{$IFDEF UseBASSPlayback}
  UAudioPlayback_Bass    in 'Classes\UAudioPlayback_Bass.pas',
{$ENDIF}
{$IFDEF UsePortaudioInput}
  UAudioInput_Portaudio  in 'Classes\UAudioInput_Portaudio.pas',
{$ENDIF}
{$IF Defined(UsePortaudioPlayback) or Defined(UseSDLPlayback)}
  UFFT                      in 'lib\fft\UFFT.pas',
  //samplerate                in 'lib\samplerate\samplerate.pas',
  UAudioPlayback_Softmixer  in 'Classes\UAudioPlayback_SoftMixer.pas',
{$IFEND}
{$IFDEF UsePortaudioPlayback}
  UAudioPlayback_Portaudio  in 'Classes\UAudioPlayback_Portaudio.pas',
{$ENDIF}
{$IFDEF UseSDLPlayback}
  UAudioPlayback_SDL        in 'Classes\UAudioPlayback_SDL.pas',
{$ENDIF}


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

  SysUtils;

begin
  Main;
end.

