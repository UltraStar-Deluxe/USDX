program ultrastardx;

{$IFDEF MSWINDOWS}
  {$R 'ultrastardx.res' 'ultrastardx.rc'}
{$ENDIF}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

//{$DEFINE CONSOLE}

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

  {$IFNDEF FPC}
  ctypes                 in 'lib\ctypes\ctypes.pas', // FPC compatibility types for C libs
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
  //sdl_ttf                in 'lib\JEDI-SDL\SDL_ttf\Pas\sdl_ttf.pas',
  sdlutils               in 'lib\JEDI-SDL\SDL\Pas\sdlutils.pas',
  UMediaCore_SDL         in 'classes\UMediaCore_SDL.pas',

  zlib                   in 'lib\zlib\zlib.pas',
  png                    in 'lib\libpng\png.pas',

  {$IFDEF UseBass}
  bass                   in 'lib\bass\delphi\bass.pas',
  UAudioCore_Bass        in 'classes\UAudioCore_Bass.pas',
  {$ENDIF}
  {$IFDEF UsePortaudio}
  portaudio              in 'lib\portaudio\delphi\portaudio.pas',
  UAudioCore_Portaudio   in 'classes\UAudioCore_Portaudio.pas',
  {$ENDIF}
  {$IFDEF UsePortmixer}
  portmixer              in 'lib\portmixer\delphi\portmixer.pas',
  {$ENDIF}

  {$IFDEF UseFFmpeg}
  avcodec                in 'lib\ffmpeg\avcodec.pas',
  avformat               in 'lib\ffmpeg\avformat.pas',
  avutil                 in 'lib\ffmpeg\avutil.pas',
  rational               in 'lib\ffmpeg\rational.pas',
  opt                    in 'lib\ffmpeg\opt.pas',
  avio                   in 'lib\ffmpeg\avio.pas',
  mathematics            in 'lib\ffmpeg\mathematics.pas',
  UMediaCore_FFmpeg      in 'classes\UMediaCore_FFmpeg.pas',
  {$IFDEF UseSWScale}
  swscale                in 'lib\ffmpeg\swscale.pas',
  {$ENDIF}
  {$ENDIF}

  {$IFDEF UseSRCResample}
  samplerate             in 'lib\samplerate\samplerate.pas',
  {$ENDIF}

  {$IFDEF UseProjectM}
  projectM      in 'lib\projectM\projectM.pas',
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  {$IFDEF FPC}
  // FPC compatibility file for Allocate/DeallocateHWnd
  WinAllocation in 'lib\other\WinAllocation.pas',
  {$ENDIF}

  midiout       in 'lib\midi\midiout.pas',
  CIRCBUF       in 'lib\midi\CIRCBUF.PAS',
  MidiType      in 'lib\midi\MidiType.PAS',
  MidiDefs      in 'lib\midi\MidiDefs.PAS',
  MidiCons      in 'lib\midi\MidiCons.PAS',
  MidiFile      in 'lib\midi\MidiFile.PAS',
  Delphmcb      in 'lib\midi\Delphmcb.PAS',

  DirWatch      in 'lib\other\DirWatch.pas',
  {$ENDIF}

  {$IFDEF DARWIN}
  PseudoThread  in 'macosx/Wrapper/PseudoThread.pas',
  {$ENDIF}
  
  SQLiteTable3  in 'lib\SQLite\SQLiteTable3.pas',
  SQLite3       in 'lib\SQLite\SQLite3.pas',


  //------------------------------
  //Includes - Menu System
  //------------------------------
  UDisplay               in 'menu\UDisplay.pas',
  UMenu                  in 'menu\UMenu.pas',
  UMenuStatic            in 'menu\UMenuStatic.pas',
  UMenuText              in 'menu\UMenuText.pas',
  UMenuButton            in 'menu\UMenuButton.pas',
  UMenuInteract          in 'menu\UMenuInteract.pas',
  UMenuSelectSlide       in 'menu\UMenuSelectSlide.pas',
  UDrawTexture           in 'menu\UDrawTexture.pas',
  UMenuButtonCollection  in 'menu\UMenuButtonCollection.pas',

  //------------------------------
  //Includes - Classes
  //------------------------------
  UConfig           in 'classes\UConfig.pas',
  
  UCommon           in 'classes\UCommon.pas',
  UGraphic          in 'classes\UGraphic.pas',
  UTexture          in 'classes\UTexture.pas',
  ULanguage         in 'classes\ULanguage.pas',
  UMain             in 'classes\UMain.pas',
  UDraw             in 'classes\UDraw.pas',
  URecord           in 'classes\URecord.pas',
  UTime             in 'classes\UTime.pas',
  TextGL            in 'classes\TextGL.pas',
  USong             in 'classes\USong.pas',
  UXMLSong          in 'classes\UXMLSong.pas',
  USongs            in 'classes\USongs.pas',
  UIni              in 'classes\UIni.pas',
  UImage            in 'classes\UImage.pas',
  ULyrics           in 'classes\ULyrics.pas',
  UEditorLyrics     in 'classes\UEditorLyrics.pas',
  USkins            in 'classes\USkins.pas',
  UThemes           in 'classes\UThemes.pas',
  ULog              in 'classes\ULog.pas',
  UJoystick         in 'classes\UJoystick.pas',
  UDataBase         in 'classes\UDataBase.pas',
  UCovers           in 'classes\UCovers.pas',
  UCatCovers        in 'classes\UCatCovers.pas',
  UFiles            in 'classes\UFiles.pas',
  UGraphicClasses   in 'classes\UGraphicClasses.pas',
  UDLLManager       in 'classes\UDLLManager.pas',
  UPlaylist         in 'classes\UPlaylist.pas',
  UCommandLine      in 'classes\UCommandLine.pas',
  URingBuffer       in 'classes\URingBuffer.pas',
  UTextClasses      in 'classes\UTextClasses.pas',
  USingScores       in 'classes\USingScores.pas',
  USingNotes        in 'classes\USingNotes.pas',

  UModules          in 'classes\UModules.pas',          //List of Modules to Load
  UHooks            in 'classes\UHooks.pas',            //Hook Managing
  UServices         in 'classes\UServices.pas',         //Service Managing
  UCore             in 'classes\UCore.pas',             //Core, Maybe remove this
  UCoreModule       in 'classes\UCoreModule.pas',       //^
  UPluginInterface  in 'classes\UPluginInterface.pas',  //Interface offered by Core to Plugins
  uPluginLoader     in 'classes\uPluginLoader.pas',     //New Plugin Loader Module

  UParty            in 'classes\UParty.pas',            // TODO: rewrite Party Manager as Module, reomplent ability to offer party Mody by Plugin
  UPlatform         in 'classes\UPlatform.pas',
{$IFDEF MSWINDOWS}
  UPlatformWindows  in 'classes\UPlatformWindows.pas',
{$ENDIF}
{$IFDEF LINUX}
  UPlatformLinux    in 'classes\UPlatformLinux.pas',
{$ENDIF}
{$IFDEF DARWIN}
  UPlatformMacOSX   in 'classes/UPlatformMacOSX.pas',
{$ENDIF}

  //------------------------------
  //Includes - Media
  //------------------------------

  UMusic          in 'classes\UMusic.pas',
  UAudioPlaybackBase in 'classes\UAudioPlaybackBase.pas',
{$IF Defined(UsePortaudioPlayback) or Defined(UseSDLPlayback)}
  UFFT                      in 'lib\fft\UFFT.pas',
  UAudioPlayback_Softmixer  in 'classes\UAudioPlayback_SoftMixer.pas',
{$IFEND}
  UAudioConverter           in 'classes\UAudioConverter.pas',

  //******************************
  //Pluggable media modules
  // The modules are prioritized as in the include list below.
  // This means the first entry has highest priority, the last lowest.
  //******************************

  // TODO :  these all should be moved to a media folder

{$IFDEF UseFFmpegVideo}
  UVideo                    in 'classes\UVideo.pas',
{$ENDIF}
{$IFDEF UseProjectM}
  // must be after UVideo, so it will not be the default video module
  UVisualizer               in 'classes\UVisualizer.pas',
{$ENDIF}
{$IFDEF UseBASSInput}
  UAudioInput_Bass          in 'classes\UAudioInput_Bass.pas',
{$ENDIF}
{$IFDEF UseBASSDecoder}
  // prefer Bass to FFmpeg if possible
  UAudioDecoder_Bass        in 'classes\UAudioDecoder_Bass.pas',
{$ENDIF}
{$IFDEF UseBASSPlayback}
  UAudioPlayback_Bass       in 'classes\UAudioPlayback_Bass.pas',
{$ENDIF}
{$IFDEF UseSDLPlayback}
  UAudioPlayback_SDL        in 'classes\UAudioPlayback_SDL.pas',
{$ENDIF}
{$IFDEF UsePortaudioInput}
  UAudioInput_Portaudio     in 'classes\UAudioInput_Portaudio.pas',
{$ENDIF}
{$IFDEF UsePortaudioPlayback}
  UAudioPlayback_Portaudio  in 'classes\UAudioPlayback_Portaudio.pas',
{$ENDIF}
{$IFDEF UseFFmpegDecoder}
  UAudioDecoder_FFmpeg      in 'classes\UAudioDecoder_FFmpeg.pas',
{$ENDIF}
  // fallback dummy, must be last
  UMedia_dummy              in 'classes\UMedia_dummy.pas',


  //------------------------------
  //Includes - Screens
  //------------------------------  
  UScreenLoading          in 'screens\UScreenLoading.pas',
  UScreenWelcome          in 'screens\UScreenWelcome.pas',
  UScreenMain             in 'screens\UScreenMain.pas',
  UScreenName             in 'screens\UScreenName.pas',
  UScreenLevel            in 'screens\UScreenLevel.pas',
  UScreenSong             in 'screens\UScreenSong.pas',
  UScreenSing             in 'screens\UScreenSing.pas',
  UScreenScore            in 'screens\UScreenScore.pas',
  UScreenOptions          in 'screens\UScreenOptions.pas',
  UScreenOptionsGame      in 'screens\UScreenOptionsGame.pas',
  UScreenOptionsGraphics  in 'screens\UScreenOptionsGraphics.pas',
  UScreenOptionsSound     in 'screens\UScreenOptionsSound.pas',
  UScreenOptionsLyrics    in 'screens\UScreenOptionsLyrics.pas',
  UScreenOptionsThemes    in 'screens\UScreenOptionsThemes.pas',
  UScreenOptionsRecord    in 'screens\UScreenOptionsRecord.pas',
  UScreenOptionsAdvanced  in 'screens\UScreenOptionsAdvanced.pas',
  UScreenEditSub          in 'screens\UScreenEditSub.pas',
  UScreenEdit             in 'screens\UScreenEdit.pas',
  UScreenEditConvert      in 'screens\UScreenEditConvert.pas',
  UScreenEditHeader       in 'screens\UScreenEditHeader.pas',
  UScreenOpen             in 'screens\UScreenOpen.pas',
  UScreenTop5             in 'screens\UScreenTop5.pas',
  UScreenSongMenu         in 'screens\UScreenSongMenu.pas',
  UScreenSongJumpto       in 'screens\UScreenSongJumpto.pas',
  UScreenStatMain         in 'screens\UScreenStatMain.pas',
  UScreenStatDetail       in 'screens\UScreenStatDetail.pas',
  UScreenCredits          in 'screens\UScreenCredits.pas',
  UScreenPopup            in 'screens\UScreenPopup.pas',

  //Includes - Screens PartyMode
  UScreenSingModi         in 'screens\UScreenSingModi.pas',
  UScreenPartyNewRound    in 'screens\UScreenPartyNewRound.pas',
  UScreenPartyScore       in 'screens\UScreenPartyScore.pas',
  UScreenPartyPlayer      in 'screens\UScreenPartyPlayer.pas',
  UScreenPartyOptions     in 'screens\UScreenPartyOptions.pas',
  UScreenPartyWin         in 'screens\UScreenPartyWin.pas',


  //------------------------------
  //Includes - Modi SDK
  //------------------------------
  ModiSDK       in '..\plugins\SDK\ModiSDK.pas', //Old SDK, will be deleted soon
  UPluginDefs   in '..\plugins\SDK\UPluginDefs.pas', //New SDK, not only Modis
  UPartyDefs    in '..\plugins\SDK\UPartyDefs.pas', //Headers to register Party Modes

  SysUtils;

begin
  Main;
end.

