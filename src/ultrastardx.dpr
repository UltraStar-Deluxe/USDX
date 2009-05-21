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
 * $URL$
 * $Id$
 *}

program ultrastardx;

{$IFDEF MSWINDOWS}
  {$R '..\icons\ultrastardx-icon.res' '..\icons\ultrastardx-icon.rc'}
{$ENDIF}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

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
  // cwstring crashes in FPC 2.2.2 so do not use the cwstring stuff
  {.$IFNDEF DARWIN}
  {$IFDEF NOIGNORE}
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
  sdlutils               in 'lib\JEDI-SDL\SDL\Pas\sdlutils.pas',
  UMediaCore_SDL         in 'media\UMediaCore_SDL.pas',

  zlib                   in 'lib\zlib\zlib.pas',
  png                    in 'lib\libpng\png.pas',

  {$IFDEF UseFreetype}
  freetype               in 'lib\freetype\freetype.pas',
  UFont                  in 'base\UFont.pas',
  UTextEncoding          in 'base\UTextEncoding.pas',
  {$ENDIF}

  {$IFDEF UseBass}
  bass                   in 'lib\bass\delphi\bass.pas',
  UAudioCore_Bass        in 'media\UAudioCore_Bass.pas',
  {$ENDIF}
  {$IFDEF UsePortaudio}
  portaudio              in 'lib\portaudio\portaudio.pas',
  UAudioCore_Portaudio   in 'media\UAudioCore_Portaudio.pas',
  {$ENDIF}
  {$IFDEF UsePortmixer}
  portmixer              in 'lib\portmixer\portmixer.pas',
  {$ENDIF}

  {$IFDEF UseFFmpeg}
  avcodec                in 'lib\ffmpeg\avcodec.pas',
  avformat               in 'lib\ffmpeg\avformat.pas',
  avutil                 in 'lib\ffmpeg\avutil.pas',
  rational               in 'lib\ffmpeg\rational.pas',
  opt                    in 'lib\ffmpeg\opt.pas',
  avio                   in 'lib\ffmpeg\avio.pas',
  mathematics            in 'lib\ffmpeg\mathematics.pas',
  UMediaCore_FFmpeg      in 'media\UMediaCore_FFmpeg.pas',
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
  PseudoThread  in 'macosx\PseudoThread.pas',
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
  UMenuEqualizer         in 'menu\UMenuEqualizer.pas',
  UDrawTexture           in 'menu\UDrawTexture.pas',
  UMenuButtonCollection  in 'menu\UMenuButtonCollection.pas',

  UMenuBackground        in 'menu\UMenuBackground.pas',
  UMenuBackgroundNone    in 'menu\UMenuBackgroundNone.pas',
  UMenuBackgroundColor   in 'menu\UMenuBackgroundColor.pas',
  UMenuBackgroundTexture in 'menu\UMenuBackgroundTexture.pas',
  UMenuBackgroundVideo   in 'menu\UMenuBackgroundVideo.pas',
  UMenuBackgroundFade    in 'menu\UMenuBackgroundFade.pas',

  //------------------------------
  //Includes - base
  //------------------------------
  UConfig           in 'base\UConfig.pas',
  
  UCommon           in 'base\UCommon.pas',
  UGraphic          in 'base\UGraphic.pas',
  UTexture          in 'base\UTexture.pas',
  ULanguage         in 'base\ULanguage.pas',
  UMain             in 'base\UMain.pas',
  UDraw             in 'base\UDraw.pas',
  URecord           in 'base\URecord.pas',
  UTime             in 'base\UTime.pas',
  TextGL            in 'base\TextGL.pas',
  USong             in 'base\USong.pas',
  UXMLSong          in 'base\UXMLSong.pas',
  USongs            in 'base\USongs.pas',
  UIni              in 'base\UIni.pas',
  UImage            in 'base\UImage.pas',
  ULyrics           in 'base\ULyrics.pas',
  UEditorLyrics     in 'base\UEditorLyrics.pas',
  USkins            in 'base\USkins.pas',
  UThemes           in 'base\UThemes.pas',
  ULog              in 'base\ULog.pas',
  UJoystick         in 'base\UJoystick.pas',
  UDataBase         in 'base\UDataBase.pas',
  UCovers           in 'base\UCovers.pas',
  UCatCovers        in 'base\UCatCovers.pas',
  UFiles            in 'base\UFiles.pas',
  UGraphicClasses   in 'base\UGraphicClasses.pas',
  UDLLManager       in 'base\UDLLManager.pas',
  UPlaylist         in 'base\UPlaylist.pas',
  UCommandLine      in 'base\UCommandLine.pas',
  URingBuffer       in 'base\URingBuffer.pas',
  USingScores       in 'base\USingScores.pas',
  USingNotes        in 'base\USingNotes.pas',
  UPath             in 'base\UPath.pas',
  UNote             in 'base\UNote.pas',
  UBeatTimer        in 'base\UBeatTimer.pas',

  //------------------------------
  //Includes - Plugin Support
  //------------------------------
  UParty            in 'base\UParty.pas',            // TODO: rewrite Party Manager as Module, reomplent ability to offer party Mody by Plugin

  //------------------------------
  //Includes - Platform
  //------------------------------
  
  UPlatform         in 'base\UPlatform.pas',
{$IF Defined(MSWINDOWS)}
  UPlatformWindows  in 'base\UPlatformWindows.pas',
{$ELSEIF Defined(DARWIN)}
  UPlatformMacOSX   in 'base/UPlatformMacOSX.pas',
{$ELSEIF Defined(UNIX)}
  UPlatformLinux    in 'base\UPlatformLinux.pas',
{$IFEND}

  //------------------------------
  //Includes - Media
  //------------------------------

  UMusic                    in 'base\UMusic.pas',
  UAudioPlaybackBase        in 'media\UAudioPlaybackBase.pas',
{$IF Defined(UsePortaudioPlayback) or Defined(UseSDLPlayback)}
  UFFT                      in 'lib\fft\UFFT.pas',
  UAudioPlayback_SoftMixer  in 'media\UAudioPlayback_SoftMixer.pas',
{$IFEND}
  UAudioConverter           in 'media\UAudioConverter.pas',

  //******************************
  //Pluggable media modules
  // The modules are prioritized as in the include list below.
  // This means the first entry has highest priority, the last lowest.
  //******************************

{$IFDEF UseFFmpegVideo}
  UVideo                    in 'media\UVideo.pas',
{$ENDIF}
{$IFDEF UseProjectM}
  // must be after UVideo, so it will not be the default video module
  UVisualizer               in 'media\UVisualizer.pas',
{$ENDIF}
{$IFDEF UseBASSInput}
  UAudioInput_Bass          in 'media\UAudioInput_Bass.pas',
{$ENDIF}
{$IFDEF UseBASSDecoder}
  // prefer Bass to FFmpeg if possible
  UAudioDecoder_Bass        in 'media\UAudioDecoder_Bass.pas',
{$ENDIF}
{$IFDEF UseBASSPlayback}
  UAudioPlayback_Bass       in 'media\UAudioPlayback_Bass.pas',
{$ENDIF}
{$IFDEF UseSDLPlayback}
  UAudioPlayback_SDL        in 'media\UAudioPlayback_SDL.pas',
{$ENDIF}
{$IFDEF UsePortaudioInput}
  UAudioInput_Portaudio     in 'media\UAudioInput_Portaudio.pas',
{$ENDIF}
{$IFDEF UsePortaudioPlayback}
  UAudioPlayback_Portaudio  in 'media\UAudioPlayback_Portaudio.pas',
{$ENDIF}
{$IFDEF UseFFmpegDecoder}
  UAudioDecoder_FFmpeg      in 'media\UAudioDecoder_FFmpeg.pas',
{$ENDIF}
  // fallback dummy, must be last
  UMedia_dummy              in 'media\UMedia_dummy.pas',


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

  SysUtils;

begin
  Main;
end.

