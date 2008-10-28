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

unit UDisplay;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  ucommon,
  SDL,
  UMenu,
  gl,
  glu,
  SysUtils;

type
  TDisplay = class
    private
      //fade-to-black-hack
      BlackScreen: Boolean;

      FadeEnabled:  Boolean;  // true if fading is enabled
      FadeFailed: Boolean;    // true if fading is possible (enough memory, etc.)
      FadeState: integer;     // fading state, 0 means that the fade texture must be initialized
      LastFadeTime: Cardinal; // last fade update time

      FadeTex: array[1..2] of GLuint;

      FPSCounter    : Cardinal;
      LastFPS       : Cardinal;
      NextFPSSwap   : Cardinal;

      OSD_LastError : String;

      procedure DrawDebugInformation;
    public
      NextScreen   : PMenu;
      CurrentScreen : PMenu;

      //popup data
      NextScreenWithCheck: Pmenu;
      CheckOK  : Boolean;

      // FIXME: Fade is set to 0 in UMain and other files but not used here anymore.
      Fade     : Real;

      constructor Create;
      destructor  Destroy; override;

      procedure SaveScreenShot;

      function  Draw: Boolean;
  end;

var
  Display:          TDisplay;

implementation

uses
  UImage,
  TextGL,
  ULog,
  UMain,
  UTexture,
  UIni,
  UGraphic,
  UTime,
  UCommandLine;

constructor TDisplay.Create;
var
  i: integer;
begin
  inherited Create;

  //popup hack
  CheckOK             := False;
  NextScreen          := nil;
  NextScreenWithCheck := nil;
  BlackScreen         := False;

  // fade mod
  FadeState := 0;
  FadeEnabled := (Ini.ScreenFade = 1);
  FadeFailed:= false;

  glGenTextures(2, @FadeTex);

  for i := 1 to 2 do
  begin
    glBindTexture(GL_TEXTURE_2D, FadeTex[i]);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  end;

  //Set LastError for OSD to No Error
  OSD_LastError := 'No Errors';
end;

destructor TDisplay.Destroy;
begin
  glDeleteTextures(2, @FadeTex);    
  inherited Destroy;
end;

function TDisplay.Draw: Boolean;
var
  S: integer;
  FadeStateSquare: Real;
  currentTime: Cardinal;
  glError: glEnum;
begin
  Result := True;

  //We don't need this here anymore,
  //Because the background care about cleaning the buffers
  //glClearColor(1, 1, 1 , 0);
  //glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  for S := 1 to Screens do
  begin
    ScreenAct := S;

    //if Screens = 1 then ScreenX := 0;
    //if (Screens = 2) and (S = 1) then ScreenX := -1;
    //if (Screens = 2) and (S = 2) then ScreenX := 1;
    ScreenX := 0;

    glViewPort((S-1) * ScreenW div Screens, 0, ScreenW div Screens, ScreenH);

    // popup hack
    // check was successful... move on
    if CheckOK then
    begin
      if assigned(NextScreenWithCheck) then
      begin
        NextScreen := NextScreenWithCheck;
        NextScreenWithCheck := nil;
        CheckOk := False;
      end
      else
      begin
        // on end of game fade to black before exit
        BlackScreen := True;
      end;
    end;

    if (not assigned(NextScreen)) and (not BlackScreen) then
    begin
      CurrentScreen.Draw;

      //popup mod
      if (ScreenPopupError <> nil) and ScreenPopupError.Visible then
        ScreenPopupError.Draw
      else if (ScreenPopupCheck <> nil) and ScreenPopupCheck.Visible then
        ScreenPopupCheck.Draw;

      // fade mod
      FadeState := 0;
      if ((Ini.ScreenFade = 1) and (not FadeFailed)) then
        FadeEnabled := True
      else if (Ini.ScreenFade = 0) then
        FadeEnabled := False;
    end
    else
    begin
      // disable fading if initialization failed
      if (FadeEnabled and FadeFailed) then
      begin
        FadeEnabled := False;
      end;
      
      if (FadeEnabled and not FadeFailed) then
      begin
        //Create Fading texture if we're just starting
        if FadeState = 0 then
        begin
          // save old viewport and resize to fit texture
          glPushAttrib(GL_VIEWPORT_BIT);
          glViewPort(0, 0, 512, 512);

          // draw screen that will be faded
          CurrentScreen.Draw;

          // clear OpenGL errors, otherwise fading might be disabled due to some
          // older errors in previous OpenGL calls.
          glGetError();

          // copy screen to texture
          glBindTexture(GL_TEXTURE_2D, FadeTex[S]);
          glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 0, 0, 512, 512, 0);
          glError := glGetError();
          if (glError <> GL_NO_ERROR) then
          begin
            FadeFailed := true;
            Log.LogWarn('Fading disabled: ' + gluErrorString(glError), 'TDisplay.Draw');
          end;

          // restore viewport
          glPopAttrib();

          // blackscreen-hack
          if not BlackScreen then
            NextScreen.onShow;

          // update fade state
          LastFadeTime := SDL_GetTicks();
          if (S = 2) or (Screens = 1) then
            FadeState := FadeState + 1;
        end; // end texture creation in first fading step

        //do some time-based fading
        currentTime := SDL_GetTicks();
        if (currentTime > LastFadeTime+30) and (S = 1) then
        begin
          FadeState := FadeState + 4;
          LastFadeTime := currentTime;
        end;

        // blackscreen-hack
        if not BlackScreen then
          NextScreen.Draw // draw next screen
        else if ScreenAct = 1 then
        begin
          glClearColor(0, 0, 0 , 0);
          glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
        end;

        // and draw old screen over it... slowly fading out

        FadeStateSquare := (FadeState*FadeState)/10000;

        glBindTexture(GL_TEXTURE_2D, FadeTex[S]);
        glColor4f(1, 1, 1, 1-FadeStateSquare);

        glEnable(GL_TEXTURE_2D);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glEnable(GL_BLEND);
        glBegin(GL_QUADS);
          glTexCoord2f(0+FadeStateSquare, 0+FadeStateSquare); glVertex2f(0,   600);
          glTexCoord2f(0+FadeStateSquare, 1-FadeStateSquare); glVertex2f(0,   0);
          glTexCoord2f(1-FadeStateSquare, 1-FadeStateSquare); glVertex2f(800, 0);
          glTexCoord2f(1-FadeStateSquare, 0+FadeStateSquare); glVertex2f(800, 600);
        glEnd;
        glDisable(GL_BLEND);
        glDisable(GL_TEXTURE_2D);
      end
      // blackscreen hack
      else if not BlackScreen then
      begin
        NextScreen.OnShow;
      end;

      if ((FadeState > 40) or (not FadeEnabled) or FadeFailed) and (S = 1) then
      begin
        // fade out complete...
        FadeState := 0;
        CurrentScreen.onHide;
        CurrentScreen.ShowFinish := False;
        CurrentScreen := NextScreen;
        NextScreen := nil;
        if not BlackScreen then
        begin
          CurrentScreen.onShowFinish;
          CurrentScreen.ShowFinish := true;
        end
        else
        begin
          Result := False;
          Break;
        end;
      end;
    end; // if

    //Draw OSD only on first Screen if Debug Mode is enabled
    if ((Ini.Debug = 1) or (Params.Debug)) and (S = 1) then
      DrawDebugInformation;      
  end; // for
end;

procedure TDisplay.SaveScreenShot;
var
  Num:        integer;
  FileName:   string;
  ScreenData: PChar;
  Surface:    PSDL_Surface;
  Success:    boolean;
  Align:      integer;
  RowSize:    integer;
begin
  // Exit if Screenshot-path does not exist or read-only
  if (ScreenshotsPath = '') then
    Exit;

  for Num := 1 to 9999 do
  begin
    FileName := IntToStr(Num);
    while Length(FileName) < 4 do
      FileName := '0' + FileName;
    FileName := ScreenshotsPath + 'screenshot' + FileName + '.png';
    if not FileExists(FileName) then
      break
  end;

  // we must take the row-alignment (4byte by default) into account
  glGetIntegerv(GL_PACK_ALIGNMENT, @Align);
  // calc aligned row-size
  RowSize := ((ScreenW*3 + (Align-1)) div Align) * Align;

  GetMem(ScreenData, RowSize * ScreenH);
  glReadPixels(0, 0, ScreenW, ScreenH, GL_RGB, GL_UNSIGNED_BYTE, ScreenData);
// on big endian machines (powerpc) this may need to be changed to
// Needs to be tests. KaMiSchi Sept 2008
// in this case one may have to add " glext, " to the list of used units
//  glReadPixels(0, 0, ScreenW, ScreenH, GL_BGR, GL_UNSIGNED_BYTE, ScreenData);
  Surface := SDL_CreateRGBSurfaceFrom(
      ScreenData, ScreenW, ScreenH, 24, RowSize,
      $0000FF, $00FF00, $FF0000, 0);

  //Success := WriteJPGImage(FileName, Surface, 95);
  //Success := WriteBMPImage(FileName, Surface);
  Success := WritePNGImage(FileName, Surface);
  if Success then
    ScreenPopupError.ShowPopup('Screenshot saved: ' + ExtractFileName(FileName))
  else
    ScreenPopupError.ShowPopup('Screenshot failed');

  SDL_FreeSurface(Surface);
  FreeMem(ScreenData);
end;

//------------
// DrawDebugInformation - Procedure draw FPS and some other Informations on Screen
//------------
procedure TDisplay.DrawDebugInformation;
var Ticks: Cardinal;
begin
  //Some White Background for information
  glEnable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glColor4f(1, 1, 1, 0.5);
  glBegin(GL_QUADS);
    glVertex2f(690, 44);
    glVertex2f(690, 0);
    glVertex2f(800, 0);
    glVertex2f(800, 44);
  glEnd;
  glDisable(GL_BLEND);

  //Set Font Specs
  SetFontStyle(0);
  SetFontSize(21);
  SetFontItalic(False);
  glColor4f(0, 0, 0, 1);

  //Calculate FPS
  Ticks := SDL_GetTicks();
  if (Ticks >= NextFPSSwap) then
  begin
    LastFPS := FPSCounter * 4;
    FPSCounter := 0;
    NextFPSSwap := Ticks + 250;
  end;

  Inc(FPSCounter);

  //Draw Text

  //FPS
  SetFontPos(695, 0);
  glPrint ('FPS: ' + InttoStr(LastFPS));

  //RSpeed
  SetFontPos(695, 13);
  glPrint ('RSpeed: ' + InttoStr(Round(1000 * TimeMid)));

  //LastError
  SetFontPos(695, 26);
  glColor4f(1, 0, 0, 1);
  glPrint (OSD_LastError);

  glColor4f(1, 1, 1, 1);
end;

end.
