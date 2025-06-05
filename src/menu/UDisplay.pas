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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/menu/UDisplay.pas $
 * $Id: UDisplay.pas 3150 2015-10-20 00:07:57Z basisbit $
 *}

unit UDisplay;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UCommon,
  Math,
  sdl2,
  dglOpenGL,
  SysUtils,
  UMenu,
  UPath,
  UMusic,
  UHookableEvent;

type
  TDisplay = class
    private
      ePreDraw: THookableEvent;
      eDraw: THookableEvent;

      // fade-to-black
      BlackScreen:   boolean;

      FadeEnabled:   boolean;  // true if fading is enabled
      FadeFailed:    boolean;  // true if fading is possible (enough memory, etc.)
      FadeStartTime: cardinal; // time when fading starts, 0 means that the fade texture must be initialized
      DoneOnShow:    boolean;  // true if passed onShow after fading

      FadeTex:       array[0..1] of GLuint;
      TexW, TexH:    Cardinal;
 
      FPSCounter:    cardinal;
      NextFPSSwap:   cardinal;

      OSD_LastError: string;

      { software cursor data }
      Cursor_X:              double;
      Cursor_Y:              double;
      Cursor_Pressed:        boolean;

      // used for cursor fade out when there is no movement
      Cursor_Visible:      boolean;
      Cursor_LastMove:     cardinal;
      Cursor_Fade:         boolean;

      Cursor_Update:       boolean;

      Console_Draw:         boolean;
      Console_ScrollOffset: integer;
      procedure DrawDebugInformation;
      procedure DrawDebugConsole;

      { Handles parsing of inputs when console is opened. Called from ParseInput }
      function ConsoleParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown : boolean): boolean;
       { Handles parsing of inputs when console is opened. Called from ParseMouse }
      function ConsoleParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;

      { called by MoveCursor and OnMouseButton to update last move and start fade in }
      procedure UpdateCursorFade;

    public
      Cursor_HiddenByScreen: boolean; // hides software cursor and deactivate auto fade in, must be public for access in UMenuButton

      NextScreen:          PMenu;
      CurrentScreen:       PMenu;

      // popup data
      NextScreenWithCheck: Pmenu;
      CheckOK:             boolean;

      // FIXME: Fade is set to 0 in UMain and other files but not used here anymore.
      Fade:                real;

      constructor Create;
      destructor  Destroy; override;

      procedure InitFadeTextures();

      procedure ToggleConsole;
      
      procedure SaveScreenShot;

      procedure DrawDelay(Delay: Integer);

      function  Draw: boolean;

      // TODO rewrite ParseInput to include handling/suppressing input as return, use KeepGoing as by-reference
      { checks if display is handling the passed key in ParseInput. calls HandleInput of cur or next Screen if assigned }
      function ShouldHandleInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown : boolean; out SuppressKey: boolean): boolean;

      { calls ParseInput of cur or next Screen if assigned }
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown : boolean): boolean;

      { calls ParseMouse of cur or next Screen if assigned }
      function ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;

      { sets SDL_ShowCursor depending on options set in Ini }
      procedure SetCursor;

      { called when cursor moves, positioning of software cursor }
      procedure MoveCursor(X, Y: double);

      { called when left or right mousebutton is pressed or released }
      procedure OnMouseButton(Pressed: boolean);
      { fades to specific screen (playing specified sound) }
      function FadeTo(Screen: PMenu; const aSound: TAudioPlaybackStream = nil): PMenu;

      { abort fading to the current screen, may be used in OnShow, or during fade process }
      procedure AbortScreenChange;

      { draws software cursor }
      procedure DrawCursor;

      { forces to update cursor (position and set visibility) }
      procedure UpdateCursor;

      { returns whether this display is requesting an cursor update }
      function NeedsCursorUpdate(): boolean;

      { called when the window has been resized }
      procedure OnWindowResized(); virtual;

  end;

var
  Display: TDisplay;
  SupportsNPOT: Boolean;
  LastFPS:       cardinal;

const
  { constants for screen transition
    time in milliseconds }
  FADE_DURATION = 400;
  { constants for software cursor effects
    time in milliseconds }
  CURSOR_FADE_IN_TIME = 500;      // seconds the fade in effect lasts
  CURSOR_FADE_OUT_TIME = 2000;    // seconds the fade out effect lasts
  CURSOR_AUTOHIDE_TIME = 5000;   // seconds until auto fade out starts if there is no mouse movement

implementation

uses
  TextGL,
  StrUtils,
  UCommandLine,
  UGraphic,
  UIni,
  UImage,
  ULog,
  UMain,
  UTexture,
  UTime,
  ULanguage,
  UPathUtils;

constructor TDisplay.Create;
begin
  inherited Create;

  // create events for plugins
  ePreDraw := THookableEvent.Create('Display.PreDraw');
  eDraw := THookableEvent.Create('Display.Draw');

  // init popup
  CheckOK             := false;
  NextScreen          := nil;
  NextScreenWithCheck := nil;
  BlackScreen         := false;

  // init fade
  FadeStartTime := 0;
  FadeEnabled := (Ini.ScreenFade = 1);
  FadeFailed  := false;
  DoneOnShow  := false;

  glGenTextures(2, PGLuint(@FadeTex));
  SupportsNPOT := (AnsiContainsStr(glGetString(GL_EXTENSIONS),'texture_non_power_of_two')) and not (AnsiContainsStr(glGetString(GL_EXTENSIONS), 'Radeon X16'));
  InitFadeTextures();

  // set LastError for OSD to No Error
  OSD_LastError := 'No Errors';

  // software cursor default values
  Cursor_LastMove := 0;
  Cursor_Visible  := false;
  Cursor_Pressed  := false;
  Cursor_X        := -1;
  Cursor_Y        := -1;
  Cursor_Fade     := false;
  Cursor_HiddenByScreen := true;
  Cursor_Update   := false;
end;

destructor TDisplay.Destroy;
begin
  glDeleteTextures(2, @FadeTex);
  inherited Destroy;
end;

procedure TDisplay.InitFadeTextures();
var
  i: integer;
begin
  if (SupportsNPOT = false) then
  begin
  TexW := Round(Power(2, Ceil(Log2(ScreenW div Screens))));
  TexH := Round(Power(2, Ceil(Log2(ScreenH))));
  end
  else
  begin
    TexW := ScreenW div Screens;
    TexH := ScreenH;
  end;
  for i := 0 to 1 do
  begin
    glBindTexture(GL_TEXTURE_2D, FadeTex[i]);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, 3, TexW, TexH, 0, GL_RGB, GL_UNSIGNED_BYTE, nil);
  end;
end;

function TDisplay.Draw: boolean;
var
  S:               integer;
  FadeStateSquare: real;
  FadeW, FadeH:    real;
  FadeCopyW, FadeCopyH: integer;
  glError:         glEnum;

begin
  Result := true;

  for S := 1 to Screens do
  begin
    ScreenAct := S;
    glViewPort((S-1) * ScreenW div Screens, 0, ScreenW div Screens, ScreenH);

    // popup check was successful... move on
    if CheckOK then
    begin
      if assigned(NextScreenWithCheck) then
      begin
        NextScreen := NextScreenWithCheck;
        NextScreenWithCheck := nil;
        CheckOk := false;
      end
      else
      begin
        // on end of game fade to black before exit
        BlackScreen := true;
      end;
    end;

    if (not assigned(NextScreen)) and (not BlackScreen) then
    begin
      ePreDraw.CallHookChain(false);
      CurrentScreen.Draw;

      // popup
      if (ScreenPopupError <> nil) and ScreenPopupError.Visible then
        ScreenPopupError.Draw
      else if (ScreenPopupInfo <> nil) and ScreenPopupInfo.Visible then
        ScreenPopupInfo.Draw
      else if (ScreenPopupCheck <> nil) and ScreenPopupCheck.Visible then
        ScreenPopupCheck.Draw
      else if (ScreenPopupInsertUser <> nil) and ScreenPopupInsertUser.Visible then
        ScreenPopupInsertUser.Draw
      else if (ScreenPopupSendScore <> nil) and ScreenPopupSendScore.Visible then
        ScreenPopupSendScore.Draw
      else if (ScreenPopupScoreDownload <> nil) and ScreenPopupScoreDownload.Visible then
        ScreenPopupScoreDownload.Draw
      else if (ScreenPopupHelp <> nil) and ScreenPopupHelp.Visible then
        ScreenPopupHelp.Draw;

      // fade
      FadeStartTime := 0;
      if ((Ini.ScreenFade = 1) and (not FadeFailed)) then
        FadeEnabled := true
      else
        FadeEnabled := false;

      eDraw.CallHookChain(false);
    end
    else
    begin
      // disable fading if initialization failed
      if (FadeEnabled and FadeFailed) then
      begin
        FadeEnabled := false;
      end;
      
      if (FadeEnabled and not FadeFailed) then
      begin
        // create fading texture if we're just starting
        if FadeStartTime = 0 then
        begin
          // draw screen that will be faded
          ePreDraw.CallHookChain(false);
          CurrentScreen.Draw;
          eDraw.CallHookChain(false);

          // clear OpenGL errors, otherwise fading might be disabled due to some
          // older errors in previous OpenGL calls.
          glGetError();

          FadeCopyW := ScreenW div Screens;
          FadeCopyH := ScreenH;

          // it is possible that our fade textures are too small after a window
          // resize. In that case resize the fade texture to fit the requirements.
          if (TexW < FadeCopyW) or (TexH < FadeCopyH) then
            InitFadeTextures();

          // copy screen to texture
          glBindTexture(GL_TEXTURE_2D, FadeTex[S-1]);
          glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, (S-1) * ScreenW div Screens, 0,
            FadeCopyW, FadeCopyH);

          glError := glGetError();
          if (glError <> GL_NO_ERROR) then
          begin
            FadeFailed := true;
            Log.LogError('Fading disabled: $' + IntToHex(glError, 4), 'TDisplay.Draw');
          end;

          if not BlackScreen and (S = 1) and not DoneOnShow then
          begin
            NextScreen.OnShow;
            DoneOnShow := true;
          end;


          // set fade time once on second screen (or first if screens = 1)
          if (Screens = 1) or (S = 2) then
            FadeStartTime := SDL_GetTicks;
        end; // end texture creation in first fading step

        if not BlackScreen then
        begin
          ePreDraw.CallHookChain(false);
          NextScreen.Draw; // draw next screen
          eDraw.CallHookChain(false);
        end
        else if ScreenAct = 1 then
        begin
          // draw black screen
          glClearColor(0, 0, 0, 1);
          glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
        end;

        // and draw old screen over it... slowly fading out
        if (FadeStartTime = 0) then
          FadeStateSquare := 0 // for first screen if screens = 2
        else
          FadeStateSquare := sqr((SDL_GetTicks - FadeStartTime) / FADE_DURATION);

        if (FadeStateSquare < 1) then
        begin
          FadeW := (ScreenW div Screens)/TexW;
          FadeH := ScreenH/TexH;

          glBindTexture(GL_TEXTURE_2D, FadeTex[S-1]);
          // TODO: check if glTexEnvi() gives any speed improvement
          //glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
          glColor4f(1, 1, 1, 1-FadeStateSquare);

          glEnable(GL_TEXTURE_2D);
          glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
          glEnable(GL_BLEND);
          glBegin(GL_QUADS);
            glTexCoord2f((0+FadeStateSquare/2)*FadeW, (0+FadeStateSquare/2)*FadeH);
            glVertex2f(0,   RenderH);

            glTexCoord2f((0+FadeStateSquare/2)*FadeW, (1-FadeStateSquare/2)*FadeH);
            glVertex2f(0,   0);

            glTexCoord2f((1-FadeStateSquare/2)*FadeW, (1-FadeStateSquare/2)*FadeH);
            glVertex2f(RenderW, 0);

            glTexCoord2f((1-FadeStateSquare/2)*FadeW, (0+FadeStateSquare/2)*FadeH);
            glVertex2f(RenderW, RenderH);
          glEnd;
          glDisable(GL_BLEND);
          glDisable(GL_TEXTURE_2D);

          // reset to default
          //glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
        end;
      end

      // there is no need to init next screen if it is a black screen
      else if not BlackScreen then
      begin
        NextScreen.OnShow;
      end;

      if ((FadeStartTime + FADE_DURATION < SDL_GetTicks) or
          (not FadeEnabled) or FadeFailed) and
         ((Screens = 1) or (S = 2)) then
      begin
        // fade out complete...
        FadeStartTime := 0;
        DoneOnShow := false;
        CurrentScreen.onHide;
        CurrentScreen.ShowFinish := false;
        CurrentScreen := NextScreen;
        NextScreen := nil;
        if not BlackScreen then
        begin
          CurrentScreen.OnShowFinish;
          CurrentScreen.ShowFinish := true;
          Cursor_Update := true;
        end
        else
        begin
          Result := false;
          Break;
        end;
      end;
    end; // if

    // Draw OSD only on first Screen if Debug Mode is enabled
    if ((Ini.Debug = 1) or (Params.Debug)) and (S = 1) then
    begin
      DrawDebugInformation;
      if Console_Draw then DrawDebugConsole;
    end else if Console_Draw then
    begin
      // clear flag to prevent drawing console when toggling debug
      // TODO: considers using event in ScreenOptions for clearing debug
      Console_Draw := false;
    end;

    if not BlackScreen then
      DrawCursor;
  end; // for
end;

{ sets SDL_ShowCursor depending on options set in Ini }
procedure TDisplay.SetCursor;
var
  Cursor: Integer;
begin
  Cursor := 0;

  if (CurrentScreen <> @ScreenSing) or (Cursor_HiddenByScreen) then
  begin // hide cursor on singscreen
    if (Ini.Mouse = 0) and (Ini.FullScreen = 0) then
      // show sdl (os) cursor in window mode even when mouse support is off
      Cursor := 1
    else if (Ini.Mouse = 1) then
      // show sdl (os) cursor when hardware cursor is selected
      Cursor := 1;

    if (Ini.Mouse <> 2) then
      Cursor_HiddenByScreen := false;
  end
  else if (Ini.Mouse <> 2) then
    Cursor_HiddenByScreen := true;


  SDL_ShowCursor(Cursor);

  if (Ini.Mouse = 2) then
  begin
    if Cursor_HiddenByScreen then
    begin
      // show software cursor
      Cursor_HiddenByScreen := false;
      Cursor_Visible := false;
      Cursor_Fade := false;
    end
    else if (CurrentScreen = @ScreenSing) then
    begin
      // hide software cursor in singscreen
      Cursor_HiddenByScreen := true;
      Cursor_Visible := false;
      Cursor_Fade := false;
    end;
  end;
end;

{ called by MoveCursor and OnMouseButton to update last move and start fade in }
procedure TDisplay.UpdateCursorFade;
var
  Ticks: cardinal;
begin
  Ticks := SDL_GetTicks;

  { fade in on movement (or button press) if not first movement }
  if (not Cursor_Visible) and (Cursor_LastMove <> 0) then
  begin
    if Cursor_Fade then // we use a trick here to consider progress of fade out
      Cursor_LastMove := Ticks - round(CURSOR_FADE_IN_TIME * (1 - (Ticks - Cursor_LastMove)/CURSOR_FADE_OUT_TIME))
    else
      Cursor_LastMove := Ticks;

    Cursor_Visible := true;
    Cursor_Fade := true;
  end
  else if not Cursor_Fade then
  begin
    Cursor_LastMove := Ticks;
  end;
end;

{ called when cursor moves, positioning of software cursor }
procedure TDisplay.MoveCursor(X, Y: double);
begin
  if (Ini.Mouse = 2) and
     ((X <> Cursor_X) or (Y <> Cursor_Y)) then
  begin
    Cursor_X := X;
    Cursor_Y := Y;

    UpdateCursorFade;
  end;
end;

{ called when left or right mousebutton is pressed or released }
procedure TDisplay.OnMouseButton(Pressed: boolean);
begin
  if (Ini.Mouse = 2) then
  begin
    Cursor_Pressed := Pressed;

    UpdateCursorFade;
  end;
end;

{ draws software cursor }
procedure TDisplay.DrawCursor;
var
  Alpha: single;
  Ticks: cardinal;
  DrawX: double;
begin
  if (Ini.Mouse = 2) and ((Screens = 1) or ((ScreenAct - 1) = (Round(Cursor_X+16) div RenderW))) then
  begin // draw software cursor
    Ticks := SDL_GetTicks;

    if (Cursor_Visible) and (Cursor_LastMove + CURSOR_AUTOHIDE_TIME <= Ticks) then
    begin // start fade out after 5 secs w/o activity
      Cursor_Visible := false;
      Cursor_LastMove := Ticks;
      Cursor_Fade := true;
    end;

    // fading
    if Cursor_Fade then
    begin
      if Cursor_Visible then
      begin // fade in
        if (Cursor_LastMove + CURSOR_FADE_IN_TIME <= Ticks) then
          Cursor_Fade := false
        else
          Alpha := sin((Ticks - Cursor_LastMove) * 0.5 * pi / CURSOR_FADE_IN_TIME) * 0.7;
      end
      else
      begin //fade out
        if (Cursor_LastMove + CURSOR_FADE_OUT_TIME <= Ticks) then
          Cursor_Fade := false
        else
          Alpha := cos((Ticks - Cursor_LastMove) * 0.5 * pi / CURSOR_FADE_OUT_TIME) * 0.7;
      end;
    end;

    // no else if here because we may turn off fade in if block
    if not Cursor_Fade then
    begin
      if Cursor_Visible then
        Alpha := 0.7 // alpha when cursor visible and not fading
      else
        Alpha := 0;  // alpha when cursor is hidden
    end;

    if (Alpha > 0) and (not Cursor_HiddenByScreen) then
    begin
      DrawX := Cursor_X;
      if (ScreenAct = 2) then
        DrawX := DrawX - RenderW;
      glColor4f(1, 1, 1, Alpha);
      glEnable(GL_TEXTURE_2D);
      glEnable(GL_BLEND);
      glDisable(GL_DEPTH_TEST);

      if (Cursor_Pressed) and (Tex_Cursor_Pressed.TexNum > 0) then
        glBindTexture(GL_TEXTURE_2D, Tex_Cursor_Pressed.TexNum)
      else
        glBindTexture(GL_TEXTURE_2D, Tex_Cursor_Unpressed.TexNum);

      glBegin(GL_QUADS);
        glTexCoord2f(0, 0);
        glVertex2f(DrawX, Cursor_Y);

        glTexCoord2f(0, 1);
        glVertex2f(DrawX, Cursor_Y + 32);

        glTexCoord2f(1, 1);
        glVertex2f(DrawX + 32, Cursor_Y + 32);

        glTexCoord2f(1, 0);
        glVertex2f(DrawX + 32, Cursor_Y);
      glEnd;

      glDisable(GL_BLEND);
      glDisable(GL_TEXTURE_2D);
    end;
  end;
end;

function TDisplay.ShouldHandleInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown : boolean; out SuppressKey: boolean): boolean;
begin
  if Console_Draw then
  begin
    Result := true;
    SuppressKey := true;
    Exit;
  end;

  if (assigned(NextScreen)) then
    Result := NextScreen^.ShouldHandleInput(PressedKey, CharCode, PressedDown, SuppressKey)
  else if (assigned(CurrentScreen)) then
    Result := CurrentScreen^.ShouldHandleInput(PressedKey, CharCode, PressedDown, SuppressKey)
  else
    Result := True;
end;

function TDisplay.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown : boolean): boolean;
begin
  if Console_Draw and ConsoleParseInput(PressedKey, CharCode, PressedDown) then Exit;

  if (assigned(NextScreen)) then
    Result := NextScreen^.ParseInput(PressedKey, CharCode, PressedDown)
  else if (assigned(CurrentScreen)) then
    Result := CurrentScreen^.ParseInput(PressedKey, CharCode, PressedDown)
  else
    Result := True;
end;

function TDisplay.ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
begin
  if Console_Draw and ConsoleParseMouse(MouseButton, BtnDown, X, Y) then Exit;

  if (assigned(NextScreen)) then
    Result := NextScreen^.ParseMouse(MouseButton, BtnDown, X, Y)
  else
  if (assigned(CurrentScreen)) then
    Result := CurrentScreen^.ParseMouse(MouseButton, BtnDown, X, Y)
  else
    Result := True;
end;

{ abort fading to the next screen, may be used in OnShow, or during fade process }
procedure TDisplay.AbortScreenChange;
  var
    Temp: PMenu;
begin
  // this is some kind of "hack" it is based on the
  // code that is used to change the screens in TDisplay.Draw
  // we should rewrite this whole behaviour, as it is not well
  // structured and not well extendable. Also we should offer
  // a possibility to change screens to plugins
  // change this code when restructuring is done
  if (assigned(NextScreen)) then
  begin
    // we have to swap the screens
    Temp := CurrentScreen;
    CurrentScreen := NextScreen;
    NextScreen := Temp;

    // and call the OnShow procedure of the previous screen
    // because it was already called by default fade procedure
    NextScreen.OnShow;
    
  end;
end;

{ fades to specific screen (playing specified sound)
  returns old screen }
function TDisplay.FadeTo(Screen: PMenu; const aSound: TAudioPlaybackStream = nil): PMenu;
begin
  Result := CurrentScreen;
  if (Result <> nil) then
  begin
    if (aSound <> nil) then
      Result.FadeTo(Screen, aSound)
    else
      Result.FadeTo(Screen);
  end;
end;

procedure TDisplay.UpdateCursor;
begin
  UpdateCursorFade;
  Cursor_Update := true;
end;

function TDisplay.NeedsCursorUpdate: boolean;
begin
  Result := Cursor_Update and Cursor_Visible and not Cursor_Fade;
  Cursor_Update := false;
end;

procedure TDisplay.OnWindowResized();
begin
  // update cursor position once the window has been resized, otherwise the cursor jumps
  Cursor_Update := true;

  if (assigned(NextScreen)) then NextScreen^.OnWindowResized()
  else if (assigned(CurrentScreen)) then CurrentScreen^.OnWindowResized()

end;

procedure TDisplay.SaveScreenShot;
var
  Num:        integer;
  FileName:   IPath;
  Prefix:     UTF8String;
  ScreenData: PChar;
  Surface:    PSDL_Surface;
  Success:    boolean;
  Align:      integer;
  RowSize:    integer;
begin
  // Exit if Screenshot-path does not exist or read-only
  if (ScreenshotsPath.IsUnset) then
    Exit;

  for Num := 1 to 9999 do
  begin
    // fill prefix to 4 digits with leading '0', e.g. '0001'
    Prefix := Format('screenshot%.4d', [Num]);
    FileName := ScreenshotsPath.Append(Prefix + '.jpg');
    if not FileName.Exists() then
      break;
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

   Success := WriteJPGImage(FileName, Surface, 95);
  //  Success := WriteBMPImage(FileName, Surface);
  //Success := WritePNGImage(FileName, Surface);
  if Success then
    ScreenPopupInfo.ShowPopup(Format(Language.Translate('SCREENSHOT_SAVED'), [FileName.GetName.ToUTF8()]))
  else
    ScreenPopupError.ShowPopup(Language.Translate('SCREENSHOT_FAILED'));

  SDL_FreeSurface(Surface);
  FreeMem(ScreenData);
end;

//------------
// DrawDebugInformation - procedure draw fps and some other informations on screen
//------------
procedure TDisplay.DrawDebugInformation;
var
  Ticks: cardinal;
begin
  // Some White Background for information
  glEnable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glColor4f(1, 1, 1, 0.5);
  glBegin(GL_QUADS);
    glVertex2f(690, 35);
    glVertex2f(690, 0);
    glVertex2f(800, 0);
    glVertex2f(800, 35);
  glEnd;
  glDisable(GL_BLEND);

  // set font specs
  SetFontFamily(0);
  SetFontStyle(ftRegular);
  SetFontSize(21);
  SetFontItalic(false);
  glColor4f(0, 0, 0, 1);

  // calculate fps
  Ticks := SDL_GetTicks();
  if (Ticks >= NextFPSSwap) then
  begin
    LastFPS := FPSCounter * 4;
    FPSCounter := 0;
    NextFPSSwap := Ticks + 250;
  end;

  Inc(FPSCounter);

  // draw text

  // fps
  SetFontPos(695, 0);
  glPrint ('FPS: ' + InttoStr(LastFPS));

  // muffins
  SetFontPos(695, 13);
  glColor4f(0.8, 0.5, 0.2, 1);
  glPrint ('Muffins!');

  glColor4f(1, 1, 1, 1);
end;

procedure TDisplay.ToggleConsole;
begin
  Console_Draw := not Console_Draw;
  Console_ScrollOffset := 0;
end;

function TDisplay.ConsoleParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown : boolean): boolean;
var
  SDL_ModState:  word;
begin
  Result := false;

  if (PressedDown) then
  begin // Key Down
    Result := true;
    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

    case PressedKey of
      SDLK_PAGEUP: Console_ScrollOffset := Min(Console_ScrollOffset+3, Log.ConsoleCount-1);
      SDLK_PAGEDOWN: Console_ScrollOffset := Max(Console_ScrollOffset-3, 0);
      SDLK_HOME: Console_ScrollOffset := Log.ConsoleCount-1;
      SDLK_END: Console_ScrollOffset := 0;
      SDLK_DELETE: if SDL_ModState and KMOD_CTRL <> 0 then Log.ClearConsoleLog;
      otherwise Result := false;
    end;
  end;
end;

function TDisplay.ConsoleParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
begin
  Result := false;

  if (BtnDown) then
  begin // button Down
    Result := true;
    case MouseButton of
      SDL_BUTTON_WHEELUP: Console_ScrollOffset := Min(Console_ScrollOffset+3, Log.ConsoleCount-1);
      SDL_BUTTON_WHEELDOWN: Console_ScrollOffset := Max(Console_ScrollOffset-3, 0);
      otherwise Result := false;
    end;
  end;
end;

procedure TDisplay.DrawDelay(Delay: Integer);
begin
  // set font specs using UDisplay methods
  SetFontFamily(0);
  SetFontStyle(ftRegular);
  SetFontSize(21);
  SetFontItalic(false);
  glColor4f(0, 0, 0, 1);
  SetFontPos(605, 0);
  glPrint('Delay measured: ' + IntToStr(Delay) + ' ms');
  glColor4f(1, 1, 1, 1);
end;

//------------
// DrawDebugInformation - procedure draw fps and some other informations on screen
//------------
procedure TDisplay.DrawDebugConsole;
var
  I, LineCount: integer;
  YOffset, ScaleF, FontSize: real;
  PosX, PosY: real;
  W, H: real;
  ScrollPad, ScrollW: real;
  OldStretch: real;
begin
  FontSize := 16.0;
  W := 800.0;
  H := 400.0;
  ScrollPad := 5.0;
  ScrollW := 10.0;

  // Some black background
  glEnable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glColor4f(0, 0, 0, 0.85);
  glBegin(GL_QUADS);
    glVertex2f(0, 0);
    glVertex2f(0, H);
    glVertex2f(W, H);
    glVertex2f(W, 0);
  glEnd;
  glDisable(GL_BLEND);

  // scale sizes to DPI/aspect
  ScaleF := (1.0*ScreenH)/(1.0*ScreenW);
  FontSize := FontSize * 600.0/(1.0*ScreenH);
  ScrollW := ScrollW * 800.0/(1.0*ScreenW);
  ScrollPad := ScrollPad * ScaleF;

  // set font specs
  SetFontFamily(0);
  SetFontStyle(ftRegular);
  SetFontSize(FontSize);
  SetFontItalic(false);
  SetFontReflection(false, 0);
  glColor4f(1, 1, 1, 1);

  OldStretch := Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.Stretch;
  Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.Stretch := 1.4*ScaleF * Min(1.3, Max(0.8, power((1.0*ScreenW)/800.0, 1.2)));

  // don't draw anything else if nothing's logged
  if Log.ConsoleCount < 1 then Exit;


  // draw log buffer
  YOffset := H; // start at bottom
  LineCount := 0;
  I := Log.ConsoleCount-1 - Console_ScrollOffset;
  while (I >= 0) and (YOffset > 0) do
  begin
    YOffset := YOffset - FontSize;
    SetFontPos(5, YOffset);
    glPrint(Log.GetConsole(i));

    Dec(i);
    Inc(LineCount);
  end;

  // draw scoll bar
  glEnable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glColor4f(0.33, 0.33, 0.33, 1);
  glBegin(GL_QUADS);
    glVertex2f(W-ScrollPad-ScrollW, ScrollPad); // top left
    glVertex2f(W-ScrollPad, ScrollPad); // top right
    glVertex2f(W-ScrollPad, H-ScrollPad); // bottom right
    glVertex2f(W-ScrollPad-ScrollW, H-ScrollPad); // bottom left
  glEnd;

  // visible height bar + offset
  YOffset := H * ((1.0*LineCount)/(1.0*Log.ConsoleCount));
  PosY := 0;
  if I > 0 then PosY := (H-2.0*ScrollPad) * Max(0.0, I)/(1.0*Log.ConsoleCount);

  glColor4f(1, 1, 1, 1);
  glBegin(GL_QUADS);
    glVertex2f(W-ScrollPad-ScrollW, ScrollPad + PosY); // top left
    glVertex2f(W-ScrollPad, ScrollPad + PosY); // top right
    glVertex2f(W-ScrollPad, ScrollPad + PosY + YOffset); // bottom right
    glVertex2f(W-ScrollPad-ScrollW, ScrollPad + PosY + YOffset); // bottom left
  glEnd;
  glDisable(GL_BLEND);

  Fonts[CurrentFont.FontFamily][CurrentFont.FontStyle].Font.Stretch := OldStretch;
  glColor4f(1, 1, 1, 1);
end;

end.
