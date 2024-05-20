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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenCredits.pas $
 * $Id: UScreenCredits.pas 2582 2010-07-18 11:11:57Z whiteshark0 $
 *}

unit UScreenCredits;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UDisplay,
  UFiles,
  UGraphicClasses,
  UMenu,
  UMusic,
  UPath,
  UTexture,
  UThemes,
  dglOpenGL,
  SDL2,
  SDL2_image,
  SysUtils;

{ beat detection constants and types }
const
  SubChannelCount = 32;
  HistoryLength = 44;
  SamplesPerChannel = (FFTSize div 2) div SubChannelCount;
  BeatEnergyModifier = 80; // modifies detected energy
                           // higher values equal a more sensitive detection

type
  TEnergyHistory = array [0..HistoryLength-1] of single;
  TSubchannelHistory = array [0..SubChannelCount-1] of TEnergyHistory;

type
  TCreditsStages=(InitialDelay, Intro, MainPart, Outro);

  TScreenCredits = class(TMenu)
    private
      CreditsPath: IPath;

      Credits_X:     real;
      Credits_Time:  cardinal;
      CTime_hold:    cardinal;

      credits_bg_tex:      TTexture;
      credits_bg_ovl:      TTexture;
      //credits_bg_logo:   TTexture;
      credits_names:       array of TTexture;
      intro_layer01:       TTexture;
      intro_layer02:       TTexture;
      intro_layer03:       TTexture;
      intro_layer04:       TTexture;
      intro_layer05:       TTexture;
      intro_layer06:       TTexture;
      intro_layer07:       TTexture;
      intro_layer08:       TTexture;
      intro_layer09:       TTexture;
      outro_bg:            TTexture;
      outro_esc:           TTexture;
      outro_exd:           TTexture;

      CurrentScrollStart, CurrentScrollEnd: integer;

      CRDTS_Stage: TCreditsStages;

      { beat detection }
      SubChannelHistory: TSubchannelHistory;

      { mouse movement easter eggs: }
      MouseMoved: boolean;
      MouseX, MouseY: double;

      { saves last x and y angle for easter egg }
      LogoAngleX, LogoAngleY: single;

      procedure LoadNameTextures;

      { draw different stages }
      procedure DrawInitialDelay;

      { Intro }
      procedure DrawIntro;
      procedure DrawLayeredLogo(Separation, Scale, AngleX, AngleY, AngleZ: single);

      { Main }
      procedure DrawMain;
      procedure DrawMainBG;
      procedure DrawFunkyText;

      procedure DrawMainFG;

      procedure DrawNames;
      procedure DoLogoBling;

      { Outro }
      procedure DrawOutro;


      { beat detection }
      procedure DetectBeat;
    protected
      { beat detection stuff
        protected cause we need this information for "on beat
        effect"}
      LastBeatTime: cardinal;
      BeatDetected: boolean;
      CTime:        cardinal;
    public
      Fadeout: boolean;
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      function ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean; override;
      procedure OnShow; override;
      procedure OnHide; override;
      function Draw: boolean; override;
   end;

const
  Funky_Text: string =
    'Grandma Deluxe v2 has arrived! Thanks to Corvus5, brian-ch, brunzelchen, canni0, k-m_schindler, whiteshark0, BasisBit and the USDX WP team for the massive work on UltraStar and USDX, Wome for the nice tune you are hearing, '+
    'all the people who put massive effort and work in new songs (do not forget UltraStar w/o songs would be nothing), ppl from '+
    'irc helping us, pouet.net, KakiArts, Sourceforge,..';

{ texture names (loaded from gameshared/resources/credits}
  CRDTS_BG_FILE           = 'credits_v5_bg.png';
  CRDTS_OVL_FILE          = 'credits_v5_overlay.png';
  INTRO_L01_FILE          = 'intro-l-01.png';
  INTRO_L02_FILE          = 'intro-l-02.png';
  INTRO_L03_FILE          = 'intro-l-03.png';
  INTRO_L04_FILE          = 'intro-l-04.png';
  INTRO_L05_FILE          = 'intro-l-05.png';
  INTRO_L06_FILE          = 'intro-l-06.png';
  INTRO_L07_FILE          = 'intro-l-07.png';
  INTRO_L08_FILE          = 'intro-l-08.png';
  INTRO_L09_FILE          = 'intro-l-09.png';
  OUTRO_BG_FILE           = 'outro-bg.png';
  OUTRO_ESC_FILE          = 'outro-esc.png';
  OUTRO_EXD_FILE          = 'outro-exit-dark.png';

{ some timings }
  Delay_Before_Start = 20;
  Intro_Flare_Start = 60;
  Intro_Zoom_End = 149;
  Intro_Stand_End = 155;
  Intro_Separation_End = 170;
  Intro_FadeToWhite_Start = 261;
  Intro_Zoomout_Start = 271;
  Main_Start = 271;
  Main_OnBeatTwinkle_Start = 280;
  Main_Names_Start = 359;
  Main_Names_End = 2833;
  Main_FadeOut_Start = 3096;
  Tune_End = 3366;

{ cosntants for developer names }

type
  TFadeEffect = procedure (const Tex: TTexture; Progress: double);
  TCRTZ_Developer = record
    Name: string;           // developer name for texture loading (names_"devel".png)
    Twinkle: boolean;       // should there be twinkles on show
    FadeIn:   TFadeEffect;  // fade in effect
    Draw:     TFadeEffect;  // effect during draw
    FadeOut:  TFadeEffect;  // fade out effect
  end;

{ effects are called with blending, texture and matrix prepared }
procedure Effect_Draw             (const Tex: TTexture; Progress: double);
procedure Effect_OnBeatJitter     (const Tex: TTexture; Progress: double);

procedure Effect_Rotate_Left_Top  (const Tex: TTexture; Progress: double);
procedure Effect_Rotate_Right_Bot (const Tex: TTexture; Progress: double);
procedure Effect_ZoomIn_Rotate    (const Tex: TTexture; Progress: double);
procedure Effect_ZoomOut_Shift    (const Tex: TTexture; Progress: double);
procedure Effect_Shift_Left       (const Tex: TTexture; Progress: double);
procedure Effect_Shift_Right_Top  (const Tex: TTexture; Progress: double);
procedure Effect_Flip_Bot         (const Tex: TTexture; Progress: double);
procedure Effect_Flip_Right_Top   (const Tex: TTexture; Progress: double);
procedure Effect_Flip_Right       (const Tex: TTexture; Progress: double);
procedure Effect_Flip_Right_Bot   (const Tex: TTexture; Progress: double);
procedure Effect_Rotate_Right_Top (const Tex: TTexture; Progress: double);
procedure Effect_Shift_Weird      (const Tex: TTexture; Progress: double);
procedure Effect_Shift_Right_Bot  (const Tex: TTexture; Progress: double);
procedure Effect_Rotate_Right_Top2(const Tex: TTexture; Progress: double);
procedure Effect_Flip_Left_Bot    (const Tex: TTexture; Progress: double);
procedure Effect_Flip_Right_Top2  (const Tex: TTexture; Progress: double);
procedure Effect_Twinkle_Down     (const Tex: TTexture; Progress: double);

const
  Developers: array[0..10] of TCRTZ_Developer = (
    (Name: 'alexanders';  Twinkle: true;  FadeIn: Effect_Rotate_Left_Top;   Draw: Effect_OnBeatJitter;  FadeOut: Effect_Rotate_Right_Bot),
    (Name: 'blindy';      Twinkle: true;  FadeIn: Effect_ZoomIn_Rotate;     Draw: Effect_OnBeatJitter;  FadeOut: Effect_ZoomOut_Shift),
    (Name: 'brunzel';     Twinkle: true;  FadeIn: Effect_Shift_Left;        Draw: Effect_Draw;          FadeOut: Effect_Shift_Right_Top),
    (Name: 'canni';       Twinkle: true;  FadeIn: Effect_Flip_Bot;          Draw: Effect_Draw;          FadeOut: Effect_Flip_Right_Top),
    (Name: 'hennymcc';    Twinkle: true;  FadeIn: Effect_Flip_Right;        Draw: Effect_OnBeatJitter;  FadeOut: Effect_Flip_Right_Bot),
    (Name: 'jaybinks';    Twinkle: true;  FadeIn: Effect_Rotate_Right_Top;  Draw: Effect_OnBeatJitter;  FadeOut: Effect_Shift_Weird),
    (Name: 'krueger';     Twinkle: true;  FadeIn: Effect_Shift_Right_Bot;   Draw: Effect_OnBeatJitter;  FadeOut: Effect_Rotate_Right_Top2),
    (Name: 'mezzox';      Twinkle: true;  FadeIn: Effect_Flip_Left_Bot;     Draw: Effect_OnBeatJitter;  FadeOut: Effect_Flip_Right_Top),
    (Name: 'mischi';      Twinkle: true;  FadeIn: Effect_Shift_Weird;       Draw: Effect_OnBeatJitter;  FadeOut: Effect_Flip_Bot),
    (Name: 'mog';         Twinkle: false; FadeIn: Effect_Twinkle_Down;      Draw: Effect_OnBeatJitter;  FadeOut: Effect_ZoomIn_Rotate),
    (Name: 'whiteshark';  Twinkle: true;  FadeIn: Effect_Rotate_Right_Top2; Draw: Effect_OnBeatJitter;  FadeOut: Effect_Shift_Left)
  );

  { name specific times }
  TimePerName = (Main_Names_End - Main_Names_Start) div Length(Developers);
  NameFadeTime = 12;   // duration of fade in/out in 1/100 secs
  NameWaitTime = 5;    // delay between fade out and fade in of the next devel in 1/100 secs
  NameTwinkleTime = 2; // duration of star effects in 1/100 secs
  BeatJitterTime = 3;  // duration of on beat jitter effect
  { position at which the names show up
    note: due to use of translate this is the center
    of the names not the upper left corner as usual }
  NameX = 223;
  NameY = 329;
  NameW = 326;
  NameH = 258;

const
  ID='ID_003';   //for help system

implementation

uses
  UCommon,
  UGraphic,
  UHelp,
  UIni,
  ULanguage,
  ULog,
  UMain,
  UPathUtils,
  USongs,
  UUnicodeUtils,
  Math,
  Textgl;

function TScreenCredits.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
          Exit;
        end;
    end;

    // check special keys
    case PressedKey of

      SDLK_ESCAPE,
      SDLK_BACKSPACE,
      SDLK_RETURN:
        begin
          FadeTo(@ScreenMain);
          AudioPlayback.PlaySound(SoundLib.Back);
        end;
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;

{
      SDLK_SPACE:
         begin
           setlength(CTime_hold,length(CTime_hold)+1);
           CTime_hold[high(CTime_hold)]:=CTime;
         end;
}
     end; // esac
    end;  // fi
end;

function TScreenCredits.ParseMouse(MouseButton: integer; BtnDown: boolean; X, Y: integer): boolean;
begin
  Result := inherited ParseMouse(MouseButton, BtnDown, X, Y);
  
  { calculate mouse coordinates from -1 to 1
    relative to screen center }
  MouseX := (X - (ScreenW / Screens) / 2) / ((ScreenW / Screens) / 2);
  MouseY := (Y - ScreenH / 2) / (ScreenH / 2);

  MouseMoved := true;
end;

procedure TScreenCredits.LoadNameTextures;
  var I: integer;
begin
  SetLength(credits_names, Length(Developers));

  for I  := 0 to High(Developers) do
  begin
    credits_names[I] := Texture.LoadTexture(CreditsPath.Append('names_' + Developers[I].Name + '.png'),  TEXTURE_TYPE_TRANSPARENT, 0);
  end;
end;

constructor TScreenCredits.Create;
begin
  inherited Create;

  CreditsPath := ResourcesPath.Append('credits', pdAppend);

  credits_bg_tex := Texture.LoadTexture(CreditsPath.Append(CRDTS_BG_FILE), TEXTURE_TYPE_PLAIN, 0);
  credits_bg_ovl := Texture.LoadTexture(CreditsPath.Append(CRDTS_OVL_FILE), TEXTURE_TYPE_TRANSPARENT, 0);

  LoadNameTextures;

  intro_layer01 := Texture.LoadTexture(CreditsPath.Append(INTRO_L01_FILE), TEXTURE_TYPE_TRANSPARENT, 0);
  intro_layer02 := Texture.LoadTexture(CreditsPath.Append(INTRO_L02_FILE), TEXTURE_TYPE_TRANSPARENT, 0);
  intro_layer03 := Texture.LoadTexture(CreditsPath.Append(INTRO_L03_FILE), TEXTURE_TYPE_TRANSPARENT, 0);
  intro_layer04 := Texture.LoadTexture(CreditsPath.Append(INTRO_L04_FILE), TEXTURE_TYPE_TRANSPARENT, 0);
  intro_layer05 := Texture.LoadTexture(CreditsPath.Append(INTRO_L05_FILE), TEXTURE_TYPE_TRANSPARENT, 0);
  intro_layer06 := Texture.LoadTexture(CreditsPath.Append(INTRO_L06_FILE), TEXTURE_TYPE_TRANSPARENT, 0);
  intro_layer07 := Texture.LoadTexture(CreditsPath.Append(INTRO_L07_FILE), TEXTURE_TYPE_TRANSPARENT, 0);
  intro_layer08 := Texture.LoadTexture(CreditsPath.Append(INTRO_L08_FILE), TEXTURE_TYPE_TRANSPARENT, 0);
  intro_layer09 := Texture.LoadTexture(CreditsPath.Append(INTRO_L09_FILE), TEXTURE_TYPE_TRANSPARENT, 0);

  outro_bg  := Texture.LoadTexture(CreditsPath.Append(OUTRO_BG_FILE),  TEXTURE_TYPE_PLAIN, 0);
  outro_esc := Texture.LoadTexture(CreditsPath.Append(OUTRO_ESC_FILE), TEXTURE_TYPE_TRANSPARENT, 0);
  outro_exd := Texture.LoadTexture(CreditsPath.Append(OUTRO_EXD_FILE), TEXTURE_TYPE_TRANSPARENT, 0);

  CRDTS_Stage:=InitialDelay;
end;

procedure TScreenCredits.OnShow;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenCredits');

 { pause background music }
  SoundLib.PauseBgMusic;

  CRDTS_Stage    := InitialDelay;
  CTime := 0;
  Credits_X      := 580;

  { open credits tune, we play it after initial delay }
  AudioPlayback.Open(soundpath.Append('wome-credits-tune.mp3'),nil); // thank you wetue

  { reset twinkling stars }
  GoldenRec.KillAll;
  
  { reset mouse coords }
  MouseMoved := false;
  MouseX := 0;
  MouseY := 0;

  { hide cursor }
  Display.SetCursor;
end;

procedure TScreenCredits.OnHide;
begin
  AudioPlayback.Stop;

  { show cursor }
  Display.SetCursor;

  SoundLib.StartBgMusic;
end;

function TScreenCredits.Draw: boolean;
  var
    T: cardinal;
begin
  Result := true;
  
  // reset beat detection
  BeatDetected := false;

  T := SDL_GetTicks() div 33;
  if T <> Credits_Time then
  begin
    Credits_Time := T;
    inc(CTime);
    inc(CTime_hold);
    Credits_X := Credits_X-2;

    if (CRDTS_Stage = InitialDelay) and (CTime >= Delay_Before_Start) then
    begin
      CRDTS_Stage := Intro;
      CTime := 0;
      AudioPlayback.Play;
    end
    else if (CRDTS_Stage = Intro) and (CTime >= Main_Start) then
    begin
      CRDTS_Stage := MainPart;
    end
    else if (CRDTS_Stage = MainPart) and (CTime >= Tune_End) then
    begin
      CRDTS_Stage := Outro;
    end;
    
    // dis does teh muiwk y0r   to be translated :-)
    DetectBeat;
  end;

  case CRDTS_Stage of
    InitialDelay: DrawInitialDelay;
    Intro:        DrawIntro;
    MainPart:     DrawMain;
    Outro:        DrawOutro;  
  end;

  // make the stars shine
  GoldenRec.Draw;
end;

procedure TScreenCredits.DrawInitialDelay;
begin
  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

procedure TScreenCredits.DrawIntro;
  var
    Separation, Scale,
    AngleX, AngleY, AngleZ: single;
    FlareX, FlareY: single;
    I: integer;
begin
  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  { rotate logo anti clockwise and make it grow }
  if (CTime >= Intro_Separation_End) then
  begin
    Separation := 1;
    Scale := 1 + sqr(CTime - Intro_Separation_End) / (32 * (Main_Start - Intro_Separation_End));
    AngleX := 0;
    AngleY := 0;
    AngleZ := 20 * sqr(CTime - Intro_Separation_End) / sqr((Main_Start - Intro_Separation_End) / 2);
  end

  { separate layers }
  else if (CTime >= Intro_Stand_End) then
  begin
    Separation := 0.5 + 0.5 * (CTime - Intro_Stand_End) / (Intro_Separation_End - Intro_Stand_End);
    Scale := 1;
    AngleX := 0;
    AngleY := 0;
    AngleZ := 0;
  end

  { stand still }
  else if (CTime >= Intro_Zoom_End) then
  begin
    Separation := 0.5;
    Scale := 1;
    AngleX := 0;
    AngleY := 0;
    AngleZ := 0;
  end

  { rotate left }
  else 
  begin
    Separation := 0.5 + 0.5 * (Intro_Zoom_End - CTime) / (Intro_Zoom_End);
    Scale := 1;
    AngleX := 10 * (Intro_Zoom_End - CTime) / (Intro_Zoom_End);
    AngleY := 20 * (Intro_Zoom_End - CTime) / (Intro_Zoom_End);
    AngleZ := 0;
  end;

  { the user moved the mouse, overwrite X and Y angle with
    according to mouse position }
  if (MouseMoved) then
  begin
    // calculate destination angle
    AngleX := 30 * MouseY;
    AngleY := 30 * MouseX;

    { move angle towards destination }
    if not SameValue(LogoAngleX, AngleX, 0.001) then
      AngleX := LogoAngleX + 0.05 * (AngleX - LogoAngleX);

    if not SameValue(LogoAngleY, AngleY, 0.001) then
      AngleY := LogoAngleY + 0.05 * (AngleY - LogoAngleY);
  end;

  // save last angle
  LogoAngleX := AngleX;
  LogoAngleY := AngleY;

  DrawLayeredLogo(Separation, Scale, AngleX, AngleY, AngleZ);

  { do some sparkling effects }
  if (CTime < Intro_Zoom_End) and (CTime > Intro_Flare_Start) then
  begin
    for I := 1 to 3 do
    begin
       FlareX := 410 + Floor((CTime - Intro_Flare_Start) / (Intro_Zoom_End - Intro_Flare_Start) * (536 - 410)) + RandomRange(-5, 5);
       FlareY := Floor((Intro_Zoom_End - CTime) / 22) + RandomRange(285, 301);
       GoldenRec.Spawn(FlareX, FlareY, 1, 16, 0, -1, Flare, 0);
    end;
  end;

  { fade to white at end }
  if Ctime > Intro_FadeToWhite_Start then
  begin
    glColor4f(1, 1, 1, sqr(CTime - Intro_FadeToWhite_Start) * (CTime - Intro_FadeToWhite_Start) / sqr(Main_Start - Intro_FadeToWhite_Start));
    glEnable(GL_BLEND);
    glBegin(GL_QUADS);
      glVertex2f(  0,   0);
      glVertex2f(  0, 600);
      glVertex2f(800, 600);
      glVertex2f(800,   0);
    glEnd;
    glDisable(GL_BLEND);
  end;
end;

procedure Start3D;
begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  glFrustum(-0.3 * 4 / 3, 0.3 * 4 / 3, -0.3, 0.3, 1, 1000);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure End3D;
begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
end;

procedure TScreenCredits.DrawLayeredLogo(Separation, Scale, AngleX, AngleY, AngleZ: single);
  var
    TotalAngle: single;
begin
  Start3D;
  glPushMatrix;

  glEnable(GL_TEXTURE_2D);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  glTranslatef(0, 0, -5 + 0.5 * Separation);

  TotalAngle := Abs(AngleX) + Abs(AngleY) + Abs(AngleZ);
  if not isZero(TotalAngle) then
    glRotatef(TotalAngle, AngleX / TotalAngle, AngleY / TotalAngle, AngleZ / TotalAngle);

  glScalef(Scale, Scale, 1);

  glScalef(4/3, -1, 1);
  glColor4f(1, 1, 1, 1);

  glBindTexture(GL_TEXTURE_2D, intro_layer01.TexNum);
  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex3f(-1, -1, -0.4 * Separation);
    glTexCoord2f(0, 1); glVertex3f(-1,  1, -0.4 * Separation);
    glTexCoord2f(1, 1); glVertex3f( 1,  1, -0.4 * Separation);
    glTexCoord2f(1, 0); glVertex3f( 1, -1, -0.4 * Separation);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, intro_layer02.TexNum);
  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex3f(-1, -1, -0.3 * Separation);
    glTexCoord2f(0, 1); glVertex3f(-1,  1, -0.3 * Separation);
    glTexCoord2f(1, 1); glVertex3f( 1,  1, -0.3 * Separation);
    glTexCoord2f(1, 0); glVertex3f( 1, -1, -0.3 * Separation);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, intro_layer03.TexNum);
  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex3f(-1, -1, -0.2 * Separation);
    glTexCoord2f(0, 1); glVertex3f(-1,  1, -0.2 * Separation);
    glTexCoord2f(1, 1); glVertex3f( 1,  1, -0.2 * Separation);
    glTexCoord2f(1, 0); glVertex3f( 1, -1, -0.2 * Separation);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, intro_layer04.TexNum);
  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex3f(-1, -1, -0.1 * Separation);
    glTexCoord2f(0, 1); glVertex3f(-1,  1, -0.1 * Separation);
    glTexCoord2f(1, 1); glVertex3f( 1,  1, -0.1 * Separation);
    glTexCoord2f(1, 0); glVertex3f( 1, -1, -0.1 * Separation);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, intro_layer05.TexNum);
  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex3f(-1, -1, 0 * Separation);
    glTexCoord2f(0, 1); glVertex3f(-1,  1, 0 * Separation);
    glTexCoord2f(1, 1); glVertex3f( 1,  1, 0 * Separation);
    glTexCoord2f(1, 0); glVertex3f( 1, -1, 0 * Separation);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, intro_layer06.TexNum);
  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex3f(-1, -1, 0.1 * Separation);
    glTexCoord2f(0, 1); glVertex3f(-1,  1, 0.1 * Separation);
    glTexCoord2f(1, 1); glVertex3f( 1,  1, 0.1 * Separation);
    glTexCoord2f(1, 0); glVertex3f( 1, -1, 0.1 * Separation);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, intro_layer07.TexNum);
  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex3f(-1, -1, 0.2 * Separation);
    glTexCoord2f(0, 1); glVertex3f(-1,  1, 0.2 * Separation);
    glTexCoord2f(1, 1); glVertex3f( 1,  1, 0.2 * Separation);
    glTexCoord2f(1, 0); glVertex3f( 1, -1, 0.2 * Separation);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, intro_layer08.TexNum);
  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex3f(-1, -1, 0.3 * Separation);
    glTexCoord2f(0, 1); glVertex3f(-1,  1, 0.3 * Separation);
    glTexCoord2f(1, 1); glVertex3f( 1,  1, 0.3 * Separation);
    glTexCoord2f(1, 0); glVertex3f( 1, -1, 0.3 * Separation);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, intro_layer09.TexNum);
  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex3f(-1, -1, 0.22 * Separation);
    glTexCoord2f(0, 1); glVertex3f(-1,  1, 0.22 * Separation);
    glTexCoord2f(1, 1); glVertex3f( 1,  1, 0.22 * Separation);
    glTexCoord2f(1, 0); glVertex3f( 1, -1, 0.22 * Separation);
  glEnd;

  glDisable(Gl_Texture_2D);
  glDisable(GL_BLEND);

  glPopMatrix;
  End3D;
end;

procedure TScreenCredits.DrawMain;  
begin
  DrawMainBG;
  DrawFunkyText;
  DrawNames;
  DrawMainFG;
  DoLogoBling;

  // fade out at end of main part
  if (Ctime > Main_FadeOut_Start) then
  begin
    glColor4f(0, 0, 0, (CTime - Main_FadeOut_Start) / (Tune_End - Main_FadeOut_Start));
    glEnable(GL_BLEND);
    glBegin(GL_QUADS);
      glVertex2f(  0,   0);
      glVertex2f(  0, 600);
      glVertex2f(800, 600);
      glVertex2f(800,   0);
    glEnd;
    glDisable(GL_BLEND);
  end;
end;

procedure TScreenCredits.DrawMainBG;
begin
  glEnable(GL_TEXTURE_2D);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, credits_bg_tex.TexNum);
  glBegin(Gl_Quads);
    glTexCoord2f(       0,        0); glVertex2f(      0,       0);
    glTexCoord2f(       0, 600/1024); glVertex2f(      0, RenderH);
    glTexCoord2f(800/1024, 600/1024); glVertex2f(RenderW, RenderH);
    glTexCoord2f(800/1024,        0); glVertex2f(RenderW,       0);
  glEnd;
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
end;

procedure TScreenCredits.DrawFunkyText;
var
  S:           integer;
  X, Y, A:     real;
  visibleText: string;
begin
  SetFontSize(30);

  // init ScrollingText
  if (CTime = Main_Start) then
  begin
    // set position of text
    Credits_X          := 600;
    CurrentScrollStart := 1;
    CurrentScrollEnd   := 1;
  end;

  if (CTime > Main_Start) and
     (CurrentScrollStart < length(Funky_Text)) then
  begin
    X := 0;
    visibleText := Copy(Funky_Text, CurrentScrollStart, CurrentScrollEnd);

    for S := 1 to length(visibleText) do
    begin
      Y := abs(sin((Credits_X + X) * 0.93 { * (((Credits_X + X)) / 1200) } / 100 * pi));
      SetFontPos(Credits_X + X, 538 - Y * (Credits_X + X) * (Credits_X + X) * (Credits_X + X) / 1000000);

      if (Credits_X + X > 32) then
        A := 17
      else if (Credits_X + X >= 15) then
        A := Credits_X + X - 15
      else
        A := 0;

      glColor4f(230 / 255 - 40 / 255 + Y * (Credits_X + X)/  900,
                200 / 255 - 30 / 255 + Y * (Credits_X + X)/ 1000,
                155 / 255 - 20 / 255 + Y * (Credits_X + X)/ 1100,
                  A / 17);
      glPrint(visibleText[S]);
      X := X + glTextWidth(visibleText[S]);
    end;

    if (Credits_X < 0) and (CurrentScrollStart < length(Funky_Text)) then
    begin
      Credits_X := Credits_X + glTextWidth(Funky_Text[CurrentScrollStart]);
      inc(CurrentScrollStart);
    end;

    visibleText := Copy(Funky_Text, CurrentScrollStart, CurrentScrollEnd);

    if (Credits_X + glTextWidth(visibleText) < 600) and
       (CurrentScrollEnd < length(Funky_Text)) then
    begin
      inc(CurrentScrollEnd);
    end;
  end;
{
// timing hack
  X:=5;
  SetFontStyle(2);
  SetFontItalic(false);
  SetFontSize(27);
  glColor4f(1, 1, 1, 1);
  for S := 0 to high(CTime_hold) do
  begin
    visibleText := inttostr(CTime_hold[S]);
    SetFontPos (500, X);
    glPrint(visibleText[0]);
    X := X + 20;
  end;
}
end;

procedure TScreenCredits.DrawNames;
  var
    Dev: integer;
    Ticks: integer;
    DevTicks: integer;
    TwinkleW, TwinkleH: integer;
begin
  Ticks := (CTime - Main_Names_Start);
  Dev := Ticks div TimePerName;
  DevTicks := Ticks mod TimePerName;

  {// debug stuff
  SetFontPos(20, 20);
  glPrint('Ticks: ' + IntToStr(Ticks));
  SetFontPos(20, 45);
  glPrint('Dev: ' + IntToStr(Dev));
  SetFontPos(20, 70);
  glPrint('DevTicks: ' + IntToStr(DevTicks)); //}

  if (Ticks >= 0) and (Dev <= High(Developers)) then
  begin
    { spawn twinkling stars }
    if (Developers[Dev].Twinkle) and (DevTicks >= NameFadeTime) and (DevTicks <= NameFadeTime + NameTwinkleTime) then
    begin
      TwinkleW := Round(NameW * 0.6);
      TwinkleH := Round(NameH * 0.6);

      GoldenRec.Spawn(NameX + RandomRange(-TwinkleW, TwinkleW), NameY + RandomRange(-TwinkleH, TwinkleH), 1, 16, 0, -1, PerfectLineTwinkle, 0);
      GoldenRec.Spawn(NameX + RandomRange(-TwinkleW, TwinkleW), NameY + RandomRange(-TwinkleH, TwinkleH), 1, 16, 0, -1, PerfectLineTwinkle, 1);
      GoldenRec.Spawn(NameX + RandomRange(-TwinkleW, TwinkleW), NameY + RandomRange(-TwinkleH, TwinkleH), 1, 16, 0, -1, PerfectLineTwinkle, 5);
      GoldenRec.Spawn(NameX + RandomRange(-TwinkleW, TwinkleW), NameY + RandomRange(-TwinkleH, TwinkleH), 1, 16, 0, -1, PerfectLineTwinkle, 0);
      GoldenRec.Spawn(NameX + RandomRange(-TwinkleW, TwinkleW), NameY + RandomRange(-TwinkleH, TwinkleH), 1, 16, 0, -1, PerfectLineTwinkle, 1);
      GoldenRec.Spawn(NameX + RandomRange(-TwinkleW, TwinkleW), NameY + RandomRange(-TwinkleH, TwinkleH), 1, 16, 0, -1, PerfectLineTwinkle, 5);
      GoldenRec.Spawn(NameX + RandomRange(-TwinkleW, TwinkleW), NameY + RandomRange(-TwinkleH, TwinkleH), 1, 16, 0, -1, PerfectLineTwinkle, 0);
      GoldenRec.Spawn(NameX + RandomRange(-TwinkleW, TwinkleW), NameY + RandomRange(-TwinkleH, TwinkleH), 1, 16, 0, -1, PerfectLineTwinkle, 1);
      GoldenRec.Spawn(NameX + RandomRange(-TwinkleW, TwinkleW), NameY + RandomRange(-TwinkleH, TwinkleH), 1, 16, 0, -1, PerfectLineTwinkle, 5);
    end;

    { prepare drawing }
    glPushMatrix;
    glTranslatef(NameX, NameY, 0);
    glBindTexture(GL_TEXTURE_2D, credits_names[Dev].TexNum);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glEnable(GL_TEXTURE_2D);

    // calculate progress and call effect
    if (DevTicks <= NameFadeTime) then
      Developers[Dev].FadeIn(credits_names[Dev], DevTicks / NameFadeTime)
    else if (DevTicks >= TimePerName - NameFadeTime - NameWaitTime) then
    begin
      if (DevTicks < TimePerName - NameWaitTime) then
        Developers[Dev].FadeOut(credits_names[Dev], ((TimePerName - NameWaitTime) - DevTicks) / NameFadeTime);
    end
    else
      Developers[Dev].Draw(credits_names[Dev], (DevTicks - NameFadeTime) / (TimePerName - NameFadeTime * 2 - NameWaitTime));

    glDisable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
    glPopMatrix;
  end;
end;

procedure TScreenCredits.DrawMainFG;
begin
  glEnable(GL_TEXTURE_2D);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, credits_bg_ovl.TexNum);
  glBegin(gl_Quads);
    glTexCoord2f(      0,        0); glVertex2f(800-393,   0);
    glTexCoord2f(      0, 600/1024); glVertex2f(800-393, 600);
    glTexCoord2f(393/512, 600/1024); glVertex2f(800,     600);
    glTexCoord2f(393/512,        0); glVertex2f(800,       0);
  glEnd;


  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
end;

procedure TScreenCredits.DoLogoBling;
  const
    myLogoCoords: array[0..27,0..1] of cardinal = (
      ( 39,32),( 84,32),(100,16),(125,24),
      (154,31),(156,58),(168,32),(203,36),
      (258,34),(251,50),(274,93),(294,84),
      (232,54),(278,62),(319,34),(336,92),
      (347,23),(374,32),(377,58),(361,83),
      (385,91),(405,91),(429,35),(423,51),
      (450,32),(485,34),(444,91),(486,93)
    );
  var
    Coords: integer;
    StartFrame: integer;
begin
  if (CTime > Main_OnBeatTwinkle_Start ) and
     (CTime < Main_FadeOut_Start) then
    begin
      { spawn stars only in frames where a beat was detected }
      if BeatDetected then
      begin
         StartFrame := RandomRange(6, 16);
         Coords := RandomRange(0, 27);

         GoldenRec.Spawn(myLogoCoords[Coords,0], myLogoCoords[Coords,1], 16-StartFrame, StartFrame, 0, -1, PerfectNote, 0);
      end;
    end;
end;

procedure TScreenCredits.DrawOutro;
begin
  if CTime = Tune_End then
  begin
    CTime_hold := 0;
    AudioPlayback.Stop;
    AudioPlayback.Open(SoundPath.Append('credits-outro-tune.mp3'),nil);
    AudioPlayback.SetVolume(0.2);
    AudioPlayback.SetLoop(true);
    AudioPlayback.Play;
  end;

  if CTime_hold > 231 then
  begin
    AudioPlayback.Play;
    Ctime_hold := 0;
  end;

  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  // do something useful
  // outro background
  glEnable(GL_TEXTURE_2D);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, outro_bg.TexNum);
  glBegin(gl_quads);
    glTexCoord2f(       0,        0); glVertex2f(  0,   0);
    glTexCoord2f(       0, 600/1024); glVertex2f(  0, 600);
    glTexCoord2f(800/1024, 600/1024); glVertex2f(800, 600);
    glTexCoord2f(800/1024,        0); glVertex2f(800,   0);
  glEnd;

  // outro overlays
  glColor4f(1, 1, 1, (2 + sin(CTime / 15)) / 3);
  glBindTexture(GL_TEXTURE_2D, outro_esc.TexNum);
  glBegin(Gl_Quads);
    glTexCoord2f(      0,       0); glVertex2f(  0,   0);
    glTexCoord2f(      0, 223/256); glVertex2f(  0, 223);
    glTexCoord2f(487/512, 223/256); glVertex2f(487, 223);
    glTexCoord2f(487/512,       0); glVertex2f(487,   0);
  glEnd;

  if (RandomRange(0,20) <= 18) then
  begin
    glColor4f(1, 1, 1, 1);
    glBindTexture(GL_TEXTURE_2D, outro_exd.TexNum);
    glBegin(Gl_Quads);
      glTexCoord2f(      0,       0); glVertex2f(800-310, 600-247);
      glTexCoord2f(      0, 247/256); glVertex2f(800-310, 600    );
      glTexCoord2f(310/512, 247/256); glVertex2f(800,     600    );
      glTexCoord2f(310/512,       0); glVertex2f(800,     600-247);
    glEnd;
  end;

  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);

  // outro scrollers?
  // ...
end;

{ name effects }
{ effects are called with blending texture and matrix prepared }
procedure Effect_Draw (const Tex: TTexture; Progress: double);
begin
  glColor4f(1, 1, 1, 1);
  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2, -NameH/2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2,  NameH/2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2,  NameH/2);
    glTexCoord2f(1, 0); glVertex2f( NameW/2, -NameH/2);
  glEnd;
end;

procedure Effect_OnBeatJitter (const Tex: TTexture; Progress: double);
  var
    Diff: cardinal;
    Alpha: double;
begin
  Diff := ScreenCredits.CTime - ScreenCredits.LastBeatTime;
  if (Diff < BeatJitterTime) then  
    Alpha := 0.5 + 0.5 * Diff / BeatJitterTime
  else
    Alpha := 1;

  glColor4f(1, 1, 1, Alpha);
  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2, -NameH/2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2,  NameH/2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2,  NameH/2);
    glTexCoord2f(1, 0); glVertex2f( NameW/2, -NameH/2);
  glEnd;
end;

procedure Effect_Rotate_Left_Top (const Tex: TTexture; Progress: double);
begin
  glColor4f(1, 1, 1, Progress);

  gltranslatef(-NameX, 0, 0);
  glrotatef(Progress * 90 + 270, 0, 0, 1);
  gltranslatef(NameX, 0, 0);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2, -NameH/2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2,  NameH/2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2,  NameH/2);
    glTexCoord2f(1, 0); glVertex2f( NameW/2, -NameH/2);
  glEnd;
end;

procedure Effect_Rotate_Right_Bot (const Tex: TTexture; Progress: double);
begin
  glColor4f(1, 1, 1, Progress);

  gltranslatef(NameX, 0, 0);
  glrotatef((Progress - 1) * 90, 0, 0, 1);
  gltranslatef(-NameX, 0, 0);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2, -NameH/2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2,  NameH/2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2,  NameH/2);
    glTexCoord2f(1, 0); glVertex2f( NameW/2, -NameH/2);
  glEnd;
end;

procedure Effect_ZoomIn_Rotate (const Tex: TTexture; Progress: double);
begin
  glColor4f(1, 1, 1, Progress);

  glscalef(sqr(Progress), sqr(Progress), sqr(Progress));
  glrotatef(Progress * 360, 0, 0, 1);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2, -NameH/2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2,  NameH/2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2,  NameH/2);
    glTexCoord2f(1, 0); glVertex2f( NameW/2, -NameH/2);
  glEnd;
end;

procedure Effect_ZoomOut_Shift (const Tex: TTexture; Progress: double);
  var
    X: double;
begin
  glColor4f(1, 1, 1, Progress);

  X := (1 - Progress);
  gltranslatef(X * 300, -X * 100, 0);
  glscalef(1 + X, 1 + X, 1 + X);
  glrotatef(X * 90, 0, 0, 1);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2, -NameH/2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2,  NameH/2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2,  NameH/2);
    glTexCoord2f(1, 0); glVertex2f( NameW/2, -NameH/2);
  glEnd;
end;

procedure Effect_Shift_Left (const Tex: TTexture; Progress: double);
begin
  glColor4f(1, 1, 1, Progress);

  glTranslatef((Progress - 1) * 210, 0, 0);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2, -NameH/2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2,  NameH/2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2,  NameH/2);
    glTexCoord2f(1, 0); glVertex2f( NameW/2, -NameH/2);
  glEnd;
end;

procedure Effect_Shift_Right_Top (const Tex: TTexture; Progress: double);
begin
  glColor4f(1, 1, 1, Progress);

  glTranslatef((1 - Progress) * 210, (Progress - 1) * 105, 0);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2, -NameH/2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2,  NameH/2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2,  NameH/2);
    glTexCoord2f(1, 0); glVertex2f( NameW/2, -NameH/2);
  glEnd;
end;

procedure Effect_Flip_Bot (const Tex: TTexture; Progress: double);
  var
    X: double;
begin
  glColor4f(1, 1, 1, Progress);

  X := NameH * (1 - Progress);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2 - 1.5 * X, -NameH/2 + 1.5 * X);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2,  NameH/2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2,  NameH/2);
    glTexCoord2f(1, 0); glVertex2f( NameW/2 + 1.5 * X, -NameH/2 + 1.5 * X);
  glEnd;
end;

procedure Effect_Flip_Right_Top (const Tex: TTexture; Progress: double);
  var
    X: double;
begin
  glColor4f(1, 1, 1, Progress);

  X := NameW * (1 - Progress);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2 + X, -NameH/2 - X/2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2 + X,  NameH/2 - (X * 1.5 * NameH / NameW));
    glTexCoord2f(1, 1); glVertex2f( NameW/2 + X,  NameH/2 + X / 4);
    glTexCoord2f(1, 0); glVertex2f( NameW/2 + X, -NameH/2 - X / 4);
  glEnd;
end;

procedure Effect_Flip_Right (const Tex: TTexture; Progress: double);
  var
    X: double;
begin
  glColor4f(1, 1, 1, Progress);

  X := NameW * (1 - Progress);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2,     -NameH/2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2,      NameH/2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2 - X,  NameH/2 + X * 1.5);
    glTexCoord2f(1, 0); glVertex2f( NameW/2 - X, -NameH/2 - X * 1.5);
  glEnd;
end;

procedure Effect_Flip_Right_Bot (const Tex: TTexture; Progress: double);
  var
    X: double;
begin
  glColor4f(1, 1, 1, Progress);

  X := NameW * (1 - Progress);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2 + X * 1.5, -NameH/2 + X * 1.5);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2 + X * 1.2,  NameH/2 + X);
    glTexCoord2f(1, 1); glVertex2f( NameW/2 + X / 2,    NameH/2 + X / 4);
    glTexCoord2f(1, 0); glVertex2f( NameW/2 + X * 1.5, -NameH/2);
  glEnd;
end;

procedure Effect_Rotate_Right_Top (const Tex: TTexture; Progress: double);
begin
  glColor4f(1, 1, 1, Progress);

  glTranslatef(NameX, 0, 0);
  glrotatef((1 - Progress) * 90, 0, 0, 1);
  glTranslatef(-NameX, 0, 0);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2, -NameH/2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2,  NameH/2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2,  NameH/2);
    glTexCoord2f(1, 0); glVertex2f( NameW/2, -NameH/2);
  glEnd;
end;

procedure Effect_Shift_Weird (const Tex: TTexture; Progress: double);
  var
    X: double;
begin
  glColor4f(1, 1, 1, Progress);

  X := (Progress - 1);

  glTranslatef(X * 200, X * 100, 0);
  glScalef(Progress, Progress, Progress);
  glRotatef(X * 90, 0, 0, 1);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2, -NameH/2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2,  NameH/2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2,  NameH/2);
    glTexCoord2f(1, 0); glVertex2f( NameW/2, -NameH/2);
  glEnd;
end;

procedure Effect_Shift_Right_Bot (const Tex: TTexture; Progress: double);
begin
  glColor4f(1, 1, 1, Progress);

  glTranslatef((1 - Progress) * 200, (1 - Progress) * 100, 0);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2, -NameH/2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2,  NameH/2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2,  NameH/2);
    glTexCoord2f(1, 0); glVertex2f( NameW/2, -NameH/2);
  glEnd;
end;

procedure Effect_Rotate_Right_Top2 (const Tex: TTexture; Progress: double);
begin
  glColor4f(1, 1, 1, Progress);

  glTranslatef(0, -NameX, 0);
  glRotatef((Progress - 1) * 90, 0, 0, 1);
  glTranslatef(0, NameX, 0);
  glRotatef((1 - Progress) * 90, 0, 0, 1);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2, -NameH/2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2,  NameH/2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2,  NameH/2);
    glTexCoord2f(1, 0); glVertex2f( NameW/2, -NameH/2);
  glEnd;
end;

procedure Effect_Flip_Left_Bot (const Tex: TTexture; Progress: double);
  var
    X: double;
begin
  glColor4f(1, 1, 1, Progress);

  X := (1 - Progress) * NameW;

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2 - X,       -NameH/2 + X / 4);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2 - X / 4,    NameH/2 + X / 4);
    glTexCoord2f(1, 1); glVertex2f( NameW/2 - X * 1.2,  NameH/2 + X / 2);
    glTexCoord2f(1, 0); glVertex2f( NameW/2 - X * 1.5, -NameH/2 + X * 1.5);
  glEnd;
end;

procedure Effect_Flip_Right_Top2 (const Tex: TTexture; Progress: double);
  var
    X: double;
begin
  glColor4f(1, 1, 1, Progress);

  X := (1 - Progress) * NameW;

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2 + X,     -NameH/2 - X / 2);
    glTexCoord2f(0, 1); glVertex2f(-NameW/2 + X,      NameH/2 + X / 2);
    glTexCoord2f(1, 1); glVertex2f( NameW/2 + X / 4,  NameH/2 - X / 4);
    glTexCoord2f(1, 0); glVertex2f( NameW/2 + X / 4, -NameH/2 + X / 4);
  glEnd;
end;

procedure Effect_Twinkle_Down     (const Tex: TTexture; Progress: double);
begin
  // draw name
  glColor4f(1, 1, 1, 1);

  glTranslatef(0, NameH/2, 0);

  glBegin(gl_Quads);
    glTexCoord2f(0, 0); glVertex2f(-NameW/2, -NameH * Progress);
    glTexCoord2f(0, Progress); glVertex2f(-NameW/2, 0);
    glTexCoord2f(1, Progress); glVertex2f( NameW/2, 0);
    glTexCoord2f(1, 0); glVertex2f( NameW/2, -NameH * Progress);
  glEnd;

  //spawn some stars on the edge
  GoldenRec.Spawn(NameX + RandomRange(-NameW div 2, NameW div 2), NameY - NameH/2 + (1 - Progress) * NameH, 1, 16, 0, -1, PerfectLineTwinkle, 0);
  GoldenRec.Spawn(NameX + RandomRange(-NameW div 2, NameW div 2), NameY - NameH/2 + (1 - Progress) * NameH, 1, 16, 0, -1, PerfectLineTwinkle, 1);
  GoldenRec.Spawn(NameX + RandomRange(-NameW div 2, NameW div 2), NameY - NameH/2 + (1 - Progress) * NameH, 1, 16, 0, -1, PerfectLineTwinkle, 5);
  GoldenRec.Spawn(NameX + RandomRange(-NameW div 2, NameW div 2), NameY - NameH/2 + (1 - Progress) * NameH, 1, 16, 0, -1, PerfectLineTwinkle, 0);
  GoldenRec.Spawn(NameX + RandomRange(-NameW div 2, NameW div 2), NameY - NameH/2 + (1 - Progress) * NameH, 1, 16, 0, -1, PerfectLineTwinkle, 1);
  GoldenRec.Spawn(NameX + RandomRange(-NameW div 2, NameW div 2), NameY - NameH/2 + (1 - Progress) * NameH, 1, 16, 0, -1, PerfectLineTwinkle, 5);
  GoldenRec.Spawn(NameX + RandomRange(-NameW div 2, NameW div 2), NameY - NameH/2 + (1 - Progress) * NameH, 1, 16, 0, -1, PerfectLineTwinkle, 0);
  GoldenRec.Spawn(NameX + RandomRange(-NameW div 2, NameW div 2), NameY - NameH/2 + (1 - Progress) * NameH, 1, 16, 0, -1, PerfectLineTwinkle, 1);
  GoldenRec.Spawn(NameX + RandomRange(-NameW div 2, NameW div 2), NameY - NameH/2 + (1 - Progress) * NameH, 1, 16, 0, -1, PerfectLineTwinkle, 5);
end;

{ calculates average value of a history buffer }
function Average(History: TEnergyHistory): single;
  var I: integer; 
begin
  Result := 0;

  for I := 0 to HistoryLength - 1 do
    Result := Result + History[I];

  Result := Result / HistoryLength;
end;

{ calculates variance value of a history buffer }
function Variance(History: TEnergyHistory; Average: single): single;
  var I: integer;
begin
  Result := 0;

  for I := 0 to HistoryLength - 1 do
    Result := Result + sqr(History[I] - Average);

  Result := Result / HistoryLength;
end;

{ shifts all values of the history to the right and
  adds the new value at the front }
procedure AddHistory(Value: single; var History: TEnergyHistory);
  var I: integer;
begin
  for I := HistoryLength - 1 downto 1 do
    History[I] := History[I-1];

  History[0] := Value;
end;

{ calculates instant energy from FFT data for a specific
  subchannel (0..SubChannelCount - 1) }
function CalculateInstantEnergy(SubChannel: integer; Data: TFFTData): single;
  var I: integer;
begin
  Result := 0;
  for I := SubChannel * SamplesPerChannel to (SubChannel + 1) * SamplesPerChannel - 1 do
    Result := Result + Data[I] * BeatEnergyModifier;

  Result := Result / SamplesPerChannel;
end;

procedure TScreenCredits.DetectBeat;
  var
    Data: TFFTData;
    I: integer;
    Instant: single;
    C, E, V: single;
begin
  AudioPlayback.GetFFTData(Data);

  // do beatdetection for every subchannel
  for I := 0 to SubChannelCount - 1 do
  begin
    Instant := CalculateInstantEnergy(I, Data);
    E := Average(SubchannelHistory[I]);
    V := Variance(SubchannelHistory[I], E);

    C := (-0.0025714 * V) + 1.5142857;

    AddHistory(Instant, SubChannelHistory[I]);

    if (Instant > 2) and (Instant > C * E) then
    begin
      // beat detected
      BeatDetected := true;
      LastBeatTime := CTime;
    end;
  end;
end;

end.
