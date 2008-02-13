unit UScreenMain;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  SDL,
  UDisplay,
  UMusic,
  UFiles,
  SysUtils,
  UThemes,
  ULCD,
  ULight;

type
  TScreenMain = class(TMenu)
    public
      TextDescription:        integer;
      TextDescriptionLong:    integer;

      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure InteractNext; override;
      procedure InteractPrev; override;
      procedure InteractInc; override;
      procedure InteractDec; override;
      procedure UpdateLCD;
      procedure SetAnimationProgress(Progress: real); override;
      //function Draw: boolean; override;
  end;

implementation

uses {$IFDEF win32}
     windows,
     {$ENDIF}
     UGraphic,
     UMain,
     UIni,
     UTexture,
     USongs,
     Textgl,
//     opengl,
     ULanguage,
     UParty,
     UDLLManager,
     UScreenCredits,
     USkins;


function TScreenMain.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
I: Integer;
SDL_ModState:  Word;
begin
  Result := true;

  SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

  //Deactivate Credits when Key is pressed
//  if Credits_Visible then
//  begin
//    Credits_Visible := False;
//    exit;
//  end;

  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Result := False;
        end;

      SDLK_C:
        begin
          if (SDL_ModState = KMOD_LALT) then
          begin
            //Credits_Y := 600;
            //Credits_Alpha := 0;
            //Credits_Visible := True;
            FadeTo(@ScreenCredits , SoundLib.Start );
          end;
        end;
      SDLK_M:
        begin
          if (Ini.Players >= 1) AND (Length(DLLMan.Plugins)>=1) then
          begin
            FadeTo(@ScreenPartyOptions, SoundLib.Start);
          end;
        end;

      SDLK_S:
        begin
          FadeTo(@ScreenStatMain, SoundLib.Start);
        end;

      SDLK_E:
        begin
          FadeTo(@ScreenEdit, SoundLib.Start);
        end;

      SDLK_RETURN:
        begin
          //Solo
          if (Interaction = 0) then
          begin
            if (Songs.SongList.Count >= 1) then
            begin
              if (Ini.Players >= 0) and (Ini.Players <= 3) then PlayersPlay := Ini.Players + 1;
              if (Ini.Players = 4) then PlayersPlay := 6;

              ScreenName.Goto_SingScreen := False;
              FadeTo(@ScreenName, SoundLib.Start);
            end
            else //show error message
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
          end;

          //Multi
          if Interaction = 1 then
          begin
            if (Songs.SongList.Count >= 1) then
            begin
              if (Length(DLLMan.Plugins)>=1) then
              begin
                FadeTo(@ScreenPartyOptions, SoundLib.Start);
              end
              else //show error message, No Plugins Loaded
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_PLUGINS'));
            end
            else //show error message, No Songs Loaded
              ScreenPopupError.ShowPopup(Language.Translate('ERROR_NO_SONGS'));
          end;

          //Stats
          if Interaction = 2 then
          begin
            FadeTo(@ScreenStatMain, SoundLib.Start);
          end;

          //Editor
          if Interaction = 3 then
          begin
            FadeTo(@ScreenEdit, SoundLib.Start);
          end;

          //Options
          if Interaction = 4 then
          begin
            FadeTo(@ScreenOptions, SoundLib.Start);
          end;

          //Exit
          if Interaction = 5 then
          begin
            Result := false;
          end;
        end;
      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    InteractInc;
      SDLK_UP:      InteractDec;
      SDLK_RIGHT:   InteractNext;
      SDLK_LEFT:    InteractPrev;
    end;
  end
  else // Key Up
    case PressedKey of
      SDLK_RETURN :
        begin
        end;
    end;
end;

constructor TScreenMain.Create;
var
  I:    integer;
begin
  inherited Create;

  //----------------
  //Attention ^^:
  //New Creation Order needed because of LoadFromTheme
  //and Button Collections.
  //At First Custom Texts and Statics
  //Then LoadFromTheme
  //after LoadFromTheme the Buttons and Selects
  //----------------


  TextDescription     := AddText(Theme.Main.TextDescription);
  TextDescriptionLong := AddText(Theme.Main.TextDescriptionLong);

  LoadFromTheme(Theme.Main);

  AddButton(Theme.Main.ButtonSolo);
  AddButton(Theme.Main.ButtonMulti);
  AddButton(Theme.Main.ButtonStat);
  AddButton(Theme.Main.ButtonEditor);
  AddButton(Theme.Main.ButtonOptions);
  AddButton(Theme.Main.ButtonExit);

  Interaction := 0;
end;

procedure TScreenMain.onShow;
begin
  LCD.WriteText(1, '  Choose mode:  ');
  UpdateLCD;
end;

procedure TScreenMain.InteractNext;
begin
  inherited InteractNext;
  Text[TextDescription].Text     := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
  UpdateLCD;
  Light.LightOne(1, 200);
end;

procedure TScreenMain.InteractPrev;
begin
  inherited InteractPrev;
  Text[TextDescription].Text     := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
  UpdateLCD;
  Light.LightOne(0, 200);
end;

procedure TScreenMain.InteractDec;
begin
  inherited InteractDec;
  Text[TextDescription].Text     := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
  UpdateLCD;
  Light.LightOne(0, 200);
end;

procedure TScreenMain.InteractInc;
begin
  inherited InteractInc;
  Text[TextDescription].Text := Theme.Main.Description[Interaction];
  Text[TextDescriptionLong].Text := Theme.Main.DescriptionLong[Interaction];
  UpdateLCD;
  Light.LightOne(1, 200);
end;

procedure TScreenMain.UpdateLCD;
begin
  case Interaction of
    0:  LCD.WriteText(2, '      sing      ');
    1:  LCD.WriteText(2, '     editor     ');
    2:  LCD.WriteText(2, '    options     ');
    3:  LCD.WriteText(2, '      exit      ');
  end
end;

procedure TScreenMain.SetAnimationProgress(Progress: real);
begin
  Static[0].Texture.ScaleW := Progress;
  Static[0].Texture.ScaleH := Progress;
end;
end.
