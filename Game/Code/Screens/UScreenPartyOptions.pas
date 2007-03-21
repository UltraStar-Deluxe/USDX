unit UScreenPartyOptions;

interface

uses
  UMenu, SDL, UDisplay, UMusic, UPliki, SysUtils, UThemes;

type
  TScreenPartyOptions = class(TMenu)
    public
      SelectLevel: Cardinal;
      SelectPlayList: Cardinal;
      SelectPlayList2: Cardinal;
      SelectRounds: Cardinal;
      SelectTeams: Cardinal;
      SelectPlayers1: Cardinal;
      SelectPlayers2: Cardinal;
      SelectPlayers3: Cardinal;

      PlayList:  Integer;
      PlayList2: Integer;
      Rounds:    Integer;
      NumTeams:  Integer;
      NumPlayer1, NumPlayer2, NumPlayer3: Integer;
      
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;
  end;

var
  ILevel: array[0..2] of String;
const
  ITeams: array[0..1] of String =('2', '3');
  IPlayers: array[0..3] of String =('1', '2', '3', '4');
  IRounds: array[0..5] of String = ('2', '3', '4', '5', '6', '7');

implementation

uses UGraphic, UMain, UIni, UTexture, ULanguage, UParty, UDLLManager;

function TScreenPartyOptions.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var I: Integer;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_ESCAPE :
        begin
          Music.PlayBack;
          FadeTo(@ScreenMain);
        end;

      SDLK_RETURN:
        begin

          //Save Difficulty
          Ini.Difficulty := SelectsS[SelectLevel].SelectedOption;
          Ini.SaveLevel;
          //Save PlayList
          //(Todo)
          //Save Num Teams:
          PartySession.Teams.NumTeams := NumTeams + 2;
          PartySession.Teams.Teaminfo[0].NumPlayers := NumPlayer1+1;
          PartySession.Teams.Teaminfo[1].NumPlayers := NumPlayer2+1;
          PartySession.Teams.Teaminfo[2].NumPlayers := NumPlayer3+1;
          //Save Rounds + Random
          SetLength (PartySession.Rounds, Rounds + 2);
          For I := 0 to high (PartySession.Rounds) do
          begin
            PartySession.Rounds[I].Plugin := Random (Length(DLLMan.Plugins));
            PartySession.Rounds[I].Winner := 0;
          end;
          Music.PlayStart;
          //Go to Player Screen
          FadeTo(@ScreenPartyPlayer);
        end;

      // Up and Down could be done at the same time,
      // but I don't want to declare variables inside
      // functions like this one, called so many times
      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;
      SDLK_RIGHT:
        begin
          Music.PlayOption;
          InteractInc;
        end;
      SDLK_LEFT:
        begin
          Music.PlayOption;
          InteractDec;
        end;
    end;
  end
  else // Key Up
    case PressedKey of
      SDLK_RETURN :
        begin
        end;
    end;
end;

constructor TScreenPartyOptions.Create;
var
  I:    integer;
begin
  inherited Create;

  //Fill ILevel
  ILevel[0] := Language.Translate('SING_EASY');
  ILevel[1] := Language.Translate('SING_MEDIUM');
  ILevel[2] := Language.Translate('SING_HARD');

  NumTeams := 0;
  NumPlayer1 := 0;
  NumPlayer2 := 0;
  NumPlayer3 := 0;
  Rounds := 5;
  PlayList := 0;
  PlayList2 := 0;

  AddBackground(Theme.PartyOptions.Background.Tex);

  SelectLevel := AddSelectSlide (Theme.PartyOptions.SelectLevel, Ini.Difficulty, ILevel);
  SelectPlayList := AddSelectSlide (Theme.PartyOptions.SelectPlayList, PlayList, ITeams);
  SelectPlayList2 := AddSelectSlide (Theme.PartyOptions.SelectPlayList2, PlayList2, ITeams);
  SelectRounds := AddSelectSlide (Theme.PartyOptions.SelectRounds, Rounds, IRounds);
  SelectTeams := AddSelectSlide (Theme.PartyOptions.SelectTeams, NumTeams, ITeams);
  SelectPlayers1 := AddSelectSlide (Theme.PartyOptions.SelectPlayers1, NumPlayer1, IPlayers);
  SelectPlayers2 := AddSelectSlide (Theme.PartyOptions.SelectPlayers2, NumPlayer2, IPlayers);
  SelectPlayers3 := AddSelectSlide (Theme.PartyOptions.SelectPlayers3, NumPlayer3, IPlayers);

  for I := 0 to High(Theme.PartyOptions.Static) do
    AddStatic(Theme.PartyOptions.Static[I]);

  for I := 0 to High(Theme.PartyOptions.Text) do
    AddText(Theme.PartyOptions.Text[I]);

  Interaction := 0;
end;

procedure TScreenPartyOptions.onShow;
begin
  Randomize;

//  LCD.WriteText(1, '  Choose mode:  ');
//  UpdateLCD;
end;

procedure TScreenPartyOptions.SetAnimationProgress(Progress: real);
begin
  {for I := 0 to 6 do
    SelectS[I].Texture.ScaleW := Progress;}
end;

end.
