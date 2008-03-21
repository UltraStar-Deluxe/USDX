unit UScreenTop5;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu, SDL, SysUtils, UDisplay, UMusic, USongs, UThemes;

type
  TScreenTop5 = class(TMenu)
    public
      TextLevel:        integer;
      TextArtistTitle:  integer;

      StaticNumber:     array[1..5] of integer;
      TextNumber:       array[1..5] of integer;
      TextName:         array[1..5] of integer;
      TextScore:        array[1..5] of integer;

      Fadeout:      boolean;
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      function Draw: boolean; override;
  end;

implementation

uses UGraphic, UDataBase, UMain, UIni;

function TScreenTop5.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then begin
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;


      SDLK_ESCAPE,
      SDLK_BACKSPACE,
      SDLK_RETURN:
        begin
          if (not Fadeout) then begin
            FadeTo(@ScreenSong);
            Fadeout := true;
          end;
        end;
      SDLK_SYSREQ:
        begin
          Display.PrintScreen;
        end;
    end;
  end;
end;

constructor TScreenTop5.Create;
var
  I:    integer;
begin
  inherited Create;

  LoadFromTheme(Theme.Top5);


  TextLevel := AddText(Theme.Top5.TextLevel);
  TextArtistTitle := AddText(Theme.Top5.TextArtistTitle);

  for I := 0 to 4 do
    StaticNumber[I+1] := AddStatic( Theme.Top5.StaticNumber[I] );

  for I := 0 to 4 do
    TextNumber[I+1] := AddText(Theme.Top5.TextNumber[I]);
  for I := 0 to 4 do
    TextName[I+1] := AddText(Theme.Top5.TextName[I]);
  for I := 0 to 4 do
    TextScore[I+1] := AddText(Theme.Top5.TextScore[I]);

end;

procedure TScreenTop5.onShow;
var
  I:      integer;
  PMax:   integer;
begin
  inherited;

  Fadeout := false;

  //ReadScore(CurrentSong);

  PMax := Ini.Players;
  if Ini.Players = 4 then Ini.Players := 5;
  for I := 0 to PMax do
    DataBase.AddScore(CurrentSong, Ini.Difficulty, Ini.Name[I], Round(Player[I].ScoreTotalI));

  DataBase.WriteScore(CurrentSong);
  DataBase.ReadScore(CurrentSong);

  Text[TextArtistTitle].Text := CurrentSong.Artist + ' - ' + CurrentSong.Title;

  for I := 1 to Length(CurrentSong.Score[Ini.Difficulty]) do begin
    Static[StaticNumber[I]].Visible := true;
    Text[TextNumber[I]].Visible := true;
    Text[TextName[I]].Visible := true;
    Text[TextScore[I]].Visible := true;

    Text[TextName[I]].Text := CurrentSong.Score[Ini.Difficulty, I-1].Name;
    Text[TextScore[I]].Text := IntToStr(CurrentSong.Score[Ini.Difficulty, I-1].Score);
  end;

  for I := Length(CurrentSong.Score[Ini.Difficulty])+1 to 5 do begin
    Static[StaticNumber[I]].Visible := false;
    Text[TextNumber[I]].Visible := false;
    Text[TextName[I]].Visible := false;
    Text[TextScore[I]].Visible := false;
  end;

  Text[TextLevel].Text := IDifficulty[Ini.Difficulty];
end;

function TScreenTop5.Draw: boolean;
//var
{  Min:    real;
  Max:    real;
  Wsp:    real;
  Wsp2:   real;
  Pet:    integer;}

{  Item:   integer;
  P:      integer;
  C:      integer;}
begin
  // Singstar - let it be...... with 6 statics
(*  if PlayersPlay = 6 then begin
    for Item := 4 to 6 do begin
      if ScreenAct = 1 then P := Item-4;
      if ScreenAct = 2 then P := Item-1;

      FillPlayer(Item, P);

{      if ScreenAct = 1 then begin
        LoadColor(
          Static[StaticBoxLightest[Item]].Texture.ColR,
          Static[StaticBoxLightest[Item]].Texture.ColG,
          Static[StaticBoxLightest[Item]].Texture.ColB,
          'P1Dark');
      end;

      if ScreenAct = 2 then begin
        LoadColor(
          Static[StaticBoxLightest[Item]].Texture.ColR,
          Static[StaticBoxLightest[Item]].Texture.ColG,
          Static[StaticBoxLightest[Item]].Texture.ColB,
          'P4Dark');
      end; }

    end;
  end; *)

  inherited Draw;
end;

end.
