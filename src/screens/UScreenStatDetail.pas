unit UScreenStatDetail;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  SDL,
  SysUtils,
  UDisplay,
  UMusic,
  UIni,
  UDataBase,
  UThemes;

type
  TScreenStatDetail = class(TMenu)
    public
      Typ:  TStatType;
      Page: Cardinal;
      Count: Byte;
      Reversed: Boolean;

      TotEntrys: Cardinal;
      TotPages:  Cardinal;


      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;

      procedure SetTitle;
      Procedure SetPage(NewPage: Cardinal);
  end;

implementation

uses
  UGraphic,
  ULanguage,
  Math,
  Classes,
  ULog;

function TScreenStatDetail.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    // check normal keys
    case WideCharUpperCase(CharCode)[1] of
      'Q':
        begin
          Result := false;
          Exit;
        end;
    end;
    
    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenStatMain);
        end;
      SDLK_RETURN:
        begin
          if Interaction = 0 then begin
            //Next Page
            SetPage(Page+1);
          end;

          if Interaction = 1 then begin
            //Previous Page
            if (Page > 0) then
              SetPage(Page-1);
          end;

          if Interaction = 2 then begin
            //Reverse Order
            Reversed := not Reversed;
            SetPage(Page);
          end;

          if Interaction = 3 then begin
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenStatMain);
          end;
        end;
      SDLK_LEFT:
      begin
          InteractPrev;
      end;
      SDLK_RIGHT:
      begin
          InteractNext;
      end;
      SDLK_UP:
      begin
          InteractPrev;
      end;
      SDLK_DOWN:
      begin
          InteractNext;
      end;
    end;
  end;
end;

constructor TScreenStatDetail.Create;
var
  I:    integer;
begin
  inherited Create;

  for I := 0 to High(Theme.StatDetail.TextList) do
    AddText(Theme.StatDetail.TextList[I]);

  Count := Length(Theme.StatDetail.TextList);

  AddText(Theme.StatDetail.TextDescription);
  AddText(Theme.StatDetail.TextPage);

  LoadFromTheme(Theme.StatDetail);

  AddButton(Theme.StatDetail.ButtonNext);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Language.Translate('STAT_NEXT'));

  AddButton(Theme.StatDetail.ButtonPrev);
  if (Length(Button[1].Text)=0) then
    AddButtonText(14, 20, Language.Translate('STAT_PREV'));

  AddButton(Theme.StatDetail.ButtonReverse);
  if (Length(Button[2].Text)=0) then
    AddButtonText(14, 20, Language.Translate('STAT_REVERSE'));

  AddButton(Theme.StatDetail.ButtonExit);
  if (Length(Button[3].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

  Interaction := 0;
  Typ := TStatType(0);
end;

procedure TScreenStatDetail.onShow;
begin
  inherited;

  //Set Tot Entrys and PAges
  TotEntrys := DataBase.GetTotalEntrys(Typ);
  TotPages := Ceil(TotEntrys / Count);

  //Show correct Title
  SetTitle;

  //Show First Page
  Reversed := False;
  SetPage(0);
end;

procedure TScreenStatDetail.SetTitle;
begin
  if Reversed then
    Text[Count].Text := Theme.StatDetail.DescriptionR[Ord(Typ)]
  else
    Text[Count].Text := Theme.StatDetail.Description[Ord(Typ)];
end;

procedure TScreenStatDetail.SetPage(NewPage: Cardinal);
var
  StatList: TList;
  I: Integer;
  FormatStr: String;
  PerPage: Byte;
begin
  // fetch statistics
  StatList := Database.GetStats(Typ, Count, NewPage, Reversed);
  if ((StatList <> nil) and (StatList.Count > 0)) then
  begin
    Page := NewPage;

    // reset texts
    for I := 0 to Count-1 do
      Text[I].Text := '';

    FormatStr := Theme.StatDetail.FormatStr[Ord(Typ)];

    //refresh Texts
    for I := 0 to StatList.Count-1 do
    begin
      try
        case Typ of
          stBestScores: begin //Best Scores
            with TStatResultBestScores(StatList[I]) do
            begin
              //Set Texts
              if (Score > 0) then
              begin
                Text[I].Text := Format(FormatStr,
                  [Singer, Score, Theme.ILevel[Difficulty], SongArtist, SongTitle]);
              end;
            end;
          end;

          stBestSingers: begin //Best Singers
            with TStatResultBestSingers(StatList[I]) do
            begin
              //Set Texts
              if (AverageScore > 0) then
                Text[I].Text := Format(FormatStr, [Player, AverageScore]);
            end;
          end;

          stMostSungSong: begin //Popular Songs
            with TStatResultMostSungSong(StatList[I]) do
            begin
              //Set Texts
              if (Artist <> '') then
                Text[I].Text := Format(FormatStr, [Artist, Title, TimesSung]);
            end;
          end;

          stMostPopBand: begin //Popular Bands
            with TStatResultMostPopBand(StatList[I]) do
            begin
              //Set Texts
              if (ArtistName <> '') then
                Text[I].Text := Format(FormatStr, [ArtistName, TimesSungtot]);
            end;
          end;
        end;
      except
        on E: EConvertError do
          Log.LogError('Error Parsing FormatString in UScreenStatDetail: ' + E.Message);
      end;
    end;

    if (Page + 1 = TotPages) and (TotEntrys mod Count <> 0) then
      PerPage := (TotEntrys mod Count)
    else
      PerPage := Count;

    try      
      Text[Count+1].Text := Format(Theme.StatDetail.PageStr,
          [Page + 1, TotPages, PerPage, TotEntrys]);
    except
      on E: EConvertError do
        Log.LogError('Error Parsing FormatString in UScreenStatDetail: ' + E.Message);
    end;

    //Show correct Title
    SetTitle;
  end;

  Database.FreeStats(StatList);
end;


procedure TScreenStatDetail.SetAnimationProgress(Progress: real);
var I: Integer;
begin
  for I := 0 to High(Button) do
    Button[I].Texture.ScaleW := Progress;
end;

end.
