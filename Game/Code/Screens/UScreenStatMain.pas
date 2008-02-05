unit UScreenStatMain;

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
  UThemes;

type
  TScreenStatMain = class(TMenu)
    private
      //Some Stat Value that don't need to be calculated 2 times
      SongswithVid: Cardinal;
    public
      TextOverview:    integer;
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure SetAnimationProgress(Progress: real); override;

      procedure SetOverview;
  end;

implementation

uses UGraphic,
     UDataBase,
     USongs,
     USong,
     ULanguage,
     UCommon,
     {$IFDEF win32}
     windows,
     {$ELSE}
     sysconst,
     {$ENDIF}
     ULog;

function TScreenStatMain.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
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
          Ini.Save;
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenMain);
        end;
      SDLK_RETURN:
        begin
          //Exit Button Pressed
          if Interaction = 4 then
          begin
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenMain);
          end
          else //One of the Stats Buttons Pressed
          begin
            AudioPlayback.PlaySound(SoundLib.Back);
            ScreenStatDetail.Typ := Interaction;
            FadeTo(@ScreenStatDetail);
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

constructor TScreenStatMain.Create;
var
  I:    integer;
begin
  inherited Create;

  TextOverview := AddText(Theme.StatMain.TextOverview);

  LoadFromTheme(Theme.StatMain);

  AddButton(Theme.StatMain.ButtonScores);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.StatDetail.Description[0]);

  AddButton(Theme.StatMain.ButtonSingers);
  if (Length(Button[1].Text)=0) then
    AddButtonText(14, 20, Theme.StatDetail.Description[1]);

  AddButton(Theme.StatMain.ButtonSongs);
  if (Length(Button[2].Text)=0) then
    AddButtonText(14, 20, Theme.StatDetail.Description[2]);

  AddButton(Theme.StatMain.ButtonBands);
  if (Length(Button[3].Text)=0) then
    AddButtonText(14, 20, Theme.StatDetail.Description[3]);

  AddButton(Theme.StatMain.ButtonExit);
  if (Length(Button[4].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[4]);

  Interaction := 0;

  //Set Songs with Vid
  SongswithVid := 0;
  For I := 0 to Songs.SongList.Count -1 do
    if (TSong(Songs.SongList[I]).Video <> '') then
      Inc(SongswithVid);
end;

procedure TScreenStatMain.onShow;
begin
  //Set Overview Text:
  SetOverview;
end;

procedure TScreenStatMain.SetOverview;
type
  TwSystemTime = record
    wYear,
    wMonth,
    wDayOfWeek,
    wDay,
    wHour,
    wMinute,
    wSecond,
    wMilliseconds: Word;
  end;
var
  Overview, Formatstr: String;
  I: Integer;
  //Some Vars to Save Attributes to
  A1, A2, A3: Integer;
  A4, A5: String;
  Result1, Result2: AStatResult;
  ResetTime: TSystemTime;
  
  {$IFDEF MSWINDOWS}
  function GetFileCreation(Filename: String): TSystemTime;
  var
    FindData: TWin32FindData;
    Handle: THandle;
  begin
    Handle := FindFirstFile(PChar(Filename), FindData);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      FileTimeToSystemTime(FindData.ftCreationTime, Result);
      windows.FindClose(Handle);
    end;
  end;

  {$ELSE}
  
  function GetFileCreation(Filename: String): TSystemTime;
  Var
    F,D : Longint;
  Begin
    F:=FileCreate( Filename );
    try
      D:=FileGetDate(F);
      DateTimeToSystemTime( FileDateToDateTime(D) , result);
    finally
      FileClose(F);
    end;
  end;
  {$ENDIF}
  
begin
  //Song Overview

  //Introduction
  Formatstr := Language.Translate ('STAT_OVERVIEW_INTRO');
  (*Format:
    %0:d Ultrastar Version
    %1:d Day of Reset (A1)
    %2:d Month of Reset (A2)
    %3:d Year of Reset (A3)*)

  ResetTime := GetFileCreation(Database.Filename);

  {$IFDEF MSWINDOWS}
    A1 := ResetTime.wDay;
    A2 := ResetTime.wMonth;
    A3 := ResetTime.wYear;
  {$ELSE}
    A1 := ResetTime.Day;
    A2 := ResetTime.Month;
    A3 := ResetTime.Year;
  {$ENDIF}
  
  
  try
    Overview := Format(Formatstr, [Language.Translate('US_VERSION'), A1, A2, A3]);
  except
    on E: EConvertError do
      Log.LogError('Error Parsing FormatString "STAT_OVERVIEW_INTRO": ' + E.Message);
  end;

  Formatstr := Language.Translate ('STAT_OVERVIEW_SONG');
  {Format:
    %0:d Count Songs (A1)
    %1:d Count of Sung Songs (A2)
    %2:d Count of UnSung Songs
    %3:d Count of Songs with Video (A3)
    %4:s Name of the most popular Song}
  A1 := Songs.SongList.Count;
  A2 := Database.GetTotalEntrys(2);

  A3 := SongswithVid;

  SetLength(Result1, 1);
  Database.GetStats(Result1, 2, 1, 0, False);
  A4 := Result1[0].Artist;
  A5 := Result1[0].Title;

  try
    Overview := Overview + '\n \n' + Format(Formatstr, [A1, A2, A1-A2, A3, A4, A5]);
  except
    on E: EConvertError do
      Log.LogError('Error Parsing FormatString "STAT_OVERVIEW_SONG": ' + E.Message);
  end;

  //Player Overview
  Formatstr := Language.Translate ('STAT_OVERVIEW_PLAYER');
  {Format:
    %0:d Count Players (A1)
    %1:s Best Player (Result)
    %2:d Best Players Score
    %3:s Best Score Player (Result2)
    %4:d Best Score}
  A1 := Database.GetTotalEntrys(1);

  SetLength(Result1, 1);
  Database.GetStats(Result1, 1, 1, 0, False);

  SetLength(Result2, 1);
  Database.GetStats(Result2, 0, 1, 0, False);

  try
    Overview := Overview + '\n \n' + Format(Formatstr, [A1, Result1[0].Player, Result1[0].AverageScore, Result2[0].Singer, Result2[0].Score]);
  except
    on E: EConvertError do
      Log.LogError('Error Parsing FormatString "STAT_OVERVIEW_PLAYER": ' + E.Message);
  end;

  Text[0].Text := Overview;
end;


procedure TScreenStatMain.SetAnimationProgress(Progress: real);
var I: Integer;
begin
  For I := 0 to high(Button) do
    Button[I].Texture.ScaleW := Progress;
end;

end.
