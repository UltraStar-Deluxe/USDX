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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenPopup.pas $
 * $Id: UScreenPopup.pas 1939 2009-11-09 00:27:55Z s_alexander $
 *}

unit UScreenPopup;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  sdl2,
  SysUtils,
  UDataBase,
  UDLLManager,
  ULog,
  UMenu,
  UMusic,
  md5,
  USkins,
  USongs,
  UScreenSong,
  UNote,
  UFiles,
  UTexture,
  UThemes,
  UWebSDK,
  UHelp,
  TextGL,
  Classes;

type
  TPopupCheckHandler = procedure(Value: boolean; Data: Pointer);

  TScreenPopupCheck = class(TMenu)
    private
      fHandler: TPopupCheckHandler;
      fHandlerData: Pointer;

    public
      Visible: boolean; // whether the menu should be drawn

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure ShowPopup(const Msg: UTF8String; Handler: TPopupCheckHandler;
          HandlerData: Pointer; DefaultValue: boolean = false);
      function Draw: boolean; override;
  end;

type
  TPopupInsertUserHandler = procedure(Value: boolean; Data: Pointer);

  TScreenPopupInsertUser = class(TMenu)
    private
      fHandler: TPopupInsertUserHandler;
      fHandlerData: Pointer;

    public
      Visible: boolean; // whether the menu should be drawn
      Username: UTF8String;
      Password: UTF8String;
      InteractionTmp: integer;

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure ShowPopup(const Title: UTF8String; Msg: UTF8String; Handler: TPopupInsertUserHandler;
          HandlerData: Pointer);
      function Draw: boolean; override;
  end;

type
  TPopupSendScoreHandler = procedure(Value: integer; Data: Pointer);

  TScreenPopupSendScore = class(TMenu)
    private
      fHandler: TPopupSendScoreHandler;
      fHandlerData: Pointer;

      TColorR: real;
      TColorG: real;
      TColorB: real;

      TDColorR: real;
      TDColorG: real;
      TDColorB: real;

    public
      Visible: boolean; // whether the menu should be drawn
      IWebsite:      array of UTF8String;
      IUsername:     array of UTF8String;
      IPlayersPlay:  array of UTF8String;
      Password: UTF8String;
      Username: UTF8String;

      SelectValueP: integer;
      SelectValueW: integer;
      SelectValueU: integer;

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure ShowPopup(const Title: UTF8String; Handler: TPopupSendScoreHandler;
          HandlerData: Pointer);
      function Draw: boolean; override;
  end;

type
  TScreenPopup = class(TMenu)
    {
    private
      CurMenu: byte; //Num of the cur. Shown Menu
    }
    public
      Visible: boolean; //Whether the Menu should be Drawn

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure OnHide; override;
      procedure ShowPopup(const Msg: UTF8String);
      function Draw: boolean; override;
  end;

  TScreenPopupError = class(TScreenPopup)
    public
      constructor Create; override;
  end;

  TScreenPopupInfo = class(TScreenPopup)
    public
      constructor Create; override;
  end;

type
  TScreenPopupScoreDownload = class(TMenu)
    public
      Visible: boolean; // whether the menu should be drawn
      Actual_Song: integer;
      Actual_Web: integer;
      Index_Song: integer;
      Num_Songs: integer;
      Num_Webs: integer;
      CountSongsUpdate: integer;

      OpScoreFile: boolean;
      ScoreFile: TextFile;

      Download_Phase: integer;

      Text_SongSituation: UTF8String;
      Text_WebSituation: UTF8String;

      Texture_ProgressBar: TTexture;

      List_MD5Song: widestring;
      Receive_List: array[0..2] of widestring;
      Actual_Level: integer;
      Position_Receive_List: array[0..2] of integer;

      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure ShowPopup(optmode: integer; optsong: integer; optweb: integer);
      procedure DownloadTimeBarSong();
      procedure DownloadTimeBarWeb();
      function Draw: boolean; override;
      procedure ReadMD5FileSong();
      procedure ReceiveListScore();
      procedure SaveScoreSong();
      procedure FileSaveScoreSong();
      procedure LogSongUpdate(Artist, Title, WebName: UTF8String);
      procedure OpenFile();
  end;

type
  TRect = record
    left, right, top, bottom: integer;
  end;

  TLine = record
    fX, fY, tX, tY: integer;
  end;

  TText = record
    X, Y:   integer;
    Font:   integer;
    Style:  integer;
    Size:   real;
    Italic: boolean;
    text:   string;
  end;

  TResLine = record
    Y:      integer;
    H:      integer;
    lines:  array of TLine;
    texts:  array of TText;
  end;

  TScreenPopupHelp = class(TMenu)
  private
    TextsGFX:   array of TResLine;
    msg:        TTextResult;
    Rect:       TRect;

    max_high:   real;
    step:       double;
    barH:       double;

    procedure   DrawTable;
    procedure   DrawLine(line, index, Y: integer);
    procedure   DrawText(line, index, Y: integer);
    procedure   DrawScroll(X, Y, W, H: integer; pos, len: double);
  public
    Visible:    Boolean; //Whether the Menu should be Drawn

    constructor Create; override;
    function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
    procedure onShow; override;
    procedure onHide; override;
    procedure ShowPopup();
    function Draw: boolean; override;
end;

var
  //ISelections: array of string;
  SelectValue: integer;

implementation

uses
  dglOpenGL,
  UGraphic,
  UMain,
  UIni,
  ULanguage,
  UParty,
  USong,
  UPlaylist,
  UDisplay,
  UPathUtils,
  UUnicodeUtils;

{ TScreenPopupCheck }

function TScreenPopupCheck.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  Value: boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Value := false;
          Visible := false;
          Result := false;
        end;

      SDLK_RETURN:
        begin
          Value := (Interaction = 0);
          Visible := false;
          Result := false;
        end;

      SDLK_DOWN:  InteractNext;
      SDLK_UP:    InteractPrev;

      SDLK_RIGHT: InteractNext;
      SDLK_LEFT:  InteractPrev;
    end;
  end;

  if (not Result) then
  begin
    if (@fHandler <> nil) then
      fHandler(Value, fHandlerData);
  end;
end;

constructor TScreenPopupCheck.Create;
begin
  inherited Create;

  fHandler := nil;
  fHandlerData := nil;

  AddText(Theme.CheckPopup.TextCheck);
  
  LoadFromTheme(Theme.CheckPopup);

  AddButton(Theme.CheckPopup.Button1);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, 'Button 1');

  AddButton(Theme.CheckPopup.Button2);
  if (Length(Button[1].Text) = 0) then
    AddButtonText(14, 20, 'Button 2');

  Interaction := 0;
end;

function TScreenPopupCheck.Draw: boolean;
begin
  Result := inherited Draw;
end;

procedure TScreenPopupCheck.OnShow;
begin
  inherited;
end;

procedure TScreenPopupCheck.ShowPopup(const Msg: UTF8String; Handler: TPopupCheckHandler;
    HandlerData: Pointer; DefaultValue: boolean);
begin
  if (DefaultValue) then
    Interaction := 0
  else
    Interaction := 1;
  Visible := true;  //Set Visible
  fHandler := Handler;
  fHandlerData := HandlerData;

  Text[0].Text := Language.Translate(msg);

  Button[0].Visible := true;
  Button[1].Visible := true;

  Button[0].Text[0].Text := Language.Translate('SONG_MENU_YES');
  Button[1].Text[0].Text := Language.Translate('SONG_MENU_NO');

  Background.OnShow
end;

{ TScreenPopupInsertUser }

function TScreenPopupInsertUser.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  Value: boolean;
  I: integer;
  Password_TMP: UTF8String;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    if (IsPrintableChar(CharCode)) then
    begin
      if (Interaction = 0) or (Interaction = 1) then
      begin
        if (Interaction = 0) then
        begin
          Button[0].Text[0].Text := Button[0].Text[0].Text + UCS4ToUTF8String(CharCode);
          Username := Username + UCS4ToUTF8String(CharCode);
        end
        else
        begin
          Password := Password + UCS4ToUTF8String(CharCode);
          Button[1].Text[0].Text := Button[1].Text[0].Text + '*';
        end;
      end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE:
        begin
          Value := false;
          Visible := false;
          Result := false;
        end;

      SDLK_BACKSPACE :
      begin
        if (Interaction = 0) or (Interaction = 1) then
        begin
          Button[Interaction].Text[0].DeleteLastLetter();

          if (Interaction = 0) then
            Username := Button[Interaction].Text[0].Text;

          if (Interaction = 1) then
          begin
            Password_TMP := '';
            for I := 1 to High(Password) do
              Password_TMP := Password_TMP + Password[I];
            Password := Password_TMP;
          end;
        end;
      end;

      SDLK_RETURN:
        begin
          InteractionTmp := Interaction;

          if (Interaction <> 3) then
            Interaction := 2;

          Value := (Interaction = 2);
          if (Interaction = 3) then
            Visible := false;
          Result := false;
        end;

      SDLK_TAB:   InteractNext;

      SDLK_DOWN:  InteractNext;
      SDLK_UP:    InteractPrev;

      SDLK_RIGHT: InteractNext;
      SDLK_LEFT:  InteractPrev;
    end;
  end;

  if (not Result) then
  begin
    if (@fHandler <> nil) then
      fHandler(Value, fHandlerData);
  end;
end;

constructor TScreenPopupInsertUser.Create;
begin
  inherited Create;

  fHandler := nil;
  fHandlerData := nil;

  AddText(Theme.InsertUserPopup.TextInsertUser);

  LoadFromTheme(Theme.InsertUserPopup);

  AddButton(Theme.InsertUserPopup.ButtonUsername);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, '');

  AddButton(Theme.InsertUserPopup.ButtonPassword);
  if (Length(Button[1].Text) = 0) then
    AddButtonText(14, 20, '');

  AddButton(Theme.InsertUserPopup.Button1);
  if (Length(Button[2].Text) = 0) then
    AddButtonText(14, 20, 'Add');

  AddButton(Theme.InsertUserPopup.Button2);
  if (Length(Button[3].Text) = 0) then
    AddButtonText(14, 20, 'Cancel');

  Button[0].Text[0].Writable := true;
  Button[1].Text[0].Writable := true;

  Interaction := 0;
end;

function TScreenPopupInsertUser.Draw: boolean;
begin
  Result := inherited Draw;
end;

procedure TScreenPopupInsertUser.OnShow;
begin
  inherited;
end;

procedure TScreenPopupInsertUser.ShowPopup(const Title: UTF8String; Msg: UTF8String; Handler: TPopupInsertUserHandler;
    HandlerData: Pointer);
begin

  Visible := true;  //Set Visible
  fHandler := Handler;
  fHandlerData := HandlerData;

  Text[0].Text := Language.Translate(Msg);
  Text[1].Text := Title;

  Button[0].Visible := true;
  Button[1].Visible := true;
  Button[2].Visible := true;
  Button[3].Visible := true;

  Password := '';
  Username := '';

  Button[0].Text[0].Text := '';
  Button[1].Text[0].Text := '';
  Button[2].Text[0].Text := Language.Translate('MSG_INSERT_USER_ADD');
  Button[3].Text[0].Text := Language.Translate('MSG_INSERT_USER_CANCEL');

  Interaction := 0;

  Background.OnShow
end;

{ TScreenPopupSendScore }

function TScreenPopupSendScore.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  New_User: boolean;
  I, Value: integer;
  Password_TMP: UTF8String;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    if (IsPrintableChar(CharCode)) then
    begin
      if (Interaction = 3) or (Interaction = 4) then
      begin
        if (Interaction = 3) then
        begin
          Button[0].Text[0].Text := Button[0].Text[0].Text + UCS4ToUTF8String(CharCode);
          Username := Username + UCS4ToUTF8String(CharCode);
        end;

        if (Interaction = 4) then
        begin
          Button[1].Text[0].Text := Button[1].Text[0].Text + '*';
          Password := Password + UCS4ToUTF8String(CharCode);
        end;
      end
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE:
        begin
          Value := 0;
          Visible := false;
          Result := false;
        end;

      SDLK_BACKSPACE:
        begin
          if (Interaction = 3) or (Interaction = 4) then
          begin
            Button[Interaction - 3].Text[0].DeleteLastLetter;

            if (Interaction = 3) then
              Username := Button[Interaction - 3].Text[0].Text;

            if (Interaction = 4) then
            begin
              Password_TMP := '';
              for I := 1 to High(Password) do
                Password_TMP := Password_TMP + Password[I];
              Password := Password_TMP;
            end;

          end
          else
          begin
            Value := 0;
            Visible := false;
            Result := false;
          end;
        end;

      SDLK_RETURN:
        begin

          if (Interaction = 5) then
          begin
            Value := 1;
            Visible := false;
          end;

          if (Interaction = 6) then
          begin
            Value := 2;
            Visible := false;
          end;

          Result := false;
        end;

      SDLK_DOWN: InteractNext;
      SDLK_UP:   InteractPrev;

      SDLK_TAB:  InteractNext;

      SDLK_RIGHT:
        begin
          if (Interaction < 3) then
            InteractInc;

          if (Interaction = 1)  and (SelectValueW <= High(IWebsite)) then
          begin
            New_User := false;

            if (SelectValueU <> High(IUsername)) then
              SelectValueU := 0
            else
              New_User := true;

            SetLength(IUsername, Length(DataBase.NetworkUser[SelectValueW].UserList));
            for I := 0 to High(DataBase.NetworkUser[SelectValueW].UserList) do
              IUsername[I] := DataBase.NetworkUser[SelectValueW].UserList[I].Username;
            SetLength(IUsername, Length(IUsername) + 1);
            IUsername[High(IUsername)] := Language.Translate('SCORE_SEND_OTHER_USER');


            if (New_User = true) then
              SelectValueU := High(IUsername);

            UpdateSelectSlideOptions(Theme.SendScorePopup.SelectSlide3, 2, IUsername, SelectValueU);
          end;

          if (SelectValueU = High(IUsername)) then
          begin
            Button[0].Text[1].ColR := TColorR;
            Button[0].Text[1].ColG := TColorG;
            Button[0].Text[1].ColB := TColorB;

            Button[1].Text[1].ColR := TColorR;
            Button[1].Text[1].ColG := TColorG;
            Button[1].Text[1].ColB := TColorB;

            Button[0].Selectable := true;
            Button[1].Selectable := true;
          end
          else
          begin
            Button[0].Text[1].ColR := TDColorR;
            Button[0].Text[1].ColG := TDColorG;
            Button[0].Text[1].ColB := TDColorB;

            Button[1].Text[1].ColR := TDColorR;
            Button[1].Text[1].ColG := TDColorG;
            Button[1].Text[1].ColB := TDColorB;

            Button[0].Selectable := false;
            Button[1].Selectable := false;
          end;

        end;
      SDLK_LEFT:
        begin
          if (Interaction < 3) then
            InteractDec;

          if (Interaction = 1) then
          begin
            New_User := false;

            SetLength(IUsername, Length(DataBase.NetworkUser[SelectValueW].UserList));

            for I := 0 to High(DataBase.NetworkUser[SelectValueW].UserList) do
              IUsername[I] := DataBase.NetworkUser[SelectValueW].UserList[I].Username;

            SetLength(IUsername, Length(IUsername) + 1);
            IUsername[High(IUsername)] := Language.Translate('SCORE_SEND_OTHER_USER');

            if ((SelectValueU <> High(IUsername)) and (High(IUsername) <> 0)) then
              SelectValueU := 0
            else
              New_User := true;

            if (New_User = true) then
              SelectValueU := High(IUsername);

            UpdateSelectSlideOptions(Theme.SendScorePopup.SelectSlide3, 2, IUsername, SelectValueU);
          end;

          if (SelectValueU = High(IUsername)) then
          begin
            Button[0].Text[1].ColR := TColorR;
            Button[0].Text[1].ColG := TColorG;
            Button[0].Text[1].ColB := TColorB;

            Button[1].Text[1].ColR := TColorR;
            Button[1].Text[1].ColG := TColorG;
            Button[1].Text[1].ColB := TColorB;

            Button[0].Selectable := true;
            Button[1].Selectable := true;
          end
          else
          begin
            Button[0].Text[1].ColR := TDColorR;
            Button[0].Text[1].ColG := TDColorG;
            Button[0].Text[1].ColB := TDColorB;

            Button[1].Text[1].ColR := TDColorR;
            Button[1].Text[1].ColG := TDColorG;
            Button[1].Text[1].ColB := TDColorB;

            Button[0].Selectable := false;
            Button[1].Selectable := false;
          end;

        end;

    end;
  end;

  if (not Result) then
  begin
    if (@fHandler <> nil) then
      fHandler(Value, fHandlerData);
  end;

end;

constructor TScreenPopupSendScore.Create;
var
  I: integer;
begin
  inherited Create;

  fHandler := nil;
  fHandlerData := nil;
  SelectValueP := 0;
  SelectValueW := 0;
  SelectValueU := 0;

  LoadFromTheme(Theme.SendScorePopup);

  SetLength(IWebsite, 0);
  for I := 0 to High(DataBase.NetworkUser) do
  begin
    SetLength(IWebsite, Length(IWebsite) + 1);
    IWebsite[High(IWebsite)] := DataBase.NetworkUser[I].Website;
  end;

  AddSelectSlide(Theme.SendScorePopup.SelectSlide1, SelectValueP, IPlayersPlay);
  AddSelectSlide(Theme.SendScorePopup.SelectSlide2, SelectValueW, IWebsite);
  AddSelectSlide(Theme.SendScorePopup.SelectSlide3, SelectValueU, IUsername);

  TColorR := Theme.SendScorePopup.ButtonUsername.ColR;
  TColorG := Theme.SendScorePopup.ButtonUsername.ColG;
  TColorB := Theme.SendScorePopup.ButtonUsername.ColB;

  TDColorR := Theme.SendScorePopup.ButtonUsername.DColR;
  TDColorG := Theme.SendScorePopup.ButtonUsername.DColG;
  TDColorB := Theme.SendScorePopup.ButtonUsername.DColB;

  AddButton(Theme.SendScorePopup.ButtonUsername);
  AddButton(Theme.SendScorePopup.ButtonPassword);

  AddButton(Theme.SendScorePopup.Button1);
  AddButton(Theme.SendScorePopup.Button2);

  Interaction := 0;
end;

function TScreenPopupSendScore.Draw: boolean;
begin
  Result := inherited Draw;
end;

procedure TScreenPopupSendScore.OnShow;
begin
  inherited;
end;

procedure TScreenPopupSendScore.ShowPopup(const Title: UTF8String; Handler: TPopupSendScoreHandler;
    HandlerData: Pointer);
var
  I: integer;
begin

  Visible := true;  //Set Visible
  fHandler := Handler;
  fHandlerData := HandlerData;
  Password := '';
  Username := '';

  SelectValueP := 0;
  SelectValueW := 0;
  SelectValueU := 0;

  Interaction := 0;

  Text[0].Text := Language.Translate(Title);

  for I := 0 to 3 do
    Button[I].Visible := true;

  Button[0].Text[0].Text := '';
  Button[1].Text[0].Text := '';

  Button[2].Text[0].Text := Language.Translate('SCORE_SEND');
  Button[3].Text[0].Text := Language.Translate('SCORE_SAVE');

  SetLength(IPlayersPlay, PlayersPlay);

  for I := 0 to PlayersPlay - 1 do
    IPlayersPlay[I] := Ini.Name[I];

  UpdateSelectSlideOptions(Theme.SendScorePopup.SelectSlide1, 0, IPlayersPlay, SelectValueP);

  //UpdateSelectSlideOptions(Theme.SendScorePopup.SelectSlide2, 1, IWebsite, SelectValueW);

  SetLength(IUsername, Length(DataBase.NetworkUser[SelectValueW].UserList));

  for I := 0 to High(DataBase.NetworkUser[SelectValueW].UserList) do
    IUsername[I] := DataBase.NetworkUser[SelectValueW].UserList[I].Username;

  SetLength(IUsername, Length(IUsername) + 1);
  IUsername[High(IUsername)] := Language.Translate('SCORE_SEND_OTHER_USER');

  UpdateSelectSlideOptions(Theme.SendScorePopup.SelectSlide3, 2, IUsername, SelectValueU);

  if (SelectValueU = High(IUsername)) then
  begin
    Button[0].Text[1].ColR := TColorR;
    Button[0].Text[1].ColG := TColorG;
    Button[0].Text[1].ColB := TColorB;

    Button[1].Text[1].ColR := TColorR;
    Button[1].Text[1].ColG := TColorG;
    Button[1].Text[1].ColB := TColorB;

    Button[0].Selectable := true;
    Button[1].Selectable := true;
  end
  else
  begin
    Button[0].Selectable := false;
    Button[1].Selectable := false;

    Button[0].Text[1].ColR := TDColorR;
    Button[0].Text[1].ColG := TDColorG;
    Button[0].Text[1].ColB := TDColorB;

    Button[1].Text[1].ColR := TDColorR;
    Button[1].Text[1].ColG := TDColorG;
    Button[1].Text[1].ColB := TDColorB;
  end;

  SelectsS[0].Visible := true;
  SelectsS[1].Visible := true;
  SelectsS[2].Visible := true;

  Interaction := 0;

  Background.OnShow
end;

{ TScreenPopupScoreDownload }

function TScreenPopupScoreDownload.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  Value: boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Value := false;
          Visible := false;
          Result := false;
          Text[0].Text := Language.Translate('SCORE_DOWNLOAD_RECEIVE_LIST');
          Text[1].Text := '';
        end;

      SDLK_RETURN:
        begin
          Value := (Interaction = 0);
          if (Interaction = 0) then
          begin
            Visible := false;
            Result := false;
            Text[0].Text := Language.Translate('SCORE_DOWNLOAD_RECEIVE_LIST');
            Text[1].Text := '';
          end;
        end;

      SDLK_DOWN:  Interaction := -1;
      SDLK_UP:    Interaction := -1;

      SDLK_RIGHT: InteractNext;
      SDLK_LEFT:  InteractPrev;
    end;
  end;

end;

constructor TScreenPopupScoreDownload.Create;
begin
  inherited Create;

  Texture_ProgressBar := Texture.LoadTexture(Skin.GetTextureFileName('ProgressBar'));

  Theme.ScoreDownloadPopup.TextSongScoreDownload.Text := Language.Translate('SCORE_DOWNLOAD_RECEIVE_LIST');
  Theme.ScoreDownloadPopup.TextWebScoreDownload.Text := '';

  AddText(Theme.ScoreDownloadPopup.TextSongScoreDownload);
  AddText(Theme.ScoreDownloadPopup.TextWebScoreDownload);

  LoadFromTheme(Theme.ScoreDownloadPopup);

  AddButton(Theme.ScoreDownloadPopup.Button1);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, 'Button 1');

  Interaction := 0;
end;

procedure TScreenPopupScoreDownload.LogSongUpdate(Artist, Title, WebName: UTF8String);
var
  UpdateFile: TextFile;
begin
  AssignFile(UpdateFile, WebScoresPath.Append(WebName + ' [Song Update].txt').ToNative);

  if FileExists(WebScoresPath.Append(WebName + ' [Song Update].txt').ToNative) then
    Append(UpdateFile)
  else
    Rewrite(UpdateFile);

  WriteLn(UpdateFile, Artist + ' - ' + Title);

  Flush(UpdateFile);
  Close(UpdateFile);
end;

procedure TScreenPopupScoreDownload.SaveScoreSong();
var
  String_Text, User_Score, Max_Score, Media_Score: string;
  I, J, Update: integer;
  DeleteSongLevel: array [0..2] of boolean;
begin

  if not(CatSongs.Song[Index_Song].Main) then
  begin
    Actual_Song := Actual_Song + 1;
    Text_SongSituation := Language.Translate('SCORE_DOWNLOAD_SONG') + ' ' + IntToStr(Actual_Song) + '/' + IntToStr(Num_Songs);
    Text_WebSituation := IntToStr(Actual_Web) + '/' + IntToStr(Num_Webs);

    for J := 0 to 2 do //for each difficulty level
    begin

      if (Position_Receive_List[J] <= Length(Receive_List[J])) then
      begin

        String_Text := '';

        while (Receive_List[J][Position_Receive_List[J]] <> #10) and (Position_Receive_List[J] <= Length(Receive_List[J])) do
        begin
          String_Text := String_Text + Receive_List[J][Position_Receive_List[J]];
          if((Position_Receive_List[J] < Length(Receive_List[J]))) then
          begin
            Position_Receive_List[J] := Position_Receive_List[J] + 1;
          end
          else
          begin
            Break;
          end;
        end;

        // E -> Error song no exist in web
        if (String_Text <> 'ERROR') then
        begin
          DeleteSongLevel[J] := false;

          DataBase.AddSong(CatSongs.Song[Index_Song]);

          Max_Score := Copy(String_Text, 0, 5);
          Media_Score := Copy(String_Text, 6, 5);
          User_Score := Copy(String_Text, 11, Length(String_Text) - 10);

          DataBase.AddMax_Score(CatSongs.Song[Index_Song], DllMan.Websites[Actual_Web - 1].ID, StrToInt(Max_Score), J);
          DataBase.AddMedia_Score(CatSongs.Song[Index_Song], DllMan.Websites[Actual_Web - 1].ID, StrToInt(Media_Score), J);
          DataBase.AddUser_Score(CatSongs.Song[Index_Song], DllMan.Websites[Actual_Web - 1].ID, User_Score, J);
        end
        else
          DeleteSongLevel[J] := true;

        Position_Receive_List[J] := Position_Receive_List[J] + 1;
      end;
    end;

    if (DeleteSongLevel[0]) and (DeleteSongLevel[1]) and (DeleteSongLevel[2]) then
    begin
      Update := DataBase.Delete_Score(CatSongs.Song[Index_Song], DllMan.Websites[Actual_Web - 1].ID);

      if (Update <> 0) then
      begin
        LogSongUpdate(CatSongs.Song[Index_Song].Artist, CatSongs.Song[Index_Song].Title, DllMan.Websites[Actual_Web - 1].Name);
        CountSongsUpdate := CountSongsUpdate + 1;
      end;
    end;

  end;

  Index_Song := Index_Song + 1;

end;

procedure TScreenPopupScoreDownload.FileSaveScoreSong();
var
  String_Text, User_Score, Max_Score, Media_Score, MD5_Song: string;
  Level: byte;
  Update: integer;
  SongExist: boolean;
begin

  if not(CatSongs.Song[Index_Song].Main) then
  begin
    Actual_Song := Actual_Song + 1;
    Text_SongSituation := Language.Translate('SCORE_DOWNLOAD_SONG') + ' ' + IntToStr(Actual_Song) + '/' + IntToStr(Num_Songs);
    Text_WebSituation := IntToStr(Actual_Web) + '/' + IntToStr(Num_Webs);

    SongExist := false;

    while not (EOF(ScoreFile)) do
    begin
      ReadLn(ScoreFile, String_Text);

      MD5_Song := Copy(String_Text, 0, 32);

      if (CatSongs.Song[Index_Song].MD5 = MD5_Song) then
      begin
        SongExist := true;
        DataBase.AddSong(CatSongs.Song[Index_Song]);

        Level := StrToInt(Copy(String_Text, 33, 1)) - 1;
        Max_Score := Copy(String_Text, 34, 5);
        Media_Score := Copy(String_Text, 39, 5);
        User_Score := Copy(String_Text, 44, Length(String_Text) - 43);

        DataBase.AddMax_Score(CatSongs.Song[Index_Song], DllMan.Websites[Actual_Web - 1].ID, StrToInt(Max_Score), Level);
        DataBase.AddMedia_Score(CatSongs.Song[Index_Song], DllMan.Websites[Actual_Web - 1].ID, StrToInt(Media_Score), Level);
        DataBase.AddUser_Score(CatSongs.Song[Index_Song], DllMan.Websites[Actual_Web - 1].ID, User_Score, Level);
      end;
    end;

    if not(SongExist) then
    begin
      Update := DataBase.Delete_Score(CatSongs.Song[Index_Song], DllMan.Websites[Actual_Web - 1].ID);

      if (Update <> 0) then
      begin
        LogSongUpdate(CatSongs.Song[Index_Song].Artist, CatSongs.Song[Index_Song].Title, DllMan.Websites[Actual_Web - 1].Name);
        CountSongsUpdate := CountSongsUpdate + 1;
      end;
    end;

    Reset(ScoreFile);
  end;

  Index_Song := Index_Song + 1;

end;

procedure TScreenPopupScoreDownload.ReadMD5FileSong();
var
  I: integer;
begin

  if (Num_Songs = 1) then
  begin
    Index_Song := ScreenSong.Interaction;
    List_MD5Song := CatSongs.Song[ScreenSong.Interaction].MD5;
  end
  else
  begin
    for I := 0 to High(CatSongs.Song) do
    begin
      if not (CatSongs.Song[I].Main) then
        List_MD5Song := List_MD5Song + CatSongs.Song[I].MD5;
    end;
  end;

  ScreenPopupScoreDownload.Download_Phase := 1;
end;

procedure TScreenPopupScoreDownload.ReceiveListScore();
begin

  Text_WebSituation := IntToStr(Actual_Web) + '/' + IntToStr(Num_Webs);
  Text_SongSituation := Language.Translate('SCORE_DOWNLOAD_RECEIVE_LIST');

  Receive_List[Actual_Level] := '';
  Receive_List[Actual_Level] := DllMan.WebsiteDownloadScore(List_MD5Song, Actual_Level + 1);

  if (Receive_List[Actual_Level] = '0') then
  begin
    ScreenPopupError.ShowPopup(Format(Language.Translate('SONG_MENU_REFRESH_SCORES_ERROR_CONNECTION'), [UTF8Encode(DllMan.Websites[Actual_Web -1].Name)]));
    Text_SongSituation := Language.Translate('WEBSITE_NO_CONNECTION');
    Actual_Song := Num_Songs;
    Actual_Level := 2;

    if (Num_Webs = 1) then
      Visible := false
  end;

  Actual_Level := Actual_Level + 1;

end;

procedure TScreenPopupScoreDownload.OpenFile;
var
  Filename: string;
begin

  Filename := WebScoresPath.Append(DataBase.NetworkUser[Actual_Web - 1].Website + ' [Download Scores].txt').ToNative;

  if (FileExists(Filename)) then
  begin
    AssignFile(ScoreFile, Filename);
    Reset(ScoreFile);
  end
  else
  begin
    ScreenPopupError.ShowPopup(Format(Language.Translate('SONG_MENU_REFRESH_SCORES_ERROR_FILE'), [UTF8Encode(DllMan.Websites[Actual_Web -1].Name)]));

    Actual_Song := Num_Songs;
    Actual_Level := 2;

    if (Num_Webs = 1) then
      Visible := false
  end;

end;

function TScreenPopupScoreDownload.Draw: boolean;
var
  I: integer;
begin
  inherited Draw;

  Text[0].Text := Text_SongSituation;
  Text[1].Text := Text_WebSituation;

  DownloadTimeBarSong();

  if (Num_Webs > 1) then
    DownloadTimeBarWeb();

  if (Download_Phase = 0) then
  begin

    if (OpScoreFile) then
    begin
      OpenFile;
      Download_Phase := 1;
    end
    else
      ReadMD5FileSong();

  end
  else
  begin
    // ONLINE DOWNLOAD SCORE
    if not (OpScoreFile) then
    begin
      if (Download_Phase = 1) then
      begin
        if (Actual_Level <= 2) then
        begin
          DLLMan.LoadWebsite(Actual_Web - 1);
          ReceiveListScore();
        end
        else
          Download_Phase := 2;
      end
      else
      begin
        if (Actual_Song < Num_Songs) then
        begin
          SaveScoreSong();
        end
        else
        begin
          if ((Actual_Web < Num_Webs) and (Num_Webs > 1)) then
          begin
            for I:= 0 to 2 do
            begin
              Receive_List[I] := '';
              Position_Receive_List[I] := 1;
            end;

            Download_Phase := 1;
            Actual_Song := 0;
            Index_Song := 0;
            Actual_Level := 0;
            Actual_Web := Actual_Web + 1;
          end
          else
          begin
            Button[0].Text[0].Text := Language.Translate('SCORE_DOWNLOAD_OK');
            if (CountSongsUpdate > 0) then
            begin
              Visible := false;
              ScreenPopupInfo.ShowPopup(Format(Language.Translate('SCORE_DOWNLOAD_SONG_UPDATE'), [CountSongsUpdate]));
            end
            else
              Interaction := 0;
          end;
        end;
      end;
    end
    else
    begin
      // FILE DOWNLOAD
      if (Actual_Song < Num_Songs) then
      begin
        FileSaveScoreSong();
      end
      else
      begin
        if ((Actual_Web < Num_Webs) and (Num_Webs > 1)) then
        begin
          //Download_Phase := 1;
          Actual_Song := 0;
          Index_Song := 0;
          Actual_Level := 0;
          Actual_Web := Actual_Web + 1;
          OpenFile;
        end
        else
        begin
          Button[0].Text[0].Text := Language.Translate('SCORE_DOWNLOAD_OK');

          if (CountSongsUpdate > 0) then
          begin
            Visible := false;
            ScreenPopupInfo.ShowPopup(Format(Language.Translate('SCORE_DOWNLOAD_SONG_UPDATE'), [CountSongsUpdate]));
          end
          else
            Interaction := 0;
        end;

      end;
    end;
  end;

  Result := true;

end;

procedure TScreenPopupScoreDownload.OnShow;
begin
  inherited;
end;

procedure TScreenPopupScoreDownload.DownloadTimeBarSong();
var
  x, y:           real;
  width, height:  real;
  Progress:       real;
  CurProgress:    real;
begin
  x := Theme.ScoreDownloadPopup.DownloadProgressSong.X;
  y := Theme.ScoreDownloadPopup.DownloadProgressSong.Y;

  width  := Theme.ScoreDownloadPopup.DownloadProgressSong.W;
  height := Theme.ScoreDownloadPopup.DownloadProgressSong.H;

  glColor4f(Theme.ScoreDownloadPopup.DownloadProgressSong.ColR, Theme.ScoreDownloadPopup.DownloadProgressSong.ColG, Theme.ScoreDownloadPopup.DownloadProgressSong.ColB, 1); //Set Color

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);

  glBindTexture(GL_TEXTURE_2D,  Texture_ProgressBar.TexNum);

  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex2f(x, y);

    CurProgress := Actual_Song;
    if (CurProgress > 0) then
    begin
      Progress := CurProgress / Num_Songs;
      glTexCoord2f((width * Progress) / 8, 0);
      glVertex2f(x + width * Progress, y);

      glTexCoord2f((width * Progress) / 8, 1);
      glVertex2f(x + width * Progress, y + height);
    end;

    glTexCoord2f(0, 1);
    glVertex2f(x, y + height);
  glEnd;

 glDisable(GL_TEXTURE_2D);
 glDisable(GL_BLEND);
 glcolor4f(1, 0, 0, 1);

end;

procedure TScreenPopupScoreDownload.DownloadTimeBarWeb();
var
  x, y:           real;
  width, height:  real;
  Progress:       real;
  CurProgress:    real;
begin
  x := Theme.ScoreDownloadPopup.DownloadProgressWeb.X;
  y := Theme.ScoreDownloadPopup.DownloadProgressWeb.Y;

  width  := Theme.ScoreDownloadPopup.DownloadProgressWeb.W;
  height := Theme.ScoreDownloadPopup.DownloadProgressWeb.H;

  glColor4f(Theme.ScoreDownloadPopup.DownloadProgressWeb.ColR, Theme.ScoreDownloadPopup.DownloadProgressWeb.ColG, Theme.ScoreDownloadPopup.DownloadProgressWeb.ColB, 1); //Set Color

  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);

  glBindTexture(GL_TEXTURE_2D, Texture_ProgressBar.TexNum);

  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex2f(x, y);

    CurProgress := Actual_Song + ((Actual_Web - 1) * Num_Songs);
    if (CurProgress > 0) then
    begin
      Progress := CurProgress / (Num_Songs * Num_Webs);
      glTexCoord2f((width * Progress) / 8, 0);
      glVertex2f(x + width * Progress, y);

      glTexCoord2f((width * Progress) / 8, 1);
      glVertex2f(x + width * Progress, y + height);
    end;

    glTexCoord2f(0, 1);
    glVertex2f(x, y + height);
  glEnd;

 glDisable(GL_TEXTURE_2D);
 glDisable(GL_BLEND);
 glcolor4f(1, 0, 0, 1);

end;

procedure TScreenPopupScoreDownload.ShowPopup(optmode: integer; optsong: integer; optweb: integer);
var
  I: integer;
begin
  Background.OnShow;

  //reset vars
  Actual_Song := 0;
  Actual_Web := 1;
  Actual_Level := 0;
  Index_Song := 0;
  Download_Phase := 0;
  List_MD5Song := '';
  OpScoreFile := false;
  CountSongsUpdate := 0;

  if (optmode = 1) then
    OpScoreFile := true;

  for I:= 0 to 2 do
  begin
    Receive_List[I] := '';
    Position_Receive_List[I] := 1;
  end;

  Text_SongSituation := Language.Translate('SCORE_DOWNLOAD_CREATE_LIST');
  Text_WebSituation := '';

  if (optsong = 0) then
    Num_Songs := 1
  else
    Num_Songs := Songs.SongList.Count;

  if (optweb = 0) then
  begin
    Num_Webs := High(DataBase.NetworkUser) + 1;
    if (Num_Webs > 1) then
    begin
      Statics[2].Visible := true;
      Text[1].Visible := true;
    end
    else
    begin
      Text[1].Visible := false;
      Statics[2].Visible := false;
    end;
  end
  else
  begin
    Num_Webs := 1;
    Actual_Web := optweb;
    Text[1].Visible := false;
    Statics[2].Visible := false;
  end;

  Visible := true;  //Set Visible
  Button[0].Visible := true;
  Button[0].Text[0].Text := Language.Translate('SCORE_DOWNLOAD_CANCEL');

  Interaction := -1;

end;

{ TScreenPopup }

function TScreenPopup.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down

    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;

      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Visible := false;
          Result := false;
        end;

      SDLK_RETURN:
        begin
          Visible := false;
          Result := false;
        end;

      SDLK_DOWN:    InteractNext;
      SDLK_UP:      InteractPrev;

      SDLK_RIGHT: InteractNext;
      SDLK_LEFT: InteractPrev;
    end;
  end;
end;

constructor TScreenPopup.Create;
begin
  inherited Create;

  AddText(Theme.ErrorPopup.TextError);

  LoadFromTheme(Theme.ErrorPopup);

  AddButton(Theme.ErrorPopup.Button1);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, 'Button 1');

  Interaction := 0;
end;

function TScreenPopup.Draw: boolean;
begin
  Draw := inherited Draw;
end;

procedure TScreenPopup.OnShow;
begin
  inherited;

end;

procedure TScreenPopup.OnHide;
begin
end;

procedure TScreenPopup.ShowPopup(const Msg: UTF8String);
begin
  Interaction := 0; //Reset Interaction
  Visible := true;  //Set Visible
  Background.OnShow;

{  //dirty hack... Text[0] is invisible for some strange reason
  for i:=1 to high(Text) do
    if i-1 <= high(msg) then
    begin
      Text[i].Visible := true;
      Text[i].Text := msg[i-1];
    end
    else
    begin
      Text[i].Visible := false;
    end;}
  Text[0].Text := msg;

  Button[0].Visible := true;

  Button[0].Text[0].Text := 'OK';
end;

{ TScreenPopupError }

constructor TScreenPopupError.Create;
begin
  inherited;
  Text[1].Text := Language.Translate('MSG_ERROR_TITLE');
end;

{ TScreenPopupInfo }

constructor TScreenPopupInfo.Create;
begin
  inherited;
  Text[1].Text := Language.Translate('MSG_INFO_TITLE');
end;

// Help popup

function TScreenPopupHelp.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  pos:  double;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    pos := Help.GetScrollPos();
    case PressedKey of
      SDLK_TAB,
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Visible:=False;
//          if (Help.GetHelpID() = ScreenSing.GetHelpID()) then
//            ScreenSing.Pause;

          Result := false;
        end;

      SDLK_RETURN:
        begin
          Visible:=False;
          Result := false;
        end;

      SDLK_DOWN:
        begin
          InteractNext;
          if pos<(1-step) then
            Help.SetScrollPos(pos+step)
          else if pos>0 then
            Help.SetScrollPos(1);
        end;
      SDLK_UP:
        begin
          InteractPrev;
          if pos>step then
            Help.SetScrollPos(pos-step)
          else if pos>0 then
            Help.SetScrollPos(0);
        end;

      SDLK_RIGHT: InteractNext;
      SDLK_LEFT: InteractPrev;
    end;
  end;
end;

constructor TScreenPopupHelp.Create;
begin
  inherited Create;

  AddButton(Theme.HelpPopup.Button1);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, 'Button 1');
  Button[0].Visible := false;
  Interaction := 0;
end;

function TScreenPopupHelp.Draw: boolean;
var
  abs:  real;
begin
//inherited Draw; TODO: FIX
  if step<1 then
    abs := 20
  else
    abs := 5;

  //Background:
  glEnable(GL_BLEND);
  glbegin(gl_quads);
    glColor4f(0.2, 0.2, 0.2, 0.8); glVertex2f(Rect.left-5, Rect.top-5);
    glColor4f(0.2, 0.2, 0.2, 0.8); glVertex2f(Rect.right+abs, Rect.top-5);
    glColor4f(0.2, 0.2, 0.2, 0.8); glVertex2f(Rect.right+abs, Rect.bottom+5);
    glColor4f(0.2, 0.2, 0.2, 0.8); glVertex2f(Rect.left-5, Rect.bottom+5);
  glEnd;
  glDisable(GL_BLEND);
  glScissor(Rect.left-1, ScreenH-Rect.bottom-1, Rect.right-Rect.left+2, Rect.bottom-Rect.top+2);
  glScissor(round((Rect.left-1)*(ScreenW/Screens)/RenderW+(ScreenW/Screens)*(ScreenAct-1)),
    round((RenderH-Rect.bottom-1)*ScreenH/RenderH),
    round((Rect.right-Rect.left+2)*(ScreenW/Screens)/RenderW),
    round((Rect.bottom-Rect.top+2)*ScreenH/RenderH));
  glEnable(GL_SCISSOR_TEST);
  DrawTable();
  glDisable(GL_SCISSOR_TEST);
  if step<1 then
    DrawScroll(Rect.right+5, Rect.top, 10, Rect.bottom-Rect.top, Help.GetScrollPos(), barH);
end;

procedure TScreenPopupHelp.onShow;
begin
end;

procedure TScreenPopupHelp.onHide;
begin
end;

procedure TScreenPopupHelp.ShowPopup();
var
  I, J, K:  integer;
  line:     integer;
  SL:       TStringList;
  tempStr:  String;
  KeyEnd:   integer;
  Font:     integer;
  Style:    integer;
  Size:     real;
  Italic:   boolean;
  fieldh:   integer;
  tline:    integer;
  countline:integer;

  procedure AddLine(l, i, fX, fY, tX, tY: integer);
  begin
    TextsGFX[l].lines[i].fX := fX;
    TextsGFX[l].lines[i].fY := fY;
    TextsGFX[l].lines[i].tX := tX;
    TextsGFX[l].lines[i].tY := tY;
  end;

  procedure NewLine(h, lines: integer);
  begin
    inc(line);
    tline := -1;
    SetLength(TextsGFX, line+1);
    TextsGFX[line].H := h;
    TextsGFX[line].Y := TextsGFX[line-1].Y + TextsGFX[line-1].H;
    SetLength(TextsGFX[line].lines, lines);
  end;

  procedure NewText(X, Y: integer);
  begin
    inc(tline);
    SetLength(TextsGFX[line].texts, tline+1);
    TextsGFX[line].texts[tline].X := X;
    TextsGFX[line].texts[tline].Y := Y;
    TextsGFX[line].texts[tline].Font := Font;
    TextsGFX[line].texts[tline].Style := Style;
    TextsGFX[line].texts[tline].Size := Size;
    TextsGFX[line].texts[tline].Italic := Italic;
  end;

begin
  Interaction := 0; //Reset Interaction
  Visible := True;  //Set Visible

  SetLength(TextsGFX, 0);
  line := 0;
  tline := -1;

  Font := 0;
  Style := 1;
  Size := 7;
  Italic := false;

  SetFontFamily(Font);
  SetFontStyle(Style);
  SetFontSize(Size);
  SetFontItalic(Italic);


  Rect.left := 25;
  Rect.right := 770;
  Rect.top := 25;
  Rect.bottom := 575;

  KeyEnd := round((Rect.right - Rect.left)*0.4);
  fieldh := 22;

  msg := Help.GetHelpStr();

  //Title
  SetLength(TextsGFX, 1);
  TextsGFX[line].H := round(fieldh/4);
  TextsGFX[line].Y := Rect.top;
  SetLength(TextsGFX[line].lines, 3);

  AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y);
  AddLine(line, 1, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
  AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

  NewLine(fieldh*2, 2);
  AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
  AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

  NewText(Rect.left + 5, TextsGFX[line].Y + 2);
  TextsGFX[line].texts[tline].Size:=36;
  TextsGFX[line].texts[tline].text := Language.Translate('MSG_HELP_TITLE') + ': ' + msg.Title;

  NewLine(round(fieldh/4), 3);
  AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y);
  AddLine(line, 1, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
  AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

  //Description
  Font := 0;
  SetFontFamily(Font);
  Style := 1;
  SetFontStyle(Style);
  Size := 24;
  SetFontSize(Size);
  SL:=TStringList.Create;
  try
    ExtractStrings([' '], [], PChar(msg.Description), SL);
    if SL.Count>0 then
    begin
      NewLine(round(fieldh/4), 3);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y);
      AddLine(line, 1, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

      NewLine(fieldh, 2);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

      NewText(Rect.left + 5, TextsGFX[line].Y + 2);
    end;
    tempStr := '';
    for I := 0 to SL.Count-1 do
    begin
      if glTextWidth(PChar(tempStr + SL[I] + ' ')) <= (Rect.right - Rect.left - 10) then
      begin
        if I<SL.Count-1 then
          tempStr := tempStr + SL[I] + ' '
        else
          tempStr := tempStr + SL[I];
        TextsGFX[line].texts[tline].text := tempStr;
      end else
      begin
        TextsGFX[line].texts[tline].text := tempStr;
        NewLine(fieldh, 2);
        AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
        AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

        NewText(Rect.left + 5, TextsGFX[line].Y + 2);

        if I<SL.Count-1 then
          tempStr := SL[I] + ' '
        else
          tempStr := SL[I];
        TextsGFX[line].texts[tline].text := tempStr;
      end;
    end;
  Finally
    SL.Free;
    if Length(msg.Subs)<1 then
    begin
      NewLine(round(fieldh/4), 3);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y + TextsGFX[line].H , Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 1, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
    end else
    begin
      NewLine(round(fieldh/4), 2);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
    end;
  end;

  //Subs
  for K := 0 to Length(msg.Subs) - 1 do
  begin
    //Sub title
    Font := 0;
    SetFontFamily(Font);
    Style := 1;
    SetFontStyle(Style);
    Size := 24;
    SetFontSize(Size);

    tempStr := '';
    NewLine(round(fieldh/2), 2);
    AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
    AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

    NewLine(round(fieldh), 2);
    AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
    AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

    NewText(Rect.left+5, TextsGFX[line].Y + 2);
    TextsGFX[line].texts[tline].text := msg.Subs[K].title+':';

    //text
    Font := 0;
    SetFontFamily(Font);
    Style := 1;
    SetFontStyle(Style);
    Size := 20;
    SetFontSize(Size);


    for J := 0 to length(msg.Subs[K].text) - 1 do
    begin
      NewLine(fieldh, 2);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

      SL:=TStringList.Create;
      try
        ExtractStrings([' '], [], PChar(msg.Subs[K].text[J]), SL);

        NewText(Rect.left + 5, TextsGFX[line].Y + 2);
        tempStr := '';
        for I := 0 to SL.Count-1 do
        begin
          if glTextWidth(PChar(tempStr + SL[I] + ' ')) <= (Rect.right - Rect.left - 10) then
          begin
            if I<SL.Count-1 then
              tempStr := tempStr + SL[I] + ' '
            else
              tempStr := tempStr + SL[I];
            TextsGFX[line].texts[tline].text := tempStr;
          end else
          begin
            TextsGFX[line].texts[tline].text := tempStr;

            NewLine(fieldh, 2);
            AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
            AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

            NewText(Rect.left + 5, TextsGFX[line].Y + 2);

            if I<SL.Count-1 then
              tempStr := SL[I] + ' '
            else
              tempStr := SL[I];
            TextsGFX[line].texts[tline].text := tempStr;
          end;
        end;
      Finally
        SL.Free;
      end;
    end;

    if K<Length(msg.Subs) - 1 then
    begin
      NewLine(round(fieldh/4), 2);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
    end else
    begin
      NewLine(round(fieldh/2), 3);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y + TextsGFX[line].H , Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 1, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
    end;
  end;

  //Sections
  for K := 0 to Length(msg.Sections) - 1 do
  begin
    //Section title
    Font := 0;
    SetFontFamily(Font);
    Style := 1;
    SetFontStyle(Style);
    Size := 28;
    SetFontSize(Size);

    tempStr := '';
    NewLine(round(fieldh/2), 2);
    AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
    AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

    NewLine(round(fieldh*1.4), 2);
    AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
    AddLine(line, 1, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

    tempStr := msg.Sections[K].name;
    NewText(Rect.left + round((Rect.right - Rect.left - 10)/2 - glTextWidth((PChar(tempStr)))/2), TextsGFX[line].Y + 2);
    TextsGFX[line].texts[tline].text := tempStr;

    NewLine(round(fieldh/2), 3);
    AddLine(line, 0, Rect.left, TextsGFX[line].Y + TextsGFX[line].H , Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
    AddLine(line, 1, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
    AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

    Font := 0;
    SetFontFamily(0);
    Style := 1;
    SetFontStyle(Style);
    Size := 20;
    SetFontSize(Size);
    //keys
    for J := 0 to Length(msg.Sections[K].Keys) - 1 do
    begin
      NewLine(fieldh, 3);
      AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 1, KeyEnd, TextsGFX[line].Y, KeyEnd, TextsGFX[line].Y + TextsGFX[line].H);
      AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

      countline := 1;
      NewText(Rect.left + 5, TextsGFX[line].Y + 2);
      tempStr := '';
      for I := 0 to Length(msg.Sections[K].Keys[J].Key) - 1 do
      begin
        if glTextWidth(PChar(tempStr + msg.Sections[K].Keys[J].Key[I] + '+')) <= (KeyEnd - Rect.left - 10) then
        begin
          if I<Length(msg.Sections[K].Keys[J].Key)-1 then
            tempStr := tempStr + msg.Sections[K].Keys[J].Key[I] + '+'
          else
            tempStr := tempStr + msg.Sections[K].Keys[J].Key[I];
          TextsGFX[line].texts[tline].text := tempStr;
        end else
        begin
          TextsGFX[line].texts[tline].text := tempStr;
          NewLine(fieldh, 3);
          AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
          AddLine(line, 1, KeyEnd, TextsGFX[line].Y, KeyEnd, TextsGFX[line].Y + TextsGFX[line].H);
          AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);

          NewText(Rect.left + 5, TextsGFX[line].Y + 2);

          if I<Length(msg.Sections[K].Keys[J].Key)-1 then
            tempStr := msg.Sections[K].Keys[J].Key[I] + '+'
          else
            tempStr := msg.Sections[K].Keys[J].Key[I];
          TextsGFX[line].texts[tline].text := tempStr;
          inc(countline);
        end;
      end;

      //key-description
      SL:=TStringList.Create;
      try
        ExtractStrings([' '], [], PChar(msg.Sections[K].KeyDescription[J]), SL);
        line := line - countline + 1;
        tline := Length(TextsGFX[line].texts) -1;

        NewText(KeyEnd + 5, TextsGFX[line].Y + 2);
        tempStr := '';
        for I := 0 to SL.Count-1 do
        begin
          if glTextWidth(PChar(tempStr + SL[I] + ' ')) <= (Rect.right - KeyEnd - 10) then
          begin
            if I<SL.Count-1 then
              tempStr := tempStr + SL[I] + ' '
            else
              tempStr := tempStr + SL[I];
            TextsGFX[line].texts[tline].text := tempStr;
          end else
          begin
            TextsGFX[line].texts[tline].text := tempStr;
            if countline<2 then
            begin
              NewLine(fieldh, 3);
              AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
              AddLine(line, 1, KeyEnd, TextsGFX[line].Y, KeyEnd, TextsGFX[line].Y + TextsGFX[line].H);
              AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
            end else
            begin
              dec(countline);
              inc(line);
              tline := Length(TextsGFX[line].texts) -1;
            end;

            NewText(KeyEnd + 5, TextsGFX[line].Y + 2);

            if I<SL.Count-1 then
              tempStr := SL[I] + ' '
            else
              tempStr := SL[I];
            TextsGFX[line].texts[tline].text := tempStr;
          end;
        end;
      Finally
        SL.Free;
        line := line + countline -1;
        NewLine(round(fieldh/4), 4);
        AddLine(line, 0, Rect.left, TextsGFX[line].Y, Rect.left, TextsGFX[line].Y + TextsGFX[line].H);
        AddLine(line, 1, KeyEnd, TextsGFX[line].Y, KeyEnd, TextsGFX[line].Y + TextsGFX[line].H);
        AddLine(line, 2, Rect.right, TextsGFX[line].Y, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
        AddLine(line, 3, Rect.left, TextsGFX[line].Y+TextsGFX[line].H, Rect.right, TextsGFX[line].Y + TextsGFX[line].H);
      end;
    end;
  end;

  max_high := (TextsGFX[Length(TextsGFX)-1].Y + TextsGFX[Length(TextsGFX)-1].H);

  if max_high=0 then
    max_high:=1.0; //TODO error.log!
  if max_high<=Rect.bottom-Rect.top then
  begin
    barH := 1;
    step := 1;
  end
  else
  begin
    barH := (Rect.bottom-Rect.top)/max_high;
    step := barH/(1/barH);
  end;
end;

procedure TScreenPopupHelp.DrawTable();
var
  I, J:integer;
  maxh:   integer;
  h, offset:      integer;
begin

  maxh := 2*RenderH+Rect.Bottom;
  h := 0;

  offset := round(Help.GetScrollPos()*(max_high-Rect.Bottom));

  I := 0;
  while (I<Length(TextsGFX)) and (h<maxh) do
  begin
    if (TextsGFX[I].Y >= offset-20) then
    begin
      for J := 0 to Length(TextsGFX[I].lines) - 1 do
        DrawLine(I, J, offset);
      for J := 0 to Length(TextsGFX[I].texts) - 1 do
        DrawText(I, J, offset);

      h := h + TextsGFX[I].H;
    end;
    inc(I);
  end;
end;

procedure TScreenPopupHelp.DrawLine(line, index, Y: integer);
begin
  glColor4f(1, 1, 1, 1);
  glLineWidth(2);
  glBegin(GL_LINES);
    glVertex2f(TextsGFX[line].lines[index].fX, TextsGFX[line].lines[index].fY - Y);
    glVertex2f(TextsGFX[line].lines[index].tX, TextsGFX[line].lines[index].tY - Y);
  glEnd;
end;

procedure TScreenPopupHelp.DrawText(line, index, Y: integer);
begin
  glColor4f(1, 1, 1, 1);
  SetFontFamily(TextsGFX[line].texts[index].Font);
  SetFontStyle(TextsGFX[line].texts[index].Style);
  SetFontItalic(TextsGFX[line].texts[index].Italic);
  SetFontSize(TextsGFX[line].texts[index].Size);
  SetFontPos (TextsGFX[line].texts[index].X, TextsGFX[line].texts[index].Y - Y);
  glPrint(PChar(TextsGFX[line].texts[index].text));
end;

procedure TScreenPopupHelp.DrawScroll(X, Y, W, H: integer; pos, len: double);
var
  fY, tY: double;
begin
  glColor4f(1, 1, 1, 1);

  glLineWidth(1);
  glBegin(GL_LINE_LOOP);
    glVertex2f(X, Y);
    glVertex2f(X+W, Y);
    glVertex2f(X+W, Y+H);
    glVertex2f(X, Y+H);
  glEnd;

  fY := Y+(H-H*len)*Pos;
  tY := fY+H*len;
  if tY+0.001>=Y+H then
    tY := Y+H;

  glBegin(GL_QUADS);
    glVertex2f(X, fY);
    glVertex2f(X+W, fY);
    glVertex2f(X+W, tY);
    glVertex2f(X, tY);
  glEnd;
end;

end.
