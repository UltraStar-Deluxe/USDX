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
 * $URL:  $
 * $Id: $
 *}

unit UScreenOptionsNetwork;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UDataBase,
  UDisplay,
  UDLLManager,
  UFiles,
  UIni,
  UMenu,
  UMusic,
  UThemes,
  UWebSDK,
  //curlobj,
  sdl2;

type
  TScreenOptionsNetwork = class(TOptionsMenu)
    CurrentUserIndex: integer;
    CurrentWebsiteIndex: integer;
    CurrentUserSendNameIndex: integer;
    CurrentUserModeIndex: integer;
    CurrentUserPlayerIndex: integer;
    CurrentUserScoreEasyIndex: integer;
    CurrentUserScoreMediumIndex: integer;
    CurrentUserScoreHardIndex: integer;
    IWebsite:  array of UTF8String;
    IUsername: array of UTF8String;
    No_DLL: boolean;

    TextInsertUser_Warning: integer;

    NewUser_Username: UTF8String;
    EncryptPassword: UTF8String;

    InsertButton: integer;
    
    public
      constructor Create; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure OnShow; override;
      procedure UpdateUsernameList(ResetIndex: boolean);
      procedure UpdateUsernameSettings;
      procedure UpdateSettings;
      procedure DeleteUser;

    protected
      procedure LoadLegend; override;
      procedure LoadWidgets; override;
  end;

const
  ID='ID_079';   //for help system

implementation

uses
  UGraphic,
  UHelp,
  ULanguage,
  ULog,
  UUnicodeUtils,
  SysUtils;

var
  Receive_String: UTF8String;

// My   *silly*   write function just counts the number of "<" characters.
// Your *serious* write function will probably want to do something else...
function MyWriteFunction (IncomingData: pChar; ItemSize, ItemCount:LongWord; UserData:pointer):LongWord; cdecl;
var I:LongInt;
begin
  Result:= ( ItemSize * ItemCount );
  for I:=0 to Result-1 do
    Receive_String := Receive_String + UTF8String(AnsiChar(IncomingData[I]));
end;

procedure OnDeleteUser(Value: boolean; Data: Pointer);
begin
  if (Value) then
  begin
    DataBase.UpdateUsers;
    ScreenOptionsNetwork.DeleteUser;
  end;
end;

procedure SaveNewUser(Value: boolean; Data: Pointer);
var
  I: integer;
begin
  if (Value) then
  begin
    DataBase.NewUser(ScreenOptionsNetwork.SelectsS[0].TextOpt[0].Text, ScreenOptionsNetwork.NewUser_Username, ScreenOptionsNetwork.EncryptPassword);
    ScreenPopupInsertUser.Visible := false;
    ScreenOptionsNetwork.SelectsS[1].Visible := true;
    ScreenOptionsNetwork.SelectsS[2].Visible := true;
    ScreenOptionsNetwork.SelectsS[3].Visible := true;

    DataBase.ReadUsers;

    for I := 0 to High(DataBase.NetworkUser[ScreenOptionsNetwork.CurrentWebsiteIndex].UserList) do
    begin
      if (DataBase.NetworkUser[ScreenOptionsNetwork.CurrentWebsiteIndex].UserList[I].Username = ScreenOptionsNetwork.NewUser_Username) then
        ScreenOptionsNetwork.CurrentUserIndex := I;
    end;

    ScreenOptionsNetwork.UpdateUsernameList(false);
  end;
end;

procedure OnNewUser(Value: boolean; Data: Pointer);
var
  ExistUser, CorrectLogin: boolean;
  I: integer;
  NewUser_Password: UTF8String;
  LoginInfo: TLoginInfo;
  LoginStatus: byte;
begin
  DataBase.UpdateUsers;

  ExistUser := false;
  ScreenOptionsNetwork.NewUser_Username := ScreenPopupInsertUser.Username;
  NewUser_Password := ScreenPopupInsertUser.Password;

  if (Value) then
  begin

    if (ScreenOptionsNetwork.NewUser_Username = '') or (NewUser_Password = '') then
    begin
      ScreenPopupInsertUser.Text[0].Text := Language.Translate('WEBSITE_BLANK_LOGIN');
      ScreenPopupInsertUser.Interaction := ScreenPopupInsertUser.InteractionTmp;
    end
    else
    begin

      for I := 0 to High(DataBase.NetworkUser[ScreenOptionsNetwork.CurrentWebsiteIndex].UserList) do
      begin
        if (DataBase.NetworkUser[ScreenOptionsNetwork.CurrentWebsiteIndex].UserList[I].Username = ScreenOptionsNetwork.NewUser_Username) then
          ExistUser := true;
      end;

      if (ExistUser = true) then
      begin
        ScreenPopupInsertUser.Text[0].Text := Language.Translate('WEBSITE_EXIST_USER');
        ScreenPopupInsertUser.Interaction := ScreenPopupInsertUser.InteractionTmp;
      end
      else
      begin


        CorrectLogin := false;

        {
          *****************************
          ** 100: NOT A VALID DLL :p
          ** 0: ERROR_CONNECT
          ** 1: OK_LOGIN
          ** 2: ERROR_LOGIN
          *****************************
        }

        DllMan.LoadWebsite(ScreenOptionsNetwork.CurrentWebsiteIndex);

        LoginInfo.Username := ScreenOptionsNetwork.NewUser_Username;
        LoginInfo.Password := NewUser_Password;
        {
        log.LogError('_User: ' + LoginInfo.Username);
        log.LogError('_Pass: ' + LoginInfo.Password);
         }
        // Encrypt Password (with username and password)
        ScreenOptionsNetwork.EncryptPassword := DllMan.WebsiteEncryptPassword(LoginInfo);
        LoginInfo.Password := ScreenOptionsNetwork.EncryptPassword;
        LoginStatus := DllMan.WebsiteLogin(LoginInfo);

        if (LoginStatus = 1) then
          CorrectLogin := true
        else
        begin
          if (LoginStatus = 0) then
          begin
            ScreenPopupInsertUser.Visible := false;
            ScreenPopupCheck.ShowPopup('WEBSITE_NO_CONNECTION_SAVE', SaveNewUser, nil);
          end
          else
          begin
            ScreenPopupInsertUser.Text[0].Text := Language.Translate('WEBSITE_LOGIN_ERROR');
            ScreenPopupInsertUser.Interaction := ScreenPopupInsertUser.InteractionTmp;
          end;
        end;

        if (CorrectLogin = true) then
        begin
          DataBase.NewUser(ScreenOptionsNetwork.SelectsS[0].TextOpt[0].Text, ScreenOptionsNetwork.NewUser_Username, ScreenOptionsNetwork.EncryptPassword);
          ScreenPopupInsertUser.Visible := false;
          ScreenOptionsNetwork.SelectsS[1].Visible := true;
          ScreenOptionsNetwork.SelectsS[2].Visible := true;
          ScreenOptionsNetwork.SelectsS[3].Visible := true;

          DataBase.ReadUsers;

          for I := 0 to High(DataBase.NetworkUser[ScreenOptionsNetwork.CurrentWebsiteIndex].UserList) do
          begin
            if (DataBase.NetworkUser[ScreenOptionsNetwork.CurrentWebsiteIndex].UserList[I].Username = ScreenOptionsNetwork.NewUser_Username) then
              ScreenOptionsNetwork.CurrentUserIndex := I;
          end;

          ScreenOptionsNetwork.UpdateUsernameList(false);
        end;
      end;
    end;
  end;

end;

function TScreenOptionsNetwork.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  SDL_ModState:  word;
begin
  Result := true;

  if (PressedDown) then
  begin // Key Down

    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE:
        begin
          DataBase.UpdateUsers;
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenOptions);
        end;
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;
      SDLK_INSERT:
        begin
          ScreenPopupInsertUser.ShowPopup(Format(Language.Translate('MSG_INSERT_USER_TITLE'), [DataBase.NetworkUser[CurrentWebsiteIndex].Website]), Language.Translate('MSG_INSERT_USER_DESC'), OnNewUser, nil);
        end;
      SDLK_DELETE:
        begin
          if (High(DataBase.NetworkUser[CurrentWebsiteIndex].UserList) >= 0) then
            ScreenPopupCheck.ShowPopup(Format(Language.Translate('SING_OPTIONS_NETWORK_DELETE_PLAYER'), [DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].Username,DataBase.NetworkUser[CurrentWebsiteIndex].Website]), OnDeleteUser, nil, false);
        end;
      SDLK_RETURN:
        begin
          if (SelInteraction = 8) then
          begin
            DataBase.UpdateUsers;
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenOptions);
          end;

          if (SelInteraction = 9) then
            ScreenPopupInsertUser.ShowPopup(Format(Language.Translate('MSG_INSERT_USER_TITLE'), [DataBase.NetworkUser[CurrentWebsiteIndex].Website]), Language.Translate('MSG_INSERT_USER_DESC'), OnNewUser, nil);
        end;
      SDLK_DOWN:
        begin
          if (SelInteraction = 8) then
            Interaction := 0
          else
            InteractNext;
        end;
      SDLK_UP :
        begin
          if (SelInteraction = 0) then
            Interaction := 8
          else
            InteractPrev;
        end;
      SDLK_RIGHT:
        begin
          if (SelInteraction = 8) then
            Interaction := 9;

          if (SelInteraction >= 0) and (SelInteraction < 5) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;

          // Navigate Website List
          if (SelInteraction = 0) and (High(DataBase.NetworkUser) > 0) then
            UpdateUsernameList(true);

          // Navigate Username List
          if (SelInteraction = 1) then
            UpdateUsernameSettings;

          // Modify User Options
          if (SelInteraction >= 5) and (SelInteraction <= 7) then
          begin
            DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].Save := true;

            if (SDL_ModState and (KMOD_LSHIFT or KMOD_RSHIFT) <> 0) and (SelectsS[SelInteraction].SelectOptInt + 999 < 9999) then
            begin
              SelectsS[SelInteraction].SelectOptInt := SelectsS[SelInteraction].SelectOptInt + 999;
              InteractInc;
            end
            else
            begin
              if (SDL_ModState and (KMOD_LCTRL or KMOD_RCTRL) <> 0) and (SelectsS[SelInteraction].SelectOptInt + 99 < 9999) then
              begin
                SelectsS[SelInteraction].SelectOptInt := SelectsS[SelInteraction].SelectOptInt + 99;
                InteractInc;
              end
              else
              begin
                if (SelectsS[SelInteraction].SelectOptInt + 9 < 9999) then
                begin
                  SelectsS[SelInteraction].SelectOptInt := SelectsS[SelInteraction].SelectOptInt + 9;
                  InteractInc;
                end;
              end;
            end;
          end;

          if (SelInteraction >= 2) and (SelInteraction <= 7) then
          begin
            UpdateSettings;
            DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].Save := true;
          end;

        end;
      SDLK_LEFT:
        begin
          if (SelInteraction = 9) then
            Interaction := 8;

          if (SelInteraction >= 0) and (SelInteraction < 5) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;

          // Navigate Website List
          if (SelInteraction = 0) and (High(DataBase.NetworkUser) > 0) then
            UpdateUsernameList(true);

          // Navigate Username List
          if (SelInteraction = 1) then
            UpdateUsernameSettings;

          // Modify User Options
          if (SelInteraction >= 5) and (SelInteraction <= 7) then
          begin
            DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].Save := true;

            if (SDL_ModState and (KMOD_LSHIFT or KMOD_RSHIFT) <> 0) and (SelectsS[SelInteraction].SelectOptInt - 999 > 0) then
            begin
              SelectsS[SelInteraction].SelectOptInt := SelectsS[SelInteraction].SelectOptInt - 999;
              InteractDec;
            end
            else
            begin
              if (SDL_ModState and (KMOD_LCTRL or KMOD_RCTRL) <> 0) and (SelectsS[SelInteraction].SelectOptInt - 99 > 0) then
              begin
                SelectsS[SelInteraction].SelectOptInt := SelectsS[SelInteraction].SelectOptInt - 99;
                InteractDec;
              end
              else
              begin
                if (SelectsS[SelInteraction].SelectOptInt - 9 >= 0) then
                begin
                  SelectsS[SelInteraction].SelectOptInt := SelectsS[SelInteraction].SelectOptInt - 9;
                  InteractDec;
                end;
              end
            end;
          end;

          if (SelInteraction >= 2) and (SelInteraction <= 7) then
          begin
            UpdateSettings;
            DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].Save := true;
          end;

        end;
    end;
  end;
end;

constructor TScreenOptionsNetwork.Create;
var
  But: TThemeButton;
begin
  inherited Create;
  Description := Language.Translate('SING_OPTIONS_NETWORK_DESC');
  WhereAmI := Language.Translate('SING_OPTIONS_NETWORK_WHEREAMI');
  CurrentUserIndex := 0;
  CurrentWebsiteIndex := 0;
  CurrentUserSendNameIndex := 0;
  CurrentUserModeIndex := 0;
  CurrentUserPlayerIndex := 0;
  CurrentUserScoreEasyIndex := 0;
  CurrentUserScoreMediumIndex := 0;
  CurrentUserScoreHardIndex := 0;
  Load;

  But := Theme.OptionsSub.Button;
  with But do
  begin
    X := 550;
    Y := Round(Button[High(Button)].Y);
    W := 180;
    SelectW := 180;
  end;
  SetLength(But.Text, 1);
  with But.Text[0] do
  begin
    X := 90;
    Y := 3;
    Align := 1;
    ColR := 1;
    ColG := 1;
    ColG := 1;
    Size := 30;
    Text := Language.Translate('SING_OPTIONS_NETWORK_LEGEND_INSERT');
  end;
  AddButton(But);
  TextInsertUser_Warning := AddText(400, 215, 380, 0, 0, 0, 24, 1, 1, 1, 1, Language.Translate('SING_OPTIONS_NETWORK_INSERT_USER_INFO'), false, 0, 1, false);

  Interaction := 0;
end;

procedure TScreenOptionsNetwork.OnShow;
var
  I, J: integer;
begin
  inherited;

  if not Help.SetHelpID(ID) then
    Log.LogError('No Entry for Help-ID ' + ID + ' (ScreenOptionsNetwork)');

  CurrentWebsiteIndex := 0;
  CurrentUserIndex := 0;

  if (High(DataBase.NetworkUser[CurrentWebsiteIndex].UserList) > -1) then
  begin
    // Not Show Text Insert User
    Text[TextInsertUser_Warning].Visible := false;

    SelectsS[0].SetSelectOpt(CurrentWebsiteIndex);
    SelectsS[1].SetSelectOpt(CurrentUserIndex);
    UpdateUsernameList(true);

    // reset save
    for I := 0 to High(DataBase.NetworkUser) do
    begin
      for J := 0 to High(DataBase.NetworkUser[I].UserList) do
        DataBase.NetworkUser[I].UserList[J].Save := false;
    end;

  end
  else
  begin
    // no users in database for current website
    SelectsS[0].SetSelectOpt(CurrentWebsiteIndex);
    for I := 1 to 7 do
      SelectsS[I].Visible := false;

    // Text Insert User
    Text[TextInsertUser_Warning].Visible := true;
  end;

  Interaction := 0;

end;

procedure TScreenOptionsNetwork.UpdateUsernameList(ResetIndex: boolean);
var
  I: integer;
begin

  If (ResetIndex = true) then
    CurrentUserIndex := 0;

  SetLength(IUsername, Length(DataBase.NetworkUser[CurrentWebsiteIndex].UserList));
  //SetLength(IPassword, Length(DataBase.NetworkUser[CurrentWebsiteIndex].UserList));

  for I := 0 to High(DataBase.NetworkUser[CurrentWebsiteIndex].UserList) do
  begin
    IUsername[I] := DataBase.NetworkUser[CurrentWebsiteIndex].UserList[I].Username;
  //  IPassword[I] := DataBase.NetworkUser[CurrentWebsiteIndex].UserList[I].Password;
  end;

  if (High(IUsername) > 0) then
    SelectsS[1].ShowArrows := true
  else
    SelectsS[1].ShowArrows := false;
  UpdateSelectSlideOptions(1, IUsername, CurrentUserIndex);
  //UpdateSelectSlideOptions(2, IPassword, CurrentUserIndex);

  if (High(IUsername) > -1) then
  begin
    // Not Show Text Insert User
    Text[TextInsertUser_Warning].Visible := false;

    for I := 1 to 7 do
      SelectsS[I].Visible := true;
    UpdateUsernameSettings;
  end
  else
  begin
    // Show Text Insert User
    Text[TextInsertUser_Warning].Visible := true;

    for I := 1 to 7 do
      SelectsS[I].Visible := false;
  end;

end;

procedure TScreenOptionsNetwork.UpdateUsernameSettings;
begin

  SelectsS[2].SetSelectOpt(DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].SendSavePlayer);
  SelectsS[3].SetSelectOpt(DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoMode);
  SelectsS[4].SetSelectOpt(DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoPlayer);
  SelectsS[5].SetSelectOpt(DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoScoreEasy);
  SelectsS[6].SetSelectOpt(DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoScoreMedium);
  SelectsS[7].SetSelectOpt(DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoScoreHard);

  UpdateSettings;
end;

procedure TScreenOptionsNetwork.UpdateSettings;
begin

  DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].SendSavePlayer := SelectsS[2].SelectOptInt;
  DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoMode := SelectsS[3].SelectOptInt;
  DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoPlayer := SelectsS[4].SelectOptInt;
  DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoScoreEasy := SelectsS[5].SelectOptInt;
  DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoScoreMedium := SelectsS[6].SelectOptInt;
  DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoScoreHard := SelectsS[7].SelectOptInt;

  // Hide SelectS Auto Score when Auto Mode is OFF
  if (SelectsS[3].SelectedOption = 0) then
  begin
    SelectsS[4].Visible := false;
    SelectsS[5].Visible := false;
    SelectsS[6].Visible := false;
    SelectsS[7].Visible := false;
  end
  else
  begin
    SelectsS[4].Visible := true;
    SelectsS[5].Visible := true;
    SelectsS[6].Visible := true;
    SelectsS[7].Visible := true;
  end

end;

procedure TScreenOptionsNetwork.DeleteUser;
var
  I: integer;
begin

  DataBase.DeleteUser(DataBase.NetworkUser[CurrentWebsiteIndex].Website, DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].Username);
  DataBase.ReadUsers;

  SetLength(IUsername, Length(DataBase.NetworkUser[CurrentWebsiteIndex].UserList));
  //SetLength(IPassword, Length(DataBase.NetworkUser[CurrentWebsiteIndex].UserList));

  if (High(DataBase.NetworkUser[CurrentWebsiteIndex].UserList) > -1) then
  begin
    for I := 0 to High(DataBase.NetworkUser[CurrentWebsiteIndex].UserList) do
    begin
      IUsername[I] := DataBase.NetworkUser[CurrentWebsiteIndex].UserList[I].Username;
      //IPassword[I] := DataBase.NetworkUser[CurrentWebsiteIndex].UserList[I].Password;
    end;
  end;

  if (CurrentUserIndex > 0) then
    CurrentUserIndex := CurrentUserIndex - 1
  else
    CurrentUserIndex := 0;

  if (High(IUsername) > -1) then
  begin
    if (High(IUsername) = 0) then
      SelectsS[1].ShowArrows := false;

    UpdateSelectSlideOptions(1, IUsername, CurrentUserIndex);
    //UpdateSelectSlideOptions(2, IPassword, CurrentUserIndex);
    UpdateUsernameSettings;
  end
  else
  begin
    // Show Text Insert User
    Text[TextInsertUser_Warning].Visible := true;

    SelectsS[1].ShowArrows := false;
    for I := 1 to 7 do
      SelectsS[I].Visible := false;
    Interaction := 0;
  end;

end;

procedure TScreenOptionsNetwork.LoadLegend;
var
  I: integer;
begin
  for I := Low(Theme.OptionsNetworkLegendStatic) to High(Theme.OptionsNetworkLegendStatic) do
    AddStatic(Theme.OptionsNetworkLegendStatic[I]);

  for I := Low(Theme.OptionsNetworkLegendText) to High(Theme.OptionsNetworkLegendText) do
    AddText(Theme.OptionsNetworkLegendText[I]);
end;

procedure TScreenOptionsNetwork.LoadWidgets;
var
  I:  integer;
begin
  if (High(DataBase.NetworkUser) = -1) then
  begin
    // No Exist Compatible Dll's
    No_Dll := true;
  end
  else
  begin
    No_Dll := false;
    SetLength(IWebsite, Length(DataBase.NetworkUser));

    for I := 0 to High(DataBase.NetworkUser) do
      IWebsite[I] := DataBase.NetworkUser[I].Website;

    SetLength(IUsername, Length(DataBase.NetworkUser[CurrentWebsiteIndex].UserList));
    //SetLength(IPassword, Length(DataBase.NetworkUser[CurrentWebsiteIndex].UserList));

    for I := 0 to High(DataBase.NetworkUser[CurrentWebsiteIndex].UserList) do
    begin
      IUsername[I] := DataBase.NetworkUser[CurrentWebsiteIndex].UserList[I].Username;
      //IPassword[I] := DataBase.NetworkUser[CurrentWebsiteIndex].UserList[I].Password;
    end;

    // when editing this, also search for SelInteraction
    AddSelectSlide('SING_OPTIONS_NETWORK_WEBSITE', CurrentWebsiteIndex, IWebsite);
    if (High(IWebsite) > 0) then
      SelectsS[High(SelectsS)].showArrows := true
    else
      SelectsS[High(SelectsS)].showArrows := false;

    AddSelectSlide('SING_OPTIONS_NETWORK_USERNAME', CurrentUserIndex, IUsername);
    if (High(IUsername) > 0) then
      SelectsS[High(SelectsS)].showArrows := true
    else
      SelectsS[High(SelectsS)].showArrows := false;

    AddSelectSlide('SING_OPTIONS_NETWORK_SEND_SAVE_PLAYER_NAME', CurrentUserSendNameIndex, ISendNameTranslated);
    AddSelectSlide('SING_OPTIONS_NETWORK_AUTO_MODE', CurrentUserModeIndex, IAutoModeTranslated);
    AddSelectSlide('SING_OPTIONS_NETWORK_AUTO_PLAYER', CurrentUserPlayerIndex, IAutoPlayerTranslated);
    AddSelectSlide('SING_OPTIONS_NETWORK_AUTO_SCORE_EASY', CurrentUserScoreEasyIndex, IAutoScoreEasyTranslated);
    AddSelectSlide('SING_OPTIONS_NETWORK_AUTO_SCORE_MEDIUM', CurrentUserScoreMediumIndex, IAutoScoreMediumTranslated);
    AddSelectSlide('SING_OPTIONS_NETWORK_AUTO_SCORE_HARD', CurrentUserScoreHardIndex, IAutoScoreHardTranslated);

  end;
end;

end.
