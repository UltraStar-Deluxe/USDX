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
  sdl2;

type
  TScreenOptionsNetwork = class(TOptionsMenu)
  private
    RemoteBridgeWebAppIndex: integer;
    RemoteBridgeServerUrlIndex: integer;
    RemoteBridgeIpcHostIndex: integer;
    RemoteBridgeIpcPortIndex: integer;
    RemoteBridgeNodeExecutableIndex: integer;
    RemoteBridgeScriptPathIndex: integer;
    RemoteBridgeStartCommandIndex: integer;
    RemoteEditingSlide: integer;
    RemoteEditingOriginal: UTF8String;
    RemoteEditingText: UTF8String;

    function HasWebsiteConfig: boolean;
    function HasCurrentUser: boolean;
    function IsRemoteTextSlide(Slide: integer): boolean;
    function GetRemoteText(Slide: integer): UTF8String;
    function GetRemoteDefaultText(Slide: integer): UTF8String;
    function GetRemoteDisplayText(Slide: integer): UTF8String;
    procedure SetRemoteText(Slide: integer; const Value: UTF8String);
    procedure AddRemoteTextSlide(const Caption, Value: UTF8String; var Data: integer);
    procedure RefreshRemoteSlide(Slide: integer; const Value: UTF8String; Writable: boolean = false);
    procedure RefreshRemoteSlides;
    procedure StartRemoteTextEdit(Slide: integer);
    procedure StopRemoteTextEdit(Save: boolean);
    procedure SetWebsiteControlsVisible(WebsiteVisible, UserVisible: boolean);

  public
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

    constructor Create; override;
    function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean; override;
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
  UMain,
  UUnicodeUtils,
  SysUtils;

type
  InteractionID = (
    iRemoteEnabledSlide,
    iRemoteWebAppSlide,
    iRemoteServerUrlSlide,
    iRemoteIpcHostSlide,
    iRemoteIpcPortSlide,
    iRemoteNodeExecutableSlide,
    iRemoteBridgeScriptSlide,
    iRemoteStartCommandSlide,
    iWebsiteSlide,
    iUsernameSlide,
    iSendNameSlide,
    iAutoModeSlide,
    iAutoPlayerSlide,
    iAutoScoreEasySlide,
    iAutoScoreMediumSlide,
    iAutoScoreHardSlide,
    iBackButton,
    iInsertUserButton
  );

var
  Receive_String: widestring;

function SingleOption(const Value: UTF8String): TUTF8StringArray;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0] := Value;
end;

function ReadIntegerText(const Value: UTF8String; DefaultValue, MinValue: integer): integer;
begin
  Result := StrToIntDef(Trim(Value), DefaultValue);
  if (Result < MinValue) then
    Result := MinValue;
end;

function QuoteCommandPart(const Value: UTF8String): UTF8String;
begin
  if (Pos(' ', Value) > 0) or (Pos(#9, Value) > 0) then
    Result := '"' + Value + '"'
  else
    Result := Value;
end;

function DefaultBridgeScriptPath: UTF8String;
begin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) +
    'webs' + DirectorySeparator + 'usdx-bridge.mjs');
end;

function DefaultNodeExecutablePath: UTF8String;
begin
  Result := GetEnvironmentVariable('USDX_REMOTE_NODE');
  if (Result <> '') then
    Exit;

  {$IF Defined(MSWindows)}
  Result := 'C:\Program Files\nodejs\node.exe';
  if FileExists(Result) then
    Exit;

  Result := 'C:\Program Files (x86)\nodejs\node.exe';
  if FileExists(Result) then
    Exit;
  {$IFEND}

  Result := 'node';
end;

// My   *silly*   write function just counts the number of "<" characters.
// Your *serious* write function will probably want to do something else...
function MyWriteFunction (IncomingData: pChar; ItemSize, ItemCount:LongWord; UserData:pointer):LongWord; cdecl;
var I:LongInt;
begin
  Result:= ( ItemSize * ItemCount );
  for I:=0 to Result-1 do
    Receive_String := Receive_String + widechar(IncomingData[I]);
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
  if (Value) and ScreenOptionsNetwork.HasWebsiteConfig then
  begin
    DataBase.NewUser(
      DataBase.NetworkUser[ScreenOptionsNetwork.CurrentWebsiteIndex].Website,
      ScreenOptionsNetwork.NewUser_Username,
      ScreenOptionsNetwork.EncryptPassword);
    ScreenPopupInsertUser.Visible := false;

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
  if not ScreenOptionsNetwork.HasWebsiteConfig then
    Exit;

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

        DllMan.LoadWebsite(ScreenOptionsNetwork.CurrentWebsiteIndex);

        LoginInfo.Username := ScreenOptionsNetwork.NewUser_Username;
        LoginInfo.Password := NewUser_Password;
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
          DataBase.NewUser(
            DataBase.NetworkUser[ScreenOptionsNetwork.CurrentWebsiteIndex].Website,
            ScreenOptionsNetwork.NewUser_Username,
            ScreenOptionsNetwork.EncryptPassword);
          ScreenPopupInsertUser.Visible := false;

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

function TScreenOptionsNetwork.HasWebsiteConfig: boolean;
begin
  Result := High(DataBase.NetworkUser) >= 0;
end;

function TScreenOptionsNetwork.HasCurrentUser: boolean;
begin
  Result := HasWebsiteConfig and
    (CurrentWebsiteIndex >= 0) and
    (CurrentWebsiteIndex <= High(DataBase.NetworkUser)) and
    (CurrentUserIndex >= 0) and
    (CurrentUserIndex <= High(DataBase.NetworkUser[CurrentWebsiteIndex].UserList));
end;

function TScreenOptionsNetwork.IsRemoteTextSlide(Slide: integer): boolean;
begin
  Result := Slide in [
    Ord(iRemoteWebAppSlide),
    Ord(iRemoteServerUrlSlide),
    Ord(iRemoteIpcHostSlide),
    Ord(iRemoteIpcPortSlide),
    Ord(iRemoteNodeExecutableSlide),
    Ord(iRemoteBridgeScriptSlide),
    Ord(iRemoteStartCommandSlide)
  ];
end;

function TScreenOptionsNetwork.GetRemoteText(Slide: integer): UTF8String;
begin
  case InteractionID(Slide) of
    iRemoteWebAppSlide:
      Result := Ini.RemoteBridgeWebApp;
    iRemoteServerUrlSlide:
      Result := Ini.RemoteBridgeServerUrl;
    iRemoteIpcHostSlide:
      Result := Ini.RemoteBridgeIpcHost;
    iRemoteIpcPortSlide:
      Result := IntToStr(Ini.RemoteBridgeIpcPort);
    iRemoteNodeExecutableSlide:
      Result := Ini.RemoteBridgeNodeExecutable;
    iRemoteBridgeScriptSlide:
      Result := Ini.RemoteBridgeScriptPath;
    iRemoteStartCommandSlide:
      Result := Ini.RemoteBridgeStartCommand;
  else
    Result := '';
  end;
end;

function TScreenOptionsNetwork.GetRemoteDefaultText(Slide: integer): UTF8String;
var
  WebApp: UTF8String;
begin
  case InteractionID(Slide) of
    iRemoteWebAppSlide:
      begin
        Result := Trim(GetEnvironmentVariable('USDX_REMOTE_WEB_APP'));
        if (Result = '') then
          Result := 'default';
      end;
    iRemoteServerUrlSlide:
      Result := DEFAULT_REMOTE_BRIDGE_SERVER_URL;
    iRemoteIpcHostSlide:
      Result := DEFAULT_REMOTE_BRIDGE_IPC_HOST;
    iRemoteIpcPortSlide:
      Result := IntToStr(DEFAULT_REMOTE_BRIDGE_IPC_PORT);
    iRemoteNodeExecutableSlide:
      Result := DefaultNodeExecutablePath;
    iRemoteBridgeScriptSlide:
      Result := DefaultBridgeScriptPath;
    iRemoteStartCommandSlide:
      begin
        Result :=
          QuoteCommandPart(GetRemoteDisplayText(Ord(iRemoteNodeExecutableSlide))) + ' ' +
          QuoteCommandPart(GetRemoteDisplayText(Ord(iRemoteBridgeScriptSlide))) +
          ' --server ' + QuoteCommandPart(GetRemoteDisplayText(Ord(iRemoteServerUrlSlide))) +
          ' --ipc-host ' + QuoteCommandPart(GetRemoteDisplayText(Ord(iRemoteIpcHostSlide))) +
          ' --ipc-port ' + GetRemoteDisplayText(Ord(iRemoteIpcPortSlide)) +
          ' --mock-song=false --auto-ack=false --auto-assign=false --p2p=false';
        WebApp := GetRemoteDisplayText(Ord(iRemoteWebAppSlide));
        if (WebApp <> '') then
          Result := Result + ' --web-app ' + QuoteCommandPart(WebApp);
      end;
  else
    Result := '';
  end;
end;

function TScreenOptionsNetwork.GetRemoteDisplayText(Slide: integer): UTF8String;
begin
  Result := GetRemoteText(Slide);
  if (Trim(Result) = '') then
    Result := GetRemoteDefaultText(Slide);
end;

procedure TScreenOptionsNetwork.SetRemoteText(Slide: integer; const Value: UTF8String);
begin
  case InteractionID(Slide) of
    iRemoteWebAppSlide:
      Ini.RemoteBridgeWebApp := Trim(Value);
    iRemoteServerUrlSlide:
      begin
        Ini.RemoteBridgeServerUrl := Trim(Value);
        if (Ini.RemoteBridgeServerUrl = '') then
          Ini.RemoteBridgeServerUrl := DEFAULT_REMOTE_BRIDGE_SERVER_URL;
      end;
    iRemoteIpcHostSlide:
      begin
        Ini.RemoteBridgeIpcHost := Trim(Value);
        if (Ini.RemoteBridgeIpcHost = '') then
          Ini.RemoteBridgeIpcHost := DEFAULT_REMOTE_BRIDGE_IPC_HOST;
      end;
    iRemoteIpcPortSlide:
      Ini.RemoteBridgeIpcPort := ReadIntegerText(Value, DEFAULT_REMOTE_BRIDGE_IPC_PORT, 1);
    iRemoteNodeExecutableSlide:
      Ini.RemoteBridgeNodeExecutable := Trim(Value);
    iRemoteBridgeScriptSlide:
      Ini.RemoteBridgeScriptPath := Trim(Value);
    iRemoteStartCommandSlide:
      Ini.RemoteBridgeStartCommand := Trim(Value);
  end;

  RefreshRemoteSlide(Slide, GetRemoteDisplayText(Slide));
end;

procedure TScreenOptionsNetwork.AddRemoteTextSlide(const Caption, Value: UTF8String; var Data: integer);
var
  Slide: integer;
  Values: TUTF8StringArray;
begin
  Data := 0;
  Values := SingleOption(Value);
  Slide := AddSelectSlide(Caption, Data, Values);
  SelectsS[Slide].ShowArrows := false;
end;

procedure TScreenOptionsNetwork.RefreshRemoteSlide(Slide: integer; const Value: UTF8String; Writable: boolean);
begin
  if (Slide < 0) or (Slide > High(SelectsS)) then
    Exit;

  if (Length(SelectsS[Slide].TextOptT) = 0) then
    SetLength(SelectsS[Slide].TextOptT, 1);
  if Writable and (Value = '') then
    SelectsS[Slide].TextOptT[0] := ' '
  else
    SelectsS[Slide].TextOptT[0] := Value;
  SelectsS[Slide].ShowArrows := false;
  SelectsS[Slide].GenLines;
  SelectsS[Slide].SetSelectOpt(0);
  if (Length(SelectsS[Slide].TextOpt) > 0) then
  begin
    SelectsS[Slide].TextOpt[0].Writable := Writable;
    SelectsS[Slide].TextOpt[0].Selected := Writable;
  end;
end;

procedure TScreenOptionsNetwork.RefreshRemoteSlides;
begin
  SelectsS[Ord(iRemoteEnabledSlide)].SetSelectOpt(Ini.RemoteBridgeEnabled);
  RefreshRemoteSlide(Ord(iRemoteWebAppSlide), GetRemoteDisplayText(Ord(iRemoteWebAppSlide)));
  RefreshRemoteSlide(Ord(iRemoteServerUrlSlide), GetRemoteDisplayText(Ord(iRemoteServerUrlSlide)));
  RefreshRemoteSlide(Ord(iRemoteIpcHostSlide), GetRemoteDisplayText(Ord(iRemoteIpcHostSlide)));
  RefreshRemoteSlide(Ord(iRemoteIpcPortSlide), GetRemoteDisplayText(Ord(iRemoteIpcPortSlide)));
  RefreshRemoteSlide(Ord(iRemoteNodeExecutableSlide), GetRemoteDisplayText(Ord(iRemoteNodeExecutableSlide)));
  RefreshRemoteSlide(Ord(iRemoteBridgeScriptSlide), GetRemoteDisplayText(Ord(iRemoteBridgeScriptSlide)));
  RefreshRemoteSlide(Ord(iRemoteStartCommandSlide), GetRemoteDisplayText(Ord(iRemoteStartCommandSlide)));
end;

procedure TScreenOptionsNetwork.StartRemoteTextEdit(Slide: integer);
begin
  if not IsRemoteTextSlide(Slide) then
    Exit;

  RemoteEditingSlide := Slide;
  RemoteEditingOriginal := GetRemoteDisplayText(Slide);
  RemoteEditingText := RemoteEditingOriginal;
  RefreshRemoteSlide(Slide, RemoteEditingText, true);
  StartTextInput;
end;

procedure TScreenOptionsNetwork.StopRemoteTextEdit(Save: boolean);
begin
  if (RemoteEditingSlide < 0) then
    Exit;

  StopTextInput;
  if Save then
  begin
    SetRemoteText(RemoteEditingSlide, RemoteEditingText);
    Ini.Save;
  end
  else
    RefreshRemoteSlide(RemoteEditingSlide, GetRemoteDisplayText(RemoteEditingSlide));

  RemoteEditingSlide := -1;
  RemoteEditingOriginal := '';
  RemoteEditingText := '';
end;

procedure TScreenOptionsNetwork.SetWebsiteControlsVisible(WebsiteVisible, UserVisible: boolean);
var
  I: integer;
begin
  SelectsS[Ord(iWebsiteSlide)].Visible := WebsiteVisible;
  for I := Ord(iUsernameSlide) to Ord(iAutoScoreHardSlide) do
    SelectsS[I].Visible := UserVisible;
end;

function TScreenOptionsNetwork.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean; Repeated: boolean = false): boolean;
var
  SDL_ModState:  word;
begin
  Result := true;

  if (PressedDown) then
  begin // Key Down

    if (RemoteEditingSlide >= 0) then
    begin
      if (IsPrintableChar(CharCode)) then
      begin
        RemoteEditingText := RemoteEditingText + UCS4ToUTF8String(CharCode);
        RefreshRemoteSlide(RemoteEditingSlide, RemoteEditingText, true);
        Exit;
      end;

      case PressedKey of
        SDLK_ESCAPE:
          StopRemoteTextEdit(false);
        SDLK_RETURN:
          StopRemoteTextEdit(true);
        SDLK_BACKSPACE:
          begin
            if LengthUTF8(RemoteEditingText) > 0 then
              UTF8Delete(RemoteEditingText, LengthUTF8(RemoteEditingText), 1);
            RefreshRemoteSlide(RemoteEditingSlide, RemoteEditingText, true);
          end;
        SDLK_TAB:
          ScreenPopupHelp.ShowPopup();
      end;
      Exit;
    end;

    SDL_ModState := SDL_GetModState and (KMOD_LSHIFT + KMOD_RSHIFT
    + KMOD_LCTRL + KMOD_RCTRL + KMOD_LALT  + KMOD_RALT);

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE:
        begin
          if HasWebsiteConfig then
            DataBase.UpdateUsers;
          Ini.Save;
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenOptions);
        end;
      SDLK_TAB:
        begin
          ScreenPopupHelp.ShowPopup();
        end;
      SDLK_INSERT:
        begin
          if HasWebsiteConfig then
            ScreenPopupInsertUser.ShowPopup(Format(Language.Translate('MSG_INSERT_USER_TITLE'), [DataBase.NetworkUser[CurrentWebsiteIndex].Website]), Language.Translate('MSG_INSERT_USER_DESC'), OnNewUser, nil);
        end;
      SDLK_DELETE:
        begin
          if HasCurrentUser then
            ScreenPopupCheck.ShowPopup(Format(Language.Translate('SING_OPTIONS_NETWORK_DELETE_PLAYER'), [DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].Username,DataBase.NetworkUser[CurrentWebsiteIndex].Website]), OnDeleteUser, nil, false);
        end;
      SDLK_RETURN:
        begin
          if IsRemoteTextSlide(SelInteraction) then
          begin
            StartRemoteTextEdit(SelInteraction);
            Exit;
          end;

          if (SelInteraction = ord(iBackButton)) then
          begin
            if HasWebsiteConfig then
              DataBase.UpdateUsers;
            Ini.Save;
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenOptions);
          end;

          if (SelInteraction = ord(iInsertUserButton)) and HasWebsiteConfig then
            ScreenPopupInsertUser.ShowPopup(Format(Language.Translate('MSG_INSERT_USER_TITLE'), [DataBase.NetworkUser[CurrentWebsiteIndex].Website]), Language.Translate('MSG_INSERT_USER_DESC'), OnNewUser, nil);
        end;
      SDLK_DOWN:
        begin
          if (SelInteraction = ord(iBackButton)) then
            Interaction := 0
          else
            InteractNext;
        end;
      SDLK_UP :
        begin
          if (SelInteraction = 0) then
            Interaction := ord(iBackButton)
          else
            InteractPrev;
        end;
      SDLK_RIGHT:
        begin
          if (SelInteraction = ord(iBackButton)) and HasWebsiteConfig then
            Interaction := ord(iInsertUserButton);

          if (SelInteraction = Ord(iRemoteEnabledSlide)) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
            Ini.RemoteBridgeEnabled := SelectsS[Ord(iRemoteEnabledSlide)].SelectOptInt;
          end;

          if HasWebsiteConfig and (InteractionID(SelInteraction) in [iWebsiteSlide, iUsernameSlide, iSendNameSlide, iAutoModeSlide]) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;

          // Navigate Website List
          if HasWebsiteConfig and (SelInteraction = ord(iWebsiteSlide)) and (High(DataBase.NetworkUser) > 0) then
            UpdateUsernameList(true);

          // Navigate Username List
          if HasWebsiteConfig and (SelInteraction = ord(iUsernameSlide)) then
            UpdateUsernameSettings;

          // Modify User Options
          if HasCurrentUser and (InteractionID(SelInteraction) in [iAutoScoreEasySlide, iAutoScoreMediumSlide, iAutoScoreHardSlide]) then
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

          if HasCurrentUser and (InteractionID(SelInteraction) in [iSendNameSlide, iAutoModeSlide, iAutoPlayerSlide, iAutoScoreEasySlide, iAutoScoreMediumSlide, iAutoScoreHardSlide]) then
          begin
            UpdateSettings;
            DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].Save := true;
          end;

        end;
      SDLK_LEFT:
        begin
          if (SelInteraction = ord(iInsertUserButton)) then
            Interaction := ord(iBackButton);

          if (SelInteraction = Ord(iRemoteEnabledSlide)) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
            Ini.RemoteBridgeEnabled := SelectsS[Ord(iRemoteEnabledSlide)].SelectOptInt;
          end;

          if HasWebsiteConfig and (InteractionID(SelInteraction) in [iWebsiteSlide, iUsernameSlide, iSendNameSlide, iAutoModeSlide]) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;

          // Navigate Website List
          if HasWebsiteConfig and (SelInteraction = ord(iWebsiteSlide)) and (High(DataBase.NetworkUser) > 0) then
            UpdateUsernameList(true);

          // Navigate Username List
          if HasWebsiteConfig and (SelInteraction = ord(iUsernameSlide)) then
            UpdateUsernameSettings;

          // Modify User Options
          if HasCurrentUser and (InteractionID(SelInteraction) in [iAutoScoreEasySlide, iAutoScoreMediumSlide, iAutoScoreHardSlide]) then
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

          if HasCurrentUser and (InteractionID(SelInteraction) in [iSendNameSlide, iAutoModeSlide, iAutoPlayerSlide, iAutoScoreEasySlide, iAutoScoreMediumSlide, iAutoScoreHardSlide]) then
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
  RemoteEditingSlide := -1;
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
    ColB := 1;
    Size := 30;
    Text := Language.Translate('SING_OPTIONS_NETWORK_LEGEND_INSERT');
  end;
  InsertButton := AddButton(But);
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

  RefreshRemoteSlides;
  CurrentWebsiteIndex := 0;
  CurrentUserIndex := 0;
  No_DLL := not HasWebsiteConfig;

  Button[InsertButton].Visible := HasWebsiteConfig;

  if not HasWebsiteConfig then
  begin
    SetWebsiteControlsVisible(false, false);
    Text[TextInsertUser_Warning].Visible := false;
  end
  else if (High(DataBase.NetworkUser[CurrentWebsiteIndex].UserList) > -1) then
  begin
    Text[TextInsertUser_Warning].Visible := false;

    SelectsS[Ord(iWebsiteSlide)].SetSelectOpt(CurrentWebsiteIndex);
    SelectsS[Ord(iUsernameSlide)].SetSelectOpt(CurrentUserIndex);
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
    SelectsS[Ord(iWebsiteSlide)].SetSelectOpt(CurrentWebsiteIndex);
    SetWebsiteControlsVisible(true, false);
    Text[TextInsertUser_Warning].Visible := true;
  end;

  Interaction := 0;
end;

procedure TScreenOptionsNetwork.UpdateUsernameList(ResetIndex: boolean);
var
  I: integer;
begin
  if not HasWebsiteConfig then
  begin
    SetWebsiteControlsVisible(false, false);
    Exit;
  end;

  If (ResetIndex = true) then
    CurrentUserIndex := 0;

  SetLength(IUsername, Length(DataBase.NetworkUser[CurrentWebsiteIndex].UserList));

  for I := 0 to High(DataBase.NetworkUser[CurrentWebsiteIndex].UserList) do
    IUsername[I] := DataBase.NetworkUser[CurrentWebsiteIndex].UserList[I].Username;

  if (High(IUsername) > 0) then
    SelectsS[Ord(iUsernameSlide)].ShowArrows := true
  else
    SelectsS[Ord(iUsernameSlide)].ShowArrows := false;
  UpdateSelectSlideOptions(Ord(iUsernameSlide), IUsername, CurrentUserIndex);

  if (High(IUsername) > -1) then
  begin
    Text[TextInsertUser_Warning].Visible := false;
    SetWebsiteControlsVisible(true, true);
    UpdateUsernameSettings;
  end
  else
  begin
    Text[TextInsertUser_Warning].Visible := true;
    SetWebsiteControlsVisible(true, false);
  end;
end;

procedure TScreenOptionsNetwork.UpdateUsernameSettings;
begin
  if not HasCurrentUser then
    Exit;

  SelectsS[Ord(iSendNameSlide)].SetSelectOpt(DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].SendSavePlayer);
  SelectsS[Ord(iAutoModeSlide)].SetSelectOpt(DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoMode);
  SelectsS[Ord(iAutoPlayerSlide)].SetSelectOpt(DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoPlayer);
  SelectsS[Ord(iAutoScoreEasySlide)].SetSelectOpt(DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoScoreEasy);
  SelectsS[Ord(iAutoScoreMediumSlide)].SetSelectOpt(DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoScoreMedium);
  SelectsS[Ord(iAutoScoreHardSlide)].SetSelectOpt(DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoScoreHard);

  UpdateSettings;
end;

procedure TScreenOptionsNetwork.UpdateSettings;
begin
  if not HasCurrentUser then
    Exit;

  DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].SendSavePlayer := SelectsS[Ord(iSendNameSlide)].SelectOptInt;
  DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoMode := SelectsS[Ord(iAutoModeSlide)].SelectOptInt;
  DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoPlayer := SelectsS[Ord(iAutoPlayerSlide)].SelectOptInt;
  DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoScoreEasy := SelectsS[Ord(iAutoScoreEasySlide)].SelectOptInt;
  DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoScoreMedium := SelectsS[Ord(iAutoScoreMediumSlide)].SelectOptInt;
  DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].AutoScoreHard := SelectsS[Ord(iAutoScoreHardSlide)].SelectOptInt;

  // Hide SelectS Auto Score when Auto Mode is OFF
  if (SelectsS[Ord(iAutoModeSlide)].SelectedOption = 0) then
  begin
    SelectsS[Ord(iAutoPlayerSlide)].Visible := false;
    SelectsS[Ord(iAutoScoreEasySlide)].Visible := false;
    SelectsS[Ord(iAutoScoreMediumSlide)].Visible := false;
    SelectsS[Ord(iAutoScoreHardSlide)].Visible := false;
  end
  else
  begin
    SelectsS[Ord(iAutoPlayerSlide)].Visible := true;
    SelectsS[Ord(iAutoScoreEasySlide)].Visible := true;
    SelectsS[Ord(iAutoScoreMediumSlide)].Visible := true;
    SelectsS[Ord(iAutoScoreHardSlide)].Visible := true;
  end
end;

procedure TScreenOptionsNetwork.DeleteUser;
var
  I: integer;
begin
  if not HasCurrentUser then
    Exit;

  DataBase.DeleteUser(DataBase.NetworkUser[CurrentWebsiteIndex].Website, DataBase.NetworkUser[CurrentWebsiteIndex].UserList[CurrentUserIndex].Username);
  DataBase.ReadUsers;

  SetLength(IUsername, Length(DataBase.NetworkUser[CurrentWebsiteIndex].UserList));

  if (High(DataBase.NetworkUser[CurrentWebsiteIndex].UserList) > -1) then
  begin
    for I := 0 to High(DataBase.NetworkUser[CurrentWebsiteIndex].UserList) do
      IUsername[I] := DataBase.NetworkUser[CurrentWebsiteIndex].UserList[I].Username;
  end;

  if (CurrentUserIndex > 0) then
    CurrentUserIndex := CurrentUserIndex - 1
  else
    CurrentUserIndex := 0;

  if (High(IUsername) > -1) then
  begin
    if (High(IUsername) = 0) then
      SelectsS[Ord(iUsernameSlide)].ShowArrows := false;

    UpdateSelectSlideOptions(Ord(iUsernameSlide), IUsername, CurrentUserIndex);
    UpdateUsernameSettings;
  end
  else
  begin
    Text[TextInsertUser_Warning].Visible := true;

    SelectsS[Ord(iUsernameSlide)].ShowArrows := false;
    SetWebsiteControlsVisible(true, false);
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
  AddSelectSlide('SING_OPTIONS_NETWORK_REMOTE_ENABLED', Ini.RemoteBridgeEnabled, ITabsTranslated);
  AddRemoteTextSlide('SING_OPTIONS_NETWORK_REMOTE_WEB_APP', GetRemoteDisplayText(Ord(iRemoteWebAppSlide)), RemoteBridgeWebAppIndex);
  AddRemoteTextSlide('SING_OPTIONS_NETWORK_REMOTE_SERVER_URL', GetRemoteDisplayText(Ord(iRemoteServerUrlSlide)), RemoteBridgeServerUrlIndex);
  AddRemoteTextSlide('SING_OPTIONS_NETWORK_REMOTE_IPC_HOST', GetRemoteDisplayText(Ord(iRemoteIpcHostSlide)), RemoteBridgeIpcHostIndex);
  AddRemoteTextSlide('SING_OPTIONS_NETWORK_REMOTE_IPC_PORT', GetRemoteDisplayText(Ord(iRemoteIpcPortSlide)), RemoteBridgeIpcPortIndex);
  AddRemoteTextSlide('SING_OPTIONS_NETWORK_REMOTE_NODE_EXECUTABLE', GetRemoteDisplayText(Ord(iRemoteNodeExecutableSlide)), RemoteBridgeNodeExecutableIndex);
  AddRemoteTextSlide('SING_OPTIONS_NETWORK_REMOTE_BRIDGE_SCRIPT', GetRemoteDisplayText(Ord(iRemoteBridgeScriptSlide)), RemoteBridgeScriptPathIndex);
  AddRemoteTextSlide('SING_OPTIONS_NETWORK_REMOTE_START_COMMAND', GetRemoteDisplayText(Ord(iRemoteStartCommandSlide)), RemoteBridgeStartCommandIndex);

  if (High(DataBase.NetworkUser) = -1) then
  begin
    No_Dll := true;
    SetLength(IWebsite, 1);
    IWebsite[0] := Language.Translate('SING_OPTIONS_NETWORK_NO_DLL');
    SetLength(IUsername, 0);
  end
  else
  begin
    No_Dll := false;
    SetLength(IWebsite, Length(DataBase.NetworkUser));

    for I := 0 to High(DataBase.NetworkUser) do
      IWebsite[I] := DataBase.NetworkUser[I].Website;

    SetLength(IUsername, Length(DataBase.NetworkUser[CurrentWebsiteIndex].UserList));

    for I := 0 to High(DataBase.NetworkUser[CurrentWebsiteIndex].UserList) do
      IUsername[I] := DataBase.NetworkUser[CurrentWebsiteIndex].UserList[I].Username;
  end;

  // when editing this, also update the InteractionID enum declaration
  AddSelectSlide('SING_OPTIONS_NETWORK_WEBSITE', CurrentWebsiteIndex, IWebsite);
  if (High(IWebsite) > 0) then
    SelectsS[High(SelectsS)].ShowArrows := true
  else
    SelectsS[High(SelectsS)].ShowArrows := false;

  AddSelectSlide('SING_OPTIONS_NETWORK_USERNAME', CurrentUserIndex, IUsername);
  if (High(IUsername) > 0) then
    SelectsS[High(SelectsS)].ShowArrows := true
  else
    SelectsS[High(SelectsS)].ShowArrows := false;

  AddSelectSlide('SING_OPTIONS_NETWORK_SEND_SAVE_PLAYER_NAME', CurrentUserSendNameIndex, ISendNameTranslated);
  AddSelectSlide('SING_OPTIONS_NETWORK_AUTO_MODE', CurrentUserModeIndex, IAutoModeTranslated);
  AddSelectSlide('SING_OPTIONS_NETWORK_AUTO_PLAYER', CurrentUserPlayerIndex, IAutoPlayerTranslated);
  AddSelectSlide('SING_OPTIONS_NETWORK_AUTO_SCORE_EASY', CurrentUserScoreEasyIndex, IAutoScoreEasyTranslated);
  AddSelectSlide('SING_OPTIONS_NETWORK_AUTO_SCORE_MEDIUM', CurrentUserScoreMediumIndex, IAutoScoreMediumTranslated);
  AddSelectSlide('SING_OPTIONS_NETWORK_AUTO_SCORE_HARD', CurrentUserScoreHardIndex, IAutoScoreHardTranslated);

  if No_Dll then
    SetWebsiteControlsVisible(false, false);
end;

end.
