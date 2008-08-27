library Hold_The_Line;

uses
  ModiSDK in '..\SDK\ModiSDK.pas',
  StrUtils in '..\SDK\StrUtils.pas',
  OpenGL12,
  Windows;

var
  PointerTex: TSmallTexture;
  CountSentences: Cardinal;
  Limit: Byte;
  MethodRec: TMethodRec;
  Frame: Integer;
  PlayerTimes: array[0..5] of Integer;
  LastTick: Cardinal;
  PointerVisible: Boolean;

  DismissedSound: Cardinal;

//Gave the Plugins Info
procedure PluginInfo (var Info: TPluginInfo); stdcall;
begin
  Info.Name    := 'PLUGIN_HDL_NAME';

  Info.Creator    := 'Whiteshark';
  Info.PluginDesc := 'PLUGIN_HDL_DESC';

  //Set to Party Modi Plugin
  Info.Typ := 8;

  Info.NumPlayers := 31;
  //Options
  Info.LoadSong := True;  //Whether or not a Song should be Loaded
  //Only When Song is Loaded:
  Info.ShowScore := True; //Whether or not the Score should be shown
  Info.ShowNotes := True; //Whether the Note Lines should be displayed
  Info.LoadVideo := True; //Should the Video be loaded ?
  Info.LoadBack  := True; //Should the Background be loaded ?

  Info.BGShowFull := False;   //Whether the Background or the Video should be shown Fullsize
  Info.BGShowFull_O := True;  //Whether the Background or the Video should be shown Fullsize

  Info.ShowRateBar:= True;   //Whether the Bar that shows how good the player was sould be displayed
  Info.ShowRateBar_O := False; //Load from Ini whether the Bar should be Displayed

  Info.EnLineBonus := False;  //Whether LineBonus Should be enabled
  Info.EnLineBonus_O := True; //Load from Ini whether LineBonus Should be enabled

  //Options even when song is Not loaded
  Info.ShowBars := False; //Whether the White Bars on Top and Bottom should be Drawn
  Info.TeamModeOnly := False;  //If True the Plugin can only be Played in Team Mode
  Info.GetSoundData := False;  //If True the RData Procedure is called when new SoundData is available
  Info.Dummy := False;         //Should be Set to False... for Updateing Plugin Interface
end;

//Executed on Game Start //If True Game begins, else Failure
function Init (const TeamInfo: TTeamInfo; var Playerinfo: TPlayerinfo; const Sentences: TSentences; const Methods: TMethodRec): boolean; stdcall;
var
  I: Integer;
  Texname: PChar;
  TexType: TTextureType;
begin
  TexName := CreateStr(PChar('HDL_Pointer'));
  TexType := TEXTURE_TYPE_TRANSPARENT;
  PointerTex := Methods.LoadTex(TexName, TexType);

  FreeStr(TexName);

  TexName := CreateStr(PChar('dismissed.mp3'));
  DismissedSound := Methods.LoadSound (TexName);
  FreeStr(TexName);

  CountSentences := Sentences.High;
  Limit := 0;
  Frame := 0;

  MethodRec := Methods;

  for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
    PlayerInfo.Playerinfo[I].Enabled := True;
    PlayerInfo.Playerinfo[I].Percentage := 100;
    PlayerTimes[I] := 0;
  end;

  LoadOpenGL;

  Result := True;
end;

//Executed everytime the Screen is Drawed //If False The Game finishes
function Draw (var Playerinfo: TPlayerinfo; const CurSentence: Cardinal): boolean; stdcall;
var
  I: Integer;
  L: Byte;
  C: Byte;
  Text: PChar;
  Blink: Boolean;
  tick: Cardinal;
begin
  //Aktivate Blink
  If (CurSentence = CountSentences div 5 * 2 - 1) OR (CurSentence = CountSentences div 3 * 2 - 1) then
  begin
    Tick := Gettickcount div 400;
    If (Tick <> LastTick) then
    begin
      LastTick := Tick;
      PointerVisible := Not PointerVisible;
    end;
  end
  else
    PointerVisible := True;

  //Inc Limit
  if (Limit = 0) And  (CurSentence >= CountSentences div 5 * 2) then
    Inc(Limit)
  else if (Limit = 1) And  (CurSentence >= CountSentences div 3 * 2) then
    Inc(Limit);

  case Limit of
    0: L := 20;
    1: L := 50;
    2: L := 75;
  end;

  C:= 0;

  Result := True;

  for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
    if PlayerInfo.Playerinfo[I].Enabled then
    begin
      if PlayerInfo.Playerinfo[I].Bar < L then
      begin
        PlayerInfo.Playerinfo[I].Enabled := False;
        Inc(C);
        PlayerTimes[I] := CurSentence; //Save Time of Dismission
        //PlaySound
        MethodRec.PlaySound (DismissedSound);
      end;

      //Draw Pointer
      if (PointerVisible) then
      begin
        glColor4f (0.2, 0.8, 0.1, 1);

        glEnable(GL_TEXTURE_2D);
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

        glBindTexture(GL_TEXTURE_2D, PointerTex.TexNum);

        glBegin(GL_QUADS);
          glTexCoord2f(1/32, 0); glVertex2f(PlayerInfo.Playerinfo[I].PosX + L - 3, PlayerInfo.Playerinfo[I].PosY - 4);
          glTexCoord2f(1/32, 1); glVertex2f(PlayerInfo.Playerinfo[I].PosX + L - 3, PlayerInfo.Playerinfo[I].PosY + 12);
          glTexCoord2f(31/32, 1); glVertex2f(PlayerInfo.Playerinfo[I].PosX+ L + 3, PlayerInfo.Playerinfo[I].PosY + 12);
          glTexCoord2f(31/32, 0); glVertex2f(PlayerInfo.Playerinfo[I].PosX+ L + 3, PlayerInfo.Playerinfo[I].PosY - 4);
        glEnd;

        glDisable(GL_TEXTURE_2D);
        glDisable(GL_BLEND);
      end;

    end
    else
    begin
      Inc(C);
      //Draw Dismissed
      Text := CreateStr(PChar('PARTY_DISMISSED'));

      glColor4f (0.8, 0.8, 0.8, 1);

      MethodRec.Print (1, 6, PlayerInfo.Playerinfo[I].PosX, PlayerInfo.Playerinfo[I].PosY-8, Text);
      FreeStr(Text);
    end;
  end;
  if (C >= PlayerInfo.NumPlayers-1) then
    Result := False;
end;

//Is Executed on Finish, Returns the Playernum of the Winner
function Finish (var Playerinfo: TPlayerinfo): byte; stdcall;
var
  I:Integer;
begin
Result := 0;
for I := 0 to PlayerInfo.NumPlayers-1 do
  begin
  PlayerInfo.Playerinfo[I].Percentage := (PlayerTimes[I] * 100) div CountSentences;
    if (PlayerInfo.Playerinfo[I].Enabled) then
    begin
      PlayerInfo.Playerinfo[I].Percentage := 100;
      Case I of
        0: Result := Result OR 1;
        1: Result := Result OR 2;
        2: Result := Result OR 4;
        3: Result := Result OR 8;
        4: Result := Result OR 16;
        5: Result := Result OR 32;
      end;
    end;
  end;
end;

exports
PluginInfo, Init, Draw, Finish;

begin

end.
