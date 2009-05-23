library Hold_The_Line;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  SysUtils,
  ModiSDK in '..\SDK\ModiSDK.pas',
  gl      in '..\..\src\lib\JEDI-SDL\OpenGL\Pas\gl.pas';

var
  PointerTex:     TSmallTexture;
  CountSentences: cardinal;
  Limit:          byte;
  MethodRec:      TMethodRec;
//  Frame:          integer;
  PlayerTimes:    array[0..5] of integer;
  LastTick:       cardinal;
  PointerVisible: boolean;

  DismissedSound: cardinal;

// Give the plugin's info
procedure PluginInfo (var Info: TPluginInfo); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
begin
  Info.Name       := 'PLUGIN_HDL_NAME';

  Info.Creator    := 'Whiteshark';
  Info.PluginDesc := 'PLUGIN_HDL_DESC';

  // Set to Party Modi Plugin
  Info.Typ        := 8;

  Info.NumPlayers := 31;
  // Options
  Info.LoadSong   := true; // Whether or not a song should be loaded
  // Only when song is loaded:
  Info.ShowScore  := true; // Whether or not the score should be shown
  Info.ShowNotes  := true; // Whether the note lines should be displayed
  Info.LoadVideo  := true; // Should the video be loaded?
  Info.LoadBack   := true; // Should the background be loaded?

  Info.BGShowFull    := false; // Whether the background or the video should be shown full size
  Info.BGShowFull_O  := true;  // Whether the Background or the Video should be shown full size

  Info.ShowRateBar   := true;  // Whether the bar that shows how good the player was should be displayed
  Info.ShowRateBar_O := false; // Load from ini whether the bar should be displayed

  Info.EnLineBonus   := false; // Whether line bonus should be enabled
  Info.EnLineBonus_O := true;  // Load from ini whether line bonus should be enabled

  // Options even when song is not loaded
  Info.ShowBars      := false; // Whether the white bars on top and bottom should be drawn
  Info.TeamModeOnly  := false; // if true the plugin can only be played in team mode
  Info.GetSoundData  := false; // if true the rdata procedure is called when new sound data is available
  Info.Dummy         := false; // Should be set to false... for updating plugin interface
end;

// executed on game start. if true game begins, else failure
function Init (const TeamInfo:   TTeamInfo; 
               var   Playerinfo: TPlayerinfo;
	       const Sentences:  TSentences;
	       const Methods:    TMethodRec)
	      : boolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

const
  TextureName : PChar = 'HDL_Pointer';
  SoundName   : PChar = 'dismissed.mp3';
var
  Index:   integer;
//  Texname: PChar;
  TexType: TTextureType;
begin
{
  TexName := CreateStr(PChar('HDL_Pointer'));
  TexType := TEXTURE_TYPE_TRANSPARENT;
  PointerTex := Methods.LoadTex(TexName, TexType);

  FreeStr(TexName);

  TexName := CreateStr(PChar('dismissed.mp3'));
  DismissedSound := Methods.LoadSound (TexName);
  FreeStr(TexName);
}
  TexType := TEXTURE_TYPE_TRANSPARENT;
  PointerTex := Methods.LoadTex(TextureName, TexType);

  DismissedSound := Methods.LoadSound (SoundName);

  CountSentences := Sentences.High;
  Limit := 0;
//  Frame := 0;

  MethodRec := Methods;

  for Index := 0 to PlayerInfo.NumPlayers-1 do
  begin
    PlayerInfo.Playerinfo[Index].Enabled := true;
    PlayerInfo.Playerinfo[Index].Percentage := 100;
    PlayerTimes[Index] := 0;
  end;

  Result := true;
end;

function Draw (var   Playerinfo:  TPlayerinfo; 
               const CurSentence: cardinal)
	      : boolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
const
  SoundName : PChar = 'PARTY_DISMISSED';
var
  Index: integer;
  L:     byte;
  C:     byte;
  Tick:  cardinal;
begin
  // activate blink
  if (CurSentence = CountSentences div 5 * 2 - 1) or (CurSentence = CountSentences div 3 * 2 - 1) then
  begin
    Tick := round(TimeStampToMSecs(DateTimeToTimeStamp(Now))) div 400;
    if (Tick <> LastTick) then
    begin
      LastTick := Tick;
      PointerVisible := not PointerVisible;
    end;
  end
  else
    PointerVisible := true;

  // inc limit
  if (Limit = 0) and  (CurSentence >= CountSentences div 5 * 2) then
    Inc(Limit)
  else if (Limit = 1) and  (CurSentence >= CountSentences div 3 * 2) then
    Inc(Limit);

  case Limit of
    0: L := 20;
    1: L := 50;
    2: L := 75;
  end;

  C:= 0;

  Result := true;

  for Index := 0 to PlayerInfo.NumPlayers-1 do
  begin
    if PlayerInfo.Playerinfo[Index].Enabled then
    begin
      if PlayerInfo.Playerinfo[Index].Bar < L then
      begin
        PlayerInfo.Playerinfo[Index].Enabled := false;
        Inc(C);
        PlayerTimes[Index] := CurSentence; // Save Time of Dismission
        // PlaySound
        MethodRec.PlaySound (DismissedSound);
      end;

      // Draw pointer
      if (PointerVisible) then
      begin
        glColor4f (0.2, 0.8, 0.1, 1);

        glEnable(GL_TEXTURE_2D);
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

        glBindTexture(GL_TEXTURE_2D, PointerTex.TexNum);

        glBegin(GL_QUADS);
          glTexCoord2f(1/32, 0); glVertex2f(PlayerInfo.Playerinfo[Index].PosX + L - 3, PlayerInfo.Playerinfo[Index].PosY - 4);
          glTexCoord2f(1/32, 1); glVertex2f(PlayerInfo.Playerinfo[Index].PosX + L - 3, PlayerInfo.Playerinfo[Index].PosY + 12);
          glTexCoord2f(31/32, 1); glVertex2f(PlayerInfo.Playerinfo[Index].PosX+ L + 3, PlayerInfo.Playerinfo[Index].PosY + 12);
          glTexCoord2f(31/32, 0); glVertex2f(PlayerInfo.Playerinfo[Index].PosX+ L + 3, PlayerInfo.Playerinfo[Index].PosY - 4);
        glEnd;

        glDisable(GL_TEXTURE_2D);
        glDisable(GL_BLEND);
      end;

    end
    else
    begin
      Inc(C);
      // Draw dismissed
      glColor4f (0.8, 0.8, 0.8, 1);
      MethodRec.Print (1, 18, PlayerInfo.Playerinfo[Index].PosX, PlayerInfo.Playerinfo[Index].PosY-8, SoundName);
    end;
  end;
  if (C >= PlayerInfo.NumPlayers-1) then
    Result := false;
end;

// is executed on finish, returns the player number of the winner
function Finish (var Playerinfo: TPlayerinfo): byte; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
var
  Index: integer;
begin
  Result := 0;
  for Index := 0 to PlayerInfo.NumPlayers-1 do
  begin
  PlayerInfo.Playerinfo[Index].Percentage := (PlayerTimes[Index] * 100) div CountSentences;
    if (PlayerInfo.Playerinfo[Index].Enabled) then
    begin
      PlayerInfo.Playerinfo[Index].Percentage := 100;
      case Index of
        0: Result := Result or 1;
        1: Result := Result or 2;
        2: Result := Result or 4;
        3: Result := Result or 8;
        4: Result := Result or 16;
        5: Result := Result or 32;
      end;
    end;
  end;
end;

exports
  PluginInfo,
  Init,
  Draw,
  Finish;

begin

end.
