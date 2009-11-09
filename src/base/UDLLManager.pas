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
 * $URL$
 * $Id$
 *}

unit UDLLManager;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  ModiSDK,
  UFiles,
  UPath,
  UFilesystem;

type
  TDLLMan = class
    private
      hLib:     THandle;
      P_Init:   fModi_Init;
      P_Draw:   fModi_Draw;
      P_Finish: fModi_Finish;
      P_RData:  pModi_RData;
    public
      Plugins: array of TPluginInfo;
      PluginPaths: array of IPath;
      Selected: ^TPluginInfo;

      constructor Create;

      procedure GetPluginList;
      procedure ClearPluginInfo(No: cardinal);
      function  LoadPluginInfo(const Filename: IPath; No: cardinal): boolean;

      function  LoadPlugin(No: cardinal): boolean;
      procedure UnLoadPlugin;

      function  PluginInit   (const TeamInfo:   TTeamInfo; 
                              var   Playerinfo: TPlayerinfo;
			      const Sentences:  TSentences;
			      const LoadTex:    fModi_LoadTex;
			      const Print:      fModi_Print;
			            LoadSound:  fModi_LoadSound;
				    PlaySound:  pModi_PlaySound)
			     : boolean;
      function  PluginDraw   (var Playerinfo: TPlayerinfo; const CurSentence: cardinal): boolean;
      function  PluginFinish (var Playerinfo: TPlayerinfo): byte;
      procedure PluginRData  (handle: HSTREAM; buffer: Pointer; len: dword; user: dword);
  end;

var
  DLLMan: TDLLMan;

const
{$IF Defined(MSWINDOWS)}
  DLLExt  = '.dll';
{$ELSEIF Defined(DARWIN)}
  DLLExt  = '.dylib';
{$ELSEIF Defined(UNIX)}
  DLLExt  = '.so';
{$IFEND}

implementation

uses
  {$IFDEF MSWINDOWS}
  windows,
  {$ELSE}
  dynlibs,
  {$ENDIF}
  UPathUtils,
  ULog,
  SysUtils;


constructor TDLLMan.Create;
begin
  inherited;
  SetLength(Plugins, 0);
  SetLength(PluginPaths, Length(Plugins));
  GetPluginList;
end;

procedure TDLLMan.GetPluginList;
var
  Iter: IFileIterator;
  FileInfo: TFileInfo;
begin
  Iter := FileSystem.FileFind(PluginPath.Append('*' + DLLExt), 0);
  while (Iter.HasNext) do
  begin
    SetLength(Plugins, Length(Plugins)+1);
    SetLength(PluginPaths, Length(Plugins));

    FileInfo := Iter.Next;

    if LoadPluginInfo(FileInfo.Name, High(Plugins)) then // loaded succesful
    begin
      PluginPaths[High(PluginPaths)] := FileInfo.Name;
    end
    else // error loading
    begin
      SetLength(Plugins, Length(Plugins)-1);
      SetLength(PluginPaths, Length(Plugins));
    end;
  end;
end;

procedure TDLLMan.ClearPluginInfo(No: cardinal);
begin
// set to party modi plugin
  Plugins[No].Typ := 8;

  Plugins[No].Name := 'unknown';
  Plugins[No].NumPlayers := 0;

  Plugins[No].Creator := 'Nobody';
  Plugins[No].PluginDesc := 'NO_PLUGIN_DESC';

  Plugins[No].LoadSong  := true;
  Plugins[No].ShowScore := true;
  Plugins[No].ShowBars  := true;
  Plugins[No].ShowNotes := true;
  Plugins[No].LoadVideo := true;
  Plugins[No].LoadBack  := true;

  Plugins[No].TeamModeOnly := true;
  Plugins[No].GetSoundData := true;
  Plugins[No].Dummy := true;


  Plugins[No].BGShowFull   := true;
  Plugins[No].BGShowFull_O := true;

  Plugins[No].ShowRateBar   := true;
  Plugins[No].ShowRateBar_O := true;

  Plugins[No].EnLineBonus   := true;
  Plugins[No].EnLineBonus_O := true;
end;

function TDLLMan.LoadPluginInfo(const Filename: IPath; No: cardinal): boolean;
var
  hLibg: THandle;
  Info: pModi_PluginInfo;
//  I: integer;
begin
  Result := true;
// clear plugin info
  ClearPluginInfo(No);

{
// workaround plugins loaded 2 times
  for i := low(pluginpaths) to high(pluginpaths) do
    if (pluginpaths[i] = filename) then
      exit;
}

// load libary
  hLibg := LoadLibrary(PChar(PluginPath.Append(Filename).ToNative));
// if loaded
  if (hLibg <> 0) then
  begin
// load info procedure
    @Info := GetProcAddress(hLibg, PChar('PluginInfo'));

// if loaded
    if (@Info <> nil) then
    begin
// load plugininfo
      Info(Plugins[No]);
      Result := true;
    end
    else
      Log.LogError('Could not load plugin "' + Filename.ToNative + '": Info procedure not found');

    FreeLibrary (hLibg);
  end
  else
    Log.LogError('Could not load plugin "' + Filename.ToNative + '": Libary not loaded');
end;

function TDLLMan.LoadPlugin(No: cardinal): boolean;
begin
  Result := true;
// load libary
  hLib := LoadLibrary(PChar(PluginPath.Append(PluginPaths[No]).ToNative));
// if loaded
  if (hLib <> 0) then
  begin
// load info procedure
    @P_Init := GetProcAddress (hLib, 'Init');
    @P_Draw := GetProcAddress (hLib, 'Draw');
    @P_Finish := GetProcAddress (hLib, 'Finish');

// if loaded
    if (@P_Init <> nil) and (@P_Draw <> nil) and (@P_Finish <> nil) then
    begin
      Selected := @Plugins[No]; 
      Result := true;
    end
    else
    begin
      Log.LogError('Could not load plugin "' + PluginPaths[No].ToNative + '": Procedures not found');
    end;
  end
  else
    Log.LogError('Could not load plugin "' + PluginPaths[No].ToNative + '": Libary not loaded');
end;

procedure TDLLMan.UnLoadPlugin;
begin
  if (hLib <> 0) then
    FreeLibrary (hLib);

//  Selected := nil;
  @P_Init   := nil;
  @P_Draw   := nil;
  @P_Finish := nil;
  @P_RData  := nil;
end;

function TDLLMan.PluginInit (const TeamInfo:   TTeamInfo;
                             var   Playerinfo: TPlayerinfo;
			     const Sentences:  TSentences;
			     const LoadTex:    fModi_LoadTex;
			     const Print:      fModi_Print;
			           LoadSound:  fModi_LoadSound;
			           PlaySound:  pModi_PlaySound)
			    : boolean;
var
  Methods: TMethodRec;
begin
  Methods.LoadTex := LoadTex;
  Methods.Print := Print;
  Methods.LoadSound := LoadSound;
  Methods.PlaySound := PlaySound;
  
  if (@P_Init <> nil) then
    Result := P_Init (TeamInfo, PlayerInfo, Sentences, Methods)
  else
    Result := true
end;

function TDLLMan.PluginDraw   (var Playerinfo: TPlayerinfo; const CurSentence: cardinal): boolean;
begin
  if (@P_Draw <> nil) then
    Result := P_Draw (PlayerInfo, CurSentence)
  else
    Result := true
end;

function TDLLMan.PluginFinish (var Playerinfo: TPlayerinfo): byte;
begin
  if (@P_Finish <> nil) then
    Result := P_Finish (PlayerInfo)
  else
    Result := 0;
end;

procedure TDLLMan.PluginRData  (handle: HStream; buffer: Pointer; len: dword; user: dword);
begin
if (@P_RData <> nil) then
  P_RData (handle, buffer, len, user);
end;

end.
