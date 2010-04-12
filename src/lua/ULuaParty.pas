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

unit ULuaParty;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses ULua;

{ lua c functions from Party table. Enables creating of party modes w/ lua scripts }

{ Party.Register - register party mode at party manager
  arguments: info: table
                   Name: String;          //< Name used as identifier (language strings, etc.). Has to be set.
                   CanNonParty: Boolean   //< mode is playable when not in party mode. defaulted to false if not set
                   CanParty: Boolean      //< mode is playable in party mode. defaulted to false if not set
                   PlayerCount: Table     //< playable with one, two, three etc. players per team. defaulted to no restrictions if not set. (use table constructor e.g. {1, 2, 3) means playable w/ 1, 2 or three players)
                   TeamCount: Table       //< playable with one, two, three etc. different teams. defaulted to no restrictions if not set. (use table constructor e.g. {1, 2, 3) means playable w/ 1, 2 or three players)

                   BeforeSongSelect: String   //< name of global that will be called before song select screen is shown (if nil, not callable or returns true, default action will be executed)
                   AfterSongSelect: String    //< name of global that will be called after song is selected (if nil, not callable or returns true, default action will be executed)

                   BeforeSing: String         //< name of global that will be called before song select screen is shown (if nil, not callable or returns true, default action will be executed)
                   OnSing: String             //< name of global that will be called before song select screen is shown (if nil, not callable or returns true, default action will be executed)
                   AfterSing: String          //< name of global that will be called before song select screen is shown (if nil, not callable or returns true, default action will be executed)}
function ULuaParty_Register(L: Plua_State): Integer; cdecl;

{ Party.GameFinished - returns true if no party game is running or all rounds
  of current game were played }
function ULuaParty_GameFinished(L: Plua_State): Integer; cdecl;

(* Party.SetRoundRanking - sets ranking of current party round,
  arguments: Ranking: table
  ranking of team i is the value (integer from 1 to number of teams) of the
  table with index [i: number].
  you may call this function in the following way:
    Party.SetRoundRanking({3, 1, 2});
    this means: team 1 is ranked third, team 2 is ranked first and team 3 is
    ranked second.
  if no party game is started or party game is finished
  it will raise an error *)
function ULuaParty_SetRoundRanking(L: Plua_State): Integer; cdecl;

{ Party.GetTeams - returns a table with all information and structure as
  in the TPartyGame.Teams array }
function ULuaParty_GetTeams(L: Plua_State): Integer; cdecl;

{ Party.SetTeams - changes all fields from TPartyGame.Teams that have been
  set in the table given as first argument}
function ULuaParty_SetTeams(L: Plua_State): Integer; cdecl;

const
  ULuaParty_Lib_f: array [0..4] of lual_reg = (
    (name:'Register'; func:ULuaParty_Register),
    (name:'GameFinished'; func:ULuaParty_GameFinished),
    (name:'SetRoundRanking'; func:ULuaParty_SetRoundRanking),
    (name:'GetTeams'; func:ULuaParty_GetTeams),
    (name:'SetTeams'; func:ULuaParty_SetTeams)
  );

implementation
uses ULuaCore, ULuaUtils, UParty, SysUtils;


{ Party.Register - register party mode at party manager
  arguments: info: table
                   Name: String;          //< Name used as identifier (language strings, etc.). Has to be set.
                   CanNonParty: Boolean   //< mode is playable when not in party mode. defaulted to false if not set
                   CanParty: Boolean      //< mode is playable in party mode. defaulted to false if not set
                   PlayerCount: Table     //< playable with one, two, three etc. players per team. defaulted to no restrictions if not set. (use table constructor e.g. {1, 2, 3) means playable w/ 1, 2 or three players)
                   TeamCount: Table       //< playable with one, two, three etc. different teams. defaulted to no restrictions if not set. (use table constructor e.g. {1, 2, 3) means playable w/ 1, 2 or three players)

                   BeforeSongSelect: String   //< name of global that will be called before song select screen is shown (if nil, not callable or returns true, default action will be executed)
                   AfterSongSelect: String    //< name of global that will be called after song is selected (if nil, not callable or returns true, default action will be executed)

                   BeforeSing: String         //< name of global that will be called before song select screen is shown (if nil, not callable or returns true, default action will be executed)
                   OnSing: String             //< name of global that will be called before song select screen is shown (if nil, not callable or returns true, default action will be executed)
                   AfterSing: String          //< name of global that will be called before song select screen is shown (if nil, not callable or returns true, default action will be executed)}
function ULuaParty_Register(L: Plua_State): Integer; cdecl;
  var
    Info: TParty_ModeInfo;
    Key: String;
    P: TLuaPlugin;
begin
  Result := 0;
  
  // check for table on stack
  luaL_checkType(L, 1, LUA_TTABLE);

  // get parent id
  P := Lua_GetOwner(L);


  // set mode info to default
  Party.DefaultModeInfo(Info);


  // set parent in info rec and pop it from stack
  Info.Parent := P.Id;

  // go through table elements
  lua_pushNil(L);
  while (lua_Next(L, 1) <> 0) do
  begin
    Key := lowercase(lua_ToString(L, -2));

    if (Key = 'name') and lua_isString(L, -1) then
      Info.Name := lua_toString(L, -1)
    else if (Key = 'cannonparty') and lua_isBoolean(L, -1) then
      Info.CanNonParty := lua_toBoolean(L, -1)
    else if (Key = 'canparty') and lua_isBoolean(L, -1) then
      Info.CanParty := lua_toBoolean(L, -1)
    else if (Key = 'playercount') and lua_isTable(L, -1) then
      Info.PlayerCount := lua_toBinInt(L, -1)
    else if (Key = 'teamcount') and lua_isTable(L, -1) then
      Info.TeamCount := lua_toBinInt(L, -1)
    else if (Key = 'beforesongselect') and lua_isString(L, -1) then
      Info.Functions.BeforeSongSelect := lua_toString(L, -1)
    else if (Key = 'aftersongselect') and lua_isString(L, -1) then
      Info.Functions.AfterSongSelect := lua_toString(L, -1)
    else if (Key = 'beforesing') and lua_isString(L, -1) then
      Info.Functions.BeforeSing := lua_toString(L, -1)
    else if (Key = 'onsing') and lua_isString(L, -1) then
      Info.Functions.OnSing := lua_toString(L, -1)
    else if (Key = 'aftersing') and lua_isString(L, -1) then
      Info.Functions.AfterSing := lua_toString(L, -1);

    // pop value from stack so key is on top
    lua_pop(L, 1);
  end;

  // clear stack from table
  lua_pop(L, lua_gettop(L));

  if not Party.RegisterMode(Info) then
    luaL_error(L, PChar('can''t register party mode at party manager in Party.Register. Is Info.Name defined or is there another mode with this name?'));
end;

{ Party.GameFinished - returns true if no party game is running or all rounds
  of current game were played }
function ULuaParty_GameFinished(L: Plua_State): Integer; cdecl;
begin
  // clear stack
  lua_pop(L, lua_gettop(L));

  // push result
  lua_pushBoolean(L, Party.GameFinished);

  //we return one value
  Result := 1;
end;

{ Party.SetRoundRanking - sets ranking of current party round,
  if no party game is started or party game is finished
  it will raise an error }
function ULuaParty_SetRoundRanking(L: Plua_State): Integer; cdecl;
var
  R: AParty_TeamRanking;
  I: Integer;
  Rank: Integer;
begin
  Result := 0;

  luaL_checktype(L, 1, LUA_TTABLE);

  lua_checkstack(L, 1);

  SetLength(R, Length(Party.Teams));

  for I := 0 to High(R) do
  begin
    lua_pushInteger(L, (I+1));
    lua_gettable(L, 1);

    R[I].Rank := Length(R);
    R[I].Team := I;
    if (lua_isnumber(L, -1)) then
    begin
      Rank := lua_toInteger(L, -1);
      if (Rank >= 1) and (Rank <= Length(R)) then
        R[I].Rank := Rank
    end;

    lua_pop(L, 1);
    
  end;

  // pop table
  lua_pop(L, 1);

  if (not Party.SetRanking(R)) then
    luaL_error(L, PChar('cann''t set party round ranking. Is party started and not finished yet?'));
end;

{ Party.GetTeams - returns a table with all information and structure as
  in the TPartyGame.Teams array }
function ULuaParty_GetTeams(L: Plua_State): Integer; cdecl;
  var
    Team: Integer;
    Player: Integer;
begin
  // clear stack
  lua_pop(L, lua_gettop(L));

  // ensure we have enough stack slots left
  lua_checkstack(L, 7);

  // create the table we want to return
  lua_createtable(L, Length(Party.Teams), 0);

  // add the teams
  for Team := 0 to High(Party.Teams) do
  begin
    // push key for current teams value. lua array beggins at 1
    lua_pushInteger(L, Team + 1);

    // push table containing team info and players table
    lua_createtable(L, 0, 5);

    // team name
    lua_pushString(L, PChar(Party.Teams[Team].Name));
    lua_setField(L, -2, 'Name');

    // team score
    lua_pushInteger(L, Party.Teams[Team].Score);
    lua_setField(L, -2, 'Score');

    // team jokers left
    lua_pushInteger(L, Party.Teams[Team].JokersLeft);
    lua_setField(L, -2, 'JokersLeft');

    // team nextPlayer
    lua_pushInteger(L, Party.Teams[Team].NextPlayer);
    lua_setField(L, -2, 'NextPlayer');

    // team players table
    lua_createtable(L, Length(Party.Teams[Team].Players), 0);

    //add players
    for Player := 0 to High(Party.Teams[Team].Players) do
    begin
      // push key for current players value. lua array beggins at 1
      lua_pushInteger(L, Player + 1);

      // push table containing player info
      lua_createTable(L, 0, 2);

      // player name
      lua_PushString(L, PChar(Party.Teams[Team].Players[Player].Name));
      lua_SetField(L, -2, 'Name');

      // players times played
      lua_PushInteger(L, Party.Teams[Team].Players[Player].TimesPlayed);
      lua_SetField(L, -2, 'TimesPlayed');

      // add value - key - pair to teams player table
      lua_setTable(L, -3);
    end;

    lua_setField(L, -2, 'Players');

    // add value - key - pair to returned table
    lua_setTable(L, -3);
  end;

  // we return 1 value (the first table)
  Result := 1;
end;

{ Party.SetTeams - changes all fields from TPartyGame.Teams that have been
  set in the table given as first argument}
function ULuaParty_SetTeams(L: Plua_State): Integer; cdecl;

  procedure Do_Player(Team, Player: Integer);
    var
      Key: String;
  begin
    if (Player >= 0) and (Player <= High(Party.Teams[Team].Players)) then
    begin
      // go through table elements
      lua_pushNil(L);
      while (lua_Next(L, -2) <> 0) do
      begin
        Key := lowercase(lua_ToString(L, -2));

        if (Key = 'name') and lua_isString(L, -1) then
          Party.Teams[Team].Players[Player].Name := lua_toString(L, -1)
        else if (Key = 'timesplayed') and lua_isNumber(L, -1) then
          Party.Teams[Team].Players[Player].TimesPlayed := lua_toInteger(L, -1);

        // pop value from stack so key is on top
        lua_pop(L, 1);
      end;
    end;
  end;

  procedure Do_Players(Team: Integer);
  begin
    // go through table elements
    lua_pushNil(L);
    while (lua_Next(L, -2) <> 0) do
    begin
      // check if key is a number and value is a table
      if (lua_isNumber(L, -2)) and (lua_isTable(L, -1)) then
        Do_Player(Team, lua_toInteger(L, -2));

      // pop value from stack so key is on top
      lua_pop(L, 1);
    end;
  end;

  procedure Do_Team(Team: Integer);
    var
      Key: String;
  begin
    if (Team >= 0) and (Team <= High(Party.Teams)) then
    begin
      // go through table elements
      lua_pushNil(L);
      while (lua_Next(L, -2) <> 0) do
      begin
        Key := lowercase(lua_ToString(L, -2));

        if (Key = 'name') and lua_isString(L, -1) then
          Party.Teams[Team].Name := lua_toString(L, -1)
        else if (Key = 'score') and lua_isNumber(L, -1) then
          Party.Teams[Team].Score := lua_toInteger(L, -1)
        else if (Key = 'jokersleft') and lua_isNumber(L, -1) then
          Party.Teams[Team].JokersLeft := lua_toInteger(L, -1)
        else if (Key = 'currentplayer') and lua_isNumber(L, -1) then
          Party.Teams[Team].NextPlayer := lua_toInteger(L, -1)
        else if (Key = 'players') and lua_isTable(L, -1) then
          Do_Players(Team);

        // pop value from stack so key is on top
        lua_pop(L, 1);
      end;
    end;
  end;
begin
  Result := 0;
  
  // check for table on stack
  luaL_checkType(L, 1, LUA_TTABLE);


  // go through table elements
  lua_pushNil(L);
  while (lua_Next(L, 1) <> 0) do
  begin
    // check if key is a number and value is a table
    if (lua_isNumber(L, -2)) and (lua_isTable(L, -1)) then
      Do_Team(lua_toInteger(L, -2));

    // pop value from stack so key is on top
    lua_pop(L, 1);
  end;

  // clear stack from table
  lua_pop(L, lua_gettop(L));
end;

end.