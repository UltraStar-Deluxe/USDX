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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/branches/experimental/Lua/src/lua/ULuaTexture.pas $
 * $Id: ULuaTexture.pas 1551 2009-01-04 14:08:33Z Hawkear $
 *}

unit ULuaScreenSing;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UIni,
  ULua;

{ returns a table with following structure:
    t[1..playercount] = score of player i }
function ULuaScreenSing_GetScores(L: Plua_State): Integer; cdecl;

{ returns a table with following structure:
    t[1..playercount] = note score of player i }
function ULuaScreenSing_GetNoteScores(L: Plua_State): Integer; cdecl;

{ returns a table with following structure:
    t[1..playercount] = golden score of player i }
function ULuaScreenSing_GetGoldenScores(L: Plua_State): Integer; cdecl;

{ returns a table with following structure:
    t[1..playercount] = line score of player i }
function ULuaScreenSing_GetLineScores(L: Plua_State): Integer; cdecl;

{ returns a table with following structure:
    t[1..playercount] = name of player i }
function ULuaScreenSing_GetPlayerNames(L: Plua_State): Integer; cdecl;

{ returns a table with following structure:
    t[1..playercount] = color of player i }
function ULuaScreenSing_GetPlayerColors(L: Plua_State): Integer; cdecl;

{ returns a table with following structure:
    t[1..playercount] = level of player i }
function ULuaScreenSing_GetPlayerLevels(L: Plua_State): Integer; cdecl;

{ returns a table with following structure:
    t[1..playercount] = rating of player i range: [0..1] }
function ULuaScreenSing_GetRating(L: Plua_State): Integer; cdecl;

{ returns a table with following structure:
    t[1..playercount] = rect of players score background: table(x, y, w, h) }
function ULuaScreenSing_GetScoreBGRect(L: Plua_State): Integer; cdecl;

{ returns a table with following structure:
    t[1..playercount] = rect of players rating bar: table(x, y, w, h) }
function ULuaScreenSing_GetRBRect(L: Plua_State): Integer; cdecl;

{ ScreenSing.GetBPM - no arguments
  returns the beats per minutes of the current song in quarts }
function ULuaScreenSing_GetBPM(L: Plua_State): Integer; cdecl;

{ ScreenSing.GetTotalBeats - no arguments
  returns the total beats of the current song in quarts }
function ULuaScreenSing_GetTotalBeats(L: Plua_State): Integer; cdecl;

{ ScreenSing.GetBackground - no arguments
  returns the background image path of the current song }
function ULuaScreenSing_GetBackground(L: Plua_State): Integer; cdecl;

{ ScreenSing.GetArtist - no arguments
  returns the artist of the current song }
function ULuaScreenSing_GetArtist(L: Plua_State): Integer; cdecl;

{ ScreenSing.GetTitle - no arguments
  returns the title of the current song }
function ULuaScreenSing_GetTitle(L: Plua_State): Integer; cdecl;

{ ScreenSing.GetMD5 - no arguments
  returns the md5 of the current song }
function ULuaScreenSing_GetMD5(L: Plua_State): Integer; cdecl;

{ ScreenSing.GetSungToEndWithoutPausing - no arguments
  returns whether the current/last song was sung without pausing and to the end}
function ULuaScreenSing_GetSungToEndWithoutPausing(L: Plua_State): Integer; cdecl;

{ ScreenSing.GetCurrentScreen - no arguments
  returns the name of the current screen }
function ULuaScreenSing_GetCurrentScreen(L: Plua_State): Integer; cdecl;

{ ScreenSing.BeatsToSeconds(Beats: float)
  returns the time in seconds that the given number of beats (in quarts) last }
function ULuaScreenSing_BeatsToSeconds(L: Plua_State): Integer; cdecl;

{ ScreenSing.SecondsToBeats(Seconds: float)
  returns the Beats in quarts that the given seconds last }
function ULuaScreenSing_SecondsToBeats(L: Plua_State): Integer; cdecl;

{ ScreenSing.GetBeat() - returns current beat of lyricstate (in quarts) }
function ULuaScreenSing_GetBeat(L: Plua_State): Integer; cdecl;

{ finishes current song, if sing screen is not shown it will raise
  an error }
function ULuaScreenSing_Finish(L: Plua_State): Integer; cdecl;

{ ScreenSing.GetSettings - no arguments
  returns a table filled with the data of TScreenSing.Settings }
function ULuaScreenSing_GetSettings(L: Plua_State): Integer; cdecl;

{ ScreenSing.SetSettings - arguments: Table
  sets all attributes of TScreenSing.Settings that are
  unequal to nil in Table }
function ULuaScreenSing_SetSettings(L: Plua_State): Integer; cdecl;

{ ScreenSing.GetSongLines - no arguments
  returns a table filled with lines of the loaded song or
  nil if no song is loaded (singscreen is not displayed)
  structure of returned table:
    array [1.."count of lines"]
     \
     | Start: integer - beat the line is displayed at (on top of lyrics display)
     | Lyric: string  - full lyric of the line
     | Notes: array [1.."count notes of this line"]
     \
      | Start: integer    - beat the note starts at
      | Length: integer   - length in beats
      | Tone: integer     - pitch that has to be sung, full range
      | NoteType: integer - 0 for freestyle, 1 for normal, 2 for golden
      | Text: string      - text of this fragment }
function ULuaScreenSing_GetSongLines(L: Plua_State): Integer; cdecl;

function ULuaScreenSing_GetPlayerNotes(L: Plua_State): Integer; cdecl;

{ THESE SHOULD BE IN ULuaScreenSong INSTEAD }

{ ScreenSong.GetVisibleSongs - no arguments
  returns number of visible songs }
  function ULuaScreenSong_GetVisibleSongs(L: Plua_State): Integer; cdecl;
{ ScreenSong.GetSelectedIndex - no arguments
  returns index of currently selected song [0...visible songs -1] }
  function ULuaScreenSong_GetSelectedIndex(L: Plua_State): Integer; cdecl;
{ ScreenSong.GetSongNumbers - no arguments
  returns "<selected> / <total>" }
  function ULuaScreenSong_GetSongNumbers(L: Plua_State): Integer; cdecl;
{ ScreenSong.GetSelectedSong - no arguments
  returns Artist,Title,Year of currently selected song }
  function ULuaScreenSong_GetSelectedSong(L: Plua_State): Integer; cdecl;

const
  ULuaScreenSing_Lib_f: array [0..29] of lual_reg = (
    (name:'GetScores';func:ULuaScreenSing_GetScores),
    (name:'GetNoteScores';func:ULuaScreenSing_GetNoteScores),
    (name:'GetGoldenScores';func:ULuaScreenSing_GetGoldenScores),
    (name:'GetLineScores';func:ULuaScreenSing_GetLineScores),
    (name:'GetPlayerNames';func:ULuaScreenSing_GetPlayerNames),
    (name:'GetPlayerColors';func:ULuaScreenSing_GetPlayerColors),
    (name:'GetPlayerLevels';func:ULuaScreenSing_GetPlayerLevels),
    (name:'GetRating';func:ULuaScreenSing_GetRating),
    (name:'GetBPM';func:ULuaScreenSing_GetBPM),
    (name:'GetTotalBeats';func:ULuaScreenSing_GetTotalBeats),
    (name:'GetBackground';func:ULuaScreenSing_GetBackground),
    (name:'GetArtist';func:ULuaScreenSing_GetArtist),
    (name:'GetTitle';func:ULuaScreenSing_GetTitle),
    (name:'GetMD5';func:ULuaScreenSing_GetMD5),
    (name:'GetSungToEndWithoutPausing';func:ULuaScreenSing_GetSungToEndWithoutPausing),
    (name:'GetCurrentScreen';func:ULuaScreenSing_GetCurrentScreen),
    (name:'BeatsToSeconds';func:ULuaScreenSing_BeatsToSeconds),
    (name:'SecondsToBeats';func:ULuaScreenSing_SecondsToBeats),
    (name:'GetBeat';func:ULuaScreenSing_GetBeat),
    (name:'GetScoreBGRect';func:ULuaScreenSing_GetScoreBGRect),
    (name:'GetRBRect';func:ULuaScreenSing_GetRBRect),
    (name:'Finish';func:ULuaScreenSing_Finish),
    (name:'GetSettings';func:ULuaScreenSing_GetSettings),
    (name:'SetSettings';func:ULuaScreenSing_SetSettings),
    (name:'GetSongLines';func:ULuaScreenSing_GetSongLines),
    (name:'GetPlayerNotes';func:ULuaScreenSing_GetPlayerNotes),
    {next ones are for ULuaScreenSong instead}
    {TODO: most of these do not actually work}
    (name:'GetVisibleSongs';func:ULuaScreenSong_GetVisibleSongs),
    (name:'GetSelectedIndex';func:ULuaScreenSong_GetSelectedIndex),
    (name:'GetSongNumbers';func:ULuaScreenSong_GetSongNumbers),
    (name:'GetSelectedSong';func:ULuaScreenSong_GetSelectedSong)
  );

implementation
uses
  UScreenSingController,
  UNote,
  UDisplay,
  UGraphic,
  UMusic,
  ULuaUtils,
  {next imports are for ULuaScreenSong instead}
  USong,
  USongs,
  UThemes,
  {this was always here}
  SysUtils;

{ returns a table with following structure:
    t[1..playercount] = score of player i }
function ULuaScreenSing_GetScores(L: Plua_State): Integer; cdecl;
  var
    Top: Integer;
    I: Integer;
begin
  Result := 1;

  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  // create table
  lua_createtable(L, Length(Player), 0);

  // fill w/ values
  for I := 0 to High(Player) do
  begin
    lua_pushInteger(L, I + 1);
    lua_pushInteger(L, Player[I].ScoreTotalInt);

    lua_settable(L, -3);
  end;

  // leave table on stack, it is our result
end;

{ returns a table with following structure:
    t[1..playercount] = note score of player i }
function ULuaScreenSing_GetNoteScores(L: Plua_State): Integer; cdecl;
  var
    Top: Integer;
    I: Integer;
begin
  Result := 1;

  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  // create table
  lua_createtable(L, Length(Player), 0);

  // fill w/ values
  for I := 0 to High(Player) do
  begin
    lua_pushInteger(L, I + 1);
    lua_pushInteger(L, Player[I].ScoreInt);

    lua_settable(L, -3);
  end;

  // leave table on stack, it is our result
end;

{ returns a table with following structure:
    t[1..playercount] = golden score of player i }
function ULuaScreenSing_GetGoldenScores(L: Plua_State): Integer; cdecl;
  var
    Top: Integer;
    I: Integer;
begin
  Result := 1;

  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  // create table
  lua_createtable(L, Length(Player), 0);

  // fill w/ values
  for I := 0 to High(Player) do
  begin
    lua_pushInteger(L, I + 1);
    lua_pushInteger(L, Player[I].ScoreGoldenInt);

    lua_settable(L, -3);
  end;

  // leave table on stack, it is our result
end;

{ returns a table with following structure:
    t[1..playercount] = line score of player i }
function ULuaScreenSing_GetLineScores(L: Plua_State): Integer; cdecl;
  var
    Top: Integer;
    I: Integer;
begin
  Result := 1;

  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  // create table
  lua_createtable(L, Length(Player), 0);

  // fill w/ values
  for I := 0 to High(Player) do
  begin
    lua_pushInteger(L, I + 1);
    lua_pushInteger(L, Player[I].ScoreLineInt);

    lua_settable(L, -3);
  end;

  // leave table on stack, it is our result
end;

{ returns a table with following structure:
    t[1..playercount] = name of player i }
function ULuaScreenSing_GetPlayerNames(L: Plua_State): Integer; cdecl;
  var
    Top: Integer;
    I: Integer;
begin
  Result := 1;

  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  // create table
  lua_createtable(L, Length(Player), 0);

  // fill w/ values
  for I := 0 to High(Player) do
  begin
    lua_pushInteger(L, I + 1);
    lua_pushstring(L, PChar(Ini.Name[I]));

    lua_settable(L, -3);
  end;

  // leave table on stack, it is our result
end;

{ returns a table with following structure:
    t[1..playercount] = color of player i }
function ULuaScreenSing_GetPlayerColors(L: Plua_State): Integer; cdecl;
  var
    Top: Integer;
    I: Integer;
begin
  Result := 1;

  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  // create table
  lua_createtable(L, Length(Player), 0);

  // fill w/ values
  for I := 0 to High(Player) do
  begin
    lua_pushInteger(L, I + 1);
    lua_pushInteger(L, Ini.PlayerColor[I]);

    lua_settable(L, -3);
  end;

  // leave table on stack, it is our result
end;

{ returns a table with following structure:
    t[1..playercount] = level of player i }
function ULuaScreenSing_GetPlayerLevels(L: Plua_State): Integer; cdecl;
  var
    Top: Integer;
    I: Integer;
begin
  Result := 1;

  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  // create table
  lua_createtable(L, Length(Player), 0);

  // fill w/ values
  for I := 0 to High(Player) do
  begin
    lua_pushInteger(L, I + 1);
    lua_pushInteger(L, Ini.PlayerLevel[I]);

    lua_settable(L, -3);
  end;

  // leave table on stack, it is our result
end;

{ returns a table with following structure:
    t[1..playercount] = rating of player i range: [0..1] }
function ULuaScreenSing_GetRating(L: Plua_State): Integer; cdecl;
  var
    Top: Integer;
    I: Integer;
begin
  Result := 1;

  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  // create table
  lua_createtable(L, Length(Player), 0);

  // fill w/ values
  for I := 0 to High(ScreenSing.Scores.Players) do
  begin
    lua_pushInteger(L, I + 1);
    lua_pushNumber(L, ScreenSing.Scores.Players[I].RBPos);

    lua_settable(L, -3);
  end;

  // leave table on stack, it is our result
end;

{ ScreenSing.GetBPM - no arguments
  returns the beats per minutes of the current song in quarts }
function ULuaScreenSing_GetBPM(L: Plua_State): Integer; cdecl;
begin
  lua_ClearStack(L);
  Result := 1;

  if (CurrentSong = nil) or (Length(CurrentSong.BPM) = 0) or (Display.CurrentScreen <> @ScreenSing) then
    lua_PushNumber(L, 0) // in case of error
  else if (Length(CurrentSong.BPM) = 1) then
    lua_PushNumber(L, CurrentSong.BPM[0].BPM)
  else
  begin
    // to-do: do this for songs w/ BPM changes
    //        or drop support for BPM changes?!
  end;
end;

{ ScreenSing.GetTotalBeats - no arguments
  returns the total beats minutes of the current song in quarts }
function ULuaScreenSing_GetTotalBeats(L: Plua_State): Integer; cdecl;
begin
  lua_ClearStack(L);
  Result := 1;

  if (CurrentSong = nil) or (Length(CurrentSong.BPM) = 0) or (Display.CurrentScreen <> @ScreenSing) then
    lua_PushNumber(L, 0) // in case of error
  else if (Length(CurrentSong.BPM) = 1) then
    lua_PushNumber(L, CurrentSong.BPM[0].BPM*TotalTime/60)
  else
  begin
    // to-do: do this for songs w/ BPM changes
    //        or drop support for BPM changes?!
  end;
end;

{ ScreenSing.GetBackground - no arguments
  returns background image path of the current song, or empty string if unset }
function ULuaScreenSing_GetBackground(L: Plua_State): Integer; cdecl;
begin
  lua_ClearStack(L);
  Result := 1;

  if (CurrentSong = nil) or (CurrentSong.Background = nil) or (CurrentSong.Background.ToNative() = '') or (Display.CurrentScreen <> @ScreenSing) then
    lua_PushString(L, PChar('')) // in case of error
  else begin
    lua_PushString(L, PChar(CurrentSong.Path.GetAbsolutePath().ToNative()+CurrentSong.Background.ToNative()))
  end;
end;

{ ScreenSing.GetArtist - no arguments
  returns artist of the current/last song, or '' if unset }
function ULuaScreenSing_GetArtist(L: Plua_State): Integer; cdecl;
begin
  lua_ClearStack(L);
  Result := 1;

  if (CurrentSong = nil) then
    lua_PushString(L, PChar('')) // in case of error
  else begin
    lua_PushString(L, PChar(CurrentSong.Artist))
  end;
end;

{ ScreenSing.GetTitle - no arguments
  returns title of the current/last song, or '' if unset }
function ULuaScreenSing_GetTitle(L: Plua_State): Integer; cdecl;
begin
  lua_ClearStack(L);
  Result := 1;

  if (CurrentSong = nil) then
    lua_PushString(L, PChar('')) // in case of error
  else begin
    lua_PushString(L, PChar(CurrentSong.Title))
  end;
end;

{ ScreenSing.GetMD5 - no arguments
  returns md5 of the current/last song, or '' if unset }
function ULuaScreenSing_GetMD5(L: Plua_State): Integer; cdecl;
begin
  lua_ClearStack(L);
  Result := 1;

  if (CurrentSong = nil) then
    lua_PushString(L, PChar('')) // in case of error
  else begin
    lua_PushString(L, PChar(CurrentSong.MD5))
  end;
end;

{ ScreenSing.GetSungToEndWithoutPausing - no arguments
  returns whether the current/last song was sung without pausing and to the end}
function ULuaScreenSing_GetSungToEndWithoutPausing(L: Plua_State): Integer; cdecl;
begin
  lua_ClearStack(L);
  Result := 1;

  if (CurrentSong = nil) then
    lua_pushboolean(L, false) // in case of error
  else begin
    lua_pushboolean(L, ScreenSing.SungToEnd and not ScreenSing.SungPaused)
  end;
end;

{ ScreenSing.GetCurrentScreen - no arguments
  returns name of current screen, or 'unknown' if this particular screen is not coded }
function ULuaScreenSing_GetCurrentScreen(L: Plua_State): Integer; cdecl;
begin
  lua_ClearStack(L);
  Result := 1;

  if (Display.CurrentScreen = @ScreenScore) then
    lua_PushString(L, PChar('score'))
  else if (Display.CurrentScreen = @ScreenSing) then
    lua_PushString(L, PChar('sing'))
  else if (Display.CurrentScreen = @ScreenSong) then
    lua_PushString(L, PChar('song'))
  else begin
    lua_PushString(L, PChar('unknown'))
  end;
end;

{ ScreenSing.BeatsToSeconds(Beats: float)
  returns the time in seconds that the given number of beats (in quarts) last }
function ULuaScreenSing_BeatsToSeconds(L: Plua_State): Integer; cdecl;
begin
  Result := 1;

  if (CurrentSong = nil) or (Length(CurrentSong.BPM) = 0) or (Display.CurrentScreen <> @ScreenSing) then
    lua_PushNumber(L, 0) // in case of error
  else if (Length(CurrentSong.BPM) = 1) then
    lua_PushNumber(L, luaL_CheckNumber(L, 1) * 60 / CurrentSong.BPM[0].BPM)
  else
  begin
    // to-do: do this for songs w/ BPM changes
    //        or drop support for BPM changes?!
  end;
end;

{ ScreenSing.BeatsToSeconds(Seconds: float)
  returns the Beats in quarts that the given seconds last }
function ULuaScreenSing_SecondsToBeats(L: Plua_State): Integer; cdecl;
begin
  Result := 1;

  if (CurrentSong = nil) or (Length(CurrentSong.BPM) = 0) or (Display.CurrentScreen <> @ScreenSing) then
    lua_PushNumber(L, 0)
  else if (Length(CurrentSong.BPM) = 1) then
    lua_PushNumber(L, luaL_CheckNumber(L, 1) * CurrentSong.BPM[0].BPM / 60)
  else
  begin
    // to-do: do this for songs w/ BPM changes
    //        or drop support for BPM changes?!
  end;
end;

{ ScreenSing.GetBeat() - returns current beat of lyricstate (in quarts) }
function ULuaScreenSing_GetBeat(L: Plua_State): Integer; cdecl;
var top: Integer;
begin
  //remove arguments (if any)
  top := lua_gettop(L);

  if (top > 0) then
    lua_pop(L, top);

  //push result
  lua_pushnumber(L, LyricsState.MidBeat);
  Result := 1; //one result
end;

{ returns a table with following structure:
    t[1..playercount] = rect of players ScoreBG: table(x, y, w, h) }
function ULuaScreenSing_GetScoreBGRect(L: Plua_State): Integer; cdecl;
  var
    Top: Integer;
    I: Integer;
begin
  Result := 1;

  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  // create table
  lua_createtable(L, Length(ScreenSing.Scores.Players), 0);

  // fill w/ values
  for I := 0 to High(ScreenSing.Scores.Players) do
  begin
    lua_pushInteger(L, I + 1);

    if (ScreenSing.Scores.Players[I].Position = High(Byte)) then
      // player has no position, prevent crash by pushing nil
      lua_pushNil(L)
    else
      with ScreenSing.Scores.Positions[ScreenSing.Scores.Players[I].Position] do
        lua_PushRect(L, BGX, BGY, BGW, BGH);


    lua_settable(L, -3);
  end;

  // leave table on stack, it is our result
end;

{ returns a table with following structure:
    t[1..playercount] = rect of players rating bar: table(x, y, w, h) }
function ULuaScreenSing_GetRBRect(L: Plua_State): Integer; cdecl;
  var
    Top: Integer;
    I: Integer;
begin
  Result := 1;

  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  // create table
  lua_createtable(L, Length(ScreenSing.Scores.Players), 0);

  // fill w/ values
  for I := 0 to High(ScreenSing.Scores.Players) do
  begin
    lua_pushInteger(L, I + 1);

    if (ScreenSing.Scores.Players[I].Position = High(Byte)) then
      // player has no position, prevent crash by pushing nil
      lua_pushNil(L)
    else
      with ScreenSing.Scores.Positions[ScreenSing.Scores.Players[I].Position] do
        lua_PushRect(L, RBX, RBY, RBW, RBH);


    lua_settable(L, -3);
  end;

  // leave table on stack, it is our result
end;

{ finishes current song, if sing screen is not shown it will raise
  an error }
function ULuaScreenSing_Finish(L: Plua_State): Integer; cdecl;
  var Top: Integer;
begin
  Result := 0;

  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  if (Display.CurrentScreen^ = ScreenSing) then
  begin
    ScreenSing.EndSong;
  end
  else
    LuaL_error(L, 'Usdx.ScreenSing.Finish is called, but sing screen is not shown.');
end;

{ ScreenSing.GetSettings - no arguments
  returns a table filled with the data of TScreenSing }
function ULuaScreenSing_GetSettings(L: Plua_State): Integer; cdecl;
  var Top: Integer;
begin
  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  lua_createtable(L, 0, 3);

  //fill table w/ info
  lua_pushBoolean(L, ScreenSing.Settings.LyricsVisible);
  lua_setField(L, -2, 'LyricsVisible');

  lua_pushBinInt(L, ScreenSing.Settings.NotesVisible);
  lua_setField(L, -2, 'NotesVisible');

  lua_pushBinInt(L, ScreenSing.Settings.PlayerEnabled);
  lua_setField(L, -2, 'PlayerEnabled');

  lua_pushBoolean(L, ScreenSing.Settings.SoundEnabled);
  lua_setField(L, -2, 'SoundEnabled');

  Result := 1;
end;

{ ScreenSing.SetSettings - arguments: Table
  sets all attributes of TScreenSing.Settings that are
  unequal to nil in Table }
function ULuaScreenSing_SetSettings(L: Plua_State): Integer; cdecl;
  var
    Key: String;
begin
  Result := 0;

  // check for table on stack
  luaL_checkType(L, 1, LUA_TTABLE);

  // go through table elements
  lua_pushNil(L);
  while (lua_Next(L, 1) <> 0) do
  begin
    Key := lowercase(lua_ToString(L, -2));

    if (Key = 'lyricsvisible') and (lua_isBoolean(L, -1)) then
      ScreenSing.settings.LyricsVisible := lua_toBoolean(L, -1)
    else if (Key = 'notesvisible') and (lua_isTable(L, -1)) then
      ScreenSing.settings.NotesVisible := lua_toBinInt(L, -1)
    else if (Key = 'playerenabled') and (lua_isTable(L, -1)) then
      ScreenSing.settings.PlayerEnabled := lua_toBinInt(L, -1)
    else if (Key = 'soundenabled') and (lua_isBoolean(L, -1)) then
      ScreenSing.settings.SoundEnabled := lua_toBoolean(L, -1);

    // pop value from stack so key is on top
    lua_pop(L, 1);
  end;

  // clear stack from table
  lua_pop(L, lua_gettop(L));

  ScreenSing.ApplySettings;
end;

{ ScreenSing.GetSongLines - no arguments
  returns a table filled with lines of the loaded song or
  nil if no song is loaded (singscreen is not displayed)
  structure of returned table:
    array [1.."count of lines"]
     \
     | Start: integer - beat the line is displayed at (on top of lyrics display)
     | Lyric: string  - full lyric of the line
     | Notes: array [1.."count notes of this line"]
     \
      | Start: integer    - beat the note starts at
      | Length: integer   - length in beats
      | Tone: integer     - pitch that has to be sung, full range
      | NoteType: integer - 0 for freestyle, 1 for normal, 2 for golden
      | Text: string      - text of this fragment }
function ULuaScreenSing_GetSongLines(L: Plua_State): Integer; cdecl;
  var
    I, J: Integer;
begin
  Result := 1;
  if  (Length(Tracks) >= 1) then
  begin
    lua_ClearStack(L);

    if not lua_CheckStack(L, 7) then
      luaL_Error(L, PChar('can''t allocate enough stack space in ULuaScreenSing_GetSongLines'));

    // lines array table
    lua_CreateTable(L, Length(Tracks[0].Lines), 0);

    for I := 0 to High(Tracks[0].Lines) do
    with Tracks[0].Lines[I] do
    begin
      lua_pushInteger(L, I+1);

      // line struct table
      lua_CreateTable(L, 0, 3);

      // line start
      lua_PushInteger(L, StartBeat);
      lua_SetField(L, -2, PChar('Start'));

      // line lyric
      lua_PushString(L, PChar(Lyric));
      lua_SetField(L, -2, PChar('Lyric'));

      //line notes array table
      lua_CreateTable(L, Length(Notes), 0);

      for J := 0 to High(Notes) do
      begin
        lua_PushInteger(L, J + 1);

        // note struct table
        lua_CreateTable(L, 0, 5);

        // Notes[J+1].Start
        lua_PushInteger(L, Notes[J].StartBeat);
        lua_SetField(L, -2, PChar('Start'));

        // Notes[J+1].Length
        lua_PushInteger(L, Notes[J].Duration);
        lua_SetField(L, -2, PChar('Length'));

        // Notes[J+1].Tone
        lua_PushInteger(L, Notes[J].Tone);
        lua_SetField(L, -2, PChar('Tone'));

        // Notes[J+1].NoteType
        lua_PushInteger(L, Integer(Notes[J].NoteType));
        lua_SetField(L, -2, PChar('NoteType'));

        // Notes[J+1].Text
        lua_PushString(L, PChar(Notes[J].Text));
        lua_SetField(L, -2, PChar('Text'));

        lua_SetTable(L, -3);
      end;

      lua_SetField(L, -2, PChar('Notes'));

      // save line to array table
      lua_setTable(L, -3);
    end;
  end
  else
  begin
    lua_ClearStack(L);
    lua_pushNil(L);
  end;
end;

    { ScreenSing.GetPlayerNotes - no arguments
    returns a table filled with lines of the loaded song or
    nil if no song is loaded (singscreen is not displayed)
    structure of returned table:
      array [1.."count of players"]
       \
       | Notes: array [1.."count notes of this line"]
       \
        | Start: integer    - beat the note starts at
        | Length: integer   - length in beats
        | Tone: integer     - pitch that was sung, full range }
function ULuaScreenSing_GetPlayerNotes(L: Plua_State): Integer; cdecl;
  var
    I, J: Integer;
begin
  // TODO: get rid of the useless 'Notes' object
  Result := 1;
  if  (Length(Player) >= 1) then
  begin
    lua_ClearStack(L);

    if not lua_CheckStack(L, 7) then
      luaL_Error(L, PChar('can''t allocate enough stack space in ULuaScreenSing_GetPlayerNotes'));

    // lines array table
    lua_CreateTable(L, Length(Player), 0);

    for I := 0 to High(Player) do
      with Player[I] do
      begin
        lua_pushInteger(L, I+1);

        // line struct table
        lua_CreateTable(L, 0, 1);

        //line notes array table
        lua_CreateTable(L, Length(Player[I].Note), 0);

        for J := 0 to High(Player[I].Note) do
        begin
          lua_PushInteger(L, J + 1);

          // note struct table
          lua_CreateTable(L, 0, 3);

          // Notes[J+1].Start
          lua_PushInteger(L, Player[I].Note[J].Start);
          lua_SetField(L, -2, PChar('Start'));

          // Notes[J+1].Length
          lua_PushInteger(L, Player[I].Note[J].Duration);
          lua_SetField(L, -2, PChar('Length'));

          // Notes[J+1].Tone
          lua_PushInteger(L, Round(Player[I].Note[J].Tone));
          lua_SetField(L, -2, PChar('Tone'));

          lua_SetTable(L, -3);
        end;

        lua_SetField(L, -2, PChar('Notes'));

        // save line to array table
        lua_setTable(L, -3);
      end;
  end
  else
  begin
    lua_ClearStack(L);
    lua_pushNil(L);
  end;
end;

  {TODO: these should be in ULuaScreenSong instead? }
function ULuaScreenSong_GetVisibleSongs(L: Plua_State): Integer; cdecl;
begin
  Result := 1;
  lua_pushinteger(L, CatSongs.VisibleSongs())
end;

function ULuaScreenSong_GetSelectedIndex(L: Plua_State): Integer; cdecl;
begin
  Result := 1;
  lua_pushinteger(L, CatSongs.Selected)
end;

function ULuaScreenSong_GetSongNumbers(L: Plua_State): Integer; cdecl;
begin
  Result := 1;
  lua_pushstring(L, PChar(Theme.Song.TextNumber.Text))
end;

function ULuaScreenSong_GetSelectedSong(L: Plua_State): Integer; cdecl;
var Top: Integer;
var Song: TSong;
begin
  // pop arguments
  Top := lua_getTop(L);
  if (Top > 0) then
    lua_pop(L, Top);

  Song := CatSongs.Song[CatSongs.Selected];

  lua_createtable(L, 0, 2);

  //fill table w/ info
  lua_pushstring(L, PChar(Song.Artist));
  lua_setField(L, -2, 'Artist');

  lua_pushstring(L, PChar(Song.Title));
  lua_setField(L, -2, 'Title');

  lua_pushinteger(L, Song.Year);
  lua_setField(L, -2, 'Year');

  Result := 1;
end;

end.
