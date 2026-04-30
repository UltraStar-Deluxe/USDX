{* UltraStar Deluxe - Karaoke Game
 *
 * Typed remote-control command names for browser-based USDX remotes.
 * This unit deliberately models semantic commands instead of OS-level
 * keyboard injection.
 *}

unit URemoteControl;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils;

type
  TRemoteControlCommand = (
    rccUnknown,
    rccMenuUp,
    rccMenuDown,
    rccMenuLeft,
    rccMenuRight,
    rccMenuSelect,
    rccMenuBack,
    rccGamePause,
    rccGameResume,
    rccSongSkipIntro,
    rccSongRestart,
    rccSongMenu,
    rccSongPlaylistMenu,
    rccSongJumpTo,
    rccSongRandom,
    rccSongSelect,
    rccSongStart,
    rccPreviewStart,
    rccPreviewStop,
    rccSearchSetText,
    rccPlaylistLoad,
    rccPlaylistCreate,
    rccPlaylistDelete,
    rccPlaylistRemoveItem,
    rccPlaylistAddSelected,
    rccPlaylistRemoveSelected,
    rccPlaylistAddSongs,
    rccPlaylistRemoveSong,
    rccPlaylistMoveItemUp,
    rccPlaylistMoveItemDown
  );

function TryParseRemoteControlCommand(const CommandName: UTF8String;
  out Command: TRemoteControlCommand): boolean;
function RemoteControlCommandToString(Command: TRemoteControlCommand): UTF8String;
function IsMenuRemoteControlCommand(Command: TRemoteControlCommand): boolean;
function IsSongRemoteControlCommand(Command: TRemoteControlCommand): boolean;

implementation

function TryParseRemoteControlCommand(const CommandName: UTF8String;
  out Command: TRemoteControlCommand): boolean;
var
  Name: string;
begin
  Name := LowerCase(string(CommandName));
  Result := true;

  if (Name = 'menu.up') then
    Command := rccMenuUp
  else if (Name = 'menu.down') then
    Command := rccMenuDown
  else if (Name = 'menu.left') then
    Command := rccMenuLeft
  else if (Name = 'menu.right') then
    Command := rccMenuRight
  else if (Name = 'menu.select') then
    Command := rccMenuSelect
  else if (Name = 'menu.back') then
    Command := rccMenuBack
  else if (Name = 'game.pause') then
    Command := rccGamePause
  else if (Name = 'game.resume') then
    Command := rccGameResume
  else if (Name = 'song.skipintro') then
    Command := rccSongSkipIntro
  else if (Name = 'song.restart') then
    Command := rccSongRestart
  else if (Name = 'song.menu') then
    Command := rccSongMenu
  else if (Name = 'song.playlistmenu') then
    Command := rccSongPlaylistMenu
  else if (Name = 'song.jumpto') then
    Command := rccSongJumpTo
  else if (Name = 'song.random') then
    Command := rccSongRandom
  else if (Name = 'song.select') then
    Command := rccSongSelect
  else if (Name = 'song.start') then
    Command := rccSongStart
  else if (Name = 'preview.start') then
    Command := rccPreviewStart
  else if (Name = 'preview.stop') then
    Command := rccPreviewStop
  else if (Name = 'search.settext') then
    Command := rccSearchSetText
  else if (Name = 'playlist.load') then
    Command := rccPlaylistLoad
  else if (Name = 'playlist.create') then
    Command := rccPlaylistCreate
  else if (Name = 'playlist.delete') then
    Command := rccPlaylistDelete
  else if (Name = 'playlist.removeitem') then
    Command := rccPlaylistRemoveItem
  else if (Name = 'playlist.addselected') then
    Command := rccPlaylistAddSelected
  else if (Name = 'playlist.removeselected') then
    Command := rccPlaylistRemoveSelected
  else if (Name = 'playlist.addsongs') then
    Command := rccPlaylistAddSongs
  else if (Name = 'playlist.removesong') then
    Command := rccPlaylistRemoveSong
  else if (Name = 'playlist.moveitemup') then
    Command := rccPlaylistMoveItemUp
  else if (Name = 'playlist.moveitemdown') then
    Command := rccPlaylistMoveItemDown
  else
  begin
    Command := rccUnknown;
    Result := false;
  end;
end;

function RemoteControlCommandToString(Command: TRemoteControlCommand): UTF8String;
begin
  case Command of
    rccMenuUp:        Result := 'menu.up';
    rccMenuDown:      Result := 'menu.down';
    rccMenuLeft:      Result := 'menu.left';
    rccMenuRight:     Result := 'menu.right';
    rccMenuSelect:    Result := 'menu.select';
    rccMenuBack:      Result := 'menu.back';
    rccGamePause:     Result := 'game.pause';
    rccGameResume:    Result := 'game.resume';
    rccSongSkipIntro: Result := 'song.skipIntro';
    rccSongRestart:   Result := 'song.restart';
    rccSongMenu:      Result := 'song.menu';
    rccSongPlaylistMenu: Result := 'song.playlistMenu';
    rccSongJumpTo:    Result := 'song.jumpTo';
    rccSongRandom:    Result := 'song.random';
    rccSongSelect:    Result := 'song.select';
    rccSongStart:     Result := 'song.start';
    rccPreviewStart:  Result := 'preview.start';
    rccPreviewStop:   Result := 'preview.stop';
    rccSearchSetText: Result := 'search.setText';
    rccPlaylistLoad:  Result := 'playlist.load';
    rccPlaylistCreate: Result := 'playlist.create';
    rccPlaylistDelete: Result := 'playlist.delete';
    rccPlaylistRemoveItem: Result := 'playlist.removeItem';
    rccPlaylistAddSelected: Result := 'playlist.addSelected';
    rccPlaylistRemoveSelected: Result := 'playlist.removeSelected';
    rccPlaylistAddSongs: Result := 'playlist.addSongs';
    rccPlaylistRemoveSong: Result := 'playlist.removeSong';
    rccPlaylistMoveItemUp: Result := 'playlist.moveItemUp';
    rccPlaylistMoveItemDown: Result := 'playlist.moveItemDown';
  else
    Result := '';
  end;
end;

function IsMenuRemoteControlCommand(Command: TRemoteControlCommand): boolean;
begin
  Result := Command in [
    rccMenuUp,
    rccMenuDown,
    rccMenuLeft,
    rccMenuRight,
    rccMenuSelect,
    rccMenuBack
  ];
end;

function IsSongRemoteControlCommand(Command: TRemoteControlCommand): boolean;
begin
  Result := Command in [
    rccGamePause,
    rccGameResume,
    rccSongSkipIntro,
    rccSongRestart,
    rccSongMenu,
    rccSongPlaylistMenu,
    rccSongJumpTo,
    rccSongRandom,
    rccSongSelect,
    rccSongStart,
    rccPreviewStart,
    rccPreviewStop,
    rccSearchSetText,
    rccPlaylistLoad,
    rccPlaylistCreate,
    rccPlaylistDelete,
    rccPlaylistRemoveItem,
    rccPlaylistAddSelected,
    rccPlaylistRemoveSelected,
    rccPlaylistAddSongs,
    rccPlaylistRemoveSong,
    rccPlaylistMoveItemUp,
    rccPlaylistMoveItemDown
  ];
end;

end.
