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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/base/UPlaylist.pas $
 * $Id: UPlaylist.pas 2199 2010-03-14 20:56:20Z brunzelchen $
 *}

unit UPlaylist;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  UIni,
  USong,
  UPath,
  UPathUtils;

type
  TPlaylistItem = record
    Artist: UTF8String;
    Title:  UTF8String;
    SongID: Integer;
  end;

  APlaylistItem = array of TPlaylistItem;

  TPlaylist = record
    Name:     UTF8String;
    Filename: IPath;
    Items:    APlaylistItem;
    FixedOrder: Boolean;
    LastRead: LongInt;
  end;

  APlaylist = array of TPlaylist;

  TPlaylistSortOrder = (psoArtist, psoTitle, psoEdition, psoGenre, psoLanguage, psoShuffle);

  //----------
  //TPlaylistManager - Class for Managing Playlists (Loading, Displaying, Saving)
  //----------
  TPlaylistManager = class
    private
      SongOrderDirty: Boolean;
      function  FindSongIndexByNames(const Artist, Title: UTF8String): Integer;
      function  PlaylistContainsSongID(const iPlaylist: Cardinal; const SongID: Integer): Boolean;
      procedure MoveSongIndex(const OldIndex, NewIndex: Integer);
      procedure ApplyPlaylistOrder(Index: Cardinal);
      procedure ReindexAllPlaylists;
      procedure SortItems(var Items: APlaylistItem; Order: TPlaylistSortOrder);

    public
      Mode:         TSongMode;     //Current Playlist Mode for SongScreen
      CurPlayList:  Integer;
      CurItem:      Cardinal;

      Playlists:    APlaylist;

      constructor Create;
      procedure   LoadPlayLists;
      function    LoadPlayList(Index: Cardinal; const Filename: IPath): Boolean;
      function    ReloadPlayList(Index: Cardinal): Boolean;
      procedure   SavePlayList(Index: Cardinal);

      procedure   RestoreSongOrder;
      procedure   SetPlayList(Index: Integer; SongID: Integer = -1);
      procedure   UnsetPlaylist;

      function    AddPlaylist(const Name: UTF8String): Cardinal;
      procedure   DelPlaylist(const Index: Cardinal);

      function    AddItem(const SongID: Cardinal; const iPlaylist: Integer = -1): Boolean;
      function    AddVisibleItems(const iPlaylist: Integer = -1): Cardinal;
      procedure   DelItem(const iItem: Cardinal; const iPlaylist: Integer = -1);
      procedure   DelVisibleItems(const iPlaylist: Integer = -1);
      procedure   SortPlaylist(const Order: TPlaylistSortOrder; const iPlaylist: Integer = -1);
      function    MoveItem(const SongID: Cardinal; const Direction: Integer; const iPlaylist: Integer = -1): Integer;

      procedure   GetNames(var PLNames: array of UTF8String);
      function    GetIndexbySongID(const SongID: Cardinal; const iPlaylist: Integer = -1): Integer;
    end;

    {Modes:
      0: Standard Mode
      1: Category Mode
      2: PlayList Mode}

  var
    PlayListMan:  TPlaylistManager;


implementation

uses
  SysUtils,
  USongs,
  ULog,
  UMain,
  UFilesystem,
  UGraphic,
  UThemes,
  UUnicodeUtils;

//----------
//Create - Construct Class - Dummy for now
//----------
constructor TPlayListManager.Create;
begin
  inherited;
  SongOrderDirty := False;
  LoadPlayLists;
  CurPlayList := -1;
end;

//----------
//LoadPlayLists - Load list of Playlists from PlayList Folder
//----------
Procedure   TPlayListManager.LoadPlayLists;
var
  Len:  Integer;
  PlayListBuffer: TPlayList;
  Iter: IFileIterator;
  FileInfo: TFileInfo;
begin
  SetLength(Playlists, 0);

  Iter := FileSystem.FileFind(PlayListPath.Append('*.upl'), 0);
  while (Iter.HasNext) do
  begin
    Len := Length(Playlists);
    SetLength(Playlists, Len + 1);

    FileInfo := Iter.Next;

    if not LoadPlayList(Len, FileInfo.Name) then
      SetLength(Playlists, Len)
    else
    begin
      // Sort the Playlists - Insertion Sort
      PlayListBuffer := Playlists[Len];
      Dec(Len);
      while (Len >= 0) AND (CompareText(Playlists[Len].Name, PlayListBuffer.Name) >= 0) do
      begin
          Playlists[Len+1] := Playlists[Len];
          Dec(Len);
      end;
      Playlists[Len+1] := PlayListBuffer;
    end;
  end;
end;

//----------
//LoadPlayList - Load a Playlist in the Array
//----------
function TPlayListManager.LoadPlayList(Index: Cardinal; const Filename: IPath): Boolean;

  function FindSong(Artist, Title: UTF8String): Integer;
  var I: Integer;
  begin
    Result := -1;

    For I := low(CatSongs.Song) to high(CatSongs.Song) do
    begin
      if (CatSongs.Song[I].Title = Title) and (CatSongs.Song[I].Artist = Artist) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;

var
  TextStream: TTextFileStream;
  Line: UTF8String;
  PosDelimiter: Integer;
  SongID: Integer;
  Len: Integer;
  FilenameAbs: IPath;
begin
  //Load File
  try
    FilenameAbs := PlaylistPath.Append(Filename);
    TextStream := TMemTextFileStream.Create(FilenameAbs, fmOpenRead);
  except
    begin
      Log.LogError('Could not load Playlist: ' + FilenameAbs.ToNative);
      Result := False;
      Exit;
    end;
  end;
  Result := True;

  //Set Filename
  Playlists[Index].Filename := Filename;
  Playlists[Index].Name := '';
  Playlists[Index].FixedOrder := False;
  Playlists[Index].LastRead := FileAge(Filename.ToNative);

  //Read Until End of File
  while TextStream.ReadLine(Line) do
  begin
    if (Length(Line) > 0) then
    begin
      PosDelimiter := UTF8Pos(':', Line);
      if (PosDelimiter <> 0) then
      begin
        //Comment or Name String
        if (Line[1] = '#') then
        begin
          //Found Name Value
          if (Uppercase(Trim(copy(Line, 2, PosDelimiter - 2))) = 'NAME') then
            PlayLists[Index].Name := Trim(copy(Line, PosDelimiter + 1,Length(Line) - PosDelimiter))
          else if (Uppercase(Trim(copy(Line, 2, PosDelimiter - 2))) = 'FIXEDORDER') then
          begin
            // Accept On/Off (case-insensitive, allow spaces)
            Line := Trim(copy(Line, PosDelimiter + 1,Length(Line) - PosDelimiter));
            PlayLists[Index].FixedOrder := (UpperCase(Line) = 'ON');
          end
            
        end
        //Song Entry
        else
        begin
          SongID := FindSong(Trim(copy(Line, 1, PosDelimiter - 1)), Trim(copy(Line, PosDelimiter + 1, Length(Line) - PosDelimiter)));
          if (SongID <> -1) then
          begin
            Len := Length(PlayLists[Index].Items);
            SetLength(PlayLists[Index].Items, Len + 1);

            PlayLists[Index].Items[Len].SongID := SongID;

            PlayLists[Index].Items[Len].Artist := Trim(copy(Line, 1, PosDelimiter - 1));
            PlayLists[Index].Items[Len].Title  := Trim(copy(Line, PosDelimiter + 1, Length(Line) - PosDelimiter));
          end
          else Log.LogError('Could not find Song in Playlist: ' + PlayLists[Index].Filename.ToNative + ', ' + Line);
        end;
      end;
    end;
  end;

  //If no special name is given, use Filename
  if PlayLists[Index].Name = '' then
  begin
    PlayLists[Index].Name := FileName.SetExtension('').ToUTF8;
  end;

  //Finish (Close File)
  TextStream.Free;
end;

//----------
//ReloadPlayList - Reload a Playlist in the Array
//----------
function TPlayListManager.ReloadPlayList(Index: Cardinal): Boolean;
var
  PlaylistFile: IPath;
begin
  if (Int(Index) > High(PlayLists)) then
  begin
    Result := False;
    Exit;
  end;

  PlaylistFile := PlaylistPath.Append(Playlists[Index].Filename);
  if not FileExists(PlaylistFile.ToNative) or (FileAge(PlaylistFile.ToNative) <= Playlists[Index].LastRead) then
  begin
    Result := false;
    Exit;
  end;

  SetLength(PlayLists[Index].Items, 0);

  Result := LoadPlayList(Index, PlayLists[Index].Filename);
end;

{**
 * Saves the specified Playlist
 *}
procedure   TPlayListManager.SavePlayList(Index: Cardinal);
var
  TextStream: TTextFileStream;
  PlaylistFile: IPath;
  I: Integer;
begin
  PlaylistFile := PlaylistPath.Append(Playlists[Index].Filename);

  // cannot update read-only file
  if PlaylistFile.IsFile() and PlaylistFile.IsReadOnly() then
    Exit;

  // open file for rewriting
  TextStream := TMemTextFileStream.Create(PlaylistFile, fmCreate);
  try
    // Write version (not nessecary but helpful)
    TextStream.WriteLine('######################################');
    TextStream.WriteLine('#Ultrastar Deluxe Playlist Format v1.0');
    TextStream.WriteLine(Format('#Playlist %s with %d Songs.',
                         [ Playlists[Index].Name, Length(Playlists[Index].Items) ]));
    TextStream.WriteLine('######################################');

    // Write name information
    TextStream.WriteLine('#Name: ' + Playlists[Index].Name);
    // Write playlist order information
    if Playlists[Index].FixedOrder then
      TextStream.WriteLine('#FixedOrder: On')
    else
      TextStream.WriteLine('#FixedOrder: Off');

    // Write song information
    TextStream.WriteLine('#Songs:');

    for I := 0 to high(Playlists[Index].Items) do
    begin
      TextStream.WriteLine(Playlists[Index].Items[I].Artist + ' : ' + Playlists[Index].Items[I].Title);
    end;
  except
    Log.LogError('Could not write Playlistfile "' + Playlists[Index].Name + '"');
  end;
  TextStream.Free;
end;

{**
 * Display a Playlist in CatSongs
 *}
procedure TPlayListManager.SetPlayList(Index: Integer; SongID: Integer = -1);
var
  I, SongIdx, TargetSongID, RandomVisibleIndex, VisibleCount: Integer;
begin
  if (Index < 0) or (Index > High(PlayLists)) then
    exit;

  RestoreSongOrder;

  if Playlists[Index].FixedOrder then
    ApplyPlaylistOrder(Index)
  else
    SongOrderDirty := False;

  ScreenSong.SyncCoversToSongs;

  //Hide all Songs
  for I := 0 to high(CatSongs.Song) do
     CatSongs.Song[I].Visible := False;

  //Show Songs in PL
  for I := 0 to high(PlayLists[Index].Items) do
  begin
    SongIdx := PlayLists[Index].Items[I].SongID;
    if (SongIdx >= 0) and (SongIdx <= High(CatSongs.Song)) then
      CatSongs.Song[SongIdx].Visible := True
    else
      Log.LogError('Playlist entry has invalid SongID after reordering: ' + Playlists[Index].Items[I].Artist + ' - ' + Playlists[Index].Items[I].Title);
  end;

  CatSongs.ResetVisibleIndexCache;

  if ScreenSong.ListMinLine > 0 then
  begin
    I := CatSongs.VisibleSongs - Theme.Song.ListCover.Rows;
    if I < 0 then
      I := 0;
    if ScreenSong.ListMinLine > I then
      ScreenSong.ListMinLine := I;
  end;

  if Assigned(ScreenSong) then
    ScreenSong.FilterDuetsInPartyMode; // in party mode

  //Set CatSongsMode + Playlist Mode
  CatSongs.CatNumShow := -3;
  Mode := smPlayList;

  //Set CurPlaylist
  CurPlaylist := Index;

  //Show Cat in Topleft:
  // Append order status to the displayed playlist name
  if Playlists[Index].FixedOrder then
    ScreenSong.ShowCatTLCustom(Format(Theme.Playlist.CatText,[Playlists[Index].Name + '  [Fixed Order: On]']))
  else
    ScreenSong.ShowCatTLCustom(Format(Theme.Playlist.CatText,[Playlists[Index].Name + '  [Fixed Order: Off]']));

  ScreenSong.ResetRandomSongState;

  // Fix SongSelection and keep the viewport aligned with the selected song.
  TargetSongID := -1;
  if (SongID <> -1) then
  begin
    for I := 0 to high(PlayLists[Index].Items) do
    begin
      if (PlayLists[Index].Items[I].SongID = SongID) then
      begin
        TargetSongID := SongID;
        Break;
      end;
    end;
  end;

  if (TargetSongID = -1) and not Playlists[Index].FixedOrder and (CatSongs.VisibleSongs > 0) then
  begin
    RandomVisibleIndex := Random(CatSongs.VisibleSongs);
    VisibleCount := 0;

    for I := 0 to High(CatSongs.Song) do
    begin
      if CatSongs.Song[I].Visible then
      begin
        if (VisibleCount = RandomVisibleIndex) then
        begin
          TargetSongID := I;
          Break;
        end;
        Inc(VisibleCount);
      end;
    end;
  end;

  if (TargetSongID = -1) then
  begin
    for I := 0 to high(PlayLists[Index].Items) do
    begin
      SongIdx := PlayLists[Index].Items[I].SongID;
      if (SongIdx >= 0) and (SongIdx <= High(CatSongs.Song)) and CatSongs.Song[SongIdx].Visible then
      begin
        TargetSongID := SongIdx;
        Break;
      end;
    end;
  end;

  if (TargetSongID = -1) then
  begin
    ScreenSong.Interaction := 0;
    case TSongMenuMode(Ini.SongMenu) of
      smChessboard:
        ScreenSong.ChessboardMinLine := 0;
      smList:
        ScreenSong.ListMinLine := 0;
    end;
  end
  else
  begin
    case TSongMenuMode(Ini.SongMenu) of
      smRoulette,
      smChessboard,
      smList:
        ScreenSong.SkipTo(CatSongs.VisibleIndex(TargetSongID), TargetSongID, CatSongs.VisibleSongs);
    else
      begin
        ScreenSong.Interaction := TargetSongID;
        ScreenSong.FixSelected;
      end;
    end;
  end;

  if (TargetSongID = -1) then
    ScreenSong.FixSelected;

  //Play correct Music
  //ScreenSong.ChangeMusic;
end;

{**
 * Unset Playlist for CatSongs
 *}
procedure TPlayListManager.UnsetPlayList;
begin
  CurPlayList := -1; // no playlist selected
end;

//----------
//AddPlaylist - Adds a Playlist and Returns the Index
//----------
function TPlayListManager.AddPlaylist(const Name: UTF8String): cardinal;
var
  I: Integer;
  PlaylistFile: IPath;
begin
  Result := Length(Playlists);
  SetLength(Playlists, Result + 1);
  
  // Sort the Playlists - Insertion Sort
  while (Result > 0) and (CompareText(Playlists[Result - 1].Name, Name) >= 0) do
  begin
    Dec(Result);
    Playlists[Result+1] := Playlists[Result];
  end;
  Playlists[Result].Name := Name;
  Playlists[Result].FixedOrder := False;

  // clear playlist items
  SetLength(Playlists[Result].Items, 0);

  I := 1;
  PlaylistFile := PlaylistPath.Append(Name + '.upl');
  while (PlaylistFile.Exists) do
  begin
    Inc(I);
    PlaylistFile := PlaylistPath.Append(Name + InttoStr(I) + '.upl');
  end;
  Playlists[Result].Filename := PlaylistFile.GetName;

  //Save new Playlist
  SavePlayList(Result);
end;

//----------
//DelPlaylist - Deletes a Playlist
//----------
procedure   TPlayListManager.DelPlaylist(const Index: Cardinal);
var
  I: Integer;
  Filename: IPath;
begin
  if Int(Index) > High(Playlists) then
    Exit;

  Filename := PlaylistPath.Append(Playlists[Index].Filename);

  //If not FileExists or File is not Writeable then exit
  if (not Filename.IsFile()) or (Filename.IsReadOnly()) then
    Exit;


  //Delete Playlist from FileSystem
  if not Filename.DeleteFile() then
    Exit;

  //Delete Playlist from Array
  //move all PLs to the Hole
  for I := Index to High(Playlists)-1 do
    PlayLists[I] := PlayLists[I+1];

  //Delete last Playlist
  SetLength (Playlists, High(Playlists));

  //If Playlist is Displayed atm
  //-> Display Songs
  if (CatSongs.CatNumShow = -3) and (Index = CurPlaylist) then
  begin
    ScreenSong.UnloadCover(ScreenSong.Interaction);
    ScreenSong.HideCatTL;
    CatSongs.SetFilter('', fltAll);
    ScreenSong.Interaction := 0;
    ScreenSong.FixSelected;
    ScreenSong.ChangeMusic;
  end;
end;

//----------
//AddItem - Adds an Item to a specific Playlist
//----------
Function    TPlayListManager.AddItem(const SongID: Cardinal; const iPlaylist: Integer): Boolean;
var
  P: Cardinal;
  Len: Cardinal;
begin
  Result := False;

  if iPlaylist = -1 then
    P := CurPlaylist
  else if (iPlaylist >= 0) AND (iPlaylist <= high(Playlists)) then
    P := iPlaylist
  else
    exit;

  if (Int(SongID) <= High(CatSongs.Song)) AND (NOT CatSongs.Song[SongID].Main) then
  begin
    if PlaylistContainsSongID(P, SongID) then
      Exit;

    Len := Length(Playlists[P].Items);
    SetLength(Playlists[P].Items, Len + 1);

    Playlists[P].Items[Len].SongID  := SongID;
    Playlists[P].Items[Len].Title   := CatSongs.Song[SongID].Title;
    Playlists[P].Items[Len].Artist  := CatSongs.Song[SongID].Artist;
    Result := True;

    //Save Changes
    SavePlayList(P);

    //Correct Display when Editing current Playlist
    if (CatSongs.CatNumShow = -3) and (P = CurPlaylist) then
      SetPlaylist(P);
  end;
end;

function TPlayListManager.AddVisibleItems(const iPlaylist: Integer): Cardinal;
var
  P: Cardinal;
  I, Len: Integer;
begin
  Result := 0;

  if iPlaylist = -1 then
    P := CurPlaylist
  else if (iPlaylist >= 0) AND (iPlaylist <= high(Playlists)) then
    P := iPlaylist
  else
    exit;

  Len := Length(Playlists[P].Items);

  for I := Low(CatSongs.Song) to High(CatSongs.Song) do
  begin
    if CatSongs.Song[I].Visible and (not CatSongs.Song[I].Main) then
    begin
      if PlaylistContainsSongID(P, I) then
        Continue;

      SetLength(Playlists[P].Items, Len + 1);
      Playlists[P].Items[Len].SongID := I;
      Playlists[P].Items[Len].Title  := CatSongs.Song[I].Title;
      Playlists[P].Items[Len].Artist := CatSongs.Song[I].Artist;
      Inc(Len);
      Inc(Result);
    end;
  end;

  if Result > 0 then
  begin
    SavePlayList(P);

    if (CatSongs.CatNumShow = -3) and (P = CurPlaylist) then
      SetPlaylist(P);
  end;
end;

//----------
//DelItem - Deletes an Item from a specific Playlist
//----------
Procedure   TPlayListManager.DelItem(const iItem: Cardinal; const iPlaylist: Integer);
var
  I: Integer;
  P: Cardinal;
begin
  if iPlaylist = -1 then
    P := CurPlaylist
  else if (iPlaylist >= 0) AND (iPlaylist <= high(Playlists)) then
    P := iPlaylist
  else
    exit;

  if (Int(iItem) <= high(Playlists[P].Items)) then
  begin
    //Move all entrys behind deleted one to Front
    For I := iItem to High(Playlists[P].Items) - 1 do
      Playlists[P].Items[I] := Playlists[P].Items[I + 1];

    //Delete Last Entry
    SetLength(PlayLists[P].Items, Length(PlayLists[P].Items) - 1);

    //Save Changes
    SavePlayList(P);
  end;

  //Delete Playlist if Last Song is deleted
  if (Length(PlayLists[P].Items) = 0) then
  begin
    DelPlaylist(P);
  end
  //Correct Display when Editing current Playlist
  else if (CatSongs.CatNumShow = -3) and (P = CurPlaylist) then
    SetPlaylist(P);
end;

procedure TPlayListManager.DelVisibleItems(const iPlaylist: Integer);
var
  P: Cardinal;
  I, Len: Integer;
begin
  if iPlaylist = -1 then
    P := CurPlaylist
  else if (iPlaylist >= 0) AND (iPlaylist <= high(Playlists)) then
    P := iPlaylist
  else
    exit;

  Len := 0;
  for I := 0 to High(Playlists[P].Items) do
  begin
    if (Playlists[P].Items[I].SongID < Low(CatSongs.Song)) or
       (Playlists[P].Items[I].SongID > High(CatSongs.Song)) or
       (not CatSongs.Song[Playlists[P].Items[I].SongID].Visible) then
    begin
      Playlists[P].Items[Len] := Playlists[P].Items[I];
      Inc(Len);
    end;
  end;
  SetLength(Playlists[P].Items, Len);
  SavePlayList(P);

  if (CatSongs.CatNumShow = -3) and (P = CurPlaylist) then
  begin
    if Length(Playlists[P].Items) = 0 then
    begin
      ScreenSong.UnloadCover(ScreenSong.Interaction);
      ScreenSong.HideCatTL;
      CatSongs.SetFilter('', fltAll);
      ScreenSong.Interaction := 0;
      ScreenSong.FixSelected;
      ScreenSong.ChangeMusic;
      UnsetPlaylist;
    end
    else
      SetPlaylist(P);
  end;
end;

procedure TPlayListManager.SortPlaylist(const Order: TPlaylistSortOrder; const iPlaylist: Integer);
var
  P: Cardinal;
begin
  if iPlaylist = -1 then
    P := CurPlaylist
  else if (iPlaylist >= 0) AND (iPlaylist <= high(Playlists)) then
    P := iPlaylist
  else
    exit;

  SortItems(Playlists[P].Items, Order);
  Playlists[P].FixedOrder := True;
  SavePlayList(P);

  if (CatSongs.CatNumShow = -3) and (P = CurPlaylist) then
    SetPlaylist(P);
end;

function TPlayListManager.MoveItem(const SongID: Cardinal; const Direction: Integer; const iPlaylist: Integer): Integer;
var
  P: Cardinal;
  ItemIndex, NewIndex, I, MovedSongID: Integer;
  Item: TPlaylistItem;
  WasVisible: APlaylistItem;

  function WasShown(const Song: TSong): Boolean;
  var
    V: Integer;
  begin
    Result := False;
    for V := 0 to High(WasVisible) do
    begin
      if (WasVisible[V].Artist = Song.Artist) and
         (WasVisible[V].Title = Song.Title) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  function PlaylistItemWasShown(const PlaylistItem: TPlaylistItem): Boolean;
  var
    V: Integer;
  begin
    Result := False;
    for V := 0 to High(WasVisible) do
    begin
      if (WasVisible[V].Artist = PlaylistItem.Artist) and
         (WasVisible[V].Title = PlaylistItem.Title) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
begin
  Result := -1;

  if Direction = 0 then
    Exit;

  if iPlaylist = -1 then
    P := CurPlaylist
  else if (iPlaylist >= 0) AND (iPlaylist <= high(Playlists)) then
    P := iPlaylist
  else
    exit;

  ItemIndex := GetIndexbySongID(SongID, P);
  if ItemIndex = -1 then
    Exit;

  SetLength(WasVisible, 0);
  for I := Low(CatSongs.Song) to High(CatSongs.Song) do
  begin
    if CatSongs.Song[I].Visible and (not CatSongs.Song[I].Main) then
    begin
      SetLength(WasVisible, Length(WasVisible) + 1);
      WasVisible[High(WasVisible)].Artist := CatSongs.Song[I].Artist;
      WasVisible[High(WasVisible)].Title := CatSongs.Song[I].Title;
      WasVisible[High(WasVisible)].SongID := I;
    end;
  end;

  NewIndex := ItemIndex;
  repeat
    Inc(NewIndex, Direction);
  until (NewIndex < 0) or
        (NewIndex > High(Playlists[P].Items)) or
        PlaylistItemWasShown(Playlists[P].Items[NewIndex]);

  if NewIndex < 0 then
    NewIndex := 0
  else if NewIndex > High(Playlists[P].Items) then
    NewIndex := High(Playlists[P].Items);

  if NewIndex = ItemIndex then
  begin
    Result := SongID;
    Exit;
  end;

  Item := Playlists[P].Items[ItemIndex];
  if ItemIndex > NewIndex then
  begin
    for I := ItemIndex downto NewIndex + 1 do
      Playlists[P].Items[I] := Playlists[P].Items[I - 1];
  end
  else
  begin
    for I := ItemIndex to NewIndex - 1 do
      Playlists[P].Items[I] := Playlists[P].Items[I + 1];
  end;
  Playlists[P].Items[NewIndex] := Item;

  Playlists[P].FixedOrder := True;
  SavePlayList(P);
  SetPlaylist(P);

  for I := Low(CatSongs.Song) to High(CatSongs.Song) do
    CatSongs.Song[I].Visible := CatSongs.Song[I].Visible and WasShown(CatSongs.Song[I]);

  CatSongs.ResetVisibleIndexCache;

  MovedSongID := FindSongIndexByNames(Item.Artist, Item.Title);
  if MovedSongID <> -1 then
  begin
    ScreenSong.SkipTo(CatSongs.VisibleIndex(MovedSongID), MovedSongID, CatSongs.VisibleSongs);
    Result := MovedSongID;
  end
  else
    Result := SongID;
end;

//----------
//GetNames - Writes Playlist Names in a Array
//----------
procedure TPlayListManager.GetNames(var PLNames: array of UTF8String);
var
  I: Integer;
  Len: Integer;
begin
  Len := High(Playlists);
  
  if (Length(PLNames) <> Len + 1) then
    exit;

  For I := 0 to Len do
    PLNames[I] := Playlists[I].Name;
end;

//----------
//GetIndexbySongID - Returns Index in the specified Playlist of the given Song
//----------
Function    TPlayListManager.GetIndexbySongID(const SongID: Cardinal; const iPlaylist: Integer): Integer;
var
  P: Integer;
  I: Integer;
begin
  Result := -1;

  if iPlaylist = -1 then
    P := CurPlaylist
  else if (iPlaylist >= 0) AND (iPlaylist <= high(Playlists)) then
    P := iPlaylist
  else
    exit;

  For I := 0 to high(Playlists[P].Items) do
  begin
    if (Playlists[P].Items[I].SongID = Int(SongID)) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TPlayListManager.FindSongIndexByNames(const Artist, Title: UTF8String): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := Low(CatSongs.Song) to High(CatSongs.Song) do
  begin
    if CatSongs.Song[I].Main then
      Continue;

    if (CatSongs.Song[I].Artist = Artist) and (CatSongs.Song[I].Title = Title) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TPlayListManager.PlaylistContainsSongID(const iPlaylist: Cardinal; const SongID: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;

  if iPlaylist > High(Playlists) then
    Exit;

  for I := 0 to High(Playlists[iPlaylist].Items) do
  begin
    if Playlists[iPlaylist].Items[I].SongID = SongID then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TPlayListManager.MoveSongIndex(const OldIndex, NewIndex: Integer);
var
  SongRef: TSong;
  I: Integer;
begin
  if (OldIndex < 0) or (OldIndex > High(CatSongs.Song)) or
     (NewIndex < 0) or (NewIndex > High(CatSongs.Song)) or
     (OldIndex = NewIndex) then
    Exit;

  SongRef := CatSongs.Song[OldIndex];

  if OldIndex > NewIndex then
  begin
    for I := OldIndex downto NewIndex + 1 do
      CatSongs.Song[I] := CatSongs.Song[I-1];
  end
  else
  begin
    for I := OldIndex to NewIndex - 1 do
      CatSongs.Song[I] := CatSongs.Song[I+1];
  end;

  CatSongs.Song[NewIndex] := SongRef;
end;

procedure TPlayListManager.ReindexAllPlaylists;
var
  P, I, NewIndex: Integer;
begin
  for P := 0 to High(Playlists) do
  begin
    for I := 0 to High(Playlists[P].Items) do
    begin
      NewIndex := FindSongIndexByNames(Playlists[P].Items[I].Artist,
                                       Playlists[P].Items[I].Title);
      if NewIndex = -1 then
        Log.LogError('Could not reindex playlist entry: ' + Playlists[P].Items[I].Artist + ' - ' + Playlists[P].Items[I].Title);
      Playlists[P].Items[I].SongID := NewIndex;
    end;
  end;
end;

procedure TPlayListManager.ApplyPlaylistOrder(Index: Cardinal);
var
  I, SongIdx: Integer;
begin
  if (Index > High(Playlists)) then
    Exit;

  if Length(Playlists[Index].Items) = 0 then
    Exit;

  for I := High(Playlists[Index].Items) downto 0 do
  begin
    SongIdx := FindSongIndexByNames(Playlists[Index].Items[I].Artist,
                                    Playlists[Index].Items[I].Title);
    if SongIdx <> -1 then
      MoveSongIndex(SongIdx, 0);
  end;

  ReindexAllPlaylists;
  SongOrderDirty := True;
end;

procedure TPlayListManager.SortItems(var Items: APlaylistItem; Order: TPlaylistSortOrder);
var
  I, J, R: Integer;
  Item: TPlaylistItem;

  function CompareItems(const Left, Right: TPlaylistItem): Integer;
  var
    LeftSong, RightSong: TSong;
  begin
    LeftSong := nil;
    RightSong := nil;

    if (Left.SongID >= 0) and (Left.SongID <= High(CatSongs.Song)) then
      LeftSong := CatSongs.Song[Left.SongID];
    if (Right.SongID >= 0) and (Right.SongID <= High(CatSongs.Song)) then
      RightSong := CatSongs.Song[Right.SongID];

    case Order of
      psoArtist:
        Result := UTF8CompareText(Left.Artist, Right.Artist);
      psoTitle:
        Result := UTF8CompareText(Left.Title, Right.Title);
      psoEdition:
        if Assigned(LeftSong) and Assigned(RightSong) then
          Result := UTF8CompareText(LeftSong.Edition, RightSong.Edition)
        else
          Result := 0;
      psoGenre:
        if Assigned(LeftSong) and Assigned(RightSong) then
          Result := UTF8CompareText(LeftSong.Genre, RightSong.Genre)
        else
          Result := 0;
      psoLanguage:
        if Assigned(LeftSong) and Assigned(RightSong) then
          Result := UTF8CompareText(LeftSong.Language, RightSong.Language)
        else
          Result := 0;
      else
        Result := 0;
    end;

    if Result = 0 then
      Result := UTF8CompareText(Left.Artist, Right.Artist);
    if Result = 0 then
      Result := UTF8CompareText(Left.Title, Right.Title);
  end;

begin
  if Length(Items) < 2 then
    Exit;

  if Order = psoShuffle then
  begin
    for I := High(Items) downto 1 do
    begin
      R := Random(I + 1);
      Item := Items[I];
      Items[I] := Items[R];
      Items[R] := Item;
    end;
    Exit;
  end;

  for I := 1 to High(Items) do
  begin
    Item := Items[I];
    J := I;
    while (J > 0) and (CompareItems(Item, Items[J - 1]) < 0) do
    begin
      Items[J] := Items[J - 1];
      Dec(J);
    end;
    Items[J] := Item;
  end;
end;

procedure TPlayListManager.RestoreSongOrder;
begin
  if not SongOrderDirty then
    Exit;

  CatSongs.Refresh;
  ScreenSong.SyncCoversToSongs;
  ReindexAllPlaylists;
  SongOrderDirty := False;
end;

end.
