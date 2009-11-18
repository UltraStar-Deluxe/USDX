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

unit UPlaylist;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
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
  end;

  APlaylist = array of TPlaylist;

  //----------
  //TPlaylistManager - Class for Managing Playlists (Loading, Displaying, Saving)
  //----------
  TPlaylistManager = class
    private

    public
      Mode:         TSingMode;     //Current Playlist Mode for SongScreen
      CurPlayList:  Cardinal;
      CurItem:      Cardinal;

      Playlists:    APlaylist;

      constructor Create;
      procedure   LoadPlayLists;
      function    LoadPlayList(Index: Cardinal; const Filename: IPath): Boolean;
      procedure   SavePlayList(Index: Cardinal);

      procedure   SetPlayList(Index: Cardinal);

      function    AddPlaylist(const Name: UTF8String): Cardinal;
      procedure   DelPlaylist(const Index: Cardinal);

      procedure   AddItem(const SongID: Cardinal; const iPlaylist: Integer = -1);
      procedure   DelItem(const iItem: Cardinal; const iPlaylist: Integer = -1);

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
  LoadPlayLists;
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
procedure TPlayListManager.SetPlayList(Index: Cardinal);
var
  I: Integer;
begin
  if (Int(Index) > High(PlayLists)) then
    exit;

  //Hide all Songs
  for I := 0 to high(CatSongs.Song) do
     CatSongs.Song[I].Visible := False;

  //Show Songs in PL
  for I := 0 to high(PlayLists[Index].Items) do
  begin
    CatSongs.Song[PlayLists[Index].Items[I].SongID].Visible := True;
  end;

  //Set CatSongsMode + Playlist Mode
  CatSongs.CatNumShow := -3;
  Mode := smPlayListRandom;

  //Set CurPlaylist
  CurPlaylist := Index;

  //Show Cat in Topleft:
  ScreenSong.ShowCatTLCustom(Format(Theme.Playlist.CatText,[Playlists[Index].Name]));

  //Fix SongSelection
  ScreenSong.Interaction := 0;
  ScreenSong.SelectNext;
  ScreenSong.FixSelected;

  //Play correct Music
  ScreenSong.ChangeMusic;
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
    ScreenSong.UnLoadDetailedCover;
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
Procedure   TPlayListManager.AddItem(const SongID: Cardinal; const iPlaylist: Integer);
var
  P: Cardinal;
  Len: Cardinal;
begin
  if iPlaylist = -1 then
    P := CurPlaylist
  else if (iPlaylist >= 0) AND (iPlaylist <= high(Playlists)) then
    P := iPlaylist
  else
    exit;

  if (Int(SongID) <= High(CatSongs.Song)) AND (NOT CatSongs.Song[SongID].Main) then
  begin
    Len := Length(Playlists[P].Items);
    SetLength(Playlists[P].Items, Len + 1);

    Playlists[P].Items[Len].SongID  := SongID;
    Playlists[P].Items[Len].Title   := CatSongs.Song[SongID].Title;
    Playlists[P].Items[Len].Artist  := CatSongs.Song[SongID].Artist;

    //Save Changes
    SavePlayList(P);

    //Correct Display when Editing current Playlist
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

end.
