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
  USong;

type
  TPlaylistItem = record
    Artist: String;
    Title:  String;
    SongID: Integer;
  end;

  APlaylistItem = array of TPlaylistItem;

  TPlaylist = record
    Name:     String;
    Filename: String;
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
      Procedure   LoadPlayLists;
      Function    LoadPlayList(Index: Cardinal; Filename: String): Boolean;
      Procedure   SavePlayList(Index: Cardinal);

      Procedure   SetPlayList(Index: Cardinal);

      Function    AddPlaylist(Name: String): Cardinal;
      Procedure   DelPlaylist(const Index: Cardinal);

      Procedure   AddItem(const SongID: Cardinal; const iPlaylist: Integer = -1);
      Procedure   DelItem(const iItem: Cardinal; const iPlaylist: Integer = -1);

      Procedure   GetNames(var PLNames: array of String);
      Function    GetIndexbySongID(const SongID: Cardinal; const iPlaylist: Integer = -1): Integer;
    end;

    {Modes:
      0: Standard Mode
      1: Category Mode
      2: PlayList Mode}

  var
    PlayListMan:  TPlaylistManager;


implementation

uses USongs,
     ULog,
     UMain,
     //UFiles,
     UGraphic,
     UThemes,
     SysUtils;

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
  SR:   TSearchRec;
  Len:  Integer;
  PlayListBuffer: TPlayList;
begin
  SetLength(Playlists, 0);

  if FindFirst(PlayListPath + '*.upl', 0, SR) = 0 then
  begin
    repeat
      Len := Length(Playlists);
      SetLength(Playlists, Len +1);

      if not LoadPlayList (Len, Sr.Name) then
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

    until FindNext(SR) <> 0;
    FindClose(SR);
  end;  
end;

//----------
//LoadPlayList - Load a Playlist in the Array
//----------
Function    TPlayListManager.LoadPlayList(Index: Cardinal; Filename: String): Boolean;
  var
    F: TextFile;
    Line: String;
    PosDelimiter: Integer;
    SongID: Integer;
    Len: Integer;

  Function FindSong(Artist, Title: String): Integer;
  var I: Integer;
  begin
    Result := -1;

    For I := low(CatSongs.Song) to high(CatSongs.Song) do
    begin
      if (CatSongs.Song[I].Title = Title) AND (CatSongs.Song[I].Artist = Artist) then
      begin
        Result := I;
        Break;
      end;
    end;
  end;
begin
  if not FileExists(PlayListPath + Filename) then
  begin
    Log.LogError('Could not load Playlist: ' + Filename);
    Result := False;
    Exit;
  end;
  Result := True;

  //Load File
  AssignFile(F, PlayListPath + FileName);
  Reset(F);

  //Set Filename
  PlayLists[Index].Filename := Filename;
  PlayLists[Index].Name := '';

  //Read Until End of File
  While not Eof(F) do
  begin
    //Read Curent Line
    Readln(F, Line);

    if (Length(Line) > 0) then
    begin
    PosDelimiter := Pos(':', Line);
      if (PosDelimiter  <> 0) then
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
          else Log.LogError('Could not find Song in Playlist: ' + PlayLists[Index].Filename + ', ' + Line);
        end;
      end;
    end;
  end;

  //If no special name is given, use Filename
  if PlayLists[Index].Name = '' then
  begin
    PlayLists[Index].Name := ChangeFileExt(FileName, '');
  end;

  //Finish (Close File)
  CloseFile(F);
end;

//----------
//SavePlayList - Saves the specified Playlist
//----------
Procedure   TPlayListManager.SavePlayList(Index: Cardinal);
var
  F: TextFile;
  I: Integer;
begin
  if (Not FileExists(PlaylistPath + Playlists[Index].Filename)) OR (Not FileisReadOnly(PlaylistPath + Playlists[Index].Filename)) then
  begin

    //open File for Rewriting
    AssignFile(F, PlaylistPath + Playlists[Index].Filename);
    try
      try
        Rewrite(F);

        //Write Version (not nessecary but helpful)
        WriteLn(F, '######################################');
        WriteLn(F, '#Ultrastar Deluxe Playlist Format v1.0');
        WriteLn(F, '#Playlist "' + Playlists[Index].Name + '" with ' + InttoStr(Length(Playlists[Index].Items)) + ' Songs.');
        WriteLn(F, '######################################');

        //Write Name Information
        WriteLn(F, '#Name: ' + Playlists[Index].Name);

        //Write Song Information
        WriteLn(F, '#Songs:');

        For I := 0 to high(Playlists[Index].Items) do
        begin
          WriteLn(F, Playlists[Index].Items[I].Artist + ' : ' + Playlists[Index].Items[I].Title);
        end;
      except
        log.LogError('Could not write Playlistfile "' + Playlists[Index].Name + '"');
      end;
    finally
      CloseFile(F);
    end;
  end;
end;

//----------
//SetPlayList - Display a Playlist in CatSongs
//----------
Procedure   TPlayListManager.SetPlayList(Index: Cardinal);
var
  I: Integer;
begin
  If (Int(Index) > High(PlayLists)) then
    exit;

  //Hide all Songs
  For I := 0 to high(CatSongs.Song) do
     CatSongs.Song[I].Visible := False;

  //Show Songs in PL
  For I := 0 to high(PlayLists[Index].Items) do
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
Function    TPlayListManager.AddPlaylist(Name: String): Cardinal;
var
  I: Integer;
begin
  Result := Length(Playlists);
  SetLength(Playlists, Result + 1);
  
  // Sort the Playlists - Insertion Sort
  while (Result > 0) AND (CompareText(Playlists[Result - 1].Name, Name) >= 0) do
  begin
    Dec(Result);
    Playlists[Result+1] := Playlists[Result];
  end;
  Playlists[Result].Name      := Name;

  I := 1;
  if (not FileExists(PlaylistPath + Name + '.upl')) then
    Playlists[Result].Filename  := Name + '.upl'
  else
  begin
    repeat
      Inc(I);
    until not FileExists(PlaylistPath + Name + InttoStr(I) + '.upl');
    Playlists[Result].Filename := Name + InttoStr(I) + '.upl';
  end;

  //Save new Playlist
  SavePlayList(Result);
end;

//----------
//DelPlaylist - Deletes a Playlist
//----------
Procedure   TPlayListManager.DelPlaylist(const Index: Cardinal);
var
  I: Integer;
  Filename: String;
begin
  If Int(Index) > High(Playlists) then
    Exit;

  Filename := PlaylistPath + Playlists[Index].Filename;

  //If not FileExists or File is not Writeable then exit
  If (Not FileExists(Filename)) OR (FileisReadOnly(Filename)) then
    Exit;


  //Delete Playlist from FileSystem
  if Not DeleteFile(Filename) then
    Exit;

  //Delete Playlist from Array
  //move all PLs to the Hole
  For I := Index to High(Playlists)-1 do
    PlayLists[I] := PlayLists[I+1];

  //Delete last Playlist
  SetLength (Playlists, High(Playlists));

  //If Playlist is Displayed atm
  //-> Display Songs
  if (CatSongs.CatNumShow = -3) and (Index = CurPlaylist) then
  begin
    ScreenSong.UnLoadDetailedCover;
    ScreenSong.HideCatTL;
    CatSongs.SetFilter('', 0);
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
Procedure    TPlayListManager.GetNames(var PLNames: array of String);
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
