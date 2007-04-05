unit UPlaylist;

interface

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
      Mode:         Byte;     //Current Playlist Mode for SongScreen
      CurPlayList:  Cardinal;
      CurItem:      Cardinal;

      Playlists:    APlaylist;

      constructor Create;
      Procedure   LoadPlayLists;
      Function    LoadPlayList(Index: Cardinal; Filename: String): Boolean;
      Procedure   SavePlayList(Index: Cardinal);

      Procedure   SetPlayList(Index: Cardinal);

      Function    AddPlaylist(Name: String): Cardinal;

      Procedure   AddItem(const SongID: Cardinal; const iPlaylist: Integer = -1);
      Procedure   DelItem(const iItem: Cardinal; const iPlaylist: Integer = -1);

      Procedure   GetNames(var PLNames: array of String);
      Function    GetIndexbySongID(const SongID: Cardinal; const iPlaylist: Integer = -1): Integer;
    end;

    {Modes:
      0: Standard Mode
      1: PlayList Mode
      2: Category Mode}

  var
    PlayListMan:  TPlaylistManager;


implementation
uses USongs, ULog, UPliki, UGraphic, UThemes, SysUtils;

//----------
//Create - Construct Class - Dummy for now
//----------
constructor TPlayListManager.Create;
begin
  LoadPlayLists;
end;

//----------
//LoadPlayLists - Load list of Playlists from PlayList Folder
//----------
Procedure   TPlayListManager.LoadPlayLists;
var
  SR:   TSearchRec;
  Len:  Integer;
begin
  SetLength(Playlists, 0);

  if FindFirst(PlayListPath + '*.upl', 0, SR) = 0 then
  begin
    repeat
      Len := Length(Playlists);
      SetLength(Playlists, Len +1);

      if not LoadPlayList (Len, Sr.Name) then
        SetLength(Playlists, Len);

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
  If (Index > High(PlayLists)) then
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
  Mode := 1;

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
var I: Integer;
begin
  Result := Length(Playlists);
  SetLength(Playlists, Result + 1);

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

  if (SongID <= High(CatSongs.Song)) AND (NOT CatSongs.Song[SongID].Main) then
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

  if (iItem <= high(Playlists[P].Items)) then
  begin
    //Move all entrys behind deleted one to Front
    For I := iItem to High(Playlists[P].Items) - 1 do
      Playlists[P].Items[I] := Playlists[P].Items[I + 1];

    //Delete Last Entry
    SetLength(PlayLists[P].Items, Length(PlayLists[P].Items) - 1);

    //Save Changes
    SavePlayList(P);
  end;

  //Correct Display when Editing current Playlist
  if (CatSongs.CatNumShow = -3) and (P = CurPlaylist) then
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
  if iPlaylist = -1 then
    P := CurPlaylist
  else if (iPlaylist >= 0) AND (iPlaylist <= high(Playlists)) then
    P := iPlaylist
  else
    exit;

  Result := -1;

  For I := 0 to high(Playlists[P].Items) do
  begin
    if (Playlists[P].Items[I].SongID = SongID) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

end.
