unit UPliki;

interface

uses USongs, SysUtils, ULog, UMusic;

procedure InitializePaths;
function ReadHeader(var Song: TSong): boolean;
function SkanujPlik(var Song: TSong): boolean;
procedure CzyscNuty;
function WczytajCzesci(Name: string): boolean;
function SaveSong(Song: TSong; Czesc: TCzesci; Name: string; Relative: boolean): boolean;
function SaveSongDebug(Song: TSong; Czesc: TCzesci; Name: string; Relative: boolean): boolean;

var
  GamePath:         string;
  SoundPath:        string;
  SongPath:         string;
  LogPath:          string;
  ThemePath:        string;
  ScreenshotsPath:  string;
  CoversPath:       string;
  LanguagesPath:    string;
  PluginPath:       string;

  Plik:       TextFile;   // all procedures in this unit operates on this file
  PlikC:      char;
  Lineno:   integer;

  // variables available for all procedures
  Base:       array[0..1] of integer;
  Rel:        array[0..1] of integer;
  Mult:     integer;
  MultBPM:  integer;

implementation
uses TextGL, UIni, UMain, math;

procedure InitializePaths;
begin
  GamePath :=   ExtractFilePath(ParamStr(0));
  SoundPath :=  GamePath + 'Sounds\';
  SongPath :=   GamePath + 'Songs\';
  LogPath := GamePath;
  ThemePath := GamePath + 'Themes\';
  ScreenshotsPath := GamePath + 'Screenshots\';
  CoversPath := GamePath + 'Covers\';
  LanguagesPath := GamePath + 'Languages\';
  //Modi Loader
  PluginPath := GamePath + 'Plugins\';

  DecimalSeparator := ',';
end;

function ReadHeader(var Song: TSong): boolean;
var
  TempC:    char;
  Tekst:    string;
  Done:     integer;
begin
  // clear
  Song.Title := '';
  Song.Artist := '';
  Song.Genre := 'Unknown';
  Song.Edition := 'Unknown';
  Song.Language := 'Unknown'; //Language Patch
  Song.Mp3 := '';
  Song.BPM := 0;
  Song.GAP := 0;
  Song.Start := 0;
  Song.Finish := 0;
  Song.Background := '';
  Song.Video := '';
  Song.VideoGAP := 0;
  Song.NotesGAP := 0;
  Song.Resolution := 4;

  //Creator Patch
  Song.Creator := '';

  Done := 0;

  //Editor Error Reporting Hack
  LineNo := 0;
  try

  // read
  Read(Plik, PlikC);
  while (PlikC = '#') do begin
    ReadLn(Plik, Tekst);

    //Editor Error Reporting Hack
    Inc (LineNo);

    //Header Improvements Patch

    if UpperCase(Copy(Tekst, 1, 6)) = 'TITLE:' then begin
      Delete(Tekst, 1, 6);
      Song.Title := Trim(Tekst);
      Tekst := '';
      Done := Done or 1;
    end

    else if UpperCase(Copy(Tekst, 1, 7)) = 'ARTIST:' then begin
      Delete(Tekst, 1, 7);
      Song.Artist := Trim(Tekst);
      Tekst := '';
      Done := Done or 2;
    end

    else if UpperCase(Copy(Tekst, 1, 4)) = 'MP3:' then begin
      Delete(Tekst, 1, 4);
      Song.Mp3 := Trim(Tekst);
      Tekst := '';
      Done := Done or 4;
    end

    else if UpperCase(Copy(Tekst, 1, 8)) = 'CREATOR:' then begin // this goes for edit
      Delete(Tekst, 1, 8);
      Song.Creator := Trim(Tekst);
      Tekst := '';
    end

    else if UpperCase(Copy(Tekst, 1, 6)) = 'GENRE:' then begin // this goes for edit
      Delete(Tekst, 1, 6);
      Song.Genre := Trim(Tekst);
      Tekst := '';
    end

    else if UpperCase(Copy(Tekst, 1, 8)) = 'EDITION:' then begin // this goes for edit
      Delete(Tekst, 1, 8);
      Song.Edition := Trim(Tekst);
      Tekst := '';
    end

    else if UpperCase(Copy(Tekst, 1, 9)) = 'LANGUAGE:' then begin // this goes for edit
      Delete(Tekst, 1, 9);
      Song.Language := Trim(Tekst);
      Tekst := '';
    end

    else if UpperCase(Copy(Tekst, 1, 6)) = 'COVER:' then begin
      Delete(Tekst, 1, 6);
      Song.Cover := Trim(Tekst);
      Tekst := '';
    end

    else if UpperCase(Copy(Tekst, 1, 11)) = 'BACKGROUND:' then begin
      Delete(Tekst, 1, 11);
      Song.Background := Trim(Tekst);
      Tekst := '';
    end

    else if UpperCase(Copy(Tekst, 1, 6)) = 'VIDEO:' then begin
      Delete(Tekst, 1, 6);
      Song.Video := Trim(Tekst);
      Tekst := '';
    end

    else if UpperCase(Copy(Tekst, 1, 9)) = 'VIDEOGAP:' then begin
      Delete(Tekst, 1, 9);

      //Change . to , Mod by Whiteshark :P
      if (Pos('.',Tekst) <> 0) then
      begin
        Tekst[Pos('.',Tekst)] := ',';
        //Little Annonce for the User
        Log.LogError('VideoGap Seperator wrong in SongHeader: ' + Song.FileName + ' [Corrected for this Session]');
      end;

      Song.VideoGAP := StrToFloat(Tekst);
      Tekst := ''
    end

    else if UpperCase(Copy(Tekst, 1, 9)) = 'NOTESGAP:' then begin
      Delete(Tekst, 1, 9);
      Song.NotesGAP := StrToInt(Tekst);
      Tekst := ''
    end

    else if UpperCase(Copy(Tekst, 1, 9)) = 'RELATIVE:' then begin
      Delete(Tekst, 1, 9);
      if LowerCase(Tekst) = 'yes' then Song.Relative := true;
    end

    else if UpperCase(Copy(Tekst, 1, 6)) = 'START:' then begin
      Delete(Tekst, 1, 6);
      Song.Start := StrToFloat(Tekst);
//      Muzyka.Start := StrToInt(Tekst);
    end

    else if UpperCase(Copy(Tekst, 1, 4)) = 'END:' then begin
      Delete(Tekst, 1, 4);
      Song.Finish := StrToInt(Tekst);
    end

    else if UpperCase(Copy(Tekst, 1, 11)) = 'RESOLUTION:' then begin
      Delete(Tekst, 1, 11);
      Song.Resolution := StrToInt(Tekst);
    end

    else if UpperCase(Copy(Tekst, 1, 4)) = 'BPM:' then begin
      Delete(Tekst, 1, 4);

//      Muzyka.BPMOld := StrToFloat(Tekst) * Mult * MultBPM; // old system

      (* new system with variable BPM *)
//      Muzyka.BPMOld := 50;

      //Change . to , Mod by Whiteshark :P
      if (Pos('.',Tekst) <> 0) then
      begin
        Tekst[Pos('.',Tekst)] := ',';
        //Little Annonce for the User
        Log.LogError('BPM Seperator wrong in SongHeader: ' + Song.FileName + ' [Corrected for this Session]');
      end;

      SetLength(Song.BPM, 1);
      Song.BPM[0].StartBeat := 0;
      Song.BPM[0].BPM := StrToFloat(Tekst) * Mult * MultBPM;
      Tekst := '';
      Done := Done or 8;
    end

    else if UpperCase(Copy(Tekst, 1, 4)) = 'GAP:' then begin
      Delete(Tekst, 1, 4);
      Song.GAP := StrToFloat(Tekst);
     Tekst := '';
//      Muzyka.GAP := StrToFloat(Tekst);
//      Done := Done or 16;
    end;

    //Header Improvements Patch Ende

    Read(Plik, PlikC);
  end;

  //Editor Error Reporting Hack
  except //An Error happened<- bad english :P
  Log.LogError('An Error occured reading Line ' + inttostr(LineNo) + ' from SongHeader: ' + Song.FileName);
  Halt;
  end;
  //Editor Error Reporting Hack End

  if Song.Background = '' then begin
    Song.Background := Songs.FindSongFile(Song.Path, '*[BG].jpg');
  end;

  if (Done and 15) = 15 then Result := true
  else Result := false;
end;

function SkanujPlik(var Song: TSong): boolean;
var
  Done:     integer;
  Tekst:    string;
  C:        integer; // category
  P:        integer; // position
begin
//  try
  AssignFile(Plik, Song.Path + Song.FileName);
  Reset(Plik);

  Result := ReadHeader(Song);

{  ReadLn(Plik, Tekst);
  while (Copy(Tekst, 1, 1) = '#') do begin
    if Copy(Tekst, 1, 10) = '#CATEGORY:' then begin
      Delete(Tekst, 1, 10);

      Trim(Tekst);
      while (Length(Tekst) > 0) do begin
        C := Length(Song.Category);
        SetLength(Song.Category, C+1);

        P := Pos(',', Tekst);
        if P = 0 then P := Length(Tekst);
        Song.Category[C] := Copy(Tekst, 1, P);

        Delete(Tekst, 1, P);
        Trim(Tekst);
      end;

    end;}


end;

procedure CzyscNuty;
var
  Pet:  integer;
begin
  SetLength(Czesci, Length(Player));
  SetLength(AktSong.BPM, 0);
  for Pet := 0 to High(Player) do begin
    SetLength(Czesci[Pet].Czesc, 1);
    SetLength(Czesci[Pet].Czesc[0].Nuta, 0);
    Czesci[Pet].Czesc[0].Lyric := '';
    Czesci[Pet].Czesc[0].LyricWidth := 0;
    Player[pet].Score := 0;
    Player[pet].IlNut := 0;
    Player[pet].HighNut := -1;
  end;
end;

procedure DodajNute(NrCzesci: integer; TypeP: char; StartP, DurationP, NoteP: integer; LyricS: string);
var
  Space:  boolean;
begin
  case Ini.Solmization of
    1:  // european
      begin
        case (NoteP mod 12) of
          0..1:  LyricS := ' do ';
          2..3:  LyricS := ' re ';
          4:  LyricS := ' mi ';
          5..6:  LyricS := ' fa ';
          7..8:  LyricS := ' sol ';
          9..10:  LyricS := ' la ';
          11:  LyricS := ' si ';
        end;
      end;
    2:  // japanese
      begin
        case (NoteP mod 12) of
          0..1:  LyricS := ' do ';
          2..3:  LyricS := ' re ';
          4:  LyricS := ' mi ';
          5..6:  LyricS := ' fa ';
          7..8:  LyricS := ' so ';
          9..10:  LyricS := ' la ';
          11:  LyricS := ' shi ';
        end;
      end;
    3:  // american
      begin
        case (NoteP mod 12) of
          0..1:  LyricS := ' do ';
          2..3:  LyricS := ' re ';
          4:  LyricS := ' mi ';
          5..6:  LyricS := ' fa ';
          7..8:  LyricS := ' sol ';
          9..10:  LyricS := ' la ';
          11:  LyricS := ' ti ';
        end;
      end;
  end; // case

//  Log.LogStatus('Czesc: ' + IntToStr(Czesci[NrCzesci].High), 'DodajNute');
//  Log.LogStatus('Dodano: [' + IntToStr(NrCzesci) + '] ' + IntToStr(StartP) + ' '
//    + IntToStr(DurationP) + ' '+ IntToStr(NoteP) + ' ' + LyricS, 'DodajNute');

{  Delete(LyricS, 1, 1);
  Space := false;
  if Copy(LyricS, Length(LyricS), 1) = ' ' then begin
    Space := true;
    Delete(LyricS, Length(LyricS), 1);
  end;
  if LyricS = 'a' then LyricS := chr($B1);
  if LyricS = 'i' then LyricS := chr($B2);
  if LyricS = 'u' then LyricS := chr($B3);
  if LyricS = 'e' then LyricS := chr($B4);
  if LyricS = 'o' then LyricS := chr($B5);

  if LyricS = 'ka' then LyricS := chr($B6);
  if LyricS = 'ki' then LyricS := chr($B7);
  if LyricS = 'ku' then LyricS := chr($B8);
  if LyricS = 'ke' then LyricS := chr($B9);
  if LyricS = 'ko' then LyricS := chr($BA);

  if LyricS = 'ga' then LyricS := chr($B6) + chr($DE);
  if LyricS = 'gi' then LyricS := chr($B7) + chr($DE);
  if LyricS = 'gu' then LyricS := chr($B8) + chr($DE);
  if LyricS = 'ge' then LyricS := chr($B9) + chr($DE);
  if LyricS = 'go' then LyricS := chr($BA) + chr($DE);

  if LyricS = 'sa' then LyricS := chr($BB);
  if LyricS = 'shi' then LyricS := chr($BC);
  if LyricS = 'su' then LyricS := chr($BD);
  if LyricS = 'se' then LyricS := chr($BE);
  if LyricS = 'so' then LyricS := chr($BF);

  if LyricS = 'za' then LyricS := chr($BB) + chr($DE);
  if LyricS = 'ji' then LyricS := chr($BC) + chr($DE);
  if LyricS = 'zu' then LyricS := chr($BD) + chr($DE);
  if LyricS = 'ze' then LyricS := chr($BE) + chr($DE);
  if LyricS = 'zo' then LyricS := chr($BF) + chr($DE);

  if LyricS = 'ta' then LyricS := chr($C0);
  if LyricS = 'chi' then LyricS := chr($C1);
  if LyricS = 'tsu' then LyricS := chr($C2);
  if LyricS = 'te' then LyricS := chr($C3);
  if LyricS = 'to' then LyricS := chr($C4);

  if LyricS = 'da' then LyricS := chr($C0) + chr($DE);
//  if LyricS = 'ji' then LyricS := chr($C1) + chr($DE);
//  if LyricS = 'zu' then LyricS := chr($C2) + chr($DE);
  if LyricS = 'de' then LyricS := chr($C3) + chr($DE);
  if LyricS = 'do' then LyricS := chr($C4) + chr($DE);

  if LyricS = 'na' then LyricS := chr($C5);
  if LyricS = 'ni' then LyricS := chr($C6);
  if LyricS = 'nu' then LyricS := chr($C7);
  if LyricS = 'ne' then LyricS := chr($C8);
  if LyricS = 'no' then LyricS := chr($C9);

  if LyricS = 'ha' then LyricS := chr($CA);
  if LyricS = 'hi' then LyricS := chr($CB);
  if LyricS = 'hu' then LyricS := chr($CC);
  if LyricS = 'he' then LyricS := chr($CD);
  if LyricS = 'ho' then LyricS := chr($CE);

  if LyricS = 'ba' then LyricS := chr($CA) + chr($DE);
  if LyricS = 'bi' then LyricS := chr($CB) + chr($DE);
  if LyricS = 'bu' then LyricS := chr($CC) + chr($DE);
  if LyricS = 'be' then LyricS := chr($CD) + chr($DE);
  if LyricS = 'bo' then LyricS := chr($CE) + chr($DE);

  if LyricS = 'pa' then LyricS := chr($CA) + chr($DF);
  if LyricS = 'pi' then LyricS := chr($CB) + chr($DF);
  if LyricS = 'pu' then LyricS := chr($CC) + chr($DF);
  if LyricS = 'pe' then LyricS := chr($CD) + chr($DF);
  if LyricS = 'po' then LyricS := chr($CE) + chr($DF);

  if LyricS = 'ma' then LyricS := chr($CF);
  if LyricS = 'mi' then LyricS := chr($D0);
  if LyricS = 'mu' then LyricS := chr($D1);
  if LyricS = 'me' then LyricS := chr($D2);
  if LyricS = 'mo' then LyricS := chr($D3);

  if LyricS = 'ya' then LyricS := chr($D4);
  if LyricS = 'yu' then LyricS := chr($D5);
  if LyricS = 'yo' then LyricS := chr($D6);

  if LyricS = 'ra' then LyricS := chr($D7);
  if LyricS = 'ri' then LyricS := chr($D8);
  if LyricS = 'ru' then LyricS := chr($D9);
  if LyricS = 're' then LyricS := chr($DA);
  if LyricS = 'ro' then LyricS := chr($DB);

  if LyricS = 'wa' then LyricS := chr($DC);
  if LyricS = 'n' then LyricS := chr($DD);

  LyricS := ' ' + LyricS;
  if Space then LyricS := LyricS + ' ';}



  with Czesci[NrCzesci].Czesc[Czesci[NrCzesci].High] do begin
    SetLength(Nuta, Length(Nuta) + 1);
    IlNut := IlNut + 1;
    HighNut := HighNut + 1;
    Muzyka.IlNut := Muzyka.IlNut + 1;

    Nuta[HighNut].Start := StartP;
    if IlNut = 1 then begin
      StartNote := Nuta[HighNut].Start;
      if Czesci[NrCzesci].Ilosc = 1 then
        Start := -100;
//        Start := Nuta[HighNut].Start;
    end;

    Nuta[HighNut].Dlugosc := DurationP;
    Muzyka.DlugoscNut := Muzyka.DlugoscNut + Nuta[HighNut].Dlugosc;

    // back to the normal system with normal, golden and now freestyle notes
    case TypeP of
      'F':  Nuta[HighNut].Wartosc := 0;
      ':':  Nuta[HighNut].Wartosc := 1;
      '*':  Nuta[HighNut].Wartosc := 2;
    end;
    Czesci[NrCzesci].Wartosc := Czesci[NrCzesci].Wartosc + Nuta[HighNut].Dlugosc * Nuta[HighNut].Wartosc;

    Nuta[HighNut].Ton := NoteP;
    if Nuta[HighNut].Ton < Base[NrCzesci] then Base[NrCzesci] := Nuta[HighNut].Ton;
    Nuta[HighNut].TonGamy := Nuta[HighNut].TonGamy mod 12;

    Nuta[HighNut].Tekst := Copy(LyricS, 2, 100);
    Lyric := Lyric + Nuta[HighNut].Tekst;

    if TypeP = 'F' then
      Nuta[HighNut].FreeStyle := true;

    Koniec := Nuta[HighNut].Start + Nuta[HighNut].Dlugosc;
  end; // with
end;

procedure NewSentence(NrCzesciP: integer; Param1, Param2: integer);
var
I: Integer;
begin
//  Log.LogStatus('IlCzesci: ' + IntToStr(Czesci[NrCzesciP].Ilosc), 'NewSentece');
//  Log.LogStatus('Dane: ' + IntToStr(NrCzesciP) + ' ' + IntToStr(Param1) + ' ' + IntToStr(Param2) , 'NewSentece');

  // stara czesc //Alter Satz //Update Old Part
  Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].BaseNote := Base[NrCzesciP];
  Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].LyricWidth := glTextWidth(PChar(Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Lyric));

  //Total Notes Patch
  Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].TotalNotes := 0;
  for I := low(Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Nuta) to high(Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Nuta) do
  begin
    Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].TotalNotes := Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].TotalNotes + Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Nuta[I].Dlugosc * Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Nuta[I].Wartosc;
  end;
  //Log.LogError('Total Notes(' + inttostr(Czesci[NrCzesciP].High) +'): ' + inttostr(Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].TotalNotes));
  //Total Notes Patch End


  // nowa czesc //Neuer Satz //Update New Part
  SetLength(Czesci[NrCzesciP].Czesc, Czesci[NrCzesciP].Ilosc + 1);
  Czesci[NrCzesciP].High := Czesci[NrCzesciP].High + 1;
  Czesci[NrCzesciP].Ilosc := Czesci[NrCzesciP].Ilosc + 1;
  Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].HighNut := -1;

  if not AktSong.Relative then
    Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Start := Param1;

  if AktSong.Relative then begin
    Czesci[NrCzesciP].Czesc[Czesci[NrCzesciP].High].Start := Param1;
    Rel[NrCzesciP] := Rel[NrCzesciP] + Param2;
  end;

  Base[NrCzesciP] := 100; // high number
end;

function WczytajCzesci(Name: string): boolean;
var
  TempC:    char;
  Tekst:    string;
  CP:       integer; // Current Player (0 or 1)
  Pet:      integer;
  Both:     boolean;
  Param1:   integer;
  Param2:   integer;
  Param3:   integer;
  ParamS:   string;
  I: Integer;
begin
  Result := false;

  if not FileExists(Name) then begin
    Log.LogError('File not found: "' + Name + '"', 'WczytajCzesci');
    exit;
  end;

  try
  MultBPM := 4; // 4 - mnoznik dla czasu nut
  Mult := 1; // 4 - dokladnosc pomiaru nut
  Base[0] := 100; // high number
//  Base[1] := 100; // high number
  Czesci[0].Wartosc := 0;
//  Czesci[1].Wartosc := 0; // here was the error in 0.3.2
  AktSong.Relative := false;

  Rel[0] := 0;
//  Rel[1] := 0;
  CP := 0;
  Both := false;
  if Length(Player) = 2 then Both := true;

  FileMode := fmOpenRead;
  AssignFile(Plik, Name);
  Reset(Plik);

  ReadHeader(AktSong);
(*  if AktSong.Title = 'Hubba Hubba Zoot Zoot' then begin
    Mult := 2;
    AktSong.BPM[0].BPM := AktSong.BPM[0].BPM * 2;
  end;*)

  SetLength(Czesci, 2);
  for Pet := 0 to High(Czesci) do begin
    SetLength(Czesci[Pet].Czesc, 1);
    Czesci[Pet].High := 0;
    Czesci[Pet].Ilosc := 1;
    Czesci[Pet].Akt := 0;
    Czesci[Pet].Resolution := AktSong.Resolution;
    Czesci[Pet].NotesGAP := AktSong.NotesGAP;
    Czesci[Pet].Czesc[0].IlNut := 0;
    Czesci[Pet].Czesc[0].HighNut := -1;
  end;

//  TempC := ':';
  TempC := PlikC; // read from backup variable, don't use default ':' value

  while (TempC <> 'E') do begin
    Inc(LineNo);
    if (TempC = ':') or (TempC = '*') or (TempC = 'F') then begin
      // wczytuje nute
      Read(Plik, Param1);
      Read(Plik, Param2);
      Read(Plik, Param3);
      Read(Plik, ParamS);

      // dodaje nute
      if not Both then
        // P1
        DodajNute(0, TempC, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamS)
      else begin
        // P1 + P2
        DodajNute(0, TempC, (Param1+Rel[0]) * Mult, Param2 * Mult, Param3, ParamS);
        DodajNute(1, TempC, (Param1+Rel[1]) * Mult, Param2 * Mult, Param3, ParamS);
      end;
    end; // if
    if TempC = '-' then begin
      // reads sentence
      Read(Plik, Param1);
      if AktSong.Relative then Read(Plik, Param2); // read one more data for relative system

      // new sentence
      if not Both then
        // P1
        NewSentence(0, (Param1 + Rel[0]) * Mult, Param2)
      else begin
        // P1 + P2
        NewSentence(0, (Param1 + Rel[0]) * Mult, Param2);
        NewSentence(1, (Param1 + Rel[1]) * Mult, Param2);
      end;

    end; // if

    if TempC = 'B' then begin
      SetLength(AktSong.BPM, Length(AktSong.BPM) + 1);
      Read(Plik, AktSong.BPM[High(AktSong.BPM)].StartBeat);
      AktSong.BPM[High(AktSong.BPM)].StartBeat := AktSong.BPM[High(AktSong.BPM)].StartBeat + Rel[0];

      Read(Plik, Tekst);
      AktSong.BPM[High(AktSong.BPM)].BPM := StrToFloat(Tekst);
      AktSong.BPM[High(AktSong.BPM)].BPM := AktSong.BPM[High(AktSong.BPM)].BPM * Mult * MultBPM;
    end;


    if not Both then begin
      Czesci[CP].Czesc[Czesci[CP].High].BaseNote := Base[CP];
      Czesci[CP].Czesc[Czesci[CP].High].LyricWidth := glTextWidth(PChar(Czesci[CP].Czesc[Czesci[CP].High].Lyric));
      //Total Notes Patch
      Czesci[CP].Czesc[Czesci[CP].High].TotalNotes := 0;
      for I := low(Czesci[CP].Czesc[Czesci[CP].High].Nuta) to high(Czesci[CP].Czesc[Czesci[CP].High].Nuta) do
      begin
       Czesci[CP].Czesc[Czesci[CP].High].TotalNotes := Czesci[CP].Czesc[Czesci[CP].High].TotalNotes + Czesci[CP].Czesc[Czesci[CP].High].Nuta[I].Dlugosc * Czesci[CP].Czesc[Czesci[CP].High].Nuta[I].Wartosc;
      end;
      //Total Notes Patch End
    end else begin
      for Pet := 0 to High(Czesci) do begin
        Czesci[Pet].Czesc[Czesci[Pet].High].BaseNote := Base[Pet];
        Czesci[Pet].Czesc[Czesci[Pet].High].LyricWidth := glTextWidth(PChar(Czesci[Pet].Czesc[Czesci[Pet].High].Lyric));
        //Total Notes Patch
        Czesci[Pet].Czesc[Czesci[Pet].High].TotalNotes := 0;
        for I := low(Czesci[Pet].Czesc[Czesci[Pet].High].Nuta) to high(Czesci[Pet].Czesc[Czesci[Pet].High].Nuta) do
        begin
          Czesci[Pet].Czesc[Czesci[Pet].High].TotalNotes := Czesci[Pet].Czesc[Czesci[Pet].High].TotalNotes + Czesci[Pet].Czesc[Czesci[Pet].High].Nuta[I].Dlugosc * Czesci[Pet].Czesc[Czesci[Pet].High].Nuta[I].Wartosc;
        end;
        //Total Notes Patch End
      end;
    end;

    Read(Plik, TempC);
  end; // while}

  CloseFile(Plik);
  except
    Log.LogError('Error Loading File: "' + Name + '" in Line ' + inttostr(LineNo));
    exit;
  end;

  Result := true;
end;

function SaveSong(Song: TSong; Czesc: TCzesci; Name: string; Relative: boolean): boolean;
var
  C:      integer;
  N:      integer;
  S:      string;
  B:      integer;
  RelativeSubTime:    integer;
  NoteState: String;

begin
//  Relative := true; // override (idea - use shift+S to save with relative)
  AssignFile(Plik, Name);
  Rewrite(Plik);

  WriteLn(Plik, '#TITLE:' + Song.Title + '');
  WriteLn(Plik, '#ARTIST:' + Song.Artist);

  if Song.Creator     <> '' then    WriteLn(Plik, '#CREATOR:'     + Song.Creator);
  if Song.Edition     <> 'Unknown' then WriteLn(Plik, '#EDITION:' + Song.Edition);
  if Song.Genre       <> 'Unknown' then   WriteLn(Plik, '#GENRE:' + Song.Genre);
  if Song.Language    <> 'Unknown' then    WriteLn(Plik, '#LANGUAGE:'    + Song.Language);
  if Song.Cover       <> '' then    WriteLn(Plik, '#COVER:'       + Song.Cover);

  WriteLn(Plik, '#MP3:' + Song.Mp3);

  if Song.Background  <> '' then    WriteLn(Plik, '#BACKGROUND:'  + Song.Background);
  if Song.Video       <> '' then    WriteLn(Plik, '#VIDEO:'       + Song.Video);
  if Song.VideoGAP    <> 0  then    WriteLn(Plik, '#VIDEOGAP:'    + FloatToStr(Song.VideoGAP));
  if Song.Resolution  <> 4  then    WriteLn(Plik, '#RESOLUTION:'  + IntToStr(Song.Resolution));
  if Song.NotesGAP    <> 0  then    WriteLn(Plik, '#NOTESGAP:'    + IntToStr(Song.NotesGAP));
  if Song.Start       <> 0  then    WriteLn(Plik, '#START:'       + FloatToStr(Song.Start));
  if Song.Finish      <> 0  then    WriteLn(Plik, '#END:'         + IntToStr(Song.Finish));
  if Relative               then    WriteLn(Plik, '#RELATIVE:yes');

  WriteLn(Plik, '#BPM:' + FloatToStr(Song.BPM[0].BPM / 4));
  WriteLn(Plik, '#GAP:' + FloatToStr(Song.GAP));

  RelativeSubTime := 0;
  for B := 1 to High(AktSong.BPM) do
    WriteLn(Plik, 'B ' + FloatToStr(AktSong.BPM[B].StartBeat) + ' ' + FloatToStr(AktSong.BPM[B].BPM/4));

  for C := 0 to Czesc.High do begin
    for N := 0 to Czesc.Czesc[C].HighNut do begin
      with Czesc.Czesc[C].Nuta[N] do begin


        //Golden + Freestyle Note Patch
        case Czesc.Czesc[C].Nuta[N].Wartosc of
          0: NoteState := 'F ';
          1: NoteState := ': ';
          2: NoteState := '* ';
        end; // case
        S := NoteState + IntToStr(Start-RelativeSubTime) + ' ' + IntToStr(Dlugosc) + ' ' + IntToStr(Ton) + ' ' + Tekst;


        WriteLn(Plik, S);
      end; // with
    end; // N

    if C < Czesc.High then begin      // don't write end of last sentence
      if not Relative then
        S := '- ' + IntToStr(Czesc.Czesc[C+1].Start)
      else begin
        S := '- ' + IntToStr(Czesc.Czesc[C+1].Start - RelativeSubTime) +
          ' ' + IntToStr(Czesc.Czesc[C+1].Start - RelativeSubTime);
        RelativeSubTime := Czesc.Czesc[C+1].Start;
      end;
      WriteLn(Plik, S);
    end;

  end; // C


  WriteLn(Plik, 'E');
  CloseFile(Plik);
end;

function SaveSongDebug(Song: TSong; Czesc: TCzesci; Name: string; Relative: boolean): boolean;
var
  C:      integer;
  N:      integer;
  S:      string;
  STon:   integer;
  SLen:   integer;
  NTot:   integer;
  PlikB:  TextFile;
  LastTime:   integer;
begin
  AssignFile(Plik, Name);
  Rewrite(Plik);

  AssignFile(PlikB, 'C:\song db.asm');
  Rewrite(PlikB);

  NTot := 0;
  LastTime := 0;

  for C := 0 to Czesc.High do begin
    WriteLn(Plik, '; ' + IntToStr(C));

    for N := 0 to Czesc.Czesc[C].HighNut do begin
      with Czesc.Czesc[C].Nuta[N] do begin

        // timespace
        if LastTime < Start then begin
          STon := 0;
          SLen := Round((Start - LastTime) * 16320 / 255 / 12);
          WriteLn(PlikB, '    .dw ' + IntToStr(STon + SLen*256) + ' ; timespace (0, ' + IntToStr(SLen) + ')');

        end;



        // ton
        STon := Round(98940/(2*261.62*Power(1.05946309436, Ton)));
        S := '    ldi R18, ' + IntToStr(STon);
        if STon > 255 then begin
          beep;
          S := '!!!!' + S;
        end;
        WriteLn(Plik, S);

        // length
	      //ldi R19, 43
        SLen := Round(Dlugosc * 16320 / STon / 12);
        S := '    ldi R19, ' + IntToStr(SLen);
        if SLen > 255 then begin
          beep;
          S := '!!!!' + S;
        end;
        WriteLn(Plik, S);

        // function
        S := '    rcall playtone';
        WriteLn(Plik, S);

        // song dw
        WriteLn(PlikB, '    .dw ' + IntToStr(STon + SLen*256));


        LastTime := Start + Dlugosc;
        Inc(NTot);

      end; // with
    end; // N
    WriteLn(Plik, '');
    WriteLn(PlikB, '');
  end; // C

  WriteLn(Plik, '; nut ' + IntToStr(NTot));
  WriteLn(Plik, '; bajtów ' + IntToStr(8*NTot));

  WriteLn(PlikB, '    .dw 0');
  WriteLn(PlikB, '; nut ' + IntToStr(NTot));
  WriteLn(PlikB, '; bajtów ' + IntToStr(2*NTot));


  CloseFile(Plik);
  CloseFile(PlikB);
end;

end.
