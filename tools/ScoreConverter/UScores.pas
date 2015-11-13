unit UScores;

interface

uses USongs;

procedure ReadScore(var Song: TSong);
procedure WriteScore(var Song: TSong);
procedure AddScore(var Song: TSong; Level: integer; Name: string; Score: integer);

implementation

uses IniFiles, SysUtils;

procedure ReadScore(var Song: TSong);
var
  F:    TIniFile;
  S:    string;
  P:    integer;
  Lev:  integer;
  LevS: string;
begin
  F := TIniFile.Create(Song.Path + ChangeFileExt(Song.FileName, '.sco'));

  for Lev := 0 to 2 do begin
    case Lev of
      0:  LevS := 'Easy';
      1:  LevS := 'Normal';
      2:  LevS := 'Hard';
    end;

    P := 1;
    S := F.ReadString(LevS + IntToStr(P), 'Name', '');
    while (S <> '') and (P<=5) do begin
      SetLength(Song.Score[Lev], P);
      Song.Score[Lev, P-1].Name := S;
      Song.Score[Lev, P-1].Score := F.ReadInteger(LevS + IntToStr(P), 'Score', 0);

      Inc(P);
      S := F.ReadString(LevS + IntToStr(P), 'Name', '');
    end;
  end;
end;

procedure AddScore(var Song: TSong; Level: integer; Name: string; Score: integer);
var
  S:    integer;
  S2:   integer;
begin
  S := 0;
  while (S <= High(Song.Score[Level])) and (Score <= Song.Score[Level, S].Score) do
    Inc(S);
  // S has the number for new score


  // we create new score
  SetLength(Song.Score[Level], Length(Song.Score[Level]) + 1);

  // we move down old scores
  for S2 := High(Song.Score[Level])-1 downto S do
    Song.Score[Level, S2+1] := Song.Score[Level, S2];

  // we fill new score
  Song.Score[Level, S].Name := Name;
  Song.Score[Level, S].Score := Score;

  if Length(Song.Score[Level]) > 5 then begin
    SetLength(Song.Score[Level], 5);
  end;
end;

procedure WriteScore(var Song: TSong);
var
  F:    TIniFile;
  S:    integer;
  Lev:      integer;
  LevS:     string;
  FileName: string;
begin
  FileName := Song.Path + ChangeFileExt(Song.FileName, '.sco');
  if (not FileExists(FileName)) or (FileExists(FileName) and  DeleteFile(FileName)) then begin
    // file has been deleted -> creating new file
    F := TIniFile.Create(FileName);

    for Lev := 0 to 2 do begin
      case Lev of
        0:  LevS := 'Easy';
        1:  LevS := 'Normal';
        2:  LevS := 'Hard';
      end;

      for S := 0 to high(Song.Score[Lev]) do begin
        F.WriteString(LevS + IntToStr(S+1), 'Name', Song.Score[Lev, S].Name);
        F.WriteInteger(LevS + IntToStr(S+1), 'Score', Song.Score[Lev, S].Score);

      end; // for S
    end; // for Lev
    F.Free;
  end; // if
end;

end.
