unit Umainform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, UDataBase, ShellAPI, ShlObj, USongs;

type
  Tmainform = class(TForm)
    Label1: TLabel;
    lFolder: TLabel;
    bFLoad: TButton;
    Label2: TLabel;
    lDatabase: TLabel;
    bDLoad: TButton;
    lDatabase2: TLabel;
    lFolder2: TLabel;
    bToDB: TButton;
    bFromDB: TButton;
    pProgress: TProgressBar;
    oDatabase: TOpenDialog;
    lStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure bDLoadClick(Sender: TObject);
    function BrowseDialog (const Title: string; const Flag: integer): string;
    procedure bFLoadClick(Sender: TObject);
    procedure UpdateLoadedSongs(Path: String; Count: integer);
    procedure bToDBClick(Sender: TObject);
    procedure bFromDBClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  mainform: Tmainform;
  DBLoaded: Boolean;
  SFLoaded: Boolean;


implementation

uses UScores;

{$R *.dfm}

function Tmainform.BrowseDialog
 (const Title: string; const Flag: integer): string;
var
  lpItemID : PItemIDList;
  BrowseInfo : TBrowseInfo;
  DisplayName : array[0..MAX_PATH] of char;
  TempPath : array[0..MAX_PATH] of char;
begin
  Result:='';
  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  with BrowseInfo do begin
    hwndOwner := Application.Handle;
    pszDisplayName := @DisplayName;
    lpszTitle := PChar(Title);
    ulFlags := Flag;
  end;
  lpItemID := SHBrowseForFolder(BrowseInfo);
  if lpItemId <> nil then begin
    SHGetPathFromIDList(lpItemID, TempPath);
    Result := TempPath;
    GlobalFreePtr(lpItemID);
  end;
end;

procedure Tmainform.FormCreate(Sender: TObject);
begin
  Database := TDataBaseSystem.Create;
  Songs := TSongs.Create;
  lStatus.Caption := 'Welcome to USD Score Converter';
  lFolder2.Caption := 'No Songs loaded';
  lFolder.Caption := '';
  lDataBase2.Caption := 'No Database loaded';
  lDataBase.Caption := '';
end;

procedure Tmainform.bDLoadClick(Sender: TObject);
begin
  if oDatabase.Execute then
  begin
    try
      Database.Init(oDataBase.FileName);
      lDataBase2.Caption := 'Database loaded';
      lDataBase.Caption := oDataBase.FileName;
      DBLoaded := True;
    except
      lDataBase2.Caption := 'No Database loaded';
      lDataBase.Caption := '';
      DBLoaded := False;
    end;
  end;
  bToDB.Enabled := DBLoaded and SFLoaded;
  bFromDB.Enabled := bToDB.Enabled;
end;

procedure Tmainform.bFLoadClick(Sender: TObject);
var
  Path: String;
begin
  Path := BrowseDialog('Select UltraStar SongFolder', BIF_RETURNONLYFSDIRS);

  if Path <> '' then
  begin
    SetLength(Songs.Song, 0);
    try
      Songs.BrowseDir(Path + '\');
      lFolder2.Caption := Inttostr(Length(Songs.Song)) + ' Songs loaded';
      lFolder.Caption := Path;
      SFLoaded := True;
    except
      lFolder2.Caption := 'No Songs loaded';
      lFolder.Caption := '';
      SFLoaded := False;
    end;
  end;

  bToDB.Enabled := DBLoaded and SFLoaded;
  bFromDB.Enabled := bToDB.Enabled;
end;

procedure Tmainform.UpdateLoadedSongs(Path: String; Count: integer);
begin
  lFolder2.Caption := Inttostr(Count) + ' Songs loaded';
  lFolder.Caption := Path;
  Application.ProcessMessages;
end;

procedure Tmainform.bToDBClick(Sender: TObject);
var
  I, J, K: Integer;
  LastI: integer;
begin
  if (Messagebox(0, PChar('If the same directory is added more than one time the Score-File will be useless. Contìnue ?'), PChar(Mainform.Caption), MB_ICONWARNING or MB_YESNO) = IDYes) then
  begin
    pProgress.Max := high(Songs.Song);
    pProgress.Position := 0;
    // Go through all Songs
    For I := 0 to high(Songs.Song) do
    begin
      try
        //Read Scores from .SCO File
        ReadScore (Songs.Song[I]);

        //Go from Easy to Difficult
        For J := 0 to 2 do
        begin
          //Go through all Score Entrys with Difficulty J
          For K := 0 to high(Songs.Song[I].Score[J]) do
          begin
            //Add to DataBase
            DataBase.AddScore(Songs.Song[I], J, Songs.Song[I].Score[J][K].Name, Songs.Song[I].Score[J][K].Score);
          end;
        end;

      except
        showmessage ('Error Converting Score From Song: ' + Songs.Song[I].Path  + Songs.Song[I].FileName);
      end;

      //Update ProgressBar
      J := I div 30;
      if (LastI <> J) then
      begin
        LastI := J;
        pProgress.Position := I;
        lStatus.Caption := 'Adding Songscore: ' + Songs.Song[I].Artist + ' - ' + Songs.Song[I].Title;
        Application.ProcessMessages;
      end;
    end;

    pProgress.Position := pProgress.Max;
    lStatus.Caption := 'Finished';
  end;
end;

procedure Tmainform.bFromDBClick(Sender: TObject);
var
  I, J: Integer;
  LastI: integer;
  anyScoreinthere: boolean;
begin
  if (Messagebox(0, PChar('All Score Entrys in the Song Directory having an equivalent will be Overwritten. Contìnue ?'), PChar(Mainform.Caption), MB_ICONWARNING or MB_YESNO) = IDYes) then
  begin
    pProgress.Max := high(Songs.Song);
    pProgress.Position := 0;
    // Go through all Songs
    For I := 0 to high(Songs.Song) do
    begin
      try
        //Not Write ScoreFile when there are no Scores for this File
        anyScoreinthere := false;
        //Read Scores from DB File
        Database.ReadScore (Songs.Song[I]);

        //Go from Easy to Difficult
        For J := 0 to 2 do
        begin
          anyScoreinthere := anyScoreinthere or (Length(Songs.Song[I].Score[J]) > 0);
        end;

        if AnyScoreinThere then
          WriteScore(Songs.Song[I]);

      except
        showmessage ('Error Converting Score From Song: ' + Songs.Song[I].Path  + Songs.Song[I].FileName);
      end;

      //Update ProgressBar
      J := I div 30;
      if (LastI <> J) then
      begin
        LastI := J;
        pProgress.Position := I;
        lStatus.Caption := 'Writing ScoreFile: ' + Songs.Song[I].Artist + ' - ' + Songs.Song[I].Title;
        Application.ProcessMessages;
      end;
    end;
    
    pProgress.Position := pProgress.Max;
    lStatus.Caption := 'Finished';
  end;
end;

end.
