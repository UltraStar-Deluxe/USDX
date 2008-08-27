program ScoreConverter;

uses
  Forms,
  Umainform in 'Umainform.pas' {mainform},
  UScores in 'UScores.pas',
  UDataBase in '..\Game\Code\Classes\UDataBase.pas',
  USongs in 'USongs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Score Converter';
  Application.CreateForm(Tmainform, mainform);
  Application.Run;
end.
