unit UScreenEditHeader;

interface

uses UMenu, SDL, USongs, UThemes;

type
  TScreenEditHeader = class(TMenu)
    public
      AktSong:        TSong;
      TextTitle:      integer;
      TextArtist:     integer;
      TextMp3:        integer;
      TextBackground: integer;
      TextVideo:      integer;
      TextVideoGAP:   integer;
      TextRelative:   integer;
      TextResolution: integer;
      TextNotesGAP:   integer;
      TextStart:      integer;
      TextGAP:        integer;
      TextBPM:        integer;
      StaticTitle:    integer;
      StaticArtist:     integer;
      StaticMp3:        integer;
      StaticBackground: integer;
      StaticVideo:      integer;
      StaticVideoGAP:   integer;
      StaticRelative:   integer;
      StaticResolution: integer;
      StaticNotesGAP:   integer;
      StaticStart:      integer;
      StaticGAP:        integer;
      StaticBPM:        integer;
      Sel:            array[0..11] of boolean;
      procedure SetRoundButtons;

      constructor Create(Back: String); override;
      procedure onShow; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
{      function Draw: boolean; override;
      procedure Finish;}
  end;

implementation

uses UGraphic, UMusic, SysUtils, UPliki, USkins;

function TScreenEditHeader.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  T:    integer;
begin
  Result := true;
  If (PressedDown) Then begin // Key Down
    case PressedKey of
      SDLK_ESCAPE :
        begin
//          Music.PlayBack;
//          FadeTo(@MainScreen);
          Result := false;
        end;

      SDLK_RETURN:
        begin
          if Interaction = 1 then begin
//            Save;
          end;
        end;

      SDLK_RIGHT:
        begin
          case Interaction of
            0..0: InteractNext;
            1:  Interaction := 0;
          end;
        end;

      SDLK_LEFT:
        begin
          case Interaction of
            0:  Interaction := 1;
            1..1: InteractPrev;
          end;
        end;

      SDLK_DOWN:
        begin
          case Interaction of
            0..1:   Interaction := 2;
            2..12:  InteractNext;
            13:     Interaction := 0;
          end;
        end;

      SDLK_UP:
        begin
          case Interaction of
            0..1:   Interaction := 13;
            2:      Interaction := 0;
            3..13:  InteractPrev;
          end;
        end;

      SDLK_BACKSPACE:
        begin
          T := Interaction - 2 + TextTitle;
          if (Interaction >= 2) and (Interaction <= 13) and (Length(Text[T].Text) >= 1) then begin
            Delete(Text[T].Text, Length(Text[T].Text), 1);
            SetRoundButtons;            
          end;
        end;

    end;
    case ScanCode of
      32..255:
        begin
          if (Interaction >= 2) and (Interaction <= 13) then begin
            Text[Interaction - 2 + TextTitle].Text :=
              Text[Interaction - 2 + TextTitle].Text + chr(ScanCode);
            SetRoundButtons;
          end;
        end;
    end;
  end;
end;

constructor TScreenEditHeader.Create(Back: String);
begin
  inherited Create(Back);

  AddButton(40, 20, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(15, 5, 'Open');

  AddButton(160, 20, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(20, 5, 'Save');

  AddBox(80, 60, 640, 550);

  AddText(160, 110 + 0*30, 0, 10, 0, 0, 0, 'Title:');
  AddText(160, 110 + 1*30, 0, 10, 0, 0, 0, 'Artist:');
  AddText(160, 110 + 2*30, 0, 10, 0, 0, 0, 'MP3:');

  AddText(160, 110 + 4*30, 0, 10, 0, 0, 0, 'Background:');
  AddText(160, 110 + 5*30, 0, 10, 0, 0, 0, 'Video:');
  AddText(160, 110 + 6*30, 0, 10, 0, 0, 0, 'VideoGAP:');

  AddText(160, 110 + 8*30,  0, 10, 0, 0, 0, 'Relative:');
  AddText(160, 110 + 9*30,  0, 10, 0, 0, 0, 'Resolution:');
  AddText(160, 110 + 10*30, 0, 10, 0, 0, 0, 'NotesGAP:');

  AddText(160, 110 + 12*30, 0, 10, 0, 0, 0, 'Start:');
  AddText(160, 110 + 13*30, 0, 10, 0, 0, 0, 'GAP:');
  AddText(160, 110 + 14*30, 0, 10, 0, 0, 0, 'BPM:');

  TextTitle :=  AddText(340, 110 + 0*30, 0, 10, 0, 0, 0, '');
  TextArtist := AddText(340, 110 + 1*30, 0, 10, 0, 0, 0, '');
  TextMp3 :=    AddText(340, 110 + 2*30, 0, 10, 0, 0, 0, '');

  TextBackground := AddText(340, 110 + 4*30, 0, 10, 0, 0, 0, '');
  TextVideo :=      AddText(340, 110 + 5*30, 0, 10, 0, 0, 0, '');
  TextVideoGAP :=   AddText(340, 110 + 6*30, 0, 10, 0, 0, 0, '');

  TextRelative :=   AddText(340, 110 + 8*30, 0, 10, 0, 0, 0, '');
  TextResolution := AddText(340, 110 + 9*30, 0, 10, 0, 0, 0, '');
  TextNotesGAP :=   AddText(340, 110 + 10*30, 0, 10, 0, 0, 0, '');

  TextStart :=  AddText(340, 110 + 12*30, 0, 10, 0, 0, 0, '');
  TextGAP :=    AddText(340, 110 + 13*30, 0, 10, 0, 0, 0, '');
  TextBPM :=    AddText(340, 110 + 14*30, 0, 10, 0, 0, 0, '');

  StaticTitle :=  AddStatic(130, 115 + 0*30, 20, 20, 1, 1, 1, 'RoundButton', 'BMP', 'Transparent', $FF00FF);
  StaticArtist := AddStatic(130, 115 + 1*30, 20, 20, 1, 1, 1, 'RoundButton', 'BMP', 'Transparent', $FF00FF);
  StaticMp3 :=    AddStatic(130, 115 + 2*30, 20, 20, 1, 1, 1, 'RoundButton', 'BMP', 'Transparent', $FF00FF);
  StaticBackground := AddStatic(130, 115 + 4*30, 20, 20, 1, 1, 1, 'RoundButton', 'BMP', 'Transparent', $FF00FF);
  StaticVideo :=      AddStatic(130, 115 + 5*30, 20, 20, 1, 1, 1, 'RoundButton', 'BMP', 'Transparent', $FF00FF);
  StaticVideoGAP :=   AddStatic(130, 115 + 6*30, 20, 20, 1, 1, 1, 'RoundButton', 'BMP', 'Transparent', $FF00FF);
  StaticRelative :=   AddStatic(130, 115 + 8*30, 20, 20, 1, 1, 1, 'RoundButton', 'BMP', 'Transparent', $FF00FF);
  StaticResolution := AddStatic(130, 115 + 9*30, 20, 20, 1, 1, 1, 'RoundButton', 'BMP', 'Transparent', $FF00FF);
  StaticNotesGAP :=   AddStatic(130, 115 + 10*30, 20, 20, 1, 1, 1, 'RoundButton', 'BMP', 'Transparent', $FF00FF);
  StaticStart :=  AddStatic(130, 115 + 12*30, 20, 20, 1, 1, 1, 'RoundButton', 'BMP', 'Transparent', $FF00FF);
  StaticGAP :=    AddStatic(130, 115 + 13*30, 20, 20, 1, 1, 1, 'RoundButton', 'BMP', 'Transparent', $FF00FF);
  StaticBPM :=    AddStatic(130, 115 + 14*30, 20, 20, 1, 1, 1, 'RoundButton', 'BMP', 'Transparent', $FF00FF);

  AddInteraction(iText, TextTitle);
  AddInteraction(iText, TextArtist);
  AddInteraction(iText, TextMp3);
  AddInteraction(iText, TextBackground);
  AddInteraction(iText, TextVideo);
  AddInteraction(iText, TextVideoGAP);
  AddInteraction(iText, TextRelative);
  AddInteraction(iText, TextResolution);
  AddInteraction(iText, TextNotesGAP);
  AddInteraction(iText, TextStart);
  AddInteraction(iText, TextGAP);
  AddInteraction(iText, TextBPM);
end;

procedure TScreenEditHeader.onShow;
begin
{  if FileExists(FileName) then begin // load file
    AktSong.FileName := FileName;
    SkanujPlik(AktSong);

    SetLength(TrueBoolStrs, 1);
    TrueBoolStrs[0] := 'yes';
    SetLength(FalseBoolStrs, 1);
    FalseBoolStrs[0] := 'no';

    Text[TextTitle].Text :=   AktSong.Title;
    Text[TextArtist].Text :=  AktSong.Artist;
    Text[TextMP3].Text :=     AktSong.Mp3;
    Text[TextBackground].Text :=  AktSong.Background;
    Text[TextVideo].Text :=       AktSong.Video;
    Text[TextVideoGAP].Text :=    FloatToStr(AktSong.VideoGAP);
    Text[TextRelative].Text :=    BoolToStr(AktSong.Relative, true);
    Text[TextResolution].Text :=  IntToStr(AktSong.Resolution);
    Text[TextNotesGAP].Text :=    IntToStr(AktSong.NotesGAP);
    Text[TextStart].Text := FloatToStr(AktSong.Start);
    Text[TextGAP].Text :=   FloatToStr(AktSong.GAP);
    Text[TextBPM].Text :=   FloatToStr(AktSong.BPM[0].BPM);
    SetRoundButtons;
  end;}

  Interaction := 0;
end;

(*function TScreenEdit.Draw: boolean;
var
  Min:    integer;
  Sec:    integer;
  Tekst:  string;
  Pet:    integer;
  AktBeat:  integer;
begin
{  glClearColor(1,1,1,1);

  // control music
  if PlaySentence then begin
    // stop the music
    if (Music.Position > PlayStopTime) then begin
      Music.Stop;
      PlaySentence := false;
    end;

    // click
    if (Click) and (PlaySentence) then begin
      AktBeat := Floor(AktSong.BPM[0].BPM * (Music.Position - AktSong.GAP / 1000) / 60);
      Text[TextDebug].Text := IntToStr(AktBeat);
      if AktBeat <> LastClick then begin
        for Pet := 0 to Czesci[0].Czesc[Czesci[0].Akt].HighNut do
          if (Czesci[0].Czesc[Czesci[0].Akt].Nuta[Pet].Start = AktBeat) then begin
            Music.PlayClick;
            LastClick := AktBeat;
          end;
      end;
    end; // click
  end; // if PlaySentence

  Text[TextSentence].Text := IntToStr(Czesci[0].Akt + 1) + ' / ' + IntToStr(Czesci[0].Ilosc);
  Text[TextNote].Text := IntToStr(AktNuta + 1) + ' / ' + IntToStr(Czesci[0].Czesc[Czesci[0].Akt].IlNut);

  // Song info
  Text[TextBPM].Text := FloatToStr(AktSong.BPM[0].BPM / 4);
  Text[TextGAP].Text := FloatToStr(AktSong.GAP);

  // Note info
  Text[TextNStart].Text :=    IntToStr(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);
  Text[TextNDlugosc].Text :=  IntToStr(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
  Text[TextNTon].Text :=      IntToStr(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Ton);
  Text[TextNText].Text :=              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Tekst;

  // draw static menu
  inherited Draw;

  // draw notes
  SingDrawNoteLines(20, 300, 780, 15);
  SingDrawBeatDelimeters(40, 300, 760, 0);
  SingDrawCzesc(40, 405, 760, 0);

  // draw text
  Lyric.Draw;}

end;*)

procedure TScreenEditHeader.SetRoundButtons;
begin
  if Length(Text[TextTitle].Text) > 0 then Static[StaticTitle].Visible := true
  else Static[StaticTitle].Visible := false;

  if Length(Text[TextArtist].Text) > 0 then Static[StaticArtist].Visible := true
  else Static[StaticArtist].Visible := false;

  if Length(Text[TextMp3].Text) > 0 then Static[StaticMp3].Visible := true
  else Static[StaticMp3].Visible := false;

  if Length(Text[TextBackground].Text) > 0 then Static[StaticBackground].Visible := true
  else Static[StaticBackground].Visible := false;

  if Length(Text[TextVideo].Text) > 0 then Static[StaticVideo].Visible := true
  else Static[StaticVideo].Visible := false;

  try
    StrToFloat(Text[TextVideoGAP].Text);
    if StrToFloat(Text[TextVideoGAP].Text)<> 0 then Static[StaticVideoGAP].Visible := true
    else Static[StaticVideoGAP].Visible := false;
  except
    Static[StaticVideoGAP].Visible := false;
  end;

  if LowerCase(Text[TextRelative].Text) = 'yes' then Static[StaticRelative].Visible := true
  else Static[StaticRelative].Visible := false;

  try
    StrToInt(Text[TextResolution].Text);
    if (StrToInt(Text[TextResolution].Text) <> 0) and (StrToInt(Text[TextResolution].Text) >= 1)
    then Static[StaticResolution].Visible := true
    else Static[StaticResolution].Visible := false;
  except
    Static[StaticResolution].Visible := false;
  end;

  try
    StrToInt(Text[TextNotesGAP].Text);
    Static[StaticNotesGAP].Visible := true;
  except
    Static[StaticNotesGAP].Visible := false;
  end;

  // start
  try
    StrToFloat(Text[TextStart].Text);
    if (StrToFloat(Text[TextStart].Text) > 0) then Static[StaticStart].Visible := true
    else Static[StaticStart].Visible := false;
  except
    Static[StaticStart].Visible := false;
  end;

  // GAP
  try
    StrToFloat(Text[TextGAP].Text);
    Static[StaticGAP].Visible := true;
  except
    Static[StaticGAP].Visible := false;
  end;

  // BPM
  try
    StrToFloat(Text[TextBPM].Text);
    if (StrToFloat(Text[TextBPM].Text) > 0) then Static[StaticBPM].Visible := true
    else Static[StaticBPM].Visible := false;
  except
    Static[StaticBPM].Visible := false;
  end;

end;

(*procedure TScreenEdit.Finish;
begin
//
end;*)

end.
