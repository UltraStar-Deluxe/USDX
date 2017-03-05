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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenEditHeader.pas $
 * $Id: UScreenEditHeader.pas 2246 2010-04-18 13:43:36Z tobigun $
 *}

unit UScreenEditHeader;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  SDL,
  USongs,
  USong,
  UPath,
  UThemes;

type
  TScreenEditHeader = class(TMenu)
    public
      CurrentSong:      TSong;
      TextTitle:        integer;
      TextArtist:       integer;
      TextMp3:          integer;
      TextBackground:   integer;
      TextVideo:        integer;
      TextVideoGAP:     integer;
      TextRelative:     integer;
      TextResolution:   integer;
      TextNotesGAP:     integer;
      TextStart:        integer;
      TextGAP:          integer;
      TextBPM:          integer;
      StaticTitle:      integer;
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
      Sel:              array[0..11] of boolean;
      procedure SetRoundButtons;

      constructor Create; override;
      procedure OnShow; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
{      function Draw: boolean; override;
      procedure Finish;}
  end;

implementation

uses
  UGraphic,
  UMusic,
  SysUtils,
  UFiles,
  USkins,
  UTexture,
  UUnicodeUtils;

function TScreenEditHeader.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
var
  T:    integer;
begin
  Result := true;
  if (PressedDown) then    // Key Down
   begin   // check normal keys
    case UCS4UpperCase(CharCode) of
      Ord('Q'):
        begin
          Result := false;
          Exit;
        end;
    end;
    
    // check special keys
    case PressedKey of
      SDLK_ESCAPE:
        begin
          //Music.PlayBack;
          //FadeTo(@MainScreen);
          Result := false;
        end;

      SDLK_RETURN:
        begin
          if Interaction = 1 then
          begin
            //Save;
          end;
        end;

      SDLK_RIGHT:
        begin
          case Interaction of
            0..0: InteractNext;
            1:    Interaction := 0;
          end;
        end;

      SDLK_LEFT:
        begin
          case Interaction of
            0:    Interaction := 1;
            1..1: InteractPrev;
          end;
        end;

      SDLK_DOWN:
        begin
          case Interaction of
            0..1:  Interaction := 2;
            2..12: InteractNext;
            13:    Interaction := 0;
          end;
        end;

      SDLK_UP:
        begin
          case Interaction of
            0..1:  Interaction := 13;
            2:     Interaction := 0;
            3..13: InteractPrev;
          end;
        end;

      SDLK_BACKSPACE:
        begin
          T := Interaction - 2 + TextTitle;
          if (Interaction >= 2) and (Interaction <= 13) and (Length(Text[T].Text) >= 1) then
          begin
            Text[T].DeleteLastLetter;
            SetRoundButtons;            
          end;
        end;

    end;
    case CharCode of
      32..255:
        begin
          if (Interaction >= 2) and (Interaction <= 13) then
          begin
            Text[Interaction - 2 + TextTitle].Text :=
              Text[Interaction - 2 + TextTitle].Text + UCS4ToUTF8String(CharCode);
            SetRoundButtons;
          end;
        end;
    end;
  end;
end;

constructor TScreenEditHeader.Create;
begin
  inherited Create;

  AddButton(40, 20, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(15, 5, 'Open');

  AddButton(160, 20, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(20, 5, 'Save');

  AddBox(80, 60, 640, 550);

  AddText(160, 110 + 0*30, 0, 30, 0, 0, 0, 'Title:');
  AddText(160, 110 + 1*30, 0, 30, 0, 0, 0, 'Artist:');
  AddText(160, 110 + 2*30, 0, 30, 0, 0, 0, 'MP3:');

  AddText(160, 110 + 4*30, 0, 30, 0, 0, 0, 'Background:');
  AddText(160, 110 + 5*30, 0, 30, 0, 0, 0, 'Video:');
  AddText(160, 110 + 6*30, 0, 30, 0, 0, 0, 'VideoGAP:');

  AddText(160, 110 + 8*30,  0, 30, 0, 0, 0, 'Relative:');
  AddText(160, 110 + 9*30,  0, 30, 0, 0, 0, 'Resolution:');
  AddText(160, 110 + 10*30, 0, 30, 0, 0, 0, 'NotesGAP:');

  AddText(160, 110 + 12*30, 0, 30, 0, 0, 0, 'Start:');
  AddText(160, 110 + 13*30, 0, 30, 0, 0, 0, 'GAP:');
  AddText(160, 110 + 14*30, 0, 30, 0, 0, 0, 'BPM:');

  TextTitle  := AddText(340, 110 + 0*30, 0, 30, 0, 0, 0, '');
  TextArtist := AddText(340, 110 + 1*30, 0, 30, 0, 0, 0, '');
  TextMp3    := AddText(340, 110 + 2*30, 0, 30, 0, 0, 0, '');

  TextBackground := AddText(340, 110 + 4*30, 0, 30, 0, 0, 0, '');
  TextVideo      := AddText(340, 110 + 5*30, 0, 30, 0, 0, 0, '');
  TextVideoGAP   := AddText(340, 110 + 6*30, 0, 30, 0, 0, 0, '');

  TextRelative   := AddText(340, 110 + 8*30, 0, 30, 0, 0, 0, '');
  TextResolution := AddText(340, 110 + 9*30, 0, 30, 0, 0, 0, '');
  TextNotesGAP   := AddText(340, 110 + 10*30, 0, 30, 0, 0, 0, '');

  TextStart :=  AddText(340, 110 + 12*30, 0, 30, 0, 0, 0, '');
  TextGAP   :=  AddText(340, 110 + 13*30, 0, 30, 0, 0, 0, '');
  TextBPM   :=  AddText(340, 110 + 14*30, 0, 30, 0, 0, 0, '');

  StaticTitle      := AddStatic(130, 115 + 0*30, 20, 20, 1, 1, 1, Path('RoundButton'), TEXTURE_TYPE_TRANSPARENT, $FF00FF);
  StaticArtist     := AddStatic(130, 115 + 1*30, 20, 20, 1, 1, 1, Path('RoundButton'), TEXTURE_TYPE_TRANSPARENT, $FF00FF);
  StaticMp3        := AddStatic(130, 115 + 2*30, 20, 20, 1, 1, 1, Path('RoundButton'), TEXTURE_TYPE_TRANSPARENT, $FF00FF);
  StaticBackground := AddStatic(130, 115 + 4*30, 20, 20, 1, 1, 1, Path('RoundButton'), TEXTURE_TYPE_TRANSPARENT, $FF00FF);
  StaticVideo      := AddStatic(130, 115 + 5*30, 20, 20, 1, 1, 1, Path('RoundButton'), TEXTURE_TYPE_TRANSPARENT, $FF00FF);
  StaticVideoGAP   := AddStatic(130, 115 + 6*30, 20, 20, 1, 1, 1, Path('RoundButton'), TEXTURE_TYPE_TRANSPARENT, $FF00FF);
  StaticRelative   := AddStatic(130, 115 + 8*30, 20, 20, 1, 1, 1, Path('RoundButton'), TEXTURE_TYPE_TRANSPARENT, $FF00FF);
  StaticResolution := AddStatic(130, 115 + 9*30, 20, 20, 1, 1, 1, Path('RoundButton'), TEXTURE_TYPE_TRANSPARENT, $FF00FF);
  StaticNotesGAP   := AddStatic(130, 115 + 10*30, 20, 20, 1, 1, 1, Path('RoundButton'), TEXTURE_TYPE_TRANSPARENT, $FF00FF);
  StaticStart      := AddStatic(130, 115 + 12*30, 20, 20, 1, 1, 1, Path('RoundButton'), TEXTURE_TYPE_TRANSPARENT, $FF00FF);
  StaticGAP        := AddStatic(130, 115 + 13*30, 20, 20, 1, 1, 1, Path('RoundButton'), TEXTURE_TYPE_TRANSPARENT, $FF00FF);
  StaticBPM        := AddStatic(130, 115 + 14*30, 20, 20, 1, 1, 1, Path('RoundButton'), TEXTURE_TYPE_TRANSPARENT, $FF00FF);

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

procedure TScreenEditHeader.OnShow;
begin
  inherited;

{  if FileExists(FileName) then  // load file
  begin
    CurrentSong.FileName := FileName;
    SkanujPlik(CurrentSong);

    SetLength(TrueBoolStrs, 1);
    TrueBoolStrs[0] := 'yes';
    SetLength(FalseBoolStrs, 1);
    FalseBoolStrs[0] := 'no';

    Text[TextTitle].Text :=   CurrentSong.Title;
    Text[TextArtist].Text :=  CurrentSong.Artist;
    Text[TextMP3].Text :=     CurrentSong.Mp3;
    Text[TextBackground].Text :=  CurrentSong.Background;
    Text[TextVideo].Text :=       CurrentSong.Video;
    Text[TextVideoGAP].Text :=    FloatToStr(CurrentSong.VideoGAP);
    Text[TextRelative].Text :=    BoolToStr(CurrentSong.Relative, true);
    Text[TextResolution].Text :=  IntToStr(CurrentSong.Resolution);
    Text[TextNotesGAP].Text :=    IntToStr(CurrentSong.NotesGAP);
    Text[TextStart].Text := FloatToStr(CurrentSong.Start);
    Text[TextGAP].Text :=   FloatToStr(CurrentSong.GAP);
    Text[TextBPM].Text :=   FloatToStr(CurrentSong.BPM[0].BPM);
    SetRoundButtons;
  end;}

  Interaction := 0;
end;

(*function TScreenEdit.Draw: boolean;
var
  Min:     integer;
  Sec:     integer;
  Count:   integer;
  AktBeat: integer;
begin
{  glClearColor(1,1,1,1);

  // control music
  if PlaySentence then
  begin
    // stop the music
    if (Music.Position > PlayStopTime) then
    begin
      Music.Stop;
      PlaySentence := false;
    end;

    // click
    if (Click) and (PlaySentence) then
    begin
      AktBeat := Floor(CurrentSong.BPM[0].BPM * (Music.Position - CurrentSong.GAP / 1000) / 60);
      Text[TextDebug].Text := IntToStr(AktBeat);
      if AktBeat <> LastClick then
      begin
        for Count := 0 to Czesci[0].Czesc[Czesci[0].Akt].HighNut do
          if (Czesci[0].Czesc[Czesci[0].Akt].Nuta[Count].Start = AktBeat) then
          begin
            Music.PlayClick;
            LastClick := AktBeat;
          end;
      end;
    end; // click
  end; // if PlaySentence

  Text[TextSentence].Text := IntToStr(Czesci[0].Akt + 1) + ' / ' + IntToStr(Czesci[0].Ilosc);
  Text[TextNote].Text := IntToStr(AktNuta + 1) + ' / ' + IntToStr(Czesci[0].Czesc[Czesci[0].Akt].LengthNote);

  // Song info
  Text[TextBPM].Text := FloatToStr(CurrentSong.BPM[0].BPM / 4);
  Text[TextGAP].Text := FloatToStr(CurrentSong.GAP);

  // Note info
  Text[TextNStart].Text :=    IntToStr(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Start);
  Text[TextNDlugosc].Text :=  IntToStr(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Dlugosc);
  Text[TextNTon].Text :=      IntToStr(Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Ton);
  Text[TextNText].Text :=              Czesci[0].Czesc[Czesci[0].Akt].Nuta[AktNuta].Text;

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
  if Length(Text[TextTitle].Text) > 0 then
    Statics[StaticTitle].Visible := true
  else
    Statics[StaticTitle].Visible := false;

  if Length(Text[TextArtist].Text) > 0 then
    Statics[StaticArtist].Visible := true
  else
    Statics[StaticArtist].Visible := false;

  if Length(Text[TextMp3].Text) > 0 then
    Statics[StaticMp3].Visible := true
  else 
    Statics[StaticMp3].Visible := false;

  if Length(Text[TextBackground].Text) > 0 then
    Statics[StaticBackground].Visible := true
  else
    Statics[StaticBackground].Visible := false;

  if Length(Text[TextVideo].Text) > 0 then
    Statics[StaticVideo].Visible := true
  else
    Statics[StaticVideo].Visible := false;

  try
    StrToFloat(Text[TextVideoGAP].Text);
    if StrToFloat(Text[TextVideoGAP].Text)<> 0 then
      Statics[StaticVideoGAP].Visible := true
    else
      Statics[StaticVideoGAP].Visible := false;
  except
    Statics[StaticVideoGAP].Visible := false;
  end;

  if LowerCase(Text[TextRelative].Text) = 'yes' then
    Statics[StaticRelative].Visible := true
  else
    Statics[StaticRelative].Visible := false;

  try
    StrToInt(Text[TextResolution].Text);
    if (StrToInt(Text[TextResolution].Text) <> 0) and (StrToInt(Text[TextResolution].Text) >= 1) then
      Statics[StaticResolution].Visible := true
    else
      Statics[StaticResolution].Visible := false;
  except
    Statics[StaticResolution].Visible := false;
  end;

  try
    StrToInt(Text[TextNotesGAP].Text);
    Statics[StaticNotesGAP].Visible := true;
  except
    Statics[StaticNotesGAP].Visible := false;
  end;

  // start
  try
    StrToFloat(Text[TextStart].Text);
    if (StrToFloat(Text[TextStart].Text) > 0) then
      Statics[StaticStart].Visible := true
    else
      Statics[StaticStart].Visible := false;
  except
    Statics[StaticStart].Visible := false;
  end;

  // GAP
  try
    StrToFloat(Text[TextGAP].Text);
    Statics[StaticGAP].Visible := true;
  except
    Statics[StaticGAP].Visible := false;
  end;

  // BPM
  try
    StrToFloat(Text[TextBPM].Text);
    if (StrToFloat(Text[TextBPM].Text) > 0) then
      Statics[StaticBPM].Visible := true
    else
      Statics[StaticBPM].Visible := false;
  except
    Statics[StaticBPM].Visible := false;
  end;

end;

(*procedure TScreenEdit.Finish;
begin
//
end;*)

end.
