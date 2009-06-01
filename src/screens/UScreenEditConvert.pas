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

unit UScreenEditConvert;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  math,
  UMenu,
  SDL,
  {$IFDEF UseMIDIPort}
  MidiFile,
  MidiOut,
  {$ENDIF}
  ULog,
  USongs,
  USong,
  UMusic,
  UThemes;

type
  TNote = record
    Event:     integer;
    EventType: integer;
    Channel:   integer;
    Start:     real;
    Len:       real;
    Data1:     integer;
    Data2:     integer;
    Str:       string;
  end;

  TTrack = record
    Note:   array of TNote;
    Name:   string;
    Hear:   boolean;
    Status: set of (notes, lyrics);
  end;

  TNuta = record
    Start:    integer;
    Len:      integer;
    Tone:     integer;
    Lyric:    string;
    NewSentence:  boolean;
  end;

  TArrayTrack = array of TTrack;

  TScreenEditConvert = class(TMenu)
    public
      ATrack:    TArrayTrack; // actual track
//      Track:     TArrayTrack;
      Channel:   TArrayTrack;
      ColR:      array[0..100] of real;
      ColG:      array[0..100] of real;
      ColB:      array[0..100] of real;
      Len:       real;
      Sel:       integer;
      Selected:  boolean;
//      FileName:  string;

      {$IFDEF UseMIDIPort}
      MidiFile:  TMidiFile;
      MidiTrack: TMidiTrack;
      MidiEvent: pMidiEvent;
      MidiOut:   TMidiOutput;
      {$ENDIF}
      
      Song:      TSong;
      Lines:     TLines;
      BPM:       real;
      Ticks:     real;
      Note:      array of TNuta;

      procedure AddLyric(Start: integer; Text: string);
      procedure Extract;

      {$IFDEF UseMIDIPort}
      procedure MidiFile1MidiEvent(event: PMidiEvent);
      {$ENDIF}
      
      function SelectedNumber: integer;
      constructor Create; override;
      procedure onShow; override;
      function ParseInput(PressedKey: cardinal; CharCode: WideChar; PressedDown: boolean): boolean; override;
      function Draw: boolean; override;
      procedure onHide; override;
  end;

var
  ConversionFileName: string;

implementation

uses
  SysUtils,
  TextGL,
  gl,
  UDrawTexture,
  UFiles,
  UGraphic,
  UIni,
  UMain,
  UPath,
  USkins;

function TScreenEditConvert.ParseInput(PressedKey: cardinal; CharCode: WideChar; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case WideCharUpperCase(CharCode)[1] of
      'Q':
        begin
          Result := false;
          Exit;
        end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
      {$IFDEF UseMIDIPort}
          MidiFile.StopPlaying;
      {$ENDIF}
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenEdit);
        end;

      SDLK_RETURN:
        begin
          if Interaction = 0 then
          begin
            AudioPlayback.PlaySound(SoundLib.Start);
            ScreenOpen.BackScreen := @ScreenEditConvert;
            FadeTo(@ScreenOpen);
          end;

          if Interaction = 1 then
          begin
            Selected := false;
      {$IFDEF UseMIDIPort}
            MidiFile.OnMidiEvent := MidiFile1MidiEvent;
//            MidiFile.GoToTime(MidiFile.GetTrackLength div 2);
            MidiFile.StartPlaying;
            {$ENDIF}
          end;

          if Interaction = 2 then
          begin
            Selected := true;
            {$IFDEF UseMIDIPort}
            MidiFile.OnMidiEvent := nil;
            {$ENDIF}
            {for T := 0 to High(ATrack) do
            begin
              if ATrack[T].Hear then
              begin
                MidiTrack := MidiFile.GetTrack(T);
                MidiTrack.OnMidiEvent := MidiFile1MidiEvent;
              end;
            end;
            MidiFile.StartPlaying;//}
          end;

          if Interaction = 3 then
          begin
            if SelectedNumber > 0 then
            begin
              Extract;
              SaveSong(Song, Lines, ChangeFileExt(ConversionFileName, '.txt'), false);
            end;
          end;

        end;

      SDLK_SPACE:
        begin
//          ATrack[Sel].Hear := not ATrack[Sel].Hear;
          if Notes in ATrack[Sel].Status then
          begin
            ATrack[Sel].Status := ATrack[Sel].Status - [Notes];
            if Lyrics in ATrack[Sel].Status then
              ATrack[Sel].Status := ATrack[Sel].Status - [Lyrics]
            else
              ATrack[Sel].Status := ATrack[Sel].Status + [Lyrics];
          end
          else
            ATrack[Sel].Status := ATrack[Sel].Status + [Notes];

{          if Selected then
          begin
            MidiTrack := MidiFile.GetTrack(Sel);
            if Track[Sel].Hear then
              MidiTrack.OnMidiEvent := MidiFile1MidiEvent
            else
              MidiTrack.OnMidiEvent := nil;
          end;}
        end;

      SDLK_RIGHT:
        begin
          InteractNext;
        end;

      SDLK_LEFT:
        begin
          InteractPrev;
        end;

      SDLK_DOWN:
        begin
          Inc(Sel);
          if Sel > High(ATrack) then
            Sel := 0;
        end;
      SDLK_UP:
        begin
          Dec(Sel);
          if Sel < 0 then
            Sel := High(ATrack);
        end;
    end;
  end;
end;

procedure TScreenEditConvert.AddLyric(Start: integer; Text: string);
var
  N:    integer;
begin
  for N := 0 to High(Note) do
  begin
    if Note[N].Start = Start then
    begin
      // check for new sentece
      if Copy(Text, 1, 1) = '\' then
        Delete(Text, 1, 1);
      if Copy(Text, 1, 1) = '/' then
      begin
        Delete(Text, 1, 1);
        Note[N].NewSentence := true;
      end;

      // overwrite lyric od append
      if Note[N].Lyric = '-' then
        Note[N].Lyric := Text
      else
        Note[N].Lyric := Note[N].Lyric + Text;
    end;
  end;
end;

procedure TScreenEditConvert.Extract;
var
  T:    integer;
  C:    integer;
  N:    integer;
  Nu:   integer;
  NoteTemp: TNuta;
  Move: integer;
  Max, Min: integer;
begin
  // song info
  Song.Title := '';
  Song.Artist := '';
  Song.Mp3 := '';
  Song.Resolution := 4;
  SetLength(Song.BPM, 1);
  Song.BPM[0].BPM := BPM*4;

  SetLength(Note, 0);

  // extract notes
  for T := 0 to High(ATrack) do
  begin
//    if ATrack[T].Hear then
//    begin
    if Notes in ATrack[T].Status then
    begin
      for N := 0 to High(ATrack[T].Note) do
      begin
        if (ATrack[T].Note[N].EventType = 9) and (ATrack[T].Note[N].Data2 > 0) then
        begin
          Nu := Length(Note);
          SetLength(Note, Nu + 1);
          Note[Nu].Start := Round(ATrack[T].Note[N].Start / Ticks);
          Note[Nu].Len := Round(ATrack[T].Note[N].Len / Ticks);
          Note[Nu].Tone := ATrack[T].Note[N].Data1 - 12*5;
          Note[Nu].Lyric := '-';
        end;
      end;
    end;
  end;

  // extract lyrics
  for T := 0 to High(ATrack) do
  begin
//    if ATrack[T].Hear then
//    begin
    if Lyrics in ATrack[T].Status then
    begin
      for N := 0 to High(ATrack[T].Note) do
      begin
        if (ATrack[T].Note[N].EventType = 15) then
        begin
//          Log.LogStatus('<' + Track[T].Note[N].Str + '>', 'MIDI');
          AddLyric(Round(ATrack[T].Note[N].Start / Ticks), ATrack[T].Note[N].Str);
        end;
      end;
    end;
  end;

  // sort notes
  for N := 0 to High(Note) do
    for Nu := 0 to High(Note)-1 do
      if Note[Nu].Start > Note[Nu+1].Start then
      begin
        NoteTemp := Note[Nu];
        Note[Nu] := Note[Nu+1];
        Note[Nu+1] := NoteTemp;
      end;

  // move to 0 at beginning
  Move := Note[0].Start;
  for N := 0 to High(Note) do
    Note[N].Start := Note[N].Start - Move;

  // copy notes
  SetLength(Lines.Line, 1);
  Lines.Number := 1;
  Lines.High := 0;

  C := 0;
  N := 0;
  Lines.Line[C].HighNote := -1;

  for Nu := 0 to High(Note) do
  begin
    if Note[Nu].NewSentence then // new line
    begin
      SetLength(Lines.Line, Length(Lines.Line)+1);
      Lines.Number := Lines.Number + 1;
      Lines.High := Lines.High + 1;
      C := C + 1;
      N := 0;
      SetLength(Lines.Line[C].Note, 0);
      Lines.Line[C].HighNote := -1;

      //Calculate Start of the Last Sentence
      if (C > 0) and (Nu > 0) then
      begin
        Max := Note[Nu].Start;
        Min := Note[Nu-1].Start + Note[Nu-1].Len;
        
        case (Max - Min) of
          0:    Lines.Line[C].Start := Max;
          1:    Lines.Line[C].Start := Max;
          2:    Lines.Line[C].Start := Max - 1;
          3:    Lines.Line[C].Start := Max - 2;
          else
            if ((Max - Min) > 4) then
              Lines.Line[C].Start := Min + 2
            else
              Lines.Line[C].Start := Max;

        end; // case

      end;
    end;

    // create space for new note
    SetLength(Lines.Line[C].Note, Length(Lines.Line[C].Note)+1);

    // initialize note
    Lines.Line[C].Note[N].Start := Note[Nu].Start;
    Lines.Line[C].Note[N].Length := Note[Nu].Len;
    Lines.Line[C].Note[N].Tone := Note[Nu].Tone;
    Lines.Line[C].Note[N].Text := Note[Nu].Lyric;
    //All Notes are Freestyle when Converted Fix:
    Lines.Line[C].Note[N].NoteType := ntNormal;
    Inc(N);
  end;
end;

function TScreenEditConvert.SelectedNumber: integer;
var
  T:    integer; // track
begin
  Result := 0;
  for T := 0 to High(ATrack) do
//    if ATrack[T].Hear then
//      Inc(Result);
    if Notes in ATrack[T].Status then
      Inc(Result);
end;

{$IFDEF UseMIDIPort}
procedure TScreenEditConvert.MidiFile1MidiEvent(event: PMidiEvent);
begin
//  Log.LogStatus(IntToStr(event.event), 'MIDI');
  MidiOut.PutShort(event.event, event.data1, event.data2);
end;
{$ENDIF}

constructor TScreenEditConvert.Create;
var
  P:  integer;
begin
  inherited Create;
  AddButton(40, 20, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(15, 5, 0, 0, 0, 'Open');
//  Button[High(Button)].Text[0].Size := 11;

  AddButton(160, 20, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(25, 5, 0, 0, 0, 'Play');

  AddButton(280, 20, 200, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(25, 5, 0, 0, 0, 'Play Selected');

  AddButton(500, 20, 100, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(20, 5, 0, 0, 0, 'Save');

{  MidiOut := TMidiOutput.Create(nil);
//  MidiOut.Close;
//  MidiOut.DeviceID := 0;
  if Ini.Debug = 1 then
    MidiOut.ProductName := 'Microsoft GS Wavetable SW Synth'; // for my kxproject without midi table
  Log.LogStatus(MidiOut.ProductName, 'MIDI');
  MidiOut.Open;
//  MidiOut.SetVolume(100, 100); // temporary}

  ConversionFileName := GamePath + 'file.mid';
  {$IFDEF UseMIDIPort}
  MidiFile := TMidiFile.Create(nil);
  {$ENDIF}

  for P := 0 to 100 do
  begin
    ColR[P] := Random(10)/10;
    ColG[P] := Random(10)/10;
    ColB[P] := Random(10)/10;
  end;

end;

procedure TScreenEditConvert.onShow;
var
  T:    integer; // track
  N:    integer; // note
  C:    integer; // channel
  CN:   integer; // channel note
begin
  inherited;

{$IFDEF UseMIDIPort}
  MidiOut := TMidiOutput.Create(nil);
  if Ini.Debug = 1 then
    MidiOut.ProductName := 'Microsoft GS Wavetable SW Synth'; // for my kxproject without midi table
  Log.LogStatus(MidiOut.ProductName, 'MIDI');
  MidiOut.Open;

  if FileExists(ConversionFileName) then
  begin
    MidiFile.Filename := ConversionFileName;
    MidiFile.ReadFile;

    Len := 0;
    Sel := 0;
    BPM := MidiFile.Bpm;
    Ticks := MidiFile.TicksPerQuarter / 4;

{    for T := 0 to MidiFile.NumberOfTracks-1 do
    begin
      SetLength(Track, Length(Track)+1);
      MidiTrack := MidiFile.GetTrack(T);
      MidiTrack.OnMidiEvent := MidiFile1MidiEvent;
      Track[T].Name := MidiTrack.getName;

      for N := 0 to MidiTrack.getEventCount-1 do
      begin
        SetLength(Track[T].Note, Length(Track[T].Note)+1);
        MidiEvent := MidiTrack.GetEvent(N);
        Track[T].Note[N].Start := MidiEvent.time;
        Track[T].Note[N].Len := MidiEvent.len;
        Track[T].Note[N].Event := MidiEvent.event;
        Track[T].Note[N].EventType := MidiEvent.event div 16;
        Track[T].Note[N].Channel := MidiEvent.event and 15;
        Track[T].Note[N].Data1 := MidiEvent.data1;
        Track[T].Note[N].Data2 := MidiEvent.data2;
        Track[T].Note[N].Str := MidiEvent.str;

        if Track[T].Note[N].Start + Track[T].Note[N].Len > Len then
          Len := Track[T].Note[N].Start + Track[T].Note[N].Len;
      end;
    end;}

    SetLength(Channel, 16);
    for T := 0 to 15 do
    begin
      Channel[T].Name := IntToStr(T+1);
      SetLength(Channel[T].Note, 0);
      Channel[T].Status := [];
    end;

    for T := 0 to MidiFile.NumberOfTracks-1 do
    begin
      MidiTrack := MidiFile.GetTrack(T);
      MidiTrack.OnMidiEvent := MidiFile1MidiEvent;

      for N := 0 to MidiTrack.getEventCount-1 do
      begin
        MidiEvent := MidiTrack.GetEvent(N);
        C := MidiEvent.event and 15;

        CN := Length(Channel[C].Note);
        SetLength(Channel[C].Note, CN+1);

        Channel[C].Note[CN].Start := MidiEvent.time;
        Channel[C].Note[CN].Len := MidiEvent.len;
        Channel[C].Note[CN].Event := MidiEvent.event;
        Channel[C].Note[CN].EventType := MidiEvent.event div 16;
        Channel[C].Note[CN].Channel := MidiEvent.event and 15;
        Channel[C].Note[CN].Data1 := MidiEvent.data1;
        Channel[C].Note[CN].Data2 := MidiEvent.data2;
        Channel[C].Note[CN].Str := MidiEvent.str;

        if Channel[C].Note[CN].Start + Channel[C].Note[CN].Len > Len then
          Len := Channel[C].Note[CN].Start + Channel[C].Note[CN].Len;
      end;
    end;
    ATrack := Channel;

  end;

  Interaction := 0;
{$ENDIF}
end;

function TScreenEditConvert.Draw: boolean;
var
  Count:  integer;
  Count2: integer;
  Bottom: real;
  X:      real;
  Y:      real;
  Height: real;
  YSkip:  real;
begin
  // draw static menu
  inherited Draw;

  Y := 100;

  Height := min(480, 40 * Length(ATrack));
  Bottom := Y + Height;

  if Length(ATrack) = 0 then // prevent crash with uncomplete code. 
  begin
    Log.LogDebug ('UScreenEditConvert -> TScreenEditConvert.Draw:', 'Length(ATrack) = 0');
    YSkip := 40;
  end
  else 
    YSkip := Height / Length(ATrack);

  // select
  DrawQuad(10, Y + Sel*YSkip, 780, YSkip, 0.8, 0.8, 0.8);

  // selected - now me use Status System
  for Count := 0 to High(ATrack) do
    if ATrack[Count].Hear then
      DrawQuad(10, Y + Count*YSkip, 50, YSkip, 0.8, 0.3, 0.3);
  glColor3f(0, 0, 0);
  for Count := 0 to High(ATrack) do
  begin
    if Notes in ATrack[Count].Status then
    begin
      SetFontPos(25, Y + Count*YSkip + 10);
      SetFontSize(15);
      glPrint('N');
    end;
    if Lyrics in ATrack[Count].Status then
    begin
      SetFontPos(40, Y + Count*YSkip + 10);
      SetFontSize(15);
      glPrint('L');
    end;
  end;

  DrawLine( 10, Y,  10, Bottom, 0, 0, 0);
  DrawLine( 60, Y,  60, Bottom, 0, 0, 0);
  DrawLine(790, Y, 790, Bottom, 0, 0, 0);

  for Count := 0 to Length(ATrack) do
    DrawLine(10, Y + Count*YSkip, 790, Y + Count*YSkip, 0, 0, 0);

  for Count := 0 to High(ATrack) do
  begin
    SetFontPos(11, Y + 10 + Count*YSkip);
    SetFontSize(15);
    glPrint(ATrack[Count].Name);
  end;

  for Count := 0 to High(ATrack) do
    for Count2 := 0 to High(ATrack[Count].Note) do
    begin
      if ATrack[Count].Note[Count2].EventType = 9 then
        DrawQuad(60 + ATrack[Count].Note[Count2].Start/Len*725,
                 Y + (Count+1)*YSkip - ATrack[Count].Note[Count2].Data1*35/127,
                 3,
                 3,
                 ColR[Count],
                 ColG[Count],
                 ColB[Count]);
      if ATrack[Count].Note[Count2].EventType = 15 then
        DrawLine(60 + ATrack[Count].Note[Count2].Start/Len*725,
                 Y + 0.75*YSkip + Count*YSkip,
                 60 + ATrack[Count].Note[Count2].Start/Len*725,
                 Y + YSkip + Count*YSkip,
                 ColR[Count],
                 ColG[Count],
                 ColB[Count]);
    end;

  // playing line
  {$IFDEF UseMIDIPort}
  X := 60 + MidiFile.GetCurrentTime/MidiFile.GetTrackLength*730;
  {$ENDIF}
  DrawLine(X, Y, X, Bottom, 0.3, 0.3, 0.3);

  Result := true;
end;

procedure TScreenEditConvert.onHide;
begin
{$IFDEF UseMIDIPort}
  MidiOut.Close;
  MidiOut.Free;
{$ENDIF}
end;

end.
