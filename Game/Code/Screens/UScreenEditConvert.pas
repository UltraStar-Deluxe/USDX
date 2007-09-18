unit UScreenEditConvert;

interface

{$I switches.inc}

uses UMenu,
     SDL,
     {$IFDEF UseMIDIPort}
     MidiFile,
     MidiOut,
     {$ENDIF}
     ULog,
     USongs,
     UMusic,
     UThemes;

type
  TNote = record
    Event:        integer;
    EventType:    integer;
    Channel:    integer;
    Start:    real;
    Len:      real;
    Data1:    integer;
    Data2:    integer;
    Str:      string;
  end;

  TTrack = record
    Note:     array of TNote;
    Name:     string;
    Hear:     boolean;
    Status:   byte;     // 0 - none, 1 - notes, 2 - lyrics, 3 - notes + lyrics
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
      ATrack:             TArrayTrack; // actual track
//      Track:              TArrayTrack;
      Channel:            TArrayTrack;
      ColR:               array[0..100] of real;
      ColG:               array[0..100] of real;
      ColB:               array[0..100] of real;
      Len:                real;
      Sel:                integer;
      Selected:           boolean;
//      FileName:           string;

      {$IFDEF UseMIDIPort}
      MidiFile:           TMidiFile;
      MidiTrack:          TMidiTrack;
      MidiEvent:          pMidiEvent;
      MidiOut:            TMidiOutput;
      {$ENDIF}
      
      Song:               TSong;
      Czesc:              TCzesci;
      BPM:                real;
      Ticks:              real;
      Nuta:               array of TNuta;

      procedure AddLyric(Start: integer; Tekst: string);
      procedure Extract;

      {$IFDEF UseMIDIPort}
      procedure MidiFile1MidiEvent(event: PMidiEvent);
      {$ENDIF}
      
      function SelectedNumber: integer;
      constructor Create; override;
      procedure onShow; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      function Draw: boolean; override;
      procedure onHide; override;
  end;

implementation
uses UGraphic,
     SysUtils,
     UDrawTexture,
     TextGL,
     UFiles,
     UMain,
     UIni,
     OpenGL12,
     USkins;

function TScreenEditConvert.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
var
  T:    integer;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;


      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
      {$IFDEF UseMIDIPort}
          MidiFile.StopPlaying;
      {$ENDIF}
          Music.PlayBack;
          FadeTo(@ScreenEdit);
        end;

      SDLK_RETURN:
        begin
          if Interaction = 0 then begin
            Music.PlayStart;
            ScreenOpen.BackScreen := @ScreenEditConvert;
            FadeTo(@ScreenOpen);
          end;

          if Interaction = 1 then begin
            Selected := false;
      {$IFDEF UseMIDIPort}
            MidiFile.OnMidiEvent := MidiFile1MidiEvent;
//            MidiFile.GoToTime(MidiFile.GetTrackLength div 2);
            MidiFile.StartPlaying;
            {$ENDIF}
          end;

          if Interaction = 2 then begin
            Selected := true;
            {$IFDEF UseMIDIPort}
            MidiFile.OnMidiEvent := nil;
            {$ENDIF}
            {for T := 0 to High(ATrack) do begin
              if ATrack[T].Hear then begin
                MidiTrack := MidiFile.GetTrack(T);
                MidiTrack.OnMidiEvent := MidiFile1MidiEvent;
              end;
            end;
            MidiFile.StartPlaying;//}
          end;

          if Interaction = 3 then begin
            if SelectedNumber > 0 then begin
              Extract;
              SaveSong(Song, Czesc, ChangeFileExt(FileName, '.txt'), false);
            end;
          end;

        end;

      SDLK_SPACE:
        begin
//          ATrack[Sel].Hear := not ATrack[Sel].Hear;
          ATrack[Sel].Status := (ATrack[Sel].Status + 1) mod 4;

{          if Selected then begin
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
          if Sel > High(ATrack) then Sel := 0;
        end;
      SDLK_UP:
        begin
          Dec(Sel);
          if Sel < 0 then Sel := High(ATrack);
        end;
    end;
  end;
end;

procedure TScreenEditConvert.AddLyric(Start: integer; Tekst: string);
var
  N:    integer;
begin
  for N := 0 to High(Nuta) do begin
    if Nuta[N].Start = Start then begin
      // check for new sentece
      if Copy(Tekst, 1, 1) = '\' then Delete(Tekst, 1, 1);
      if Copy(Tekst, 1, 1) = '/' then begin
        Delete(Tekst, 1, 1);
        Nuta[N].NewSentence := true;
      end;

      // overwrite lyric od append
      if Nuta[N].Lyric = '-' then
        Nuta[N].Lyric := Tekst
      else
        Nuta[N].Lyric := Nuta[N].Lyric + Tekst;
    end;
  end;
end;

procedure TScreenEditConvert.Extract;
var
  T:    integer;
  C:    integer;
  N:    integer;
  Nu:   integer;
  NutaTemp: TNuta;
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

  SetLength(Nuta, 0);

  // extract notes
  for T := 0 to High(ATrack) do begin
//    if ATrack[T].Hear then begin
    if ((ATrack[T].Status div 1) and 1) = 1 then begin
      for N := 0 to High(ATrack[T].Note) do begin
        if (ATrack[T].Note[N].EventType = 9) and (ATrack[T].Note[N].Data2 > 0) then begin
          Nu := Length(Nuta);
          SetLength(Nuta, Nu + 1);
          Nuta[Nu].Start := Round(ATrack[T].Note[N].Start / Ticks);
          Nuta[Nu].Len := Round(ATrack[T].Note[N].Len / Ticks);
          Nuta[Nu].Tone := ATrack[T].Note[N].Data1 - 12*5;
          Nuta[Nu].Lyric := '-';
        end;
      end;
    end;
  end;

  // extract lyrics
  for T := 0 to High(ATrack) do begin
//    if ATrack[T].Hear then begin
    if ((ATrack[T].Status div 2) and 1) = 1 then begin
      for N := 0 to High(ATrack[T].Note) do begin
        if (ATrack[T].Note[N].EventType = 15) then begin
//          Log.LogStatus('<' + Track[T].Note[N].Str + '>', 'MIDI');
          AddLyric(Round(ATrack[T].Note[N].Start / Ticks), ATrack[T].Note[N].Str);
        end;
      end;
    end;
  end;

  // sort notes
  for N := 0 to High(Nuta) do
    for Nu := 0 to High(Nuta)-1 do
      if Nuta[Nu].Start > Nuta[Nu+1].Start then begin
        NutaTemp := Nuta[Nu];
        Nuta[Nu] := Nuta[Nu+1];
        Nuta[Nu+1] := NutaTemp;
      end;

  // move to 0 at beginning
  Move := Nuta[0].Start;
  for N := 0 to High(Nuta) do
    Nuta[N].Start := Nuta[N].Start - Move;

  // copy notes
  SetLength(Czesc.Czesc, 1);
  Czesc.Ilosc := 1;
  Czesc.High := 0;

  C := 0;
  N := 0;
  Czesc.Czesc[C].IlNut := 0;
  Czesc.Czesc[C].HighNut := -1;

  for Nu := 0 to High(Nuta) do begin
    if Nuta[Nu].NewSentence then begin // nowa linijka
      SetLength(Czesc.Czesc, Length(Czesc.Czesc)+1);
      Czesc.Ilosc := Czesc.Ilosc + 1;
      Czesc.High := Czesc.High + 1;
      C := C + 1;
      N := 0;
      SetLength(Czesc.Czesc[C].Nuta, 0);
      Czesc.Czesc[C].IlNut := 0;
      Czesc.Czesc[C].HighNut := -1;

      //Calculate Start of the Last Sentence
      if (C > 0) and (Nu > 0) then
      begin
        Max := Nuta[Nu].Start;
        Min := Nuta[Nu-1].Start + Nuta[Nu-1].Len;
        
        case (Max - Min) of
          0:    Czesc.Czesc[C].Start := Max;
          1:    Czesc.Czesc[C].Start := Max;
          2:    Czesc.Czesc[C].Start := Max - 1;
          3:    Czesc.Czesc[C].Start := Max - 2;
          else
            if ((Max - Min) > 4) then
              Czesc.Czesc[C].Start := Min + 2
            else
              Czesc.Czesc[C].Start := Max;

        end; // case

      end;
    end;

    // tworzy miejsce na nowa nute
    SetLength(Czesc.Czesc[C].Nuta, Length(Czesc.Czesc[C].Nuta)+1);
    Czesc.Czesc[C].IlNut := Czesc.Czesc[C].IlNut + 1;
    Czesc.Czesc[C].HighNut := Czesc.Czesc[C].HighNut + 1;

    // dopisuje
    Czesc.Czesc[C].Nuta[N].Start := Nuta[Nu].Start;
    Czesc.Czesc[C].Nuta[N].Dlugosc := Nuta[Nu].Len;
    Czesc.Czesc[C].Nuta[N].Ton := Nuta[Nu].Tone;
    Czesc.Czesc[C].Nuta[N].Tekst := Nuta[Nu].Lyric;
    //All Notes are Freestyle when Converted Fix:
    Czesc.Czesc[C].Nuta[N].Wartosc := 1;
    Inc(N);
  end;
end;

function TScreenEditConvert.SelectedNumber: integer;
var
  T:    integer; // track
begin
  Result := 0;
  for T := 0 to High(ATrack) do
//    if ATrack[T].Hear then Inc(Result);
    if ((ATrack[T].Status div 1) and 1) = 1 then Inc(Result);
end;

procedure TScreenEditConvert.MidiFile1MidiEvent(event: PMidiEvent);
begin
//  Log.LogStatus(IntToStr(event.event), 'MIDI');
  MidiOut.PutShort(event.event, event.data1, event.data2);
end;

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

  FileName := GamePath + 'file.mid';
  {$IFDEF UseMIDIPort}
  MidiFile := TMidiFile.Create(nil);
  {$ENDIF}

  for P := 0 to 100 do begin
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
  MidiOut := TMidiOutput.Create(nil);
  if Ini.Debug = 1 then
    MidiOut.ProductName := 'Microsoft GS Wavetable SW Synth'; // for my kxproject without midi table
  Log.LogStatus(MidiOut.ProductName, 'MIDI');
  MidiOut.Open;


  if FileExists(FileName) then
  begin
    MidiFile.Filename := FileName;
    MidiFile.ReadFile;


    Len := 0;
    Sel := 0;
    BPM := MidiFile.Bpm;
    Ticks := MidiFile.TicksPerQuarter / 4;

{    for T := 0 to MidiFile.NumberOfTracks-1 do begin
      SetLength(Track, Length(Track)+1);
      MidiTrack := MidiFile.GetTrack(T);
      MidiTrack.OnMidiEvent := MidiFile1MidiEvent;
      Track[T].Name := MidiTrack.getName;

      for N := 0 to MidiTrack.getEventCount-1 do begin
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
      Channel[T].Status := 0;
    end;

    for T := 0 to MidiFile.NumberOfTracks-1 do begin
      MidiTrack := MidiFile.GetTrack(T);
      MidiTrack.OnMidiEvent := MidiFile1MidiEvent;

      for N := 0 to MidiTrack.getEventCount-1 do begin
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
end;

function TScreenEditConvert.Draw: boolean;
var
  Pet:    integer;
  Pet2:   integer;
  Bottom: real;
  X:      real;
  Y:      real;
  H:      real;
  YSkip:  real;
begin
  // draw static menu
  inherited Draw;

  Y := 100;

  H := Length(ATrack)*40;
  if H > 480 then H := 480;
  Bottom := Y + H;

  YSkip := H / Length(ATrack);

  // select
  DrawQuad(10, Y+Sel*YSkip, 780, YSkip, 0.8, 0.8, 0.8);

  // selected - now me use Status System
  for Pet := 0 to High(ATrack) do
    if ATrack[Pet].Hear then
      DrawQuad(10, Y+Pet*YSkip, 50, YSkip, 0.8, 0.3, 0.3);
  glColor3f(0, 0, 0);
  for Pet := 0 to High(ATrack) do begin
    if ((ATrack[Pet].Status div 1) and 1) = 1 then begin
      SetFontPos(25, Y + Pet*YSkip + 10);
      SetFontSize(5);
      glPrint('N');
    end;
    if ((ATrack[Pet].Status div 2) and 1) = 1 then begin
      SetFontPos(40, Y + Pet*YSkip + 10);
      SetFontSize(5);
      glPrint('L');
    end;
  end;

  DrawLine(10, Y,   10, Bottom, 0, 0, 0);
  DrawLine(60, Y,   60, Bottom, 0, 0, 0);
  DrawLine(790, Y, 790, Bottom, 0, 0, 0);

  for Pet := 0 to Length(ATrack) do
    DrawLine(10, Y+Pet*YSkip, 790, Y+Pet*YSkip, 0, 0, 0);

  for Pet := 0 to High(ATrack) do begin
    SetFontPos(11, Y + 10 + Pet*YSkip);
    SetFontSize(5);
    glPrint(pchar(ATrack[Pet].Name));
  end;

  for Pet := 0 to High(ATrack) do
    for Pet2 := 0 to High(ATrack[Pet].Note) do begin
      if ATrack[Pet].Note[Pet2].EventType = 9 then
        DrawQuad(60 + ATrack[Pet].Note[Pet2].Start/Len * 725, Y + (Pet+1)*YSkip - ATrack[Pet].Note[Pet2].Data1*35/127, 3, 3, ColR[Pet], ColG[Pet], ColB[Pet]);
      if ATrack[Pet].Note[Pet2].EventType = 15 then
        DrawLine(60 + ATrack[Pet].Note[Pet2].Start/Len * 725, Y + 0.75 * YSkip + Pet*YSkip, 60 + ATrack[Pet].Note[Pet2].Start/Len * 725, Y + YSkip + Pet*YSkip, ColR[Pet], ColG[Pet], ColB[Pet]);
    end;

  // playing line
  X := 60+MidiFile.GetCurrentTime/MidiFile.GetTrackLength*730;
  DrawLine(X, Y, X, Bottom, 0.3, 0.3, 0.3);


end;

procedure TScreenEditConvert.onHide;
begin
  MidiOut.Close;
  MidiOut.Free;
end;

end.
