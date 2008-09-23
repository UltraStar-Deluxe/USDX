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

unit UXMLSong;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes;

type
  TNote = record
    Start:    Cardinal;
    Duration: Cardinal;
    Tone:     Integer;
    NoteTyp:  Byte;
    Lyric:    String;
  end;
  ANote = Array of TNote;

  TSentence = record
    Singer:   Byte;
    Duration: Cardinal;
    Notes:    ANote;
  end;
  ASentence = Array of TSentence;

  TSongInfo = Record
    ID: Cardinal;
    DualChannel: Boolean;
    Header: Record
      Artist:     String;
      Title:      String;
      Gap:        Cardinal;
      BPM:        Real;
      Resolution: Byte;
      Edition:    String;
      Genre:      String;
      Year:       String;
      Language:   String;
    end;
    CountSentences: Cardinal;
    Sentences: ASentence;
  end;
  
  TParser = class
    private
      SSFile: TStringList;

      ParserState: Byte;
      CurPosinSong: Cardinal; //Cur Beat Pos in the Song
      CurDuettSinger: Byte;   //Who sings this Part?
      BindLyrics: Boolean;    //Should the Lyrics be bind to the last Word (no Space)
      FirstNote: Boolean;     //Is this the First Note found? For Gap calculating

      Function  ParseLine(Line: String): Boolean;
    public
      SongInfo: TSongInfo;
      ErrorMessage: String;
      Edition: String;
      SingstarVersion: String;

      Settings: Record
        DashReplacement: Char;
      end;

      Constructor Create;

      Function  ParseConfigforEdition(const Filename: String): String;

      Function  ParseSongHeader(const Filename: String): Boolean; //Parse Song Header only
      Function  ParseSong (const Filename: String): Boolean;      //Parse whole Song
  end;

const
  PS_None = 0;
  PS_Melody = 1;
  PS_Sentence = 2;

  NT_Normal = 1;
  NT_Freestyle = 0;
  NT_Golden = 2;

  DS_Player1 = 1;
  DS_Player2 = 2;
  DS_Both    = 3;

implementation
uses SysUtils, StrUtils;

Constructor TParser.Create;
begin
  inherited Create;
  ErrorMessage := '';

  DecimalSeparator := '.';
end;

Function  TParser.ParseSong (const Filename: String): Boolean;
var I: Integer;
begin
  Result := False;
  if FileExists(Filename) then
  begin
    SSFile := TStringList.Create;

    try
      ErrorMessage := 'Can''t open melody.xml file';
      SSFile.LoadFromFile(Filename);
      ErrorMessage := '';
      Result := True;
      I := 0;

      SongInfo.CountSentences := 0;
      CurDuettSinger := DS_Both; //Both is Singstar Standard
      CurPosinSong   := 0; //Start at Pos 0
      BindLyrics     := True; //Dont start with Space
      FirstNote      := True; //First Note found should be the First Note ;)

      SongInfo.Header.Language := '';
      SongInfo.Header.Edition := Edition;
      SongInfo.DualChannel := False;

      ParserState    := PS_None;

      SetLength(SongInfo.Sentences, 0);

      While Result And (I < SSFile.Count) do
      begin
        Result := ParseLine(SSFile.Strings[I]);

        Inc(I);
      end;

    finally
      SSFile.Free;
    end;
  end;
end;

Function  TParser.ParseSongHeader (const Filename: String): Boolean;
var I: Integer;
begin
  Result := False;
  if FileExists(Filename) then
  begin
    SSFile := TStringList.Create;
    SSFile.Clear;
    
    try
      SSFile.LoadFromFile(Filename);

      If (SSFile.Count > 0) then
      begin
        Result := True;
        I := 0;

        SongInfo.CountSentences := 0;
        CurDuettSinger := DS_Both; //Both is Singstar Standard
        CurPosinSong   := 0; //Start at Pos 0
        BindLyrics     := True; //Dont start with Space
        FirstNote      := True; //First Note found should be the First Note ;)

        SongInfo.ID := 0;
        SongInfo.Header.Language := '';
        SongInfo.Header.Edition := Edition;
        SongInfo.DualChannel := False;
        ParserState    := PS_None;

        While (SongInfo.ID < 4) AND Result And (I < SSFile.Count) do
        begin
          Result := ParseLine(SSFile.Strings[I]);

          Inc(I);
        end;
      end
      else
        ErrorMessage := 'Can''t open melody.xml file';

    finally
      SSFile.Free;
    end;
  end
  else
    ErrorMessage := 'Can''t find melody.xml file';
end;

Function TParser.ParseLine(Line: String): Boolean;
var
  Tag:    String;
  Values: String;
  AValues: Array of Record
    Name: String;
    Value: String;
  end;
  I, J, K:   Integer;
  Duration, Tone: Integer;
  Lyric: String;
  NoteType: Byte;

  Procedure MakeValuesArray;
  var Len, Pos, State, StateChange: Integer;
  begin
    Len := -1;
    SetLength(AValues, Len + 1);

    Pos := 1;
    State := 0;
    While (Pos <= Length(Values)) AND (Pos <> 0) do
    begin
      Case State of

        0: begin //Search for ValueName
          If (Values[Pos] <> ' ') AND (Values[Pos] <> '=') then
          begin
            //Found Something
            State := 1; //State search for '='
            StateChange := Pos; //Save Pos of Change
            Pos := PosEx('=', Values, Pos + 1);
          end
          else Inc(Pos); //When nothing found then go to next char
        end;

        1: begin //Search for Equal Mark
          //Add New Value
          Inc(Len);
          SetLength(AValues, Len + 1);

          AValues[Len].Name := UpperCase(Copy(Values, StateChange, Pos - StateChange));


          State := 2; //Now Search for starting '"'
          StateChange := Pos; //Save Pos of Change
          Pos := PosEx('"', Values, Pos + 1);
        end;

        2: begin //Search for starting '"' or ' ' <- End if there was no "
          If (Values[Pos] = '"') then
          begin //Found starting '"'
            State := 3; //Now Search for ending '"'
            StateChange := Pos; //Save Pos of Change
            Pos := PosEx('"', Values, Pos + 1);
          end
          else If (Values[Pos] = ' ') then //Found ending Space
          begin
            //Save Value to Array
            AValues[Len].Value := Copy(Values, StateChange + 1, Pos - StateChange - 1);

            //Search for next Valuename
            State := 0;
            StateChange := Pos;
            Inc(Pos);
          end;
        end;

        3: begin //Search for ending '"'
          //Save Value to Array
          AValues[Len].Value := Copy(Values, StateChange + 1, Pos - StateChange - 1);

          //Search for next Valuename
          State := 0;
          StateChange := Pos;
          Inc(Pos);
        end;
      end;

      If (State >= 2) then
      begin //Save Last Value
        AValues[Len].Value := Copy(Values, StateChange + 1, Length(Values) - StateChange);
      end;
    end;
  end;
begin
  Result := True;

  Line := Trim(Line);
  If (Length(Line) > 0) then
  begin
    I := Pos('<', Line);
    J := PosEx(' ', Line, I+1);
    K := PosEx('>', Line, I+1);

    If (J = 0) then J := K
    Else If (K < J) AND (K <> 0) then J := K; //Use nearest Tagname End indicator
    Tag := UpperCase(copy(Line, I + 1, J - I - 1));
    Values := copy(Line, J + 1, K - J - 1);

    Case ParserState of
      PS_None: begin//Search for Melody Tag
        If (Tag = 'MELODY') then
        begin
          Inc(SongInfo.ID); //Inc SongID when header Information is added
          MakeValuesArray;
          For I := 0 to High(AValues) do
          begin
            If (AValues[I].Name = 'TEMPO') then
            begin
              SongInfo.Header.BPM := StrtoFloatDef(AValues[I].Value, 0);
              If (SongInfo.Header.BPM <= 0) then
              begin
                Result := False;
                ErrorMessage := 'Can''t read BPM from Song';
              end;
            end

            Else If (AValues[I].Name = 'RESOLUTION') then
            begin
              AValues[I].Value := Uppercase(AValues[I].Value);
              //Ultrastar Resolution is "how often a Beat is split / 4"
              If (AValues[I].Value = 'HEMIDEMISEMIQUAVER') then
                SongInfo.Header.Resolution := 64 div 4
              Else If (AValues[I].Value = 'DEMISEMIQUAVER') then
                SongInfo.Header.Resolution := 32 div 4
              Else If (AValues[I].Value = 'SEMIQUAVER') then
                SongInfo.Header.Resolution := 16 div 4
              Else If (AValues[I].Value = 'QUAVER') then
                SongInfo.Header.Resolution := 8 div 4
              Else If (AValues[I].Value = 'CROTCHET') then
                SongInfo.Header.Resolution := 4 div 4
              Else
              begin //Can't understand teh Resolution :/
                Result := False;
                ErrorMessage := 'Can''t read Resolution from Song';
              end;
            end

            Else If (AValues[I].Name = 'GENRE') then
            begin
              SongInfo.Header.Genre := AValues[I].Value;
            end

            Else If (AValues[I].Name = 'YEAR') then
            begin
              SongInfo.Header.Year := AValues[I].Value;
            end

            Else If (AValues[I].Name = 'VERSION') then
            begin
              SingstarVersion := AValues[I].Value;
            end;
          end;

          ParserState := PS_Melody; //In Melody Tag
        end;
      end;


      PS_Melody: begin //Search for Sentence, Artist/Title Info or eo Melody
        If (Tag = 'SENTENCE') then
        begin
          ParserState := PS_Sentence; //Parse in a Sentence Tag now
          
          //Increase SentenceCount
          Inc(SongInfo.CountSentences);

          BindLyrics := True; //Don't let Txts Begin w/ Space
          
          //Search for Duett Singer Info
          MakeValuesArray;
          For I := 0 to High(AValues) do
            If (AValues[I].Name = 'SINGER') then
            begin
              AValues[I].Value := Uppercase(AValues[I].Value);
              If (AValues[I].Value = 'SOLO 1') then
                CurDuettSinger := DS_Player1
              Else If (AValues[I].Value = 'SOLO 2') then
                CurDuettSinger := DS_Player2
              Else
                CurDuettSinger := DS_Both; //In case of "Group" or anything that is not identified use Both
            end;
        end

        Else If (Tag = '!--') then
        begin //Comment, this may be Artist or Title Info
          I := Pos(':', Values); //Search for Delimiter

          If (I <> 0) then //If Found check for Title or Artist
          begin
            //Copy Title or Artist Tag to Tag String
            Tag := Uppercase(Trim(Copy(Values, 1, I - 1)));

            If (Tag = 'ARTIST') then
            begin
              SongInfo.Header.Artist := Trim(Copy(Values, I + 1, Length(Values) - I - 2));
              Inc(SongInfo.ID); //Inc SongID when header Information is added
            end
            Else If (Tag = 'TITLE') then
            begin
              SongInfo.Header.Title := Trim(Copy(Values, I + 1, Length(Values) - I - 2));
              Inc(SongInfo.ID); //Inc SongID when header Information is added
            end;
          end;
        end

        //Parsing for weird "Die toten Hosen" Tags
        Else If (Tag = '!--ARTIST:') OR (Tag = '!--ARTIST') then
        begin //Comment, with Artist Info
          I := Pos(':', Values); //Search for Delimiter

          Inc(SongInfo.ID); //Inc SongID when header Information is added

          SongInfo.Header.Artist := Trim(Copy(Values, I + 1, Length(Values) - I - 2));
        end

        Else If (Tag = '!--TITLE:') OR (Tag = '!--TITLE') then
        begin //Comment, with Artist Info
          I := Pos(':', Values); //Search for Delimiter

          Inc(SongInfo.ID); //Inc SongID when header Information is added

          SongInfo.Header.Title := Trim(Copy(Values, I + 1, Length(Values) - I - 2));
        end

        Else If (Tag = '/MELODY') then
        begin
          ParserState := PS_None;
          Exit; //Stop Parsing, Melody iTag ended
        end
      end;


      PS_Sentence: begin //Search for Notes or eo Sentence
        If (Tag = 'NOTE') then
        begin //Found Note
          //Get Values
          MakeValuesArray;

          NoteType := NT_Normal;
          For I := 0 to High(AValues) do
          begin
            If (AValues[I].Name = 'DURATION') then
            begin
              Duration := StrtoIntDef(AValues[I].Value, -1);
              If (Duration < 0) then
              begin
                Result := False;
                ErrorMessage := 'Can''t read duration from Note in Line: "' + Line + '"';
                Exit;
              end;
            end
            Else If (AValues[I].Name = 'MIDINOTE') then
            begin
              Tone := StrtoIntDef(AValues[I].Value, 0);
            end
            Else If (AValues[I].Name = 'BONUS') AND (Uppercase(AValues[I].Value) = 'YES') then
            begin
              NoteType := NT_Golden;
            end
            Else If (AValues[I].Name = 'FREESTYLE') AND (Uppercase(AValues[I].Value) = 'YES') then
            begin
              NoteType := NT_Freestyle;
            end
            Else If (AValues[I].Name = 'LYRIC') then
            begin
              Lyric := AValues[I].Value;

              If (Length(Lyric) > 0) then
              begin
                If (Lyric = '-') then
                  Lyric[1] := Settings.DashReplacement;

                If (not BindLyrics) then
                  Lyric := ' ' + Lyric;


                If (Length(Lyric) > 2) AND (Lyric[Length(Lyric)-1] = ' ') AND (Lyric[Length(Lyric)] = '-') then
                begin //Between this and the next Lyric should be no space
                  BindLyrics := True;
                  SetLength(Lyric, Length(Lyric) - 2);
                end
                else
                  BindLyrics := False; //There should be a Space
              end;
            end;
          end;

          //Add Note
          I := SongInfo.CountSentences - 1;

          If (Length(Lyric) > 0) then
          begin //Real note, no rest
            //First Note of Sentence
            If (Length(SongInfo.Sentences) < SongInfo.CountSentences) then
            begin
              SetLength(SongInfo.Sentences, SongInfo.CountSentences);
              SetLength(SongInfo.Sentences[I].Notes, 0);
            end;

            //First Note of Song -> Generate Gap
            If (FirstNote) then
            begin
              //Calculate Gap
              If (SongInfo.Header.Resolution <> 0) AND (SongInfo.Header.BPM <> 0) then
                SongInfo.Header.Gap := Round(CurPosinSong / (SongInfo.Header.BPM*SongInfo.Header.Resolution) * 60000)
              Else
              begin
                Result := False;
                ErrorMessage := 'Can''t calculate Gap, no Resolution or BPM present.';
                Exit;
              end;

              CurPosinSong := 0; //Start at 0, because Gap goes until here
              Inc(SongInfo.ID); //Add Header Value therefore Inc
              FirstNote := False;
            end;

            J := Length(SongInfo.Sentences[I].Notes);
            SetLength(SongInfo.Sentences[I].Notes, J + 1);
            SongInfo.Sentences[I].Notes[J].Start := CurPosinSong;
            SongInfo.Sentences[I].Notes[J].Duration := Duration;
            SongInfo.Sentences[I].Notes[J].Tone := Tone;
            SongInfo.Sentences[I].Notes[J].NoteTyp := NoteType;
            SongInfo.Sentences[I].Notes[J].Lyric := Lyric;

            //Inc Pos in Song
            Inc(CurPosInSong, Duration);
          end
          else
          begin
            //just change pos in Song
            Inc(CurPosInSong, Duration);
          end;


        end
        Else If (Tag = '/SENTENCE') then
        begin //End of Sentence Tag
          ParserState := PS_Melody;

          //Delete Sentence if no Note is Added
          If (Length(SongInfo.Sentences) <> SongInfo.CountSentences) then
          begin
            SongInfo.CountSentences := Length(SongInfo.Sentences);
          end;
        end;
      end;
    end;

  end
  else  //Empty Line -> parsed succesful ;)
    Result := true;
end;

Function  TParser.ParseConfigforEdition(const Filename: String): String;
var
  txt: TStringlist;
  I: Integer;
  J, K: Integer;
  S: String;
begin
  Result := '';
  txt := TStringlist.Create;
  try
    txt.LoadFromFile(Filename);

    For I := 0 to txt.Count-1 do
    begin
      S := Trim(txt.Strings[I]);
      J := Pos('<PRODUCT_NAME>', S);

      If (J <> 0) then
      begin
        Inc(J, 14);
        K := Pos('</PRODUCT_NAME>', S);
        If (K<J) then K := Length(S) + 1;

        Result := Copy(S, J, K - J);
        Break;
      end;
    end;

    Edition := Result;
  finally
    txt.Free;
  end;
end;

end.
