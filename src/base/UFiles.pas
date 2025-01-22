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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UFiles.pas $
 * $Id: UFiles.pas 2510 2010-06-13 09:03:10Z tobigun $
 *}

unit UFiles;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$I switches.inc}

uses
  SysUtils,
  Classes,
  ULog,
  UMusic,
  USongs,
  USong,
  UPath;

procedure ResetSingTemp;

type
  TSaveSongResult = (ssrOK, ssrFileError, ssrEncodingError);

{**
 * Throws a TEncodingException if the song's fields cannot be encoded in the
 * requested encoding.
 *}
function SaveSong(const Song: TSong; const Tracks: array of TLines; const Name: IPath; Relative: boolean): TSaveSongResult;

implementation

uses
  TextGL,
  UIni,
  UNote,
  UPlatform,
  UUnicodeUtils,
  UTextEncoding;

//--------------------
// Resets the temporary Sentence Arrays for each Player and some other Variables
//--------------------
procedure ResetSingTemp;
var
  Count:  integer;
begin
  SetLength(Tracks, Length(Player));
  for Count := 0 to High(Player) do begin
    SetLength(Tracks[Count].Lines, 1);
    SetLength(Tracks[Count].Lines[0].Notes, 0);
    Tracks[Count].Lines[0].Lyric := '';
    Player[Count].Score := 0;
    Player[Count].LengthNote := 0;
    Player[Count].HighNote := -1;
  end;
end;

//--------------------
// Saves a Song
//--------------------
function SaveSong(const Song: TSong; const Tracks: array of TLines; const Name: IPath; Relative: boolean): TSaveSongResult;
var
  CurrentLine:      integer;
  CurrentNote:      integer;
  CurrentTrack:     integer;
  Line:             AnsiString;
  B:      integer;
  RelativeSubTime: integer;
  NoteState: AnsiString;
  SongFile: TTextFileStream;

  function EncodeToken(const Str: UTF8String): RawByteString;
  var
    Success: boolean;
  begin
    Success := EncodeStringUTF8(Str, Result, Song.Encoding);
    if (not Success) then
      SaveSong := ssrEncodingError;
  end;

  procedure WriteCustomTags;
    var
      I: integer;
      Line: RawByteString;
  begin
    for I := 0 to High(Song.CustomTags) do
    begin
      Line := EncodeToken(Song.CustomTags[I].Content);
      if (Length(Song.CustomTags[I].Tag) > 0) then
        Line := EncodeToken(Song.CustomTags[I].Tag) + ':' + Line;

      SongFile.WriteLine('#' + Line);
    end;

  end;

begin
  //  Relative := true; // override (idea - use shift+S to save with relative)
  Result := ssrOK;

  try
    SongFile := TMemTextFileStream.Create(Name, fmCreate);
    try
      if Song.FormatVersion.MinVersion(1,0,0,true) then
      begin
        // Only save version if it is at least 1.0.0
        SongFile.WriteLine('#VERSION:' + EncodeToken(Song.FormatVersion.VersionString));
        // RELATIVE was removed in format 1.0.0
        Relative := False;
      end
      else
      begin
        // Only save Encoding if Version is below 1.0.0
        // do not save "auto" encoding tag
        if (Song.Encoding <> encAuto) then
          SongFile.WriteLine('#ENCODING:' + EncodingName(Song.Encoding));
      end;
      SongFile.WriteLine('#TITLE:'    + EncodeToken(Song.Title));
      SongFile.WriteLine('#ARTIST:'   + EncodeToken(Song.Artist));

      if Song.Language    <> 'Unknown' then    SongFile.WriteLine('#LANGUAGE:'  + EncodeToken(Song.Language));
      if Song.Edition     <> 'Unknown' then    SongFile.WriteLine('#EDITION:'   + EncodeToken(Song.Edition));
      if Song.Genre       <> 'Unknown' then    SongFile.WriteLine('#GENRE:'     + EncodeToken(Song.Genre));
      if Song.Year        <> 0         then    SongFile.WriteLine('#YEAR:'      + IntToStr(Song.Year));
      if Song.Creator     <> ''        then    SongFile.WriteLine('#CREATOR:'   + EncodeToken(Song.Creator));

      if Song.FormatVersion.MinVersion(1,1,0) then
        SongFile.WriteLine('#AUDIO:' + EncodeToken(Song.Audio.ToUTF8));
      if Song.FormatVersion.MaxVersion(2,0,0) then
         SongFile.WriteLine('#MP3:' + EncodeToken(Song.Audio.ToUTF8));

      if Song.Cover.IsSet              then    SongFile.WriteLine('#COVER:'       + EncodeToken(Song.Cover.ToUTF8));
      if Song.Background.IsSet         then    SongFile.WriteLine('#BACKGROUND:'  + EncodeToken(Song.Background.ToUTF8));
      if Song.Video.IsSet              then    SongFile.WriteLine('#VIDEO:'       + EncodeToken(Song.Video.ToUTF8));

      if Song.VideoGAP    <> 0.0       then    SongFile.WriteLine('#VIDEOGAP:'    + FloatToStr(Song.VideoGAP));
      if Song.Resolution  <> USong.DEFAULT_RESOLUTION then    SongFile.WriteLine('#RESOLUTION:'  + IntToStr(Song.Resolution));
      if Song.NotesGAP    <> 0         then    SongFile.WriteLine('#NOTESGAP:'    + IntToStr(Song.NotesGAP));
      if Song.Start       <> 0.0       then    SongFile.WriteLine('#START:'       + FloatToStr(Song.Start));
      if Song.Finish      <> 0         then    SongFile.WriteLine('#END:'         + IntToStr(Song.Finish));
      if not Song.isDuet and Relative  then    SongFile.WriteLine('#RELATIVE:yes');

      if Song.HasPreview and (Song.PreviewStart >= 0.0) then // also allow writing 0.0 preview if set
        SongFile.WriteLine('#PREVIEWSTART:' + FloatToStr(Song.PreviewStart));

      if (Song.Medley.Source=msTag) and not Relative and (Song.Medley.EndBeat - Song.Medley.StartBeat > 0) then
      begin
        SongFile.WriteLine('#MEDLEYSTARTBEAT:' + IntToStr(Song.Medley.StartBeat));
        SongFile.WriteLine('#MEDLEYENDBEAT:' + IntToStr(Song.Medley.EndBeat));
      end;

      SongFile.WriteLine('#BPM:' + FloatToStr(Song.BPM[0].BPM / 4));
      SongFile.WriteLine('#GAP:' + FloatToStr(Song.GAP));

      if Song.isDuet then
      begin
        SongFile.WriteLine('#P1:' + EncodeToken(Song.DuetNames[0]));
        SongFile.WriteLine('#P2:' + EncodeToken(Song.DuetNames[1]));
      end;

      // write custom header tags
      WriteCustomTags;

      RelativeSubTime := 0;
      for B := 1 to High(Song.BPM) do
        SongFile.WriteLine('B ' + FloatToStr(Song.BPM[B].StartBeat) + ' '
                                + FloatToStr(Song.BPM[B].BPM/4));

      for CurrentTrack := 0 to High(Tracks) do
      begin
        if Song.isDuet then
        begin
          Line := 'P' + IntToStr(CurrentTrack+1);
          SongFile.WriteLine(Line);
        end;

        for CurrentLine := 0 to Tracks[CurrentTrack].High do
        begin
          for CurrentNote := 0 to Tracks[CurrentTrack].Lines[CurrentLine].HighNote do
          begin
            with Tracks[CurrentTrack].Lines[CurrentLine].Notes[CurrentNote] do
            begin
              //Golden + Freestyle Note Patch
              case Tracks[CurrentTrack].Lines[CurrentLine].Notes[CurrentNote].NoteType of
                ntFreestyle: NoteState := 'F ';
                ntNormal: NoteState := ': ';
                ntGolden: NoteState := '* ';
                ntRap: NoteState:= 'R ';
                ntRapGolden: NoteState:='G ';
              end; // case
              Line := NoteState + IntToStr(StartBeat - RelativeSubTime) + ' '
                                + IntToStr(Duration) + ' '
                                + IntToStr(Tone) + ' '
                                + EncodeToken(Text);

              SongFile.WriteLine(Line);
            end; // with
          end; // CurrentNote

          if CurrentLine < Tracks[CurrentTrack].High then // don't write end of last sentence
          begin
            if not Relative then
              Line := '- ' + IntToStr(Tracks[CurrentTrack].Lines[CurrentLine+1].StartBeat)
            else
            begin
              Line := '- ' + IntToStr(Tracks[CurrentTrack].Lines[CurrentLine+1].StartBeat - RelativeSubTime) +
                ' ' + IntToStr(Tracks[CurrentTrack].Lines[CurrentLine+1].StartBeat - RelativeSubTime);
              RelativeSubTime := Tracks[CurrentTrack].Lines[CurrentLine+1].StartBeat;
            end;
            SongFile.WriteLine(Line);
          end;
        end; // CurrentLine
      end;

      SongFile.WriteLine('E');
    finally
      SongFile.Free;
    end;
  except
    Result := ssrFileError;
  end;
end;

end.

