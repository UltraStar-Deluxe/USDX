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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UEditorLyrics.pas $
 * $Id: UEditorLyrics.pas 2488 2010-06-11 15:54:50Z whiteshark0 $
 *}

unit UEditorLyrics;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  dglOpenGL,
  UMusic,
  UTexture;

type
  TWord = record
    X:          real;
    Y:          real;
    Size:       real;
    Width:      real;
    Text:       string;
    ColR:       real;
    ColG:       real;
    ColB:       real;
    FontFamily: integer;
    FontStyle:  integer;
    Italic:     boolean;
    Selected:   boolean;
  end;

  TEditorLyrics = class
    private
      AlignI:      integer;
      XR:          real;
      YR:          real;
      SizeR:       real;
      SelectedI:   integer;
      FontFamilyI: integer;         // font number
      FontStyleI:  integer;         // font style number
      Word:        array of TWord;

      procedure SetX(Value: real);
      procedure SetY(Value: real);
      function GetClientX: real;
      procedure SetAlign(Value: integer);
      function GetSize: real;
      procedure SetSize(Value: real);
      procedure SetSelected(Value: integer);
      procedure SetFontFamily(Value: integer);
      procedure SetFontStyle(Value: integer);
      procedure AddWord(Text: UTF8String);
      procedure Refresh;
    public
      DColR:   real;
      DColG:   real;
      DColB:   real;
      ColR:  real;
      ColG:  real;
      ColB:  real;
      Italic: boolean;

      constructor Create;
      destructor Destroy; override;

      procedure AddLine(CurrentTrack, CurrentLine: integer);

      procedure Clear;
      procedure Draw;
    published
      property X: real write SetX;
      property Y: real write SetY;
      property ClientX: real read GetClientX;
      property Align: integer write SetAlign;
      property Size: real read GetSize write SetSize;
      property Selected: integer read SelectedI write SetSelected;
      property FontFamily: integer write SetFontFamily;
      property FontStyle: integer write SetFontStyle;
  end;

implementation

uses
  TextGL,
  UGraphic,
  UDrawTexture,
  Math,
  USkins;

constructor TEditorLyrics.Create;
begin
  inherited;
end;

destructor TEditorLyrics.Destroy;
begin
  SetLength(Word, 0);
  inherited;
end;

procedure TEditorLyrics.SetX(Value: real);
begin
  XR := Value;
end;

procedure TEditorLyrics.SetY(Value: real);
begin
  YR := Value;
end;

function TEditorLyrics.GetClientX: real;
begin
  Result := Word[0].X;
end;

procedure TEditorLyrics.SetAlign(Value: integer);
begin
  AlignI := Value;
end;

function TEditorLyrics.GetSize: real;
begin
  Result := SizeR;
end;

procedure TEditorLyrics.SetSize(Value: real);
begin
  SizeR := Value;
end;

procedure TEditorLyrics.SetSelected(Value: integer);
begin
  if (-1 < SelectedI) and (SelectedI <= High(Word)) then
  begin
    Word[SelectedI].Selected := false;
    Word[SelectedI].ColR := DColR;
    Word[SelectedI].ColG := DColG;
    Word[SelectedI].ColB := DColB;
  end;

  SelectedI := Value;
  if (-1 < Value) and (Value <= High(Word)) then
  begin
    Word[Value].Selected := true;
    Word[Value].ColR := ColR;
    Word[Value].ColG := ColG;
    Word[Value].ColB := ColB;
  end;

  Refresh;
end;

procedure TEditorLyrics.SetFontFamily(Value: integer);
begin
  FontFamilyI := Value;
end;

procedure TEditorLyrics.SetFontStyle(Value: integer);
begin
  FontStyleI := Value;
end;

procedure TEditorLyrics.AddWord(Text: UTF8String);
var
  WordNum: integer;
begin
  WordNum := Length(Word);
  SetLength(Word, WordNum + 1);
  if WordNum = 0 then
    Word[WordNum].X := XR
  else
    Word[WordNum].X := Word[WordNum - 1].X + Word[WordNum - 1].Width;

  Word[WordNum].Y := YR;
  Word[WordNum].Size := SizeR;
  Word[WordNum].FontStyle := FontStyleI;
  SetFontFamily(FontFamilyI);
  SetFontStyle(FontStyleI);
  SetFontSize(SizeR);
  SetFontItalic(Italic);
  Word[WordNum].Width := glTextWidth(Text);
  Word[WordNum].Text := Text;
  Word[WordNum].ColR := DColR;
  Word[WordNum].ColG := DColG;
  Word[WordNum].ColB := DColB;
  Word[WordNum].Italic := Italic;

  Refresh;
end;

procedure TEditorLyrics.AddLine(CurrentTrack, CurrentLine: integer);
var
  CurrentNote: integer;
begin
  Clear;
  if (Length(Tracks[CurrentTrack].Lines[CurrentLine].Notes) > 0) then
  begin
    for CurrentNote := 0 to Tracks[CurrentTrack].Lines[CurrentLine].HighNote do
    begin
      Italic := Tracks[CurrentTrack].Lines[CurrentLine].Notes[CurrentNote].NoteType = ntFreestyle;
      AddWord(Tracks[CurrentTrack].Lines[CurrentLine].Notes[CurrentNote].Text);
    end;
  {end else
  begin
    Italic := false;
    AddWord(' ', false);
    Text := ' ';}
  end;
  Selected := -1;
end;

procedure TEditorLyrics.Clear;
begin
  SetLength(Word, 0);
  SelectedI := -1;
end;

procedure TEditorLyrics.Refresh;
var
  WordIndex:  integer;
  TotalWidth: real;
begin
  if AlignI = 1 then // center
  begin
    TotalWidth := 0;
    for WordIndex := 0 to High(Word) do
      TotalWidth := TotalWidth + Word[WordIndex].Width;

    Word[0].X := XR - TotalWidth / 2;
    for WordIndex := 1 to High(Word) do
      Word[WordIndex].X := Word[WordIndex - 1].X + Word[WordIndex - 1].Width;
  end;
end;

procedure TEditorLyrics.Draw;
var
  WordIndex: integer;
begin
  for WordIndex := 0 to High(Word) do
  begin
    SetFontFamily(Word[WordIndex].FontFamily);
    SetFontStyle(Word[WordIndex].FontStyle);
    SetFontPos(Word[WordIndex].X + 10*ScreenX, Word[WordIndex].Y);
    SetFontSize(Word[WordIndex].Size);
    SetFontItalic(Word[WordIndex].Italic);
    glColor3f(Word[WordIndex].ColR, Word[WordIndex].ColG, Word[WordIndex].ColB);
    glPrint(Word[WordIndex].Text);
  end;
end;

end.
