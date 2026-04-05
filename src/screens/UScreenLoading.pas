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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenLoading.pas $
 * $Id: UScreenLoading.pas 1939 2009-11-09 00:27:55Z s_alexander $
 *}

unit UScreenLoading;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  SysUtils,
  UThemes,
  dglOpenGL;

type
  TScreenLoading = class(TMenu)
    private
      ProgressBarIndex: integer;   // Index of the progress bar static element
      ProgressBarMaxW: real;       // Maximum width of the progress bar
      TextProgress: integer;       // Index of progress count text
    public
      Fadeout: boolean;
      TextDescription: integer;
      TextStatus: integer;

      constructor Create; override;
      procedure OnShow; override;
      function ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure SetProgress(Current, Total: integer);  // Update progress bar and text
  end;

implementation

uses
  UGraphic,
  UDisplay,
  UTime;

function TScreenLoading.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
end;

constructor TScreenLoading.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.Loading);

  // Configure progress bar: use the white bar (Static[1]) as progress indicator
  if Length(Statics) >= 2 then
  begin
    // Hide the colored bar (Static[0])
    Statics[0].Visible := false;

    // Use the white bar (Static[1]) as progress bar
    ProgressBarIndex := 1;
    ProgressBarMaxW := 800;
    Statics[1].Texture.X := 0;
    Statics[1].Texture.W := 0;    // Start empty
  end
  else if Length(Statics) > 0 then
  begin
    // Fallback: just use first static as progress bar
    ProgressBarIndex := 0;
    ProgressBarMaxW := 800;
    Statics[0].Texture.X := 0;
    Statics[0].Texture.W := 0;
  end
  else
  begin
    ProgressBarIndex := -1;
    ProgressBarMaxW := 0;
  end;

  // Make "Loading..." text black and position at left with small margin
  if Length(Text) > 0 then
  begin
    Text[0].X := 20;        // Left side with 20px margin
    Text[0].Align := 0;     // Left-aligned (0=left, 1=center, 2=right)
    Text[0].ColR := 0;      // Black color
    Text[0].ColG := 0;
    Text[0].ColB := 0;
  end;

  // Add progress count text next to "Loading..." (left-aligned, same Y, same style)
  if Length(Text) > 0 then
    TextProgress := AddText(130, Text[0].Y, 0, 0, 24, 0, 0, 0, '')
  else
    TextProgress := AddText(130, 550, 0, 0, 24, 0, 0, 0, '');

  // Left-align the progress text
  if (TextProgress >= 0) and (TextProgress < Length(Text)) then
    Text[TextProgress].Align := 0;  // Left-aligned

  Fadeout := false;
end;

procedure TScreenLoading.OnShow;
begin
  inherited;
end;

procedure TScreenLoading.SetProgress(Current, Total: integer);
var
  Progress: real;
begin
  // Calculate progress percentage
  if Total > 0 then
    Progress := Current / Total
  else
    Progress := 0;

  // Clamp progress to 0-1 range
  if Progress < 0 then Progress := 0;
  if Progress > 1 then Progress := 1;

  // Update progress bar width
  if (ProgressBarIndex >= 0) and (ProgressBarIndex < Length(Statics)) then
    Statics[ProgressBarIndex].Texture.W := ProgressBarMaxW * Progress;

  // Update progress text
  if (TextProgress >= 0) and (TextProgress < Length(Text)) then
    Text[TextProgress].Text := Format('%d / %d', [Current, Total]);

  // Force a display update to show the progress
  Self.Draw;
  Display.Draw;
  SwapBuffers;
end;

end.
