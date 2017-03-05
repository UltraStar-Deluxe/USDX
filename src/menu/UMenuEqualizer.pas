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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/menu/UMenuEqualizer.pas $
 * $Id: UMenuEqualizer.pas 1692 2009-04-24 18:43:12Z k-m_schindler $
 *}

unit UMenuEqualizer;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMusic,
  UCommon,
  UThemes;

type
  //----------------
  //Tms_Equalizer
  //Class displaying an equalizer (Songscreen)
  //----------------
  Tms_Equalizer = class(TObject)
    private
      FFTData: TFFTData; // moved here to avoid stack overflows
      BandData: array of byte;
      RefreshTime: cardinal;

      Source: IAudioPlayback;

      procedure Analyse;
    public
      X: integer;
      Y: integer;
      Z: real;

      W: integer;
      H: integer;
      Space: integer;

      Visible: boolean;
      Alpha:   real;
      Color:   TRGB;

      Direction:  boolean;
      BandLength: integer;

      Reflection:        boolean;
      Reflectionspacing: real;


      constructor Create(Source: IAudioPlayback; mySkin: TThemeEqualizer);

      procedure Draw;
      procedure SetBands(Value: byte);
      function  GetBands: byte;
      property  Bands: byte read GetBands write SetBands;
      procedure SetSource(newSource: IAudioPlayback);
  end;

implementation
uses
  math,
  SDL,
  gl,
  glext;

constructor Tms_Equalizer.Create(Source: IAudioPlayback; mySkin: TThemeEqualizer);
var
  I: integer;
begin
  if (Source <> nil) then
  begin
    X          := mySkin.X;
    Y          := mySkin.Y;
    W          := mySkin.W;
    H          := mySkin.H;
    Z          := mySkin.Z;

    Space      := mySkin.Space;

    Visible    := mySkin.Visible;
    Alpha      := mySkin.Alpha;
    Color.R    := mySkin.ColR;
    Color.G    := mySkin.ColG;
    Color.B    := mySkin.ColB;

    Direction  := mySkin.Direction;
    Bands      := mySkin.Bands;
    BandLength := mySkin.Length;

    Reflection := mySkin.Reflection;
    Reflectionspacing := mySkin.Reflectionspacing;

    Self.Source := Source;


    //Check if Visible
    if (Bands  <= 0)  or
       (BandLength <= 0)  or
       (W      <= 0)  or
       (H      <= 0)  or
       (Alpha  <= 0)  then
      Visible := false;

    //ClearArray
    for I := low(BandData) to high(BandData) do
      BandData[I] := 3;
  end
  else
    Visible := false;
end;

//--------
// evaluate FFT-Data
//--------
procedure Tms_Equalizer.Analyse;
var
  I:            integer;
  ChansPerBand: byte;        // channels per band
  MaxChannel:   integer;
  Pos:          real;
  CurBand:      integer;
begin
  Source.GetFFTData(FFTData);

  Pos := 0;
  // use only the first approx. 92 of 256 FFT-channels (approx. up to 8kHz
  ChansPerBand := ceil(92 / Bands); // How much channels are used for one Band
  MaxChannel := ChansPerBand * Bands - 1;

  // Change Lengths
  for i := 0 to MaxChannel do
  begin
    // Gain higher freq. data so that the bars are visible
    if i > 35 then
      FFTData[i] := FFTData[i] * 8
    else if i > 11 then
      FFTData[i] := FFTData[i] * 4.5
    else
      FFTData[i] := FFTData[i] * 1.1;

    // clamp data
    if (FFTData[i] > 1) then
      FFTData[i] := 1;

    // Get max. pos
    if (FFTData[i] * BandLength > Pos) then
      Pos := FFTData[i] * BandLength;

    // Check if this is the last channel in the band
    if ((i+1) mod ChansPerBand = 0) then
    begin
      CurBand := i div ChansPerBand;

      // Smooth delay if new equalizer is lower than the old one
      if ((BandData[CurBand] > Pos) and (BandData[CurBand] > 1)) then
        BandData[CurBand] := BandData[CurBand] - 1
      else
        BandData[CurBand] := Round(Pos);

      Pos := 0;
    end;
  end;
end;

//--------
// Draw SpectrumAnalyser, Call Analyse
//--------
procedure Tms_Equalizer.Draw;
var
  CurTime:    cardinal;
  PosX, PosY: real;
  I, J:       integer;
  Diff:       real;

  function GetAlpha(Diff: single): single;
  begin
    if Direction then
      Result := (Alpha * 0.6) * (0.5 - Diff/(BandLength * (H + Space)))
    else
      Result := (Alpha * 0.6) * (0.5 - Diff/(Bands      * (H + Space)));
  end;

begin
  if (Visible) and not (AudioPlayback.Finished) then
  begin
    //Call Analyse if necessary
    CurTime := SDL_GetTicks();
    if (CurTime > RefreshTime) then
    begin
      Analyse;

      RefreshTime := CurTime + 44;
    end;

    //Draw Equalizer Bands
    // Setup OpenGL
    glColorRGB(Color, Alpha);
    glDisable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);

    // Set position of the first equalizer bar
    PosY := Y;
    PosX := X;

    // Draw bars for each band
    for I := 0 to High(BandData) do
    begin
      // Reset to lower or left position depending on the drawing-direction
      if Direction then // Vertical bars
        // FIXME: Is Y the upper or lower coordinate?
        PosY := Y //+ (H + Space) * BandLength
      else                                   // Horizontal bars
        PosX := X;

      // Draw the bar as a stack of blocks
      for J := 1 to BandData[I] do
      begin
        // Draw block
        glBegin(GL_QUADS);
          glVertex3f(PosX, PosY, Z);
          glVertex3f(PosX, PosY+H, Z);
          glVertex3f(PosX+W, PosY+H, Z);
          glVertex3f(PosX+W, PosY, Z);
        glEnd;

        if (Reflection) and (J <= BandLength div 2) then
        begin
          Diff := (Y-PosY) + H;

          //Draw Reflection
          if Direction then
          begin
            glBegin(GL_QUADS);
              glColorRGB(Color, GetAlpha(Diff));
              glVertex3f(PosX, Diff + Y + ReflectionSpacing, Z);

              //bottom v
              glColorRGB(Color, GetAlpha(Diff + H));
              glVertex3f(PosX, Diff + Y+H + ReflectionSpacing, Z);
              glVertex3f(PosX+W, Diff + Y+H + ReflectionSpacing, Z);

              glColorRGB(Color, GetAlpha(Diff));
              glVertex3f(PosX+W, Diff + Y + ReflectionSpacing, Z);
            glEnd;
          end
          else
          begin
            glBegin(GL_QUADS);
              glColorRGB(Color, GetAlpha(Diff));
              glVertex3f(PosX, Diff + Y + (H + Space)*Bands + ReflectionSpacing, Z);
              glVertex3f(PosX, Diff + Y+H  + (H + Space)*Bands + ReflectionSpacing, Z);
              glVertex3f(PosX+W, Diff + Y+H  + (H + Space)*Bands + ReflectionSpacing, Z);
              glVertex3f(PosX+W, Diff + Y + (H + Space)*Bands + ReflectionSpacing, Z);
              glColorRGB(Color, GetAlpha(Diff + H));
            glEnd;
          end;

          glColorRGB(Color, Alpha);
        end;


        // Calc position of the bar's next block
        if Direction then // Vertical bars
          PosY := PosY - H - Space
        else                                   // Horizontal bars
          PosX := PosX + W + Space;
      end;

      // Calc position of the next bar
      if Direction then // Vertical bars
        PosX := PosX + W + Space
      else                                   // Horizontal bars
        PosY := PosY + H + Space;
    end;


  end;
end;

procedure Tms_Equalizer.SetBands(Value: byte);
begin
  SetLength(BandData, Value);
end;

function  Tms_Equalizer.GetBands: byte;
begin
  Result := Length(BandData);
end;

procedure Tms_Equalizer.SetSource(newSource: IAudioPlayback);
begin
  if (newSource <> nil) then
    Source := newSource;
end;

end.
