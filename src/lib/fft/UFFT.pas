{**********************************************************************

  FFT.cpp

  Dominic Mazzoni

  September 2000

***********************************************************************

Fast Fourier Transform routines.

  This file contains a few FFT routines, including a real-FFT
  routine that is almost twice as fast as a normal complex FFT,
  and a power spectrum routine when you know you don't care
  about phase information.

  Some of this code was based on a free implementation of an FFT
  by Don Cross, available on the web at:

    http://www.intersrv.com/~dcross/fft.html

  The basic algorithm for his code was based on Numerican Recipes
  in Fortran.  I optimized his code further by reducing array
  accesses, caching the bit reversal table, and eliminating
  float-to-double conversions, and I added the routines to
  calculate a real FFT and a real power spectrum.

***********************************************************************

  Salvo Ventura - November 2006
  Added more window functions:
    * 4: Blackman
    * 5: Blackman-Harris
    * 6: Welch
    * 7: Gaussian(a=2.5)
    * 8: Gaussian(a=3.5)
    * 9: Gaussian(a=4.5)

***********************************************************************

  This file is part of Audacity 1.3.4 beta (http://audacity.sourceforge.net/)
  Ported to Pascal by the UltraStar Deluxe Team
}

unit UFFT;

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+} // Use long strings
{$ENDIF}

interface
type
  TSingleArray = array[0 .. (MaxInt div SizeOf(Single))-1] of Single;
  PSingleArray = ^TSingleArray;

  TFFTWindowFunc = (
    fwfRectangular,
    fwfBartlett,
    fwfHamming,
    fwfHanning,
    fwfBlackman,
    fwfBlackman_Harris,
    fwfWelch,
    fwfGaussian2_5,
    fwfGaussian3_5,
    fwfGaussian4_5
  );
  
const
  FFTWindowName: array[TFFTWindowFunc] of string = (
     'Rectangular',
     'Bartlett',
     'Hamming',
     'Hanning',
     'Blackman',
     'Blackman-Harris',
     'Welch',
     'Gaussian(a=2.5)',
     'Gaussian(a=3.5)',
     'Gaussian(a=4.5)'
  );

(*
 * This is the function you will use the most often.
 * Given an array of floats, this will compute the power
 * spectrum by doing a Real FFT and then computing the
 * sum of the squares of the real and imaginary parts.
 * Note that the output array is half the length of the
 * input array, and that NumSamples must be a power of two.
 *)
procedure PowerSpectrum(NumSamples: Integer; In_, Out_: PSingleArray);

(*
 * Computes an FFT when the input data is real but you still
 * want complex data as output.  The output arrays are half
 * the length of the input, and NumSamples must be a power of
 * two.
 *)
procedure RealFFT(NumSamples: integer;
                  RealIn, RealOut, ImagOut: PSingleArray);

(*
 * Computes a FFT of complex input and returns complex output.
 * Currently this is the only function here that supports the
 * inverse transform as well.
 *)
procedure FFT(NumSamples: Integer;
              InverseTransform: boolean;
              RealIn, ImagIn, RealOut, ImagOut: PSingleArray);

(*
 * Applies a windowing function to the data in place
 *
 * 0: Rectangular (no window)
 * 1: Bartlett    (triangular)
 * 2: Hamming
 * 3: Hanning
 * 4: Blackman
 * 5: Blackman-Harris
 * 6: Welch
 * 7: Gaussian(a=2.5)
 * 8: Gaussian(a=3.5)
 * 9: Gaussian(a=4.5)
 *)
procedure WindowFunc(whichFunction: TFFTWindowFunc; NumSamples: Integer; in_: PSingleArray);

(*
 * Returns the name of the windowing function (for UI display)
 *)
function WindowFuncName(whichFunction: TFFTWindowFunc): string;

(*
 * Returns the number of windowing functions supported
 *)
function NumWindowFuncs(): integer;


implementation

uses
  SysUtils;

type TIntArray = array[0 .. (MaxInt div SizeOf(Integer))-1] of Integer;
type PIntArray = ^TIntArray;
type TIntIntArray = array[0 .. (MaxInt div SizeOf(PIntArray))-1] of PIntArray;
type PIntIntArray = ^TIntIntArray;
var gFFTBitTable: PIntIntArray;
const MaxFastBits: Integer = 16;

function IsPowerOfTwo(x: Integer): Boolean;
begin
   if (x < 2) then
      result := false
   else if ((x and (x - 1)) <> 0) then  { Thanks to 'byang' for this cute trick! }
      result := false
   else
    result := true;
end;

function NumberOfBitsNeeded(PowerOfTwo: Integer): Integer;
var i: Integer;
begin
  if (PowerOfTwo < 2) then begin
    Writeln(ErrOutput, Format('Error: FFT called with size %d\n', [PowerOfTwo]));
    Abort;
  end;

  i := 0;
  while(true) do begin
    if (PowerOfTwo and (1 shl i) <> 0) then begin
      result := i;
      Exit;
    end;
    Inc(i);
  end;
end;

function ReverseBits(index, NumBits: Integer): Integer;
var
  i, rev: Integer;
begin
  rev := 0;
  for i := 0 to NumBits-1 do begin
    rev := (rev shl 1) or (index and 1);
    index := index shr 1;
  end;

  result := rev;
end;

procedure InitFFT();
var
  len: Integer;
  b, i: Integer;
begin
  GetMem(gFFTBitTable, MaxFastBits * sizeof(PSingle));

  len := 2;
  for b := 1 to MaxFastBits do begin
    GetMem(gFFTBitTable[b - 1], len * sizeof(Single));
    for i := 0 to len-1 do
      gFFTBitTable[b - 1][i] := ReverseBits(i, b);
      len := len shl 1;
   end;
end;

function FastReverseBits(i, NumBits: Integer): Integer; {$IFDEF HasInline}inline;{$ENDIF}
begin
  if (NumBits <= MaxFastBits) then
    result := gFFTBitTable[NumBits - 1][i]
  else
    result := ReverseBits(i, NumBits);
end;

{*
 * Complex Fast Fourier Transform
 *}
procedure FFT(NumSamples: Integer;
         InverseTransform: boolean;
         RealIn, ImagIn, RealOut, ImagOut: PSingleArray);
var
  NumBits: Integer;                 { Number of bits needed to store indices }
  i, j, k, n: Integer;
  BlockSize, BlockEnd: Integer;
  delta_angle: Double;
  angle_numerator: Double;
  tr, ti: Double;                   { temp real, temp imaginary }
  sm2, sm1, cm2, cm1: Double;
  w: Double;
  ar0, ar1, ar2, ai0, ai1, ai2: Double;
  denom: Single;
begin

   angle_numerator := 2.0 * Pi;

   if (not IsPowerOfTwo(NumSamples)) then begin
      Writeln(ErrOutput, Format('%d is not a power of two', [NumSamples]));
      Abort;
   end;

   if (gFFTBitTable = nil) then
      InitFFT();

   if (InverseTransform) then
      angle_numerator := -angle_numerator;

   NumBits := NumberOfBitsNeeded(NumSamples);

   {
    **   Do simultaneous data copy and bit-reversal ordering into outputs...
   }

   for i := 0 to NumSamples-1 do begin
      j := FastReverseBits(i, NumBits);
      RealOut[j] := RealIn[i];
      if(ImagIn = nil) then
        ImagOut[j] := 0.0
      else
        ImagOut[j] := ImagIn[i];
   end;

   {
    **   Do the FFT itself...
   }

   BlockEnd := 1;
   BlockSize := 2;
   while(BlockSize <= NumSamples) do
   begin

      delta_angle := angle_numerator / BlockSize;

      sm2 := sin(-2 * delta_angle);
      sm1 := sin(-delta_angle);
      cm2 := cos(-2 * delta_angle);
      cm1 := cos(-delta_angle);
      w := 2 * cm1;

      i := 0;
      while(i < NumSamples) do
      begin
         ar2 := cm2;
         ar1 := cm1;

         ai2 := sm2;
         ai1 := sm1;

         j := i;
         for n := 0 to BlockEnd-1 do
         begin
            ar0 := w * ar1 - ar2;
            ar2 := ar1;
            ar1 := ar0;

            ai0 := w * ai1 - ai2;
            ai2 := ai1;
            ai1 := ai0;

            k := j + BlockEnd;
            tr := ar0 * RealOut[k] - ai0 * ImagOut[k];
            ti := ar0 * ImagOut[k] + ai0 * RealOut[k];

            RealOut[k] := RealOut[j] - tr;
            ImagOut[k] := ImagOut[j] - ti;

            RealOut[j] := RealOut[j] + tr;
            ImagOut[j] := ImagOut[j] + ti;

            Inc(j);
         end;

         Inc(i, BlockSize);
      end;

      BlockEnd := BlockSize;
      BlockSize := BlockSize shl 1;
   end;

   {
      **   Need to normalize if inverse transform...
   }

   if (InverseTransform) then begin
      denom := NumSamples;

      for i := 0 to NumSamples-1 do begin
         RealOut[i] := RealOut[i] / denom;
         ImagOut[i] := ImagOut[i] / denom;
      end;
   end;
end;

(*
 * Real Fast Fourier Transform
 *
 * This function was based on the code in Numerical Recipes in C.
 * In Num. Rec., the inner loop is based on a single 1-based array
 * of interleaved real and imaginary numbers.  Because we have two
 * separate zero-based arrays, our indices are quite different.
 * Here is the correspondence between Num. Rec. indices and our indices:
 *
 * i1  <->  real[i]
 * i2  <->  imag[i]
 * i3  <->  real[n/2-i]
 * i4  <->  imag[n/2-i]
 *)
procedure RealFFT(NumSamples: integer; RealIn, RealOut, ImagOut: PSingleArray);
var
  Half: Integer;
  i: Integer;
  theta: Single;
  tmpReal, tmpImag: PSingleArray;
  wtemp: Single;
  wpr, wpi, wr, wi: Single;
  i3: Integer;
  h1r, h1i, h2r, h2i: Single;
begin
   Half := NumSamples div 2;

   theta := Pi / Half;

   GetMem(tmpReal, Half * sizeof(Single));
   GetMem(tmpImag, Half * sizeof(Single));

   for i := 0 to Half-1 do
   begin
      tmpReal[i] := RealIn[2 * i];
      tmpImag[i] := RealIn[2 * i + 1];
   end;

   FFT(Half, false, tmpReal, tmpImag, RealOut, ImagOut);

   wtemp := sin(0.5 * theta);

   wpr := -2.0 * wtemp * wtemp;
   wpi := sin(theta);
   wr := 1.0 + wpr;
   wi := wpi;

   for i := 1 to (Half div 2)-1 do
   begin
      i3 := Half - i;

      h1r := 0.5 * (RealOut[i] + RealOut[i3]);
      h1i := 0.5 * (ImagOut[i] - ImagOut[i3]);
      h2r := 0.5 * (ImagOut[i] + ImagOut[i3]);
      h2i := -0.5 * (RealOut[i] - RealOut[i3]);

      RealOut[i] := h1r + wr * h2r - wi * h2i;
      ImagOut[i] := h1i + wr * h2i + wi * h2r;
      RealOut[i3] := h1r - wr * h2r + wi * h2i;
      ImagOut[i3] := -h1i + wr * h2i + wi * h2r;

      wtemp := wr;
      wr := wtemp * wpr - wi * wpi + wr;
      wi := wi * wpr + wtemp * wpi + wi;
   end;

   h1r := RealOut[0];
   RealOut[0] := h1r + ImagOut[0];
   ImagOut[0] := h1r - ImagOut[0];

   FreeMem(tmpReal);
   FreeMem(tmpImag);
end;

{*
 * PowerSpectrum
 *
 * This function computes the same as RealFFT, above, but
 * adds the squares of the real and imaginary part of each
 * coefficient, extracting the power and throwing away the
 * phase.
 *
 * For speed, it does not call RealFFT, but duplicates some
 * of its code.
 *}
procedure PowerSpectrum(NumSamples: Integer; In_, Out_: PSingleArray);
var
  Half: Integer;
  i: Integer;
  theta: Single;
  tmpReal, tmpImag, RealOut, ImagOut: PSingleArray;
  wtemp: Single;
  wpr, wpi, wr, wi: Single;
  i3: Integer;
  h1r, h1i, h2r, h2i, rt, it: Single;
begin
   Half := NumSamples div 2;

   theta := Pi / Half;

   GetMem(tmpReal, Half * sizeof(Single));
   GetMem(tmpImag, Half * sizeof(Single));
   GetMem(RealOut, Half * sizeof(Single));
   GetMem(ImagOut, Half * sizeof(Single));

   for i := 0 to Half-1 do begin
      tmpReal[i] := In_[2 * i];
      tmpImag[i] := In_[2 * i + 1];
   end;

   FFT(Half, false, tmpReal, tmpImag, RealOut, ImagOut);

   wtemp := sin(0.5 * theta);

   wpr := -2.0 * wtemp * wtemp;
   wpi := sin(theta);
   wr := 1.0 + wpr;
   wi := wpi;

   for i := 1 to (Half div 2)-1 do
   begin
      i3 := Half - i;

      h1r := 0.5 * (RealOut[i] + RealOut[i3]);
      h1i := 0.5 * (ImagOut[i] - ImagOut[i3]);
      h2r := 0.5 * (ImagOut[i] + ImagOut[i3]);
      h2i := -0.5 * (RealOut[i] - RealOut[i3]);

      rt := h1r + wr * h2r - wi * h2i;
      it := h1i + wr * h2i + wi * h2r;

      Out_[i] := rt * rt + it * it;

      rt := h1r - wr * h2r + wi * h2i;
      it := -h1i + wr * h2i + wi * h2r;

      Out_[i3] := rt * rt + it * it;

      wtemp := wr;
      wr := wtemp * wpr - wi * wpi + wr;
      wi := wi * wpr + wtemp * wpi + wi;
   end;

   h1r := RealOut[0];
   rt := h1r + ImagOut[0];
   it := h1r - ImagOut[0];
   Out_[0] := rt * rt + it * it;

   rt := RealOut[Half div 2];
   it := ImagOut[Half div 2];
   Out_[Half div 2] := rt * rt + it * it;

   FreeMem(tmpReal);
   FreeMem(tmpImag);
   FreeMem(RealOut);
   FreeMem(ImagOut);
end;

(*
 * Windowing Functions
 *)
function NumWindowFuncs(): integer;
begin
  Result := Length(FFTWindowName);
end;

function WindowFuncName(whichFunction: TFFTWindowFunc): string;
begin
  Result := FFTWindowName[whichFunction];
end;

procedure WindowFunc(whichFunction: TFFTWindowFunc; NumSamples: Integer; in_: PSingleArray);
var
  i: Integer;
  A: Single;
begin
  case whichFunction of
    fwfBartlett:
    begin
      // Bartlett (triangular) window
      for i := 0 to (NumSamples div 2)-1 do
      begin
        in_[i] := in_[i] * (i / (NumSamples / 2));
        in_[i + (NumSamples div 2)] :=
            in_[i + (NumSamples div 2)] *
            (1.0 - (i / (NumSamples / 2)));
      end;
    end;
    fwfHamming:
    begin
      // Hamming
      for i := 0 to NumSamples-1 do
      begin
        in_[i] := in_[i] * (0.54 - 0.46 * cos(2 * Pi * i / (NumSamples - 1)));
      end;
    end;
    fwfHanning:
    begin
      // Hanning
      for i := 0 to NumSamples-1 do
      begin
        in_[i] := in_[i] * (0.50 - 0.50 * cos(2 * Pi * i / (NumSamples - 1)));
      end;
    end;
    fwfBlackman:
    begin
      // Blackman
      for i := 0 to NumSamples-1 do
      begin
        in_[i] := in_[i] * (0.42 - 0.5 * cos (2 * Pi * i / (NumSamples - 1)) + 0.08 * cos (4 * Pi * i / (NumSamples - 1)));
      end;
    end;
    fwfBlackman_Harris:
    begin
      // Blackman-Harris
      for i := 0 to NumSamples-1 do
      begin
        in_[i] := in_[i] * (0.35875 - 0.48829 * cos(2 * Pi * i /(NumSamples-1)) + 0.14128 * cos(4 * Pi * i/(NumSamples-1)) - 0.01168 * cos(6 * Pi * i/(NumSamples-1)));
      end;
    end;
    fwfWelch:
    begin
      // Welch
      for i := 0 to NumSamples-1 do
      begin
        in_[i] := in_[i] * 4*i/NumSamples*(1-(i/NumSamples));
      end;
    end;
    fwfGaussian2_5:
    begin
      // Gaussian (a=2.5)
      // Precalculate some values, and simplify the fmla to try and reduce overhead
      A := -2*2.5*2.5;

      for i := 0 to NumSamples-1 do
      begin
        // full
        // in_[i] := in_[i] * exp(-0.5*(A*((i-NumSamples/2)/NumSamples/2))*(A*((i-NumSamples/2)/NumSamples/2)));
        // reduced
        //in_[i] := in_[i] * exp(A*(0.25 + ((i/NumSamples)*(i/NumSamples)) - (i/NumSamples)));
      end;
    end;
    fwfGaussian3_5:
    begin
      // Gaussian (a=3.5)
      A := -2*3.5*3.5;

      for i := 0 to NumSamples-1 do
      begin
        // reduced
        in_[i] := in_[i] * exp(A*(0.25 + ((i/NumSamples)*(i/NumSamples)) - (i/NumSamples)));
      end;
    end;
    fwfGaussian4_5:
    begin
      // Gaussian (a=4.5)
      A := -2*4.5*4.5;

      for i := 0 to NumSamples-1 do
      begin
        // reduced
        in_[i] := in_[i] * exp(A*(0.25 + ((i/NumSamples)*(i/NumSamples)) - (i/NumSamples)));
      end;
    end;
  end;
end;

end.
