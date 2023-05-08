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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UImage.pas $
 * $Id: UImage.pas 1939 2009-11-09 00:27:55Z s_alexander $
 *}

unit UImage;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  sdl2,
  UPath;

{$DEFINE HaveBMP}
{$DEFINE HaveJPG}
//const
  //PixelFmt_RGBA
  //PixelFmt_RGB
  //PixelFmt_BGRA
  //PixelFmt_BGR

type
  TImagePixelFmt = (
    ipfRGBA, ipfRGB, ipfBGRA, ipfBGR
  );

(*******************************************************
 * Image saving
 *******************************************************)
{$IFDEF HaveBMP}
function WriteBMPImage(const FileName: IPath; Surface: PSDL_Surface): boolean;
{$ENDIF}
{$IFDEF HaveJPG}
function WriteJPGImage(const FileName: IPath; Surface: PSDL_Surface; Quality: integer): boolean;
{$ENDIF}

(*******************************************************
 * Image loading
 *******************************************************)

function LoadImage(const Filename: IPath): PSDL_Surface;

(*******************************************************
 * Image manipulation
 *******************************************************)

function PixelFormatEquals(fmt1, fmt2: PSDL_PixelFormat): boolean;
procedure ScaleImage(var ImgSurface: PSDL_Surface; Width, Height: cardinal);
procedure FitImage(var ImgSurface: PSDL_Surface; Width, Height: cardinal);
procedure ColorizeImage(ImgSurface: PSDL_Surface; NewColor: cardinal);

implementation

uses
  SysUtils,
  Classes,
  Math,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF HaveJPG}
    {$IFDEF Delphi}
    Graphics,
    jpeg,
    {$ELSE}
    jpeglib,
    jerror,
    jcparam,
    jdatadst, jcapimin, jcapistd,
    {$ENDIF}
  {$ENDIF}
  zlib,
  SDL2_image,
  UCommon,
  ULog;

function IsRGBSurface(pixelFmt: PSDL_PixelFormat): boolean;
begin
  Result := (pixelFmt.BitsPerPixel = 24) and
            (pixelFmt.RMask = $0000FF)   and
            (pixelFmt.GMask = $00FF00)   and
            (pixelFmt.BMask = $FF0000);
end;

function IsRGBASurface(pixelFmt: PSDL_PixelFormat): boolean;
begin
  Result := (pixelFmt.BitsPerPixel = 32) and
            (pixelFmt.RMask = $000000FF) and
            (pixelFmt.GMask = $0000FF00) and
            (pixelFmt.BMask = $00FF0000) and
            (pixelFmt.AMask = $FF000000);
end;

function IsBGRSurface(pixelFmt: PSDL_PixelFormat): boolean;
begin
  Result := (pixelFmt.BitsPerPixel = 24) and
            (pixelFmt.BMask = $0000FF)   and
            (pixelFmt.GMask = $00FF00)   and
            (pixelFmt.RMask = $FF0000);
end;

function IsBGRASurface(pixelFmt: PSDL_PixelFormat): boolean;
begin
  Result := (pixelFmt.BitsPerPixel = 32) and
            (pixelFmt.BMask = $000000FF) and
            (pixelFmt.GMask = $0000FF00) and
            (pixelFmt.RMask = $00FF0000) and
            (pixelFmt.AMask = $FF000000);
end;

// Converts alpha-formats to BGRA, non-alpha to BGR, and leaves BGR(A) as is
// sets converted to true if the surface needed to be converted
function ConvertToBGR_BGRASurface(Surface: PSDL_Surface; out Converted: boolean): PSDL_Surface;
var
  pixelFmt: PSDL_PixelFormat;
begin
  pixelFmt := Surface.format;
  if (IsBGRSurface(pixelFmt) or IsBGRASurface(pixelFmt)) then
  begin
    Converted := false;
    Result := Surface;
  end
  else
  begin
    // invalid format -> needs conversion
    if (pixelFmt.AMask <> 0) then
      Result := SDL_ConvertSurfaceFormat(Surface, SDL_PIXELFORMAT_BGRA8888, 0)
    else
      Result := SDL_ConvertSurfaceFormat(Surface, SDL_PIXELFORMAT_BGR24, 0);
    Converted := true;
  end;
end;

// Converts alpha-formats to RGBA, non-alpha to RGB, and leaves RGB(A) as is
// sets converted to true if the surface needed to be converted
function ConvertToRGB_RGBASurface(Surface: PSDL_Surface; out Converted: boolean): PSDL_Surface;
var
  pixelFmt: PSDL_PixelFormat;
begin
  pixelFmt := Surface.format;
  if (IsRGBSurface(pixelFmt) or IsRGBASurface(pixelFmt)) then
  begin
    Converted := false;
    Result := Surface;
  end
  else
  begin
    // invalid format -> needs conversion
    if (pixelFmt.AMask <> 0) then
      Result := SDL_ConvertSurfaceFormat(Surface, SDL_PIXELFORMAT_ARGB8888, SDL_SWSURFACE)
    else
      Result := SDL_ConvertSurfaceFormat(Surface, SDL_PIXELFORMAT_RGB24, SDL_SWSURFACE);
    Converted := true;
  end;
end;

(*******************************************************
 * Image saving
 *******************************************************)

(***************************
 * BMP section
 *****************************)

{$IFDEF HaveBMP}

{$IFNDEF MSWINDOWS}
const
  (* constants for the biCompression field *)
  BI_RGB       = 0;
  BI_RLE8      = 1;
  BI_RLE4      = 2;
  BI_BITFIELDS = 3;
  BI_JPEG      = 4;
  BI_PNG       = 5;

type
  BITMAPINFOHEADER = record
    biSize:          longword;
    biWidth:         longint;
    biHeight:        longint;
    biPlanes:        word;
    biBitCount:      word;
    biCompression:   longword;
    biSizeImage:     longword;
    biXPelsPerMeter: longint;
    biYPelsPerMeter: longint;
    biClrUsed:       longword;
    biClrImportant:  longword;
  end;
  LPBITMAPINFOHEADER = ^BITMAPINFOHEADER;
  TBITMAPINFOHEADER  = BITMAPINFOHEADER;
  PBITMAPINFOHEADER  = ^BITMAPINFOHEADER;

  RGBTRIPLE = record
    rgbtBlue:  byte;
    rgbtGreen: byte;
    rgbtRed:   byte;
  end;
  tagRGBTRIPLE = RGBTRIPLE;
  TRGBTRIPLE = RGBTRIPLE;
  PRGBTRIPLE = ^RGBTRIPLE;

  RGBQUAD = record
    rgbBlue:     byte;
    rgbGreen:    byte;
    rgbRed:      byte;
    rgbReserved: byte;
  end;
  tagRGBQUAD = RGBQUAD;
  TRGBQUAD = RGBQUAD;
  PRGBQUAD = ^RGBQUAD;

  BITMAPINFO = record
    bmiHeader: BITMAPINFOHEADER;
    bmiColors: array[0..0] of RGBQUAD;
  end;
  LPBITMAPINFO = ^BITMAPINFO;
  PBITMAPINFO = ^BITMAPINFO;
  TBITMAPINFO = BITMAPINFO;

  {$PACKRECORDS 2}
  BITMAPFILEHEADER = record
    bfType:      word;
    bfSize:      longword;
    bfReserved1: word;
    bfReserved2: word;
    bfOffBits:   longword;
  end;
  {$PACKRECORDS DEFAULT}
{$ENDIF}

(*
 * ImageData must be in BGR-format
 *)
function WriteBMPImage(const FileName: IPath; Surface: PSDL_Surface): boolean;
var
  bmpFile:    TStream;
  FileInfo:   BITMAPINFOHEADER;
  FileHeader: BITMAPFILEHEADER;
  Converted:  boolean;
  Row:        integer;
  RowSize:    integer;
begin
  Result := false;

  // open file for writing
  try
    bmpFile := TBinaryFileStream.Create(FileName, fmCreate);
  except
    Log.LogError('Could not open file: "' + FileName.ToNative + '"', 'WriteBMPImage');
    Exit;
  end;

  // only 24bit (BGR) or 32bit (BGRA) data is supported, so convert to it
  Surface := ConvertToBGR_BGRASurface(Surface, Converted);

  // aligned (4-byte) row-size in bytes
  RowSize := ((Surface.w * Surface.format.BytesPerPixel + 3) div 4) * 4;

  // initialize bitmap info
  FillChar(FileInfo, SizeOf(BITMAPINFOHEADER), 0);
  with FileInfo do
  begin
    biSize := SizeOf(BITMAPINFOHEADER);
    biWidth := Surface.w;
    biHeight := Surface.h;
    biPlanes := 1;
    biBitCount := Surface^.format^.BitsPerPixel;
    biCompression := BI_RGB;
    biSizeImage := RowSize * Surface.h;
  end;

  // initialize header-data
  FillChar(FileHeader, SizeOf(BITMAPFILEHEADER), 0);
  with FileHeader do
  begin
    bfType := $4D42; // = 'BM'
    bfOffBits := SizeOf(BITMAPFILEHEADER) + SizeOf(BITMAPINFOHEADER);
    bfSize := bfOffBits + FileInfo.biSizeImage;
  end;

  // and move the whole stuff into the file ;-)
  try
    // write headers
    bmpFile.Write(FileHeader, SizeOf(BITMAPFILEHEADER));
    bmpFile.Write(FileInfo,   SizeOf(BITMAPINFOHEADER));

    // write image-data

    if (SDL_MUSTLOCK(Surface)) then
      SDL_LockSurface(Surface);

    // BMP needs 4-byte alignment
    if (Surface.pitch mod 4 = 0) then
    begin
      // aligned correctly -> write whole image at once
      bmpFile.Write(Surface.pixels^, FileInfo.biSizeImage);
    end
    else
    begin
      // misaligned -> write each line separately
      // Note: for the last line unassigned memory (> last Surface.pixels element)
      //   will be copied to the padding area (last bytes of a row),
      //   but we do not care because the content of padding data is ignored anyhow.
      for Row := 0 to Surface.h do
        bmpFile.Write(PChar(Surface.pixels)[Row * Surface.pitch], RowSize);
    end;

    if (SDL_MUSTLOCK(Surface)) then
      SDL_UnlockSurface(Surface);

    Result := true;
  finally
    Log.LogError('Could not write file: "' + FileName.ToNative + '"', 'WriteBMPImage');
  end;

  if (Converted) then
    SDL_FreeSurface(Surface);

  // close file
  bmpFile.Free;
end;

{$ENDIF}

(***************************
 * JPG section
 *****************************)

{$IFDEF HaveJPG}

function WriteJPGImage(const FileName: IPath; Surface: PSDL_Surface; Quality: integer): boolean;
var
  {$IFDEF Delphi}
  Bitmap:     TBitmap;
  BitmapInfo: TBitmapInfo;
  Jpeg:       TJpegImage;
  row:        integer;
  FileStream: TBinaryFileStream;
  {$ELSE}
  cinfo:     jpeg_compress_struct;
  jerr :     jpeg_error_mgr;
  jpgFile:   TBinaryFileStream;
  rowPtr:    array[0..0] of JSAMPROW;
  {$ENDIF}
  converted:  boolean;
begin
  Result := false;

  {$IFDEF Delphi}
    // only 24bit (BGR) data is supported, so convert to it
    if (IsBGRSurface(Surface.format)) then
      converted := false
    else
    begin
      Surface := SDL_ConvertSurface(Surface, @PixelFmt_BGR, SDL_SWSURFACE);
      converted := true;
    end;

    // create and setup bitmap
    Bitmap := TBitmap.Create;
    Bitmap.PixelFormat := pf24bit;
    Bitmap.Width  := Surface.w;
    Bitmap.Height := Surface.h;

    // setup bitmap info on source image (Surface parameter)
    ZeroMemory(@BitmapInfo, SizeOf(BitmapInfo));
    with BitmapInfo.bmiHeader do
    begin
      biSize := SizeOf(BITMAPINFOHEADER);
      biWidth  := Surface.w;
      biHeight := Surface.h;
      biPlanes := 1;
      biBitCount := 24;
      biCompression := BI_RGB;
    end;

    if (SDL_MUSTLOCK(Surface)) then
      SDL_LockSurface(Surface);

    // use fast Win32-API functions to copy data instead of Bitmap.Canvas.Pixels
    if (Surface.pitch mod 4 = 0) then
    begin
      // if the image is aligned (to a 4-byte boundary) -> copy all data at once
      // Note: surfaces created with SDL (e.g. with SDL_ConvertSurface) are aligned
      SetDIBits(0, Bitmap.Handle, 0, Bitmap.Height, Surface.pixels, BitmapInfo, DIB_RGB_COLORS);
    end
    else
    begin
      // wrong alignment -> copy each line separately.
      // Note: for the last line unassigned memory (> last Surface.pixels element)
      //   will be copied to the padding area (last bytes of a row),
      //   but we do not care because the content of padding data is ignored anyhow.
      for row := 0 to Surface.h do
      begin
        SetDIBits(0, Bitmap.Handle, row, 1, @PChar(Surface.pixels)[row * Surface.pitch],
          BitmapInfo, DIB_RGB_COLORS);
      end;
    end;

    if (SDL_MUSTLOCK(Surface)) then
      SDL_UnlockSurface(Surface);

    // assign Bitmap to JPEG and store the latter
    try
      // init with nil so Free() will not fail if an exception occurs
      Jpeg := nil;
      Bitmap := nil;
      FileStream := nil;

      try
        Jpeg := TJPEGImage.Create;
        Jpeg.Assign(Bitmap);

        // compress image (don't forget this line, otherwise it won't be compressed)
        Jpeg.CompressionQuality := Quality;
        Jpeg.Compress();

        // Note: FileStream needed for unicode filename support
        FileStream := TBinaryFileStream.Create(Filename, fmCreate);
        Jpeg.SaveToStream(FileStream);
      finally
        FileStream.Free;
        Bitmap.Free;
        Jpeg.Free;
      end;
    except
      Log.LogError('Could not save file: "' + FileName.ToNative + '"', 'WriteJPGImage');
      Exit;
    end;
  {$ELSE}
    // based on example.pas in FPC's packages/base/pasjpeg directory

    // only 24bit (RGB) data is supported, so convert to it
    if (IsRGBSurface(Surface.format)) then
      converted := false
    else
    begin
      Surface := SDL_ConvertSurfaceFormat(Surface, SDL_PIXELFORMAT_RGB24, SDL_SWSURFACE);
      converted := true;
    end;

    // allocate and initialize JPEG compression object
    cinfo.err := jpeg_std_error(jerr);
    // msg_level that will be displayed. (Nomssi)
    //jerr.trace_level := 3;
    // initialize the JPEG compression object
    jpeg_create_compress(@cinfo);

    // open file for writing
    try
      jpgFile := TBinaryFileStream.Create(FileName, fmCreate);
    except
      Log.LogError('Could not open file: "' + FileName.ToNative + '"', 'WriteJPGImage');
      Exit;
    end;

    // specify data destination
    jpeg_stdio_dest(@cinfo, @jpgFile);

    // set parameters for compression
    cinfo.image_width := Surface.w;
    cinfo.image_height := Surface.h;
    cinfo.in_color_space := JCS_RGB;
    cinfo.input_components := 3;
    cinfo.data_precision := 8;

    // set default compression parameters
    jpeg_set_defaults(@cinfo);
    jpeg_set_quality(@cinfo, quality, true);

    // start compressor
    jpeg_start_compress(@cinfo, true);

    if (SDL_MUSTLOCK(Surface)) then
      SDL_LockSurface(Surface);

    while (cinfo.next_scanline < cinfo.image_height) do
    begin
      // Note: the byte-count of a row is pitch (which is not width*bitsPerPixel if the image is aligned)
      rowPtr[0] := JSAMPROW(@PChar(Surface.pixels)[(Surface.h-cinfo.next_scanline-1) * Surface.pitch]);
      jpeg_write_scanlines(@cinfo, JSAMPARRAY(@rowPtr), 1);
    end;

    if (SDL_MUSTLOCK(Surface)) then
      SDL_UnlockSurface(Surface);

    // finish compression
    jpeg_finish_compress(@cinfo);
    // close the output file
    jpgFile.Free;

    // release JPEG compression object
    jpeg_destroy_compress(@cinfo);
  {$ENDIF}

  if (converted) then
    SDL_FreeSurface(Surface);

  Result := true;
end;

{$ENDIF}

(*******************************************************
 * Image loading
 *******************************************************)

(*
 * Loads an image from the given file
 *)
function LoadImage(const Filename: IPath): PSDL_Surface;
var
  FilenameCaseAdj: IPath;
begin
  Result := nil;

  // try to adjust filename's case and check if it exists
  FilenameCaseAdj := Filename.AdjustCase(false);
  if (not FilenameCaseAdj.IsFile) then
  begin
    Log.LogError('Image-File does not exist "' + FilenameCaseAdj.ToNative + '"', 'LoadImage');
    Exit;
  end;

  // load from file
  try
    Result := IMG_Load(PChar(FilenameCaseAdj.ToUTF8())); //SDL2 uses wants UTF-8 strings according to doocumentation
    // Note: TBinaryFileStream is freed by SDLStream. SDLStream by IMG_Load_RW().
  except
    Log.LogError('Could not load from file "' + FilenameCaseAdj.ToNative + '"', 'LoadImage');
    Exit;
  end;
end;

(*******************************************************
 * Image manipulation
 *******************************************************)

function PixelFormatEquals(fmt1, fmt2: PSDL_PixelFormat): boolean;
begin
  Result := 
    (fmt1^.BitsPerPixel  = fmt2^.BitsPerPixel)  and
    (fmt1^.BytesPerPixel = fmt2^.BytesPerPixel) and
    (fmt1^.Rloss = fmt2^.Rloss)   and (fmt1^.Gloss = fmt2^.Gloss)   and (fmt1^.Bloss = fmt2^.Bloss)   and
    (fmt1^.Rmask = fmt2^.Rmask)   and (fmt1^.Gmask = fmt2^.Gmask)   and (fmt1^.Bmask = fmt2^.Bmask)   and
    (fmt1^.Rshift = fmt2^.Rshift) and (fmt1^.Gshift = fmt2^.Gshift) and (fmt1^.Bshift = fmt2^.Bshift)
  ;
end;

procedure ScaleImage(var ImgSurface: PSDL_Surface; Width, Height: cardinal);
var
   newstretchRect: PSDL_Rect;
   origRect: PSDL_Rect;
begin
  origRect := new(PSDL_Rect);
  origRect.x:=0;
  origRect.y:=0;
  origRect.w:=ImgSurface.w;
  origRect.h:=ImgSurface.h;
  newstretchRect := new(PSDL_Rect);
  newstretchRect.x := 0;
  newstretchRect.y := 0;
  newstretchRect.w := Width;
  newstretchRect.h := Height;
  SDL_UpperBlitScaled( ImgSurface, origRect, ImgSurface, newstretchRect );
end;

procedure FitImage(var ImgSurface: PSDL_Surface; Width, Height: cardinal);
var
  TempSurface: PSDL_Surface;
  ImgFmt: PSDL_PixelFormat;
begin
  TempSurface := ImgSurface;

  // create a new surface with given width and height
  ImgFmt := TempSurface^.format;
  ImgSurface := SDL_CreateRGBSurface(
    SDL_SWSURFACE, Width, Height, ImgFmt^.BitsPerPixel,
    ImgFmt^.RMask, ImgFmt^.GMask, ImgFmt^.BMask, ImgFmt^.AMask);

  // copy image from temp- to new surface
  SDL_SetSurfaceBlendMode(TempSurface, SDL_BLENDMODE_NONE);
  SDL_BlitSurface(TempSurface, nil, ImgSurface, nil);

  SDL_FreeSurface(TempSurface);
end;

procedure ColorizeImage(ImgSurface: PSDL_Surface; NewColor: longword);

  // First, the rgb colors are converted to hsv, second hue is replaced by
  // the NewColor, saturation and value remain unchanged, finally this
  // hsv color is converted back to rgb space.
  // For the conversion algorithms of colors from rgb to hsv space
  // and back simply check the wikipedia.
  // In order to speed up starting time of USDX the division of reals is 
  // replaced by division of longints, shifted by 10 bits to keep 
  // digits.

  // The use of longwards leeds to some type size mismatch warnings
  // whenever differences are formed.
  // This should not be a problem, since the results should all be positive.
  // replacing longword by longint would probably resolve this cosmetic fault :-)

  function ColorToHue(const Color: longword): longword;
  // returns hue within the range [0.0-6.0] but shl 10, ie. times 1024
  var
    Red, Green, Blue: longint;
    Min, Max, Delta:  longint;
    Hue: double;
  begin
    // extract the colors
    // division by 255 is omitted, since it is implicitly done
    // when deviding by delta
    Red   := ((Color and $ff0000) shr 16); // R
    Green := ((Color and   $ff00) shr  8); // G
    Blue  :=  (Color and     $ff)        ; // B

    Min := Red;
    if Green < Min then Min := Green;
    if Blue  < Min then Min := Blue;

    Max := Red;
    if Green > Max then Max := Green;
    if Blue  > Max then Max := Blue;

    // calc hue
    Delta := Max - Min;     // This gives a type size mismatch warning, because Delta is longword, ie. >= 0
                            // But the assignments above are easy enough to be sure, that Max - Min is >= 0.
    if (Delta = 0) then
      Result := 0
    else
    begin
      // The division by Delta is done separately afterwards.
      // Necessary because Delphi did not do the type conversion from
      // longword to double as expected.
      // After the change to longint, we may not need it, but left for now
      // Something to check
      if      (Max = Red  ) then Hue :=             Green - Blue
      else if (Max = Green) then Hue := 2.0*Delta + Blue  - Red
      else if (Max = Blue ) then Hue := 4.0*Delta + Red   - Green;
      Hue := Hue / Delta;
      if (Hue < 0.0) then
        Hue := Hue + 6.0;
      Result := trunc(Hue*1024);           // '*1024' is shl 10
 //     if NewColor = $000000 then
 //       Log.LogError ('Hue: ' +  FloatToStr(Hue), 'ColorToHue');
    end;
  end;

var
  PixelIndex: longword;
  Pixel: PByte;
  PixelColors: PByteArray;
  Red, Green, Blue: longword;
  Hue, Sat: longword;
  Min, Max, Delta: longword;
  HueInteger: longword;
  f, p, q, t: longword;
  GreyReal: real;
  Grey: byte;
begin

  SDL_LockSurface(ImgSurface);
  Pixel := ImgSurface^.Pixels;

  if not assigned(Pixel) then
  begin
    Log.LogError('Failed to lock surface', 'ColorizeImage');
    SDL_UnlockSurface(ImgSurface);
    Exit;
  end;

  // check of the size of a pixel in bytes.
  // It should be always 4, but this
  // additional safeguard will show,
  // whether something went wrong up to here.

  if ImgSurface^.format.BytesPerPixel <> 4 then
  begin
    Log.LogError ('ColorizeImage: The pixel size should be 4, but it is '
                   + IntToStr(ImgSurface^.format.BytesPerPixel));
  end;

  // Check whether the new color is white, grey or black, 
  // because a greyscale must be created in a different
  // way.

  Red   := ((NewColor and $ff0000) shr 16); // R
  Green := ((NewColor and   $ff00) shr  8); // G
  Blue  :=  (NewColor and     $ff)        ; // B
  
  if (Red = Green) and (Green = Blue) then // greyscale image
  begin
    // According to these recommendations (ITU-R BT.709-5)
    // the conversion parameters for rgb to greyscale are
    // 0.299, 0.587, 0.114
    for PixelIndex := 0 to (ImgSurface^.W * ImgSurface^.H)-1 do
    begin
      PixelColors := PByteArray(Pixel);
      GreyReal := 0.299*PixelColors[0] + 0.587*PixelColors[1] + 0.114*PixelColors[2];
      //       PixelColors[3] is alpha and remains untouched
      Grey := round(GreyReal);
      PixelColors[0] := Grey;
      PixelColors[1] := Grey;
      PixelColors[2] := Grey;
      //       PixelColors[3] is alpha and remains untouched
      Inc(Pixel, ImgSurface^.format.BytesPerPixel);
    end;
    exit; // we are done with a greyscale image.
  end;

  Hue := ColorToHue(NewColor);   // Hue is shl 10
  f   := Hue and $3ff;           // f is the dezimal part of hue
  HueInteger := Hue shr 10;

  for PixelIndex := 0 to (ImgSurface^.W * ImgSurface^.H)-1 do
  begin
    PixelColors := PByteArray(Pixel);
    // inlined colorize per pixel

    // uses fixed point math
    // shl 10 is used for divisions

    // get color values

    Red   := PixelColors[0];
    Green := PixelColors[1];
    Blue  := PixelColors[2];
    //       PixelColors[3] is alpha and remains untouched

    //calculate luminance and saturation from rgb

    Max := Red;
    if Green > Max then Max := Green;
    if Blue  > Max then Max := Blue ;

    if (Max = 0) then               // the color is black
    begin
      PixelColors[0] := 0;
      PixelColors[1] := 0;
      PixelColors[2] := 0;
    end
    else
    begin
      Min := Red;
      if Green < Min then Min := Green;
      if Blue  < Min then Min := Blue ;

      if (Min = 255) then           // the color is white
      begin
        PixelColors[0] := 255;
        PixelColors[1] := 255;
        PixelColors[2] := 255;
      end
      else                          // all colors except black and white
      begin
        Delta := Max - Min;         // This gives a type size mismatch warning, because Delta is longword, ie. >= 0
                                    // But the assignments above are easy enough to be sure, that Max - Min is >= 0.
        Sat := (Delta shl 10) div Max;  // shl 10

        // shr 10 corrects that Sat and f are shl 10
        // the resulting p, q and t are unshifted

        p := (Max * (1024 -  Sat                     )) shr 10;
        q := (Max * (1024 - (Sat *  f        ) shr 10)) shr 10;
        t := (Max * (1024 - (Sat * (1024 - f)) shr 10)) shr 10;

        // The above 3 lines give type size mismatch warning, but all variables are longword and the ranges should be ok.

        case HueInteger of
          0: begin Red := Max; Green := t;   Blue := p;   end; // (v,t,p)
          1: begin Red := q;   Green := Max; Blue := p;   end; // (q,v,p)
          2: begin Red := p;   Green := Max; Blue := t;   end; // (p,v,t)
          3: begin Red := p;   Green := q;   Blue := Max; end; // (p,q,v)
          4: begin Red := t;   Green := p;   Blue := Max; end; // (t,p,v)
          5: begin Red := Max; Green := p;   Blue := q;   end; // (v,p,q)
        end;

        PixelColors[0] := byte(Red);
        PixelColors[1] := byte(Green);
        PixelColors[2] := byte(Blue);

      end;
    end;

    Inc(Pixel, ImgSurface^.format.BytesPerPixel);
  end;
  SDL_UnlockSurface(ImgSurface);
end;

end.
