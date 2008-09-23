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

unit UImage;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SDL;

{$DEFINE HavePNG}
{$DEFINE HaveBMP}
{$DEFINE HaveJPG}

const
  PixelFmt_RGBA: TSDL_Pixelformat = (
    palette:      nil;
    BitsPerPixel:  32;
    BytesPerPixel:  4;
    Rloss:          0;
    Gloss:          0;
    Bloss:          0;
    Aloss:          0;
    Rshift:         0;
    Gshift:         8;
    Bshift:        16;
    Ashift:        24;
    Rmask:  $000000ff;
    Gmask:  $0000ff00;
    Bmask:  $00ff0000;
    Amask:  $ff000000;
    ColorKey:       0;
    Alpha:        255
  );

  PixelFmt_RGB: TSDL_Pixelformat = (
    palette:      nil;
    BitsPerPixel:  24;
    BytesPerPixel:  3;
    Rloss:          0;
    Gloss:          0;
    Bloss:          0;
    Aloss:          0;
    Rshift:         0;
    Gshift:         8;
    Bshift:        16;
    Ashift:         0;
    Rmask:  $000000ff;
    Gmask:  $0000ff00;
    Bmask:  $00ff0000;
    Amask:  $00000000;
    ColorKey:       0;
    Alpha:        255
  );

  PixelFmt_BGRA: TSDL_Pixelformat = (
    palette:      nil;
    BitsPerPixel:  32;
    BytesPerPixel:  4;
    Rloss:          0;
    Gloss:          0;
    Bloss:          0;
    Aloss:          0;
    Rshift:        16;
    Gshift:         8;
    Bshift:         0;
    Ashift:        24;
    Rmask:  $00ff0000;
    Gmask:  $0000ff00;
    Bmask:  $000000ff;
    Amask:  $ff000000;
    ColorKey:       0;
    Alpha:        255
  );

  PixelFmt_BGR: TSDL_Pixelformat = (
    palette:      nil;
    BitsPerPixel:  24;
    BytesPerPixel:  3;
    Rloss:          0;
    Gloss:          0;
    Bloss:          0;
    Aloss:          0;
    Rshift:        16;
    Gshift:         8;
    Bshift:         0;
    Ashift:         0;
    Rmask:  $00ff0000;
    Gmask:  $0000ff00;
    Bmask:  $000000ff;
    Amask:  $00000000;
    ColorKey:       0;
    Alpha:        255
  );

type
  TImagePixelFmt = (
    ipfRGBA, ipfRGB, ipfBGRA, ipfBGR
  );

(*******************************************************
 * Image saving
 *******************************************************)

{$IFDEF HavePNG}
function WritePNGImage(const FileName: string; Surface: PSDL_Surface): boolean;
{$ENDIF}
{$IFDEF HaveBMP}
function WriteBMPImage(const FileName: string; Surface: PSDL_Surface): boolean;
{$ENDIF}
{$IFDEF HaveJPG}
function WriteJPGImage(const FileName: string; Surface: PSDL_Surface; Quality: integer): boolean;
{$ENDIF}

(*******************************************************
 * Image loading
 *******************************************************)

function LoadImage(const Filename: string): PSDL_Surface;

(*******************************************************
 * Image manipulation
 *******************************************************)

function PixelFormatEquals(fmt1, fmt2: PSDL_PixelFormat): boolean;
procedure ScaleImage(var ImgSurface: PSDL_Surface; Width, Height: Cardinal);
procedure FitImage(var ImgSurface: PSDL_Surface; Width, Height: Cardinal);
procedure ColorizeImage(ImgSurface: PSDL_Surface; NewColor: Cardinal);


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
  {$IFDEF HavePNG}
  png,
  {$ENDIF}
  zlib,
  sdl_image,
  sdlutils,
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
      Result := SDL_ConvertSurface(Surface, @PixelFmt_BGRA, SDL_SWSURFACE)
    else
      Result := SDL_ConvertSurface(Surface, @PixelFmt_BGR, SDL_SWSURFACE);
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
      Result := SDL_ConvertSurface(Surface, @PixelFmt_RGBA, SDL_SWSURFACE)
    else
      Result := SDL_ConvertSurface(Surface, @PixelFmt_RGB, SDL_SWSURFACE);
    Converted := true;
  end;
end;


(*******************************************************
 * Image saving
 *******************************************************)

(***************************
 * PNG section
 *****************************)

{$IFDEF HavePNG}

// delphi does not support setjmp()/longjmp() -> define our own error-handler
procedure user_error_fn(png_ptr: png_structp; error_msg: png_const_charp); cdecl;
begin
  raise Exception.Create(error_msg);
end;

procedure user_read_data(png_ptr: png_structp; data: png_bytep; length: png_size_t); cdecl;
var
  inFile: TFileStream;
begin
  inFile := TFileStream(png_get_io_ptr(png_ptr));
  inFile.Read(data^, length);
end;

procedure user_write_data(png_ptr: png_structp; data: png_bytep; length: png_size_t); cdecl;
var
  outFile: TFileStream;
begin
  outFile := TFileStream(png_get_io_ptr(png_ptr));
  outFile.Write(data^, length);
end;

procedure user_flush_data(png_ptr: png_structp); cdecl;
//var
//  outFile: TFileStream;
begin
  // binary files are flushed automatically, Flush() works with Text-files only
  //outFile := TFileStream(png_get_io_ptr(png_ptr));
  //outFile.Flush();
end;

procedure DateTimeToPngTime(time: TDateTime; var pngTime: png_time);
var
  year, month, day: word;
  hour, minute, second, msecond: word;
begin
  DecodeDate(time, year, month, day);
  pngTime.year  := year;
  pngTime.month := month;
  pngTime.day   := day;
  DecodeTime(time, hour, minute, second, msecond);
  pngTime.hour   := hour;
  pngTime.minute := minute;
  pngTime.second := second;
end;

(*
 * ImageData must be in RGB-format
 *)
function WritePNGImage(const FileName: string; Surface: PSDL_Surface): boolean;
var
  png_ptr:   png_structp;
  info_ptr:  png_infop;
  pngFile:   TFileStream;
  row:       integer;
  rowData:   array of png_bytep;
//  rowStride: integer;
  converted: boolean;
  colorType: integer;
//  time:      png_time;
begin
  Result := false;

  // open file for writing
  try
    pngFile := TFileStream.Create(FileName, fmCreate);
  except
    Log.LogError('Could not open file: "' + FileName + '"', 'WritePngImage');
    Exit;
  end;

  // only 24bit (RGB) or 32bit (RGBA) data is supported, so convert to it
  Surface := ConvertToRGB_RGBASurface(Surface, converted);

  png_ptr := nil;

  try
    // initialize png (and enable a user-defined error-handler that throws an exception on error)
    png_ptr := png_create_write_struct(PNG_LIBPNG_VER_STRING, nil, @user_error_fn, nil);
    // the error-handler is called if png_create_write_struct() fails, so png_ptr should always be <> nil
    if (png_ptr = nil) then
    begin
      Log.LogError('png_create_write_struct() failed', 'WritePngImage');
      if (converted) then
        SDL_FreeSurface(Surface);
      Exit;
    end;

    info_ptr := png_create_info_struct(png_ptr);

    if (Surface^.format^.BitsPerPixel = 24) then
      colorType := PNG_COLOR_TYPE_RGB
    else
      colorType := PNG_COLOR_TYPE_RGBA;

    // define write IO-functions (POSIX-style FILE-pointers are not available in Delphi)
    png_set_write_fn(png_ptr, pngFile, @user_write_data, @user_flush_data);
    png_set_IHDR(
      png_ptr, info_ptr,
      Surface.w, Surface.h,
      8,
      colorType,
      PNG_INTERLACE_NONE,
      PNG_COMPRESSION_TYPE_DEFAULT,
      PNG_FILTER_TYPE_DEFAULT
    );

    // TODO: do we need the modification time?
    //DateTimeToPngTime(Now, time);
    //png_set_tIME(png_ptr, info_ptr, @time);

    if (SDL_MUSTLOCK(Surface)) then
      SDL_LockSurface(Surface);

    // setup data
    SetLength(rowData, Surface.h);
    for row := 0 to Surface.h-1 do
    begin
      // set rowData-elements to beginning of each image row
      // Note: the byte-count of a row is pitch (which is not width*bitsPerPixel if the image is aligned)
      rowData[row] := @PChar(Surface.pixels)[(Surface.h-row-1) * Surface.pitch];
    end;

    if (SDL_MUSTLOCK(Surface)) then
      SDL_UnlockSurface(Surface);

    png_write_info(png_ptr, info_ptr);
    png_write_image(png_ptr, png_bytepp(rowData));
    png_write_end(png_ptr, nil);

    Result := true;
  except on E: Exception do
    Log.LogError(E.message, 'WritePngImage');
  end;

  // free row-data
  SetLength(rowData, 0);

  // free png-resources
  if (png_ptr <> nil) then
    png_destroy_write_struct(@png_ptr, nil);

  if (converted) then
    SDL_FreeSurface(Surface);

  // close file
  pngFile.Free;
end;

{$ENDIF}

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
function WriteBMPImage(const FileName: string; Surface: PSDL_Surface): boolean;
var
  bmpFile:    TFileStream;
  FileInfo:   BITMAPINFOHEADER;
  FileHeader: BITMAPFILEHEADER;
  Converted:  boolean;
  Row:        integer;
  RowSize:    integer;
begin
  Result := false;

  // open file for writing
  try
    bmpFile := TFileStream.Create(FileName, fmCreate);
  except
    Log.LogError('Could not open file: "' + FileName + '"', 'WriteBMPImage');
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
    Log.LogError('Could not write file: "' + FileName + '"', 'WriteBMPImage');
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

function WriteJPGImage(const FileName: string; Surface: PSDL_Surface; Quality: integer): boolean;
var
  {$IFDEF Delphi}
  Bitmap:    TBitmap;
  BitmapInfo: TBitmapInfo;
  Jpeg:      TJpegImage;
  row:       integer;
  {$ELSE}
  cinfo:     jpeg_compress_struct;
  jerr :     jpeg_error_mgr;
  jpgFile:   TFileStream;
  rowPtr:    array[0..0] of JSAMPROW;
  {$ENDIF}
  converted: boolean;
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
    Jpeg := TJPEGImage.Create;
    Jpeg.Assign(Bitmap);
    Bitmap.Free;
    Jpeg.CompressionQuality := Quality;
    try
      // compress image (don't forget this line, otherwise it won't be compressed)
      Jpeg.Compress();
      Jpeg.SaveToFile(FileName);
    except
      Log.LogError('Could not save file: "' + FileName + '"', 'WriteJPGImage');
      Exit;
    end;
    Jpeg.Free;
  {$ELSE}
    // based on example.pas in FPC's packages/base/pasjpeg directory

    // only 24bit (RGB) data is supported, so convert to it
    if (IsRGBSurface(Surface.format)) then
      converted := false
    else
    begin
      Surface := SDL_ConvertSurface(Surface, @PixelFmt_RGB, SDL_SWSURFACE);
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
      jpgFile := TFileStream.Create(FileName, fmCreate);
    except
      Log.LogError('Could not open file: "' + FileName + '"', 'WriteJPGImage');
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
function LoadImage(const Filename: string): PSDL_Surface;
var
  FilenameFound: string;
begin
  Result   := nil;

  // FileExistsInsensitive() requires a var-arg
  FilenameFound := Filename;

  // try to find the file case insensitive
  if (not FileExistsInsensitive(FilenameFound)) then
  begin
    Log.LogError('Image-File does not exist "'+FilenameFound+'"', 'LoadImage');
    Exit;
  end;

  // load from file
  try
    Result := IMG_Load(PChar(FilenameFound));
  except
    Log.LogError('Could not load from file "'+FilenameFound+'"', 'LoadImage');
    Exit;
  end;
end;


(*******************************************************
 * Image manipulation
 *******************************************************)

 
function PixelFormatEquals(fmt1, fmt2: PSDL_PixelFormat): boolean;
begin
  if (fmt1^.BitsPerPixel = fmt2^.BitsPerPixel) and
     (fmt1^.BytesPerPixel = fmt2^.BytesPerPixel) and
     (fmt1^.Rloss = fmt2^.Rloss) and (fmt1^.Gloss = fmt2^.Gloss) and
     (fmt1^.Bloss = fmt2^.Bloss) and (fmt1^.Rmask = fmt2^.Rmask) and
     (fmt1^.Gmask = fmt2^.Gmask) and (fmt1^.Bmask = fmt2^.Bmask) and
     (fmt1^.Rshift = fmt2^.Rshift) and (fmt1^.Gshift = fmt2^.Gshift) and
     (fmt1^.Bshift = fmt2^.Bshift)
  then
    Result := true
  else
    Result := false;
end;

procedure ScaleImage(var ImgSurface: PSDL_Surface; Width, Height: Cardinal);
var
  TempSurface: PSDL_Surface;
begin
  TempSurface := ImgSurface;
  ImgSurface := SDL_ScaleSurfaceRect(TempSurface,
                  0, 0, TempSurface^.W,TempSurface^.H,
                  Width, Height);
  SDL_FreeSurface(TempSurface);
end;

procedure FitImage(var ImgSurface: PSDL_Surface; Width, Height: Cardinal);
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
  SDL_SetAlpha(ImgSurface, 0, 255);
  SDL_SetAlpha(TempSurface, 0, 255);
  SDL_BlitSurface(TempSurface, nil, ImgSurface, nil);

  SDL_FreeSurface(TempSurface);
end;

(*
// Old slow floating point version of ColorizeTexture.
// For an easier understanding of the faster fixed point version below.
procedure ColorizeTexture(TexSurface: PSDL_Surface; Col: Cardinal);
var
  clr: array[0..2] of Double; // [0: R, 1: G, 2: B]
  hsv: array[0..2] of Double; // [0: H(ue), 1: S(aturation), 2: V(alue)]
  delta, f, p, q, t: Double;
  max: Double;
begin
  clr[0] := PixelColors[0]/255;
  clr[1] := PixelColors[1]/255;
  clr[2] := PixelColors[2]/255;
  max := maxvalue(clr);
  delta := max - minvalue(clr);

  hsv[0] := DestinationHue; // set H(ue)
  hsv[2] := max; // set V(alue)
  // calc S(aturation)
  if (max = 0.0) then
    hsv[1] := 0.0
  else
    hsv[1] := delta/max;

  //ColorizePixel(PByteArray(Pixel), DestinationHue);
  h_int := trunc(hsv[0]);             // h_int = |_h_|
  f := hsv[0]-h_int;                  // f = h-h_int
  p := hsv[2]*(1.0-hsv[1]);           // p = v*(1-s)
  q := hsv[2]*(1.0-(hsv[1]*f));       // q = v*(1-s*f)
  t := hsv[2]*(1.0-(hsv[1]*(1.0-f))); // t = v*(1-s*(1-f))
  case h_int of
    0: begin clr[0] := hsv[2]; clr[1] := t;      clr[2] := p;      end; // (v,t,p)
    1: begin clr[0] := q;      clr[1] := hsv[2]; clr[2] := p;      end; // (q,v,p)
    2: begin clr[0] := p;      clr[1] := hsv[2]; clr[2] := t;      end; // (p,v,t)
    3: begin clr[0] := p;      clr[1] := q;      clr[2] := hsv[2]; end; // (p,q,v)
    4: begin clr[0] := t;      clr[1] := p;      clr[2] := hsv[2]; end; // (t,p,v)
    5: begin clr[0] := hsv[2]; clr[1] := p;      clr[2] := q;      end; // (v,p,q)
  end;

  // and store new rgb back into the image
  PixelColors[0] := trunc(255*clr[0]);
  PixelColors[1] := trunc(255*clr[1]);
  PixelColors[2] := trunc(255*clr[2]);
end;
*)

procedure ColorizeImage(ImgSurface: PSDL_Surface; NewColor: Cardinal);

  //returns hue within range [0.0-6.0)
  function col2hue(Color:Cardinal): double;
  var
    clr: array[0..2] of double;
    hue, max, delta: double;
  begin
    clr[0] := ((Color and $ff0000) shr 16)/255; // R
    clr[1] := ((Color and   $ff00) shr  8)/255; // G
    clr[2] :=  (Color and     $ff)        /255; // B
    max := maxvalue(clr);
    delta := max - minvalue(clr);
    // calc hue
    if (delta = 0.0) then       hue := 0
    else if (clr[0] = max) then hue :=     (clr[1]-clr[2])/delta
    else if (clr[1] = max) then hue := 2.0+(clr[2]-clr[0])/delta
    else if (clr[2] = max) then hue := 4.0+(clr[0]-clr[1])/delta;
    if (hue < 0.0) then
      hue := hue + 6.0;
    Result := hue;
  end;

var
  DestinationHue: Double;
  PixelIndex: Cardinal;
  Pixel: PByte;
  PixelColors: PByteArray;
  clr: array[0..2] of UInt32; // [0: R, 1: G, 2: B]
  hsv: array[0..2] of UInt32; // [0: H(ue), 1: S(aturation), 2: V(alue)]
  dhue: UInt32;
  h_int: Cardinal;
  delta, f, p, q, t: Longint;
  max: Uint32;
begin
  DestinationHue := col2hue(NewColor);

  dhue := Trunc(DestinationHue*1024);

  Pixel := ImgSurface^.Pixels;

  for PixelIndex := 0 to (ImgSurface^.W * ImgSurface^.H)-1 do
  begin
    PixelColors := PByteArray(Pixel);
    // inlined colorize per pixel

    // uses fixed point math
    // get color values
    clr[0] := PixelColors[0] shl 10;
    clr[1] := PixelColors[1] shl 10;
    clr[2] := PixelColors[2] shl 10;
    //calculate luminance and saturation from rgb

    max := clr[0];
    if clr[1] > max then max := clr[1];
    if clr[2] > max then max := clr[2];
    delta := clr[0];
    if clr[1] < delta then delta := clr[1];
    if clr[2] < delta then delta := clr[2];
    delta := max-delta;
    hsv[0] := dhue;  // shl 8
    hsv[2] := max;  // shl 8
    if (max = 0) then
      hsv[1] := 0
    else
      hsv[1] := (delta shl 10) div max; // shl 8
    h_int := hsv[0] and $fffffC00;
    f := hsv[0]-h_int; //shl 10
    p := (hsv[2]*(1024-hsv[1])) shr 10;
    q := (hsv[2]*(1024-(hsv[1]*f) shr 10)) shr 10;
    t := (hsv[2]*(1024-(hsv[1]*(1024-f)) shr 10)) shr 10;
    h_int := h_int shr 10;
    case h_int of
      0: begin clr[0] := hsv[2]; clr[1] := t;      clr[2] := p;      end; // (v,t,p)
      1: begin clr[0] := q;      clr[1] := hsv[2]; clr[2] := p;      end; // (q,v,p)
      2: begin clr[0] := p;      clr[1] := hsv[2]; clr[2] := t;      end; // (p,v,t)
      3: begin clr[0] := p;      clr[1] := q;      clr[2] := hsv[2]; end; // (p,q,v)
      4: begin clr[0] := t;      clr[1] := p;      clr[2] := hsv[2]; end; // (t,p,v)
      5: begin clr[0] := hsv[2]; clr[1] := p;      clr[2] := q;      end; // (v,p,q)
    end;

    PixelColors[0] := clr[0] shr 10;
    PixelColors[1] := clr[1] shr 10;
    PixelColors[2] := clr[2] shr 10;

    Inc(Pixel, ImgSurface^.format.BytesPerPixel);
  end;
end;

end.
