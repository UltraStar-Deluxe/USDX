unit UImage;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I ../switches.inc}

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


{$IFDEF HavePNG}
function WritePNGImage(const FileName: string; Surface: PSDL_Surface): boolean;
{$ENDIF}
{$IFDEF HaveBMP}
function WriteBMPImage(const FileName: string; Surface: PSDL_Surface): boolean;
{$ENDIF}
{$IFDEF HaveJPG}
function WriteJPGImage(const FileName: string; Surface: PSDL_Surface; Quality: integer): boolean;
{$ENDIF}

function LoadImage(const Identifier: string): PSDL_Surface;

implementation

uses
  SysUtils,
  Classes,
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
  png_ptr: png_structp;
  info_ptr: png_infop;
  pngFile: TFileStream;
  row: integer;
  rowData: array of png_bytep;
  rowStride: integer;
  converted: boolean;
  colorType: integer;
  //time: png_time;
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

(*
 * Loads an image from the given file or resource
 *)
function LoadImage(const Identifier: string): PSDL_Surface;
var
  TexRWops:  PSDL_RWops;
  TexStream: TStream;
  FileName: string;
begin
  Result   := nil;
  TexRWops := nil;

  if Identifier = '' then
    exit;
    
  //Log.LogStatus( Identifier, 'LoadImage' );

  FileName := Identifier;

  if (FileExistsInsensitive(FileName)) then
  begin
    // load from file
    //Log.LogStatus( 'Is File ( Loading : '+FileName+')', '  LoadImage' );
    try
      Result := IMG_Load(PChar(FileName));
      //Log.LogStatus( '       '+inttostr( integer( Result ) ), '  LoadImage' );
    except
      Log.LogError('Could not load from file "'+FileName+'"', 'TTextureUnit.LoadImage');
      Exit;
    end;
  end
  else
  begin
    //Log.LogStatus( 'IS Resource, because file does not exist.('+Identifier+')', '  LoadImage' );

    TexStream := GetResourceStream(Identifier, 'TEX');
    if (not assigned(TexStream)) then
    begin
      Log.LogError( 'Invalid file or resource "'+ Identifier+'"', 'TTextureUnit.LoadImage');
      Exit;
    end;

    TexRWops := RWopsFromStream(TexStream);
    if (TexRWops = nil) then
    begin
      Log.LogError( 'Could not assign resource "'+Identifier+'"', 'TTextureUnit.LoadImage');
      TexStream.Free();
      Exit;
    end;

    //Log.LogStatus( 'resource Assigned....' , Identifier);
    try
      Result := IMG_Load_RW(TexRWops, 0);
    except
      Log.LogError( 'Could not read resource "'+Identifier+'"', 'TTextureUnit.LoadImage');
    end;

    SDL_FreeRW(TexRWops);
    TexStream.Free();
  end;
end;

end.
