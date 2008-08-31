unit Windows;

{$I switches.inc}

interface

uses
  Types;

const
  opengl32 = 'OpenGL';
  MAX_PATH = 260;

type

  DWORD = Types.DWORD;
  {$EXTERNALSYM DWORD}
  BOOL = LongBool;
  {$EXTERNALSYM BOOL}
  PBOOL = ^BOOL;
  {$EXTERNALSYM PBOOL}
  PByte = Types.PByte;
  PINT = ^Integer;
  {$EXTERNALSYM PINT}
  PSingle = ^Single;
  PWORD = ^Word;
  {$EXTERNALSYM PWORD}
  PDWORD = ^DWORD;
  {$EXTERNALSYM PDWORD}
  LPDWORD = PDWORD;
  {$EXTERNALSYM LPDWORD}
  HDC = type LongWord;
  {$EXTERNALSYM HDC}
  HGLRC = type LongWord;
  {$EXTERNALSYM HGLRC}
  TLargeInteger = Int64;
  HFONT = type LongWord;
  {$EXTERNALSYM HFONT}
  HWND = type LongWord;
  {$EXTERNALSYM HWND}

  PPaletteEntry = ^TPaletteEntry;
  {$EXTERNALSYM tagPALETTEENTRY}
  tagPALETTEENTRY = packed record
    peRed: Byte;
    peGreen: Byte;
    peBlue: Byte;
    peFlags: Byte;
  end;
  TPaletteEntry = tagPALETTEENTRY;
  {$EXTERNALSYM PALETTEENTRY}
  PALETTEENTRY = tagPALETTEENTRY;
  
  PRGBQuad = ^TRGBQuad;
  {$EXTERNALSYM tagRGBQUAD}
  tagRGBQUAD = packed record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
    rgbReserved: Byte;
  end;
  TRGBQuad = tagRGBQUAD;
  {$EXTERNALSYM RGBQUAD}
  RGBQUAD = tagRGBQUAD;

  PBitmapInfoHeader = ^TBitmapInfoHeader;
  {$EXTERNALSYM tagBITMAPINFOHEADER}
  tagBITMAPINFOHEADER = packed record
    biSize: DWORD;
    biWidth: Longint;
    biHeight: Longint;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: DWORD;
    biSizeImage: DWORD;
    biXPelsPerMeter: Longint;
    biYPelsPerMeter: Longint;
    biClrUsed: DWORD;
    biClrImportant: DWORD;
  end;
  TBitmapInfoHeader = tagBITMAPINFOHEADER;
  {$EXTERNALSYM BITMAPINFOHEADER}
  BITMAPINFOHEADER = tagBITMAPINFOHEADER;

  PBitmapInfo = ^TBitmapInfo;
  {$EXTERNALSYM tagBITMAPINFO}
  tagBITMAPINFO = packed record
      bmiHeader: TBitmapInfoHeader;
      bmiColors: array[0..0] of TRGBQuad;
  end;
  TBitmapInfo = tagBITMAPINFO;
  {$EXTERNALSYM BITMAPINFO}
  BITMAPINFO = tagBITMAPINFO;
	
  PBitmapFileHeader = ^TBitmapFileHeader;
  {$EXTERNALSYM tagBITMAPFILEHEADER}
  tagBITMAPFILEHEADER = packed record
    bfType: Word;
    bfSize: DWORD;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: DWORD;
  end;
  TBitmapFileHeader = tagBITMAPFILEHEADER;
  {$EXTERNALSYM BITMAPFILEHEADER}
  BITMAPFILEHEADER = tagBITMAPFILEHEADER;
	

function MakeLong(a, b: Word): Longint;
procedure ZeroMemory(Destination: Pointer; Length: DWORD);
function QueryPerformanceFrequency(var lpFrequency: TLargeInteger): BOOL;
function QueryPerformanceCounter(var lpPerformanceCount: TLargeInteger): BOOL;
function GetTickCount : Cardinal;
Procedure ShowMessage(msg : String);
procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: DWORD);

implementation

uses
  SDL;

procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: DWORD);
begin
  Move(Source^, Destination^, Length);
end;

procedure ShowMessage(msg : String);
begin
  // to be implemented	
end;

function MakeLong(A, B: Word): Longint;
begin
  Result := (LongInt(B) shl 16) + A;
end;

procedure ZeroMemory(Destination: Pointer; Length: DWORD);
begin
  FillChar( Destination^, Length, 0);
end;

function QueryPerformanceFrequency(var lpFrequency: TLargeInteger): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.QueryPerformanceFrequency(lpFrequency);
{$ENDIF}
{$IFDEF MACOS}
  Result := true;
  lpFrequency := 1000;
{$ENDIF}
end;

function QueryPerformanceCounter(var lpPerformanceCount: TLargeInteger): BOOL;
begin
{$IFDEF MSWINDOWS}
  Result := Windows.QueryPerformanceCounter(lpPerformanceCount);
{$ENDIF}
{$IFDEF MACOS}
  Result := true;
  lpPerformanceCount := SDL_GetTicks;
{$ENDIF}
end;

function GetTickCount : Cardinal;
begin
  Result := SDL_GetTicks;
end;

end.
