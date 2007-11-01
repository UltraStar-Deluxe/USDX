unit UCommon;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
{$IFDEF LAZARUS}
   lResources,
{$ENDIF}   
   ULog,
{$IFDEF DARWIN}
  messages,
{$ENDIF}
{$IFDEF win32}
   windows;
{$ELSE}
   lcltype,
   messages;
{$ENDIF}

{$IFNDEF win32}
type
  hStream        = THandle;
  HGLRC          = THandle;
  TLargeInteger  = Int64;
  TWin32FindData = LongInt;
{$ENDIF}

{$IFDEF LAZARUS}
  function LazFindResource( const aName, aType : String ): TLResource;
{$ENDIF}

{$IFDEF FPC}

function RandomRange(aMin: Integer; aMax: Integer) : Integer;

function MaxValue(const Data: array of Double): Double;
function MinValue(const Data: array of Double): Double;

  {$IFDEF WIN32}
  type
    TWndMethod = procedure(var Message: TMessage) of object;
  function  AllocateHWnd(Method: TWndMethod): HWND;
  procedure DeallocateHWnd(Wnd: HWND);
  {$ENDIF} // Win32

{$ENDIF} // FPC Only

function StringReplaceW(text : WideString; search, rep: WideChar):WideString;
function AdaptFilePaths( const aPath : widestring ): widestring;


{$IFNDEF win32}
(*
  function QueryPerformanceCounter(lpPerformanceCount:TLARGEINTEGER):Bool;
  function QueryPerformanceFrequency(lpFrequency:TLARGEINTEGER):Bool;
*)
  procedure ZeroMemory( Destination: Pointer; Length: DWORD );
{$ENDIF}

{$IFDEF MSWINDOWS}

type
  TSearchRecW = record
    Time: Integer;
    Size: Integer;
    Attr: Integer;
    Name: WideString;
    ExcludeAttr: Integer;
    FindHandle: THandle;
    FindData: TWin32FindDataW;
  end;

  function  FindFirstW(const Path: WideString; Attr: Integer; var  F: TSearchRecW): Integer;
  function  FindNextW(var F: TSearchRecW): Integer;
  procedure FindCloseW(var F: TSearchRecW);
  function  FindMatchingFileW(var F: TSearchRecW): Integer;
  function  DirectoryExistsW(const Directory: widestring): Boolean;
{$endif}

implementation

function StringReplaceW(text : WideString; search, rep: WideChar):WideString;
var
  iPos  : integer;
//  sTemp : WideString;
begin
(*
  result := text;
  iPos   := Pos(search, result);
  while (iPos > 0) do
  begin
    sTemp  := copy(result, iPos + length(search), length(result));
    result := copy(result, 1, iPos - 1) + rep + sTEmp;
    iPos   := Pos(search, result);
  end;
*)
  result := text;

  if search = rep then
    exit;

  for iPos := 0 to length( result ) - 1 do
  begin
    if result[ iPos ] = search then
      result[ iPos ] := rep;
  end;
end;

function AdaptFilePaths( const aPath : widestring ): widestring;
begin
  result := StringReplaceW( aPath, '\', PathDelim );//, [rfReplaceAll] );
end;


{$IFNDEF win32}
procedure ZeroMemory( Destination: Pointer; Length: DWORD );
begin
  FillChar( Destination^, Length, 0 );
end; //ZeroMemory

(*
function QueryPerformanceCounter(lpPerformanceCount:TLARGEINTEGER):Bool;

  // From http://en.wikipedia.org/wiki/RDTSC
  function RDTSC: Int64; register;
  asm
    rdtsc
  end;

begin
  // Use clock_gettime  here maybe ... from libc
  lpPerformanceCount := RDTSC();
  result := true;
end;

function QueryPerformanceFrequency(lpFrequency:TLARGEINTEGER):Bool;
begin
  lpFrequency := 0;
  result := true;
end;
*)
{$ENDIF}


{$IFDEF LAZARUS}

function LazFindResource( const aName, aType : String ): TLResource;
var
  iCount : Integer;
begin
  result := nil;
  
  for iCount := 0 to LazarusResources.count -1 do
  begin
    if ( LazarusResources.items[ iCount ].Name      = aName ) AND
       ( LazarusResources.items[ iCount ].ValueType = aType ) THEN
    begin
      result := LazarusResources.items[ iCount ];
      exit;
    end;
  end;
end;
{$ENDIF}

{$IFDEF FPC}
function MaxValue(const Data: array of Double): Double;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  for I := Low(Data) + 1 to High(Data) do
    if Result < Data[I] then
      Result := Data[I];
end;

function MinValue(const Data: array of Double): Double;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  for I := Low(Data) + 1 to High(Data) do
    if Result > Data[I] then
      Result := Data[I];
end;

function RandomRange(aMin: Integer; aMax: Integer) : Integer;
begin
RandomRange := Random(aMax-aMin) + aMin ;
end;


// NOTE !!!!!!!!!!
// AllocateHWnd is in lclintfh.inc

{$IFDEF MSWINDOWS}
// TODO : JB this is dodgey and bad... find a REAL solution !
function AllocateHWnd(Method: TWndMethod): HWND;
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, '', '', WS_POPUP , 0, 0, 0, 0, 0, 0, HInstance, nil);
end;

procedure DeallocateHWnd(Wnd: HWND);
var
  Instance: Pointer;
begin
  Instance := Pointer(GetWindowLong(Wnd, GWL_WNDPROC));
  DestroyWindow(Wnd);
end;
{$ENDIF}
{$IFDEF DARWIN}
// TODO : Situation for the mac isn't better !
function AllocateHWnd(Method: TWndMethod): HWND;
begin
end;

procedure DeallocateHWnd(Wnd: HWND);
begin
end;
{$ENDIF}




{$ENDIF}

{$ifdef MSWINDOWS}
function FindFirstW(const Path: widestring; Attr: Integer; var  F: TSearchRecW): Integer;
const
  faSpecial = faHidden or faSysFile or faVolumeID or faDirectory;
begin
  F.ExcludeAttr := not Attr and faSpecial;
  F.FindHandle := FindFirstFileW(PWideChar(Path), F.FindData);
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := FindMatchingFileW(F);
    if Result <> 0 then FindCloseW(F);
  end else
    Result := GetLastError;
end;

function FindNextW(var F: TSearchRecW): Integer;
begin
  if FindNextFileW(F.FindHandle, F.FindData) then
    Result := FindMatchingFileW(F)
  else
    Result := GetLastError;
end;

procedure FindCloseW(var F: TSearchRecW);
begin
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(F.FindHandle);
    F.FindHandle := INVALID_HANDLE_VALUE;
  end;
end;

function FindMatchingFileW(var F: TSearchRecW): Integer;
var
  LocalFileTime: TFileTime;
begin
  with F do
  begin
    while FindData.dwFileAttributes and ExcludeAttr <> 0 do
      if not FindNextFileW(FindHandle, FindData) then
      begin
        Result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi, LongRec(Time).Lo);
    Size := FindData.nFileSizeLow;
    Attr := FindData.dwFileAttributes;
    Name := FindData.cFileName;
  end;
  Result := 0;
end;

function DirectoryExistsW(const Directory: widestring): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributesW(PWideChar(Directory));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;
{$endif}





end.
