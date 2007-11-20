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

// eddie: FindFirstW etc are now in UPlatformWindows.pas

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
{$ENDIF} // IFDEF DARWIN

{$ENDIF} // IFDEF FPC

end.
