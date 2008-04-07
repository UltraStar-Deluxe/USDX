unit UCommon;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  SysUtils,
  Classes,
{$IFDEF MSWINDOWS}
  Windows,
  Messages,
{$ENDIF}
  ULog;

{$IFNDEF DARWIN}
// FIXME: remove this if it is not needed anymore
type
  hStream        = THandle;
  HGLRC          = THandle;
  TLargeInteger  = Int64;
  TWin32FindData = LongInt;
{$ENDIF}

function GetResourceStream(const aName, aType : string): TStream;

procedure ShowMessage( const msg : String );

{$IFDEF FPC}
function RandomRange(aMin: Integer; aMax: Integer) : Integer;
{$ENDIF}

{$IF Defined(MSWINDOWS) and Defined(FPC)}
function  AllocateHWnd(Method: TWndMethod): HWND;
procedure DeallocateHWnd(hWnd: HWND);
{$IFEND}

function StringReplaceW(text : WideString; search, rep: WideChar):WideString;
function AdaptFilePaths( const aPath : widestring ): widestring;


{$IFNDEF win32}
  procedure ZeroMemory( Destination: Pointer; Length: DWORD );
{$ENDIF}

function FileExistsInsensitive(var FileName: string): boolean;

(*
 * Character classes
 *)

function IsAlphaChar(ch: WideChar): boolean;
function IsNumericChar(ch: WideChar): boolean;
function IsAlphaNumericChar(ch: WideChar): boolean;
function IsPunctuationChar(ch: WideChar): boolean;
function IsControlChar(ch: WideChar): boolean;


implementation

uses
{$IFDEF Delphi}
  Dialogs,
{$ENDIF}
  UMain,
  UConfig;

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

// Checks if a regular files or directory with the given name exists.
// The comparison is case insensitive.
function FileExistsInsensitive(var FileName: string): boolean;
var
  FilePath, LocalFileName: string;
  SearchInfo: TSearchRec;
begin
{$IFDEF LINUX} // eddie: Changed FPC to LINUX: Windows and Mac OS X dont have case sensitive file systems
  // speed up standard case
  if FileExists(FileName) then
  begin
    Result := true;
    exit;
  end;

  Result := false;

  FilePath := ExtractFilePath(FileName);
  if (FindFirst(FilePath+'*', faAnyFile, SearchInfo) = 0) then
  begin
    LocalFileName := ExtractFileName(FileName);
    repeat
      if (AnsiSameText(LocalFileName, SearchInfo.Name)) then
      begin
        FileName := FilePath + SearchInfo.Name;
        Result := true;
        break;
      end;
    until (FindNext(SearchInfo) <> 0);
  end;
  FindClose(SearchInfo);
{$ELSE}
  Result := FileExists(FileName);
{$ENDIF}
end;


{$IFDEF Linux}
  // include resource-file info (stored in the constant array "resources")
  {$I ../resource.inc}
{$ENDIF}

function GetResourceStream(const aName, aType: string): TStream;
{$IFDEF Linux}
var
  ResIndex: integer;
  Filename: string;
{$ENDIF}
begin
  Result := nil;

  {$IFDEF Linux}
  for ResIndex := 0 to High(resources) do
  begin
    if (resources[ResIndex][0] = aName ) and
       (resources[ResIndex][1] = aType ) then
    begin
      try
        Filename := ResourcesPath + resources[ResIndex][2];
        Result := TFileStream.Create(Filename, fmOpenRead);
      except
        Log.LogError('Failed to open: "'+ resources[ResIndex][2] +'"', 'GetResourceStream');
      end;
      exit;
    end;
  end;
  {$ELSE}
  try
    Result := TResourceStream.Create(HInstance, aName , PChar(aType));
  except
    Log.LogError('Invalid resource: "'+ aType + ':' + aName +'"', 'GetResourceStream');
  end;
  {$ENDIF}
end;

{$IFDEF FPC}
function RandomRange(aMin: Integer; aMax: Integer) : Integer;
begin
  RandomRange := Random(aMax-aMin) + aMin ;
end;
{$ENDIF}

{$IF Defined(MSWINDOWS) and Defined(FPC)}
function AllocateHWndCallback(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Msg: TMessage;
  MethodPtr: ^TWndMethod;
begin
  FillChar(Msg, SizeOf(Msg), 0);  
  Msg.msg := uMsg;
  Msg.wParam := wParam;
  Msg.lParam := lParam;

  MethodPtr := Pointer(GetWindowLongPtr(hwnd, GWL_USERDATA));
  if Assigned(MethodPtr) then
    MethodPtr^(Msg);
   
  Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
end;

function AllocateHWnd(Method: TWndMethod): HWND;
var
  ClassExists: Boolean;
  WndClass, OldClass: TWndClass;
  MethodPtr: ^TMethod;
begin
  Result := 0;

  // setup class-info
  FillChar(WndClass, SizeOf(TWndClass), 0);
  WndClass.hInstance := HInstance;
  // Important: do not enable AllocateHWndCallback before the msg-handler method is assigned,
  //   otherwise race-conditions might occur
  WndClass.lpfnWndProc := @DefWindowProc;
  WndClass.lpszClassName:= 'USDXUtilWindowClass';

  // check if class is already registered
  ClassExists := GetClassInfo(HInstance, WndClass.lpszClassName, OldClass);
  // create window-class shared by all windows created by AllocateHWnd()
  if (not ClassExists) or (@OldClass.lpfnWndProc <> @DefWindowProc) then
  begin
    if ClassExists then
      UnregisterClass(WndClass.lpszClassName, HInstance);
    if (RegisterClass(WndClass) = 0) then
       Exit;
  end;
  // create window
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, WndClass.lpszClassName, '',
    WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
  if (Result = 0) then
    Exit;
  // assign individual callback procedure to the window
  if Assigned(Method) then
  begin
    // TMethod contains two pointers but we can pass just one as USERDATA
    GetMem(MethodPtr, SizeOf(TMethod));
    MethodPtr^ := TMethod(Method);
    SetWindowLongPtr(Result, GWL_USERDATA, LONG_PTR(MethodPtr));
  end;
  // now enable AllocateHWndCallback for this window
  SetWindowLongPtr(Result, GWL_WNDPROC, LONG_PTR(@AllocateHWndCallback));
end;

procedure DeallocateHWnd(hWnd: HWND);
var
  MethodPtr: ^TMethod;
begin
  if (hWnd <> 0) then
  begin
    MethodPtr := Pointer(GetWindowLongPtr(hWnd, GWL_USERDATA));
    DestroyWindow(hWnd);
    if Assigned(MethodPtr) then
      FreeMem(MethodPtr);
  end;
end;
{$IFEND}

procedure ShowMessage( const msg : String );
begin
{$IF Defined(MSWINDOWS)}
  MessageBox(0, PChar(msg), PChar(USDXVersionStr()), MB_ICONINFORMATION);
{$ELSE}
  debugwriteln(msg);
{$IFEND}
end;

function IsAlphaChar(ch: WideChar): boolean;
begin
  // TODO: add chars > 255 when unicode-fonts work?
  case ch of
    'A'..'Z',  // A-Z
    'a'..'z',  // a-z
    #170,#181,#186,
    #192..#214,
    #216..#246,
    #248..#255:
      Result := true;
    else
      Result := false;
  end;
end;

function IsNumericChar(ch: WideChar): boolean;
begin
  case ch of
    '0'..'9':
      Result := true;
    else
      Result := false;
  end;
end;

function IsAlphaNumericChar(ch: WideChar): boolean;
begin
  Result := (IsAlphaChar(ch) or IsNumericChar(ch));
end;

function IsPunctuationChar(ch: WideChar): boolean;
begin
  // TODO: add chars outside of Latin1 basic (0..127)?
  case ch of
    ' '..'/',':'..'@','['..'`','{'..'~':
      Result := true;
    else
      Result := false;
  end;
end;

function IsControlChar(ch: WideChar): boolean;
begin
  case ch of
    #0..#31,
    #127..#159:
      Result := true;
    else
      Result := false;
  end;
end;

end.
