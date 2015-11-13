unit WinAllocation;

// FPC misses AllocateHWnd and DeallocateHWnd which is used by several
// libraries such as Midi... or DirWatch.
// Since FPC 2.2.2 there are dummies in Classes that just raise RunTime exceptions.
// To avoid those exceptions, include this unit AFTER Classes.
// Maybe the dummies will be replaced by functional routines in the future.WinAllocation
//
// THESE FUNCTIONS ARE ONLY FOR COMPATIBILITY WITH SOME EXTERNAL WIN32 LIBS.
// DO NOT USE THEM IN USDX CODE.
//

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Classes,
  Windows;

function  AllocateHWnd(Method: TWndMethod): HWND;
procedure DeallocateHWnd(hWnd: HWND);

implementation

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
    DWORD(WS_POPUP), 0, 0, 0, 0, 0, 0, HInstance, nil);
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

end.
