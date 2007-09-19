unit UCommon;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  windows;

{$IFDEF FPC}

type
  TWndMethod = procedure(var Message: TMessage) of object;

function  RandomRange(aMin: Integer; aMax: Integer) : Integer;
function  AllocateHWnd(Method: TWndMethod): HWND;
procedure DeallocateHWnd(Wnd: HWND);

function MaxValue(const Data: array of Double): Double;
function MinValue(const Data: array of Double): Double;
{$ENDIF}

implementation

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



// TODO : JB this is dodgey and bad... find a REAL solution !
function AllocateHWnd(Method: TWndMethod): HWND;
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
(*
  UtilWindowClass.hInstance := HInstance;
{$IFDEF PIC}
  UtilWindowClass.lpfnWndProc := @DefWindowProc;
{$ENDIF}
  ClassRegistered := GetClassInfo(HInstance, UtilWindowClass.lpszClassName, TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @DefWindowProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(UtilWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(UtilWindowClass);
  end;
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, UtilWindowClass.lpszClassName, '', WS_POPUP {+ 0}, 0, 0, 0, 0, 0, 0, HInstance, nil);
*)
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, '', '', WS_POPUP {+ 0}, 0, 0, 0, 0, 0, 0, HInstance, nil);

(*
  if Assigned(Method) then
    SetWindowLong(Result, GWL_WNDPROC, Longint(MakeObjectInstance(Method)));
*)
end;

procedure DeallocateHWnd(Wnd: HWND);
var
  Instance: Pointer;
begin
  Instance := Pointer(GetWindowLong(Wnd, GWL_WNDPROC));
  DestroyWindow(Wnd);
  
//  if Instance <> @DefWindowProc then
//     FreeObjectInstance(Instance);
end;

{$ENDIF}


end.
