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
  {$ENDIF}
  UConfig,
  ULog,
  UPath;

type
  TStringDynArray = array of string;

const
  SepWhitespace = [#9, #10, #13, ' ']; // tab, lf, cr, space

{**
 * Splits a string into pieces separated by Separators.
 * MaxCount specifies the max. number of pieces. If it is <= 0 the number is
 * not limited. If > 0 the last array element will hold the rest of the string
 * (with leading separators removed).
 *
 * Examples:
 *   SplitString(' split  me now ', 0) -> ['split', 'me', 'now']
 *   SplitString(' split  me now ', 1) -> ['split', 'me now']
 *}
function SplitString(const Str: string; MaxCount: integer = 0; Separators: TSysCharSet = SepWhitespace): TStringDynArray;


type
  TMessageType = (mtInfo, mtError);

procedure ShowMessage(const msg: string; msgType: TMessageType = mtInfo);

procedure ConsoleWriteLn(const msg: string);

{$IFDEF FPC}
function RandomRange(aMin: integer; aMax: integer): integer;
{$ENDIF}

procedure DisableFloatingPointExceptions();
procedure SetDefaultNumericLocale();
procedure RestoreNumericLocale();

{$IFNDEF MSWINDOWS}
procedure ZeroMemory(Destination: pointer; Length: dword);
function MakeLong(a, b: word): longint;
{$ENDIF}

// A stable alternative to TList.Sort() (use TList.Sort() if applicable, see below)
procedure MergeSort(List: TList; CompareFunc: TListSortCompare);

function GetAlignedMem(Size: cardinal; Alignment: integer): pointer;
procedure FreeAlignedMem(P: pointer);


implementation

uses
  Math,
  {$IFDEF Delphi}
  Dialogs,
  {$ENDIF}
  sdl,
  UFilesystem,
  UMain,
  UUnicodeUtils;

function SplitString(const Str: string; MaxCount: integer; Separators: TSysCharSet): TStringDynArray;

  {*
   * Adds Str[StartPos..Endpos-1] to the result array.
   *}
  procedure AddSplit(StartPos, EndPos: integer);
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := Copy(Str, StartPos, EndPos-StartPos);
  end;

var
  I: integer;
  Start: integer;
  Last: integer;
begin
  Start := 0;
  SetLength(Result, 0);

  for I := 1 to Length(Str) do
  begin
    if (Str[I] in Separators) then
    begin
      // end of component found
      if (Start > 0) then
      begin
        AddSplit(Start, I);
        Start := 0;
      end;
    end
    else if (Start = 0) then
    begin
      // mark beginning of component
      Start := I;
      // check if this is the last component
      if (Length(Result) = MaxCount-1) then
      begin
        // find last non-separator char
        Last := Length(Str);
        while (Str[Last] in Separators) do
          Dec(Last);
        // add component up to last non-separator
        AddSplit(Start, Last);
        Exit;
      end;
    end;
  end;

  // last component
  if (Start > 0) then
    AddSplit(Start, Length(Str)+1);
end;

// data used by the ...Locale() functions
{$IF Defined(Linux) or Defined(FreeBSD)}

var
  PrevNumLocale: string;

const
  LC_NUMERIC  = 1;

function setlocale(category: integer; locale: pchar): pchar; cdecl; external 'c' name 'setlocale';

{$IFEND}

// In Linux and maybe MacOSX some units (like cwstring) call setlocale(LC_ALL, '')
// to set the language/country specific locale (e.g. charset) for this application.
// Unfortunately, LC_NUMERIC is set by this call too.
// It defines the decimal-separator and other country-specific numeric settings.
// This parameter is used by the C string-to-float parsing functions atof() and strtod().
// After changing LC_NUMERIC some external C-based libs (like projectM) are not
// able to parse strings correctly
// (e.g. in Germany "0.9" is not recognized as a valid number anymore but "0,9" is).
// So we reset the numeric settings to the default ('C').
// Note: The behaviour of Pascal parsing functions (e.g. strtofloat()) is not
//   changed by this because it doesn't use the locale-settings.
// TODO:
// - Check if this is needed in MacOSX (at least the locale is set in cwstring)
// - Find out which libs are concerned by this problem.
//   If only projectM is concerned by this problem set and restore the numeric locale
//   for each call to projectM instead of changing it globally.
procedure SetDefaultNumericLocale();
begin
  {$IF Defined(LINUX) or Defined(FreeBSD)}
  PrevNumLocale := setlocale(LC_NUMERIC, nil);
  setlocale(LC_NUMERIC, 'C');
  {$IFEND}
end;

procedure RestoreNumericLocale();
begin
  {$IF Defined(LINUX) or Defined(FreeBSD)}
  setlocale(LC_NUMERIC, PChar(PrevNumLocale));
  {$IFEND}
end;

(*
 * If an invalid floating point operation was performed the Floating-point unit (FPU)
 * generates a Floating-point exception (FPE). Dependending on the settings in
 * the FPU's control-register (interrupt mask) the FPE is handled by the FPU itself
 * (we will call this as "FPE disabled" later on) or is passed to the application
 * (FPE enabled).
 * If FPEs are enabled a floating-point division by zero (e.g. 10.0 / 0.0) is
 * considered an error and an exception is thrown. Otherwise the FPU will handle
 * the error and return the result infinity (INF) (10.0 / 0.0 = INF) without
 * throwing an error to the application.
 * The same applies to a division by INF that either raises an exception
 * (FPE enabled) or returns 0.0 (FPE disabled).
 * Normally (as with C-programs), Floating-point exceptions (FPE) are DISABLED
 * on program startup (at least with Intel CPUs), but for some strange reasons
 * they are ENABLED in pascal (both delphi and FPC) by default.
 * Many libs operating with floating-point values rely heavily on the C-specific
 * behaviour. So using them in delphi is a ticking time-bomb because sooner or
 * later they will crash because of an FPE (this problem occurs massively
 * in OpenGL-based libs like projectM). In contrast to this no error will occur
 * if the lib is linked to a C-program.
 *
 * Further info on FPUs:
 * For x86 and x86_64 CPUs we have to consider two FPU instruction sets.
 * The math co-processor i387 (aka 8087 or x87) set introduced with the i386
 * and SSE (Streaming SIMD Extensions) introduced with the Pentium3.
 * Both of them have separate control-registers (x87: FPUControlWord, SSE: MXCSR)
 * to control FPEs. Either has (among others) 6bits to enable/disable several
 * exception types (Invalid,Denormalized,Zero,Overflow,Underflow,Precision).
 * Those exception-types must all be masked (=1) to get the default C behaviour.
 * The control-registers can be set with the asm-ops FLDCW (x87) and LDMXCSR (SSE).
 * Instead of using assembler code, we can use Set8087CW() provided by delphi and
 * FPC to set the x87 control-word. FPC also provides SetSSECSR() for SSE's MXCSR.
 * Note that both Delphi and FPC enable FPEs (e.g. for div-by-zero) on program
 * startup but only FPC enables FPEs (especially div-by-zero) for SSE too.
 * So we have to mask FPEs for x87  in Delphi and FPC and for SSE in FPC only.
 * FPC and Delphi both provide a SetExceptionMask() for control of the FPE
 * mask. SetExceptionMask() sets the masks for x87 in Delphi and for x87 and SSE
 * in FPC (seems as if Delphi [2005] is not SSE aware). So SetExceptionMask()
 * is what we need and it even is plattform and CPU independent.
 *
 * Pascal OpenGL headers (like the Delphi standard ones or JEDI-SDL headers)
 * already call Set8087CW() to disable FPEs but due to some bugs in the JEDI-SDL
 * headers they do not work properly with FPC. I already patched them, so they
 * work at least until they are updated the next time. In addition Set8086CW()
 * does not suffice to disable FPEs because the SSE FPEs are not disabled by this.
 * FPEs with SSE are a big problem with some libs because many linux distributions
 * optimize code for SSE or Pentium3 (for example: int(INF) which convert the
 * double value "infinity" to an integer might be automatically optimized by
 * using SSE's CVTSD2SI instruction). So SSE FPEs must be turned off in any case
 * to make USDX portable.
 *
 * Summary:
 * Call this function on initialization to make sure FPEs are turned off.
 * It will solve a lot of errors with FPEs in external libs.
 *)
procedure DisableFloatingPointExceptions();
begin
  (*
  // We will use SetExceptionMask() instead of Set8087CW()/SetSSECSR().
  // Note: Leave these lines for documentation purposes just in case
  //       SetExceptionMask() does not work anymore (due to bugs in FPC etc.).
  {$IF Defined(CPU386) or Defined(CPUI386) or Defined(CPUX86_64)}
  Set8087CW($133F);
  {$IFEND}
  {$IF Defined(FPC)}
  if (has_sse_support) then
    SetSSECSR($1F80);
  {$IFEND}
  *)
  
  // disable all of the six FPEs (x87 and SSE) to be compatible with C/C++ and
  // other libs which rely on the standard FPU behaviour (no div-by-zero FPE anymore).
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
                    exOverflow, exUnderflow, exPrecision]);
end;

{$IFNDEF MSWINDOWS}
procedure ZeroMemory(Destination: pointer; Length: dword);
begin
  FillChar(Destination^, Length, 0);
end;

function MakeLong(A, B: word): longint;
begin
  Result := (LongInt(B) shl 16) + A;
end;

{$ENDIF}

{$IFDEF FPC}
function RandomRange(aMin: integer; aMax: integer): integer;
begin
  RandomRange := Random(aMax - aMin) + aMin ;
end;
{$ENDIF}


{$IFDEF FPC}
var
  MessageList: TStringList;
  ConsoleHandler: TThreadID;
  // Note: TRTLCriticalSection is defined in the units System and Libc, use System one
  ConsoleCriticalSection: System.TRTLCriticalSection;
  ConsoleEvent: PRTLEvent;
  ConsoleQuit: boolean;
{$ENDIF}

(*
 * Write to console if one is available.
 * It checks if a console is available before output so it will not
 * crash on windows if none is available.
 * Do not use this function directly because it is not thread-safe,
 * use ConsoleWriteLn() instead.
 *)
procedure _ConsoleWriteLn(const aString: string); {$IFDEF HasInline}inline;{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  // sanity check to avoid crashes with writeln()
  if (IsConsole) then
  begin
  {$ENDIF}
    Writeln(aString);
  {$IFDEF MSWINDOWS}
  end;
  {$ENDIF}
end;

{$IFDEF FPC}
{*
 * The console-handlers main-function.
 * TODO: create a quit-event on closing.
 *}
function ConsoleHandlerFunc(param: pointer): PtrInt;
var
  i: integer;
  quit: boolean;
begin
  quit := false;
  while (not quit) do
  begin
    // wait for new output or quit-request
    RTLeventWaitFor(ConsoleEvent);

    System.EnterCriticalSection(ConsoleCriticalSection);
    // output pending messages
    for i := 0 to MessageList.Count - 1 do
    begin
      _ConsoleWriteLn(MessageList[i]);
    end;
    MessageList.Clear();

    // use local quit-variable to avoid accessing
    // ConsoleQuit outside of the critical section
    if (ConsoleQuit) then
      quit := true;

    RTLeventResetEvent(ConsoleEvent);
    System.LeaveCriticalSection(ConsoleCriticalSection);
  end;
  result := 0;
end;
{$ENDIF}

procedure InitConsoleOutput();
begin
  {$IFDEF FPC}
  // init thread-safe output
  MessageList := TStringList.Create();
  System.InitCriticalSection(ConsoleCriticalSection);
  ConsoleEvent := RTLEventCreate();
  ConsoleQuit := false;
  // must be a thread managed by FPC. Otherwise (e.g. SDL-thread)
  // it will crash when using Writeln.
  ConsoleHandler := BeginThread(@ConsoleHandlerFunc);
  {$ENDIF}
end;

procedure FinalizeConsoleOutput();
begin
  {$IFDEF FPC}
  // terminate console-handler
  System.EnterCriticalSection(ConsoleCriticalSection);
  ConsoleQuit := true;
  RTLeventSetEvent(ConsoleEvent);
  System.LeaveCriticalSection(ConsoleCriticalSection);
  WaitForThreadTerminate(ConsoleHandler, 0);
  // free data
  System.DoneCriticalsection(ConsoleCriticalSection);
  RTLeventDestroy(ConsoleEvent);
  MessageList.Free();
  {$ENDIF}
end;

{*
 * FPC uses threadvars (TLS) managed by FPC for console output locking.
 * Using WriteLn() from external threads (like in SDL callbacks)
 * will crash the program as those threadvars have never been initialized.
 * The solution is to create an FPC-managed thread which has the TLS data
 * and use it to handle the console-output (hence it is called Console-Handler)
 *}
procedure ConsoleWriteLn(const msg: string);
begin
{$IFDEF CONSOLE}
  {$IFDEF FPC}
  // TODO: check for the main-thread and use a simple _ConsoleWriteLn() then?
  //GetCurrentThreadThreadId();
  System.EnterCriticalSection(ConsoleCriticalSection);
  MessageList.Add(msg);
  RTLeventSetEvent(ConsoleEvent);
  System.LeaveCriticalSection(ConsoleCriticalSection);
  {$ELSE}
  _ConsoleWriteLn(msg);
  {$ENDIF}
{$ENDIF}
end;

procedure ShowMessage(const msg: String; msgType: TMessageType);
{$IFDEF MSWINDOWS}
var Flags: cardinal;
{$ENDIF}
begin
{$IF Defined(MSWINDOWS)}
  case msgType of
    mtInfo:  Flags := MB_ICONINFORMATION or MB_OK;
    mtError: Flags := MB_ICONERROR or MB_OK;
    else Flags := MB_OK;
  end;
  MessageBox(0, PChar(msg), PChar(USDXVersionStr()), Flags);
{$ELSE}
  ConsoleWriteln(msg);
{$IFEND}
end;

(*
 * Recursive part of the MergeSort algorithm.
 * OutList will be either InList or TempList and will be swapped in each
 * depth-level of recursion. By doing this it we can directly merge into the
 * output-list. If we only had In- and OutList parameters we had to merge into
 * InList after the recursive calls and copy the data to the OutList afterwards.
 *)
procedure _MergeSort(InList, TempList, OutList: TList; StartPos, BlockSize: integer;
                    CompareFunc: TListSortCompare);
var
  LeftSize, RightSize: integer; // number of elements in left/right block
  LeftEnd,  RightEnd:  integer; // Index after last element in left/right block
  MidPos: integer; // index of first element in right block
  Pos: integer;    // position in output list
begin
  LeftSize := BlockSize div 2;
  RightSize := BlockSize - LeftSize;
  MidPos := StartPos + LeftSize;

  // sort left and right halves of this block by recursive calls of this function
  if (LeftSize >= 2) then
    _MergeSort(InList, OutList, TempList, StartPos, LeftSize, CompareFunc)
  else
    TempList[StartPos] := InList[StartPos];
  if (RightSize >= 2) then
    _MergeSort(InList, OutList, TempList, MidPos, RightSize, CompareFunc)
  else
    TempList[MidPos] := InList[MidPos];

  // merge sorted left and right sub-lists into output-list 
  LeftEnd := MidPos;
  RightEnd := StartPos + BlockSize;
  Pos := StartPos;
  while ((StartPos < LeftEnd) and (MidPos < RightEnd)) do
  begin
    if (CompareFunc(TempList[StartPos], TempList[MidPos]) <= 0) then
    begin
      OutList[Pos] := TempList[StartPos];
      Inc(StartPos);
    end
    else
    begin
      OutList[Pos] := TempList[MidPos];
      Inc(MidPos);
    end;
    Inc(Pos);
  end;

  // copy remaining elements to output-list
  while (StartPos < LeftEnd) do
  begin
    OutList[Pos] := TempList[StartPos];
    Inc(StartPos);
    Inc(Pos);
  end;
  while (MidPos < RightEnd) do
  begin
    OutList[Pos] := TempList[MidPos];
    Inc(MidPos);
    Inc(Pos);
  end;
end;

(*
 * Stable alternative to the instable TList.Sort() (uses QuickSort) implementation.
 * A stable sorting algorithm preserves preordered items. E.g. if sorting by
 * songs by title first and artist afterwards, the songs of each artist will
 * be ordered by title. In contrast to this an unstable algorithm (like QuickSort)
 * may destroy an existing order, so the songs of an artist will not be ordered
 * by title anymore after sorting by artist in the previous example.
 * If you do not need a stable algorithm, use TList.Sort() instead.
 *)
procedure MergeSort(List: TList; CompareFunc: TListSortCompare);
var
  TempList: TList;
begin
  TempList := TList.Create();
  TempList.Count := List.Count;
  if (List.Count >= 2) then
    _MergeSort(List, TempList, List, 0, List.Count, CompareFunc);
  TempList.Free;
end;


type
  // stores the unaligned pointer of data allocated by GetAlignedMem()
  PMemAlignHeader = ^TMemAlignHeader;
  TMemAlignHeader = pointer;

(**
 * Use this function to assure that allocated memory is aligned on a specific
 * byte boundary.
 * Alignment must be a power of 2.
 *
 * Important: Memory allocated with GetAlignedMem() MUST be freed with
 * FreeAlignedMem(), FreeMem() will cause a segmentation fault.
 *
 * Hint: If you do not need dynamic memory, consider to allocate memory
 * statically and use the {$ALIGN x} compiler directive. Note that delphi
 * supports an alignment "x" of up to 8 bytes only whereas FPC supports
 * alignments on 16 and 32 byte boundaries too.
 *)
{$WARNINGS OFF}
function GetAlignedMem(Size: cardinal; Alignment: integer): pointer;
var
  OrigPtr: pointer;
const
  MIN_ALIGNMENT = 16;
begin
  // Delphi and FPC (tested with 2.2.0) align memory blocks allocated with
  // GetMem() at least on 8 byte boundaries. Delphi uses a minimal alignment
  // of either 8 or 16 bytes depending on the size of the requested block
  // (see System.GetMinimumBlockAlignment). As we do not want to change the
  // boundary for the worse, we align at least on MIN_ALIGN.
  if (Alignment < MIN_ALIGNMENT) then
    Alignment := MIN_ALIGNMENT;

  // allocate unaligned memory
  GetMem(OrigPtr, SizeOf(TMemAlignHeader) + Size + Alignment);
  if (OrigPtr = nil) then
  begin
    Result := nil;
    Exit;
  end;

  // reserve space for the header
  Result := pointer(PtrUInt(OrigPtr) + SizeOf(TMemAlignHeader));
  // align memory
  Result := pointer(PtrUInt(Result) + Alignment - PtrUInt(Result) mod Alignment);

  // set header with info on old pointer for FreeMem
  PMemAlignHeader(PtrUInt(Result) - SizeOf(TMemAlignHeader))^ := OrigPtr;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure FreeAlignedMem(P: pointer);
begin
  if (P <> nil) then
    FreeMem(PMemAlignHeader(PtrUInt(P) - SizeOf(TMemAlignHeader))^);
end;
{$WARNINGS ON}


initialization
  InitConsoleOutput();

finalization
  FinalizeConsoleOutput();

end.
