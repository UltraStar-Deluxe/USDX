unit DirWatch;

// -----------------------------------------------------------------------------
// Component Name:  TDirectoryWatch                                            .
// Module:          DirWatch                                                   .
// Description:     Implements watching for file changes in a designated       .
//                  directory (or directories).                                .
// Version:         1.4                                                        .
// Date:            10-MAR-2003                                                .
// Target:          Win32, Delphi 3 - Delphi 7                                 .
// Author:          Angus Johnson, angusj-AT-myrealbox-DOT-com                 .
//                  A portion of code has been copied from the Drag & Drop     .
//                  Component Suite which I co-authored with Anders Melander.  .
// Copyright:       © 2003 Angus Johnson                                       .
//                                                                             .
// Usage:           1. Add a TDirectoryWatch component to your form.           .
//                  2. Set its Directory property                              .
//                  3. If you wish to watch its subdirectories too then set    .
//                  the WatchSubDir property to true                           .
//                  4. Assign the OnChange event                               .
//                  5. Set Active to true                                      .
// -----------------------------------------------------------------------------

interface

{$IFDEF FPC}
  {$MODE Delphi}
  {$H+} // use long strings
{$ENDIF}

uses
  Windows,
  Messages,
  Classes,
  {$IFDEF FPC}
  WinAllocation,
  {$ENDIF}
  SysUtils;

type
  TNotifyFilters = set of (nfFilename, nfDirname, nfAttrib,
    nfSize, nfLastWrite, nfSecurity);

  TWatchThread = class;             //forward declaration

  TDirectoryWatch = class(TComponent)
  private
    fWindowHandle: THandle;
    fWatchThread: TWatchThread;
    fWatchSubDirs: boolean;
    fDirectory: string;
    fActive: boolean;
    fNotifyFilters: TNotifyFilters; //see FindFirstChangeNotification in winAPI
    fOnChangeEvent: TNotifyEvent;
    procedure SetActive(aActive: boolean);
    procedure SetDirectory(aDir: string);
    procedure SetWatchSubDirs(aWatchSubDirs: boolean);
    procedure SetNotifyFilters(aNotifyFilters: TNotifyFilters);
    procedure WndProc(var aMsg: TMessage);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Directory: string read fDirectory write SetDirectory;
    property NotifyFilters: TNotifyFilters
      read fNotifyFilters write SetNotifyFilters;
    property WatchSubDirs: boolean read fWatchSubDirs write SetWatchSubDirs;
    property Active: boolean read fActive write SetActive;
    property OnChange: TNotifyEvent read fOnChangeEvent write fOnChangeEvent;
  end;

  TWatchThread = class(TThread)
  private
    fOwnerHdl: Thandle;
    fChangeNotify : THandle; //Signals whenever Windows detects a change in    .
                             //the watched directory                           .
    fBreakEvent: THandle;    //Signals when either the Directory property      .
                             //changes or when the thread terminates           .
    fDirectory: string;
    fWatchSubDirs: longbool;
    fNotifyFilters: dword;
    fFinished: boolean;    
  protected
    procedure SetDirectory(const Value: string);
    procedure ProcessFilenameChanges;
    procedure Execute; override;
  public
    constructor Create( OwnerHdl: THandle;
      const InitialDir: string; WatchSubDirs: boolean; NotifyFilters: dword);
    destructor Destroy; override;
    procedure Terminate;
    property Directory: string write SetDirectory;
  end;

procedure Register;

implementation

const
  NOTIFYCHANGE_MESSAGE = WM_USER + 1;

resourcestring
  sInvalidDir = 'Invalid Directory: ';

//----------------------------------------------------------------------------
// Miscellaneous functions ...
//----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('Samples', [TDirectoryWatch]);
end;
//----------------------------------------------------------------------------

function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

//----------------------------------------------------------------------------
// TDirectoryWatch methods ...
//----------------------------------------------------------------------------

constructor TDirectoryWatch.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  //default Notify values - notify if either a file name or a directory name
  //changes or if a file is modified ...
  fNotifyFilters := [nfFilename, nfDirname, nfLastWrite];
  fDirectory := 'C:\';
  //this non-visual control needs to handle messages, so ...
  if not (csDesigning in ComponentState) then
    fWindowHandle := AllocateHWnd(WndProc);
end;
//----------------------------------------------------------------------------

destructor TDirectoryWatch.Destroy;
begin
  Active := false;
  if not (csDesigning in ComponentState) then
    DeallocateHWnd(fWindowHandle);
  inherited Destroy;
end;
//----------------------------------------------------------------------------

procedure TDirectoryWatch.WndProc(var aMsg: TMessage);
begin
  with aMsg do
    if Msg = NOTIFYCHANGE_MESSAGE then
    begin
      if assigned(OnChange) then OnChange(self);
    end else
      Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;
//------------------------------------------------------------------------------

procedure TDirectoryWatch.SetNotifyFilters(aNotifyFilters: TNotifyFilters);
begin
  if aNotifyFilters = fNotifyFilters then exit;
  fNotifyFilters := aNotifyFilters;
  if assigned(fWatchThread) then
  begin
    Active := false;
    Active := true;
  end;
end;
//------------------------------------------------------------------------------

procedure TDirectoryWatch.SetWatchSubDirs(aWatchSubDirs: boolean);
begin
  if aWatchSubDirs = fWatchSubDirs then exit;
  fWatchSubDirs := aWatchSubDirs;
  if assigned(fWatchThread) then
  begin
    Active := false;
    Active := true;
  end;
end;
//------------------------------------------------------------------------------

procedure TDirectoryWatch.SetDirectory(aDir: string);
begin
  if aDir = '' then
  begin
    Active := false;
    fDirectory := '';
    exit;
  end;
  if (aDir[length(aDir)] <> '\') then aDir := aDir + '\';
  if aDir = fDirectory then exit;
  if not (csDesigning in ComponentState) and not DirectoryExists(aDir) then
    raise Exception.Create( sInvalidDir + aDir);
  fDirectory := aDir;
  if assigned(fWatchThread) then
    fWatchThread.Directory := fDirectory;
end;
//------------------------------------------------------------------------------

procedure TDirectoryWatch.SetActive(aActive: boolean);
var
  nf: dword;
begin
  if aActive = fActive then exit;
  fActive := aActive;
  if csDesigning in ComponentState then exit;
  if fActive then
  begin
    if not DirectoryExists(fDirectory) then
    begin
      fActive := false;
      raise Exception.Create(sInvalidDir + fDirectory);
    end;
    nf := 0;
    if nfFilename in fNotifyFilters then
      nf := nf or FILE_NOTIFY_CHANGE_FILE_NAME;
    if nfDirname in fNotifyFilters then
      nf := nf or FILE_NOTIFY_CHANGE_DIR_NAME;
    if nfAttrib in fNotifyFilters then
      nf := nf or FILE_NOTIFY_CHANGE_ATTRIBUTES;
    if nfSize in fNotifyFilters then
      nf := nf or FILE_NOTIFY_CHANGE_SIZE;
    if nfLastWrite in fNotifyFilters then
      nf := nf or FILE_NOTIFY_CHANGE_LAST_WRITE;
    if nfSecurity in fNotifyFilters then
      nf := nf or FILE_NOTIFY_CHANGE_SECURITY;
    fWatchThread := TWatchThread.Create(
      fWindowHandle, fDirectory, fWatchSubDirs, nf);
  end else
  begin
    fWatchThread.Terminate;
    fWatchThread := nil;
  end;
end;

//----------------------------------------------------------------------------
// TWatchThread methods ...
//----------------------------------------------------------------------------

constructor TWatchThread.Create(OwnerHdl: THandle;
  const InitialDir: string; WatchSubDirs: boolean; NotifyFilters: dword);
begin
  inherited Create(True);       
  fOwnerHdl := OwnerHdl;
  if WatchSubDirs then
    cardinal(fWatchSubDirs) := 1 //workaround a Win9x OS issue
  else
    fWatchSubDirs := false;
  FreeOnTerminate := true;
  Priority := tpLowest;
  fDirectory := InitialDir;
  fNotifyFilters := NotifyFilters;
  fBreakEvent := windows.CreateEvent(nil, False, False, nil);
  Resume;
end;
//------------------------------------------------------------------------------

destructor TWatchThread.Destroy;
begin
  CloseHandle(fBreakEvent);
  inherited Destroy;
end;
//------------------------------------------------------------------------------

procedure TWatchThread.SetDirectory(const Value: string);
begin
  if (Value = FDirectory) then exit;
  FDirectory := Value;
  SetEvent(fBreakEvent);
end;
//------------------------------------------------------------------------------

procedure TWatchThread.Terminate;
begin
  inherited Terminate;
  SetEvent(fBreakEvent);
  while not fFinished do sleep(10); //avoids a reported resource leak
                                    //if called while closing the application.
end;
//------------------------------------------------------------------------------

procedure TWatchThread.Execute;
begin
  //OUTER LOOP - manages Directory property reassignments
  while (not Terminated) do
  begin
    fChangeNotify := FindFirstChangeNotification(pchar(fDirectory),
      fWatchSubDirs, fNotifyFilters);
    if (fChangeNotify = INVALID_HANDLE_VALUE) then
      //Can't monitor the specified directory so we'll just wait for
      //a new Directory assignment or the thread terminating ...
      WaitForSingleObject(fBreakEvent, INFINITE)
    else
      try
        //Now do the INNER loop...
        ProcessFilenameChanges;
      finally
        FindCloseChangeNotification(fChangeNotify);
      end;
  end;
  fFinished := true;
end;
//------------------------------------------------------------------------------

procedure TWatchThread.ProcessFilenameChanges;
var
  WaitResult : DWORD;
  HandleArray : array[0..1] of THandle;
const
  TEN_MSECS = 10;
  HUNDRED_MSECS = 100;
begin
  HandleArray[0] := fBreakEvent;
  HandleArray[1] := fChangeNotify;
  //INNER LOOP - exits only when fBreakEvent signaled
  while (not Terminated) do
  begin
    //waits for either fChangeNotify or fBreakEvent ...
    WaitResult := WaitForMultipleObjects(2, @HandleArray, False, INFINITE);
    if (WaitResult = WAIT_OBJECT_0 + 1) then //fChangeNotify
    begin
      repeat //ie: if a number of files are changing in a block
             //just post the one notification message ...
        FindNextChangeNotification(fChangeNotify);
      until Terminated or
        (WaitForSingleObject(fChangeNotify, TEN_MSECS) <> WAIT_OBJECT_0);
      if Terminated then break;
      //OK, now notify the main thread (before restarting inner loop)...
      PostMessage(fOwnerHdl, NOTIFYCHANGE_MESSAGE, 0, 0);
    end else //fBreakEvent ...
    begin
      //If the Directory property is undergoing multiple rapid reassignments
      //wait 'til this stops before restarting monitoring of a new directory ...
      while (not Terminated) and
        (WaitForSingleObject(fBreakEvent, HUNDRED_MSECS) = WAIT_OBJECT_0) do;
      break; //EXIT LOOP HERE
    end;
  end;
end;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

end.