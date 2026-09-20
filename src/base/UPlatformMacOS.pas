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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/base/UPlatformMacOSX.pas $
 * $Id: UPlatformMacOSX.pas 3018 2013-12-06 21:48:55Z k-m_schindler $
 *}

unit UPlatformMacOS;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  Classes,
  ULog,
  UPlatform,
  UFilesystem,
  UPath,
  UConfig;

type
  TPlatformMacOS = class(TPlatform)
    private
      IsBundle: boolean;
      IsLocal: boolean;
      BundleDir: IPath;
      ExecutionDir: IPath;

      procedure DetectExecutionType();

      {**
       * GetApplicationSupportPath returns the path to
       * $HOME/Library/Application Support/UltraStarDeluxe_[USDX_VERSION].
       *}
      function GetApplicationSupportPath: IPath;

      {**
       * GetHomeDir returns the path to $HOME.
       *}
      function GetHomeDir: IPath;

    public
      {**
       * Init simply calls @link(CreateUserFolders), which in turn scans the
       * folder UltraStarDeluxe.app/Contents for all files and
       * folders. $HOME/Library/Application Support/UltraStarDeluxe_[USDX_VERSION]
       * is then checked for their presence and missing ones are copied.
       *}
      procedure Init; override;

      {**
       * GetLogPath returns the path for log messages. Currently it is set to
       * $HOME/Library/Logs/UltraStar Deluxe/.
       *}
      function  GetLogPath:        IPath; override;

      {**
       * GetMusicPath returns the path for music. Currently it is set to
       * $HOME/Music/UltraStar Deluxe/.
       *}
      function  GetMusicPath:      IPath; override;

      {**
       * GetGameSharedPath returns the path for shared resources. Currently it
       * is also set to $HOME/Library/Application Support/UltraStarDeluxe_[USDX_VERSION].
       * However it is not used.
       *}
      function  GetGameSharedPath: IPath; override;

      {**
       * GetGameUserPath returns the path for user resources. Currently it is
       * set to $HOME/Library/Application Support/UltraStarDeluxe_[USDX_VERSION].
       * This is where a user can add themes, ....
       *}
      function  GetGameUserPath:   IPath; override;
      function GetModifiableAssetPaths: IInterfaceList; override;
      function GetWebsitePaths: IInterfaceList; override;
      function GetBacktraceFunction: TBacktraceStrFunc; override;
  end;

implementation
{$POINTERMATH ON}

uses
  ctypes,
  SysUtils,
  Process,
  MacOSAll;

type
  Dl_info = record
    dli_fname: PAnsiChar;
    dli_fbase: Pointer;
    dli_sname: PAnsiChar;
    dli_saddr: Pointer;
  end;
  PDl_info = ^Dl_info;
function dladdr(addr: Pointer; info: PDl_info): cint; cdecl; external 'c';

const
  {$I paths.inc}

function BackTraceStrFuncMac(Addr: CodePointer): ShortString; forward;

procedure TPlatformMacOS.Init;
begin
  inherited;
  DetectExecutionType();
end;

procedure TPlatformMacOS.DetectExecutionType();
var
  LanguageDir: IPath;
begin
  // we just check if the 'languages' folder exists in the
  // directory of the executable. If so -> local execution.
  ExecutionDir := GetExecutionDir();
  LanguageDir := ExecutionDir.Append('languages');
  IsLocal := LanguageDir.IsDirectory and not ExecutionDir.IsReadonly;
  BundleDir := ExecutionDir.GetParent().GetParent();
  if string(BundleDir.RemovePathDelim().ToUTF8()).EndsWith('.app') then
    IsBundle := true
  else
    BundleDir := PATH_NONE;
end;

function TPlatformMacOS.GetHomeDir: IPath;
begin
  Result := Path(GetEnvironmentVariable('HOME'));
end;

function TPlatformMacOS.GetApplicationSupportPath: IPath;
begin
// append the version for conflict resolution
  Result := GetHomeDir.Append('Library/Application Support/UltraStar Deluxe', pdAppend);
end;

function TPlatformMacOS.GetLogPath: IPath;
begin
  if (IsLocal) then
    Result := Path(ExecutionDir.ToNative())
  else
    Result := GetHomeDir.Append('Library/Logs/UltraStar Deluxe', pdAppend)
end;

function TPlatformMacOS.GetMusicPath: IPath;
begin
  Result := GetHomeDir.Append('Music/UltraStar Deluxe', pdAppend);
end;

function TPlatformMacOS.GetGameSharedPath: IPath;
begin
  if (IsBundle) then
    Result := BundleDir.Append('Contents')
  else if (IsLocal) then
    Result := Path(ExecutionDir.ToNative())
  else
    Result := Path(INSTALL_DATADIR);
end;

function TPlatformMacOS.GetGameUserPath: IPath;
begin
  if (IsLocal) then
    Result := Path(ExecutionDir.ToNative())
  else
    Result := GetApplicationSupportPath;
end;

function TPlatformMacOS.GetModifiableAssetPaths: IInterfaceList;
begin
  Result := TInterfaceList.Create;
  if (IsLocal) then
    Result.Add(Path(ExecutionDir.ToNative()))
  else if (IsBundle) then
  begin
    Result.Add(GetApplicationSupportPath());
    Result.Add(BundleDir.Append('Contents'));
  end
  else
  begin
    Result.Add(GetApplicationSupportPath());
    Result.Add(Path(INSTALL_DATADIR));
  end;
end;

function TPlatformMacOS.GetWebsitePaths: IInterfaceList;
begin
  Result := TInterfaceList.Create;
  if (IsLocal) then
    Result.Add(Path(ExecutionDir.ToNative()))
  else
    Result.Add(GetApplicationSupportPath());
end;

function TPlatformMacOS.GetBacktraceFunction: TBacktraceStrFunc;
begin
  Result := BackTraceStrFuncMac;
end;

function BackTraceStrFuncMac(Addr: CodePointer): ShortString;
var
  {$IFDEF DEBUG_MODE}
  Output: string;
  {$ENDIF}
  info: Dl_info;
  Offset: Int64;
  SymbolName: string;
begin
  Result := '  ';
  if (dladdr(Addr, @info) <> 0) then
  begin
    {$IFDEF DEBUG_MODE}
    // For developers running self-compiled builds, the atos utility can provide the full stacktrace including source line numbers
    if RunCommand('/usr/bin/atos', ['-o', info.dli_fname, '-l', Format('%p', [info.dli_fbase]), Format('%p', [Addr])], Output) then
      Result := Result + StringReplace(Output, #10, '', [rfReplaceAll])

    // For others (e.g. non-developers running a release build), the best we can do is report the symbol names
    else
    begin
    {$ENDIF}
      Offset := Addr - info.dli_saddr;
      if (info.dli_sname <> nil) then
        SymbolName := info.dli_sname
      else
        SymbolName := '???';
      Result := Result + Format('%s (in %s) + %d', [SymbolName, Path(info.dli_fname).GetName().ToUTF8(), Offset]);
    {$IFDEF DEBUG_MODE}
    end;
    {$ENDIF}
  end
  else
    Result := Result + Format('%p', [Addr]);
end;

end.
