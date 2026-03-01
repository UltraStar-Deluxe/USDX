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
 * $URL: https://ultrastardx.svn.sourceforge.net/svnroot/ultrastardx/trunk/src/base/UDLLManager.pas $
 * $Id: UDLLManager.pas 2259 2009-11-09 00:27:55Z zup3r_vock $
 *}

unit UDLLManager;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UWebSDK,
  UFiles,
  UPath,
  UFilesystem;

type
  TDLLMan = class
    private
      hLibW:     THandle;

      P_SendScore:       fModi_SendScore;
      P_EncryptScore:    fModi_EncryptScore;
      P_Login:           fModi_Login;
      P_EncryptPassword: fModi_EncryptPassword;
      P_DownloadScore:   fModi_DownloadScore;
      P_VerifySong:      fModi_VerifySong;

    public
      Websites: array of TWebsiteInfo;
      WebsitePaths: array of IPath;
      SelectedW: ^TWebsiteInfo;

      constructor Create;

      procedure GetWebsiteList;

      procedure ClearWebsiteInfo(No: cardinal);
      function  LoadWebsiteInfo(const Filename: IPath; No: cardinal): boolean;

      function  LoadWebsite(No: cardinal): boolean;
      procedure UnLoadWebsite;

      function  WebsiteSendScore (var SendInfo: TSendInfo): byte;
      function  WebsiteEncryptScore (var SendInfo: TSendInfo): UTF8String;
      function  WebsiteLogin (var LoginInfo: TLoginInfo): byte;
      function  WebsiteEncryptPassword (var LoginInfo: TLoginInfo): UTF8String;
      function  WebsiteDownloadScore (List_MD5Song: UTF8String; Level: byte): UTF8String;
      function  WebsiteVerifySong (MD5Song: UTF8String): UTF8String;
  end;

var
  DLLMan: TDLLMan;

const
{$IF Defined(MSWINDOWS)}
  DLLExt  = '.dll';
{$ELSEIF Defined(DARWIN)}
  DLLExt  = '.dylib';
{$ELSEIF Defined(UNIX)}
  DLLExt  = '.so';
{$IFEND}

implementation

uses
  {$IFDEF MSWINDOWS}
  windows,
  {$ELSE}
  dynlibs,
  {$ENDIF}
  UPathUtils,
  ULog,
  SysUtils;


constructor TDLLMan.Create;
begin
  inherited;

  SetLength(Websites, 0);
  SetLength(WebsitePaths, Length(Websites));
  GetWebsiteList;
end;

procedure TDLLMan.GetWebsiteList;
var
  Iter: IFileIterator;
  FileInfo: TFileInfo;
begin
  Iter := FileSystem.FileFind(WebsitePath.Append('*' + DLLExt), 0);
  while (Iter.HasNext) do
  begin
    SetLength(Websites, Length(Websites)+1);
    SetLength(WebsitePaths, Length(Websites));

    FileInfo := Iter.Next;

    if LoadWebsiteInfo(FileInfo.Name, High(Websites)) and (Websites[High(Websites)].Name <> '') then // loaded succesful
    begin
      WebsitePaths[High(WebsitePaths)] := FileInfo.Name;
    end
    else // error loading
    begin
      SetLength(Websites, Length(Websites)-1);
      SetLength(WebsitePaths, Length(Websites));
    end;
  end;
end;

procedure TDLLMan.ClearWebsiteInfo(No: cardinal);
begin
  Websites[No].Name := '';
end;

function TDLLMan.LoadWebsiteInfo(const Filename: IPath; No: cardinal): boolean;
var
  hLibg: THandle;
  Info: pModi_WebsiteInfo;
begin
  Result := true;

  // clear website info
  ClearWebsiteInfo(No);

  // load libary
  hLibg := LoadLibrary(PAnsiChar(WebsitePath.Append(Filename).ToNative));
  // if loaded
  if (hLibg <> 0) then
  begin
    // load info procedure
    @Info := GetProcAddress(hLibg, PChar('WebsiteInfo'));

    // if loaded
    if (@Info <> nil) then
    begin
      // load website info
      Info(Websites[No]);
      Result := true;
    end
    else
      Log.LogError('Could not load website "' + Filename.ToNative + '": Info procedure not found');

    FreeLibrary (hLibg);
  end
  else
    Log.LogError('Could not load website "' + Filename.ToNative + '": Library not loaded');
end;

function TDLLMan.LoadWebsite(No: cardinal): boolean;
begin
  Result := true;
  // load libary
  hLibW := LoadLibrary(PChar(WebsitePath.Append(WebsitePaths[No]).ToNative));
  // if loaded
  if (hLibW <> 0) then
  begin
    // load info procedure
    @P_Login := GetProcAddress (hLibW, 'Login');
    @P_SendScore := GetProcAddress (hLibW, 'SendScore');
    @P_EncryptScore := GetProcAddress (hLibW, 'EncryptScore');
    @P_EncryptPassword := GetProcAddress (hLibW, 'EncryptPassword');
    @P_VerifySong := GetProcAddress (hLibW, 'VerifySong');
    @P_DownloadScore := GetProcAddress (hLibW, 'DownloadScore');

    if (@P_EncryptScore <> nil) and (@P_SendScore <> nil) and (@P_Login <> nil) and (@P_EncryptPassword <> nil) and (@P_VerifySong <> nil) and (@P_DownloadScore <> nil) then
    begin
      SelectedW := @Websites[No];
      Result := true;
    end
    else
    begin
      Log.LogError('Could not load website "' + WebsitePaths[No].ToNative + '": Basic Procedures not found');
    end;
  end
  else
    Log.LogError('Could not load website "' + WebsitePaths[No].ToNative + '": Library not loaded');
end;

procedure TDLLMan.UnLoadWebsite;
begin
  if (hLibW <> 0) then
    FreeLibrary (hLibW);

  SelectedW := nil;
  @P_SendScore       := nil;
  @P_Login           := nil;
  @P_EncryptScore    := nil;
  @P_EncryptPassword := nil;
  @P_DownloadScore   := nil;
  @P_VerifySong      := nil;
end;

function TDLLMan.WebsiteSendScore (var SendInfo: TSendInfo): byte;
begin
  if (@P_SendScore <> nil) then
    Result := P_SendScore (SendInfo)
  else
    Result := 0;
end;

function TDLLMan.WebsiteEncryptScore (var SendInfo: TSendInfo): UTF8String;
var
  WideResult: WideString;
begin
  if (@P_EncryptScore <> nil) then
  begin
    WideResult := P_EncryptScore(SendInfo);
    Result := UTF8Encode(WideResult);
  end
  else
    Result := '';
end;

function TDLLMan.WebsiteLogin (var LoginInfo: TLoginInfo): byte;
begin
  if (@P_Login <> nil) then
    Result := P_Login (LoginInfo)
  else
    Result := 0;
end;

function TDLLMan.WebsiteEncryptPassword (var LoginInfo: TLoginInfo): UTF8String;
var
  WideResult: WideString;
begin
  if (@P_EncryptPassword <> nil) then
  begin
    WideResult := P_EncryptPassword(LoginInfo);
    Result := UTF8Encode(WideResult);
  end
  else
    Result := '';
end;

function TDLLMan.WebsiteDownloadScore (List_MD5Song: UTF8String; Level: byte): UTF8String;
var
  WideList: WideString;
  WideResult: WideString;
begin
  if (@P_DownloadScore <> nil) then
  begin
    WideList := UTF8Decode(List_MD5Song);
    WideResult := P_DownloadScore(WideList, Level);
    Result := UTF8Encode(WideResult);
  end
  else
    Result := '';
end;

function TDLLMan.WebsiteVerifySong (MD5Song: UTF8String): UTF8String;
var
  WideMd5: WideString;
  WideResult: WideString;
begin
  if (@P_VerifySong <> nil) then
  begin
    WideMd5 := UTF8Decode(MD5Song);
    WideResult := P_VerifySong(WideMd5);
    Result := UTF8Encode(WideResult);
  end
  else
    Result := '';
end;

end.
