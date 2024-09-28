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
 *}

unit UWebServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httproute, HTTPDefs, USongs, USong, UPlatform, UPath;

type
  TWebServer = class(TThread)
  private
    FServer: TFPHTTPServer;
    FRouter: THTTPRouter;
    FPort: Integer;
  protected
    procedure Execute; override;
    procedure HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
                             var AResponse: TFPHTTPConnectionResponse);
    procedure routeSongList(ARequest: TRequest; AResponse: TResponse);
    procedure routeFile(ARequest: TRequest; AResponse: TResponse);
    function GenerateHTMLWithSongs: string;
    function LoadTemplate: string;
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;
  end;

implementation

constructor TWebServer.Create(APort: Integer);
begin
  inherited Create(True); // Create suspended
  FPort := APort;
  FreeOnTerminate := True;
end;

destructor TWebServer.Destroy;
begin
  inherited Destroy;
end;

procedure TWebServer.routeSongList(ARequest: TRequest; AResponse: TResponse);
  var ResponseHTML: String;
begin
  AResponse.ContentType := 'text/html; charset=UTF-8';
  ResponseHTML := GenerateHTMLWithSongs;
  AResponse.Content := ResponseHTML;
  AResponse.Code := 200;
end;

procedure TWebServer.routeFile(ARequest: TRequest; AResponse: TResponse);
var
  FileName: string;
  WebFilePath: IPath;
  WebFile: TStringList;
begin
  FileName := ARequest.URI;
  while FileName.StartsWith('/') do
    Delete(FileName, 1, 1);
  
  WebFile := TStringList.Create;

  try
    try
      WebFilePath := Platform.GetGameUserPath.Append('resources\web\').Append(FileName);
      WebFile.LoadFromFile(WebFilePath.toNative);

      // MIME type
      // see https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
      if (FileName.EndsWith('.html')) then
        AResponse.ContentType := 'text/html; charset=UTF-8'
      else if (FileName.EndsWith('.js')) then
        AResponse.ContentType := 'text/javascript; charset=UTF-8'
      else if (FileName.EndsWith('.css')) then
        AResponse.ContentType := 'text/css; charset=UTF-8'
      else if (FileName.EndsWith('.min.map')) then
        AResponse.ContentType := 'application/json; charset=UTF-8';
      
      AResponse.Content := WebFile.Text;
      AResponse.Code := 200;
    except
      on E: Exception do begin
        AResponse.ContentType := 'text/html; charset=UTF-8';
        AResponse.Content := '<html><body><h1>404 - File not found</h1></body></html>';
        AResponse.Code := 404;
      end;
    end;
  finally
    WebFile.Free;
  end;
end;

procedure TWebServer.Execute;
begin
  FServer := TFPHTTPServer.Create(nil);
  FRouter := THTTPRouter.Create(nil);
  FRouter.RegisterRoute('/', rmGET, @routeSongList);
  FRouter.RegisterRoute('/style.css', rmGET, @routeFile);
  FRouter.RegisterRoute('/theme.js', rmGET, @routeFile);
  FRouter.RegisterRoute('/songs.json', rmGET, @routeSongsJSON);
  FRouter.RegisterRoute('/jquery.min.js', rmGET, @routeFile);
  FRouter.RegisterRoute('/jquery.min.map', rmGET, @routeFile);
  FRouter.RegisterRoute('/datatables.min.js', rmGET, @routeFile);
  FRouter.RegisterRoute('/datatables.min.css', rmGET, @routeFile);
  try
    FServer.OnRequest := @HandleRequest;
    FServer.Port := FPort;
    FServer.Active := True;
    WriteLn('Server is running on port ', FPort);
    while not Terminated do
      Sleep(100);
  finally
    FServer.Free;
  end;
end;

procedure TWebServer.HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
                                   var AResponse: TFPHTTPConnectionResponse);
begin
  FRouter.RouteRequest(ARequest, AResponse);
end;

function TWebServer.LoadTemplate: string;
var
  TemplateFilePath: IPath;
  TemplateFile: TStringList;
begin
  TemplateFile := TStringList.Create;
  try
    TemplateFilePath := Platform.GetGameUserPath.Append('resources\web\index.html');
    TemplateFile.LoadFromFile(TemplateFilePath.toNative);
    Result := TemplateFile.Text;
  finally
    TemplateFile.Free;
  end;
end;

function TWebServer.GenerateHTMLWithSongs: string;
var
  I: Integer;
  SongRows: string;
  Edition, Genre, Year: string;
begin
  SongRows := '';
  for I := 0 to Songs.SongList.Count - 1 do
  begin
    Edition := UTF8Encode(TSong(Songs.SongList[I]).Edition);
    Genre := UTF8Encode(TSong(Songs.SongList[I]).Genre);
    Year := IntToStr(TSong(Songs.SongList[I]).Year);

    if Edition = 'Unknown' then
      Edition := '';
    if Genre = 'Unknown' then
      Genre := '';
    if Year = '0' then
      Year := '';

    SongRows := SongRows + '<tr>' + sLineBreak +
                '<td>' + UTF8Encode(TSong(Songs.SongList[I]).Artist) + '</td>' + sLineBreak +
                '<td>' + UTF8Encode(TSong(Songs.SongList[I]).Title) + '</td>' + sLineBreak +
                '<td>' + Edition + '</td>' + sLineBreak +
                '<td>' + Genre + '</td>' + sLineBreak +
                '<td>' + Year + '</td>' + sLineBreak +
                '</tr>' + sLineBreak;
  end;

  Result := LoadTemplate;
  Result := StringReplace(Result, '<!--SONG_ROWS-->', SongRows, []);
end;

end.

