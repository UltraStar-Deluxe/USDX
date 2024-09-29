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
  Classes, SysUtils, fphttpserver, httproute, HTTPDefs, USongs, USong, UPlatform, UPath, fpjson, jsonparser;

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
    procedure routeSongsJSON(ARequest: TRequest; AResponse: TResponse);
    procedure routeFile(ARequest: TRequest; AResponse: TResponse);
    procedure routeCover(ARequest: TRequest; AResponse: TResponse);
    procedure route404(ARequest: TRequest; AResponse: TResponse);
    function ContentTypeForExt(Ext: IPath): string;
    function GenerateHTMLWithSongs: string;
    function GenerateJSONWithSongs: string;
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

procedure TWebServer.routeSongsJSON(ARequest: TRequest; AResponse: TResponse);
  var ResponseJSON: String;
begin
  AResponse.ContentType := 'application/json; charset=UTF-8';
  ResponseJSON := GenerateJSONWithSongs;
  AResponse.Content := ResponseJSON;
  AResponse.Code := 200;
end;

procedure TWebServer.route404(ARequest: TRequest; AResponse: TResponse);
begin
  if THTTPRouter.StringToRouteMethod(ARequest.Method) = rmGET then
    begin
      AResponse.ContentType := 'text/html; charset=UTF-8';
      AResponse.Content := '<html><body><h1>404 - File not found</h1></body></html>';
      AResponse.Code := 404;
    end
  else
    begin
      AResponse.ContentType := 'text/html; charset=UTF-8';
      AResponse.Content := '<html><body><h1>405 - Method Not Allowed</h1></body></html>';
      AResponse.Code := 405;
    end;
end;

procedure TWebServer.routeFile(ARequest: TRequest; AResponse: TResponse);
var
  FileName: string;
  WebFilePath: IPath;
  FileStream : TFileStream;
begin
  FileName := ARequest.URI;
  while FileName.StartsWith('/') do
    Delete(FileName, 1, 1);

  try
    try
      WebFilePath := Platform.GetGameUserPath.Append('resources\web\').Append(FileName);
      FileStream := TFileStream.Create(WebFilePath.ToNative, fmOpenRead or fmShareDenyWrite);

      AResponse.ContentType := ContentTypeForExt(WebFilePath.GetExtension);
      AResponse.ContentLength := FileStream.Size;
      AResponse.ContentStream := FileStream;
      AResponse.SendContent;
      AResponse.ContentStream:=Nil;
    except
      on E: Exception do begin
        AResponse.ContentType := 'text/html; charset=UTF-8';
        AResponse.Content := '<html><body><h1>404 - File not found</h1></body></html>';
        AResponse.Code := 404;
      end;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TWebServer.routeCover(ARequest: TRequest; AResponse: TResponse);
var
  Id: LongInt;
  Song: TSong;
  CoverPath: IPath;
  FileStream : TFileStream;
begin
  try
    try
      Id := StrToInt(ARequest.RouteParams['id']);

      if (Id < 0) or (Id >= Songs.SongList.Count) then
        raise Exception.Create('ID is out of bounds');
      
      Song := TSong(Songs.SongList[Id]);
      CoverPath := Song.Path.Append(Song.Cover);
      
      FileStream := TFileStream.Create(CoverPath.ToNative, fmOpenRead or fmShareDenyWrite);

      AResponse.ContentType := ContentTypeForExt(CoverPath.GetExtension);
      AResponse.ContentLength := FileStream.Size;
      AResponse.ContentStream := FileStream;
      AResponse.SendContent;
      AResponse.ContentStream:=Nil;
      
    except
      on E: Exception do begin
        AResponse.ContentType := 'text/html; charset=UTF-8';
        AResponse.Content := '<html><body><h1>404 - File not found</h1></body></html>';
        AResponse.Code := 404;
      end;
    end;
  finally
    FileStream.Free;
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
  FRouter.RegisterRoute('/cover/:id', rmGET, @routeCover);
  FRouter.RegisterRoute('/404', @route404, true);
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

    if (Edition = 'Unknown') or (Edition = 'None') then
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

function TWebServer.GenerateJSONWithSongs: string;
var
  I: Integer;
  Song: TSong;
  JSONRoot, Item: TJSONObject;
  JSONSongs: TJSONArray;
  Edition, Genre, Year: string;
begin
  JSONRoot := TJSONObject.Create;
  JSONSongs := TJSONArray.Create;
  JSONRoot.Add('songs', JSONSongs);

  for I := 0 to Songs.SongList.Count - 1 do
  begin
    Song := TSong(Songs.SongList[I]);
    Edition := UTF8Encode(Song.Edition);
    Genre := UTF8Encode(Song.Genre);
    Year := IntToStr(Song.Year);

    if (Edition = 'Unknown') or (Edition = 'None') then
      Edition := '';
    if Genre = 'Unknown' then
      Genre := '';
    if Year = '0' then
      Year := '';

    Item := TJSONObject.Create;
    Item.Add('artist', UTF8Encode(Song.Artist));
    Item.Add('title', UTF8Encode(Song.Title));
    Item.Add('lang', UTF8Encode(Song.Language));
    // Item.Add('cover', Song.Path.Append(Song.Cover).ToUTF8);
    Item.Add('edition', Edition);
    Item.Add('genre', Genre);
    Item.Add('year', Year);

    JSONSongs.Add(Item);
  end;

  Result := JSONRoot.AsJSON;
end;

function TWebServer.ContentTypeForExt(Ext: IPath): string;
begin
  // MIME types
  // see https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
  if Ext.Equals('.html', true) then
    Result := 'text/html; charset=UTF-8'
  else if Ext.Equals('.txt', true) then
    Result := 'text/plain; charset=UTF-8'
  else if Ext.Equals('.js', true) or Ext.Equals('.mjs', true) then
    Result := 'text/javascript; charset=UTF-8'
  else if Ext.Equals('.css', true) then
    Result := 'text/css; charset=UTF-8'
  else if Ext.Equals('.json', true) then
    Result := 'application/json; charset=UTF-8'
  else if Ext.Equals('.min.map', true) or Ext.Equals('.map', true) then
    Result := 'application/json; charset=UTF-8'
  else if Ext.Equals('.csv', true) then
    Result := 'text/csv; charset=UTF-8'
  else if Ext.Equals('.ttf', true) then
    Result := 'font/ttf'
  else if Ext.Equals('.woff', true) then
    Result := 'font/woff'
  else if Ext.Equals('.woff2', true) then
    Result := 'font/woff2'
  else if Ext.Equals('.otf', true) then
    Result := 'font/otf'
  else if Ext.Equals('.ico', true) then
    Result := 'image/vnd.microsoft.icon'
  else if Ext.Equals('.jpg', true) or Ext.Equals('.jpeg', true) then
    Result := 'image/jpeg'
  else if Ext.Equals('.png', true) then
    Result := 'image/png'
  else if Ext.Equals('.apng', true) then
    Result := 'image/apng'
  else if Ext.Equals('.avif', true) then
    Result := 'image/avif'
  else if Ext.Equals('.bmp', true) then
    Result := 'image/bmp'
  else if Ext.Equals('.gif', true) then
    Result := 'image/gif'
  else if Ext.Equals('.svg', true) then
    Result := 'image/svg+xml'
  else if Ext.Equals('.tif', true) or Ext.Equals('.tiff', true) then
    Result := 'image/tiff'
  else if Ext.Equals('.webp', true) then
    Result := 'image/webp'
  else if Ext.Equals('.aac', true) then
    Result := 'audio/aac'
  else if Ext.Equals('.mp3', true) then
    Result := 'audio/mpeg'
  else if Ext.Equals('.wav', true) then
    Result := 'audio/wav'
  else if Ext.Equals('.weba', true) then
    Result := 'audio/webm'
  else if Ext.Equals('.oga', true) or Ext.Equals('.opus', true) then
    Result := 'audio/ogg'
  else if Ext.Equals('.avi', true) then
    Result := 'video/x-msvideo'
  else if Ext.Equals('.mp4', true) then
    Result := 'video/mp4'
  else if Ext.Equals('.mpeg', true) then
    Result := 'video/mpeg'
  else if Ext.Equals('.ogv', true) then
    Result := 'video/ogg'
  else if Ext.Equals('.ts', true) then
    Result := 'video/mp2t'
  else if Ext.Equals('.webm', true) then
    Result := 'video/webm'
  else if Ext.Equals('.3gp', true) then
    Result := 'video/3gpp'
  else if Ext.Equals('.3g2', true) then
    Result := 'video/3gpp2'
  else if Ext.Equals('.ogx', true) then
    Result := 'application/ogg'
  else if Ext.Equals('.pdf', true) then
    Result := 'application/pdf'
  else if Ext.Equals('.xml', true) then
    Result := 'application/xml'
  else
    Result := 'Application/octet-stream';
end;

end.

