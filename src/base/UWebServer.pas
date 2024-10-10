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
  Classes, SysUtils, fphttpserver, HTTPDefs, USongs, USong, UPlatform, UPath;

type
  TWebServer = class(TThread)
  private
    FServer: TFPHTTPServer;
    FPort: Integer;
  protected
    procedure Execute; override;
    procedure HandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
                             var AResponse: TFPHTTPConnectionResponse);
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

procedure TWebServer.Execute;
begin
  FServer := TFPHTTPServer.Create(nil);
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
var
  ResponseHTML: string;
begin
  if ARequest.Method = 'GET' then
  begin
    // Serve the HTML page with the list of songs
    AResponse.ContentType := 'text/html; charset=UTF-8';
    ResponseHTML := GenerateHTMLWithSongs;
    AResponse.Content := ResponseHTML;
    AResponse.Code := 200;
  end
  else
  begin
    // Method not allowed
    AResponse.Content := '<h1>Method Not Allowed</h1>';
    AResponse.ContentType := 'text/html; charset=UTF-8';
    AResponse.Code := 405;
  end;
end;

function TWebServer.LoadTemplate: string;
var
  TemplateFilePath: IPath;
  TemplateFile: TStringList;
begin
  TemplateFile := TStringList.Create;
  try
    TemplateFilePath := Platform.GetGameUserPath.Append('resources\songlist_template.html');
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

