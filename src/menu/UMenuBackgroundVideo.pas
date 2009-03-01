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

unit UMenuBackgroundVideo;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UThemes,
  UMenuBackground,
  UVideo;

//TMenuBackgroundColor - Background Color
//--------

type
  //DefaultBGVideoPlayback = TVideoPlayback_FFmpeg;

{type
  TBGVideoPool = class;

  PBGVideoPoolItem = ^TBGVideoPoolItem;
  TBGVideoPoolItem = record
    Parent: TBGVideoPool;
    VideoPlayback = IVideoPlayback;
    ReferenceCounter: cardinal; //Number of Creations
  end;

  TBGVideo = class
    private
      myItem: PBGVideoPoolItem;
    public
      constructor Create(Item: PBGVideoPoolItem); override;

      function    GetVideoPlayback: IVideoPlayback;
      procedure   Draw;

      destructor  Destroy;
  end;

  TBGVideoPool = class
    private
      Items: PBGVideoPoolItem;
    public
      constructor Create;

      function    GetBGVideo(filename: string): TBGVideo;
      procedure   RemoveItem(
      procedure   FreeAllItems;

      destructor  Destroy;
  end;

type }
  TMenuBackgroundVideo = class (TMenuBackground)
    private
      fFilename: string;
    public
      constructor Create(const ThemedSettings: TThemeBackground); override;
      procedure   OnShow; override;
      procedure   Draw; override;
      procedure   OnFinish; override;
      destructor  Destroy; override;
  end;

{var
  BGVideoPool: TBGVideoPool;  }
const
  SUPPORTED_EXTS_BACKGROUNDVIDEO: array[0..6] of string = ('.avi', '.mov', '.divx', '.mpg', '.mp4', '.mpeg', '.m2v');

implementation

uses
  gl,
  glext,
  UMusic,
  SysUtils,
  UTime,
  USkins,
  UCommon,
  UGraphic;

constructor TMenuBackgroundVideo.Create(const ThemedSettings: TThemeBackground);
begin
  inherited;
  if (Length(ThemedSettings.Tex) = 0) then
    raise EMenuBackgroundError.Create('TMenuBackgroundVideo: No video filename present');

  fFileName := Skin.GetTextureFileName(ThemedSettings.Tex);
  fFileName := AdaptFilePaths( fFileName );

  if fileexists(fFilename) AND VideoPlayback.Open( fFileName ) then
  begin
    VideoBGTimer.SetTime(0);
    VideoPlayback.Play;
  end
  else
    raise EMenuBackgroundError.Create('TMenuBackgroundVideo: Can''t load background video: ' + fFilename);
end;

destructor  TMenuBackgroundVideo.Destroy;
begin

end;

procedure   TMenuBackgroundVideo.OnShow;
begin
  if VideoPlayback.Open( fFileName ) then
  begin
    VideoBGTimer.SetTime(0);
    VideoPlayback.Play;
  end;
end;

procedure   TMenuBackgroundVideo.OnFinish;
begin

end;

procedure   TMenuBackgroundVideo.Draw;
begin
  If (ScreenAct = 1) then //Clear just once when in dual screen mode
    glClear(GL_DEPTH_BUFFER_BIT);

  VideoPlayback.GetFrame(VideoBGTimer.GetTime());
    // FIXME: why do we draw on screen 2? Seems to be wrong.
  VideoPlayback.DrawGL(2);
end;

// Implementation of TBGVideo
//--------
{constructor TBGVideo.Create(Item: PBGVideoPoolItem);
begin
  myItem := PBGVideoPoolItem;
  Inc(myItem.ReferenceCounter);
end;

destructor  TBGVideo.Destroy;
begin
  Dec(myItem.ReferenceCounter);
end;

function    TBGVideo.GetVideoPlayback: IVideoPlayback;
begin

end;

procedure   TBGVideo.Draw;
begin

end;

// Implementation of TBGVideoPool
//--------

constructor TBGVideoPool.Create;
begin

end;

destructor  TBGVideoPool.Destroy;
begin

end;

function    TBGVideoPool.GetBGVideo(filename: string): TBGVideo;
begin

end;

procedure   TBGVideoPool.FreeAllItems;
begin

end;  }

end.
