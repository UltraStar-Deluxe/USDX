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
  UMusic,
  UVideo,
  UPath;

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

      function    GetBGVideo(filename: IPath): TBGVideo;
      procedure   RemoveItem(
      procedure   FreeAllItems;

      destructor  Destroy;
  end;

type }
  TMenuBackgroundVideo = class (TMenuBackground)
    private
      fFilename: IPath;
      fBgVideo: IVideo;
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
  if (not fFilename.IsFile) then
    raise EMenuBackgroundError.Create('TMenuBackgroundVideo: Can''t load background video: ' + fFilename.ToNative);
end;

destructor  TMenuBackgroundVideo.Destroy;
begin
end;

procedure TMenuBackgroundVideo.OnShow;
begin
  fBgVideo := VideoPlayback.Open(fFileName);
  if (fBgVideo <> nil) then
  begin
    VideoBGTimer.SetTime(0);
    VideoBGTimer.Start();
    fBgVideo.Loop := true;
    fBgVideo.Play;
  end;
end;

procedure   TMenuBackgroundVideo.OnFinish;
begin
  // unload video
  fBgVideo := nil;
end;

procedure TMenuBackgroundVideo.Draw;
begin
  // clear just once when in dual screen mode
  if (ScreenAct = 1) then
  begin
    glClear(GL_DEPTH_BUFFER_BIT);
    // video failure -> draw blank background
    if (fBgVideo = nil) then
      glClear(GL_COLOR_BUFFER_BIT);    
  end;

  if (fBgVideo <> nil) then
  begin
    fBgVideo.GetFrame(VideoBGTimer.GetTime());
    // FIXME: why do we draw on screen 2? Seems to be wrong.
    fBgVideo.DrawGL(2);
  end;
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

function    TBGVideoPool.GetBGVideo(filename: IPath): TBGVideo;
begin

end;

procedure   TBGVideoPool.FreeAllItems;
begin

end;  }

end.
