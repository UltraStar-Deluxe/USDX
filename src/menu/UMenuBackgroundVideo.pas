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
    ReferenceCounter: Cardinal; //Number of Creations
  end;

  TBGVideo = class
    private
      myItem: PBGVideoPoolItem;
    public
      Constructor Create(Item: PBGVideoPoolItem); override;

      Function    GetVideoPlayback: IVideoPlayback;
      Procedure   Draw;

      Destructor  Destroy;
  end;

  TBGVideoPool = class
    private
      Items: PBGVideoPoolItem;
    public
      Constructor Create;

      Function    GetBGVideo(filename: String): TBGVideo;
      Procedure   RemoveItem(
      Procedure   FreeAllItems;

      Destructor  Destroy;
  end;
        
type }
  TMenuBackgroundVideo = class (TMenuBackground)
    private
      fFilename: String;
    public
      Constructor Create(const ThemedSettings: TThemeBackground); override;
      Procedure   OnShow; override;
      Procedure   Draw; override;
      Procedure   OnFinish; override;
      Destructor  Destroy; override;
  end;

{var
  BGVideoPool: TBGVideoPool;  }
const
  SUPPORTED_EXTS_BACKGROUNDVIDEO: Array[0..6] of String = ('.avi', '.mov', '.divx', '.mpg', '.mp4', '.mpeg', '.m2v');

implementation

uses
  gl,
  glext,
  UMusic,
  SysUtils,
  UTime,
  USkins,
  UCommon;

Constructor TMenuBackgroundVideo.Create(const ThemedSettings: TThemeBackground);
begin
  inherited;
  If (Length(ThemedSettings.Tex) = 0) then
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

Destructor  TMenuBackgroundVideo.Destroy;
begin

end;


Procedure   TMenuBackgroundVideo.OnShow;
begin
  if VideoPlayback.Open( fFileName ) then
  begin
    VideoBGTimer.SetTime(0);
    VideoPlayback.Play;
  end;
end;

Procedure   TMenuBackgroundVideo.OnFinish;
begin

end;


Procedure   TMenuBackgroundVideo.Draw;
begin
  glClear(GL_DEPTH_BUFFER_BIT);

  VideoPlayback.GetFrame(VideoBGTimer.GetTime());
    // FIXME: why do we draw on screen 2? Seems to be wrong.
  VideoPlayback.DrawGL(2);
end;

// Implementation of TBGVideo
//--------
{Constructor TBGVideo.Create(Item: PBGVideoPoolItem);
begin
  myItem := PBGVideoPoolItem;
  Inc(myItem.ReferenceCounter);
end;

Destructor  TBGVideo.Destroy;
begin
  Dec(myItem.ReferenceCounter);
end;

Function    TBGVideo.GetVideoPlayback: IVideoPlayback;
begin

end;

Procedure   TBGVideo.Draw;
begin

end;

// Implementation of TBGVideoPool
//--------

Constructor TBGVideoPool.Create;
begin

end;

Destructor  TBGVideoPool.Destroy;
begin

end;

Function    TBGVideoPool.GetBGVideo(filename: String): TBGVideo;
begin

end;

Procedure   TBGVideoPool.FreeAllItems;
begin

end;  }

end.
