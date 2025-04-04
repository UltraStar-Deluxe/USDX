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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/media/UAudioConverter.pas $
 * $Id: UAudioConverter.pas 3031 2013-12-15 21:08:54Z k-m_schindler $
 *}

unit UAudioConverter;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMusic,
  ULog,
  ctypes,
  avcodec,
  avutil,
  UMediaCore_FFmpeg,
  swresample,
  UMediaCore_SDL,
  sdl2,
  SysUtils,
  Math;

type
  {*
   * Notes:
   *  - 44.1kHz to 48kHz conversion or vice versa is not supported
   *    by SDL 1.2 (will be introduced in 1.3).
   *    No conversion takes place in this cases.
   *    This is because SDL just converts differences in powers of 2.
   *    So the result might not be that accurate.
   *    This IS audible (voice to high/low) and it needs good synchronization
   *    with the video or the lyrics timer.
   *  - float<->int16 conversion is not supported (will be part of 1.3) and
   *    SDL (<1.3) is not capable of handling floats at all.
   *  -> Using FFmpeg for resampling is preferred.
   *     Use SDL for channel and format conversion only.
   *}
  TAudioConverter_SDL = class(TAudioConverter)
    private
      cvt: TSDL_AudioCVT;
    public
      function Init(SrcFormatInfo: TAudioFormatInfo; DstFormatInfo: TAudioFormatInfo): boolean; override;
      destructor Destroy(); override;

      function Convert(InputBuffer: PByteArray; OutputBuffer: PByteArray; var InputSize: integer): integer; override;
      function GetOutputBufferSize(InputSize: integer): integer; override;
      function GetRatio(): double; override;
  end;

  TAudioConverter_SWResample = class(TAudioConverter)
    private
      SwrContext: PSwrContext;
      Ratio: double;
    public
      function Init(SrcFormatInfo: TAudioFormatInfo; DstFormatInfo: TAudioFormatInfo): boolean; override;
      destructor Destroy(); override;

      function Convert(InputBuffer: PByteArray; OutputBuffer: PByteArray; var InputSize: integer): integer; override;
      function GetOutputBufferSize(InputSize: integer): integer; override;
      function GetRatio(): double; override;
  end;

implementation

uses
  UConfig;

function TAudioConverter_SDL.Init(srcFormatInfo: TAudioFormatInfo; dstFormatInfo: TAudioFormatInfo): boolean;
var
  srcFormat: UInt16;
  dstFormat: UInt16;
begin
  inherited Init(SrcFormatInfo, DstFormatInfo);

  Result := false;

  if not ConvertAudioFormatToSDL(srcFormatInfo.Format, srcFormat) or
     not ConvertAudioFormatToSDL(dstFormatInfo.Format, dstFormat) then
  begin
    Log.LogError('Audio-format not supported by SDL', 'TSoftMixerPlaybackStream.InitFormatConversion');
    Exit;
  end;

  if SDL_BuildAudioCVT(@cvt,
    srcFormat, srcFormatInfo.Channels, Round(srcFormatInfo.SampleRate),
    dstFormat, dstFormatInfo.Channels, Round(dstFormatInfo.SampleRate)) = -1 then
  begin
    Log.LogError(SDL_GetError(), 'TSoftMixerPlaybackStream.InitFormatConversion');
    Exit;
  end;

  Result := true;
end;

destructor TAudioConverter_SDL.Destroy();
begin
  // nothing to be done here
  inherited;
end;

(*
 * Returns the size of the output buffer. This might be bigger than the actual
 * size of resampled audio data.
 *)
function TAudioConverter_SDL.GetOutputBufferSize(InputSize: integer): integer;
begin
  // Note: len_ratio must not be used here. Even if the len_ratio is 1.0, len_mult might be 2.
  // Example: 44.1kHz/mono to 22.05kHz/stereo -> len_ratio=1, len_mult=2
  Result := InputSize * cvt.len_mult;
end;

function TAudioConverter_SDL.GetRatio(): double;
begin
  Result := cvt.len_ratio;
end;

function TAudioConverter_SDL.Convert(InputBuffer: PByteArray; OutputBuffer: PByteArray;
                                     var InputSize: integer): integer;
begin
  Result := -1;

  if InputSize <= 0 then
  begin
    // avoid div-by-zero problems
    if InputSize = 0 then
      Result := 0;
    Exit;
  end;

  // OutputBuffer is always bigger than or equal to InputBuffer
  Move(InputBuffer[0], OutputBuffer[0], InputSize);
  cvt.buf := PUint8(OutputBuffer);
  cvt.len := InputSize;
  if SDL_ConvertAudio(@cvt) = -1 then
    Exit;

  Result := cvt.len_cvt;
end;

{$IF LIBAVUTIL_VERSION >= 59000000}
procedure SetDefaultChannelLayout(Ctx: PSwrContext; Option: PAnsiChar; Channels: cint);
var
  Layout: TAVChannelLayout;
begin
  av_channel_layout_default(@Layout, Channels);
  av_opt_set_chlayout(Ctx, Option, @Layout, 0);
end;
{$ENDIF}

function TAudioConverter_SWResample.Init(SrcFormatInfo: TAudioFormatInfo;
                                     DstFormatInfo: TAudioFormatInfo): boolean;
var
  SrcFormat: TAVSampleFormat;
  DstFormat: TAVSampleFormat;
begin
  inherited Init(SrcFormatInfo, DstFormatInfo);

  Result := false;

  if not TMediaCore_FFmpeg.ConvertAudioFormatToFFmpeg(SrcFormatInfo.Format, SrcFormat) then
  begin
    Log.LogError('Unsupported format', 'TAudioConverter_FFmpeg.Init');
    Log.LogError('srcFormatInfo.Format: ' + intToStr(integer(srcFormatInfo.Format)),
                 'TAudioConverter_FFmpeg.Init');
    Exit;
  end;

  if not TMediaCore_FFmpeg.ConvertAudioFormatToFFmpeg(DstFormatInfo.Format, DstFormat) then
  begin
    Log.LogError('Unsupported format', 'TAudioConverter_FFmpeg.Init');
    Log.LogError('dstFormatInfo.Format: ' + intToStr(integer(dstFormatInfo.Format)),
                 'TAudioConverter_FFmpeg.Init');
    Exit;
  end;

  SwrContext:= swr_alloc();
  {$IF LIBAVUTIL_VERSION >= 59000000}
  SetDefaultChannelLayout(SwrContext, 'in_chlayout', SrcFormatInfo.Channels);
  SetDefaultChannelLayout(SwrContext, 'out_chlayout', DstFormatInfo.Channels);
  {$ELSE}
  av_opt_set_int(SwrContext, 'in_channel_count', SrcFormatInfo.Channels, 0);
  av_opt_set_int(SwrContext, 'out_channel_count', DstFormatInfo.Channels, 0);
  {$ENDIF}
  av_opt_set_int(SwrContext, 'in_sample_rate', Round(SrcFormatInfo.SampleRate), 0);
  av_opt_set_int(SwrContext, 'out_sample_rate', Round(DstFormatInfo.SampleRate), 0);
  av_opt_set_sample_fmt(SwrContext, 'in_sample_fmt', SrcFormat, 0);
  av_opt_set_sample_fmt(SwrContext, 'out_sample_fmt', DstFormat, 0);
  swr_init(SwrContext);
  // calculate ratio
  Ratio := srcFormatInfo.GetRatio(dstFormatInfo);

  Result := true;
end;

destructor TAudioConverter_SWResample.Destroy();
begin
  if SwrContext <> nil then
    swr_free(@SwrContext);
  inherited;
end;

function TAudioConverter_SWResample.Convert(InputBuffer: PByteArray; OutputBuffer: PByteArray;
                                        var InputSize: integer): integer;
var
  InputSampleCount: integer;
  OutputSampleCount: integer;
  SrcFormat: TAVSampleFormat;
  DstFormat: TAVSampleFormat;
  InBufPtr: PCuint8;
  OutBufPtr: PCuint8;
begin
  Result := -1;

  if InputSize <= 0 then
  begin
    // avoid div-by-zero in audio_resample()
    if InputSize = 0 then
      Result := 0;
    Exit;
  end;

  if not TMediaCore_FFmpeg.ConvertAudioFormatToFFmpeg(SrcFormatInfo.Format, SrcFormat) then
  begin
    Log.LogError('Unsupported format', 'TAudioConverter_FFmpeg.Init');
    Log.LogError('srcFormatInfo.Format: ' + intToStr(integer(srcFormatInfo.Format)),
                 'TAudioConverter_FFmpeg.Init');
    Exit;
  end;

  if not TMediaCore_FFmpeg.ConvertAudioFormatToFFmpeg(DstFormatInfo.Format, DstFormat) then
  begin
    Log.LogError('Unsupported format', 'TAudioConverter_FFmpeg.Init');
    Log.LogError('dstFormatInfo.Format: ' + intToStr(integer(dstFormatInfo.Format)),
                 'TAudioConverter_FFmpeg.Init');
    Exit;
  end;

  InputSampleCount := InputSize div SrcFormatInfo.FrameSize;
  OutputSampleCount := GetOutputBufferSize(InputSize) div DstFormatInfo.FrameSize;
  InBufPtr := Pcuint8(@InputBuffer[0]);
  OutBufPtr := Pcuint8(@OutputBuffer[0]);
  OutputSampleCount:= swr_convert(SwrContext, OutBufPtr, OutputSampleCount,
                                  InBufPtr, InputSampleCount);
  if (OutputSampleCount < 0) then
  begin
    Log.LogError('swr_convert failed ' + inttostr(OutputSampleCount), 'TAudioConverter_SWResample.Init');
    OutputSampleCount := GetOutputBufferSize(InputSize) div DstFormatInfo.FrameSize;
  end;
  Result := OutputSampleCount * DstFormatInfo.FrameSize;
end;

function TAudioConverter_SWResample.GetOutputBufferSize(InputSize: integer): integer;
begin
  Result := Ceil(InputSize * GetRatio());
end;

function TAudioConverter_SWResample.GetRatio(): double;
begin
  Result := Ratio;
end;

end.
