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

unit UScreenOptionsRecord;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UThemes,
  UMusic,
  URecord,
  UMenu;

type
  TDrawState = record
    ChannelIndex: integer;
    R, G, B: real;    // mapped player color (normal)
    RD, GD, BD: real; // mapped player color (dark)
  end;

  TPeakInfo = record
    Volume: single;
    Time: cardinal;
  end;

  TScreenOptionsRecord = class(TMenu)
    private
      // max. count of input-channels determined for all devices
      MaxChannelCount: integer;

      // current input device
      CurrentDeviceIndex: integer;
      PreviewDeviceIndex: integer;

      // string arrays for select-slide options
      InputSourceNames: array of string;
      InputDeviceNames: array of string;

      // dynamic generated themes for channel select-sliders
      SelectSlideChannelTheme: array of TThemeSelectSlide;

      // indices for widget-updates
      SelectInputSourceID:   integer;
      SelectSlideChannelID: array of integer;

      // interaction IDs
      ExitButtonIID: integer;

      // dummy data for non-available channels
      ChannelToPlayerMapDummy: integer;

      // preview channel-buffers
      PreviewChannel: array of TCaptureBuffer;
      ChannelPeak: array of TPeakInfo;

      // Device source volume
      SourceVolume: single;
      NextVolumePollTime: cardinal;

      procedure StartPreview;
      procedure StopPreview;
      procedure UpdateInputDevice;
      procedure ChangeVolume(VolumeChange: single);
      procedure DrawVolume(x, y, Width, Height: single);
      procedure DrawVUMeter(const State: TDrawState; x, y, Width, Height: single);
      procedure DrawPitch(const State: TDrawState; x, y, Width, Height: single);
    public
      constructor Create; override;
      function    Draw: boolean; override;
      function    ParseInput(PressedKey: cardinal; CharCode: WideChar; PressedDown: boolean): boolean; override;
      procedure   onShow; override;
      procedure   onHide; override;
  end;

const
  PeakDecay = 0.2; // strength of peak-decay (reduction after one sec)

const
  BarHeight  = 11; // height of each bar (volume/vu-meter/pitch)
  BarUpperSpacing = 1;  // spacing between a bar-area and the previous widget
  BarLowerSpacing = 3;  // spacing between a bar-area and the next widget
  SourceBarsTotalHeight = BarHeight + BarUpperSpacing + BarLowerSpacing;
  ChannelBarsTotalHeight = 2*BarHeight + BarUpperSpacing + BarLowerSpacing;

implementation

uses
  SysUtils,
  Math,
  SDL,
  gl,
  TextGL,
  UGraphic,
  UDraw,
  UMain,
  UMenuSelectSlide,
  UMenuText,
  UFiles,
  UDisplay,
  UIni,
  ULog;

function TScreenOptionsRecord.ParseInput(PressedKey: cardinal; CharCode: WideChar; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case WideCharUpperCase(CharCode)[1] of
      'Q':
        begin
          Result := false;
          Exit;
        end;
      '+':
        begin
          // FIXME: add a nice volume-slider instead
          // or at least provide visualization and acceleration if the user holds the key pressed.
          ChangeVolume(0.02);
        end;
      '-':
        begin
          // FIXME: add a nice volume-slider instead
          // or at least provide visualization and acceleration if the user holds the key pressed.
          ChangeVolume(-0.02);
        end;
      'T':
        begin
          if ((SDL_GetModState() and KMOD_SHIFT) <> 0) then
            Ini.ThresholdIndex := (Ini.ThresholdIndex + Length(IThresholdVals) - 1) mod Length(IThresholdVals)
          else
            Ini.ThresholdIndex := (Ini.ThresholdIndex + 1) mod Length(IThresholdVals);
        end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE:
        begin
          // TODO: Show Save/Abort screen
          Ini.Save;          
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          if (SelInteraction = ExitButtonIID) then
          begin
            Ini.Save;
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenOptions);
          end;
        end;
      SDLK_DOWN:
        InteractNext;
      SDLK_UP :
        InteractPrev;
      SDLK_RIGHT:
        begin
          if (SelInteraction >= 0) and (SelInteraction < ExitButtonIID) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
          UpdateInputDevice;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction < ExitButtonIID) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
          UpdateInputDevice;
        end;
    end;
  end;
end;

constructor TScreenOptionsRecord.Create;
var
  DeviceIndex:  integer;
  SourceIndex:  integer;
  ChannelIndex: integer;
  InputDevice: TAudioInputDevice;
  InputDeviceCfg: PInputDeviceConfig;
  ChannelTheme: ^TThemeSelectSlide;
  //ButtonTheme: TThemeButton;
  WidgetYPos: integer;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsRecord);

  // set CurrentDeviceIndex to a valid device
  if (Length(AudioInputProcessor.DeviceList) > 0) then
    CurrentDeviceIndex := 0
  else
    CurrentDeviceIndex := -1;

  PreviewDeviceIndex := -1;

  WidgetYPos := 0;

  // init sliders if at least one device was detected
  if (Length(AudioInputProcessor.DeviceList) > 0) then
  begin
    InputDevice := AudioInputProcessor.DeviceList[CurrentDeviceIndex];
    InputDeviceCfg := @Ini.InputDeviceConfig[InputDevice.CfgIndex];

    // init device-selection slider
    SetLength(InputDeviceNames, Length(AudioInputProcessor.DeviceList));
    for DeviceIndex := 0 to High(AudioInputProcessor.DeviceList) do
    begin
      InputDeviceNames[DeviceIndex] := AudioInputProcessor.DeviceList[DeviceIndex].Name;
    end;
    // add device-selection slider (InteractionID: 0)
    Theme.OptionsRecord.SelectSlideCard.showArrows := true;
    Theme.OptionsRecord.SelectSlideCard.oneItemOnly := true;
    AddSelectSlide(Theme.OptionsRecord.SelectSlideCard, CurrentDeviceIndex, InputDeviceNames);

    // init source-selection slider
    SetLength(InputSourceNames, Length(InputDevice.Source));
    for SourceIndex := 0 to High(InputDevice.Source) do
    begin
      InputSourceNames[SourceIndex] := InputDevice.Source[SourceIndex].Name;
    end;

    Theme.OptionsRecord.SelectSlideInput.showArrows := true;
    Theme.OptionsRecord.SelectSlideInput.oneItemOnly := true;
    // add source-selection slider (InteractionID: 1)
    SelectInputSourceID := AddSelectSlide(Theme.OptionsRecord.SelectSlideInput,
        InputDeviceCfg.Input, InputSourceNames);

    // add space for source volume bar
    WidgetYPos := Theme.OptionsRecord.SelectSlideInput.Y +
                  Theme.OptionsRecord.SelectSlideInput.H +
                  SourceBarsTotalHeight;

    // find max. channel count of all devices
    MaxChannelCount := 0;
    for DeviceIndex := 0 to High(AudioInputProcessor.DeviceList) do
    begin
      if (AudioInputProcessor.DeviceList[DeviceIndex].AudioFormat.Channels > MaxChannelCount) then
        MaxChannelCount := AudioInputProcessor.DeviceList[DeviceIndex].AudioFormat.Channels;
    end;

    // init channel-to-player mapping sliders
    SetLength(SelectSlideChannelID, MaxChannelCount);
    SetLength(SelectSlideChannelTheme, MaxChannelCount);

    for ChannelIndex := 0 to MaxChannelCount-1 do
    begin
      // copy reference slide
      SelectSlideChannelTheme[ChannelIndex] :=
        Theme.OptionsRecord.SelectSlideChannel;
      // set current channel-theme
      ChannelTheme := @SelectSlideChannelTheme[ChannelIndex];
      // adjust vertical position
      ChannelTheme.Y := WidgetYPos;
      // calc size of next slide (add space for bars)
      WidgetYPos := WidgetYPos + ChannelTheme.H + ChannelBarsTotalHeight;
      // append channel index to name
      ChannelTheme.Text := ChannelTheme.Text + IntToStr(ChannelIndex+1);

      // show/hide widgets depending on whether the channel exists
      if (ChannelIndex < Length(InputDeviceCfg.ChannelToPlayerMap)) then
      begin
        // current device has this channel

        // add slider
        SelectSlideChannelID[ChannelIndex] := AddSelectSlide(ChannelTheme^,
          InputDeviceCfg.ChannelToPlayerMap[ChannelIndex], IChannelPlayer);
      end
      else
      begin
        // current device does not have that many channels

        // add slider but hide it and assign a dummy variable to it
        SelectSlideChannelID[ChannelIndex] := AddSelectSlide(ChannelTheme^,
          ChannelToPlayerMapDummy, IChannelPlayer);
        SelectsS[SelectSlideChannelID[ChannelIndex]].Visible := false;
      end;
    end;
  end;

  // add Exit-button
  //ButtonTheme := Theme.OptionsRecord.ButtonExit;
  // adjust button position
  //if (WidgetYPos <> 0) then
  //  ButtonTheme.Y := WidgetYPos;
  //AddButton(ButtonTheme);
  // <mog> I uncommented the stuff above, because it's not skinable :X 
  AddButton(Theme.OptionsRecord.ButtonExit);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);
  // store InteractionID
  if (Length(AudioInputProcessor.DeviceList) > 0) then
    ExitButtonIID := MaxChannelCount + 2
  else
    ExitButtonIID := 0;

  // set focus
  Interaction := 0;
end;

procedure TScreenOptionsRecord.UpdateInputDevice;
var
  SourceIndex: integer;
  InputDevice: TAudioInputDevice;
  InputDeviceCfg: PInputDeviceConfig;
  ChannelIndex: integer;
begin
  //Log.LogStatus('Update input-device', 'TScreenOptionsRecord.UpdateCard') ;

  StopPreview();

  // set CurrentDeviceIndex to a valid device
  if (CurrentDeviceIndex > High(AudioInputProcessor.DeviceList)) then
    CurrentDeviceIndex := 0;

  // update sliders if at least one device was detected
  if (Length(AudioInputProcessor.DeviceList) > 0) then
  begin
    InputDevice := AudioInputProcessor.DeviceList[CurrentDeviceIndex];
    InputDeviceCfg := @Ini.InputDeviceConfig[InputDevice.CfgIndex];

    // update source-selection slider
    SetLength(InputSourceNames, Length(InputDevice.Source));
    for SourceIndex := 0 to High(InputDevice.Source) do
    begin
      InputSourceNames[SourceIndex] := InputDevice.Source[SourceIndex].Name;
    end;
    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideInput, SelectInputSourceID,
        InputSourceNames, InputDeviceCfg.Input);

    // update channel-to-player mapping sliders
    for ChannelIndex := 0 to MaxChannelCount-1 do
    begin
      // show/hide widgets depending on whether the channel exists
      if (ChannelIndex < Length(InputDeviceCfg.ChannelToPlayerMap)) then
      begin
        // current device has this channel

        // show slider
        UpdateSelectSlideOptions(SelectSlideChannelTheme[ChannelIndex],
          SelectSlideChannelID[ChannelIndex], IChannelPlayer,
          InputDeviceCfg.ChannelToPlayerMap[ChannelIndex]);
        SelectsS[SelectSlideChannelID[ChannelIndex]].Visible := true;
      end
      else
      begin
        // current device does not have that many channels

        // hide slider and assign a dummy variable to it
        UpdateSelectSlideOptions(SelectSlideChannelTheme[ChannelIndex],
          SelectSlideChannelID[ChannelIndex], IChannelPlayer,
          ChannelToPlayerMapDummy);
        SelectsS[SelectSlideChannelID[ChannelIndex]].Visible := false;
      end;
    end;
  end;

  StartPreview();
end;

procedure TScreenOptionsRecord.ChangeVolume(VolumeChange: single);
var
  InputDevice: TAudioInputDevice;
  Volume: single;
begin
  // validate CurrentDeviceIndex
  if ((CurrentDeviceIndex < 0) or
      (CurrentDeviceIndex > High(AudioInputProcessor.DeviceList))) then
  begin
    Exit;
  end;

  InputDevice := AudioInputProcessor.DeviceList[CurrentDeviceIndex];
  if not assigned(InputDevice) then
    Exit;

  // set new volume
  Volume := InputDevice.GetVolume() + VolumeChange;
  InputDevice.SetVolume(Volume);
  //DebugWriteln('Volume: ' + floattostr(InputDevice.GetVolume));

  // volume must be polled again 
  NextVolumePollTime := 0;
end;

procedure TScreenOptionsRecord.onShow;
var
  ChannelIndex: integer;
begin
  inherited;

  Interaction := 0;

  // create preview sound-buffers
  SetLength(PreviewChannel, MaxChannelCount);
  for ChannelIndex := 0 to High(PreviewChannel) do
    PreviewChannel[ChannelIndex] := TCaptureBuffer.Create();

  SetLength(ChannelPeak, MaxChannelCount);

  StartPreview();
end;

procedure TScreenOptionsRecord.onHide;
var
  ChannelIndex: integer;
begin
  StopPreview();

  // free preview buffers
  for ChannelIndex := 0 to High(PreviewChannel) do
    PreviewChannel[ChannelIndex].Free;
  SetLength(PreviewChannel, 0);
  SetLength(ChannelPeak, 0);
end;

procedure TScreenOptionsRecord.StartPreview;
var
  ChannelIndex: integer;
  Device: TAudioInputDevice;
begin
  if ((CurrentDeviceIndex >= 0) and
      (CurrentDeviceIndex <= High(AudioInputProcessor.DeviceList))) then
  begin
    Device := AudioInputProcessor.DeviceList[CurrentDeviceIndex];
    // set preview channel as active capture channel
    for ChannelIndex := 0 to High(Device.CaptureChannel) do
    begin
      PreviewChannel[ChannelIndex].Clear();
      Device.LinkCaptureBuffer(ChannelIndex, PreviewChannel[ChannelIndex]);
      FillChar(ChannelPeak[ChannelIndex], SizeOf(TPeakInfo), 0);
    end;
    Device.Start();
    PreviewDeviceIndex := CurrentDeviceIndex;

    // volume must be polled again
    NextVolumePollTime := 0;
  end;
end;

procedure TScreenOptionsRecord.StopPreview;
var
  ChannelIndex: integer;
  Device: TAudioInputDevice;
begin
  if ((PreviewDeviceIndex >= 0) and
      (PreviewDeviceIndex <= High(AudioInputProcessor.DeviceList))) then
  begin
    Device := AudioInputProcessor.DeviceList[PreviewDeviceIndex];
    Device.Stop;
    for ChannelIndex := 0 to High(Device.CaptureChannel) do
      Device.LinkCaptureBuffer(ChannelIndex, nil);
  end;
  PreviewDeviceIndex := -1;
end;

procedure TScreenOptionsRecord.DrawVolume(x, y, Width, Height: single);
var
  x1, y1, x2, y2: single;
  VolBarInnerWidth: integer;
  Volume: single;
const
  VolBarInnerHSpacing = 2;
  VolBarInnerVSpacing = 1;
begin
  // coordinates for black rect
  x1 := x;
  y1 := y;
  x2 := x1 + Width;
  y2 := y1 + Height;

  // init blend mode
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  // draw black background-rect
  glColor4f(0, 0, 0, 0.8);
  glBegin(GL_QUADS);
    glVertex2f(x1, y1);
    glVertex2f(x2, y1);
    glVertex2f(x2, y2);
    glVertex2f(x1, y2);
  glEnd();

  VolBarInnerWidth := Trunc(Width - 2*VolBarInnerHSpacing);

  // TODO: if no volume is available, show some info (a blue bar maybe)
  if (SourceVolume >= 0) then
    Volume := SourceVolume
  else
    Volume := 0;

  // coordinates for first half of the volume bar
  x1 := x + VolBarInnerHSpacing;
  x2 := x1 + VolBarInnerWidth * Volume;
  y1 := y1 + VolBarInnerVSpacing;
  y2 := y2 - VolBarInnerVSpacing;

  // draw volume-bar
  glBegin(GL_QUADS);
    // draw volume bar
    glColor3f(0.4, 0.3, 0.3);
    glVertex2f(x1, y1);
    glVertex2f(x1, y2);
    glColor3f(1, 0.1, 0.1);
    glVertex2f(x2, y2);
    glVertex2f(x2, y1);
  glEnd();

  { not needed anymore
  // coordinates for separator
  x1 := x + VolBarInnerHSpacing;
  x2 := x1 + VolBarInnerWidth;

  // draw separator
  glBegin(GL_LINE_STRIP);
    glColor4f(0.1, 0.1, 0.1, 0.2);
    glVertex2f(x1,        y2);
    glColor4f(0.4, 0.4, 0.4, 0.2);
    glVertex2f((x1+x2)/2, y2);
    glColor4f(0.1, 0.1, 0.1, 0.2);
    glVertex2f(x2,        y2);
  glEnd();
  }

  glDisable(GL_BLEND);
end;

procedure TScreenOptionsRecord.DrawVUMeter(const State: TDrawState; x, y, Width, Height: single);
var
  x1, y1, x2, y2: single;
  Volume, PeakVolume: single;
  Delta: single;
  VolBarInnerWidth: integer;
const
  VolBarInnerHSpacing = 2;
  VolBarInnerVSpacing = 1;
begin
  // coordinates for black rect
  x1 := x;
  y1 := y;
  x2 := x1 + Width;
  y2 := y1 + Height;

  // init blend mode
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  // draw black background-rect
  glColor4f(0, 0, 0, 0.8);
  glBegin(GL_QUADS);
    glVertex2f(x1, y1);
    glVertex2f(x2, y1);
    glVertex2f(x2, y2);
    glVertex2f(x1, y2);
  glEnd();

  VolBarInnerWidth := Trunc(Width - 2*VolBarInnerHSpacing);

  // vertical positions
  y1 := y1 + VolBarInnerVSpacing;
  y2 := y2 - VolBarInnerVSpacing;

  // coordinates for bevel
  x1 := x + VolBarInnerHSpacing;
  x2 := x1 + VolBarInnerWidth;

  glBegin(GL_QUADS);
    Volume := PreviewChannel[State.ChannelIndex].MaxSampleVolume();

    // coordinates for volume bar
    x1 := x + VolBarInnerHSpacing;
    x2 := x1 + VolBarInnerWidth * Volume;

    // draw volume bar
    glColor3f(State.RD, State.GD, State.BD);
    glVertex2f(x1, y1);
    glVertex2f(x1, y2);
    glColor3f(State.R, State.G, State.B);
    glVertex2f(x2, y2);
    glVertex2f(x2, y1);

    Delta := (SDL_GetTicks() - ChannelPeak[State.ChannelIndex].Time)/1000;
    PeakVolume := ChannelPeak[State.ChannelIndex].Volume - Delta*Delta*PeakDecay;

    // determine new peak-volume
    if (Volume > PeakVolume) then
    begin
      PeakVolume := Volume;
      ChannelPeak[State.ChannelIndex].Volume := Volume;
      ChannelPeak[State.ChannelIndex].Time := SDL_GetTicks();
    end;

    x1 := x + VolBarInnerHSpacing + VolBarInnerWidth * PeakVolume;
    x2 := x1 + 2;

    // draw peak
    glColor3f(0.8, 0.8, 0.8);
    glVertex2f(x1, y1);
    glVertex2f(x1, y2);
    glVertex2f(x2, y2);
    glVertex2f(x2, y1);

    // draw threshold
    x1 := x + VolBarInnerHSpacing;
    x2 := x1 + VolBarInnerWidth * IThresholdVals[Ini.ThresholdIndex];

    glColor4f(0.3, 0.3, 0.3, 0.6);
    glVertex2f(x1, y1);
    glVertex2f(x1, y2);
    glVertex2f(x2, y2);
    glVertex2f(x2, y1);
  glEnd();

  glDisable(GL_BLEND);
end;

procedure TScreenOptionsRecord.DrawPitch(const State: TDrawState; x, y, Width, Height: single);
var
  x1, y1, x2, y2: single;
  i: integer;
  ToneBoxWidth: real;
  ToneString: string;
  ToneStringWidth, ToneStringHeight: real;
  ToneStringMaxWidth: real;
  ToneStringCenterXOffset: real;
const
  PitchBarInnerHSpacing = 2;
  PitchBarInnerVSpacing = 1;
begin
  // calc tone pitch
  PreviewChannel[State.ChannelIndex].AnalyzeBuffer();

  // coordinates for black rect
  x1 := x;
  y1 := y;
  x2 := x + Width;
  y2 := y + Height;

  // init blend mode
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  // draw black background-rect
  glColor4f(0, 0, 0, 0.8);
  glBegin(GL_QUADS);
    glVertex2f(x1, y1);
    glVertex2f(x2, y1);
    glVertex2f(x2, y2);
    glVertex2f(x1, y2);
  glEnd();

  // coordinates for tone boxes
  ToneBoxWidth := Width / NumHalftones;
  y1 := y1 + PitchBarInnerVSpacing;
  y2 := y2 - PitchBarInnerVSpacing;

  glBegin(GL_QUADS);
    // draw tone boxes
    for i := 0 to NumHalftones-1 do
    begin
      x1 := x + i * ToneBoxWidth + PitchBarInnerHSpacing;
      x2 := x1 + ToneBoxWidth - 2*PitchBarInnerHSpacing;

      if ((PreviewChannel[State.ChannelIndex].ToneValid) and
          (PreviewChannel[State.ChannelIndex].ToneAbs = i)) then
      begin
        // highlight current tone-pitch
        glColor3f(1, i / (NumHalftones-1), 0)
      end
      else
      begin
        // grey other tone-pitches
        glColor3f(0.3, i / (NumHalftones-1) * 0.3, 0);
      end;

      glVertex2f(x1, y1);
      glVertex2f(x2, y1);
      glVertex2f(x2, y2);
      glVertex2f(x1, y2);
    end;
  glEnd();

  glDisable(GL_BLEND);

  ///
  // draw the name of the tone
  ///////

  ToneString := PreviewChannel[State.ChannelIndex].ToneString;
  ToneStringHeight := ChannelBarsTotalHeight;

  // initialize font
  // TODO: what about reflection, italic etc.?
  SetFontSize(ToneStringHeight);

  // center
  // Note: for centering let us assume that G#4 has the max. horizontal extent
  ToneStringWidth := glTextWidth(ToneString);
  ToneStringMaxWidth := glTextWidth('G#4');
  ToneStringCenterXOffset := (ToneStringMaxWidth-ToneStringWidth) / 2;

  // draw
  SetFontPos(x-ToneStringWidth-ToneStringCenterXOffset, y-ToneStringHeight/2);
  glColor3f(0, 0, 0);
  glPrint(ToneString);
end;

function TScreenOptionsRecord.Draw: boolean;
var
  Device: TAudioInputDevice;
  DeviceCfg: PInputDeviceConfig;
  SelectSlide: TSelectSlide;
  BarXOffset, BarYOffset, BarWidth: real;
  ChannelIndex: integer;
  State: TDrawState;
begin
  DrawBG;
  DrawFG;

  if ((PreviewDeviceIndex >= 0) and
      (PreviewDeviceIndex <= High(AudioInputProcessor.DeviceList))) then
  begin
    Device := AudioInputProcessor.DeviceList[PreviewDeviceIndex];
    DeviceCfg := @Ini.InputDeviceConfig[Device.CfgIndex];

    // update source volume
    if (SDL_GetTicks() >= NextVolumePollTime) then
    begin
      NextVolumePollTime := SDL_GetTicks() + 500; // next poll in 500ms
      SourceVolume := Device.GetVolume();
    end;

    // get source select slide
    SelectSlide := SelectsS[SelectInputSourceID];
    BarXOffset := SelectSlide.TextureSBG.X;
    BarYOffset := SelectSlide.TextureSBG.Y + SelectSlide.TextureSBG.H + BarUpperSpacing;
    BarWidth   := SelectSlide.TextureSBG.W;
    DrawVolume(SelectSlide.TextureSBG.X, BarYOffset, BarWidth, BarHeight);

    for ChannelIndex := 0 to High(Device.CaptureChannel) do
    begin
      // load player color mapped to current input channel
      if (DeviceCfg.ChannelToPlayerMap[ChannelIndex] > 0) then
      begin
        // set mapped channel to corresponding player-color
        LoadColor(State.R, State.G, State.B, 'P'+ IntToStr(DeviceCfg.ChannelToPlayerMap[ChannelIndex]) + 'Dark');
      end
      else
      begin
        // set non-mapped channel to white
        State.R := 1; State.G := 1; State.B := 1;
      end;

      // dark player colors
      State.RD := 0.2 * State.R;
      State.GD := 0.2 * State.G;
      State.BD := 0.2 * State.B;

      // channel select slide
      SelectSlide := SelectsS[SelectSlideChannelID[ChannelIndex]];

      BarXOffset := SelectSlide.TextureSBG.X;
      BarYOffset := SelectSlide.TextureSBG.Y + SelectSlide.TextureSBG.H + BarUpperSpacing;
      BarWidth   := SelectSlide.TextureSBG.W;

      State.ChannelIndex := ChannelIndex;

      DrawVUMeter(State, BarXOffset, BarYOffset, BarWidth, BarHeight);
      DrawPitch(State, BarXOffset, BarYOffset+BarHeight, BarWidth, BarHeight);
    end;
  end;

  Result := true;
end;

end.
