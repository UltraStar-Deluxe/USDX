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
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/screens/UScreenOptionsRecord.pas $
 * $Id: UScreenOptionsRecord.pas 3068 2014-01-01 19:17:11Z k-m_schindler $
 *}

unit UScreenOptionsRecord;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  UMusic,
  URecord,
  UThemes;

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
      // current input device
      CurrentDeviceIndex: integer;
      PreviewDeviceIndex: integer;
      CurrentChannel: array of integer;

      // string arrays for select-slide options
      InputSourceNames: array of UTF8String;
      InputDeviceNames: array of UTF8String;
      SelectChannelOptions: array of UTF8String;

      // indices for widget-updates
      SelectInputSourceID: integer;
      SelectChannelID: integer;
      SelectAssigneeID: integer;
      SelectThresholdID: integer;

      // interaction IDs
      ExitButtonIID: integer;

      // dummy data for non-available channels
      ChannelToPlayerMapDummy: integer;

      // preview channel-buffers
      PreviewChannel: TCaptureBuffer;
      ChannelPeak: TPeakInfo;

      // Device source volume
      SourceVolume: single;
      NextVolumePollTime: cardinal;

      procedure StartPreview;
      procedure StopPreview;
      procedure UpdateInputDevice;
      function ValidateSettings: boolean;
      procedure ChangeVolume(VolumeChange: single);
      procedure DrawVolume(x, y, Width, Height: single);
      procedure DrawVUMeter(const State: TDrawState; x, y, Width, Height: single);
      procedure DrawPitch(const State: TDrawState; x, y, Width, Height: single);
    public
      constructor Create; override;
      function    Draw: boolean; override;
      function    ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean; override;
      procedure   OnShow; override;
      procedure   OnHide; override;
  end;

const
  PeakDecay = 0.2; // strength of peak-decay (reduction after one sec)

const
  BarHeight  = 11; // height of each bar (volume/vu-meter/pitch)
  BarUpperSpacing = 1;  // spacing between a bar-area and the previous widget
  BarLowerSpacing = 3;  // spacing between a bar-area and the next widget
  SourceBarsTotalHeight = BarHeight + BarUpperSpacing + BarLowerSpacing;
  ChannelBarsTotalHeight = 2*BarHeight + BarUpperSpacing + BarLowerSpacing;

const
  ID='ID_077';   //for help system

implementation

uses
  UDisplay,
  UDraw,
  UFiles,
  UGraphic,
  UHelp,
  UIni,
  ULanguage,
  ULog,
  UMain,
  UMenuSelectSlide,
  UMenuText,
  UUnicodeUtils,
  dglOpenGL,
  Math,
  sdl2,
  SysUtils,
  TextGL;

function TScreenOptionsRecord.ParseInput(PressedKey: cardinal; CharCode: UCS4Char; PressedDown: boolean): boolean;
begin
  Result := true;
  if (PressedDown) then
  begin // Key Down
    // check normal keys
    case UCS4UpperCase(CharCode) of
      Ord('Q'):
        begin
          Result := false;
          Exit;
        end;
      Ord('+'):
        begin
          // FIXME: add a nice volume-slider instead
          // or at least provide visualization and acceleration if the user holds the key pressed.
          ChangeVolume(0.02);
        end;
      Ord('-'):
        begin
          // FIXME: add a nice volume-slider instead
          // or at least provide visualization and acceleration if the user holds the key pressed.
          ChangeVolume(-0.02);
        end;
      Ord('T'):
        begin
          if ((SDL_GetModState() and KMOD_SHIFT) <> 0) then
            Ini.ThresholdIndex := (Ini.ThresholdIndex + Length(IThresholdVals) - 1) mod Length(IThresholdVals)
          else
            Ini.ThresholdIndex := (Ini.ThresholdIndex + 1) mod Length(IThresholdVals);
          UpdateSelectSlideOptions(Theme.OptionsRecord.SelectThreshold, SelectThresholdID, IThreshold, Ini.ThresholdIndex);
        end;
    end;

    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE:
        begin
          // TODO: Show Save/Abort screen
          if (ValidateSettings()) then
          begin
            Ini.Save;
            AudioPlayback.PlaySound(SoundLib.Back);
            FadeTo(@ScreenOptions);
          end;
        end;
      SDLK_TAB:
      begin
        ScreenPopupHelp.ShowPopup();
      end;
      SDLK_RETURN:
        begin
          if (SelInteraction = ExitButtonIID) then
          begin
            if (ValidateSettings()) then
            begin
              Ini.Save;
              AudioPlayback.PlaySound(SoundLib.Back);
              FadeTo(@ScreenOptions);
            end;
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

function TScreenOptionsRecord.ValidateSettings: boolean;
var
  BadPlayer: integer;
begin
  BadPlayer := AudioInputProcessor.ValidateSettings();
  if (BadPlayer <> 0) then
  begin
    ScreenPopupError.ShowPopup(
        Format(Language.Translate('ERROR_PLAYER_DEVICE_ASSIGNMENT'),
        [BadPlayer]));
    Result := false;
  end
  else
  begin
    Result := true;
  end;
end;

constructor TScreenOptionsRecord.Create;
var
  DeviceIndex:  integer;
  SourceIndex:  integer;
  ChannelIndex: integer;
  InputDevice: TAudioInputDevice;
  InputDeviceCfg: PInputDeviceConfig;
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
    SetLength(CurrentChannel, Length(AudioInputProcessor.DeviceList));

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

    // compute list of selectable channels
    SetLength(SelectChannelOptions, InputDevice.AudioFormat.Channels);
    for ChannelIndex := 0 to InputDevice.AudioFormat.Channels-1 do
    begin
      SelectChannelOptions[ChannelIndex] := IntToStr(ChannelIndex + 1);
    end;

    // add space for source volume bar
    Theme.OptionsRecord.SelectChannel.Y := Theme.OptionsRecord.SelectChannel.Y
        + SourceBarsTotalHeight;
    Theme.OptionsRecord.SelectChannel.showArrows := true;
    Theme.OptionsRecord.SelectChannel.oneItemOnly := true;

    SelectChannelID := AddSelectSlide(Theme.OptionsRecord.SelectChannel,
        CurrentChannel[CurrentDeviceIndex], SelectChannelOptions);

    // adjust vertical position
    Theme.OptionsRecord.SelectAssignee.Y := Theme.OptionsRecord.SelectAssignee.Y
        + SourceBarsTotalHeight;

    // show/hide widgets depending on whether the channel exists
    if (Length(InputDeviceCfg.ChannelToPlayerMap) > 0) then
    begin
      // current device has a channel
      // add slider, assign to first channel of the device
      SelectAssigneeID := AddSelectSlide(Theme.OptionsRecord.SelectAssignee,
        InputDeviceCfg.ChannelToPlayerMap[0], IChannelPlayerTranslated);
    end
    else
    begin
      // current device does not have any channels
      // add slider but hide it and assign a dummy variable to it
      SelectAssigneeID := AddSelectSlide(Theme.OptionsRecord.SelectAssignee,
        ChannelToPlayerMapDummy, IChannelPlayerTranslated);
      SelectsS[SelectAssigneeID].Visible := false;
    end;

    Theme.OptionsRecord.SelectThreshold.showArrows := true; //basisbit TODO
    Theme.OptionsRecord.SelectThreshold.oneItemOnly := true;
    // adjust vertical position
    Theme.OptionsRecord.SelectThreshold.Y := Theme.OptionsRecord.SelectThreshold.Y
        + SourceBarsTotalHeight
        + ChannelBarsTotalHeight;
    SelectThresholdID := AddSelectSlide(Theme.OptionsRecord.SelectThreshold,
        Ini.ThresholdIndex, IThreshold);

    Theme.OptionsRecord.SelectMicBoost.showArrows := true;
    Theme.OptionsRecord.SelectMicBoost.oneItemOnly := true;
    // adjust vertical position
    Theme.OptionsRecord.SelectMicBoost.Y := Theme.OptionsRecord.SelectMicBoost.Y
        + SourceBarsTotalHeight
        + ChannelBarsTotalHeight;
    AddSelectSlide(Theme.OptionsRecord.SelectMicBoost, Ini.MicBoost, IMicBoostTranslated);

  end;

  // add Exit-button
  AddButton(Theme.OptionsRecord.ButtonExit);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(20, 5, Theme.Options.Description[OPTIONS_DESC_INDEX_BACK]);
  // store InteractionID
  if (Length(AudioInputProcessor.DeviceList) > 0) then
    ExitButtonIID := 1 + 4
  else
    ExitButtonIID := 2;

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

    SetLength(SelectChannelOptions, InputDevice.AudioFormat.Channels);
    for ChannelIndex := 0 to InputDevice.AudioFormat.Channels-1 do
    begin
      SelectChannelOptions[ChannelIndex] := IntToStr(ChannelIndex+1);
    end;
    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectChannel,
      SelectChannelID, SelectChannelOptions,
      CurrentChannel[CurrentDeviceIndex]);

    ChannelIndex := CurrentChannel[CurrentDeviceIndex];

    // update channel-to-player mapping slider
    // show/hide widgets depending on whether the channel exists
    if (ChannelIndex < Length(InputDeviceCfg.ChannelToPlayerMap)) then
    begin
      // current device has this channel

      // show slider
      UpdateSelectSlideOptions(Theme.OptionsRecord.SelectAssignee,
        SelectAssigneeID, IChannelPlayerTranslated,
        InputDeviceCfg.ChannelToPlayerMap[ChannelIndex]);
      SelectsS[SelectAssigneeID].Visible := true;
    end
    else
    begin
      // current device does not have that many channels,
      // which is a weird corner case,
      // as we only allow the selection of available channels

      // hide slider and assign a dummy variable to it
      UpdateSelectSlideOptions(Theme.OptionsRecord.SelectAssignee,
        SelectAssigneeID, IChannelPlayerTranslated,
        ChannelToPlayerMapDummy);
      SelectsS[SelectAssigneeID].Visible := false;
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

procedure TScreenOptionsRecord.OnShow;
begin
  inherited;

  // BgMusic distracts too much, pause it
  SoundLib.PauseBgMusic;

  Interaction := 0;

  if not Help.SetHelpID(ID) then
    Log.LogWarn('No Entry for Help-ID ' + ID, 'ScreenOptionsRecord');

  // create preview sound-buffer
  PreviewChannel := TCaptureBuffer.Create();

  UpdateInputDevice();
end;

procedure TScreenOptionsRecord.OnHide;
begin
  StopPreview();

  // free preview buffer
  PreviewChannel.Free;
end;

procedure TScreenOptionsRecord.StartPreview;
var
  Device: TAudioInputDevice;
begin
  if ((CurrentDeviceIndex >= 0) and
      (CurrentDeviceIndex <= High(AudioInputProcessor.DeviceList))) then
  begin
    Device := AudioInputProcessor.DeviceList[CurrentDeviceIndex];

    // set preview channel as active capture channel
    PreviewChannel.Clear();
    Device.LinkCaptureBuffer(CurrentChannel[CurrentDeviceIndex], PreviewChannel);
    FillChar(ChannelPeak, SizeOf(TPeakInfo), 0);
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
    Volume := PreviewChannel.MaxSampleVolume();

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

    Delta := (SDL_GetTicks() - ChannelPeak.Time)/1000;
    PeakVolume := ChannelPeak.Volume - Delta*Delta*PeakDecay;

    // determine new peak-volume
    if (Volume > PeakVolume) then
    begin
      PeakVolume := Volume;
      ChannelPeak.Volume := Volume;
      ChannelPeak.Time := SDL_GetTicks();
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
  PreviewChannel.AnalyzeBuffer();

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

      if ((PreviewChannel.ToneValid) and
          (PreviewChannel.ToneAbs = i)) then
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

  ToneString := PreviewChannel.ToneString;
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

    ChannelIndex := CurrentChannel[CurrentDeviceIndex];

    // load player color mapped to current input channel
    if (DeviceCfg.ChannelToPlayerMap[ChannelIndex] <> CHANNEL_OFF) then
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

    // assignment slide
    SelectSlide := SelectsS[SelectAssigneeID];

    BarXOffset := SelectSlide.TextureSBG.X;
    BarYOffset := SelectSlide.TextureSBG.Y + SelectSlide.TextureSBG.H + BarUpperSpacing;
    BarWidth   := SelectSlide.TextureSBG.W;

    State.ChannelIndex := ChannelIndex;

    DrawVUMeter(State, BarXOffset, BarYOffset, BarWidth, BarHeight);
    DrawPitch(State, BarXOffset, BarYOffset+BarHeight, BarWidth, BarHeight);
  end;

  Result := true;
end;

end.
