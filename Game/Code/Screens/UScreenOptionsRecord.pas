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
      SelectSlideInputID:   integer;
      SelectSlideChannelID: array of integer;
      TextPitchID: array of integer;

      // interaction IDs 
      ExitButtonIID: integer;

      // dummy data for non-available channels
      ChannelToPlayerMapDummy: integer;

      // preview channel-buffers
      PreviewChannel: array of TCaptureBuffer;

      procedure StartPreview;
      procedure StopPreview;
      procedure UpdateCard;
    public
      constructor Create; override;
      function    Draw: boolean; override;
      function    ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean; override;
      procedure   onShow; override;
      procedure   onHide; override;
  end;

implementation

uses
  SysUtils,
  SDL,
  gl,
  UGraphic,
  UDraw,
  UMain,
  UMenuSelectSlide,
  UMenuText,
  UFiles,
  UDisplay,
  UIni,
  ULog;

function TScreenOptionsRecord.ParseInput(PressedKey: Cardinal; CharCode: WideChar; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    // check normal keys
    case WideCharUpperCase(CharCode)[1] of
      'Q':
        begin
          Result := false;
          Exit;
        end;
    end;
    
    // check special keys
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE:
        begin
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
          UpdateCard;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction < ExitButtonIID) then
          begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
          UpdateCard;
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
  ButtonTheme: TThemeButton;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsRecord);

  // set CurrentDeviceIndex to a valid device
  if (Length(AudioInputProcessor.Device) > 0) then
    CurrentDeviceIndex := 0
  else
    CurrentDeviceIndex := -1;

  PreviewDeviceIndex := -1;

  // init sliders if at least one device was detected
  if (Length(AudioInputProcessor.Device) > 0) then
  begin
    InputDevice := AudioInputProcessor.Device[CurrentDeviceIndex];
    InputDeviceCfg := @Ini.InputDeviceConfig[InputDevice.CfgIndex];

    // init device-selection slider
    SetLength(InputDeviceNames, Length(AudioInputProcessor.Device));
    for DeviceIndex := 0 to High(AudioInputProcessor.Device) do
    begin
      InputDeviceNames[DeviceIndex] := AudioInputProcessor.Device[DeviceIndex].Description;
    end;
    // add device-selection slider (InteractionID: 0)
    AddSelectSlide(Theme.OptionsRecord.SelectSlideCard, CurrentDeviceIndex, InputDeviceNames);

    // init source-selection slider
    SetLength(InputSourceNames, Length(InputDevice.Source));
    for SourceIndex := 0 to High(InputDevice.Source) do
    begin
      InputSourceNames[SourceIndex] := InputDevice.Source[SourceIndex].Name;
    end;
    // add source-selection slider (InteractionID: 1)
    SelectSlideInputID := AddSelectSlide(Theme.OptionsRecord.SelectSlideInput,
        InputDeviceCfg.Input, InputSourceNames);

    // find max. channel count of all devices
    MaxChannelCount := 0;
    for DeviceIndex := 0 to High(AudioInputProcessor.Device) do
    begin
      if (AudioInputProcessor.Device[DeviceIndex].AudioFormat.Channels > MaxChannelCount) then
        MaxChannelCount := AudioInputProcessor.Device[DeviceIndex].AudioFormat.Channels;
    end;

    // init channel-to-player mapping sliders
    SetLength(SelectSlideChannelID, MaxChannelCount);
    SetLength(SelectSlideChannelTheme, MaxChannelCount);
    SetLength(TextPitchID, MaxChannelCount);

    for ChannelIndex := 0 to MaxChannelCount-1 do
    begin
      // copy reference slide
      SelectSlideChannelTheme[ChannelIndex] :=
        Theme.OptionsRecord.SelectSlideChannel;
      // set current channel-theme
      ChannelTheme := @SelectSlideChannelTheme[ChannelIndex];
      // adjust vertical position
      ChannelTheme.Y := ChannelTheme.Y + ChannelIndex * ChannelTheme.H;
      // append channel index to name
      ChannelTheme.Text := ChannelTheme.Text + IntToStr(ChannelIndex+1);

      // add tone-pitch label
      TextPitchID[ChannelIndex] := AddText(
          ChannelTheme.X + ChannelTheme.W,
          ChannelTheme.Y + ChannelTheme.H/2,
          '-');

      // show/hide widgets depending on whether the channel exists
      if (ChannelIndex < Length(InputDeviceCfg.ChannelToPlayerMap)) then
      begin
        // current device has this channel

        // add slider
        SelectSlideChannelID[ChannelIndex] := AddSelectSlide(ChannelTheme^,
          InputDeviceCfg.ChannelToPlayerMap[ChannelIndex], IChannel);
      end
      else
      begin
        // current device does not have that many channels

        // add slider but hide it and assign a dummy variable to it
        SelectSlideChannelID[ChannelIndex] := AddSelectSlide(ChannelTheme^,
          ChannelToPlayerMapDummy, IChannel);
        SelectsS[SelectSlideChannelID[ChannelIndex]].Visible := false;

        // hide pitch label
        Text[TextPitchID[ChannelIndex]].Visible := false;
      end;
    end;

    // TODO: move from sound-options to record-options (Themes must be changed first)
    //AddSelect(Theme.OptionsSound.SelectMicBoost, Ini.MicBoost, IMicBoost);
  end;

  // add Exit-button
  ButtonTheme := Theme.OptionsRecord.ButtonExit;
  ButtonTheme.Y := Theme.OptionsRecord.SelectSlideChannel.Y +
                   MaxChannelCount *
                   Theme.OptionsRecord.SelectSlideChannel.H;
  AddButton(ButtonTheme);
  if (Length(Button[0].Text) = 0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);
  // store InteractionID
  ExitButtonIID := MaxChannelCount + 2;

  // set focus
  Interaction := 0;
end;

procedure TScreenOptionsRecord.UpdateCard;
var
  SourceIndex: integer;
  InputDevice: TAudioInputDevice;
  InputDeviceCfg: PInputDeviceConfig;
  ChannelIndex: integer;
begin
  Log.LogStatus('Update input-device', 'TScreenOptionsRecord.UpdateCard') ;

  StopPreview();

  // set CurrentDeviceIndex to a valid device
  if (CurrentDeviceIndex > High(AudioInputProcessor.Device)) then
    CurrentDeviceIndex := 0;

  // update sliders if at least one device was detected
  if (Length(AudioInputProcessor.Device) > 0) then
  begin
    InputDevice := AudioInputProcessor.Device[CurrentDeviceIndex];
    InputDeviceCfg := @Ini.InputDeviceConfig[InputDevice.CfgIndex];

    // update source-selection slider
    SetLength(InputSourceNames, Length(InputDevice.Source));
    for SourceIndex := 0 to High(InputDevice.Source) do
    begin
      InputSourceNames[SourceIndex] := InputDevice.Source[SourceIndex].Name;
    end;
    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideInput, SelectSlideInputID,
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
          SelectSlideChannelID[ChannelIndex], IChannel,
          InputDeviceCfg.ChannelToPlayerMap[ChannelIndex]);
        SelectsS[SelectSlideChannelID[ChannelIndex]].Visible := true;

        // show pitch label
        Text[TextPitchID[ChannelIndex]].Visible := true;
      end
      else
      begin
        // current device does not have that many channels

        // hide slider and assign a dummy variable to it
        UpdateSelectSlideOptions(SelectSlideChannelTheme[ChannelIndex],
          SelectSlideChannelID[ChannelIndex], IChannel,
          ChannelToPlayerMapDummy);
        SelectsS[SelectSlideChannelID[ChannelIndex]].Visible := false;

        // hide pitch label
        Text[TextPitchID[ChannelIndex]].Visible := false;
      end;
    end;
  end;

  StartPreview();
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
end;

procedure TScreenOptionsRecord.StartPreview;
var
  ChannelIndex: integer;
  Device: TAudioInputDevice;
begin
  if ((CurrentDeviceIndex >= 0) and
      (CurrentDeviceIndex <= High(AudioInputProcessor.Device))) then
  begin
    Device := AudioInputProcessor.Device[CurrentDeviceIndex];
    // set preview channel as active capture channel
    for ChannelIndex := 0 to High(Device.CaptureChannel) do
    begin
      PreviewChannel[ChannelIndex].Clear();
      Device.LinkCaptureBuffer(ChannelIndex, PreviewChannel[ChannelIndex]);
    end;
    Device.Start();
    PreviewDeviceIndex := CurrentDeviceIndex;
  end;
end;

procedure TScreenOptionsRecord.StopPreview;
var
  ChannelIndex: integer;
  Device: TAudioInputDevice;
begin
  if ((PreviewDeviceIndex >= 0) and
      (PreviewDeviceIndex <= High(AudioInputProcessor.Device))) then
  begin
    Device := AudioInputProcessor.Device[PreviewDeviceIndex];
    Device.Stop;
    for ChannelIndex := 0 to High(Device.CaptureChannel) do
      Device.CaptureChannel[ChannelIndex] := nil;
  end;
  PreviewDeviceIndex := -1;
end;

function TScreenOptionsRecord.Draw: boolean;
var
  i: integer;
  x1, x2, y1, y2: real;
  R, G, B, RD, GD, BD: real;
  ChannelIndex: integer;
  Device: TAudioInputDevice;
  DeviceCfg: PInputDeviceConfig;
  SelectSlide: TSelectSlide;
  ToneBoxWidth: real;
  Volume: single;
begin
  DrawBG;
  DrawFG;

  if ((PreviewDeviceIndex >= 0) and
      (PreviewDeviceIndex <= High(AudioInputProcessor.Device))) then
  begin
    Device := AudioInputProcessor.Device[PreviewDeviceIndex];
    DeviceCfg := @Ini.InputDeviceConfig[Device.CfgIndex];

    glBegin(GL_QUADS);
      for ChannelIndex := 0 to High(Device.CaptureChannel) do
      begin
        // load player color mapped to current input channel
        if (DeviceCfg.ChannelToPlayerMap[ChannelIndex] > 0) then
        begin
          // set mapped channel to corresponding player-color
          LoadColor(R, G, B, 'P'+ IntToStr(DeviceCfg.ChannelToPlayerMap[ChannelIndex]) + 'Dark');
        end
        else
        begin
          // set non-mapped channel to white
          R := 1; G := 1; B := 1;
        end;

        // dark player colors
        RD := 0.2 * R;
        GD := 0.2 * G;
        BD := 0.2 * B;

        // channel select slide
        SelectSlide := SelectsS[SelectSlideChannelID[ChannelIndex]];

        //////////
        // draw Volume
        //

        // coordinates for black rect
        x1 := SelectSlide.TextureSBG.X;
        x2 := x1 + SelectSlide.TextureSBG.W;
        y2 := SelectSlide.TextureSBG.Y + SelectSlide.TextureSBG.H;
        y1 := y2 - 11;

        // draw black background-rect
        glColor3f(0, 0, 0);
        glVertex2f(x1, y1);
        glVertex2f(x2, y1);
        glVertex2f(x2, y2);
        glVertex2f(x1, y2);

        Volume := PreviewChannel[ChannelIndex].MaxSampleVolume();

        // coordinates for volume bar
        x1 := x1 + 1;
        x2 := x1 + Trunc((SelectSlide.TextureSBG.W-4) * Volume) + 1;
        y1 := y1 + 1;
        y2 := y2 - 1;

        // draw volume bar
        glColor3f(RD, GD, BD);
        glVertex2f(x1, y1);
        glVertex2f(x1, y2);
        glColor3f(R, G, B);
        glVertex2f(x2, y2);
        glVertex2f(x2, y1);

        //////////
        // draw Pitch
        //

        // calc tone pitch
        PreviewChannel[ChannelIndex].AnalyzeBuffer();

        // coordinates for black rect
        x1 := SelectSlide.TextureSBG.X;
        x2 := x1 + SelectSlide.TextureSBG.W;
        y1 := SelectSlide.TextureSBG.Y + SelectSlide.TextureSBG.H;
        y2 := y1 + 11;

        // draw black background-rect
        glColor3f(0, 0, 0);
        glVertex2f(x1, y1);
        glVertex2f(x2, y1);
        glVertex2f(x2, y2);
        glVertex2f(x1, y2);

        // coordinates for tone boxes
        ToneBoxWidth := SelectSlide.TextureSBG.W / NumHalftones;
        y1 := y1 + 1;
        y2 := y2 - 1;

        // draw tone boxes
        for i := 0 to NumHalftones-1 do
        begin
          x1 := SelectSlide.TextureSBG.X + i * ToneBoxWidth + 2;
          x2 := x1 + ToneBoxWidth - 4;

          if ((PreviewChannel[ChannelIndex].ToneValid) and
              (PreviewChannel[ChannelIndex].ToneAbs = i)) then
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

        // update tone-pitch label
        Text[TextPitchID[ChannelIndex]].Text :=
            PreviewChannel[ChannelIndex].ToneString;
      end;
    glEnd;
  end;

  Result := True;
end;


end.
