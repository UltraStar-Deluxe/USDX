unit UScreenOptionsRecord;

interface

{$I switches.inc}

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes;

type
  TScreenOptionsRecord = class(TMenu)
    private
      Card: integer; // current input device

      SelectSlideInput:       integer;
      SelectSlideChannelL:    integer;
      SelectSlideChannelR:    integer;
    public
      constructor Create; override;
      function    Draw: boolean; override;
      function    ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure   onShow; override;
      procedure   onHide; override;
      procedure   UpdateCard;
  end;

implementation

uses SysUtils,
     UGraphic,
     URecord,
     UDraw,
     UMain,
     ULog;

function TScreenOptionsRecord.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_Q:
        begin
          Result := false;
        end;
      SDLK_ESCAPE,
      SDLK_BACKSPACE:
        begin
          Ini.Save;
          AudioPlayback.PlaySound(SoundLib.Back);
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 5 then begin
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
          if (SelInteraction >= 0) and (SelInteraction <= 4) then begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractInc;
          end;
//          if SelInteraction = 0 then UpdateCard;
          UpdateCard;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 4) then begin
            AudioPlayback.PlaySound(SoundLib.Option);
            InteractDec;
          end;
          UpdateCard;          
//          if SelInteraction = 0 then UpdateCard;
        end;
    end;
  end;
end;

constructor TScreenOptionsRecord.Create;
var
  SC:     integer;
  SCI:    integer;
  InputDevice: TAudioInputDevice;
  InputDeviceCfg: PInputDeviceConfig;
begin
  inherited Create;

  Card := 0;

  LoadFromTheme(Theme.OptionsRecord);

  SetLength(ICard, Length(AudioInputProcessor.Device));
  for SC := 0 to High(AudioInputProcessor.Device) do
    ICard[SC] := AudioInputProcessor.Device[SC].Description;

  if (Card > High(AudioInputProcessor.Device)) then
    Card := 0;

  if (Length(AudioInputProcessor.Device) > 0) then
  begin
    InputDevice := AudioInputProcessor.Device[Card];
    InputDeviceCfg := @Ini.InputDeviceConfig[InputDevice.CfgIndex];
  
    SetLength(IInput, Length(InputDevice.Source));
    for SCI := 0 to High(InputDevice.Source) do
      IInput[SCI] := InputDevice.Source[SCI].Name;

    AddSelectSlide(Theme.OptionsRecord.SelectSlideCard, Card, ICard);

    SelectSlideInput    := AddSelectSlide(Theme.OptionsRecord.SelectSlideInput,
        InputDeviceCfg^.Input, IInput);
    SelectSlideChannelL := AddSelectSlide(Theme.OptionsRecord.SelectSlideChannelL,
        InputDeviceCfg^.ChannelToPlayerMap[0], IChannel);
    SelectSlideChannelR := AddSelectSlide(Theme.OptionsRecord.SelectSlideChannelR,
        InputDeviceCfg^.ChannelToPlayerMap[1], IChannel);

    AddSelect(Theme.OptionsSound.SelectMicBoost, Ini.MicBoost, IMicBoost);
  end;

  AddButton(Theme.OptionsRecord.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

  Interaction := 0;
end;

procedure TScreenOptionsRecord.onShow;
begin
  inherited;

  Interaction := 0;
  writeln( 'AudioInput.CaptureStart') ;

  PlayersPlay := 2;  //  TODO :  This needs fixing
  AudioInput.CaptureStart;

end;

procedure TScreenOptionsRecord.onHide;
begin
  AudioInput.CaptureStop;
end;

procedure TScreenOptionsRecord.UpdateCard;
var
  SourceIndex: integer;
  InputDevice: TAudioInputDevice;
  InputDeviceCfg: PInputDeviceConfig;
begin
  Log.LogStatus('Update input-device', 'TScreenOptionsRecord.UpdateCard') ;

  AudioInput.CaptureStop;

  if (Card > High(AudioInputProcessor.Device)) then
    Card := 0;

  if (Length(AudioInputProcessor.Device) > 0) then
  begin
    InputDevice := AudioInputProcessor.Device[Card];
    InputDeviceCfg := @Ini.InputDeviceConfig[InputDevice.CfgIndex];

    SetLength(IInput, Length(InputDevice.Source));
    for SourceIndex := 0 to High(InputDevice.Source) do begin
      IInput[SourceIndex] := InputDevice.Source[SourceIndex].Name;
    end;

    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideInput, SelectSlideInput, IInput,
        InputDeviceCfg^.Input);
    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideChannelL, SelectSlideChannelL, IChannel,
        InputDeviceCfg^.ChannelToPlayerMap[0]);
    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideChannelR, SelectSlideChannelR, IChannel,
        InputDeviceCfg^.ChannelToPlayerMap[1]);
  end;

  AudioInput.CaptureStart;
end;

function TScreenOptionsRecord.Draw: boolean;
begin
  DrawBG;
  DrawFG;

    // TODO : this needs to be positioned correctly
    if PlayersPlay = 1 then
      SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 0);

    if PlayersPlay = 2 then begin
      SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 0);
      SingDrawOscilloscope(425 + 10*ScreenX, 55, 180, 40, 1);
    end;

    if PlayersPlay = 4 then begin
      if ScreenAct = 1 then begin
        SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 0);
        SingDrawOscilloscope(425 + 10*ScreenX, 55, 180, 40, 1);
      end;
      if ScreenAct = 2 then begin
        SingDrawOscilloscope(190 + 10*ScreenX, 55, 180, 40, 2);
        SingDrawOscilloscope(425 + 10*ScreenX, 55, 180, 40, 3);
      end;
    end;

    if PlayersPlay = 3 then begin
      SingDrawOscilloscope(75 + 10*ScreenX, 95, 100, 20, 0);
      SingDrawOscilloscope(370 + 10*ScreenX, 95, 100, 20, 1);
      SingDrawOscilloscope(670 + 10*ScreenX, 95, 100, 20, 2);
    end;

    if PlayersPlay = 6 then begin
      if ScreenAct = 1 then begin
        SingDrawOscilloscope( 75 + 10*ScreenX, 95, 100, 20, 0);
        SingDrawOscilloscope(370 + 10*ScreenX, 95, 100, 20, 1);
        SingDrawOscilloscope(670 + 10*ScreenX, 95, 100, 20, 2);
      end;
      if ScreenAct = 2 then begin
        SingDrawOscilloscope( 75 + 10*ScreenX, 95, 100, 20, 3);
        SingDrawOscilloscope(370 + 10*ScreenX, 95, 100, 20, 4);
        SingDrawOscilloscope(670 + 10*ScreenX, 95, 100, 20, 5);
      end;
    end;

  Result := True;
end;


end.
