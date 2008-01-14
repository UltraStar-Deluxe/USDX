unit UScreenOptionsRecord;

interface

{$I switches.inc}

uses
  UMenu, SDL, UDisplay, UMusic, UFiles, UIni, UThemes;

type
  TScreenOptionsRecord = class(TMenu)
    private
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
          AudioPlayback.PlayBack;
          FadeTo(@ScreenOptions);
        end;
      SDLK_RETURN:
        begin
          if SelInteraction = 5 then begin
            Ini.Save;
            AudioPlayback.PlayBack;
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
            AudioPlayback.PlayOption;
            InteractInc;
          end;
//          if SelInteraction = 0 then UpdateCard;
          UpdateCard;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 4) then begin
            AudioPlayback.PlayOption;
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
  I:      integer;
  SC:     integer;
  SCI:    integer;
begin
  inherited Create;

  LoadFromTheme(Theme.OptionsRecord);

  SetLength(ICard, Length(AudioInputProcessor.SoundCard));
  for SC := 0 to High(AudioInputProcessor.SoundCard) do
    ICard[SC] := AudioInputProcessor.SoundCard[SC].Description;

  if (Length(AudioInputProcessor.SoundCard) > 0) then
  begin
    SetLength(IInput, Length(AudioInputProcessor.SoundCard[Ini.Card].Input));
    for SCI := 0 to High(AudioInputProcessor.SoundCard[Ini.Card].Input) do
      IInput[SCI] := AudioInputProcessor.SoundCard[Ini.Card].Input[SCI].Name;


    AddSelectSlide(Theme.OptionsRecord.SelectSlideCard, Ini.Card, ICard);

    SelectSlideInput    := AddSelectSlide(Theme.OptionsRecord.SelectSlideInput, Ini.CardList[0].Input, IInput);
    SelectSlideChannelL := AddSelectSlide(Theme.OptionsRecord.SelectSlideChannelL, Ini.CardList[0].ChannelL, IChannel);
    SelectSlideChannelR := AddSelectSlide(Theme.OptionsRecord.SelectSlideChannelR, Ini.CardList[0].ChannelR, IChannel);

    AddSelect(Theme.OptionsSound.SelectMicBoost, Ini.MicBoost, IMicBoost);
  end;

  AddButton(Theme.OptionsRecord.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

  Interaction := 0;
end;

procedure TScreenOptionsRecord.onShow;
begin
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
  SC:     integer;
  SCI:    integer;
begin
  writeln( 'Update Card') ;
  AudioInput.CaptureStop;
  try
    SC := Ini.Card;
  //  if SC = 1 then beep;

    SetLength(IInput, Length(AudioInputProcessor.SoundCard[SC].Input));
    for SCI := 0 to High(AudioInputProcessor.SoundCard[SC].Input) do begin
      IInput[SCI] := AudioInputProcessor.SoundCard[SC].Input[SCI].Name;
  //    Log.LogError(IInput[SCI]);
    end;


    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideInput, SelectSlideInput, IInput, Ini.CardList[SC].Input);
    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideChannelL, SelectSlideChannelL, IChannel, Ini.CardList[SC].ChannelL);
    UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideChannelR, SelectSlideChannelR, IChannel, Ini.CardList[SC].ChannelR);

  finally
    AudioInput.CaptureStart;
  end;
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
