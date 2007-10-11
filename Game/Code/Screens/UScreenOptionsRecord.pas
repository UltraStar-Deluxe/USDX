unit UScreenOptionsRecord;

interface

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
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      procedure onShow; override;
      procedure UpdateCard;
  end;

implementation

uses SysUtils, UGraphic, URecord, ULog;

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
          if SelInteraction = 4 then begin
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
          if (SelInteraction >= 0) and (SelInteraction <= 3) then begin
            AudioPlayback.PlayOption;
            InteractInc;
          end;
          if SelInteraction = 0 then UpdateCard;
        end;
      SDLK_LEFT:
        begin
          if (SelInteraction >= 0) and (SelInteraction <= 3) then begin
            AudioPlayback.PlayOption;
            InteractDec;
          end;
          if SelInteraction = 0 then UpdateCard;
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

  SetLength(ICard, Length(Recording.SoundCard));
  for SC := 0 to High(Recording.SoundCard) do
    ICard[SC] := Recording.SoundCard[SC].Description;
//  end;

//  if Length(Recording.SoundCard[Ini.Card].Input) > 0 then begin

  {$IFDEF win32}
  // TODO : JB_Linux .... Audio Input ... had to remove to get it to run ???
  
    SetLength(IInput, Length(Recording.SoundCard[Ini.Card].Input));
    for SCI := 0 to High(Recording.SoundCard[Ini.Card].Input) do
      IInput[SCI] := Recording.SoundCard[Ini.Card].Input[SCI].Name;
//  end;

  AddSelectSlide(Theme.OptionsRecord.SelectSlideCard, Ini.Card, ICard);
  SelectSlideInput    := AddSelectSlide(Theme.OptionsRecord.SelectSlideInput, Ini.CardList[0].Input, IInput);
  SelectSlideChannelL := AddSelectSlide(Theme.OptionsRecord.SelectSlideChannelL, Ini.CardList[0].ChannelL, IChannel);
  SelectSlideChannelR := AddSelectSlide(Theme.OptionsRecord.SelectSlideChannelR, Ini.CardList[0].ChannelR, IChannel);

  {$ENDIF}


  AddButton(Theme.OptionsRecord.ButtonExit);
  if (Length(Button[0].Text)=0) then
    AddButtonText(14, 20, Theme.Options.Description[7]);

  Interaction := 0;
end;

procedure TScreenOptionsRecord.onShow;
begin
  Interaction := 0;
end;

procedure TScreenOptionsRecord.UpdateCard;
var
  SC:     integer;
  SCI:    integer;
begin
  SC := Ini.Card;
//  if SC = 1 then beep;

  SetLength(IInput, Length(Recording.SoundCard[SC].Input));
  for SCI := 0 to High(Recording.SoundCard[SC].Input) do begin
    IInput[SCI] := Recording.SoundCard[SC].Input[SCI].Name;
//    Log.LogError(IInput[SCI]);
  end;

  UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideInput, SelectSlideInput, IInput, Ini.CardList[SC].Input);
  UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideChannelL, SelectSlideChannelL, IChannel, Ini.CardList[SC].ChannelL);
  UpdateSelectSlideOptions(Theme.OptionsRecord.SelectSlideChannelR, SelectSlideChannelR, IChannel, Ini.CardList[SC].ChannelR);
end;

end.
