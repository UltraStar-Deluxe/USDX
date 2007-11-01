unit UScreenWelcome;

interface

{$I switches.inc}

uses
  UMenu, SDL, SysUtils, UThemes;

type
  TScreenWelcome = class(TMenu)
    public
      Animation:    real;
      Fadeout:      boolean;
      constructor Create; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      function Draw: boolean; override;
      procedure onShow; override;
  end;

implementation

uses UGraphic, UTime, USkins;

function TScreenWelcome.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then begin
    case PressedKey of
      SDLK_ESCAPE,
      SDLK_BACKSPACE :
        begin
          Result := False;
        end;
      SDLK_RETURN:
        begin
          FadeTo(@ScreenMain);
          Fadeout := true;
        end;
    end;
  end;
end;

constructor TScreenWelcome.Create;
begin
  inherited Create;
  AddStatic(-10, -10,    0, 0, 1, 1, 1, Skin.GetTextureFileName('ButtonAlt') , 'JPG', 'Transparent');
  AddStatic(-500, 440, 200, 5, 0, 0, 0, Skin.GetTextureFileName('Rectangle'), 'JPG', 'Font Black');
  AddStatic(-500, 472, 200, 5, 0, 0, 0, Skin.GetTextureFileName('Rectangle'), 'JPG', 'Font Black');
  AddStatic(-500, 504, 200, 5, 0, 0, 0, Skin.GetTextureFileName('Rectangle'), 'JPG', 'Font Black');
  AddStatic(-500, 536, 200, 5, 0, 0, 0, Skin.GetTextureFileName('Rectangle'), 'JPG', 'Font Black');
  AddStatic(-500, 568, 200, 5, 0, 0, 0, Skin.GetTextureFileName('Rectangle'), 'JPG', 'Font Black');
  Animation := 0;
  Fadeout := false;
end;

procedure TScreenWelcome.onShow;
begin
  CountSkipTimeSet;
end;

function TScreenWelcome.Draw: boolean;
var
  Min:    real;
  Max:    real;
  Wsp:    real;
  Pet:    integer;
begin
  // star animation
  Animation := Animation + TimeSkip*1000;

  // draw nothing
  Min := 0; Max := 1000;
  if (Animation >= Min) and (Animation < Max) then begin
  end;

  // popup
  Min := 1000; Max := 1120;
  if (Animation >= Min) and (Animation < Max) then begin
    Wsp := (Animation - Min) / (Max - Min);
    Static[0].Texture.X := 600;
    Static[0].Texture.Y := 600 - Wsp * 230;
    Static[0].Texture.W := 200;
    Static[0].Texture.H := Wsp * 230;
  end;

  // bounce
  Min := 1120; Max := 1200;
  if (Animation >= Min) and (Animation < Max) then begin
    Wsp := (Animation - Min) / (Max - Min);
    Static[0].Texture.Y := 370 + Wsp * 50;
    Static[0].Texture.H := 230 - Wsp * 50;
  end;

  // run
  Min := 1500; Max := 3500;
  if (Animation >= Min) and (Animation < Max) then begin
    Wsp := (Animation - Min) / (Max - Min);

    Static[0].Texture.X := 600 - Wsp * 1400;
    Static[0].Texture.H := 180;


    for Pet := 1 to 5 do begin
      Static[Pet].Texture.X := 770 - Wsp * 1400;
      Static[Pet].Texture.W := 150 + Wsp * 200;
      Static[Pet].Texture.Alpha := Wsp * 0.5;
    end;
  end;

  Min := 3500;
  if (Animation >= Min) and (not Fadeout) then begin
    FadeTo(@ScreenMain);
    Fadeout := true;
  end;

  inherited Draw;
end;

end.
