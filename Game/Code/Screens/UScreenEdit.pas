unit UScreenEdit;

interface

uses UMenu, SDL, UThemes;

type
  TScreenEdit = class(TMenu)
    public
{      Tex_Background:     TTexture;
      FadeOut:            boolean;
      Path:               string;
      FileName:           string;}
      constructor Create; override;
      procedure onShow; override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
{      function Draw: boolean; override;
      procedure Finish;}
  end;

implementation

uses UGraphic, UMusic, USkins;

function TScreenEdit.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
  If (PressedDown) Then
  begin // Key Down
    case PressedKey of
      SDLK_ESCAPE :
        begin
          Music.PlayBack;
          FadeTo(@ScreenMain);
//          Result := false;
        end;
      SDLK_RETURN:
        begin
          if Interaction = 0 then begin
            Music.PlayStart;
            FadeTo(@ScreenEditConvert);
          end;
//          if Interaction = 1 then begin
//            Music.PlayStart;
//            FadeTo(@ScreenEditHeader);
//          end;

          if Interaction = 1 then begin
            Music.PlayBack;
            FadeTo(@ScreenMain);
          end;
        end;

      SDLK_DOWN:
        begin
          InteractNext;
        end;
      SDLK_UP:
        begin
          InteractPrev;
        end;
    end;
  end;
end;

constructor TScreenEdit.Create;
begin
  inherited Create;
  AddButton(400-200, 100 + 0*70, 400, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(10, 5, 0, 0, 0, 'Convert Midi to Txt');
//  Button[High(Button)].Text[0].Size := 11;

//  AddButton(400-200, 100 + 1*60, 400, 40, 'ButtonF');
//  AddButtonText(10, 5, 0, 0, 0, 'Edit Headers');

//  AddButton(400-200, 100 + 2*60, 400, 40, 'ButtonF');
//  AddButtonText(10, 5, 0, 0, 0, 'Set GAP');

  AddButton(400-200, 100 + 3*60, 400, 40, Skin.GetTextureFileName('ButtonF'));
  AddButtonText(10, 5, 0, 0, 0, 'Exit');

end;

procedure TScreenEdit.onShow;
begin
//  Interaction := 0;
end;

(*function TScreenEdit.Draw: boolean;
var
  Min:    integer;
  Sec:    integer;
  Tekst:  string;
  Pet:    integer;
  AktBeat:  integer;
begin
end;

procedure TScreenEdit.Finish;
begin
//
end;*)

end.
