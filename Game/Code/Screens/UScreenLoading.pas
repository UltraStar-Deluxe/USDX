unit UScreenLoading;

interface

uses
  UMenu, SDL, SysUtils, UThemes, OpenGL12;

type
  TScreenLoading = class(TMenu)
    public
      Fadeout:      boolean;
      constructor Create(Back: String); override;
      function ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      function GetBGTexNum: GLUInt;
      procedure onShow; override;
  end;

implementation

uses UGraphic, UTime;

function TScreenLoading.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
end;

constructor TScreenLoading.Create(Back: String);
var
  I:    integer;
begin
  inherited Create(Back);

  AddBackground(Theme.Loading.Background.Tex);

  for I := 0 to High(Theme.Loading.Static) do
    AddStatic(Theme.Loading.Static[I]);

  for I := 0 to High(Theme.Loading.Text) do
    AddText(Theme.Loading.Text[I]);

  Fadeout := false;
end;

procedure TScreenLoading.onShow;
begin
// nothing
end;

function TScreenLoading.GetBGTexNum: GLUInt;
begin
  Result := Self.BackImg.TexNum;
end;

end.
