unit UScreenLoading;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

uses
  UMenu,
  SDL,
  SysUtils,
  UThemes,
  OpenGL12;

type
  TScreenLoading = class(TMenu)
    public
      Fadeout:      boolean;
      constructor Create; override;
      procedure   onShow; override;
      function    ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean; override;
      function    GetBGTexNum: GLUInt;
  end;

implementation

uses UGraphic,
     UTime;

function TScreenLoading.ParseInput(PressedKey: Cardinal; ScanCode: byte; PressedDown: Boolean): Boolean;
begin
  Result := true;
end;

constructor TScreenLoading.Create;
begin
  inherited Create;

  LoadFromTheme(Theme.Loading);

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
