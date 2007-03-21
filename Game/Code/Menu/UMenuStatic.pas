unit UMenuStatic;

interface
uses UTexture, OpenGL12;

type
  TStatic = class
    public
      Texture:    TTexture; // Button Screen position and size
      Visible:    boolean;
      procedure Draw;
      constructor Create(Textura: TTexture); overload;
  end;

implementation
uses UDrawTexture;

procedure TStatic.Draw;
var
  Pet:    integer;
begin
  if Visible then
    DrawTexture(Texture);
end;

constructor TStatic.Create(Textura: TTexture);
begin
  inherited Create;
  Texture := Textura;
end;

end.
