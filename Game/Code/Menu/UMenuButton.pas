unit UMenuButton;

interface
uses TextGL, UTexture, OpenGL12, UMenuText;

type
  TButton = class
    private
      SelectBool:           Boolean;
      constructor Create(); overload;
    public
      Text:                 Array of TText;
      Texture:              TTexture; // Button Screen position and size
      Texture2:             TTexture; // second texture only used for fading full resolution covers
//      Texture2Blend:        real; // blending factor for second texture (0=invisible, 1=visible)
      // now uses alpha

      DeselectType:         integer; // not used yet
      Visible:              boolean;
      //Reflection Mod
      Reflection:           boolean;
      Reflectionspacing:    Real;
      
      Selectable:           boolean;

      SelectColR:   real;
      SelectColG:   real;
      SelectColB:   real;
      SelectInt:    real;
      SelectTInt:   real;

      DeselectColR:   real;
      DeselectColG:   real;
      DeselectColB:   real;
      DeselectInt:    real;
      DeselectTInt:   real;

      procedure SetY(Value: real);
      procedure SetSelect(Value: Boolean);
      property X: real read Texture.x write Texture.x;
      property Y: real read Texture.y write SetY;
      property Z: real read Texture.z write Texture.z;
      property W: real read Texture.w write Texture.w;
      property H: real read Texture.h write Texture.h;
      property Selected: Boolean read SelectBool write SetSelect;

      procedure Draw;

      constructor Create(Textura: TTexture); overload;
      destructor Destroy; override;
  end;

implementation
uses UDrawTexture, SysUtils;

procedure TButton.SetY(Value: real);
var
  dY:   real;
  T:    integer;    // text
begin
  dY := Value - Texture.y;

  Texture.y := Value;

  for T := 0 to High(Text) do
    Text[T].Y := Text[T].Y + dY;

end;

procedure TButton.SetSelect(Value : Boolean);
var
  T:    integer;
begin
  SelectBool := Value;
  if (Value) then begin
    Texture.ColR := SelectColR;
    Texture.ColG := SelectColG;
    Texture.ColB := SelectColB;
    Texture.Int := SelectInt;

    Texture2.ColR := SelectColR;
    Texture2.ColG := SelectColG;
    Texture2.ColB := SelectColB;
    Texture2.Int := SelectInt;

    for T := 0 to High(Text) do
      Text[T].Int := SelectTInt;
  end else begin
    Texture.ColR := DeselectColR;
    Texture.ColG := DeselectColG;
    Texture.ColB := DeselectColB;
    Texture.Int := DeselectInt;

    Texture2.ColR := DeselectColR;
    Texture2.ColG := DeselectColG;
    Texture2.ColB := DeselectColB;
    Texture2.Int := DeselectInt;

    for T := 0 to High(Text) do
      Text[T].Int := DeselectTInt;
  end;
end;

constructor TButton.Create();
begin
  inherited Create;
  // We initialize all to 0, nil or false
  Visible := true;
  SelectBool := false;
  DeselectType := 0;
  Selectable := true;
  //Reflection Mod
  Reflection := true;

  // Default
//  SelectInt := 1;
//  DeselectInt := 0.5;

{  SelectColR := 0.5;
  SelectColG := 0.75;
  SelectColB := 0;
  SelectInt := 1;
  SelectTInt := 1;

  DeselectColR := 1;
  DeselectColG := 1;
  DeselectColB := 1;
  DeselectInt := 0.5;
  DeselectTInt := 1;}

  SelectColR := 1;
  SelectColG := 1;
  SelectColB := 1;
  SelectInt := 1;
  SelectTInt := 1;

  DeselectColR := 1;
  DeselectColG := 1;
  DeselectColB := 1;
  DeselectInt := 0.5;
  DeselectTInt := 1;


end;

// ***** Public methods ****** //

procedure TButton.Draw;
var
  T:    integer;
begin
  if Visible then begin
    DrawTexture(Texture);

    if Texture2.Alpha > 0 then begin
      Texture2.ScaleW := Texture.ScaleW;
      Texture2.ScaleH := Texture.ScaleH;

      Texture2.X := Texture.X;
      Texture2.Y := Texture.Y;
      Texture2.W := Texture.W;
      Texture2.H := Texture.H;

      Texture2.ColR := Texture.ColR;
      Texture2.ColG := Texture.ColG;
      Texture2.ColB := Texture.ColB;
      Texture2.Int := Texture.Int;

      Texture2.Z := Texture.Z;

      DrawTexture(Texture2);
    end;

    //Reflection Mod
    if (Reflection) then // Draw Reflections
    begin
      with Texture do
      begin
        //Bind Tex and GL Attributes
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_BLEND);
        glDepthRange(0, 10);
        glDepthFunc(GL_LEQUAL);
        //glDepthFunc(GL_GEQUAL);
        glEnable(GL_DEPTH_TEST);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        //glBlendFunc(GL_SRC_COLOR, GL_ZERO);
        glBindTexture(GL_TEXTURE_2D, TexNum);

        //Draw
        glBegin(GL_QUADS);//Top Left
          glColor4f(ColR * Int, ColG * Int, ColB * Int, Alpha-0.3);
          glTexCoord2f(TexX1*TexW, TexY2*TexH);
          glVertex3f(x, y+h*scaleH+ Reflectionspacing, z);

          //Bottom Left
          glColor4f(ColR * Int, ColG * Int, ColB * Int, 0);
          glTexCoord2f(TexX1*TexW, {TexY1*TexH*}0.5);
          glVertex3f(x, y+h*scaleH + h*scaleH/2 + Reflectionspacing, z);


          //Bottom Right
          glColor4f(ColR * Int, ColG * Int, ColB * Int, 0);
          glTexCoord2f(TexX2*TexW, {TexY1*TexH*}0.5);
          glVertex3f(x+w*scaleW, y+h*scaleH + h*scaleH/2 + Reflectionspacing, z);

          //Top Right
          glColor4f(ColR * Int, ColG * Int, ColB * Int, Alpha-0.3);
          glTexCoord2f(TexX2*TexW, TexY2*TexH);
          glVertex3f(x+w*scaleW, y+h*scaleH + Reflectionspacing, z);
        glEnd;
      end;
    end;

    for T := 0 to High(Text) do begin
      Text[T].Draw;
    end;
  end;
end;

// *****  ****** //

destructor TButton.Destroy;
begin
  inherited;
end;

constructor TButton.Create(Textura: TTexture);
begin
  Create();
  Texture := Textura;
  Texture.ColR := 0;
  Texture.ColG := 0.5;
  Texture.ColB := 0;
  Texture.Int := 1;
end;


end.
