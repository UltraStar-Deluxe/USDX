unit UMenuButton;

interface
uses TextGL, UTexture, OpenGL12, UMenuText;

type
  CButton = class of TButton;
  TButton = class
    protected
      SelectBool:           Boolean;

      FadeProgress:         Real;
      FadeLastTick:         Cardinal;
      
      DeSelectW:            Real;
      DeSelectH:            Real;
      PosX:                 Real;
      PosY:                 Real;

      constructor Create(); overload;
      
    public
      Text:                 Array of TText;
      Texture:              TTexture; // Button Screen position and size
      Texture2:             TTexture; // second texture only used for fading full resolution covers

      FadeTex:              TTexture; //Texture for beautiful fading
      FadeTexPos:           byte;     //Pos of the FadeTexture (0: Top, 1: Left, 2: Bottom, 3: Right)
//      Texture2Blend:        real; // blending factor for second texture (0=invisible, 1=visible)
      // now uses alpha

      DeselectType:         integer; // not used yet
      Visible:              boolean;
      //Reflection Mod
      Reflection:           boolean;
      Reflectionspacing:    Real;
      DeSelectReflectionspacing: Real;

      //Fade Mod
      Fade: Boolean;
      FadeText: Boolean;

      Selectable:           boolean;

      //No of the Parent Collection, 0 if in no Collection
      Parent:  Byte;

      SelectColR:   real;
      SelectColG:   real;
      SelectColB:   real;
      SelectInt:    real;
      SelectTInt:   real;
      //Fade Mod
      SelectW:      real;
      SelectH:      real;

      DeselectColR:   real;
      DeselectColG:   real;
      DeselectColB:   real;
      DeselectInt:    real;
      DeselectTInt:   real;

      procedure SetY(Value: real);
      procedure SetX(Value: real);
      procedure SetW(Value: real);
      procedure SetH(Value: real);

      procedure SetSelect(Value: Boolean); virtual;
      property X: real read PosX write SetX;
      property Y: real read PosY write SetY;
      property Z: real read Texture.z write Texture.z;
      property W: real read DeSelectW write SetW;
      property H: real read DeSelectH write SetH;
      property Selected: Boolean read SelectBool write SetSelect;

      procedure Draw; virtual;

      constructor Create(Textura: TTexture); overload;
      destructor Destroy; override;
  end;

implementation
uses UDrawTexture, SysUtils, windows;

procedure TButton.SetX(Value: real);
var
  dx:   real;
  T:    integer;    // text
begin
  {dY := Value - Texture.y;

  Texture.X := Value;

  for T := 0 to High(Text) do
    Text[T].X := Text[T].X + dY;}

  PosX := Value;
  if (FadeTex.TexNum = -1) then
    Texture.X := Value;

end;

procedure TButton.SetY(Value: real);
var
  dY:   real;
  T:    integer;    // text
begin
  {dY := Value - PosY;


  for T := 0 to High(Text) do
    Text[T].Y := Text[T].Y + dY;}
    
  PosY := Value;
  if (FadeTex.TexNum = -1) then
    Texture.y := Value;
end;

procedure TButton.SetW(Value: real);
begin
  if SelectW = DeSelectW then
    SelectW := Value;

  DeSelectW := Value;

  if Not Fade then
  begin
    if SelectBool then
      Texture.W := SelectW
    else
      Texture.W := DeSelectW;
  end;
end;

procedure TButton.SetH(Value: real);
begin
  if SelectH = DeSelectH then
    SelectH := Value;

  DeSelectH := Value;

  if Not Fade then
  begin
    if SelectBool then
      Texture.H := SelectH
    else
      Texture.H := DeSelectH;
  end;
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

    //Fade Mod
    if Fade then
    begin
      if (FadeProgress <= 0) then
        FadeProgress := 0.125;
    end
    else
    begin
      Texture.W := SelectW;
      Texture.H := SelectH;
    end;
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

    //Fade Mod
    if Fade then
    begin
      if (FadeProgress >= 1) then
        FadeProgress := 0.875;
    end
    else
    begin
      Texture.W := DeSelectW;
      Texture.H := DeSelectH;
    end;
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

  FadeTex.TexNum := -1;

  FadeProgress := 0;
  Fade := False;
  FadeText := False;
  SelectW := DeSelectW;
  SelectH := DeSelectH;

  PosX := 0;
  PosY := 0;

  Parent := 0;
end;

// ***** Public methods ****** //

procedure TButton.Draw;
var
  T:    integer;
  Tick: Cardinal;
  Spacing: Real;
begin
  if Visible then begin
    //Fade Mod
    if Fade then
    begin
      if (FadeProgress < 1) and (FadeProgress > 0) then
      begin
        Tick := GetTickCount div 16;
        if (Tick <> FadeLastTick) then
        begin
          FadeLastTick := Tick;
          if SelectBool then
            FadeProgress := FadeProgress + 0.125
          else
            FadeProgress := FadeProgress - 0.125;

          if (FadeText) then
          begin
            For T := 0 to high(Text) do
            begin
              Text[T].MoveX := (SelectW - DeSelectW) * FadeProgress;
              Text[T].MoveY := (SelectH - DeSelectH) * FadeProgress;
            end;
          end;
        end;
      end;
      //Method without Fade Texture
      if (FadeTex.TexNum = -1) then
      begin
      Texture.W := DeSelectW + (SelectW - DeSelectW) * FadeProgress;
      Texture.H := DeSelectH + (SelectH - DeSelectH) * FadeProgress;
      end
      else //method with Fade Texture
      begin
        Texture.W := DeSelectW;
        Texture.H := DeSelectH;

        FadeTex.ColR := Texture.ColR;
        FadeTex.ColG := Texture.ColG;
        FadeTex.ColB := Texture.ColB;
        FadeTex.Int := Texture.Int;

        FadeTex.Z := Texture.Z;

        FadeTex.Alpha := Texture.Alpha;
        FadeTex.TexX1 := 0;
        FadeTex.TexX2 := 1;
        FadeTex.TexY1 := 0;
        FadeTex.TexY2 := 1;

        Case FadeTexPos of
          0: //FadeTex on Top
            begin
              //Standard Texture
              Texture.X := PosX;
              Texture.Y := PosY + (SelectH - DeSelectH) * FadeProgress;
              //Fade Tex
              FadeTex.X := PosX;
              FadeTex.Y := PosY;
              FadeTex.W := Texture.W;
              FadeTex.H := (SelectH - DeSelectH) * FadeProgress;
              FadeTex.ScaleW := Texture.ScaleW;
              //Some Hack that Fixes a little Space between both Textures
              FadeTex.TexY2 := 0.9;
            end;
          1: //FadeTex on Left
            begin
              //Standard Texture
              Texture.X := PosX + (SelectW - DeSelectW) * FadeProgress;
              Texture.Y := PosY;
              //Fade Tex
              FadeTex.X := PosX;
              FadeTex.Y := PosY;
              FadeTex.H := Texture.H;
              FadeTex.W := (SelectW - DeSelectW) * FadeProgress;
              FadeTex.ScaleH := Texture.ScaleH;
              //Some Hack that Fixes a little Space between both Textures
              FadeTex.TexX2 := 0.9;
            end;
          2: //FadeTex on Bottom
            begin
              //Standard Texture
              Texture.X := PosX;
              Texture.Y := PosY;
              //Fade Tex
              FadeTex.X := PosX;
              FadeTex.Y := PosY  + (SelectH - DeSelectH) * FadeProgress;;
              FadeTex.W := Texture.W;
              FadeTex.H := (SelectH - DeSelectH) * FadeProgress;
              FadeTex.ScaleW := Texture.ScaleW;
              //Some Hack that Fixes a little Space between both Textures
              FadeTex.TexY1 := 0.1;
            end;
          3: //FadeTex on Right
            begin
              //Standard Texture
              Texture.X := PosX;
              Texture.Y := PosY;
              //Fade Tex
              FadeTex.X := PosX + (SelectW - DeSelectW) * FadeProgress;
              FadeTex.Y := PosY;
              FadeTex.H := Texture.H;
              FadeTex.W := (SelectW - DeSelectW) * FadeProgress;
              FadeTex.ScaleH := Texture.ScaleH;
              //Some Hack that Fixes a little Space between both Textures
              FadeTex.TexX1 := 0.1;
            end;
        end;
      end;
    end
    else if (FadeText) then
    begin
      Text[T].MoveX := (SelectW - DeSelectW);
      Text[T].MoveY := (SelectH - DeSelectH);
    end;

    DrawTexture(Texture);

    //Draw FadeTex
    if (FadeTex.TexNum <> -1) then
      DrawTexture(FadeTex);

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
      if (FadeProgress <> 0) AND (FadeProgress <> 1) then
      begin
        Spacing := DeSelectReflectionspacing - (DeSelectReflectionspacing - Reflectionspacing) * FadeProgress;
      end
      else if SelectBool then
        Spacing := Reflectionspacing
      else
        Spacing := DeSelectReflectionspacing;
        
      with Texture do
      begin
        //Bind Tex and GL Attributes
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_BLEND);

        glDepthRange(0, 10);
        glDepthFunc(GL_LEQUAL);
        glEnable(GL_DEPTH_TEST);

        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glBindTexture(GL_TEXTURE_2D, TexNum);

        //Draw
        glBegin(GL_QUADS);//Top Left
          glColor4f(ColR * Int, ColG * Int, ColB * Int, (Alpha-0.3)*FadeProgress*FadeProgress*FadeProgress);
          glTexCoord2f(TexX1*TexW, TexY2*TexH);
          glVertex3f(x, y+h*scaleH+ Spacing, z);

          //Bottom Left
          glColor4f(ColR * Int, ColG * Int, ColB * Int, 0);
          glTexCoord2f(TexX1*TexW, {TexY1*TexH*}0.5);
          glVertex3f(x, y+h*scaleH + h*scaleH/2 + Spacing, z);


          //Bottom Right
          glColor4f(ColR * Int, ColG * Int, ColB * Int, 0);
          glTexCoord2f(TexX2*TexW, {TexY1*TexH*}0.5);
          glVertex3f(x+w*scaleW, y+h*scaleH + h*scaleH/2 + Spacing, z);

          //Top Right
          glColor4f(ColR * Int, ColG * Int, ColB * Int, (Alpha-0.3)*FadeProgress*FadeProgress*FadeProgress);
          glTexCoord2f(TexX2*TexW, TexY2*TexH);
          glVertex3f(x+w*scaleW, y+h*scaleH + Spacing, z);
        glEnd;

        glDisable(GL_TEXTURE_2D);
        glDisable(GL_DEPTH_TEST); 
        glDisable(GL_BLEND);
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
