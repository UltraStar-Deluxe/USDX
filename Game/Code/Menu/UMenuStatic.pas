unit UMenuStatic;

interface
uses UTexture, OpenGL12;

type
  TStatic = class
    public
      Texture:    TTexture; // Button Screen position and size
      Visible:    boolean;

      //Reflection Mod
      Reflection:           boolean;
      Reflectionspacing:    Real;

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

end;

constructor TStatic.Create(Textura: TTexture);
begin
  inherited Create;
  Texture := Textura;
end;

end.
