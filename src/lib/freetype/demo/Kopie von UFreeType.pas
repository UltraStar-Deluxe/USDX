unit UFreeType;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  FreeType,
  gl,
  glu,
  classes,
  sysutils;

type
  // This holds all of the information related to any
  // freetype font that we want to create.
  TFontData = class
    h: single;                  ///< Holds the height of the font.
    textures: array of GLuint;  ///< Holds the texture id's
    list_base: GLuint;          ///< Holds the first display list id

    // The init function will create a font of
    // of the height h from the file fname.
    constructor Create(const fname: string; h: cardinal);

    // Free all the resources assosiated with the font.
    destructor Destroy(); override;
  end;

  TFreeType = class
    public
      // The flagship function of the library - this thing will print
      // out text at window coordinates x,y, using the font ft_font.
      // The current modelview matrix will also be applied to the text.
      class procedure print(ft_font: TFontData; x, y: single; const str: string);
  end;


implementation


// This function gets the first power of 2 >= the
// int that we pass it.
function next_p2 ( a: integer ): integer; inline;
begin
  Result := 1;
  while (Result < a) do
    Result := Result shl 1;
end;

type
  PAGLuint = ^AGLuint;
  AGLuint = array[0..High(Word)] of GLuint;

// Create a display list coresponding to the given character.
procedure make_dlist ( face: FT_Face; ch: byte; list_base: GLuint; tex_base: PAGLuint );
var
  i, j: integer;
  width, height: integer;
  glyph: FT_Glyph;
  bitmap_glyph: FT_BitmapGlyph;
  bitmap: PFT_Bitmap;
  expanded_data: array of GLubyte;
  x, y: single;
begin
  // The first thing we do is get FreeType to render our character
  // into a bitmap.  This actually requires a couple of FreeType commands:

  // Load the Glyph for our character.
  if (FT_Load_Glyph( face, FT_Get_Char_Index( face, ch ), FT_LOAD_DEFAULT ) <> 0) then
    raise Exception.create('FT_Load_Glyph failed');

  // Move the face's glyph into a Glyph object.
  if (FT_Get_Glyph( face^.glyph, glyph ) <> 0) then
    raise Exception.create('FT_Get_Glyph failed');

  // Convert the glyph to a bitmap.
  FT_Glyph_To_Bitmap( glyph, ft_render_mode_normal, nil, 1 );
  bitmap_glyph := FT_BitmapGlyph(glyph);

  // This reference will make accessing the bitmap easier
  bitmap := @bitmap_glyph^.bitmap;

  // Use our helper function to get the widths of
  // the bitmap data that we will need in order to create
  // our texture.
  width := next_p2( bitmap.width );
  height := next_p2( bitmap.rows );

  // Allocate memory for the texture data.
  SetLength(expanded_data, 2 * width * height);

  // Here we fill in the data for the expanded bitmap.
  // Notice that we are using two channel bitmap (one for
  // luminocity and one for alpha), but we assign
  // both luminocity and alpha to the value that we
  // find in the FreeType bitmap.
  // We use the ?: operator so that value which we use
  // will be 0 if we are in the padding zone, and whatever
  // is the the Freetype bitmap otherwise.
  for j := 0 to height-1 do
  begin
    for i := 0 to width-1 do
    begin
      if ((i >= bitmap.width) or (j >= bitmap.rows)) then
        expanded_data[2*(i+j*width)] := 0
      else
        expanded_data[2*(i+j*width)] := byte(bitmap.buffer[i + bitmap.width*j]);
      expanded_data[2*(i+j*width)+1] := expanded_data[2*(i+j*width)];
    end;
  end;


  // Now we just setup some texture paramaters.
  glBindTexture( GL_TEXTURE_2D, tex_base[integer(ch)]);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);

  // Here we actually create the texture itself, notice
  // that we are using GL_LUMINANCE_ALPHA to indicate that
  // we are using 2 channel data.
  glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA, width, height,
      0, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @expanded_data[0] );

  //With the texture created, we don't need to expanded data anymore
  SetLength(expanded_data, 0);

  //So now we can create the display list
  glNewList(list_base+ch, GL_COMPILE);

  glBindTexture(GL_TEXTURE_2D, tex_base[ch]);

  glPushMatrix();

  //first we need to move over a little so that
  //the character has the right amount of space
  //between it and the one before it.
  glTranslatef(bitmap_glyph^.left, 0, 0);

  //Now we move down a little in the case that the
  //bitmap extends past the bottom of the line
  //(this is only true for characters like 'g' or 'y'.
  glTranslatef(0, bitmap_glyph^.top - bitmap.rows, 0);

  //Now we need to account for the fact that many of
  //our textures are filled with empty padding space.
  //We figure what portion of the texture is used by 
  //the actual character and store that information in
  //the x and y variables, then when we draw the
  //quad, we will only reference the parts of the texture
  //that we contain the character itself.
  x := bitmap.width / width;
  y := bitmap.rows  / height;

  //Here we draw the texturemaped quads.
  //The bitmap that we got from FreeType was not
  //oriented quite like we would like it to be,
  //so we need to link the texture to the quad
  //so that the result will be properly aligned.
  glBegin(GL_QUADS);
    glTexCoord2d(0, 0); glVertex2f(0, bitmap.rows);
    glTexCoord2d(0, y); glVertex2f(0, 0);
    glTexCoord2d(x, y); glVertex2f(bitmap.width, 0);
    glTexCoord2d(x, 0); glVertex2f(bitmap.width, bitmap.rows);
  glEnd();

  glPopMatrix();
  glTranslatef(face^.glyph^.advance.x shr 6, 0, 0);

  //increment the raster position as if we were a bitmap font.
  //(only needed if you want to calculate text length)
  //glBitmap(0,0,0,0,face->glyph->advance.x >> 6,0,NULL);

  //Finnish the display list
  glEndList();
end;


constructor TFontData.Create(const fname: string; h: cardinal);
var
  library_: FT_Library;
  //The object in which Freetype holds information on a given
  //font is called a "face".
  face: FT_Face;
  i: byte;
begin
  //Allocate some memory to store the texture ids.
  SetLength(textures, 128);

  Self.h := h;

  //Create and initilize a freetype font library.
  if (FT_Init_FreeType( library_ ) <> 0) then
    raise Exception.create('FT_Init_FreeType failed');

  //This is where we load in the font information from the file.
  //Of all the places where the code might die, this is the most likely,
  //as FT_New_Face will die if the font file does not exist or is somehow broken.
  if (FT_New_Face( library_, PChar(fname), 0, face ) <> 0) then
    raise Exception.create('FT_New_Face failed (there is probably a problem with your font file)');

  //For some twisted reason, Freetype measures font size
  //in terms of 1/64ths of pixels.  Thus, to make a font
  //h pixels high, we need to request a size of h*64.
  //(h shl 6 is just a prettier way of writting h*64)
  FT_Set_Char_Size( face, h shl 6, h shl 6, 96, 96);

  //Here we ask opengl to allocate resources for
  //all the textures and displays lists which we
  //are about to create.  
  list_base := glGenLists(128);
  glGenTextures( 128, @textures[0] );

  //This is where we actually create each of the fonts display lists.
  for i := 0 to 127 do
    make_dlist(face, i, list_base, @textures[0]);

  //We don't need the face information now that the display
  //lists have been created, so we free the assosiated resources.
  FT_Done_Face(face);

  //Ditto for the library.
  FT_Done_FreeType(library_);
end;

destructor TFontData.Destroy();
begin
  glDeleteLists(list_base, 128);
  glDeleteTextures(128, @textures[0]);
  SetLength(textures, 0);
end;

/// A fairly straight forward function that pushes
/// a projection matrix that will make object world
/// coordinates identical to window coordinates.
procedure pushScreenCoordinateMatrix(); inline;
var
  viewport: array [0..3] of GLint;
begin
  glPushAttrib(GL_TRANSFORM_BIT);
  glGetIntegerv(GL_VIEWPORT, @viewport);
  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();
  gluOrtho2D(viewport[0], viewport[2], viewport[1], viewport[3]);
  glPopAttrib();
end;

/// Pops the projection matrix without changing the current
/// MatrixMode.
procedure pop_projection_matrix(); inline;
begin
  glPushAttrib(GL_TRANSFORM_BIT);
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
  glPopAttrib();
end;

///Much like Nehe's glPrint function, but modified to work
///with freetype fonts.
class procedure TFreeType.print(ft_font: TFontData; x, y: single; const str: string);
var
  font: GLuint;
  h: single;
  i: cardinal;
  lines: TStringList;
  modelview_matrix: array[0..15] of single;
begin
  // We want a coordinate system where things coresponding to window pixels.
  pushScreenCoordinateMatrix();

  font := ft_font.list_base;
  h := ft_font.h / 0.63;      //We make the height about 1.5* that of

  lines := TStringList.Create();
  ExtractStrings([#13], [], PChar(str), lines);

  glPushAttrib(GL_LIST_BIT or GL_CURRENT_BIT  or GL_ENABLE_BIT or GL_TRANSFORM_BIT);
  glMatrixMode(GL_MODELVIEW);
  glDisable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glListBase(font);

  glGetFloatv(GL_MODELVIEW_MATRIX, @modelview_matrix);

  //This is where the text display actually happens.
  //For each line of text we reset the modelview matrix
  //so that the line's text will start in the correct position.
  //Notice that we need to reset the matrix, rather than just translating
  //down by h. This is because when each character is
  //draw it modifies the current matrix so that the next character
  //will be drawn immediatly after it.
  for i := 0 to lines.Count-1 do
  begin
    glPushMatrix();
    glLoadIdentity();
    glTranslatef(x, y - h*i, 0);
    glMultMatrixf(@modelview_matrix);

    //  The commented out raster position stuff can be useful if you need to
    //  know the length of the text that you are creating.
    //  If you decide to use it make sure to also uncomment the glBitmap command
    //  in make_dlist().
    //glRasterPos2f(0,0);
    glCallLists(Length(lines[i]), GL_UNSIGNED_BYTE, PChar(lines[i]));
    //float rpos[4];
    //glGetFloatv(GL_CURRENT_RASTER_POSITION ,rpos);
    //float len=x-rpos[0];

    glPopMatrix();
  end;

  glPopAttrib();

  pop_projection_matrix();
 
  lines.Free();
end;

end.
