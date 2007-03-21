unit USmpeg;

interface
uses SDL, smpeg, OpenGL12, SysUtils, UIni;

procedure OpenSmpeg(FileName: string);
procedure SkipSmpeg(Time: single);
procedure PlaySmpeg;
procedure UpdateSmpeg;
procedure CloseSmpeg;
function glmovie_init(Width : GLuint; Height : TGLuint ) : TGLenum;
procedure glmpeg_update(surface: PSDL_Surface; x: Sint32; y: Sint32; w: Uint32; h: Uint32); cdecl;
procedure DrawSmpeg(frame: PGLubyte);
procedure glmovie_resize( width : GLuint; height : GLuint );
procedure glmovie_quit;

var
  mpeg:       PSMPEG;
  mpeg_info:  TSMPEG_Info;
  surface:    PSDL_Surface;

type
  { Some data is redundant at this stage. }
  PGLMovieTexture = ^TGLMovieTexture;
  TGLMovieTexture = record
    id : TGLuint; (* OpenGL texture id. *)
    poly_width : TGLuint; (* Quad width for tile. *)
    poly_height : TGLuint; (* Quad height for tile. *)
    movie_width : TGLuint; (* Width of movie inside tile. *)
    movie_height : TGLuint; (* Height of movie inside tile. *)
    skip_rows : TGLuint; (* Number of rows of movie to skip *)
    skip_pixels : TGLuint; (* Number of columns of movie to skip *)
    row : TGLuint; (* Row number of tile in scheme. *)
    col : TGLuint; (* Column number of tile in scheme. *)
  end;

type
  TGLuintArray = array of TGLuint;
  PGLuintArray = ^TGLuintArray;
  TGLMovieTextureArray = array of TGLMovieTexture;
  PGLMovieTextureArray = ^TGLMovieTextureArray;

var
  (* Our evil maximum texture size. Boo 3Dfxnot  *)
  texture_size : TGLuint = 1024;//512;
  texture_ids : TGLuint;
  textures:     TGLMovieTexture;
  tiled_width : TGLuint = 0;
  tiled_height : TGLuint = 0;
  movie_width : TGLuint = 0;
  movie_height : TGLuint = 0;  

implementation

procedure OpenSmpeg(FileName: string);
begin
  mpeg := SMPEG_new(PChar(FileName), @mpeg_info, 0); // audio
  if ( mpeg = nil ) then begin
    SDL_Quit;
    Exit;
  end;

//  SMPEG_setvolume(mpeg, 50);
  SMPEG_enableaudio(mpeg, 0);

  (* Everything needs to be in RGB for GL, but needs to be 32-bit for SMPEG. *)
  surface := SDL_AllocSurface( SDL_SWSURFACE,
    mpeg_info.width,
    mpeg_info.height,
    32,
    $000000FF,
    $0000FF00,
    $00FF0000,
    $FF000000 );

  if ( surface = nil ) then begin
    SDL_Quit;
    Exit;
  end;

  (* *Initialize* with mpeg size. *)
  if (glmovie_init( mpeg_info.width, mpeg_info.height ) <> GL_NO_ERROR ) then begin
    SDL_Quit;
    Exit;
  end;

  SMPEG_setdisplay(mpeg, surface, nil, @glmpeg_update);
end;

procedure SkipSmpeg(Time: single);
begin
  SMPEG_skip(mpeg, Time);
end;

procedure PlaySmpeg;
begin
  SMPEG_play(mpeg);
end;

procedure UpdateSmpeg;
begin
//  glmpeg_update(surface,0,0,0,0);
  DrawSmpeg( PGLubyte( surface.pixels ) );
end;

procedure CloseSmpeg;
begin
//  glmovie_quit;
  SMPEG_delete(mpeg);
end;

function glmovie_init( Width : GLuint; Height : TGLuint ) : TGLenum;
type
  PGLubyteArray = ^TGLubyteArray;
  TGLubyteArray = array of TGLubyte;
var
  (* Initial black texels. *)
  pixels : TGLubyteArray;
  (* Absolute offsets from within tiled frame. *)
  //offset_x: GLuint;
  //offset_y: GLuint;
  skip_rows : GLuint;
  skip_pixels : GLuint;
  i, j, current : GLuint;
begin
  skip_rows := 0;
  current := 0;
  (* Save original movie dimensions. *)
  movie_width := width;
  movie_height := height;

  (* Get the power of 2 dimensions. *)
  tiled_width := 1024{512};
  tiled_height := 1024{512};

  texture_size := 1024{512};

  (* Time for fun with data type = record *)
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_DITHER);

  glGenTextures(1, @texture_ids);

      current := 0;
      (* Setup texture. *)
      textures.id := texture_ids;
      textures.poly_width := texture_size;
      textures.poly_height := texture_size;
      textures.movie_width := movie_width - 2;
      textures.movie_height := movie_height - 2;
      textures.row := i;
      textures.col := j;
      textures.skip_pixels := skip_pixels;
      textures.skip_rows := skip_rows;

      SetLength( pixels, textures.poly_width * textures.poly_height * 4 );
      if ( pixels = nil ) then
      begin
        glDeleteTextures(1, @texture_ids);
        result := GL_OUT_OF_MEMORY;
        exit;
      end;
      //FillChar( pixels^, textures[ current ].poly_width * textures[ current ].poly_height * 4, 0 );

      (* Do all of our useful binding. *)
      glBindTexture(GL_TEXTURE_2D, texture_ids);
//      glTexEnvf( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
      (* Specify our 256x256 black texture. *)
      glTexImage2D( GL_TEXTURE_2D,
        0,
        GL_RGB,
        1024{512},//textures.poly_width,
        1024{512},//textures.poly_height,
        0,
        GL_RGBA,
        GL_UNSIGNED_BYTE,
        @pixels[0] );
        SetLength( pixels, 0 );


  (* Simple state setup at the end. *)
  result := glGetError( );
end;

procedure glmpeg_update( surface : PSDL_Surface; x : Sint32; y : Sint32; w : Uint32;
  h : Uint32 ); cdecl;
var
  error : TGLenum;
begin
  glClear( GL_COLOR_BUFFER_BIT );
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
  DrawSmpeg( PGLubyte( surface.pixels ) );
  error := glGetError( );
  if ( error <> GL_NO_ERROR ) then Exit;
  SDL_GL_SwapBuffers;
end;

procedure DrawSmpeg(frame: PGLubyte);
var
  Shift:    TGLdouble;
  CropT:    real;
  CropB:    real;
  TexT:     real;
  TexB:     real;
  TexL:     real;
  TexR:     real;
  Wide:     boolean;
begin
  (* full screen mpeg *)
{  CropT := 0;
  CropB := 600;
  TexT := 0;
  TexB := 1;
  TexL := 0;
  TexW := 1;}

  // set movie texture crop
  Wide := false;
  if (textures.movie_width = 720-2) and (textures.movie_height = 344-2) then begin
    TexT := 0;
    TexB := 342/1024;
    Wide := true;
  end;
  if textures.movie_height = 304-2 then begin
    TexT := 0;
    TexB := 304/1024;
    Wide := true;
  end;
  if textures.movie_height = 152-2 then begin
    TexT := 0;
    TexB := 152/1024;
    Wide := true;
  end;

  CropT := 110; // (110/800 = 13,75% max crop)
  CropB := 490; // (110/800 = 13,75% max crop)

  if (textures.movie_height <> 304-2) and (textures.movie_height <> 152-2) and (textures.movie_height <> 344-2) then begin
    TexT := 110 / 600 * (textures.movie_height / 1024{512});
    TexB := 490 / 600 * (textures.movie_height / 1024{512});

    if Ini.MovieSize = 1 then begin
      // full screen size
      CropT := 0;
      CropB := 600;
      TexT := 0;
      TexB := textures.movie_height / 1024{512};
    end;
  end;

  TexL := {10}0/600 * (textures.movie_width / 1024{512});
  TexR := {590}600/600 * (textures.movie_width / 1024{512});

  glEnable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
  glColor3f(1, 1, 1);
  glBindTexture( GL_TEXTURE_2D, texture_ids );
//  glPixelStorei( GL_UNPACK_ROW_LENGTH, movie_width );
  glPixelStorei( GL_UNPACK_SKIP_ROWS, 0);
  glPixelStorei( GL_UNPACK_SKIP_PIXELS, 0);
  glTexSubImage2D( GL_TEXTURE_2D, 0, 0, (* offset_x *) 0, (* offset_y *) textures.movie_width + 2, textures.movie_height + 2, GL_RGBA, GL_UNSIGNED_BYTE, frame );

  // draw
  glBegin( GL_QUADS );
    glTexCoord2f(TexL, TexT); glVertex2f(0, CropT);
    glTexCoord2f(TexL, TexB); glVertex2f(0, CropB);
    glTexCoord2f(TexR, TexB); glVertex2f(800, CropB);
    glTexCoord2f(TexR, TexT); glVertex2f(800, CropT);
  glEnd;
  glDisable(GL_TEXTURE_2D);
end;

procedure glmovie_resize( width : GLuint; height : GLuint );
begin
  glViewport(0, 0, width, height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluOrtho2D(0, 800, 600, 0);
end;

procedure glmovie_quit;
begin
  glDeleteTextures(1, @texture_ids);
end;

end.
