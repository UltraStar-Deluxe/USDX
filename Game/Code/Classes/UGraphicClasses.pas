// notes:
unit UGraphicClasses;

interface
uses UTexture;
const  DelayBetweenFrames : Cardinal = 60;
type

 TParticleType=(GoldenNote, PerfectNote, NoteHitTwinkle, PerfectLineTwinkle, ColoredStar, Flare);

 TColour3f = Record
               r, g, b: Real;
             end;

 TParticle = Class
   X, Y     : Real;     //Position
   Screen   : Integer;
   W, H     : Cardinal; //dimensions of particle
   Col      : array of TColour3f; // Colour(s) of particle
   Scale    : array of Real;      // Scaling factors of particle layers
   Frame    : Byte;     //act. Frame
   Tex      : Cardinal; //Tex num from Textur Manager
   Live     : Byte;     //How many Cycles before Kill
   RecIndex : Integer;  //To which rectangle this particle belongs (only GoldenNote)
   StarType : TParticleType;  // GoldenNote | PerfectNote | NoteHitTwinkle | PerfectLineTwinkle
   Alpha    : Real;     // used for fading...
   mX, mY   : Real;     // movement-vector for PerfectLineTwinkle
   SizeMod  : Real;     // experimental size modifier
   SurviveSentenceChange : Boolean;

   Constructor Create(cX,cY: Real; cScreen: Integer; cLive: Byte; cFrame : integer; cRecArrayIndex : Integer; cStarType : TParticleType; Player: Cardinal);
   Destructor Destroy(); override;
   procedure Draw;
   procedure LiveOn;
 end;

 RectanglePositions   = Record
   xTop, yTop, xBottom, yBottom : Real;
   TotalStarCount   : Integer;
   CurrentStarCount : Integer;
   Screen           : Integer;
 end;

 PerfectNotePositions = Record
   xPos, yPos : Real;
   Screen     : Integer;
 end;

 TEffectManager = Class
   Particle      : array of TParticle;
   LastTime      : Cardinal;
   RecArray      : Array of RectanglePositions;
   TwinkleArray  : Array[0..5] of Real; // store x-position of last twinkle for every player
   PerfNoteArray : Array of PerfectNotePositions;

   FlareTex: TTexture;

   constructor Create;
   destructor  Destroy; override;
   procedure Draw;
   function  Spawn(X, Y: Real;
                   Screen: Integer;
                   Live: Byte;
                   StartFrame: Integer;
                   RecArrayIndex: Integer;  // this is only used with GoldenNotes
                   StarType: TParticleType;
                   Player: Cardinal         // for PerfectLineTwinkle
             ): Cardinal;
   procedure SpawnRec();
   procedure Kill(index: Cardinal);
   procedure KillAll();
   procedure SentenceChange();
   procedure SaveGoldenStarsRec(Xtop, Ytop, Xbottom, Ybottom: Real);
   procedure SavePerfectNotePos(Xtop, Ytop: Real);
   procedure GoldenNoteTwinkle(Top,Bottom,Right: Real; Player: Integer);
   procedure SpawnPerfectLineTwinkle();
 end;

var GoldenRec : TEffectManager;

implementation
uses sysutils, Windows,OpenGl12, UIni, UMain, UThemes, USkins, UGraphic, UDrawTexture, math, dialogs;

//TParticle
Constructor TParticle.Create(cX,cY: Real; cScreen: Integer; cLive: Byte; cFrame : integer; cRecArrayIndex : Integer; cStarType : TParticleType; Player: Cardinal);
begin
  inherited Create;
  // in this constructor we set all initial values for our particle
  X := cX;
  Y := cY;
  Screen := cScreen;
  Live := cLive;
  Frame:= cFrame;
  RecIndex := cRecArrayIndex;
  StarType := cStarType;
  Alpha := (-cos((Frame+1)*2*pi/16)+1); // neat fade-in-and-out
  SetLength(Scale,1);
  Scale[0] := 1;
  SurviveSentenceChange := False;
  SizeMod := 1;
  case cStarType of
    GoldenNote:
        begin
          Tex := Tex_Note_Star.TexNum;
          W := 20;
          H := 20;
          SetLength(Scale,4);
          Scale[1]:=0.8;
          Scale[2]:=0.4;
          Scale[3]:=0.3;
          SetLength(Col,4);
          Col[0].r := 1;
          Col[0].g := 0.7;
          Col[0].b := 0.1;

          Col[1].r := 1;
          Col[1].g := 1;
          Col[1].b := 0.4;

          Col[2].r := 1;
          Col[2].g := 1;
          Col[2].b := 1;

          Col[3].r := 1;
          Col[3].g := 1;
          Col[3].b := 1;
        end;
    PerfectNote:
        begin
          Tex := Tex_Note_Perfect_Star.TexNum;
          W := 30;
          H := 30;
          SetLength(Col,1);
          Col[0].r := 1;
          Col[0].g := 1;
          Col[0].b := 0.95;
        end;
    NoteHitTwinkle:
        begin
          Tex := Tex_Note_Star.TexNum;
          Alpha := (Live/16);  // linear fade-out
          W := 15;
          H := 15;
          Setlength(Col,1);
          Col[0].r := 1;
          Col[0].g := 1;
          Col[0].b := RandomRange(10*Live,100)/90; //0.9;
        end;
    PerfectLineTwinkle:
        begin
          Tex := Tex_Note_Star.TexNum;
          W := RandomRange(10,20);
          H := W;
          SizeMod := (-cos((Frame+1)*5*2*pi/16)*0.5+1.1);
          SurviveSentenceChange:=True;
          // assign colours according to player given
          SetLength(Scale,3);
          Scale[1]:=0.3;
          Scale[2]:=0.2;
          SetLength(Col,3);
          case Player of
            0: LoadColor(Col[0].r,Col[0].g,Col[0].b,'P1Light');
            1: LoadColor(Col[0].r,Col[0].g,Col[0].b,'P2Light');
            2: LoadColor(Col[0].r,Col[0].g,Col[0].b,'P3Light');
            3: LoadColor(Col[0].r,Col[0].g,Col[0].b,'P4Light');
            4: LoadColor(Col[0].r,Col[0].g,Col[0].b,'P5Light');
            5: LoadColor(Col[0].r,Col[0].g,Col[0].b,'P6Light');
            else LoadColor(Col[0].r,Col[0].g,Col[0].b,'P1Light');
          end;
          Col[1].r := 1;
          Col[1].g := 1;
          Col[1].b := 0.4;
          Col[2].r:=Col[0].r+0.5;
          Col[2].g:=Col[0].g+0.5;
          Col[2].b:=Col[0].b+0.5;
          mX := RandomRange(-5,5);
          mY := RandomRange(-5,5);
        end;
    ColoredStar:
        begin
          Tex := Tex_Note_Star.TexNum;
          W := RandomRange(10,20);
          H := W;
          SizeMod := (-cos((Frame+1)*5*2*pi/16)*0.5+1.1);
          SurviveSentenceChange:=True;
          // assign colours according to player given
          SetLength(Scale,1);
          SetLength(Col,1);
          Col[0].b := (Player and $ff)/255;
          Col[0].g := ((Player shr 8) and $ff)/255;
          Col[0].r := ((Player shr 16) and $ff)/255;
          mX := 0;
          mY := 0;
        end;
    Flare:
        begin
          Tex := Tex_Note_Star.TexNum;
          W := 7;
          H := 7;
          SizeMod := (-cos((Frame+1)*5*2*pi/16)*0.5+1.1);
          mX := RandomRange(-5,5);
          mY := RandomRange(-5,5);
          SetLength(Scale,4);
          Scale[1]:=0.8;
          Scale[2]:=0.4;
          Scale[3]:=0.3;
          SetLength(Col,4);
          Col[0].r := 1;
          Col[0].g := 0.7;
          Col[0].b := 0.1;

          Col[1].r := 1;
          Col[1].g := 1;
          Col[1].b := 0.4;

          Col[2].r := 1;
          Col[2].g := 1;
          Col[2].b := 1;

          Col[3].r := 1;
          Col[3].g := 1;
          Col[3].b := 1;

        end;
    else    // just some random default values
        begin
          Tex := Tex_Note_Star.TexNum;
          Alpha := 1;
          W := 20;
          H := 20;
          SetLength(Col,1);
          Col[0].r := 1;
          Col[0].g := 1;
          Col[0].b := 1;
        end;
  end;
end;

Destructor TParticle.Destroy();
begin
  SetLength(Scale,0);
  SetLength(Col,0);
  inherited;
end;

procedure TParticle.LiveOn;
begin
  //Live = 0 => Live forever  <blindy> ?? die werden doch aber im Manager bei Draw getötet, wenns 0 is
  if (Live > 0) then
    Dec(Live);

  // animate frames
  Frame := ( Frame + 1 ) mod 16;

  // make our particles do funny stuff (besides being animated)
  // changes of any particle-values throughout its life are done here
  case StarType of
    GoldenNote:
        begin
          Alpha := (-cos((Frame+1)*2*pi/16)+1); // neat fade-in-and-out
        end;
    PerfectNote:
        begin
          Alpha := (-cos((Frame+1)*2*pi/16)+1); // neat fade-in-and-out
        end;
    NoteHitTwinkle:
        begin
          Alpha := (Live/10);  // linear fade-out
        end;
    PerfectLineTwinkle:
        begin
          Alpha := (-cos((Frame+1)*2*pi/16)+1); // neat fade-in-and-out
          SizeMod := (-cos((Frame+1)*5*2*pi/16)*0.5+1.1);
          // move around
          X := X + mX;
          Y := Y + mY;
        end;
    ColoredStar:
        begin
          Alpha := (-cos((Frame+1)*2*pi/16)+1); // neat fade-in-and-out
        end;
    Flare:
        begin
          Alpha := (-cos((Frame+1)/16*1.7*pi+0.3*pi)+1); // neat fade-in-and-out
          SizeMod := (-cos((Frame+1)*5*2*pi/16)*0.5+1.1);
          // move around
          X := X + mX;
          Y := Y + mY;
          mY:=mY+1.8;
//          mX:=mX/2;
        end;
  end;
end;

procedure TParticle.Draw;
var L: Cardinal;
begin
  if ScreenAct = Screen then
    // this draws (multiple) texture(s) of our particle
    for L:=0 to High(Col) do
    begin
      glColor4f(Col[L].r, Col[L].g, Col[L].b, Alpha);

      glBindTexture(GL_TEXTURE_2D, Tex);
      glEnable(GL_TEXTURE_2D);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);

      begin
        glBegin(GL_QUADS);
        glTexCoord2f((1/16) * Frame, 0);          glVertex2f(X-W*Scale[L]*SizeMod, Y-H*Scale[L]*SizeMod);
        glTexCoord2f((1/16) * Frame + (1/16), 0); glVertex2f(X-W*Scale[L]*SizeMod, Y+H*Scale[L]*SizeMod);
        glTexCoord2f((1/16) * Frame + (1/16), 1); glVertex2f(X+W*Scale[L]*SizeMod, Y+H*Scale[L]*SizeMod);
        glTexCoord2f((1/16) * Frame, 1);          glVertex2f(X+W*Scale[L]*SizeMod, Y-H*Scale[L]*SizeMod);
        glEnd;
      end;
    end;
  glcolor4f(1,1,1,1);
end;
// end of TParticle

// TEffectManager

constructor TEffectManager.Create;
var c: Cardinal;
begin
  inherited;
  LastTime := GetTickCount;
  for c:=0 to 5 do
  begin
    TwinkleArray[c] := 0;
  end;
end;

destructor TEffectManager.Destroy;
begin
  Killall;
  inherited;
end;


procedure TEffectManager.Draw;
var
  I: Integer;
  CurrentTime: Cardinal;
//const
//  DelayBetweenFrames : Cardinal = 100;
begin

  CurrentTime := GetTickCount;
  //Manage particle life
  if (CurrentTime - LastTime) > DelayBetweenFrames then
    begin
      LastTime := CurrentTime;
      for I := 0 to high(Particle) do
        Particle[I].LiveOn;
    end;

  I := 0;
  //Kill dead particles
  while (I <= High(Particle)) do
    begin
      if (Particle[I].Live <= 0) then
        begin
          kill(I);
        end
      else
        begin
          inc(I);
        end;
    end;

 //Draw
 for I := 0 to high(Particle) do
   begin
     Particle[I].Draw;
   end;
end;

// this method creates just one particle
function TEffectManager.Spawn(X, Y: Real; Screen: Integer; Live: Byte; StartFrame : Integer; RecArrayIndex : Integer; StarType : TParticleType; Player: Cardinal): Cardinal;
begin
  Result := Length(Particle);
  SetLength(Particle, (Result + 1));
  Particle[Result] := TParticle.Create(X, Y, Screen, Live, StartFrame, RecArrayIndex, StarType, Player);
end;

// manage Sparkling of GoldenNote Bars
procedure TEffectManager.SpawnRec();
Var
  Xkatze, Ykatze    : Real;
  RandomFrame : Integer;
  P : Integer; // P as seen on TV as Positionman
begin
//Spawn a random amount of stars within the given coordinates
//RandomRange(0,14) <- this one starts at a random  frame, 16 is our last frame - would be senseless to start a particle with 16, cause it would be dead at the next frame
for P:= 0 to high(RecArray) do
  begin
    while (RecArray[P].TotalStarCount > RecArray[P].CurrentStarCount) do
      begin
        Xkatze := RandomRange(Ceil(RecArray[P].xTop), Ceil(RecArray[P].xBottom));
        Ykatze := RandomRange(Ceil(RecArray[P].yTop), Ceil(RecArray[P].yBottom));
        RandomFrame := RandomRange(0,14);
        // Spawn a GoldenNote Particle
        Spawn(Xkatze, Ykatze, RecArray[P].Screen, 16 - RandomFrame, RandomFrame, P, GoldenNote, 0);
        inc(RecArray[P].CurrentStarCount);
      end;
    end;
  draw;
end;

// kill one particle (with given index in our particle array)
procedure TEffectManager.Kill(Index: Cardinal);
var
  LastParticleIndex : Integer;
begin
// delete particle indexed by Index,
// overwrite it's place in our particle-array with the particle stored at the last array index,
// shorten array
  LastParticleIndex := high(Particle);
  if not(LastParticleIndex = -1) then  // is there still a particle to delete?
  begin
    if not(Particle[Index].RecIndex = -1) then  // if it is a GoldenNote particle...
      dec(RecArray[Particle[Index].RecIndex].CurrentStarCount); // take care of its associated GoldenRec
    // now get rid of that particle
    Particle[Index].Destroy;
    Particle[Index] := Particle[LastParticleIndex];
    SetLength(Particle, LastParticleIndex);
  end;
end;

// clean up all particles and management structures
procedure TEffectManager.KillAll();
var c: Cardinal;
begin
//It's the kill all kennies rotuine
  while Length(Particle) > 0 do  // kill all existing particles
    Kill(0);
  SetLength(RecArray,0);  // remove GoldenRec positions
  SetLength(PerfNoteArray,0); // remove PerfectNote positions
  for c:=0 to 5 do
  begin
    TwinkleArray[c] := 0; // reset GoldenNoteHit memory
  end;
end;

procedure TEffectManager.SentenceChange();
var c: Cardinal;
begin
  c:=0;
  while c <= High(Particle) do
  begin
    if Particle[c].SurviveSentenceChange then
      inc(c)
    else
      Kill(c);
  end;
  SetLength(RecArray,0);  // remove GoldenRec positions
  SetLength(PerfNoteArray,0); // remove PerfectNote positions
  for c:=0 to 5 do
  begin
    TwinkleArray[c] := 0; // reset GoldenNoteHit memory
  end;
end;

procedure TeffectManager.GoldenNoteTwinkle(Top,Bottom,Right: Real; Player: Integer);
//Twinkle stars while golden note hit
// this is called from UDraw.pas, SingDrawPlayerCzesc
var
  C, P, XKatze, YKatze, LKatze: Integer;
  H: Real;
begin
  // make sure we spawn only one time at one position
  if (TwinkleArray[Player] < Right) then
    For P := 0 to high(RecArray) do  // Are we inside a GoldenNoteRectangle?
    begin
      H := (Top+Bottom)/2; // helper...
      with RecArray[P] do
      if ((xBottom >= Right) and (xTop <= Right) and
          (yTop <= H) and (yBottom >= H))
          and (Screen = ScreenAct) then
      begin
        TwinkleArray[Player] := Right; // remember twinkle position for this player
        for C := 1 to 10 do
        begin
          Ykatze := RandomRange(ceil(Top) , ceil(Bottom));
          XKatze := RandomRange(-7,3);
          LKatze := RandomRange(7,13);
          Spawn(Ceil(Right)+XKatze, YKatze, ScreenAct, LKatze, 0, -1, NoteHitTwinkle, 0);
        end;
        for C := 1 to 3 do
        begin
          Ykatze := RandomRange(ceil(Top)-6 , ceil(Top));
          XKatze := RandomRange(-5,1);
          LKatze := RandomRange(4,7);
          Spawn(Ceil(Right)+XKatze, YKatze, ScreenAct, LKatze, 0, -1, NoteHitTwinkle, 0);
        end;
        for C := 1 to 3 do
        begin
          Ykatze := RandomRange(ceil(Bottom), ceil(Bottom)+6);
          XKatze := RandomRange(-5,1);
          LKatze := RandomRange(4,7);
          Spawn(Ceil(Right)+XKatze, YKatze, ScreenAct, LKatze, 0, -1, NoteHitTwinkle, 0);
        end;
        for C := 1 to 3 do
        begin
          Ykatze := RandomRange(ceil(Top)-10 , ceil(Top)-6);
          XKatze := RandomRange(-5,1);
          LKatze := RandomRange(1,4);
          Spawn(Ceil(Right)+XKatze, YKatze, ScreenAct, LKatze, 0, -1, NoteHitTwinkle, 0);
        end;
        for C := 1 to 3 do
        begin
          Ykatze := RandomRange(ceil(Bottom)+6 , ceil(Bottom)+10);
          XKatze := RandomRange(-5,1);
          LKatze := RandomRange(1,4);
          Spawn(Ceil(Right)+XKatze, YKatze, ScreenAct, LKatze, 0, -1, NoteHitTwinkle, 0);
        end;

        exit; // found a matching GoldenRec, did spawning stuff... done
      end;
    end;
end;

procedure TEffectManager.SaveGoldenStarsRec(Xtop, Ytop, Xbottom, Ybottom: Real);
var
  P : Integer;   // P like used in Positions
  NewIndex : Integer;
begin
  For P := 0 to high(RecArray) do  // Do we already have that "new" position?
    begin
      if (ceil(RecArray[P].xTop) = ceil(Xtop)) and
      (ceil(RecArray[P].yTop) = ceil(Ytop)) and
      (ScreenAct = RecArray[p].Screen) then
        exit; // it's already in the array, so we don't have to create a new one
    end;

  // we got a new position, add the new positions to our array
    NewIndex := Length(RecArray);
    SetLength(RecArray, NewIndex + 1);
    RecArray[NewIndex].xTop    := Xtop;
    RecArray[NewIndex].yTop    := Ytop;
    RecArray[NewIndex].xBottom := Xbottom;
    RecArray[NewIndex].yBottom := Ybottom;
    RecArray[NewIndex].TotalStarCount := ceil(Xbottom - Xtop) div 12 + 3;
    RecArray[NewIndex].CurrentStarCount := 0;
    RecArray[NewIndex].Screen := ScreenAct;
end;

procedure TEffectManager.SavePerfectNotePos(Xtop, Ytop: Real);
var
  P : Integer;   // P like used in Positions
  NewIndex : Integer;
  RandomFrame : Integer;
  Xkatze, Ykatze : Integer;
begin
  For P := 0 to high(PerfNoteArray) do  // Do we already have that "new" position?
    begin
      with PerfNoteArray[P] do
      if (ceil(xPos) = ceil(Xtop)) and (ceil(yPos) = ceil(Ytop)) and
         (Screen = ScreenAct) then
        exit; // it's already in the array, so we don't have to create a new one
    end; //for

  // we got a new position, add the new positions to our array
    NewIndex := Length(PerfNoteArray);
    SetLength(PerfNoteArray, NewIndex + 1);
    PerfNoteArray[NewIndex].xPos    := Xtop;
    PerfNoteArray[NewIndex].yPos    := Ytop;
    PerfNoteArray[NewIndex].Screen  := ScreenAct;

    for P:= 0 to 2 do
      begin
        Xkatze := RandomRange(ceil(Xtop) - 5 , ceil(Xtop) + 10);
        Ykatze := RandomRange(ceil(Ytop) - 5 , ceil(Ytop) + 10);
        RandomFrame := RandomRange(0,14);
        Spawn(Xkatze, Ykatze, ScreenAct, 16 - RandomFrame, RandomFrame, -1, PerfectNote, 0);
     end; //for

end;

procedure TEffectManager.SpawnPerfectLineTwinkle();
var
  P,I,Life: Cardinal;
  Left, Right, Top, Bottom: Cardinal;
  cScreen: Integer;
begin
// calculation of coordinates done with hardcoded values like in UDraw.pas
// might need to be adjusted if drawing of SingScreen is modified
// coordinates may still be a bit weird and need adjustment
  if Ini.SingWindow = 0 then begin
    Left := 130;
  end else begin
    Left := 30;
  end;
  Right := 770;
  // spawn effect for every player with a perfect line
  for P:=0 to PlayersPlay-1 do
    if Player[P].LastSentencePerfect then
    begin
      // calculate area where notes of this player are drawn
      case PlayersPlay of
        1: begin
             Bottom:=Skin_P2_NotesB+10;
             Top:=Bottom-105;
             cScreen:=1;
           end;
        2,4: begin
               case P of
                 0,2: begin
                        Bottom:=Skin_P1_NotesB+10;
                        Top:=Bottom-105;
                      end;
                 else begin
                        Bottom:=Skin_P2_NotesB+10;
                        Top:=Bottom-105;
                      end;
               end;
               case P of
                 0,1: cScreen:=1;
                 else cScreen:=2;
               end;
             end;
        3,6: begin
               case P of
                 0,3: begin
                        Top:=130;
                        Bottom:=Top+85;
                      end;
                 1,4: begin
                        Top:=255;
                        Bottom:=Top+85;
                      end;
                 2,5: begin
                        Top:=380;
                        Bottom:=Top+85;
                      end;
               end;
               case P of
                 0,1,2: cScreen:=1;
                 else cScreen:=2;
               end;
             end;
      end;
      // spawn Sparkling Stars inside calculated coordinates
      for I:= 0 to 80 do
      begin
        Life:=RandomRange(8,16);
        Spawn(RandomRange(Left,Right), RandomRange(Top,Bottom), cScreen, Life, 16-Life, -1, PerfectLineTwinkle, P);
      end;
    end;
end;

end.

