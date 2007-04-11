unit UGraphicClasses;

interface
const  DelayBetweenFrames : Cardinal = 100;
type

 TParticleType=(GoldenNote, PerfectNote, NoteHitTwinkle, PerfectLineTwinkle);

 TColour3f = Record
               r, g, b: Real;
             end;

 TParticle = Class
   X, Y     : Real;     //Position
   W, H     : Cardinal; //dimensions of particle
   Col      : TColour3f; // Colour of particle
   Frame    : Byte;     //act. Frame
   Tex      : Cardinal; //Tex num from Textur Manager
   Live     : Byte;     //How many Cycles before Kill
   RecIndex : Integer;  //To which rectangle this particle belongs (only GoldenNote)
   StarType : TParticleType;  // GoldenNote | PerfectNote | NoteHitTwinkle | PerfectLineTwinkle
   Alpha    : Real;     // used for fading...
   mX, mY   : Real;     // movement-vector for PerfectLineTwinkle

   Constructor Create(cX,cY: Real; cLive: Byte; cFrame : integer; cRecArrayIndex : Integer; cStarType : TParticleType; Player: Cardinal);
   procedure Draw;
   procedure LiveOn;
 end;

 RectanglePositions   = Record
   xTop, yTop, xBottom, yBottom : Real;
   TotalStarCount   : Integer;
   CurrentStarCount : Integer;
 end;

 PerfectNotePositions = Record
   xPos, yPos : Real;
 end;

 TEffectManager = Class
   Particle      : array of TParticle;
   LastTime      : Cardinal;
   RecArray      : Array of RectanglePositions;
   TwinkleArray  : Array[0..5] of Real; // store x-position of last twinkle for every player
   PerfNoteArray : Array of PerfectNotePositions;

   constructor Create;
   destructor  Destroy; override;
   procedure Draw;
   function  Spawn(X, Y: Real;
                   Live: Byte;
                   StartFrame: Integer;
                   RecArrayIndex: Integer;  // this is only used with GoldenNotes
                   StarType: TParticleType;
                   Player: Cardinal         // for PerfectLineTwinkle
             ): Cardinal;
   procedure SpawnRec();
   procedure Kill(index: Cardinal);
   procedure KillAll();
   procedure SaveGoldenStarsRec(Xtop, Ytop, Xbottom, Ybottom: Real);
   procedure SavePerfectNotePos(Xtop, Ytop: Real);
   procedure GoldenNoteTwinkle(Top,Bottom,Right: Real; Player: Integer);
 end;

var GoldenRec : TEffectManager;

implementation
uses sysutils, Windows,OpenGl12, UThemes, USkins, UGraphic, UDrawTexture, UTexture, math, dialogs;

//TParticle
Constructor TParticle.Create(cX,cY: Real; cLive: Byte; cFrame : integer; cRecArrayIndex : Integer; cStarType : TParticleType; Player: Cardinal);
begin
  inherited Create;
  X := cX;
  Y := cY;
  Live := cLive;
  Frame:= cFrame;
  RecIndex := cRecArrayIndex;
  StarType := cStarType;
  Alpha := (-cos((Frame+1)*2*pi/16)+1); // neat fade-in-and-out
  case cStarType of
    GoldenNote:
        begin
          Tex := Tex_Note_Star.TexNum;
          W := 20;
          H := 20;
          Col.r := 0.99;
          Col.g := 1;
          Col.b := 0.6;
        end;
    PerfectNote:
        begin
          Tex := Tex_Note_Perfect_Star.TexNum;
          W := 30;
          H := 30;
          Col.r := 1;
          Col.g := 1;
          Col.b := 0.95;
        end;
    NoteHitTwinkle:
        begin
          Tex := Tex_Note_Star.TexNum;
          Alpha := (Live/10);  // linear fade-out
          W := 15;
          H := 15;
          Col.r := 1;
          Col.g := 1;
          Col.b := RandomRange(10*Live,100)/80; //0.9; 
        end;
    PerfectLineTwinkle:
        begin
          Tex := Tex_Note_Star.TexNum;
          W := RandomRange(10,30);
          H := W;
          // hier muss entsprechend des players farbe gesetzt werden (sollten wir dann auch übergeben bekommen)
          // case Player of
          // ...
          Col.r := 1;
          Col.g := 0.5;
          Col.b := 0.5;
          mX := RandomRange(-5,5);
          mY := RandomRange(-5,5);
        end;
    else    // just some random default values
        begin
          Tex := Tex_Note_Star.TexNum;
          Alpha := 1;
          W := 20;
          H := 20;
          Col.r := 1;
          Col.g := 1;
          Col.b := 1;
        end;
  end;
end;

procedure TParticle.LiveOn;
begin
  //Live = 0 => Live forever  <blindy> ?? die werden doch aber im Manager bei Draw getötet, wenns 0 is
  if (Live > 0) then
    Dec(Live);

  // animate frames
  Frame := ( Frame + 1 ) mod 16;

  // make our particles do funny stuff (besides being animated)
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
          // move around
          X := X + mX;
          Y := Y + mY;
        end;
  end;
end;

procedure TParticle.Draw;
begin
  glColor4f(Col.r, Col.g, Col.b, Alpha);

  glBindTexture(GL_TEXTURE_2D, Tex);
  glEnable(GL_TEXTURE_2D);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  begin
    glBegin(GL_QUADS);
    glTexCoord2f((1/16) * Frame, 0);          glVertex2f(X-W, Y-H);
    glTexCoord2f((1/16) * Frame + (1/16), 0); glVertex2f(X-W, Y+H);
    glTexCoord2f((1/16) * Frame + (1/16), 1); glVertex2f(X+W, Y+H);
    glTexCoord2f((1/16) * Frame, 1);          glVertex2f(X+W, Y-H);
    glEnd;
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
function TEffectManager.Spawn(X, Y: Real; Live: Byte; StartFrame : Integer; RecArrayIndex : Integer; StarType : TParticleType; Player: Cardinal): Cardinal;
begin
  Result := Length(Particle);
  SetLength(Particle, (Result + 1));
  Particle[Result] := TParticle.Create(X, Y, Live, StartFrame, RecArrayIndex, StarType, Player);
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
        Spawn(Xkatze, Ykatze, 16 - RandomFrame, RandomFrame, P, GoldenNote, 0);
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
          (yTop <= H) and (yBottom >= H)) then
      begin
        TwinkleArray[Player] := Right; // remember twinkle position for this player
        for C := 1 to 20 do
        begin
          Ykatze := RandomRange(ceil(Top) , ceil(Bottom));
          XKatze := RandomRange(-7,3);
          LKatze := RandomRange(4,10);
          Spawn(Ceil(Right)+XKatze, YKatze, LKatze, 0, -1, NoteHitTwinkle, 0);
        end;
        for C := 1 to 5 do
        begin
          Ykatze := RandomRange(ceil(Top)-6 , ceil(Top));
          XKatze := RandomRange(-5,1);
          LKatze := RandomRange(2,3);
          Spawn(Ceil(Right)+XKatze, YKatze, LKatze, 0, -1, NoteHitTwinkle, 0);
        end;
        for C := 1 to 5 do
        begin
          Ykatze := RandomRange(ceil(Bottom), ceil(Bottom)+6);
          XKatze := RandomRange(-5,1);
          LKatze := RandomRange(2,3);
          Spawn(Ceil(Right)+XKatze, YKatze, LKatze, 0, -1, NoteHitTwinkle, 0);
        end;
        for C := 1 to 3 do
        begin
          Ykatze := RandomRange(ceil(Top)-10 , ceil(Top)-6);
          XKatze := RandomRange(-5,1);
          LKatze := RandomRange(1,2);
          Spawn(Ceil(Right)+XKatze, YKatze, LKatze, 0, -1, NoteHitTwinkle, 0);
        end;
        for C := 1 to 3 do
        begin
          Ykatze := RandomRange(ceil(Bottom)+6 , ceil(Bottom)+10);
          XKatze := RandomRange(-5,1);
          LKatze := RandomRange(1,2);
          Spawn(Ceil(Right)+XKatze, YKatze, LKatze, 0, -1, NoteHitTwinkle, 0);
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
      if ((ceil(RecArray[P].xTop) = ceil(Xtop)) and (ceil(RecArray[P].yTop) = ceil(Ytop))) then
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
      if ((ceil(xPos) = ceil(Xtop)) and (ceil(yPos) = ceil(Ytop))) then
        exit; // it's already in the array, so we don't have to create a new one
    end; //for

  // we got a new position, add the new positions to our array
    NewIndex := Length(PerfNoteArray);
    SetLength(PerfNoteArray, NewIndex + 1);
    PerfNoteArray[NewIndex].xPos    := Xtop;
    PerfNoteArray[NewIndex].yPos    := Ytop;

    for P:= 0 to 2 do
      begin
        Xkatze := RandomRange(ceil(Xtop) - 5 , ceil(Xtop) + 10);
        Ykatze := RandomRange(ceil(Ytop) - 5 , ceil(Ytop) + 10);
        RandomFrame := RandomRange(0,14);
        Spawn(Xkatze, Ykatze, 16 - RandomFrame, RandomFrame, -1, PerfectNote, 0);
     end; //for
end;

end.

