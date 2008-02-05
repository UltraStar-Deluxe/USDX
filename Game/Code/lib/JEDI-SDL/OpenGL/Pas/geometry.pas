unit geometry;
{
  $Id: geometry.pas,v 1.1 2004/03/30 21:53:54 savage Exp $
  
}

// This unit contains many needed types, functions and procedures for
// quaternion, vector and matrix arithmetics. It is specifically designed
// for geometric calculations within R3 (affine vector space)
// and R4 (homogeneous vector space).
//
// Note: The terms 'affine' or 'affine coordinates' are not really correct here
//       because an 'affine transformation' describes generally a transformation which leads
//       to a uniquely solvable system of equations and has nothing to do with the dimensionality
//       of a vector. One could use 'projective coordinates' but this is also not really correct
//       and since I haven't found a better name (or even any correct one), 'affine' is as good
//       as any other one.
//
// Identifiers containing no dimensionality (like affine or homogeneous)
// and no datatype (integer..extended) are supposed as R4 representation
// with 'single' floating point type (examples are TVector, TMatrix,
// and TQuaternion). The default data type is 'single' ('GLFloat' for OpenGL)
// and used in all routines (except conversions and trigonometric functions).
//
// Routines with an open array as argument can either take Func([1,2,3,4,..]) or Func(Vect).
// The latter is prefered, since no extra stack operations is required.
// Note: Be careful while passing open array elements! If you pass more elements
//       than there's room in the result the behaviour will be unpredictable.
//
// If not otherwise stated, all angles are given in radians
// (instead of degrees). Use RadToDeg or DegToRad to convert between them.
//
// Geometry.pas was assembled from different sources (like GraphicGems)
// and relevant books or based on self written code, respectivly.
//
// Note: Some aspects need to be considered when using Delphi and pure
//       assembler code. Delphi ensures that the direction flag is always
//       cleared while entering a function and expects it cleared on return.
//       This is in particular important in routines with (CPU) string commands (MOVSD etc.)
//       The registers EDI, ESI and EBX (as well as the stack management
//       registers EBP and ESP) must not be changed! EAX, ECX and EDX are
//       freely available and mostly used for parameter.
//
// Version 2.5
// last change : 04. January 2000
//
// (c) Copyright 1999, Dipl. Ing. Mike Lischke (public@lischke-online.de)
{
  $Log: geometry.pas,v $
  Revision 1.1  2004/03/30 21:53:54  savage
  Moved to it's own folder.

  Revision 1.1  2004/02/05 00:08:19  savage
  Module 1.0 release

  
}

interface

{$I jedi-sdl.inc}

type
  // data types needed for 3D graphics calculation,
  // included are 'C like' aliases for each type (to be
  // conformal with OpenGL types)

  PByte = ^Byte;
  PWord = ^Word;
  PInteger = ^Integer;
  PFloat = ^Single;
  PDouble = ^Double;
  PExtended = ^Extended;
  PPointer = ^Pointer;

  // types to specify continous streams of a specific type
  // switch off range checking to access values beyond the limits
  PByteVector = ^TByteVector;
  PByteArray = PByteVector;
  TByteVector = array[0..0] of Byte;

  PWordVector = ^TWordVector;
  PWordArray = PWordVector;  // note: there's a same named type in SysUtils
  TWordVector = array[0..0] of Word;

  PIntegerVector = ^TIntegerVector;
  PIntegerArray = PIntegerVector;
  TIntegerVector = array[0..0] of Integer;

  PFloatVector = ^TFloatVector;
  PFloatArray = PFloatVector;
  TFloatVector = array[0..0] of Single;

  PDoubleVector = ^TDoubleVector;
  PDoubleArray = PDoubleVector;
  TDoubleVector = array[0..0] of Double;

  PExtendedVector = ^TExtendedVector;
  PExtendedArray = PExtendedVector;
  TExtendedVector = array[0..0] of Extended;

  PPointerVector = ^TPointerVector;
  PPointerArray = PPointerVector;
  TPointerVector = array[0..0] of Pointer;

  PCardinalVector = ^TCardinalVector;
  PCardinalArray = PCardinalVector;
  TCardinalVector = array[0..0] of Cardinal;

  // common vector and matrix types with predefined limits
  // indices correspond like: x -> 0
  //                          y -> 1
  //                          z -> 2
  //                          w -> 3

  PHomogeneousByteVector = ^THomogeneousByteVector;
  THomogeneousByteVector = array[0..3] of Byte;
  TVector4b = THomogeneousByteVector;

  PHomogeneousWordVector = ^THomogeneousWordVector;
  THomogeneousWordVector = array[0..3] of Word;
  TVector4w = THomogeneousWordVector;

  PHomogeneousIntVector = ^THomogeneousIntVector;
  THomogeneousIntVector = array[0..3] of Integer;
  TVector4i = THomogeneousIntVector;

  PHomogeneousFltVector = ^THomogeneousFltVector;
  THomogeneousFltVector = array[0..3] of Single;
  TVector4f = THomogeneousFltVector;

  PHomogeneousDblVector = ^THomogeneousDblVector;
  THomogeneousDblVector = array[0..3] of Double;
  TVector4d = THomogeneousDblVector;

  PHomogeneousExtVector = ^THomogeneousExtVector;
  THomogeneousExtVector = array[0..3] of Extended;
  TVector4e = THomogeneousExtVector;

  PHomogeneousPtrVector = ^THomogeneousPtrVector;
  THomogeneousPtrVector = array[0..3] of Pointer;
  TVector4p = THomogeneousPtrVector;

  PAffineByteVector = ^TAffineByteVector;
  TAffineByteVector = array[0..2] of Byte;
  TVector3b = TAffineByteVector;

  PAffineWordVector = ^TAffineWordVector;
  TAffineWordVector = array[0..2] of Word;
  TVector3w = TAffineWordVector;

  PAffineIntVector = ^TAffineIntVector;
  TAffineIntVector = array[0..2] of Integer;
  TVector3i = TAffineIntVector;

  PAffineFltVector = ^TAffineFltVector;
  TAffineFltVector = array[0..2] of Single;
  TVector3f = TAffineFltVector;

  PAffineDblVector = ^TAffineDblVector;
  TAffineDblVector = array[0..2] of Double;
  TVector3d = TAffineDblVector;

  PAffineExtVector = ^TAffineExtVector;
  TAffineExtVector = array[0..2] of Extended;
  TVector3e = TAffineExtVector;

  PAffinePtrVector = ^TAffinePtrVector;
  TAffinePtrVector = array[0..2] of Pointer;
  TVector3p = TAffinePtrVector;

  // some simplified names
  PVector = ^TVector;
  TVector = THomogeneousFltVector;

  PHomogeneousVector = ^THomogeneousVector;
  THomogeneousVector = THomogeneousFltVector;

  PAffineVector = ^TAffineVector;
  TAffineVector = TAffineFltVector;

  // arrays of vectors
  PVectorArray = ^TVectorArray;
  TVectorArray = array[0..0] of TAffineVector;

  // matrices
  THomogeneousByteMatrix = array[0..3] of THomogeneousByteVector;
  TMatrix4b = THomogeneousByteMatrix;

  THomogeneousWordMatrix = array[0..3] of THomogeneousWordVector;
  TMatrix4w = THomogeneousWordMatrix;

  THomogeneousIntMatrix = array[0..3] of THomogeneousIntVector;
  TMatrix4i = THomogeneousIntMatrix;

  THomogeneousFltMatrix  = array[0..3] of THomogeneousFltVector;
  TMatrix4f = THomogeneousFltMatrix;

  THomogeneousDblMatrix = array[0..3] of THomogeneousDblVector;
  TMatrix4d = THomogeneousDblMatrix;

  THomogeneousExtMatrix = array[0..3] of THomogeneousExtVector;
  TMatrix4e = THomogeneousExtMatrix;

  TAffineByteMatrix = array[0..2] of TAffineByteVector;
  TMatrix3b = TAffineByteMatrix;

  TAffineWordMatrix = array[0..2] of TAffineWordVector;
  TMatrix3w = TAffineWordMatrix;

  TAffineIntMatrix = array[0..2] of TAffineIntVector;
  TMatrix3i = TAffineIntMatrix;

  TAffineFltMatrix = array[0..2] of TAffineFltVector;
  TMatrix3f = TAffineFltMatrix;

  TAffineDblMatrix = array[0..2] of TAffineDblVector;
  TMatrix3d = TAffineDblMatrix;

  TAffineExtMatrix = array[0..2] of TAffineExtVector;
  TMatrix3e = TAffineExtMatrix;

  // some simplified names
  PMatrix = ^TMatrix;
  TMatrix = THomogeneousFltMatrix;

  PHomogeneousMatrix = ^THomogeneousMatrix;
  THomogeneousMatrix = THomogeneousFltMatrix;

  PAffineMatrix = ^TAffineMatrix;
  TAffineMatrix = TAffineFltMatrix;

  // q = ([x, y, z], w)
  TQuaternion = record
    case Integer of
      0:
        (ImagPart: TAffineVector;
         RealPart: Single);
      1:
        (Vector: TVector4f);
  end;

  TRectangle = record
    Left,
    Top,
    Width,
    Height: Integer;
  end;

  TTransType = (ttScaleX, ttScaleY, ttScaleZ,
                ttShearXY, ttShearXZ, ttShearYZ,
                ttRotateX, ttRotateY, ttRotateZ,
                ttTranslateX, ttTranslateY, ttTranslateZ,
                ttPerspectiveX, ttPerspectiveY, ttPerspectiveZ, ttPerspectiveW);

  // used to describe a sequence of transformations in following order:
  // [Sx][Sy][Sz][ShearXY][ShearXZ][ShearZY][Rx][Ry][Rz][Tx][Ty][Tz][P(x,y,z,w)]
  // constants are declared for easier access (see MatrixDecompose below)
  TTransformations  = array[TTransType] of Single;


const
  // useful constants

  // standard vectors
  XVector: TAffineVector = (1, 0, 0);
  YVector: TAffineVector = (0, 1, 0);
  ZVector: TAffineVector = (0, 0, 1);
  NullVector: TAffineVector = (0, 0, 0);

  IdentityMatrix: TMatrix = ((1, 0, 0, 0),
                             (0, 1, 0, 0),
                             (0, 0, 1, 0),
                             (0, 0, 0, 1));
  EmptyMatrix: TMatrix = ((0, 0, 0, 0),
                          (0, 0, 0, 0),
                          (0, 0, 0, 0),
                          (0, 0, 0, 0));
  // some very small numbers
  EPSILON  = 1e-100;
  EPSILON2 = 1e-50;

//----------------------------------------------------------------------------------------------------------------------

// vector functions
function  VectorAdd(V1, V2: TVector): TVector;
function  VectorAffineAdd(V1, V2: TAffineVector): TAffineVector;
function  VectorAffineCombine(V1, V2: TAffineVector; F1, F2: Single): TAffineVector;
function  VectorAffineDotProduct(V1, V2: TAffineVector): Single;
function  VectorAffineLerp(V1, V2: TAffineVector; t: Single): TAffineVector;
function  VectorAffineSubtract(V1, V2: TAffineVector): TAffineVector;
function  VectorAngle(V1, V2: TAffineVector): Single;
function  VectorCombine(V1, V2: TVector; F1, F2: Single): TVector;
function  VectorCrossProduct(V1, V2: TAffineVector): TAffineVector;
function  VectorDotProduct(V1, V2: TVector): Single;
function  VectorLength(V: array of Single): Single;
function  VectorLerp(V1, V2: TVector; t: Single): TVector;
procedure VectorNegate(V: array of Single);
function  VectorNorm(V: array of Single): Single;
function  VectorNormalize(V: array of Single): Single;
function  VectorPerpendicular(V, N: TAffineVector): TAffineVector;
function  VectorReflect(V, N: TAffineVector): TAffineVector;
procedure VectorRotate(var Vector: TVector4f; Axis: TVector3f; Angle: Single);
procedure VectorScale(V: array of Single; Factor: Single);
function  VectorSubtract(V1, V2: TVector): TVector;

// matrix functions
function  CreateRotationMatrixX(Sine, Cosine: Single): TMatrix;
function  CreateRotationMatrixY(Sine, Cosine: Single): TMatrix;
function  CreateRotationMatrixZ(Sine, Cosine: Single): TMatrix;
function  CreateScaleMatrix(V: TAffineVector): TMatrix;
function  CreateTranslationMatrix(V: TVector): TMatrix;
procedure MatrixAdjoint(var M: TMatrix);
function  MatrixAffineDeterminant(M: TAffineMatrix): Single;
procedure MatrixAffineTranspose(var M: TAffineMatrix);
function  MatrixDeterminant(M: TMatrix): Single;
procedure MatrixInvert(var M: TMatrix);
function  MatrixMultiply(M1, M2: TMatrix): TMatrix;
procedure MatrixScale(var M: TMatrix; Factor: Single);
procedure MatrixTranspose(var M: TMatrix);

// quaternion functions
function  QuaternionConjugate(Q: TQuaternion): TQuaternion;
function  QuaternionFromPoints(V1, V2: TAffineVector): TQuaternion;
function  QuaternionMultiply(qL, qR: TQuaternion): TQuaternion;
function  QuaternionSlerp(QStart, QEnd: TQuaternion; Spin: Integer; t: Single): TQuaternion;
function  QuaternionToMatrix(Q: TQuaternion): TMatrix;
procedure QuaternionToPoints(Q: TQuaternion; var ArcFrom, ArcTo: TAffineVector);

// mixed functions
function  ConvertRotation(Angles: TAffineVector): TVector;
function  CreateRotationMatrix(Axis: TVector3f; Angle: Single): TMatrix;
function  MatrixDecompose(M: TMatrix; var Tran: TTransformations): Boolean;
function  VectorAffineTransform(V: TAffineVector; M: TAffineMatrix): TAffineVector;
function  VectorTransform(V: TVector4f; M: TMatrix): TVector4f; overload;
function  VectorTransform(V: TVector3f; M: TMatrix): TVector3f; overload;

// miscellaneous functions
function  MakeAffineDblVector(V: array of Double): TAffineDblVector;
function  MakeDblVector(V: array of Double): THomogeneousDblVector;
function  MakeAffineVector(V: array of Single): TAffineVector;
function  MakeQuaternion(Imag: array of Single; Real: Single): TQuaternion;
function  MakeVector(V: array of Single): TVector;
function  PointInPolygon(xp, yp : array of Single; x, y: Single): Boolean;
function  VectorAffineDblToFlt(V: TAffineDblVector): TAffineVector;
function  VectorDblToFlt(V: THomogeneousDblVector): THomogeneousVector;
function  VectorAffineFltToDbl(V: TAffineVector): TAffineDblVector;
function  VectorFltToDbl(V: TVector): THomogeneousDblVector;

// trigonometric functions
function  ArcCos(X: Extended): Extended;
function  ArcSin(X: Extended): Extended;
function  ArcTan2(Y, X: Extended): Extended;
function  CoTan(X: Extended): Extended;
function  DegToRad(Degrees: Extended): Extended;
function  RadToDeg(Radians: Extended): Extended;
procedure SinCos(Theta: Extended; var Sin, Cos: Extended);
function  Tan(X: Extended): Extended;

// coordinate system manipulation functions
function Turn(Matrix: TMatrix; Angle: Single): TMatrix; overload;
function Turn(Matrix: TMatrix; MasterUp: TAffineVector; Angle: Single): TMatrix; overload;
function Pitch(Matrix: TMatrix; Angle: Single): TMatrix; overload;
function Pitch(Matrix: TMatrix; MasterRight: TAffineVector; Angle: Single): TMatrix; overload;
function Roll(Matrix: TMatrix; Angle: Single): TMatrix; overload;
function Roll(Matrix: TMatrix; MasterDirection: TAffineVector; Angle: Single): TMatrix; overload;

//----------------------------------------------------------------------------------------------------------------------

implementation

const
  // FPU status flags (high order byte)
  C0 = 1;
  C1 = 2;
  C2 = 4;
  C3 = $40;

  // to be used as descriptive indices
  X = 0;
  Y = 1;
  Z = 2;
  W = 3;

//----------------- trigonometric helper functions ---------------------------------------------------------------------

function DegToRad(Degrees: Extended): Extended;

begin
  Result := Degrees * (PI / 180);
end;

//----------------------------------------------------------------------------------------------------------------------

function RadToDeg(Radians: Extended): Extended;

begin
  Result := Radians * (180 / PI);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure SinCos(Theta: Extended; var Sin, Cos: Extended); assembler; register;

// calculates sine and cosine from the given angle Theta
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack

asm
              FLD  Theta
              FSINCOS
              FSTP TBYTE PTR [EDX]    // cosine
              FSTP TBYTE PTR [EAX]    // sine
              FWAIT
end;

//----------------------------------------------------------------------------------------------------------------------

function ArcCos(X: Extended): Extended;

begin
  Result := ArcTan2(Sqrt(1 - X * X), X);
end;

//----------------------------------------------------------------------------------------------------------------------

function ArcSin(X: Extended): Extended;

begin
  Result := ArcTan2(X, Sqrt(1 - X * X))
end;

//----------------------------------------------------------------------------------------------------------------------

function ArcTan2(Y, X: Extended): Extended;

asm
              FLD  Y
              FLD  X
              FPATAN
              FWAIT
end;

//----------------------------------------------------------------------------------------------------------------------

function Tan(X: Extended): Extended;

asm
              FLD  X
              FPTAN
              FSTP ST(0)      // FPTAN pushes 1.0 after result
              FWAIT
end;

//----------------------------------------------------------------------------------------------------------------------

function CoTan(X: Extended): Extended;

asm
              FLD  X
              FPTAN
              FDIVRP
              FWAIT
end;

//----------------- miscellaneous vector functions ---------------------------------------------------------------------

function MakeAffineDblVector(V: array of Double): TAffineDblVector; assembler;

// creates a vector from given values
// EAX contains address of V
// ECX contains address to result vector
// EDX contains highest index of V

asm
              PUSH EDI
              PUSH ESI
              MOV EDI, ECX
              MOV ESI, EAX
              MOV ECX, EDX
              ADD ECX, 2
              REP MOVSD
              POP ESI
              POP EDI
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeDblVector(V: array of Double): THomogeneousDblVector; assembler;

// creates a vector from given values
// EAX contains address of V
// ECX contains address to result vector
// EDX contains highest index of V

asm
              PUSH EDI
              PUSH ESI
              MOV EDI, ECX
              MOV ESI, EAX
              MOV ECX, EDX
              ADD ECX, 2
              REP MOVSD
              POP ESI
              POP EDI
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeAffineVector(V: array of Single): TAffineVector; assembler;

// creates a vector from given values
// EAX contains address of V
// ECX contains address to result vector
// EDX contains highest index of V

asm
              PUSH EDI
              PUSH ESI
              MOV EDI, ECX
              MOV ESI, EAX
              MOV ECX, EDX
              INC ECX
              CMP ECX, 3
              JB  @@1
              MOV ECX, 3
@@1:          REP MOVSD                     // copy given values
              MOV ECX, 2
              SUB ECX, EDX                   // determine missing entries
              JS  @@Finish
              XOR EAX, EAX
              REP STOSD                     // set remaining fields to 0
@@Finish:     POP ESI
              POP EDI
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeQuaternion(Imag: array of Single; Real: Single): TQuaternion; assembler;

// creates a quaternion from the given values
// EAX contains address of Imag
// ECX contains address to result vector
// EDX contains highest index of Imag
// Real part is passed on the stack

asm
              PUSH EDI
              PUSH ESI
              MOV EDI, ECX
              MOV ESI, EAX
              MOV ECX, EDX
              INC ECX
              REP MOVSD
              MOV EAX, [Real]
              MOV [EDI], EAX
              POP ESI
              POP EDI
end;

//----------------------------------------------------------------------------------------------------------------------

function MakeVector(V: array of Single): TVector; assembler;

// creates a vector from given values
// EAX contains address of V
// ECX contains address to result vector
// EDX contains highest index of V

asm
              PUSH EDI
              PUSH ESI
              MOV EDI, ECX
              MOV ESI, EAX
              MOV ECX, EDX
              INC ECX
              CMP ECX, 4
              JB  @@1
              MOV ECX, 4
@@1:          REP MOVSD                     // copy given values
              MOV ECX, 3
              SUB ECX, EDX                   // determine missing entries
              JS  @@Finish
              XOR EAX, EAX
              REP STOSD                     // set remaining fields to 0
@@Finish:     POP ESI
              POP EDI
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorLength(V: array of Single): Single; assembler;

// calculates the length of a vector following the equation: sqrt(x * x + y * y + ...)
// Note: The parameter of this function is declared as open array. Thus
// there's no restriction about the number of the components of the vector.
//
// EAX contains address of V
// EDX contains the highest index of V
// the result is returned in ST(0)

asm
              FLDZ                           // initialize sum
@@Loop:       FLD  DWORD PTR [EAX  +  4 * EDX] // load a component
              FMUL ST, ST
              FADDP
              SUB  EDX, 1
              JNL  @@Loop
              FSQRT
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorAngle(V1, V2: TAffineVector): Single; assembler;

// calculates the cosine of the angle between Vector1 and Vector2
// Result = DotProduct(V1, V2) / (Length(V1) * Length(V2))
//
// EAX contains address of Vector1
// EDX contains address of Vector2

asm
              FLD DWORD PTR [EAX]           // V1[0]
              FLD ST                        // double V1[0]
              FMUL ST, ST                   // V1[0]^2 (prep. for divisor)
              FLD DWORD PTR [EDX]           // V2[0]
              FMUL ST(2), ST                // ST(2) := V1[0] * V2[0]
              FMUL ST, ST                   // V2[0]^2 (prep. for divisor)
              FLD DWORD PTR [EAX + 4]       // V1[1]
              FLD ST                        // double V1[1]
              FMUL ST, ST                   // ST(0) := V1[1]^2
              FADDP ST(3), ST               // ST(2) := V1[0]^2 + V1[1] *  * 2
              FLD DWORD PTR [EDX + 4]       // V2[1]
              FMUL ST(1), ST                // ST(1) := V1[1] * V2[1]
              FMUL ST, ST                   // ST(0) := V2[1]^2
              FADDP ST(2), ST               // ST(1) := V2[0]^2 + V2[1]^2
              FADDP ST(3), ST               // ST(2) := V1[0] * V2[0] + V1[1] * V2[1]
              FLD DWORD PTR [EAX + 8]       // load V2[1]
              FLD ST                        // same calcs go here
              FMUL ST, ST                   // (compare above)
              FADDP ST(3), ST
              FLD DWORD PTR [EDX + 8]
              FMUL ST(1), ST
              FMUL ST, ST
              FADDP ST(2), ST
              FADDP ST(3), ST
              FMULP                         // ST(0) := (V1[0]^2 + V1[1]^2 + V1[2]) *
                                            //          (V2[0]^2 + V2[1]^2 + V2[2])
              FSQRT                         // sqrt(ST(0))
              FDIVP                         // ST(0) := Result := ST(1) / ST(0)
  // the result is expected in ST(0), if it's invalid, an error is raised
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorNorm(V: array of Single): Single; assembler; register;

// calculates norm of a vector which is defined as norm = x * x + y * y + ...
// EAX contains address of V
// EDX contains highest index in V
// result is passed in ST(0)

asm
              FLDZ                           // initialize sum
@@Loop:       FLD  DWORD PTR [EAX + 4 * EDX] // load a component
              FMUL ST, ST                    // make square
              FADDP                          // add previous calculated sum
              SUB  EDX, 1
              JNL  @@Loop
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorNormalize(V: array of Single): Single; assembler; register;

// transforms a vector to unit length and return length
// EAX contains address of V
// EDX contains the highest index in V
// return former length of V in ST

asm
              PUSH EBX
              MOV ECX, EDX                  // save size of V
              CALL VectorLength             // calculate length of vector
              FTST                          // test if length = 0
              MOV EBX, EAX                  // save parameter address
              FSTSW AX                      // get test result
              TEST AH, C3                   // check the test result
              JNZ @@Finish
              SUB EBX, 4                    // simplyfied address calculation
              INC ECX
              FLD1                          // calculate reciprocal of length
              FDIV ST, ST(1)
@@1:          FLD ST                        // double reciprocal
              FMUL DWORD PTR [EBX + 4 * ECX] // scale component
              WAIT
              FSTP DWORD PTR [EBX + 4 * ECX] // store result
              LOOP @@1
              FSTP ST                       // remove reciprocal from FPU stack
@@Finish:     POP EBX
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorAffineSubtract(V1, V2: TAffineVector): TAffineVector; assembler; register;

// returns v1 minus v2
// EAX contains address of V1
// EDX contains address of V2
// ECX contains address of the result

asm
  {Result[X] := V1[X]-V2[X];
  Result[Y] := V1[Y]-V2[Y];
  Result[Z] := V1[Z]-V2[Z];}

                   FLD DWORD PTR [EAX]
                   FSUB DWORD PTR [EDX]
                   FSTP DWORD PTR [ECX]
                   FLD DWORD PTR [EAX + 4]
                   FSUB DWORD PTR [EDX + 4]
                   FSTP DWORD PTR [ECX + 4]
                   FLD DWORD PTR [EAX + 8]
                   FSUB DWORD PTR [EDX + 8]
                   FSTP DWORD PTR [ECX + 8]
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorReflect(V, N: TAffineVector): TAffineVector; assembler; register;

// reflects vector V against N (assumes N is normalized)
// EAX contains address of V
// EDX contains address of N
// ECX contains address of the result

//var Dot : Single;

asm
   {Dot := VectorAffineDotProduct(V, N);
   Result[X] := V[X]-2 * Dot * N[X];
   Result[Y] := V[Y]-2 * Dot * N[Y];
   Result[Z] := V[Z]-2 * Dot * N[Z];}

                   CALL VectorAffineDotProduct   // dot is now in ST(0)
                   FCHS                          // -dot
                   FADD ST, ST                   // -dot * 2
                   FLD DWORD PTR [EDX]           // ST := N[X]
                   FMUL ST, ST(1)                // ST := -2 * dot * N[X]
                   FADD DWORD PTR[EAX]           // ST := V[X] - 2 * dot * N[X]
                   FSTP DWORD PTR [ECX]          // store result
                   FLD DWORD PTR [EDX + 4]       // etc.
                   FMUL ST, ST(1)
                   FADD DWORD PTR[EAX + 4]
                   FSTP DWORD PTR [ECX + 4]
                   FLD DWORD PTR [EDX + 8]
                   FMUL ST, ST(1)
                   FADD DWORD PTR[EAX + 8]
                   FSTP DWORD PTR [ECX + 8]
                   FSTP ST                       // clean FPU stack
end;

//----------------------------------------------------------------------------------------------------------------------

procedure VectorRotate(var Vector: TVector4f; Axis: TVector3f; Angle: Single);

// rotates Vector about Axis with Angle radiants

var RotMatrix : TMatrix4f;

begin
  RotMatrix := CreateRotationMatrix(Axis, Angle);
  Vector := VectorTransform(Vector, RotMatrix);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure VectorScale(V: array of Single; Factor: Single); assembler; register;

// returns a vector scaled by a factor
// EAX contains address of V
// EDX contains highest index in V
// Factor is located on the stack

asm
  {for I := Low(V) to High(V) do V[I] := V[I] * Factor;}

              FLD DWORD PTR [Factor]        // load factor
@@Loop:       FLD DWORD PTR [EAX + 4 * EDX] // load a component
              FMUL ST, ST(1)                // multiply it with the factor
              WAIT
              FSTP DWORD PTR [EAX + 4 * EDX] // store the result
              DEC EDX                       // do the entire array
              JNS @@Loop
              FSTP ST(0)                    // clean the FPU stack
end;

//----------------------------------------------------------------------------------------------------------------------

procedure VectorNegate(V: array of Single); assembler; register;

// returns a negated vector
// EAX contains address of V
// EDX contains highest index in V

asm
  {V[X] := -V[X];
  V[Y] := -V[Y];
  V[Z] := -V[Z];}

@@Loop:       FLD DWORD PTR [EAX + 4 * EDX]
              FCHS
              WAIT
              FSTP DWORD PTR [EAX + 4 * EDX]
              DEC EDX
              JNS @@Loop
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorAdd(V1, V2: TVector): TVector; register;

// returns the sum of two vectors

begin
  Result[X] := V1[X] + V2[X];
  Result[Y] := V1[Y] + V2[Y];
  Result[Z] := V1[Z] + V2[Z];
  Result[W] := V1[W] + V2[W];
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorAffineAdd(V1, V2: TAffineVector): TAffineVector; register;

// returns the sum of two vectors

begin
  Result[X] := V1[X] + V2[X];
  Result[Y] := V1[Y] + V2[Y];
  Result[Z] := V1[Z] + V2[Z];
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorSubtract(V1, V2: TVector): TVector; register;

// returns the difference of two vectors

begin
  Result[X] := V1[X] - V2[X];
  Result[Y] := V1[Y] - V2[Y];
  Result[Z] := V1[Z] - V2[Z];
  Result[W] := V1[W] - V2[W];
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorDotProduct(V1, V2: TVector): Single; register;

begin
  Result := V1[X] * V2[X] + V1[Y] * V2[Y] + V1[Z] * V2[Z] + V1[W] * V2[W];
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorAffineDotProduct(V1, V2: TAffineVector): Single; assembler; register;

// calculates the dot product between V1 and V2
// EAX contains address of V1
// EDX contains address of V2
// result is stored in ST(0)

asm
  //Result := V1[X] * V2[X] + V1[Y] * V2[Y] + V1[Z] * V2[Z];

                   FLD DWORD PTR [EAX]
                   FMUL DWORD PTR [EDX]
                   FLD DWORD PTR [EAX + 4]
                   FMUL DWORD PTR [EDX + 4]
                   FADDP
                   FLD DWORD PTR [EAX + 8]
                   FMUL DWORD PTR [EDX + 8]
                   FADDP
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorCrossProduct(V1, V2: TAffineVector): TAffineVector;

// calculates the cross product between vector 1 and 2, Temp is necessary because
// either V1 or V2 could also be the result vector
//
// EAX contains address of V1
// EDX contains address of V2
// ECX contains address of result

var Temp: TAffineVector;

asm
  {Temp[X] := V1[Y] * V2[Z]-V1[Z] * V2[Y];
  Temp[Y] := V1[Z] * V2[X]-V1[X] * V2[Z];
  Temp[Z] := V1[X] * V2[Y]-V1[Y] * V2[X];
  Result := Temp;}

              PUSH EBX                      // save EBX, must be restored to original value
              LEA EBX, [Temp]
              FLD DWORD PTR [EDX + 8]       // first load both vectors onto FPU register stack
              FLD DWORD PTR [EDX + 4]
              FLD DWORD PTR [EDX + 0]
              FLD DWORD PTR [EAX + 8]
              FLD DWORD PTR [EAX + 4]
              FLD DWORD PTR [EAX + 0]

              FLD ST(1)                     // ST(0) := V1[Y]
              FMUL ST, ST(6)                // ST(0) := V1[Y] * V2[Z]
              FLD ST(3)                     // ST(0) := V1[Z]
              FMUL ST, ST(6)                // ST(0) := V1[Z] * V2[Y]
              FSUBP ST(1), ST               // ST(0) := ST(1)-ST(0)
              FSTP DWORD [EBX]              // Temp[X] := ST(0)
              FLD ST(2)                     // ST(0) := V1[Z]
              FMUL ST, ST(4)                // ST(0) := V1[Z] * V2[X]
              FLD ST(1)                     // ST(0) := V1[X]
              FMUL ST, ST(7)                // ST(0) := V1[X] * V2[Z]
              FSUBP ST(1), ST               // ST(0) := ST(1)-ST(0)
              FSTP DWORD [EBX + 4]          // Temp[Y] := ST(0)
              FLD ST                        // ST(0) := V1[X]
              FMUL ST, ST(5)                // ST(0) := V1[X] * V2[Y]
              FLD ST(2)                     // ST(0) := V1[Y]
              FMUL ST, ST(5)                // ST(0) := V1[Y] * V2[X]
              FSUBP ST(1), ST               // ST(0) := ST(1)-ST(0)
              FSTP DWORD [EBX + 8]          // Temp[Z] := ST(0)
              FSTP ST(0)                    // clear FPU register stack
              FSTP ST(0)
              FSTP ST(0)
              FSTP ST(0)
              FSTP ST(0)
              FSTP ST(0)
              MOV EAX, [EBX]                // copy Temp to Result
              MOV [ECX], EAX
              MOV EAX, [EBX + 4]
              MOV [ECX + 4], EAX
              MOV EAX, [EBX + 8]
              MOV [ECX + 8], EAX
              POP EBX
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorPerpendicular(V, N: TAffineVector): TAffineVector;

// calculates a vector perpendicular to N (N is assumed to be of unit length)
// subtract out any component parallel to N

var Dot: Single;

begin
   Dot := VectorAffineDotProduct(V, N);
   Result[X] := V[X]-Dot * N[X];
   Result[Y] := V[Y]-Dot * N[Y];
   Result[Z] := V[Z]-Dot * N[Z];
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorTransform(V: TVector4f; M: TMatrix): TVector4f; register;

// transforms a homogeneous vector by multiplying it with a matrix

var TV: TVector4f;

begin
  TV[X] := V[X] * M[X, X] + V[Y] * M[Y, X] + V[Z] * M[Z, X] + V[W] * M[W, X];
  TV[Y] := V[X] * M[X, Y] + V[Y] * M[Y, Y] + V[Z] * M[Z, Y] + V[W] * M[W, Y];
  TV[Z] := V[X] * M[X, Z] + V[Y] * M[Y, Z] + V[Z] * M[Z, Z] + V[W] * M[W, Z];
  TV[W] := V[X] * M[X, W] + V[Y] * M[Y, W] + V[Z] * M[Z, W] + V[W] * M[W, W];
  Result := TV
end;

//----------------------------------------------------------------------------------------------------------------------

function  VectorTransform(V: TVector3f; M: TMatrix): TVector3f;

// transforms an affine vector by multiplying it with a (homogeneous) matrix

var TV: TVector3f;

begin
  TV[X] := V[X] * M[X, X] + V[Y] * M[Y, X] + V[Z] * M[Z, X] + M[W, X];
  TV[Y] := V[X] * M[X, Y] + V[Y] * M[Y, Y] + V[Z] * M[Z, Y] + M[W, Y];
  TV[Z] := V[X] * M[X, Z] + V[Y] * M[Y, Z] + V[Z] * M[Z, Z] + M[W, Z];
  Result := TV;
end;


//----------------------------------------------------------------------------------------------------------------------

function VectorAffineTransform(V: TAffineVector; M: TAffineMatrix): TAffineVector; register;

// transforms an affine vector by multiplying it with a matrix

var TV: TAffineVector;

begin
  TV[X] := V[X] * M[X, X] + V[Y] * M[Y, X] + V[Z] * M[Z, X];
  TV[Y] := V[X] * M[X, Y] + V[Y] * M[Y, Y] + V[Z] * M[Z, Y];
  TV[Z] := V[X] * M[X, Z] + V[Y] * M[Y, Z] + V[Z] * M[Z, Z];
  Result := TV;
end;

//----------------------------------------------------------------------------------------------------------------------

function PointInPolygon(xp, yp : array of Single; x, y: Single): Boolean;

// The code below is from Wm. Randolph Franklin <wrf@ecse.rpi.edu>
// with some minor modifications for speed.  It returns 1 for strictly
// interior points, 0 for strictly exterior, and 0 or 1 for points on
// the boundary.
// This code is not yet tested!

var I, J: Integer;

begin
  Result := False;
  if High(XP) <> High(YP) then Exit;
  J := High(XP);
  for I := 0 to High(XP) do
  begin
    if ((((yp[I] <= y) and (y < yp[J])) or ((yp[J] <= y) and (y < yp[I]))) and
        (x < (xp[J] - xp[I]) * (y - yp[I]) / (yp[J] - yp[I]) + xp[I]))
    then Result := not Result;
    J := I + 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function QuaternionConjugate(Q: TQuaternion): TQuaternion; assembler;

// returns the conjugate of a quaternion
// EAX contains address of Q
// EDX contains address of result

asm
              FLD DWORD PTR [EAX]
              FCHS
              WAIT
              FSTP DWORD PTR [EDX]
              FLD DWORD PTR [EAX + 4]
              FCHS
              WAIT
              FSTP DWORD PTR [EDX + 4]
              FLD DWORD PTR [EAX + 8]
              FCHS
              WAIT
              FSTP DWORD PTR [EDX + 8]
              MOV EAX, [EAX + 12]
              MOV [EDX + 12], EAX
end;

//----------------------------------------------------------------------------------------------------------------------

function QuaternionFromPoints(V1, V2: TAffineVector): TQuaternion; assembler;

// constructs a unit quaternion from two points on unit sphere
// EAX contains address of V1
// ECX contains address to result
// EDX contains address of V2

asm
  {Result.ImagPart := VectorCrossProduct(V1, V2);
   Result.RealPart :=  Sqrt((VectorAffineDotProduct(V1, V2) + 1)/2);}

              PUSH EAX
              CALL VectorCrossProduct       // determine axis to rotate about
              POP EAX
              FLD1                          // prepare next calculation
              Call VectorAffineDotProduct   // calculate cos(angle between V1 and V2)
              FADD ST, ST(1)                // transform angle to angle/2 by: cos(a/2)=sqrt((1 + cos(a))/2)
              FXCH ST(1)
              FADD ST, ST
              FDIVP ST(1), ST
              FSQRT
              FSTP DWORD PTR [ECX + 12]     // Result.RealPart := ST(0)
end;

//----------------------------------------------------------------------------------------------------------------------

function QuaternionMultiply(qL, qR: TQuaternion): TQuaternion;

// Returns quaternion product qL * qR.  Note: order is important!
// To combine rotations, use the product QuaternionMuliply(qSecond, qFirst),
// which gives the effect of rotating by qFirst then qSecond.

var Temp : TQuaternion;

begin
  Temp.RealPart := qL.RealPart * qR.RealPart - qL.ImagPart[X] * qR.ImagPart[X] -
                   qL.ImagPart[Y] * qR.ImagPart[Y] - qL.ImagPart[Z] * qR.ImagPart[Z];
  Temp.ImagPart[X] := qL.RealPart * qR.ImagPart[X] + qL.ImagPart[X] * qR.RealPart +
                      qL.ImagPart[Y] * qR.ImagPart[Z] - qL.ImagPart[Z] * qR.ImagPart[Y];
  Temp.ImagPart[Y] := qL.RealPart * qR.ImagPart[Y] + qL.ImagPart[Y] * qR.RealPart +
                      qL.ImagPart[Z] * qR.ImagPart[X] - qL.ImagPart[X] * qR.ImagPart[Z];
  Temp.ImagPart[Z] := qL.RealPart * qR.ImagPart[Z] + qL.ImagPart[Z] * qR.RealPart +
                      qL.ImagPart[X] * qR.ImagPart[Y] - qL.ImagPart[Y] * qR.ImagPart[X];
  Result := Temp;
end;

//----------------------------------------------------------------------------------------------------------------------

function QuaternionToMatrix(Q: TQuaternion): TMatrix;

// Constructs rotation matrix from (possibly non-unit) quaternion.
// Assumes matrix is used to multiply column vector on the left:
// vnew = mat vold.  Works correctly for right-handed coordinate system
// and right-handed rotations.

// Essentially, this function is the same as CreateRotationMatrix and you can consider it as
// being for reference here.

{var Norm, S,
    XS, YS, ZS,
    WX, WY, WZ,
    XX, XY, XZ,
    YY, YZ, ZZ   : Single;

begin
  Norm := Q.Vector[X] * Q.Vector[X] + Q.Vector[Y] * Q.Vector[Y] + Q.Vector[Z] * Q.Vector[Z] + Q.RealPart * Q.RealPart;
  if Norm > 0 then S := 2 / Norm
              else S := 0;

  XS := Q.Vector[X] * S;   YS := Q.Vector[Y] * S;   ZS := Q.Vector[Z] * S;
  WX := Q.RealPart * XS;   WY := Q.RealPart * YS;   WZ := Q.RealPart * ZS;
  XX := Q.Vector[X] * XS;  XY := Q.Vector[X] * YS;  XZ := Q.Vector[X] * ZS;
  YY := Q.Vector[Y] * YS;  YZ := Q.Vector[Y] * ZS;  ZZ := Q.Vector[Z] * ZS;

  Result[X, X] := 1 - (YY + ZZ); Result[Y, X] := XY + WZ;       Result[Z, X] := XZ - WY;       Result[W, X] := 0;
  Result[X, Y] := XY - WZ;       Result[Y, Y] := 1 - (XX + ZZ); Result[Z, Y] := YZ + WX;       Result[W, Y] := 0;
  Result[X, Z] := XZ + WY;       Result[Y, Z] := YZ - WX;       Result[Z, Z] := 1 - (XX + YY); Result[W, Z] := 0;
  Result[X, W] := 0;             Result[Y, W] := 0;             Result[Z, W] := 0;             Result[W, W] := 1;}

var
  V: TAffineVector;
  SinA, CosA,
  A, B, C: Extended;

begin
  V := Q.ImagPart;
  VectorNormalize(V);
  SinCos(Q.RealPart / 2, SinA, CosA);
  A := V[X] * SinA;
  B := V[Y] * SinA;
  C := V[Z] * SinA;

  Result := IdentityMatrix;
  Result[X, X] := 1 - 2 * B * B - 2 * C * C;
  Result[X, Y] := 2 * A * B - 2 * CosA * C;
  Result[X, Z] := 2 * A * C + 2 * CosA * B;

  Result[Y, X] := 2 * A * B + 2 * CosA * C;
  Result[Y, Y] := 1 - 2 * A * A - 2 * C * C;
  Result[Y, Z] := 2 * B * C - 2 * CosA * A;

  Result[Z, X] := 2 * A * C - 2 * CosA * B;
  Result[Z, Y] := 2 * B * C + 2 * CosA * A;
  Result[Z, Z] := 1 - 2 * A * A - 2 * B * B;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure QuaternionToPoints(Q: TQuaternion; var ArcFrom, ArcTo: TAffineVector); register;

// converts a unit quaternion into two points on a unit sphere

var S: Single;

begin
  S := Sqrt(Q.ImagPart[X] * Q.ImagPart[X] + Q.ImagPart[Y] * Q.ImagPart[Y]);
  if S = 0 then ArcFrom := MakeAffineVector([0, 1, 0])
           else ArcFrom := MakeAffineVector([-Q.ImagPart[Y] / S, Q.ImagPart[X] / S, 0]);
  ArcTo[X] := Q.RealPart * ArcFrom[X] - Q.ImagPart[Z] * ArcFrom[Y];
  ArcTo[Y] := Q.RealPart * ArcFrom[Y] + Q.ImagPart[Z] * ArcFrom[X];
  ArcTo[Z] := Q.ImagPart[X] * ArcFrom[Y] - Q.ImagPart[Y] * ArcFrom[X];
  if Q.RealPart < 0 then ArcFrom := MakeAffineVector([-ArcFrom[X], -ArcFrom[Y], 0]);
end;

//----------------------------------------------------------------------------------------------------------------------

function MatrixAffineDeterminant(M: TAffineMatrix): Single; register;

// determinant of a 3x3 matrix

begin
  Result := M[X, X] * (M[Y, Y] * M[Z, Z] - M[Z, Y] * M[Y, Z]) -
            M[X, Y] * (M[Y, X] * M[Z, Z] - M[Z, X] * M[Y, Z]) +
            M[X, Z] * (M[Y, X] * M[Z, Y] - M[Z, X] * M[Y, Y]);
end;

//----------------------------------------------------------------------------------------------------------------------

function MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;

// internal version for the determinant of a 3x3 matrix

begin
  Result := a1 * (b2 * c3 - b3 * c2) -
            b1 * (a2 * c3 - a3 * c2) +
            c1 * (a2 * b3 - a3 * b2);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure MatrixAdjoint(var M: TMatrix); register;

// Adjoint of a 4x4 matrix - used in the computation of the inverse
// of a 4x4 matrix

var a1, a2, a3, a4,
    b1, b2, b3, b4,
    c1, c2, c3, c4,
    d1, d2, d3, d4: Single;


begin
    a1 :=  M[X, X]; b1 :=  M[X, Y];
    c1 :=  M[X, Z]; d1 :=  M[X, W];
    a2 :=  M[Y, X]; b2 :=  M[Y, Y];
    c2 :=  M[Y, Z]; d2 :=  M[Y, W];
    a3 :=  M[Z, X]; b3 :=  M[Z, Y];
    c3 :=  M[Z, Z]; d3 :=  M[Z, W];
    a4 :=  M[W, X]; b4 :=  M[W, Y];
    c4 :=  M[W, Z]; d4 :=  M[W, W];

    // row column labeling reversed since we transpose rows & columns
    M[X, X] :=  MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
    M[Y, X] := -MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
    M[Z, X] :=  MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
    M[W, X] := -MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);

    M[X, Y] := -MatrixDetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
    M[Y, Y] :=  MatrixDetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
    M[Z, Y] := -MatrixDetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
    M[W, Y] :=  MatrixDetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);

    M[X, Z] :=  MatrixDetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
    M[Y, Z] := -MatrixDetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
    M[Z, Z] :=  MatrixDetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
    M[W, Z] := -MatrixDetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);

    M[X, W] := -MatrixDetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
    M[Y, W] :=  MatrixDetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
    M[Z, W] := -MatrixDetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
    M[W, W] :=  MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

//----------------------------------------------------------------------------------------------------------------------

function MatrixDeterminant(M: TMatrix): Single; register;

// Determinant of a 4x4 matrix

var a1, a2, a3, a4,
    b1, b2, b3, b4,
    c1, c2, c3, c4,
    d1, d2, d3, d4  : Single;

begin
  a1 := M[X, X];  b1 := M[X, Y];  c1 := M[X, Z];  d1 := M[X, W];
  a2 := M[Y, X];  b2 := M[Y, Y];  c2 := M[Y, Z];  d2 := M[Y, W];
  a3 := M[Z, X];  b3 := M[Z, Y];  c3 := M[Z, Z];  d3 := M[Z, W];
  a4 := M[W, X];  b4 := M[W, Y];  c4 := M[W, Z];  d4 := M[W, W];

  Result := a1 * MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4) -
            b1 * MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4) +
            c1 * MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4) -
            d1 * MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure MatrixScale(var M: TMatrix; Factor: Single); register;

// multiplies all elements of a 4x4 matrix with a factor

var I, J: Integer;

begin
  for I := 0 to 3 do
    for J := 0 to 3 do M[I, J] := M[I, J] * Factor;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure MatrixInvert(var M: TMatrix); register;

// finds the inverse of a 4x4 matrix

var Det: Single;

begin
  Det := MatrixDeterminant(M);
  if Abs(Det) < EPSILON then M := IdentityMatrix
                        else
  begin
    MatrixAdjoint(M);
    MatrixScale(M, 1 / Det);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure MatrixTranspose(var M: TMatrix); register;

// computes transpose of 4x4 matrix

var I, J: Integer;
    TM: TMatrix;

begin
  for I := 0 to 3 do
    for J := 0 to 3 do TM[J, I] := M[I, J];
  M := TM;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure MatrixAffineTranspose(var M: TAffineMatrix); register;

// computes transpose of 3x3 matrix

var I, J: Integer;
    TM: TAffineMatrix;

begin
  for I := 0 to 2 do
    for J := 0 to 2 do TM[J, I] := M[I, J];
  M := TM;
end;

//----------------------------------------------------------------------------------------------------------------------

function MatrixMultiply(M1, M2: TMatrix): TMatrix; register;

// multiplies two 4x4 matrices

var I, J: Integer;
    TM: TMatrix;

begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      TM[I, J] := M1[I, X] * M2[X, J] +
                  M1[I, Y] * M2[Y, J] +
                  M1[I, Z] * M2[Z, J] +
                  M1[I, W] * M2[W, J];
  Result := TM;
end;

//----------------------------------------------------------------------------------------------------------------------

function CreateRotationMatrix(Axis: TVector3f; Angle: Single): TMatrix; register;

// Creates a rotation matrix along the given Axis by the given Angle in radians.

var cosine,
    sine,
    Len,
    one_minus_cosine: Extended;

begin
  SinCos(Angle, Sine, Cosine);
  one_minus_cosine := 1 - cosine;
  Len := VectorNormalize(Axis);

  if Len = 0 then Result := IdentityMatrix
             else
  begin
    Result[X, X] := (one_minus_cosine * Sqr(Axis[0])) + Cosine;
    Result[X, Y] := (one_minus_cosine * Axis[0] * Axis[1]) - (Axis[2] * Sine);
    Result[X, Z] := (one_minus_cosine * Axis[2] * Axis[0]) + (Axis[1] * Sine);
    Result[X, W] := 0;

    Result[Y, X] := (one_minus_cosine * Axis[0] * Axis[1]) + (Axis[2] * Sine);
    Result[Y, Y] := (one_minus_cosine * Sqr(Axis[1])) + Cosine;
    Result[Y, Z] := (one_minus_cosine * Axis[1] * Axis[2]) - (Axis[0] * Sine);
    Result[Y, W] := 0;

    Result[Z, X] := (one_minus_cosine * Axis[2] * Axis[0]) - (Axis[1] * Sine);
    Result[Z, Y] := (one_minus_cosine * Axis[1] * Axis[2]) + (Axis[0] * Sine);
    Result[Z, Z] := (one_minus_cosine * Sqr(Axis[2])) + Cosine;
    Result[Z, W] := 0;

    Result[W, X] := 0;
    Result[W, Y] := 0;
    Result[W, Z] := 0;
    Result[W, W] := 1;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function ConvertRotation(Angles: TAffineVector): TVector; register;

{ Turn a triplet of rotations about x, y, and z (in that order) into an
   equivalent rotation around a single axis (all in radians).

   Rotation of the Angle t about the axis (X, Y, Z) is given by:

     | X^2 + (1-X^2) Cos(t),    XY(1-Cos(t))  +  Z Sin(t), XZ(1-Cos(t))-Y Sin(t) |
 M = | XY(1-Cos(t))-Z Sin(t), Y^2 + (1-Y^2) Cos(t),      YZ(1-Cos(t)) + X Sin(t) |
     | XZ(1-Cos(t)) + Y Sin(t), YZ(1-Cos(t))-X Sin(t),   Z^2 + (1-Z^2) Cos(t)    |

   Rotation about the three axes (Angles a1, a2, a3) can be represented as
   the product of the individual rotation matrices:

      | 1  0       0       | | Cos(a2) 0 -Sin(a2) | |  Cos(a3) Sin(a3) 0 |
      | 0  Cos(a1) Sin(a1) | * | 0       1  0       | * | -Sin(a3) Cos(a3) 0 |
      | 0 -Sin(a1) Cos(a1) | | Sin(a2) 0  Cos(a2) | |  0       0       1 |
	     Mx                       My                     Mz

   We now want to solve for X, Y, Z, and t given 9 equations in 4 unknowns.
   Using the diagonal elements of the two matrices, we get:

      X^2 + (1-X^2) Cos(t) = M[0][0]
      Y^2 + (1-Y^2) Cos(t) = M[1][1]
      Z^2 + (1-Z^2) Cos(t) = M[2][2]

   Adding the three equations, we get:

      X^2  +  Y^2  +  Z^2 - (M[0][0]  +  M[1][1]  +  M[2][2]) =
	 - (3 - X^2 - Y^2 - Z^2) Cos(t)

   Since (X^2  +  Y^2  +  Z^2) = 1, we can rewrite as:

      Cos(t) = (1 - (M[0][0]  +  M[1][1]  +  M[2][2])) / 2

   Solving for t, we get:

      t = Acos(((M[0][0]  +  M[1][1]  +  M[2][2]) - 1) / 2)

    We can substitute t into the equations for X^2, Y^2, and Z^2 above
    to get the values for X, Y, and Z.  To find the proper signs we note
    that:

	2 X Sin(t) = M[1][2] - M[2][1]
	2 Y Sin(t) = M[2][0] - M[0][2]
	2 Z Sin(t) = M[0][1] - M[1][0]
}

var Axis1, Axis2: TVector3f;
    M, M1, M2: TMatrix;
    cost, cost1,
    sint,
    s1, s2, s3: Single;
    I: Integer;


begin
  // see if we are only rotating about a single Axis
  if Abs(Angles[X]) < EPSILON then
  begin
    if Abs(Angles[Y]) < EPSILON then
    begin
      Result := MakeVector([0, 0, 1, Angles[Z]]);
      Exit;
    end
    else
      if Abs(Angles[Z]) < EPSILON then
      begin
        Result := MakeVector([0, 1, 0, Angles[Y]]);
        Exit;
      end
   end
   else
     if (Abs(Angles[Y]) < EPSILON) and
        (Abs(Angles[Z]) < EPSILON) then
     begin
       Result := MakeVector([1, 0, 0, Angles[X]]);
       Exit;
     end;

  // make the rotation matrix
  Axis1 := MakeAffineVector([1, 0, 0]);
  M := CreateRotationMatrix(Axis1, Angles[X]);

  Axis2 := MakeAffineVector([0, 1, 0]);
  M2 := CreateRotationMatrix(Axis2, Angles[Y]);
  M1 := MatrixMultiply(M, M2);

  Axis2 := MakeAffineVector([0, 0, 1]);
  M2 := CreateRotationMatrix(Axis2, Angles[Z]);
  M := MatrixMultiply(M1, M2);

  cost := ((M[X, X] + M[Y, Y] + M[Z, Z])-1) / 2;
  if cost < -1 then cost := -1
               else
    if cost > 1 - EPSILON then
    begin
      // Bad Angle - this would cause a crash
      Result := MakeVector([1, 0, 0, 0]);
      Exit;
    end;

  cost1 := 1 - cost;
  Result := Makevector([Sqrt((M[X, X]-cost) / cost1),
                      Sqrt((M[Y, Y]-cost) / cost1),
                      sqrt((M[Z, Z]-cost) / cost1),
                      arccos(cost)]);

  sint := 2 * Sqrt(1 - cost * cost); // This is actually 2 Sin(t)

  // Determine the proper signs
  for I := 0 to 7 do
  begin
    if (I and 1) > 1 then s1 := -1 else s1 := 1;
    if (I and 2) > 1 then s2 := -1 else s2 := 1;
    if (I and 4) > 1 then s3 := -1 else s3 := 1;
    if (Abs(s1 * Result[X] * sint-M[Y, Z] + M[Z, Y]) < EPSILON2) and
       (Abs(s2 * Result[Y] * sint-M[Z, X] + M[X, Z]) < EPSILON2) and
       (Abs(s3 * Result[Z] * sint-M[X, Y] + M[Y, X]) < EPSILON2) then
        begin
          // We found the right combination of signs
          Result[X] := Result[X] * s1;
          Result[Y] := Result[Y] * s2;
          Result[Z] := Result[Z] * s3;
          Exit;
        end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function CreateRotationMatrixX(Sine, Cosine: Single): TMatrix; register;

// creates matrix for rotation about x-axis

begin
  Result := EmptyMatrix;
  Result[X, X] := 1;
  Result[Y, Y] := Cosine;
  Result[Y, Z] := Sine;
  Result[Z, Y] := -Sine;
  Result[Z, Z] := Cosine;
  Result[W, W] := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function CreateRotationMatrixY(Sine, Cosine: Single): TMatrix; register;

// creates matrix for rotation about y-axis

begin
  Result := EmptyMatrix;
  Result[X, X] := Cosine;
  Result[X, Z] := -Sine;
  Result[Y, Y] := 1;
  Result[Z, X] := Sine;
  Result[Z, Z] := Cosine;
  Result[W, W] := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function CreateRotationMatrixZ(Sine, Cosine: Single): TMatrix; register;

// creates matrix for rotation about z-axis

begin
  Result := EmptyMatrix;
  Result[X, X] := Cosine;
  Result[X, Y] := Sine;
  Result[Y, X] := -Sine;
  Result[Y, Y] := Cosine;
  Result[Z, Z] := 1;
  Result[W, W] := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function CreateScaleMatrix(V: TAffineVector): TMatrix; register;

// creates scaling matrix

begin
  Result := IdentityMatrix;
  Result[X, X] := V[X];
  Result[Y, Y] := V[Y];
  Result[Z, Z] := V[Z];
end;

//----------------------------------------------------------------------------------------------------------------------

function CreateTranslationMatrix(V: TVector): TMatrix; register;

// creates translation matrix

begin
  Result := IdentityMatrix;
  Result[W, X] := V[X];
  Result[W, Y] := V[Y];
  Result[W, Z] := V[Z];
  Result[W, W] := V[W];
end;

//----------------------------------------------------------------------------------------------------------------------

function Lerp(Start, Stop, t: Single): Single;

// calculates linear interpolation between start and stop at point t

begin
  Result := Start + (Stop - Start) * t;
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorAffineLerp(V1, V2: TAffineVector; t: Single): TAffineVector;

// calculates linear interpolation between vector1 and vector2 at point t

begin
  Result[X] := Lerp(V1[X], V2[X], t);
  Result[Y] := Lerp(V1[Y], V2[Y], t);
  Result[Z] := Lerp(V1[Z], V2[Z], t);
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorLerp(V1, V2: TVector; t: Single): TVector;

// calculates linear interpolation between vector1 and vector2 at point t

begin
  Result[X] := Lerp(V1[X], V2[X], t);
  Result[Y] := Lerp(V1[Y], V2[Y], t);
  Result[Z] := Lerp(V1[Z], V2[Z], t);
  Result[W] := Lerp(V1[W], V2[W], t);
end;

//----------------------------------------------------------------------------------------------------------------------

function QuaternionSlerp(QStart, QEnd: TQuaternion; Spin: Integer; t: Single): TQuaternion;

// spherical linear interpolation of unit quaternions with spins
// QStart, QEnd - start and end unit quaternions
// t            - interpolation parameter (0 to 1)
// Spin         - number of extra spin rotations to involve

var beta,                   // complementary interp parameter
    theta,                  // Angle between A and B
    sint, cost,             // sine, cosine of theta
    phi: Single;            // theta plus spins
    bflip: Boolean;         // use negativ t?


begin
  // cosine theta
  cost := VectorAngle(QStart.ImagPart, QEnd.ImagPart);

  // if QEnd is on opposite hemisphere from QStart, use -QEnd instead
  if cost < 0 then
  begin
    cost := -cost;
    bflip := True;
  end
  else bflip := False;

  // if QEnd is (within precision limits) the same as QStart,
  // just linear interpolate between QStart and QEnd.
  // Can't do spins, since we don't know what direction to spin.

  if (1 - cost) < EPSILON then beta := 1 - t
                          else
  begin
    // normal case
    theta := arccos(cost);
    phi := theta + Spin * Pi;
    sint := sin(theta);
    beta := sin(theta - t * phi) / sint;
    t := sin(t * phi) / sint;
  end;

  if bflip then t := -t;

  // interpolate
  Result.ImagPart[X] := beta * QStart.ImagPart[X] + t * QEnd.ImagPart[X];
  Result.ImagPart[Y] := beta * QStart.ImagPart[Y] + t * QEnd.ImagPart[Y];
  Result.ImagPart[Z] := beta * QStart.ImagPart[Z] + t * QEnd.ImagPart[Z];
  Result.RealPart := beta * QStart.RealPart + t * QEnd.RealPart;
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorAffineCombine(V1, V2: TAffineVector; F1, F2: Single): TAffineVector;

// makes a linear combination of two vectors and return the result

begin
  Result[X] := (F1 * V1[X]) + (F2 * V2[X]);
  Result[Y] := (F1 * V1[Y]) + (F2 * V2[Y]);
  Result[Z] := (F1 * V1[Z]) + (F2 * V2[Z]);
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorCombine(V1, V2: TVector; F1, F2: Single): TVector;

// makes a linear combination of two vectors and return the result

begin
  Result[X] := (F1 * V1[X]) + (F2 * V2[X]);
  Result[Y] := (F1 * V1[Y]) + (F2 * V2[Y]);
  Result[Z] := (F1 * V1[Z]) + (F2 * V2[Z]);
  Result[W] := (F1 * V1[W]) + (F2 * V2[W]);
end;

//----------------------------------------------------------------------------------------------------------------------

function MatrixDecompose(M: TMatrix; var Tran: TTransformations): Boolean; register;

// Author: Spencer W. Thomas, University of Michigan
//
// MatrixDecompose - Decompose a non-degenerated 4x4 transformation matrix into
// the sequence of transformations that produced it.
//
// The coefficient of each transformation is returned in the corresponding
// element of the vector Tran.
//
// Returns true upon success, false if the matrix is singular.

var I, J: Integer;
    LocMat,
    pmat,
    invpmat,
    tinvpmat: TMatrix;
    prhs,
    psol: TVector;
    Row: array[0..2] of TAffineVector;

begin
  Result := False;
  locmat := M;
  // normalize the matrix
  if locmat[W, W] = 0 then Exit;
  for I := 0 to 3 do
    for J := 0 to 3 do
      locmat[I, J] := locmat[I, J] / locmat[W, W];

  // pmat is used to solve for perspective, but it also provides
  // an easy way to test for singularity of the upper 3x3 component.

  pmat := locmat;
  for I := 0 to 2 do pmat[I, W] := 0;
  pmat[W, W] := 1;

  if MatrixDeterminant(pmat) = 0 then Exit;

  // First, isolate perspective.  This is the messiest.
  if (locmat[X, W] <> 0) or
     (locmat[Y, W] <> 0) or
     (locmat[Z, W] <> 0) then
  begin
    // prhs is the right hand side of the equation.
    prhs[X] := locmat[X, W];
    prhs[Y] := locmat[Y, W];
    prhs[Z] := locmat[Z, W];
    prhs[W] := locmat[W, W];

    // Solve the equation by inverting pmat and multiplying
    // prhs by the inverse.  (This is the easiest way, not
    // necessarily the best.)

    invpmat := pmat;
    MatrixInvert(invpmat);
    MatrixTranspose(invpmat);
    psol := VectorTransform(prhs, tinvpmat);

    // stuff the answer away
    Tran[ttPerspectiveX] := psol[X];
    Tran[ttPerspectiveY] := psol[Y];
    Tran[ttPerspectiveZ] := psol[Z];
    Tran[ttPerspectiveW] := psol[W];

    // clear the perspective partition
    locmat[X, W] := 0;
    locmat[Y, W] := 0;
    locmat[Z, W] := 0;
    locmat[W, W] := 1;
  end
  else
  begin
    // no perspective
    Tran[ttPerspectiveX] := 0;
    Tran[ttPerspectiveY] := 0;
    Tran[ttPerspectiveZ] := 0;
    Tran[ttPerspectiveW] := 0;
  end;

  // next take care of translation (easy)
  for I := 0 to 2 do
  begin
    Tran[TTransType(Ord(ttTranslateX) + I)] := locmat[W, I];
    locmat[W, I] := 0;
  end;

  // now get scale and shear
  for I := 0 to 2 do
  begin
    row[I, X] := locmat[I, X];
    row[I, Y] := locmat[I, Y];
    row[I, Z] := locmat[I, Z];
  end;

  // compute X scale factor and normalize first row
  Tran[ttScaleX] := Sqr(VectorNormalize(row[0])); // ml: calculation optimized

  // compute XY shear factor and make 2nd row orthogonal to 1st
  Tran[ttShearXY] := VectorAffineDotProduct(row[0], row[1]);
  row[1] := VectorAffineCombine(row[1], row[0], 1, -Tran[ttShearXY]);

  // now, compute Y scale and normalize 2nd row
  Tran[ttScaleY] := Sqr(VectorNormalize(row[1])); // ml: calculation optimized
  Tran[ttShearXY] := Tran[ttShearXY]/Tran[ttScaleY];

  // compute XZ and YZ shears, orthogonalize 3rd row
  Tran[ttShearXZ] := VectorAffineDotProduct(row[0], row[2]);
  row[2] := VectorAffineCombine(row[2], row[0], 1, -Tran[ttShearXZ]);
  Tran[ttShearYZ] := VectorAffineDotProduct(row[1], row[2]);
  row[2] := VectorAffineCombine(row[2], row[1], 1, -Tran[ttShearYZ]);

  // next, get Z scale and normalize 3rd row
  Tran[ttScaleZ] := Sqr(VectorNormalize(row[1])); // (ML) calc. optimized
  Tran[ttShearXZ] := Tran[ttShearXZ] / tran[ttScaleZ];
  Tran[ttShearYZ] := Tran[ttShearYZ] / Tran[ttScaleZ];

  // At this point, the matrix (in rows[]) is orthonormal.
  // Check for a coordinate system flip.  If the determinant
  // is -1, then negate the matrix and the scaling factors.
  if VectorAffineDotProduct(row[0], VectorCrossProduct(row[1], row[2])) < 0 then
    for I := 0 to 2 do
    begin
      Tran[TTransType(Ord(ttScaleX) + I)] := -Tran[TTransType(Ord(ttScaleX) + I)];
      row[I, X] := -row[I, X];
      row[I, Y] := -row[I, Y];
      row[I, Z] := -row[I, Z];
    end;

  // now, get the rotations out, as described in the gem
  Tran[ttRotateY] := arcsin(-row[0, Z]);
  if cos(Tran[ttRotateY]) <> 0 then
  begin
    Tran[ttRotateX] := arctan2(row[1, Z], row[2, Z]);
    Tran[ttRotateZ] := arctan2(row[0, Y], row[0, X]);
  end
  else
  begin
    tran[ttRotateX] := arctan2(row[1, X], row[1, Y]);
    tran[ttRotateZ] := 0;
  end;
  // All done!
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorDblToFlt(V: THomogeneousDblVector): THomogeneousVector; assembler;

// converts a vector containing double sized values into a vector with single sized values

asm
              FLD  QWORD PTR [EAX]
              FSTP DWORD PTR [EDX]
              FLD  QWORD PTR [EAX + 8]
              FSTP DWORD PTR [EDX + 4]
              FLD  QWORD PTR [EAX + 16]
              FSTP DWORD PTR [EDX + 8]
              FLD  QWORD PTR [EAX + 24]
              FSTP DWORD PTR [EDX + 12]
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorAffineDblToFlt(V: TAffineDblVector): TAffineVector; assembler;

// converts a vector containing double sized values into a vector with single sized values

asm
              FLD  QWORD PTR [EAX]
              FSTP DWORD PTR [EDX]
              FLD  QWORD PTR [EAX + 8]
              FSTP DWORD PTR [EDX + 4]
              FLD  QWORD PTR [EAX + 16]
              FSTP DWORD PTR [EDX + 8]
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorAffineFltToDbl(V: TAffineVector): TAffineDblVector; assembler;

// converts a vector containing single sized values into a vector with double sized values

asm
              FLD  DWORD PTR [EAX]
              FSTP QWORD PTR [EDX]
              FLD  DWORD PTR [EAX + 8]
              FSTP QWORD PTR [EDX + 4]
              FLD  DWORD PTR [EAX + 16]
              FSTP QWORD PTR [EDX + 8]
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorFltToDbl(V: TVector): THomogeneousDblVector; assembler;

// converts a vector containing single sized values into a vector with double sized values

asm
              FLD  DWORD PTR [EAX]
              FSTP QWORD PTR [EDX]
              FLD  DWORD PTR [EAX + 8]
              FSTP QWORD PTR [EDX + 4]
              FLD  DWORD PTR [EAX + 16]
              FSTP QWORD PTR [EDX + 8]
              FLD  DWORD PTR [EAX + 24]
              FSTP QWORD PTR [EDX + 12]
end;

//----------------- coordinate system manipulation functions -----------------------------------------------------------

function Turn(Matrix: TMatrix; Angle: Single): TMatrix;

// rotates the given coordinate system (represented by the matrix) around its Y-axis

begin
  Result := MatrixMultiply(Matrix, CreateRotationMatrix(MakeAffineVector(Matrix[1]), Angle));
end;

//----------------------------------------------------------------------------------------------------------------------

function Turn(Matrix: TMatrix; MasterUp: TAffineVector; Angle: Single): TMatrix;

// rotates the given coordinate system (represented by the matrix) around MasterUp

begin
  Result := MatrixMultiply(Matrix, CreateRotationMatrix(MasterUp, Angle));
end;

//----------------------------------------------------------------------------------------------------------------------

function Pitch(Matrix: TMatrix; Angle: Single): TMatrix;

// rotates the given coordinate system (represented by the matrix) around its X-axis

begin
  Result := MatrixMultiply(Matrix, CreateRotationMatrix(MakeAffineVector(Matrix[0]), Angle));
end;

//----------------------------------------------------------------------------------------------------------------------

function Pitch(Matrix: TMatrix; MasterRight: TAffineVector; Angle: Single): TMatrix; overload;

// rotates the given coordinate system (represented by the matrix) around MasterRight

begin
  Result := MatrixMultiply(Matrix, CreateRotationMatrix(MasterRight, Angle));
end;

//----------------------------------------------------------------------------------------------------------------------

function Roll(Matrix: TMatrix; Angle: Single): TMatrix;

// rotates the given coordinate system (represented by the matrix) around its Z-axis

begin
  Result := MatrixMultiply(Matrix, CreateRotationMatrix(MakeAffineVector(Matrix[2]), Angle));
end;

//----------------------------------------------------------------------------------------------------------------------

function Roll(Matrix: TMatrix; MasterDirection: TAffineVector; Angle: Single): TMatrix; overload;

// rotates the given coordinate system (represented by the matrix) around MasterDirection

begin
  Result := MatrixMultiply(Matrix, CreateRotationMatrix(MasterDirection, Angle));
end;

//----------------------------------------------------------------------------------------------------------------------

end.


