(***************************************************************************)
(*                                                                         *)
(*  fttypes.h                                                              *)
(*                                                                         *)
(*    FreeType simple types definitions (specification only).              *)
(*                                                                         *)
(*  Copyright 1996-2001, 2002, 2004, 2006, 2007 by                         *)
(*  David Turner, Robert Wilhelm, and Werner Lemberg.                      *)
(*                                                                         *)
(*  This file is part of the FreeType project, and may only be used,       *)
(*  modified, and distributed under the terms of the FreeType project      *)
(*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     *)
(*  this file you indicate that you have read the license and              *)
(*  understand and accept it fully.                                        *)
(*                                                                         *)
(***************************************************************************)
(***************************************************************************)
(* Pascal port by the UltraStar Deluxe Team                                *)
(***************************************************************************)

  (*************************************************************************)
  (*                                                                       *)
  (* <Section>                                                             *)
  (*    basic_types                                                        *)
  (*                                                                       *)
  (* <Title>                                                               *)
  (*    Basic Data Types                                                   *)
  (*                                                                       *)
  (* <Abstract>                                                            *)
  (*    The basic data types defined by the library.                       *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    This section contains the basic data types defined by FreeType 2,  *)
  (*    ranging from simple scalar types to bitmap descriptors.  More      *)
  (*    font-specific structures are defined in a different section.       *)
  (*                                                                       *)
  (* <Order>                                                               *)
  (*    FT_Byte                                                            *)
  (*    FT_Bytes                                                           *)
  (*    FT_Char                                                            *)
  (*    FT_Int                                                             *)
  (*    FT_UInt                                                            *)
  (*    FT_Short                                                           *)
  (*    FT_UShort                                                          *)
  (*    FT_Long                                                            *)
  (*    FT_ULong                                                           *)
  (*    FT_Bool                                                            *)
  (*    FT_Offset                                                          *)
  (*    FT_PtrDist                                                         *)
  (*    FT_String                                                          *)
  (*    FT_Tag                                                             *)
  (*    FT_Error                                                           *)
  (*    FT_Fixed                                                           *)
  (*    FT_Pointer                                                         *)
  (*    FT_Pos                                                             *)
  (*    FT_Vector                                                          *)
  (*    FT_BBox                                                            *)
  (*    FT_Matrix                                                          *)
  (*    FT_FWord                                                           *)
  (*    FT_UFWord                                                          *)
  (*    FT_F2Dot14                                                         *)
  (*    FT_UnitVector                                                      *)
  (*    FT_F26Dot6                                                         *)
  (*                                                                       *)
  (*                                                                       *)
  (*    FT_Generic                                                         *)
  (*    FT_Generic_Finalizer                                               *)
  (*                                                                       *)
  (*    FT_Bitmap                                                          *)
  (*    FT_Pixel_Mode                                                      *)
  (*    FT_Palette_Mode                                                    *)
  (*    FT_Glyph_Format                                                    *)
  (*    FT_IMAGE_TAG                                                       *)
  (*                                                                       *)
  (*************************************************************************)

{$IFDEF TYPE_DECL}

  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_Bool                                                            *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A typedef of unsigned char, used for simple booleans.  As usual,   *)
  (*    values 1 and 0 represent true and false, respectively.             *)
  (*                                                                       *)
  FT_Bool    = cuchar;
{$ENDIF TYPE_DECL}
{$IFNDEF TYPE_DECL}
const
  FT_FALSE = 0;
  FT_TRUE  = 1;
{$ENDIF !TYPE_DECL}
{$IFDEF TYPE_DECL}

  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_Byte                                                            *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A simple typedef for the _unsigned_ char type.                     *)
  (*                                                                       *)
  FT_Byte    = cuchar;
  PFT_Byte   = ^FT_Byte;


  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_String                                                          *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A simple typedef for the char type, usually used for strings.      *)
  (*                                                                       *)
  FT_String  = cchar;
  PFT_String = ^FT_String;

  
  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_Short                                                           *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A typedef for signed short.                                        *)
  (*                                                                       *)
  FT_Short   = csshort;
  PFT_Short  = ^FT_Short;

  PFT_ShortArray = ^FT_ShortArray;
  FT_ShortArray = array[0 .. (MaxInt div SizeOf(FT_Short))-1] of FT_Short;

  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_UShort                                                          *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A typedef for unsigned short.                                      *)
  (*                                                                       *)
  FT_UShort  = cushort;


  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_Int                                                             *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A typedef for the int type.                                        *)
  (*                                                                       *)
  FT_Int     = csint;


  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_UInt                                                            *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A typedef for the unsigned int type.                               *)
  (*                                                                       *)
  FT_UInt    = cuint;


  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_Long                                                            *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A typedef for signed long.                                         *)
  (*                                                                       *)
  FT_Long    = cslong;


  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_ULong                                                           *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A typedef for unsigned long.                                       *)
  (*                                                                       *)
  FT_ULong   = culong;


  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_F26Dot6                                                         *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A signed 26.6 fixed float type used for vectorial pixel            *)
  (*    coordinates.                                                       *)
  (*                                                                       *)
  FT_F26Dot6 = cslong;


  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_Fixed                                                           *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    This type is used to store 16.16 fixed float values, like scaling  *)
  (*    values or matrix coefficients.                                     *)
  (*                                                                       *)
  FT_Fixed   = cslong;


  (*************************************************************************)
  (*                                                                       *)
  (* <Type>                                                                *)
  (*    FT_Error                                                           *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    The FreeType error code type.  A value of 0 is always interpreted  *)
  (*    as a successful operation.                                         *)
  (*                                                                       *)
  FT_Error   = cint;


  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_Matrix                                                          *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    A simple structure used to store a 2x2 matrix.  Coefficients are   *)
  (*    in 16.16 fixed float format.  The computation performed is:        *)
  (*                                                                       *)
  (*       {                                                               *)
  (*          x' = x*xx + y*xy                                             *)
  (*          y' = x*yx + y*yy                                             *)
  (*       }                                                               *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    xx :: Matrix coefficient.                                          *)
  (*                                                                       *)
  (*    xy :: Matrix coefficient.                                          *)
  (*                                                                       *)
  (*    yx :: Matrix coefficient.                                          *)
  (*                                                                       *)
  (*    yy :: Matrix coefficient.                                          *)
  (*                                                                       *)
  PFT_Matrix = ^FT_Matrix;
  FT_Matrix = record
    xx, xy: FT_Fixed;
    yx, yy: FT_Fixed;
  end;

  
  (*************************************************************************)
  (*                                                                       *)
  (* <FuncType>                                                            *)
  (*    FT_Generic_Finalizer                                               *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Describes a function used to destroy the `client' data of any      *)
  (*    FreeType object.  See the description of the FT_Generic type for   *)
  (*    details of usage.                                                  *)
  (*                                                                       *)
  (* <Input>                                                               *)
  (*    The address of the FreeType object which is under finalization.    *)
  (*    Its client data is accessed through its `generic' field.           *)
  (*                                                                       *)
  FT_Generic_Finalizer = procedure(AnObject : pointer ); cdecl;


  (*************************************************************************)
  (*                                                                       *)
  (* <Struct>                                                              *)
  (*    FT_Generic                                                         *)
  (*                                                                       *)
  (* <Description>                                                         *)
  (*    Client applications often need to associate their own data to a    *)
  (*    variety of FreeType core objects.  For example, a text layout API  *)
  (*    might want to associate a glyph cache to a given size object.      *)
  (*                                                                       *)
  (*    Most FreeType object contains a `generic' field, of type           *)
  (*    FT_Generic, which usage is left to client applications and font    *)
  (*    servers.                                                           *)
  (*                                                                       *)
  (*    It can be used to store a pointer to client-specific data, as well *)
  (*    as the address of a `finalizer' function, which will be called by  *)
  (*    FreeType when the object is destroyed (for example, the previous   *)
  (*    client example would put the address of the glyph cache destructor *)
  (*    in the `finalizer' field).                                         *)
  (*                                                                       *)
  (* <Fields>                                                              *)
  (*    data      :: A typeless pointer to any client-specified data. This *)
  (*                 field is completely ignored by the FreeType library.  *)
  (*                                                                       *)
  (*    finalizer :: A pointer to a `generic finalizer' function, which    *)
  (*                 will be called when the object is destroyed.  If this *)
  (*                 field is set to NULL, no code will be called.         *)
  (*                                                                       *)
  FT_Generic = record
    data:      pointer;
    finalizer: FT_Generic_Finalizer;
  end;


  TByteArray = array [0 .. (MaxInt div SizeOf(byte))-1] of byte;
  PByteArray = ^TByteArray;

{$ENDIF TYPE_DECL}

