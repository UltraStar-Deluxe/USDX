(***************************************************************************)
(*                                                                         *)
(*  ftconfig.h                                                             *)
(*                                                                         *)
(*    ANSI-specific configuration file (specification only).               *)
(*                                                                         *)
(*  Copyright 1996-2001, 2002, 2003, 2004, 2006, 2007 by                   *)
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

{$IFDEF TYPE_DECL}

  (*************************************************************************)
  (*                                                                       *)
  (* IntN types                                                            *)
  (*                                                                       *)
  (*   Used to guarantee the size of some specific integers.               *)
  (*                                                                       *)
  FT_Int16   = cint16;
  FT_UInt16  = cuint16;
  FT_Int32   = cint32;
  FT_UInt32  = cuint32;

{$ENDIF TYPE_DECL}

