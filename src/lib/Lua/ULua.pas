unit ULua;

(*
 * A complete Pascal wrapper for Lua DLL module.
 * Version 5.1 or 5.2
 *
 * Created by Geo Massar, 2006
 * Distributed as free/open source.
 *)

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$DEFINE HaveCONFIG}

{$IFDEF HaveCONFIG}
uses
  UConfig;
{$ELSE}
const
  {$IFNDEF lua_VERSION_MAJOR}
  LUA_VERSION_MAJOR = '5';
  {$ENDIF}
  {$IFNDEF lua_VERSION_MINOR}
  LUA_VERSION_MINOR = '1';
  {$ENDIF}
  {$IFNDEF lua_VERSION_RELEASE}
  LUA_VERSION_RELEASE = '4';
  {$ENDIF}
  LUA_VERSION_INT = 1000000*(ord(LUA_VERSION_MAJOR) - ord('0')) + 1000*(ord(LUA_VERSION_MINOR) - ord('0')) + lua_VERSION_RELEASE;
{$ENDIF}

const
  LUA_VERSION_NUM =     100*(ord(LUA_VERSION_MAJOR) - ord('0')) +       ord(LUA_VERSION_MINOR) - ord('0');

{$IFDEF WIN32}
  LUA_LIB_NAME = 'lua' + LUA_VERSION_MAJOR + '.' + lua_VERSION_MINOR + '.dll';
{$ENDIF}
{$IFDEF UNIX}
  {$IFDEF DARWIN}
    LUA_LIB_NAME = 'liblua.dylib';
    {$linklib liblua}
  {$ELSE}
    LUA_LIB_NAME = lua_lib_name;
  {$ENDIF}
{$ENDIF}

type
  size_t   = Cardinal;
  Psize_t  = ^size_t;
  PPointer = ^Pointer;
  ptrdiff_t = LongInt;

  lua_State = record end;
  Plua_State = ^lua_State;

(* formats for Lua numbers *)
{$IFNDEF LUA_NUMBER_SCAN}
const
  LUA_NUMBER_SCAN = '%lf';
{$ENDIF}

{$IFNDEF LUA_NUMBER_FMT}
const
  LUA_NUMBER_FMT = '%.14g';
{$ENDIF}

(*****************************************************************************)
(*                               luaconfig.h                                 *)
(*****************************************************************************)

(*
** $Id: luaconf.h,v 1.81 2006/02/10 17:44:06 roberto Exp $
** Configuration file for Lua
** See Copyright Notice in lua.h
*)

type
(*
** ==================================================================
@@ LUA_NUMBER is the type of numbers in Lua.
** CHANGE the following definitions only if you want to build Lua
** with a number type different from double. You may also need to
** change lua_number2int & lua_number2integer.
** ===================================================================
*)
  LUA_NUMBER_  = type Double;            // ending underscore is needed in Pascal

(*
@@ LUA_INTEGER is the integral type used by lua_pushinteger/lua_tointeger.
** CHANGE that if ptrdiff_t is not adequate on your machine. (On most
** machines, ptrdiff_t gives a good choice between int or long.)
*)
{$IF LUA_INTEGER_BITS = 32}
  LUA_INTEGER_  = type Int32;
{$ELSEIF LUA_INTEGER_BITS = 64}
  LUA_INTEGER_  = type Int64;
{$IFEND}

{$IF LUA_VERSION_NUM >= 502}
(*
@@ LUA_UNSIGNED is the integral type used by lua_pushunsigned/lua_tounsigned.
** It must have at least 32 bits.
*)
{$IF LUA_INTEGER_BITS = 32}
  LUA_UNSIGNED_ = type UInt32;
{$ELSEIF LUA_INTEGER_BITS = 64}
  LUA_UNSIGNED_ = type UInt64;
{$IFEND}
{$IFEND}
{$IF LUA_VERSION_NUM >= 503}
(*
@@ LUA_KCONTEXT is the type of the context ('ctx') for continuation
** functions.  It must be a numerical type; Lua will use 'intptr_t' if
** available, otherwise it will use 'ptrdiff_t' (the nearest thing to
** 'intptr_t' in C89)
*)
  LUA_KCONTEXT_ = type ptrdiff_t;
{$IFEND}

(*
@@ LUA_IDSIZE gives the maximum size for the description of the source
@* of a function in debug information.
** CHANGE it if you want a different size.
*)
const
  LUA_IDSIZE = 60;

(*
@@ LUAL_BUFFERSIZE is the buffer size used by the lauxlib buffer system.
*)
const
  LUAL_BUFFERSIZE = 1024;

{$IF LUA_VERSION_NUM = 501}
(*
@@ LUA_PROMPT is the default prompt used by stand-alone Lua.
@@ LUA_PROMPT2 is the default continuation prompt used by stand-alone Lua.
** CHANGE them if you want different prompts. (You can also change the
** prompts dynamically, assigning to globals _PROMPT/_PROMPT2.)
*)
const
  LUA_PROMPT  = '> ';
  LUA_PROMPT2 = '>> ';

(*
@@ lua_readline defines how to show a prompt and then read a line from
@* the standard input.
@@ lua_saveline defines how to "save" a read line in a "history".
@@ lua_freeline defines how to free a line read by lua_readline.
** CHANGE them if you want to improve this functionality (e.g., by using
** GNU readline and history facilities).
*)
function  lua_readline(L : Plua_State; var b : PChar; p : PChar): Boolean;
procedure lua_saveline(L : Plua_State; idx : Integer);
procedure lua_freeline(L : Plua_State; b : PChar);

(*
@@ lua_stdin_is_tty detects whether the standard input is a 'tty' (that
@* is, whether we're running lua interactively).
** CHANGE it if you have a better definition for non-POSIX/non-Windows
** systems.
*/
#include <io.h>
#include <stdio.h>
#define lua_stdin_is_tty()	_isatty(_fileno(stdin))
*)
const
  lua_stdin_is_tty = TRUE;
{$IFEND}

(*
@@ LUAI_BITSINT defines the number of bits in an int.
** CHANGE here if Lua cannot automatically detect the number of bits of
** your machine. Probably you do not need to change this.
*)
(* avoid overflows in comparison *)
{$IF MaxInt < 32780}
  LUAI_BITSINT = 16;
{$ELSEIF MaxInt > 2147483640}
(* int has at least 32 bits *)
  LUAI_BITSINT = 32;
{$ELSE}
  {$INFO You must define LUA_BITSINT with number of bits in an integer}
{$ENDIF}

(*
@@ LUA_INT32 is an signed integer with exactly 32 bits.
@@ LUAI_UMEM is an unsigned integer big enough to count the total
@* memory used by Lua.
@@ LUAI_MEM is a signed integer big enough to count the total memory
@* used by Lua.
** CHANGE here if for some weird reason the default definitions are not
** good enough for your machine. Probably you do not need to change
** this.
*)
type
{$IF LUAI_BITSINT >= 32}
  LUA_INT32 = Integer;
  LUAI_UMEM = Cardinal;
  LUAI_MEM  = ptrdiff_t;
{$ELSE}
(* 16-bit ints *)
  LUA_INT32 = long;
  LUAI_UMEM = unsigned long;
  LUAI_MEM  = long;
{$ENDIF}

(*
@@ LUAI_MAXSTACK limits the size of the Lua stack.
** CHANGE it if you need a different limit. This limit is arbitrary;
** its only purpose is to stop Lua to consume unlimited stack
** space (and to reserve some numbers for pseudo-indices).
*)
const
{$IF LUAI_BITSINT >= 32}
  LUAI_MAXSTACK = 1000000;
{$ELSE}
  LUAI_MAXSTACK = 15000;
{$ENDIF}

(* reserve some space for error handling *)
  LUAI_FIRSTPSEUDOIDX = -LUAI_MAXSTACK - 1000;

(*****************************************************************************)
(*                                  lua.h                                    *)
(*****************************************************************************)

(*
** $Id: lua.h,v 1.216 2006/01/10 12:50:13 roberto Exp $
** Lua - An Extensible Extension Language
** Lua.org, PUC-Rio, Brazil (http://www.lua.org)
** See Copyright Notice at the end of this file
*)

const
  LUA_VERSION     = 'Lua ' + LUA_VERSION_MAJOR + '.' + LUA_VERSION_MINOR;
  LUA_RELEASE     = LUA_VERSION + '.' + LUA_VERSION_RELEASE;
  LUA_COPYRIGHT   = 'Copyright (C) 1994-2006 Tecgraf, PUC-Rio';
  LUA_AUTHORS     = 'R. Ierusalimschy, L. H. de Figueiredo & W. Celes';

(* mark for precompiled code ('<esc>Lua') *)
  LUA_SIGNATURE = #27'Lua';

(* option for multiple returns in `lua_pcall' and `lua_call' *)
  LUA_MULTRET = -1;

(*
** pseudo-indices
*)
{$IF LUA_VERSION_NUM = 501}
  LUA_REGISTRYINDEX = -10000;
  LUA_ENVIRONINDEX  = -10001;
  LUA_GLOBALSINDEX  = -10002;
{$ELSEIF LUA_VERSION_NUM >= 502}
  LUA_REGISTRYINDEX = LUAI_FIRSTPSEUDOIDX;
{$IFEND}

function lua_upvalueindex(idx : Integer) : Integer;   // a marco

const
  (* thread status; 0 is OK *)
{$IF LUA_VERSION_NUM >= 502}
  LUA_OK        = 0;
{$IFEND}
  LUA_YIELD_    = 1;     // Note: the ending underscore is needed in Pascal
  LUA_ERRRUN    = 2;
  LUA_ERRSYNTAX = 3;
  LUA_ERRMEM    = 4;
{$IF LUA_VERSION_NUM = 501}
  LUA_ERRERR    = 5;
{$ELSEIF LUA_VERSION_NUM >= 502}
  LUA_ERRGCMM	= 5;
  LUA_ERRERR    = 6;
{$IFEND}

type
  lua_CFunction = function(L : Plua_State) : Integer; cdecl;
  {$IF LUA_VERSION_NUM >= 503}
  lua_KFunction = function(L : Plua_State; status : Integer; ctx : LUA_KCONTEXT_) : Integer; cdecl;
  {$IFEND}
(*
** functions that read/write blocks when loading/dumping Lua chunks
*)
  lua_Reader = function (L : Plua_State; ud : Pointer;
                         sz : Psize_t) : PChar; cdecl;
  lua_Writer = function (L : Plua_State; const p : Pointer; sz : size_t;
                         ud : Pointer) : Integer; cdecl;

(*
** prototype for memory-allocation functions
*)
  lua_Alloc = function (ud, ptr : Pointer;
                        osize, nsize : size_t) : Pointer; cdecl;

const
(*
** basic types
*)
  LUA_TNONE          = -1;

  LUA_TNIL           = 0;
  LUA_TBOOLEAN       = 1;
  LUA_TLIGHTUSERDATA = 2;
  LUA_TNUMBER        = 3;
  LUA_TSTRING        = 4;
  LUA_TTABLE         = 5;
  LUA_TFUNCTION      = 6;
  LUA_TUSERDATA	     = 7;
  LUA_TTHREAD        = 8;
{$IF LUA_VERSION_NUM >= 502}
  LUA_NUMTAGS        = 9;
{$IFEND}

(* minimum Lua stack available to a C function *)
  LUA_MINSTACK = 20;

{$IF LUA_VERSION_NUM >= 502}
(* predefined values in the registry *)
  LUA_RIDX_MAINTHREAD = 1;
  LUA_RIDX_GLOBALS    = 2;
  LUA_RIDX_LAST       = LUA_RIDX_GLOBALS;
{$IFEND}

type
(* type of numbers in Lua *)
  lua_Number = LUA_NUMBER_;

(* type for integer functions *)
  lua_Integer = LUA_INTEGER_;

{$IF LUA_VERSION_NUM >= 502}
(* unsigned integer type *)
  lua_unsigned = LUA_UNSIGNED_;
{$IFEND}
{$IF LUA_VERSION_NUM >= 503}
(* type for continuation-function contexts *)
  lua_KContext = LUA_KCONTEXT_;
{$IFEND}
(*
** state manipulation
*)
function  lua_newstate(f : lua_Alloc; ud : Pointer) : Plua_State;
  cdecl; external LUA_LIB_NAME;
procedure lua_close(L : Plua_State);
  cdecl; external LUA_LIB_NAME;
function  lua_newthread(L : Plua_State) : Plua_State;
  cdecl; external LUA_LIB_NAME;

function  lua_atpanic(L : Plua_State; panicf : lua_CFunction) : lua_CFunction;
  cdecl; external LUA_LIB_NAME;

{$IF LUA_VERSION_NUM >= 502}
// Already defined as a constant.
//function  lua_version(L : Plua_State) : Plua_Number;
//  cdecl; external LUA_LIB_NAME;
{$IFEND}

(*
** basic stack manipulation
*)
{$IF LUA_VERSION_NUM >= 502}
function  lua_absindex(L : Plua_State; idx : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
{$IFEND}
function  lua_gettop(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;
procedure lua_settop(L : Plua_State; idx : Integer);
  cdecl; external LUA_LIB_NAME;
procedure lua_pushvalue(L : Plua_State; idx : Integer);
  cdecl; external LUA_LIB_NAME;
procedure lua_remove(L : Plua_State; idx : Integer);
{$IF LUA_VERSION_NUM < 503}
  cdecl; external LUA_LIB_NAME;
{$IFEND}
procedure lua_insert(L : Plua_State; idx : Integer);
{$IF LUA_VERSION_NUM >= 503}
procedure lua_rotate(L : Plua_State; idx : Integer; n : Integer);
{$IFEND}
  cdecl; external LUA_LIB_NAME;
procedure lua_replace(L : Plua_State; idx : Integer);
{$IF LUA_VERSION_NUM < 503}
  cdecl; external LUA_LIB_NAME;
{$IFEND}
{$IF LUA_VERSION_NUM >= 502}
procedure lua_copy(L : Plua_State; fromidx, toidx : Integer);
  cdecl; external LUA_LIB_NAME;
{$IFEND}
function  lua_checkstack(L : Plua_State; sz : Integer) : LongBool;
  cdecl; external LUA_LIB_NAME;

procedure lua_xmove(src, dest : Plua_State; n : Integer);
  cdecl; external LUA_LIB_NAME;


(*
** access functions (stack -> C)
*)
function lua_isnumber(L : Plua_State; idx : Integer) : LongBool;
  cdecl; external LUA_LIB_NAME;
function lua_isstring(L : Plua_State; idx : Integer) : LongBool;
  cdecl; external LUA_LIB_NAME;
function lua_iscfunction(L : Plua_State; idx : Integer) : LongBool;
  cdecl; external LUA_LIB_NAME;
{$IF LUA_VERSION_NUM >= 503}
function lua_isinteger(L : Plua_State; idx : Integer) : LongBool;
  cdecl; external LUA_LIB_NAME;
{$IFEND}
function lua_isuserdata(L : Plua_State; idx : Integer) : LongBool;
  cdecl; external LUA_LIB_NAME;
function lua_type(L : Plua_State; idx : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
function lua_typename(L : Plua_State; tp : Integer) : PChar;
  cdecl; external LUA_LIB_NAME;

{$IF LUA_VERSION_NUM = 501}
function lua_equal(L : Plua_State; idx1, idx2 : Integer) : LongBool;
  cdecl; external LUA_LIB_NAME;
function lua_rawequal(L : Plua_State; idx1, idx2 : Integer) : LongBool;
  cdecl; external LUA_LIB_NAME;
function lua_lessthan(L : Plua_State; idx1, idx2 : Integer) : LongBool;
  cdecl; external LUA_LIB_NAME;
function lua_tonumber(L : Plua_State; idx : Integer) : lua_Number;
  cdecl; external LUA_LIB_NAME;
function lua_tointeger(L : Plua_State; idx : Integer) : lua_Integer;
  cdecl; external LUA_LIB_NAME;
{$ELSEIF LUA_VERSION_NUM >= 502}
function lua_tonumberx(L : Plua_State; idx : Integer; isnum: PInteger) : lua_Number;
  cdecl; external LUA_LIB_NAME;
function lua_tointegerx(L : Plua_State; idx : Integer; isnum: PInteger) : lua_Integer;
  cdecl; external LUA_LIB_NAME;
function lua_tounsignedx(L : Plua_State; idx : Integer; isnum : PInteger) : lua_Unsigned;
{$IF LUA_VERSION_NUM < 503}
  cdecl; external LUA_LIB_NAME;
{$IFEND}
{$IFEND}
function lua_toboolean(L : Plua_State; idx : Integer) : LongBool;
  cdecl; external LUA_LIB_NAME;
function lua_tolstring(L : Plua_State; idx : Integer;
                       len : Psize_t) : PChar;
  cdecl; external LUA_LIB_NAME;
{$IF LUA_VERSION_NUM = 501}
function lua_objlen(L : Plua_State; idx : Integer) : size_t;
  cdecl; external LUA_LIB_NAME;
{$ELSEIF LUA_VERSION_NUM >= 502}
function lua_objlen(L : Plua_State; idx : Integer) : size_t;
function lua_rawlen(L : Plua_State; idx : Integer) : size_t;
  cdecl; external LUA_LIB_NAME;
{$IFEND}
function lua_tocfunction(L : Plua_State; idx : Integer) : lua_CFunction;
  cdecl; external LUA_LIB_NAME;
function lua_touserdata(L : Plua_State; idx : Integer) : Pointer;
  cdecl; external LUA_LIB_NAME;
function lua_tothread(L : Plua_State; idx : Integer) : Plua_State;
  cdecl; external LUA_LIB_NAME;
function lua_topointer(L : Plua_State; idx : Integer) : Pointer;
  cdecl; external LUA_LIB_NAME;

{$IF LUA_VERSION_NUM >= 502}
{$IF LUA_VERSION_NUM >= 503}
(*
 ** Comparison and arithmetic functions
 *)
const
  LUA_OPADD    = 0;       (* ORDER TM, ORDER OP *)
  LUA_OPSUB   = 1;
  LUA_OPMUL   = 2;
  LUA_OPMOD   = 3;
  LUA_OPPOW   = 4;
  LUA_OPDIV   =	5;
  LUA_OPIDIV  = 6;
  LUA_OPBAND  = 7;
  LUA_OPBOR   = 8;
  LUA_OPBXOR  = 9;
  LUA_OPSHL   = 10;
  LUA_OPSHR   = 11;
  LUA_OPUNM   = 12;
  LUA_OPBNOT  = 13;
{$ELSE}
(*
 ** Comparison and arithmetic functions
 *)
const
  LUA_OPADD = 0;       (* ORDER TM *)
  LUA_OPSUB = 1;
  LUA_OPMUL = 2;
  LUA_OPDIV = 3;
  LUA_OPMOD = 4;
  LUA_OPPOW = 5;
  LUA_OPUNM = 6;
{$IFEND}

procedure lua_arith(L : Plua_State; op : Integer);
  cdecl; external LUA_LIB_NAME;

const
  LUA_OPEQ = 0;
  LUA_OPLT = 1;
  LUA_OPLE = 2;

function lua_rawequal (L : Plua_State; idx1 : Integer; idx2 : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
function lua_compare  (L : Plua_State; idx1 : Integer; idx2 : Integer; op : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
{$IFEND}

(*
** push functions (C -> stack)
*)
procedure lua_pushnil(L : Plua_State);
  cdecl; external LUA_LIB_NAME;
procedure lua_pushnumber(L : Plua_State; n : lua_Number);
  cdecl; external LUA_LIB_NAME;
procedure lua_pushinteger(L : Plua_State; n : lua_Integer);
  cdecl; external LUA_LIB_NAME;
{$IF LUA_VERSION_NUM = 501}
procedure lua_pushlstring(L : Plua_State; const s : PChar; ls : size_t);
  cdecl; external LUA_LIB_NAME;
procedure lua_pushstring(L : Plua_State; const s : PChar);
  cdecl; external LUA_LIB_NAME;
{$ELSEIF LUA_VERSION_NUM >= 502}
procedure lua_pushunsigned(L : Plua_State; n : lua_Unsigned);
{$IF LUA_VERSION_NUM < 503}
  cdecl; external LUA_LIB_NAME;
{$IFEND}
procedure lua_pushlstring(L : Plua_State; const s : PChar; ls : size_t);
  cdecl; external LUA_LIB_NAME;
procedure lua_pushstring(L : Plua_State; const s : PChar);
  cdecl; external LUA_LIB_NAME;
{$IFEND}
function  lua_pushvfstring(L : Plua_State; const fmt : PChar; argp : Pointer) : PChar;
  cdecl; external LUA_LIB_NAME;
function  lua_pushfstring(L : Plua_State; const fmt : PChar) : PChar; varargs;
  cdecl; external LUA_LIB_NAME;
procedure lua_pushcclosure(L : Plua_State; fn : lua_CFunction; n : Integer);
  cdecl; external LUA_LIB_NAME;
procedure lua_pushboolean(L : Plua_State; b : LongBool);
  cdecl; external LUA_LIB_NAME;
procedure lua_pushlightuserdata(L : Plua_State; p : Pointer);
  cdecl; external LUA_LIB_NAME;
function  lua_pushthread(L : Plua_state) : Cardinal;
  cdecl; external LUA_LIB_NAME;

(*
** get functions (Lua -> stack)
*)
{$IF LUA_VERSION_NUM >= 503}
function lua_getglobal(L : Plua_State; name: PChar) : Integer;
  cdecl; external LUA_LIB_NAME;
function lua_gettable(L : Plua_State; idx : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
function lua_getfield(L : Plua_State; idx : Integer; k : PChar) : Integer;
  cdecl; external LUA_LIB_NAME;
function lua_geti(L : Plua_State; idx : Integer; n : lua_Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
function lua_rawget(L : Plua_State; idx : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
function lua_rawgeti(L : Plua_State; idx : Integer; n : lua_Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
function lua_rawgetp(L : Plua_State; idx : Integer; p : Pointer) : Integer;
  cdecl; external LUA_LIB_NAME;

procedure lua_createtable(L : Plua_State; narr, nrec : Integer);
  cdecl; external LUA_LIB_NAME;
function  lua_newuserdata(L : Plua_State; sz : size_t) : Pointer;
  cdecl; external LUA_LIB_NAME;
function  lua_getmetatable(L : Plua_State; objindex : Integer) : LongBool;
  cdecl; external LUA_LIB_NAME;
function lua_getuservalue(L : Plua_State; idx : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
{$ELSE}
{$IF LUA_VERSION_NUM >= 502}
procedure lua_getglobal(L : Plua_State; var_: PChar);
  cdecl; external LUA_LIB_NAME;
{$IFEND}
procedure lua_gettable(L : Plua_State; idx : Integer);
  cdecl; external LUA_LIB_NAME;
procedure lua_getfield(L : Plua_State; idx : Integer; k : PChar);
  cdecl; external LUA_LIB_NAME;
procedure lua_rawget(L : Plua_State; idx : Integer);
  cdecl; external LUA_LIB_NAME;
procedure lua_rawgeti(L : Plua_State; idx, n : Integer);
  cdecl; external LUA_LIB_NAME;
{$IF LUA_VERSION_NUM >= 502}
procedure lua_rawgetp(L : Plua_State; idx : Integer; p : Pointer);
  cdecl; external LUA_LIB_NAME;
{$IFEND}
procedure lua_createtable(L : Plua_State; narr, nrec : Integer);
  cdecl; external LUA_LIB_NAME;
function  lua_newuserdata(L : Plua_State; sz : size_t) : Pointer;
  cdecl; external LUA_LIB_NAME;
function  lua_getmetatable(L : Plua_State; objindex : Integer) : LongBool;
  cdecl; external LUA_LIB_NAME;
{$IF LUA_VERSION_NUM = 501}
procedure lua_getfenv(L : Plua_State; idx : Integer);
  cdecl; external LUA_LIB_NAME;
{$ELSEIF LUA_VERSION_NUM >= 502}
procedure lua_getuservalue(L : Plua_State; idx : Integer);
  cdecl; external LUA_LIB_NAME;
{$IFEND}
{$IFEND}
(*
** set functions (stack -> Lua)
*)
{$IF LUA_VERSION_NUM >= 502}
procedure lua_setglobal(L : Plua_State; var_: PChar);
  cdecl; external LUA_LIB_NAME;
{$IFEND}
procedure lua_settable(L : Plua_State; idx : Integer);
  cdecl; external LUA_LIB_NAME;
procedure lua_setfield(L : Plua_State; idx : Integer; const k : PChar);
  cdecl; external LUA_LIB_NAME;
{$IF LUA_VERSION_NUM >= 503}
procedure lua_rawset(L : Plua_State; idx : Integer);
  cdecl; external LUA_LIB_NAME;
procedure lua_seti(L : Plua_State; idx : Integer; n: lua_Integer);
  cdecl; external LUA_LIB_NAME;
{$ELSE}
procedure lua_rawseti(L : Plua_State; idx , n: Integer);
  cdecl; external LUA_LIB_NAME;
{$IFEND}
{$IF LUA_VERSION_NUM >= 502}
procedure lua_rawsetp(L : Plua_State; idx : Integer; p : Pointer);
  cdecl; external LUA_LIB_NAME;
{$IFEND}
function lua_setmetatable(L : Plua_State; objindex : Integer): LongBool;
  cdecl; external LUA_LIB_NAME;
{$IF LUA_VERSION_NUM = 501}
function lua_setfenv(L : Plua_State; idx : Integer): LongBool;
  cdecl; external LUA_LIB_NAME;
{$ELSEIF LUA_VERSION_NUM >= 502}
procedure lua_setuservalue(L : Plua_State; idx : Integer);
  cdecl; external LUA_LIB_NAME;
{$IFEND}

(*
** 'load' and 'call' functions (load and run Lua code)
*)
{$IF LUA_VERSION_NUM = 501}
procedure lua_call(L : Plua_State; nargs, nresults : Integer);
  cdecl; external LUA_LIB_NAME;
function  lua_pcall(L : Plua_State; nargs, nresults, errfunc : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
function  lua_cpcall(L : Plua_State; func : lua_CFunction; ud : Pointer) : Integer;
  cdecl; external LUA_LIB_NAME;
function  lua_load(L : Plua_State; reader : lua_Reader; dt : Pointer; const chunkname : PChar) : Integer;
  cdecl; external LUA_LIB_NAME;
{$ELSEIF LUA_VERSION_NUM = 502}
procedure lua_callk(L : Plua_State; nargs, nresults, ctx : Integer; k : lua_CFunction);
  cdecl; external LUA_LIB_NAME;
procedure lua_call(L : Plua_State; nargs, nresults : Integer);

function  lua_getctx(L : Plua_State; ctx : PInteger) : Integer;
  cdecl; external LUA_LIB_NAME;

function  lua_pcallk(L : Plua_State; nargs, nresults, errfunc : Integer;
                     ctx : Integer; k : lua_CFunction) : Integer;
  cdecl; external LUA_LIB_NAME;
function  lua_pcall(L : Plua_State; nargs, nresults, errfunc : Integer) : Integer;

function  lua_load(L : Plua_State; reader : lua_Reader; dt : Pointer; 
                   const chunkname : PChar;
		   const mode : PChar) : Integer;
  cdecl; external LUA_LIB_NAME;
{$ELSEIF LUA_VERSION_NUM = 503}
procedure lua_callk(L : Plua_State; nargs, nresults : Integer;
 ctx : Lua_KContext; k : lua_CFunction);
  cdecl; external LUA_LIB_NAME;
procedure lua_call(L : Plua_State; nargs, nresults : Integer);

function  lua_pcallk(L : Plua_State; nargs, nresults, errfunc : Integer;
                     ctx : lua_KContext; k : lua_KFunction) : Integer;
  cdecl; external LUA_LIB_NAME;
function  lua_pcall(L : Plua_State; nargs, nresults, errfunc : Integer) : Integer;

function  lua_load(L : Plua_State; reader : lua_Reader; dt : Pointer; 
                   const chunkname : PChar; const mode : PChar) : Integer;
  cdecl; external LUA_LIB_NAME;
{$IFEND}

{$IF LUA_VERSION_NUM >= 503}
function lua_dump(L : Plua_State; writer : lua_Writer; data: Pointer; strip : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
{$ELSE}
function lua_dump(L : Plua_State; writer : lua_Writer; data: Pointer) : Integer;
  cdecl; external LUA_LIB_NAME;
{$IFEND}

(*
** coroutine functions
*)
{$IF LUA_VERSION_NUM = 501}
function lua_yield(L : Plua_State; nresults : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
{$ELSEIF LUA_VERSION_NUM >= 502}
{$IF LUA_VERSION_NUM >= 503}
function lua_yieldk(L : Plua_State; nresults, ctx : lua_KContext;
                    k : lua_KFunction) : Integer;
  cdecl; external LUA_LIB_NAME;
function lua_isyieldable(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;
{$ELSE}
function lua_yieldk(L : Plua_State; nresults, ctx : Integer;
                    k : lua_CFunction) : Integer;
  cdecl; external LUA_LIB_NAME;
{$IFEND}
function lua_yield(L : Plua_State; n : Integer) : Integer;
{$IFEND}
function lua_resume(L : Plua_State; narg : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
function lua_status(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;

(*
** garbage-collection functions and options
*)
const
  LUA_GCSTOP       = 0;
  LUA_GCRESTART    = 1;
  LUA_GCCOLLECT    = 2;
  LUA_GCCOUNT      = 3;
  LUA_GCCOUNTB	   = 4;
  LUA_GCSTEP       = 5;
  LUA_GCSETPAUSE   = 6;
  LUA_GCSETSTEPMUL = 7;
{$IF LUA_VERSION_NUM >= 503}
  LUA_GCISRUNNING   = 9;
{$ELSEIF LUA_VERSION_NUM >= 502}
  LUA_GCSETMAJORINC = 8;
  LUA_GCISRUNNING   = 9;
  LUA_GCGEN         = 10;
  LUA_GCINC         = 11;
{$IFEND}

function lua_gc(L : Plua_State; what, data : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;

(*
** miscellaneous functions
*)
function lua_error(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;

function lua_next(L : Plua_State; idx : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;

procedure lua_concat(L : Plua_State; n : Integer);
  cdecl; external LUA_LIB_NAME;

{$IF LUA_VERSION_NUM >= 502}
procedure lua_len(L : Plua_State; idx : Integer);
  cdecl; external LUA_LIB_NAME;
{$IFEND}

{$IF LUA_VERSION_NUM >= 503}
function lua_stringtonumber(L : Plua_State; s : {const} PChar) : size_t;
  cdecl; external LUA_LIB_NAME;
{$IFEND}

function  lua_getallocf(L : Plua_State; ud : PPointer) : lua_Alloc;
  cdecl; external LUA_LIB_NAME;
procedure lua_setallocf(L : Plua_State; f : lua_Alloc; ud : Pointer);
  cdecl; external LUA_LIB_NAME;

(*
** ===============================================================
** some useful macros
** ===============================================================
*)
{$IF LUA_VERSION_NUM >= 502}
function  lua_tonumber(L : Plua_State; i : Integer): lua_Number;
function  lua_tointeger(L : Plua_State; i : Integer): lua_Integer;
function  lua_tounsigned(L : Plua_State; i : Integer): lua_Unsigned;
{$IFEND}
procedure lua_pop(L : Plua_State; n : Integer);

procedure lua_newtable(L : Plua_State);

procedure lua_register(L : Plua_State; n : PChar; f : lua_CFunction);

procedure lua_pushcfunction(L : Plua_State; f : lua_CFunction);

{$IF LUA_VERSION_NUM = 501}
function  lua_strlen(L : Plua_State; idx : Integer) : Integer;
{$IFEND}

function lua_isfunction(L : Plua_State; n : Integer) : Boolean;
function lua_istable(L : Plua_State; n : Integer) : Boolean;
function lua_islightuserdata(L : Plua_State; n : Integer) : Boolean;
function lua_isnil(L : Plua_State; n : Integer) : Boolean;
function lua_isboolean(L : Plua_State; n : Integer) : Boolean;
function lua_isthread(L : Plua_State; n : Integer) : Boolean;
function lua_isnone(L : Plua_State; n : Integer) : Boolean;
function lua_isnoneornil(L : Plua_State; n : Integer) : Boolean;

procedure lua_pushliteral(L : Plua_State; s : PChar);

{$IF LUA_VERSION_NUM = 501}
procedure lua_setglobal(L : Plua_State; s : PChar);
procedure lua_getglobal(L : Plua_State; s : PChar);
{$IFEND}

function lua_tostring(L : Plua_State; idx : Integer) : PChar;


{$IF LUA_VERSION_NUM = 501}
(*
** compatibility macros and functions
*)
function lua_open : Plua_State;

procedure lua_getregistry(L : Plua_State);

function lua_getgccount(L : Plua_State) : Integer;

type
  lua_Chuckreader = lua_Reader;
  lua_Chuckwriter = lua_Writer;

(* hack *)
procedure lua_setlevel(from : Plua_State; to_ : Plua_State);
  cdecl; external LUA_LIB_NAME;

{$IFEND}

(* ====================================================================== *)

(*
** ======================================================================
** Debug API
** =======================================================================
*)

(*
** Event codes
*)
const
  LUA_HOOKCALL    = 0;
  LUA_HOOKRET     = 1;
  LUA_HOOKLINE    = 2;
  LUA_HOOKCOUNT   = 3;
{$IF LUA_VERSION_NUM = 501}
  LUA_HOOKTAILRET = 4;
{$ELSEIF LUA_VERSION_NUM >= 502}
  LUA_HOOKTAILCALL = 4;
{$IFEND}


(*
** Event masks
*)
  LUA_MASKCALL  = 1 shl LUA_HOOKCALL;
  LUA_MASKRET   = 1 shl LUA_HOOKRET;
  LUA_MASKLINE  = 1 shl LUA_HOOKLINE;
  LUA_MASKCOUNT = 1 shl LUA_HOOKCOUNT;

type
  TCallInfo = record // dummy for the definition in lua/lstate.h
  end;
  PCallInfo = ^TCallInfo;
  
  lua_Debug = packed record
    event : Integer;
    name : PChar;          (* (n) *)
    namewhat : PChar;      (* (n) `global', `local', `field', `method' *)
    what : PChar;          (* (S) `Lua', `C', `main', `tail' *)
    source : PChar;        (* (S) *)
    currentline : Integer; (* (l) *)
{$IF LUA_VERSION_NUM = 501}
    nups : Integer;        (* (u) number of upvalues *)
    linedefined : Integer; (* (S) *)
    lastlinedefined : Integer; (* (S) *)
    short_src : array [0..LUA_IDSIZE-1] of Char; (* (S) *)
    (* private part *)
    i_ci : Integer;        (* active function *)
{$ELSEIF LUA_VERSION_NUM >= 502}
    linedefined : Integer;     (* (S) *)
    lastlinedefined : Integer; (* (S) *)
	nups : Byte;               (* (u) number of upvalues *)
	nparams : Byte;            (* (u) number of parameters *)
	isvararg : Char;           (* (u) *)
	istailcall: Char;          (* (t) *)
    short_src : array [0..LUA_IDSIZE-1] of Char; (* (S) *)
	(* private part *)
	i_ci : PCallInfo           (* active function *)
{$IFEND}
  end;
  Plua_Debug = ^lua_Debug;

(* Functions to be called by the debuger in specific events *)
  lua_Hook = procedure (L : Plua_State; ar : Plua_Debug); cdecl;

function lua_getstack(L : Plua_State; level : Integer; ar : Plua_Debug) : Integer;
  cdecl; external LUA_LIB_NAME;
function lua_getinfo(L : Plua_State; const what : PChar; ar: Plua_Debug): Integer;
  cdecl; external LUA_LIB_NAME;
function lua_getlocal(L : Plua_State; ar : Plua_Debug; n : Integer) : PChar;
  cdecl; external LUA_LIB_NAME;
function lua_setlocal(L : Plua_State; ar : Plua_Debug; n : Integer) : PChar;
  cdecl; external LUA_LIB_NAME;
function lua_getupvalue(L : Plua_State; funcindex, n : Integer) : PChar;
  cdecl; external LUA_LIB_NAME;
function lua_setupvalue(L : Plua_State; funcindex, n : Integer) : PChar;
  cdecl; external LUA_LIB_NAME;

{$IF LUA_VERSION_NUM >= 502}
function  lua_upvalueid   (L : Plua_State; fidx, n : Integer): Pointer;
  cdecl; external LUA_LIB_NAME;
procedure lua_upvaluejoin (L : Plua_State; fidx1, n1, fidx2, n2 : Integer);
  cdecl; external LUA_LIB_NAME;
{$IFEND}

{$IF LUA_VERSION_NUM >= 502}
procedure lua_sethook(L : Plua_State; func : lua_Hook; mask, count: Integer);
  cdecl; external LUA_LIB_NAME;
{$ELSE}
function lua_sethook(L : Plua_State; func : lua_Hook; mask, count: Integer): Integer;
  cdecl; external LUA_LIB_NAME;
{$IFEND}

function lua_gethook(L : Plua_State) : lua_Hook;
  cdecl; external LUA_LIB_NAME;

function lua_gethookmask(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;
function lua_gethookcount(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;

(*****************************************************************************)
(*                                  lualib.h                                 *)
(*****************************************************************************)

(*
** $Id: lualib.h,v 1.36 2005/12/27 17:12:00 roberto Exp $
** Lua standard libraries
** See Copyright Notice at the end of this file
*)

const

  LUA_COLIBNAME   = 'coroutine';
  LUA_TABLIBNAME  = 'table';
  LUA_IOLIBNAME   = 'io';
  LUA_OSLIBNAME   = 'os';
  LUA_STRLIBNAME  = 'string';
{$IF LUA_VERSION_NUM >= 502}
  LUA_BITLIBNAME  = 'bit32';
{$IFEND}
  LUA_MATHLIBNAME = 'math';
  LUA_DBLIBNAME   = 'debug';
  LUA_LOADLIBNAME = 'package';

function luaopen_base(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;

function luaopen_table(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;

function luaopen_io(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;

function luaopen_os(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;

function luaopen_string(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;

{$IF LUA_VERSION_NUM >= 502}
function luaopen_bit32(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;
{$IFEND}

function luaopen_math(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;

function luaopen_debug(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;

function luaopen_package(L : Plua_State) : Integer;
  cdecl; external LUA_LIB_NAME;

{* open all previous libraries *}
procedure luaL_openlibs(L : Plua_State);
  cdecl; external LUA_LIB_NAME;

procedure lua_assert(x : Boolean);    // a macro


(*****************************************************************************)
(*                                  lauxlib.h                                *)
(*****************************************************************************)

(*
** $Id: lauxlib.h,v 1.87 2005/12/29 15:32:11 roberto Exp $
** Auxiliary functions for building Lua libraries
** See Copyright Notice at the end of this file.
*)

{$IF LUA_VERSION_NUM = 501}
// not compatibility with the behavior of setn/getn in Lua 5.0
function  luaL_getn(L : Plua_State; idx : Integer) : Integer; deprecated;
procedure luaL_setn(L : Plua_State; i, j : Integer); deprecated;
{$IFEND}

(* extra error code for `luaL_load' *)
const
  LUA_ERRFILE = LUA_ERRERR + 1;

type
  luaL_Reg = packed record
    name : PChar;
    func : lua_CFunction;
  end;
  PluaL_Reg = ^luaL_Reg;

{$IF LUA_VERSION_NUM = 501}
procedure luaL_openlib(L : Plua_State; const libname : PChar;
                       const lr : PluaL_Reg; nup : Integer);
  cdecl; external LUA_LIB_NAME;
procedure luaL_register(L : Plua_State; const libname : PChar;
                       const lr : PluaL_Reg);
  cdecl; external LUA_LIB_NAME;
{$ELSEIF LUA_VERSION_NUM >= 502}
// Added for simplicity --KMS
procedure luaL_register(L : Plua_State; const libname : PChar;
                        const lr : PluaL_Reg);

procedure luaL_checkversion(L : Plua_State);
procedure luaL_checkversion_(L : Plua_State; ver : lua_Number);
  cdecl; external LUA_LIB_NAME;
{$IFEND}

function luaL_getmetafield(L : Plua_State; obj : Integer;
                           const e : PChar) : Integer;
  cdecl; external LUA_LIB_NAME;
function luaL_callmeta(L : Plua_State; obj : Integer;
                       const e : PChar) : Integer;
  cdecl; external LUA_LIB_NAME;
{$IF LUA_VERSION_NUM = 501}
function luaL_typerror(L : Plua_State; narg : Integer;
                       const tname : PChar) : Integer;
  cdecl; external LUA_LIB_NAME;
{$ELSEIF LUA_VERSION_NUM >= 502}
function luaL_tolstring(L : Plua_State; idx : Integer;
                        len : Psize_t) : PChar;
  cdecl; external LUA_LIB_NAME;
{$IFEND}
function luaL_argerror(L : Plua_State; numarg : Integer;
                       const extramsg : PChar) : Integer;
  cdecl; external LUA_LIB_NAME;
function luaL_checklstring(L : Plua_State; numArg : Integer;
                           ls : Psize_t) : PChar;
  cdecl; external LUA_LIB_NAME;
function luaL_optlstring(L : Plua_State; numArg : Integer;
                         const def: PChar; ls: Psize_t) : PChar;
  cdecl; external LUA_LIB_NAME;
function luaL_checknumber(L : Plua_State; numArg : Integer) : lua_Number;
  cdecl; external LUA_LIB_NAME;
function luaL_optnumber(L : Plua_State; nArg : Integer;
                        def : lua_Number) : lua_Number;
  cdecl; external LUA_LIB_NAME;

function luaL_checkinteger(L : Plua_State; numArg : Integer) : lua_Integer;
  cdecl; external LUA_LIB_NAME;
function luaL_optinteger(L : Plua_State; nArg : Integer;
                        def : lua_Integer) : lua_Integer;
  cdecl; external LUA_LIB_NAME;
{$IF LUA_VERSION_NUM >= 502}
function luaL_checkunsigned(L : Plua_State; numArg : Integer) : lua_Unsigned;
  cdecl; external LUA_LIB_NAME;
function luaL_optunsigned(L : Plua_State; numArg : Integer;
                          def : lua_Unsigned) : lua_Unsigned;
  cdecl; external LUA_LIB_NAME;
{$IFEND}

procedure luaL_checkstack(L : Plua_State; sz : Integer; const msg : PChar);
  cdecl; external LUA_LIB_NAME;
procedure luaL_checktype(L : Plua_State; narg, t : Integer);
  cdecl; external LUA_LIB_NAME;
procedure luaL_checkany(L : Plua_State; narg : Integer);
  cdecl; external LUA_LIB_NAME;

function luaL_newmetatable(L : Plua_State; const tname : PChar) : Integer;
  cdecl; external LUA_LIB_NAME;
{$IF LUA_VERSION_NUM >= 502}
procedure luaL_setmetatable(L : Plua_State; const tname : PChar);
  cdecl; external LUA_LIB_NAME;
function luaL_testudata(L : Plua_State; ud : Integer;
                        const tname : PChar) : Pointer;
  cdecl; external LUA_LIB_NAME;
{$IFEND}
function luaL_checkudata(L : Plua_State; ud : Integer;
                         const tname : PChar) : Pointer;
  cdecl; external LUA_LIB_NAME;

procedure luaL_where(L : Plua_State; lvl : Integer);
  cdecl; external LUA_LIB_NAME;
function  luaL_error(L : Plua_State; const fmt : PChar) : Integer; varargs;
  cdecl; external LUA_LIB_NAME;

function luaL_checkoption(L : Plua_State; narg : Integer; const def : PChar;
                          const lst : array of PChar) : Integer;
  cdecl; external LUA_LIB_NAME;

{$IF LUA_VERSION_NUM >= 502}
function  luaL_fileresult(L : Plua_State; stat : Integer; 
                          const fname : PChar) : Integer;
  cdecl; external LUA_LIB_NAME;
function  luaL_execresult(L : Plua_State; stat : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;

(* pre-defined references *)
const
  LUA_NOREF  = -2;
  LUA_REFNIL = -1;
{$IFEND}

function  luaL_ref(L : Plua_State; t : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
procedure luaL_unref(L : Plua_State; t, ref : Integer);
  cdecl; external LUA_LIB_NAME;

function luaL_loadfile(L : Plua_State; const filename : PChar) : Integer;
{$IF LUA_VERSION_NUM = 501}
  cdecl; external LUA_LIB_NAME;
{$ELSEIF LUA_VERSION_NUM >= 502}
function luaL_loadfilex(L: Plua_State; const filename, mode: PChar): Integer;
  cdecl; external LUA_LIB_NAME;
{$IFEND}

function luaL_loadbuffer(L : Plua_State; const buff : PChar;
                         sz : size_t; const name: PChar) : Integer;
{$IF LUA_VERSION_NUM = 501}
  cdecl; external LUA_LIB_NAME;
{$ELSEIF LUA_VERSION_NUM >= 502}
function luaL_loadbufferx(L : Plua_State; const buff : PChar;
                          sz : size_t; const name: PChar; const mode: PChar) : Integer;
  cdecl; external LUA_LIB_NAME;
{$IFEND}

function luaL_loadstring(L : Plua_State; const s : Pchar) : Integer;
  cdecl; external LUA_LIB_NAME;

function luaL_newstate : Plua_State;
  cdecl; external LUA_LIB_NAME;

{$IF LUA_VERSION_NUM >= 502}
function luaL_len(L : Plua_State; idx : Integer) : Integer;
  cdecl; external LUA_LIB_NAME;
{$IFEND}

function luaL_gsub(L : Plua_State; const s, p, r : PChar) : PChar;
  cdecl; external LUA_LIB_NAME;

{$IF LUA_VERSION_NUM = 501}
function luaL_findtable(L : Plua_State; idx : Integer;
                        const fname : PChar; szhint : Integer) : PChar;
  cdecl; external LUA_LIB_NAME;
procedure luaL_setfuncs (L : Plua_State; const lr : PluaL_Reg; nup : Integer);
{$ELSEIF LUA_VERSION_NUM >= 502}
procedure luaL_setfuncs(L : Plua_State; const lr : PluaL_Reg;
                        nup : Integer); overload;
  cdecl; external LUA_LIB_NAME;
procedure luaL_setfuncs(L : Plua_State; const lr : array of luaL_Reg;
                        nup : Integer); overload;
function luaL_getsubtable(L : Plua_State; idx : Integer;
                          const fname : PChar) : Integer;
  cdecl; external LUA_LIB_NAME;
procedure luaL_traceback(L : Plua_State; L1 : Plua_State;
                         const msg : PChar; level : Integer);
  cdecl; external LUA_LIB_NAME;
procedure luaL_requiref(L : Plua_State; const modname : PChar;
                        openf : lua_CFunction; glb : Integer);
  cdecl; external LUA_LIB_NAME;
{$IFEND}

(*
** ===============================================================
** some useful macros
** ===============================================================
*)

{$IF LUA_VERSION_NUM >= 502}
procedure luaL_newlibtable(L : Plua_State; lr : array of luaL_Reg); overload;
procedure luaL_newlibtable(L : Plua_State; lr : PluaL_Reg); overload;
procedure luaL_newlib(L : Plua_State; lr : array of luaL_Reg); overload;
procedure luaL_newlib(L : Plua_State; lr : PluaL_Reg); overload;
{$IFEND}
function luaL_argcheck(L : Plua_State; cond : Boolean; numarg : Integer;
                       extramsg : PChar): Integer;
function luaL_checkstring(L : Plua_State; n : Integer) : PChar;
function luaL_optstring(L : Plua_State; n : Integer; d : PChar) : PChar;
function luaL_checkint(L : Plua_State; n : Integer) : Integer;
function luaL_optint(L : Plua_State; n, d : Integer): Integer;
function luaL_checklong(L : Plua_State; n : LongInt) : LongInt;
function luaL_optlong(L : Plua_State; n : Integer; d : LongInt) : LongInt;

function luaL_typename(L : Plua_State; idx : Integer) : PChar;

function luaL_dofile(L : Plua_State; fn : PChar) : Integer;

function luaL_dostring(L : Plua_State; s : PChar) : Integer;

procedure luaL_getmetatable(L : Plua_State; n : PChar);

(* not implemented yet
#define luaL_opt(L,f,n,d) (lua_isnoneornil(L,(n)) ? (d) : f(L,(n)))
*)

(*
** ======================================================
** Generic Buffer manipulation
** =======================================================
*)

type
  luaL_Buffer = packed record
{$IF LUA_VERSION_NUM = 501}
    p : PChar;       (* current position in buffer *)
    lvl : Integer;   (* number of strings in the stack (level) *)
    L : Plua_State;
    buffer : array [0..LUAL_BUFFERSIZE-1] of Char;
{$ELSEIF LUA_VERSION_NUM >= 502}
    b     : PChar;      (* buffer address *)
    size  : size_t;     (* buffer size *)
    n     : size_t;     (* number of characters in buffer *)
    L     : Plua_State;
    initb : array [0..LUAL_BUFFERSIZE-1] of Char;  (* initial buffer *)
{$IFEND}
  end;
  PluaL_Buffer = ^luaL_Buffer;

{$IF LUA_VERSION_NUM = 501}
(* compatibility only *)
procedure luaL_addchar(B : PluaL_Buffer; c : Char);

(* compatibility only *)
procedure luaL_putchar(B : PluaL_Buffer; c : Char);
{$IFEND}

procedure luaL_addsize(B : PluaL_Buffer; n : Integer);

procedure luaL_buffinit(L : Plua_State; B : PluaL_Buffer);
  cdecl; external LUA_LIB_NAME;
function  luaL_prepbuffer(B : PluaL_Buffer) : PChar;
  cdecl; external LUA_LIB_NAME;
procedure luaL_addlstring(B : PluaL_Buffer; const s : PChar; ls : size_t);
  cdecl; external LUA_LIB_NAME;
procedure luaL_addstring(B : PluaL_Buffer; const s : PChar);
  cdecl; external LUA_LIB_NAME;
procedure luaL_addvalue(B : PluaL_Buffer);
  cdecl; external LUA_LIB_NAME;
procedure luaL_pushresult(B : PluaL_Buffer);
  cdecl; external LUA_LIB_NAME;

{$IF LUA_VERSION_NUM >= 502}
procedure luaL_pushresultsize(B : PluaL_Buffer; sz : size_t);
  cdecl; external LUA_LIB_NAME;
function  luaL_buffinitsize(L : Plua_State; B : PluaL_Buffer; sz : size_t) : PChar;
  cdecl; external LUA_LIB_NAME;
{$IFEND}

(* ====================================================== *)

{$IF LUA_VERSION_NUM = 501}
(* compatibility with ref system *)

(* pre-defined references *)
const
  LUA_NOREF  = -2;
  LUA_REFNIL = -1;

function lua_ref(L : Plua_State; lock : Boolean) : Integer;

procedure lua_unref(L : Plua_State; ref : Integer);

procedure lua_getref(L : Plua_State; ref : Integer);

{$ELSEIF LUA_VERSION_NUM >= 502}

(*
** =======================================================
** File handles for IO library
** =======================================================
*)

(*
** A file handle is a userdata with metatable 'LUA_FILEHANDLE' and
** initial structure 'luaL_Stream' (it may contain other fields
** after that initial structure).
*)

const
  LUA_FILEHANDLE  = 'FILE*';

type
  luaL_Stream = record
    f:  Pointer;           (* stream (NULL for incompletely created streams) *)
    closef: lua_CFunction; (* to close stream (NULL for closed streams) *)
  end;

{$IFEND}

(******************************************************************************)
(******************************************************************************)
(******************************************************************************)

implementation

uses
  SysUtils;

(*****************************************************************************)
(*                            luaconfig.h                                    *)
(*****************************************************************************)

function  lua_readline(L : Plua_State; var b : PChar; p : PChar): Boolean;
var
  s : AnsiString;
begin
  Write(p);                        // show prompt
  ReadLn(s);                       // get line
  b := PChar(s);                   //   and return it
  lua_readline := (b[0] <> #4);          // test for ctrl-D
end;

procedure lua_saveline(L : Plua_State; idx : Integer);
begin
end;

procedure lua_freeline(L : Plua_State; b : PChar);
begin
end;


(*****************************************************************************)
(*                                  lua.h                                    *)
(*****************************************************************************)

function lua_upvalueindex(idx : Integer) : Integer;
begin
{$IF LUA_VERSION_NUM = 501}
  lua_upvalueindex := LUA_GLOBALSINDEX - idx;
{$ELSEIF LUA_VERSION_NUM >= 502}
  lua_upvalueindex := LUA_REGISTRYINDEX - idx;
{$IFEND}
end;

{$IF LUA_VERSION_NUM >= 502}
function lua_objlen(L : Plua_State; idx : Integer) : size_t;
begin
  lua_objlen := lua_rawlen(L, idx);  
end;

procedure lua_call(L : Plua_State; nargs, nresults : Integer);
begin
  lua_callk(L, nargs, nresults, 0, NIL);
end;

function  lua_pcall(L : Plua_State; nargs, nresults, errfunc : Integer) : Integer;
begin
  lua_pcall := lua_pcallk(L, nargs, nresults, errfunc, 0, NIL);
end;

function  lua_yield(L : Plua_State; n : Integer) : Integer;
begin
  lua_yield := lua_yieldk(L, n, 0, NIL);
end;

function  lua_tonumber(L : Plua_State; i : Integer): lua_Number;
begin
  lua_tonumber := lua_tonumberx(L, i, NIL);
end;

function  lua_tointeger(L : Plua_State; i : Integer): lua_Integer;
begin
  lua_tointeger := lua_tointegerx(L, i, NIL);
end;

function  lua_tounsigned(L : Plua_State; i : Integer): lua_Unsigned;
begin
  lua_tounsigned := lua_tounsignedx(L, i, NIL);
end;
{$IFEND}

procedure lua_pop(L : Plua_State; n : Integer);
begin
  lua_settop(L, -n - 1);
end;

procedure lua_newtable(L : Plua_State);
begin
  lua_createtable(L, 0, 0);
end;

procedure lua_register(L : Plua_State; n : PChar; f : lua_CFunction);
begin
  lua_pushcfunction(L, f);
  lua_setglobal(L, n);
end;

procedure lua_pushcfunction(L : Plua_State; f : lua_CFunction);
begin
  lua_pushcclosure(L, f, 0);
end;

{$IF LUA_VERSION_NUM = 501}
function  lua_strlen(L : Plua_State; idx : Integer) : Integer;
begin
  lua_strlen := lua_objlen(L, idx);
end;
{$IFEND}

function lua_isfunction(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isfunction := lua_type(L, n) = LUA_TFUNCTION;
end;

function lua_istable(L : Plua_State; n : Integer) : Boolean;
begin
  lua_istable := lua_type(L, n) = LUA_TTABLE;
end;

function lua_islightuserdata(L : Plua_State; n : Integer) : Boolean;
begin
  lua_islightuserdata := lua_type(L, n) = LUA_TLIGHTUSERDATA;
end;

function lua_isnil(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isnil := lua_type(L, n) = LUA_TNIL;
end;

function lua_isboolean(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isboolean := lua_type(L, n) = LUA_TBOOLEAN;
end;

function lua_isthread(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isthread := lua_type(L, n) = LUA_TTHREAD;
end;

function lua_isnone(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isnone := lua_type(L, n) = LUA_TNONE;
end;

function lua_isnoneornil(L : Plua_State; n : Integer) : Boolean;
begin
  lua_isnoneornil := lua_type(L, n) <= 0;
end;

procedure lua_pushliteral(L : Plua_State; s : PChar);
begin
  lua_pushlstring(L, s, StrLen(s));
end;

{$IF LUA_VERSION_NUM = 501}
procedure lua_setglobal(L : Plua_State; s : PChar);
begin
  lua_setfield(L, LUA_GLOBALSINDEX, s);
end;

procedure lua_getglobal(L: Plua_State; s: PChar);
begin
  lua_getfield(L, LUA_GLOBALSINDEX, s);
end;
{$IFEND}

function lua_tostring(L : Plua_State; idx : Integer) : PChar;
begin
  lua_tostring := lua_tolstring(L, idx, nil);
end;

function lua_open : Plua_State;
begin
  lua_open := luaL_newstate;
end;

procedure lua_getregistry(L : Plua_State);
begin
  lua_pushvalue(L, LUA_REGISTRYINDEX);
end;

function lua_getgccount(L : Plua_State) : Integer;
begin
  lua_getgccount := lua_gc(L, LUA_GCCOUNT, 0);
end;


(*****************************************************************************)
(*                                  lualib.h                                 *)
(*****************************************************************************)

procedure lua_assert(x : Boolean);
begin
end;


(*****************************************************************************)
(*                                  lauxlib.h    n                           *)
(*****************************************************************************)

{$IF LUA_VERSION_NUM = 501}
function luaL_getn(L : Plua_State; idx : Integer) : Integer;
begin
  luaL_getn := lua_objlen(L, idx);
end;

procedure luaL_setn(L : plua_State; i, j : Integer);
begin
  (* no op *)
end;

{$ELSEIF LUA_VERSION_NUM >= 502}
procedure luaL_register(L : Plua_State; const libname : PChar;
                        const lr : PluaL_Reg);
begin
  lua_newtable(L); 
  luaL_setfuncs(L, lr, 0); 
  lua_pushvalue(L, -1); 
  lua_setglobal(L, libname); 
end;

procedure luaL_checkversion(L : Plua_State);
begin
  luaL_checkversion_(L, LUA_VERSION_NUM);
end;
{$IFEND}

{$IF LUA_VERSION_NUM >= 502}
function luaL_loadfile(L : Plua_State; const filename: PChar): Integer;
begin
   Result := luaL_loadfilex(L, filename, nil);
end;
{$IFEND}

{$IF LUA_VERSION_NUM >= 502}
function luaL_loadbuffer(L : Plua_State; const buff : PChar;
                         sz : size_t; const name: PChar) : Integer;
begin
   Result := luaL_loadbufferx(L, buff, sz, name, nil);
end;
{$IFEND}

{$IF LUA_VERSION_NUM = 501}
(*
** Adapted from Lua 5.2.0 and this page 
** http://lua-users.org/wiki/CompatibilityWithLuaFive
** It enables the replacement of luaL_register by luaL_setfuncs in ULuaCore
** As of now, it is not used.
*)
procedure luaL_setfuncs (L : Plua_State; const lr : PluaL_Reg; nup : Integer);
var
  i : integer;
begin
  luaL_checkstack(L, nup, 'too many upvalues');
  repeat                        (* fill the table with given functions *)
    for i := 1 to nup do        (* copy upvalues to the top            *)
      lua_pushvalue(L, -nup);
    lua_pushstring(L, lr.name);
    lua_pushcclosure(L, lr.func, nup);  (* closure with those upvalues *)
    lua_settable(L, -(nup + 3));
//    inc(lr);
  until (lr.name = NIL);
  lua_pop(L, nup);                      (* remove upvalues *)
end;
{$ELSEIF LUA_VERSION_NUM >= 502}
procedure luaL_setfuncs(L : Plua_State; const lr : array of luaL_Reg;
                        nup : Integer); overload;
begin
  luaL_setfuncs(L, @lr, nup);
end;
{$IFEND}

procedure luaL_newlibtable(L : Plua_State; lr : array of luaL_Reg); overload;
begin
   lua_createtable(L, 0, High(lr));
end;

procedure luaL_newlibtable(L : Plua_State; lr : PluaL_Reg); overload;
   var
      n: Integer;
begin
   n := 0;
   while lr^.name <> nil do
   begin
     inc(n);
     inc(lr);
   end;
   lua_createtable(L, 0, n);
end;

procedure luaL_newlib(L : Plua_State; lr : array of luaL_Reg); overload;
begin
   luaL_newlibtable(L, lr);
   luaL_setfuncs(L, @lr, 0);
end;

procedure luaL_newlib(L : Plua_State; lr : PluaL_Reg); overload;
begin
   luaL_newlibtable(L, lr);
   luaL_setfuncs(L, lr, 0);
end;

function luaL_argcheck(L : Plua_State; cond : Boolean; numarg : Integer;
                       extramsg : PChar): Integer;
begin
  if not cond then
    luaL_argcheck := luaL_argerror(L, numarg, extramsg)
  else
    luaL_argcheck := 0;
end;

function luaL_checkstring(L : Plua_State; n : Integer) : PChar;
begin
  luaL_checkstring := luaL_checklstring(L, n, nil);
end;

function luaL_optstring(L : Plua_State; n : Integer; d : PChar) : PChar;
begin
  luaL_optstring := luaL_optlstring(L, n, d, nil);
end;

function luaL_checkint(L : Plua_State; n : Integer) : Integer;
begin
  luaL_checkint := luaL_checkinteger(L, n);
end;

function luaL_optint(L : Plua_State; n, d : Integer): Integer;
begin
  luaL_optint := luaL_optinteger(L, n, d);
end;

function luaL_checklong(L : Plua_State; n : LongInt) : LongInt;
begin
  luaL_checklong := luaL_checkinteger(L, n);
end;

function luaL_optlong(L : Plua_State; n : Integer; d : LongInt) : LongInt;
begin
  luaL_optlong := luaL_optinteger(L, n, d);
end;

function luaL_typename(L : Plua_State; idx : Integer) : PChar;
begin
  luaL_typename := lua_typename( L, lua_type(L, idx) );
end;

function luaL_dofile(L : Plua_State; fn : PChar) : Integer;
Var
  Res : Integer;
begin
  // WC 2007\03\22 - Updated for Delphi
  Res := luaL_loadfile(L, fn);
  if Res = 0 then
    Res := lua_pcall(L, 0, LUA_MULTRET, 0);
  Result := Res;
end;

function luaL_dostring(L : Plua_State; s : PChar) : Integer;
Var
  Res : Integer;
begin
  // WC 2007\03\22 - Updated for Delphi
  Res := luaL_loadstring(L, s);
  if Res = 0 then
    Res := lua_pcall(L, 0, LUA_MULTRET, 0);
  Result := Res;
end;

procedure luaL_getmetatable(L : Plua_State; n : PChar);
begin
  lua_getfield(L, LUA_REGISTRYINDEX, n);
end;

{$IF LUA_VERSION_NUM = 501}
procedure luaL_addchar(B : PluaL_Buffer; c : Char);
begin
  if not(B^.p < B^.buffer + LUAL_BUFFERSIZE) then
    luaL_prepbuffer(B);
  B^.p^ := c;
  Inc(B^.p);
end;

procedure luaL_putchar(B : PluaL_Buffer; c : Char);
begin
  luaL_addchar(B, c);
end;
{$IFEND}

procedure luaL_addsize(B : PluaL_Buffer; n : Integer);
begin
{$IF LUA_VERSION_NUM = 501}
  Inc(B^.p, n);
{$ELSEIF LUA_VERSION_NUM >= 502}
  Inc(B^.n, n);
{$IFEND}
end;

{$IF LUA_VERSION_NUM = 501}
function lua_ref(L : Plua_State; lock : Boolean) : Integer;
begin
  if lock then
    lua_ref := luaL_ref(L, LUA_REGISTRYINDEX)
  else begin
    lua_pushstring(L, 'unlocked references are obsolete');
    lua_error(L);
    lua_ref := 0;
  end;
end;

procedure lua_unref(L : Plua_State; ref : Integer);
begin
  luaL_unref(L, LUA_REGISTRYINDEX, ref);
end;

procedure lua_getref(L : Plua_State; ref : Integer);
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, ref);
end;
{$IFEND}

{$IF LUA_VERSION_NUM >= 503}
procedure lua_insert(L : Plua_State; idx : Integer);
begin
  lua_rotate(L, (idx), 1);
end;

procedure lua_remove(L : Plua_State; idx : Integer);
begin
  lua_rotate(L, (idx), -1);
  lua_pop(L, 1);
end;

procedure lua_replace(L : Plua_State; idx : Integer);
begin
  lua_copy(L, -1, (idx));
  lua_pop(L, 1);
end;

(*
** {==============================================================
** compatibility macros for unsigned conversions
** ===============================================================
*)
procedure lua_pushunsigned(L : Plua_State; n : lua_Unsigned);
begin
  lua_pushinteger(L, lua_Integer(n));
end;

function lua_tounsignedx(L : Plua_State; idx : Integer; isnum : PInteger) : lua_Unsigned;
begin
  lua_tounsignedx := lua_tointegerx(L, idx, isnum);
end;

{$IFEND}

(******************************************************************************
* Original copyright for the lua source and headers:
*  1994-2004 Tecgraf, PUC-Rio.
*  www.lua.org.
*
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)

end.

