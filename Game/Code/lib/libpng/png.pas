(*
 * libpng pascal headers
 * Version: 1.2.12
 *)

{$IFDEF FPC}
  {$ifndef NO_SMART_LINK}
  {$smartlink on}
  {$endif}
{$ENDIF}

unit png;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKRECORDS C}
{$ENDIF}

uses
  ctypes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  baseunix,
  {$ENDIF}
  zlib;

const
{$IFDEF MSWINDOWS}
  // use libpng12-0 (Version 1.2.18), delivered wih SDL_Image
  LibPng = 'libpng12-0'; // 'libpng13';
  // matching lib version for libpng13.dll, needed for initialization
  PNG_LIBPNG_VER_STRING='1.2.12';
  // define the compiler that was used to built the DLL (necessary for jmp_buf)
  // SDL_Image was compiled with GCC
  //{$define MSVC_DLL} // MS Visual C++
  {$DEFINE GCC_DLL}  // GCC
{$ELSE}
  LibPng = 'png';
  // matching lib version for libpng, needed for initialization
  PNG_LIBPNG_VER_STRING='1.2.12';
  {$IFDEF DARWIN}
    {$linklib libpng}
  {$ENDIF}
{$ENDIF}


{$IFDEF MSWINDOWS}
const
   // JB_LEN (#elements in jmp_buf) depends on the compiler used to compile the DLL
   //   MSVC++: 16 (x86/AMD64), GCC: 52
   {$IF Defined(MSVC_DLL)}
   JB_LEN = 16;
   {$ELSEIF Defined(GCC_DLL)}
   JB_LEN = 52;
   {$ELSE}
   JB_LEN = 0;
   {$IFEND}
{$ENDIF}

type
   {$IFNDEF FPC}
   // defines for Delphi
   size_t = culong;
   {$ENDIF}

   {$ifdef MSWINDOWS}
     {$if JB_LEN > 0}
     jmp_buf = array[0..JB_LEN-1] of cint;
     // the png_struct cannot be accessed if the size of jmp_buf is unknown
     {$define UsePngStruct}
     {$ifend}
     // Do NOT use time_t on windows! It might be 32 or 64bit, depending on the compiler and system.
     // MSVS-2005 starts using 64bit for time_t on x86 by default, but GCC uses just 32bit.
     //time_t = clong;
   {$endif}

   z_stream = TZStream;

   png_uint_32 = cuint32;
   png_int_32 = cint32;
   png_uint_16 = cuint16;
   png_int_16 = cint16;
   png_byte = cuint8;
   ppng_uint_32 = ^png_uint_32;
   ppng_int_32 = ^png_int_32;
   ppng_uint_16 = ^png_uint_16;
   ppng_int_16 = ^png_int_16;
   ppng_byte = ^png_byte;
   pppng_uint_32 = ^ppng_uint_32;
   pppng_int_32 = ^ppng_int_32;
   pppng_uint_16 = ^ppng_uint_16;
   pppng_int_16 = ^ppng_int_16;
   pppng_byte = ^ppng_byte;
   png_size_t = size_t;
   png_fixed_point = png_int_32;
   ppng_fixed_point = ^png_fixed_point;
   pppng_fixed_point = ^ppng_fixed_point;
   png_voidp = pointer;
   png_bytep = Ppng_byte;
   ppng_bytep = ^png_bytep;
   png_uint_32p = Ppng_uint_32;
   png_int_32p = Ppng_int_32;
   png_uint_16p = Ppng_uint_16;
   ppng_uint_16p = ^png_uint_16p;
   png_int_16p = Ppng_int_16;
   png_const_charp = {const} Pchar;
   png_charp = Pchar;
   ppng_charp = ^png_charp;
   png_fixed_point_p = Ppng_fixed_point;
   png_FILE_p = Pointer;
   png_doublep = PCdouble;
   png_bytepp = PPpng_byte;
   png_uint_32pp = PPpng_uint_32;
   png_int_32pp = PPpng_int_32;
   png_uint_16pp = PPpng_uint_16;
   png_int_16pp = PPpng_int_16;
   png_const_charpp = {const} PPchar;
   png_charpp = PPchar;
   ppng_charpp = ^png_charpp;
   png_fixed_point_pp = PPpng_fixed_point;
   PPCdouble = ^PCdouble;
   png_doublepp = PPCdouble;
   PPPChar = ^PPChar;
   png_charppp = PPPChar;
   PCharf = PChar;
   PPCharf = ^PCharf;
   png_zcharp = PCharf;
   png_zcharpp = PPCharf;
   png_zstreamp = Pzstream;

const
  (* Maximum positive integer used in PNG is (2^31)-1 *)
  PNG_UINT_31_MAX = (png_uint_32($7fffffff));
  PNG_UINT_32_MAX = (png_uint_32(-1));
  PNG_SIZE_MAX    = (png_size_t(-1));
  {$if defined(PNG_1_0_X) or defined (PNG_1_2_X)}
  (* PNG_MAX_UINT is deprecated; use PNG_UINT_31_MAX instead. *)
  PNG_MAX_UINT = PNG_UINT_31_MAX;
  {$ifend}

  (* These describe the color_type field in png_info. *)
  (* color type masks *)
  PNG_COLOR_MASK_PALETTE  = 1;
  PNG_COLOR_MASK_COLOR    = 2;
  PNG_COLOR_MASK_ALPHA    = 4;

  (* color types.  Note that not all combinations are legal *)
  PNG_COLOR_TYPE_GRAY = 0;
  PNG_COLOR_TYPE_PALETTE    = (PNG_COLOR_MASK_COLOR or PNG_COLOR_MASK_PALETTE);
  PNG_COLOR_TYPE_RGB        = (PNG_COLOR_MASK_COLOR);
  PNG_COLOR_TYPE_RGB_ALPHA  = (PNG_COLOR_MASK_COLOR or PNG_COLOR_MASK_ALPHA);
  PNG_COLOR_TYPE_GRAY_ALPHA = (PNG_COLOR_MASK_ALPHA);
  (* aliases *)
  PNG_COLOR_TYPE_RGBA = PNG_COLOR_TYPE_RGB_ALPHA;
  PNG_COLOR_TYPE_GA   = PNG_COLOR_TYPE_GRAY_ALPHA;

  (* This is for compression type. PNG 1.0-1.2 only define the single type. *)
  PNG_COMPRESSION_TYPE_BASE = 0; (* Deflate method 8, 32K window *)
  PNG_COMPRESSION_TYPE_DEFAULT = PNG_COMPRESSION_TYPE_BASE;

  (* This is for filter type. PNG 1.0-1.2 only define the single type. *)
  PNG_FILTER_TYPE_BASE     = 0; (* Single row per-byte filtering *)
  PNG_INTRAPIXEL_DIFFERENCING = 64; (* Used only in MNG datastreams *)
  PNG_FILTER_TYPE_DEFAULT  = PNG_FILTER_TYPE_BASE;

  (* These are for the interlacing type.  These values should NOT be changed. *)
  PNG_INTERLACE_NONE      = 0; (* Non-interlaced image *)
  PNG_INTERLACE_ADAM7     = 1; (* Adam7 interlacing *)
  PNG_INTERLACE_LAST      = 2; (* Not a valid value *)

  (* These are for the oFFs chunk.  These values should NOT be changed. *)
  PNG_OFFSET_PIXEL        = 0; (* Offset in pixels *)
  PNG_OFFSET_MICROMETER   = 1; (* Offset in micrometers (1/10^6 meter) *)
  PNG_OFFSET_LAST         = 2; (* Not a valid value *)

  (* These are for the pCAL chunk.  These values should NOT be changed. *)
  PNG_EQUATION_LINEAR     = 0; (* Linear transformation *)
  PNG_EQUATION_BASE_E     = 1; (* Exponential base e transform *)
  PNG_EQUATION_ARBITRARY  = 2; (* Arbitrary base exponential transform *)
  PNG_EQUATION_HYPERBOLIC = 3; (* Hyperbolic sine transformation *)
  PNG_EQUATION_LAST       = 4; (* Not a valid value *)

  (* These are for the sCAL chunk.  These values should NOT be changed. *)
  PNG_SCALE_UNKNOWN       = 0; (* unknown unit (image scale) *)
  PNG_SCALE_METER         = 1; (* meters per pixel *)
  PNG_SCALE_RADIAN        = 2; (* radians per pixel *)
  PNG_SCALE_LAST          = 3; (* Not a valid value *)

  (* These are for the pHYs chunk.  These values should NOT be changed. *)
  PNG_RESOLUTION_UNKNOWN  = 0; (* pixels/unknown unit (aspect ratio) *)
  PNG_RESOLUTION_METER    = 1; (* pixels/meter *)
  PNG_RESOLUTION_LAST     = 2; (* Not a valid value *)

  (* These are for the sRGB chunk.  These values should NOT be changed. *)
  PNG_sRGB_INTENT_PERCEPTUAL = 0;
  PNG_sRGB_INTENT_RELATIVE   = 1;
  PNG_sRGB_INTENT_SATURATION = 2;
  PNG_sRGB_INTENT_ABSOLUTE   = 3;
  PNG_sRGB_INTENT_LAST       = 4; (* Not a valid value *)

  (* This is for text chunks *)
  PNG_KEYWORD_MAX_LENGTH   = 79;

  (* Maximum number of entries in PLTE/sPLT/tRNS arrays *)
  PNG_MAX_PALETTE_LENGTH  = 256;

  (* These determine if an ancillary chunk's data has been successfully read
   * from the PNG header, or if the application has filled in the corresponding
   * data in the info_struct to be written into the output file.  The values
   * of the PNG_INFO_<chunk> defines should NOT be changed.
   *)
  PNG_INFO_gAMA = $0001;
  PNG_INFO_sBIT = $0002;
  PNG_INFO_cHRM = $0004;
  PNG_INFO_PLTE = $0008;
  PNG_INFO_tRNS = $0010;
  PNG_INFO_bKGD = $0020;
  PNG_INFO_hIST = $0040;
  PNG_INFO_pHYs = $0080;
  PNG_INFO_oFFs = $0100;
  PNG_INFO_tIME = $0200;
  PNG_INFO_pCAL = $0400;
  PNG_INFO_sRGB = $0800;  (* GR-P, 0.96a *)
  PNG_INFO_iCCP = $1000;  (* ESR, 1.0.6 *)
  PNG_INFO_sPLT = $2000;  (* ESR, 1.0.6 *)
  PNG_INFO_sCAL = $4000;  (* ESR, 1.0.6 *)
  PNG_INFO_IDAT = $8000;  (* ESR, 1.0.6 *)


(*
var
  png_libpng_ver    : array[0..11] of char; external LibPng name 'png_libpng_ver';
  png_pass_start    : array[0..6] of cint; external LibPng name 'png_pass_start';
  png_pass_inc      : array[0..6] of cint; external LibPng name 'png_pass_inc';
  png_pass_ystart   : array[0..6] of cint; external LibPng name 'png_pass_ystart';
  png_pass_yinc     : array[0..6] of cint; external LibPng name 'png_pass_yinc';
  png_pass_mask     : array[0..6] of cint; external LibPng name 'png_pass_mask';
  png_pass_dsp_mask : array[0..6] of cint; external LibPng name 'png_pass_dsp_mask';
*)

type
  (* Three color definitions.  The order of the red, green, and blue, (and the
   * exact size) is not important, although the size of the fields need to
   * be png_byte or png_uint_16 (as defined below).
   *)
  png_color = record
       red : png_byte;
       green : png_byte;
       blue : png_byte;
    end;
  ppng_color = ^png_color;
  pppng_color = ^ppng_color;
  png_color_struct = png_color;
  png_colorp = Ppng_color;
  ppng_colorp = ^png_colorp;
  png_colorpp = PPpng_color;

  png_color_16 = record
       index : png_byte;    (* used for palette files *)
       red : png_uint_16;   (* for use in red green blue files *)
       green : png_uint_16;
       blue : png_uint_16;
       gray : png_uint_16;  (* for use in grayscale files *)
    end;
  ppng_color_16 = ^png_color_16 ;
  pppng_color_16 = ^ppng_color_16 ;
  png_color_16_struct = png_color_16;
  png_color_16p = Ppng_color_16;
  ppng_color_16p = ^png_color_16p;
  png_color_16pp = PPpng_color_16;

  png_color_8 = record
       red : png_byte;    (* for use in red green blue files *)
       green : png_byte;
       blue : png_byte;
       gray : png_byte;   (* for use in grayscale files *)
       alpha : png_byte;  (* for alpha channel files *)
    end;
  ppng_color_8 = ^png_color_8;
  pppng_color_8 = ^ppng_color_8;
  png_color_8_struct = png_color_8;
  png_color_8p = Ppng_color_8;
  ppng_color_8p = ^png_color_8p;
  png_color_8pp = PPpng_color_8;
   
  (*
   * The following two structures are used for the in-core representation
   * of sPLT chunks.
   *)
  png_sPLT_entry = record
       red : png_uint_16;
       green : png_uint_16;
       blue : png_uint_16;
       alpha : png_uint_16;
       frequency : png_uint_16;
    end;
  ppng_sPLT_entry = ^png_sPLT_entry;
  pppng_sPLT_entry = ^ppng_sPLT_entry;
  png_sPLT_entry_struct = png_sPLT_entry;
  png_sPLT_entryp = Ppng_sPLT_entry;
  png_sPLT_entrypp = PPpng_sPLT_entry;

  (*  When the depth of the sPLT palette is 8 bits, the color and alpha samples
   *  occupy the LSB of their respective members, and the MSB of each member
   *  is zero-filled.  The frequency member always occupies the full 16 bits.
   *)

  png_sPLT_t = record
       name : png_charp;          (* palette name *)
       depth : png_byte;          (* depth of palette samples *)
       entries : png_sPLT_entryp; (* palette entries *)
       nentries : png_int_32;     (* number of palette entries *)
    end;
  ppng_sPLT_t = ^png_sPLT_t;
  pppng_sPLT_t = ^ppng_sPLT_t;
  png_sPLT_struct = png_sPLT_t;
  png_sPLT_tp = Ppng_sPLT_t;
  png_sPLT_tpp = PPpng_sPLT_t;
   
  (* png_text holds the contents of a text/ztxt/itxt chunk in a PNG file,
   * and whether that contents is compressed or not.  The "key" field
   * points to a regular zero-terminated C string.  The "text", "lang", and
   * "lang_key" fields can be regular C strings, empty strings, or NULL pointers.
   * However, the * structure returned by png_get_text() will always contain
   * regular zero-terminated C strings (possibly empty), never NULL pointers,
   * so they can be safely used in printf() and other string-handling functions.
   *)
  png_text = record
       compression : cint;       (* compression value:
                                   -1: tEXt, none
                                    0: zTXt, deflate
                                    1: iTXt, none
                                    2: iTXt, deflate  *)
       key : png_charp;          (* keyword, 1-79 character description of "text" *)
       text : png_charp;         (* comment, may be an empty string (ie "")
                                    or a NULL pointer *)
       text_length : png_size_t; (* length of the text string *)
    end;
  ppng_text = ^png_text;
  pppng_text = ^ppng_text;
  png_text_struct = png_text;
  png_textp = Ppng_text;
  ppng_textp = ^png_textp;
  png_textpp = PPpng_text;
   
  (* png_time is a way to hold the time in an machine independent way.
   * Two conversions are provided, both from time_t and struct tm.  There
   * is no portable way to convert to either of these structures, as far
   * as I know.  If you know of a portable way, send it to me.  As a side
   * note - PNG has always been Year 2000 compliant!
   *)
  png_time = record
       year : png_uint_16; (* full year, as in, 1995 *)
       month : png_byte;   (* month of year, 1 - 12 *)
       day : png_byte;     (* day of month, 1 - 31 *)
       hour : png_byte;    (* hour of day, 0 - 23 *)
       minute : png_byte;  (* minute of hour, 0 - 59 *)
       second : png_byte;  (* second of minute, 0 - 60 (for leap seconds) *)
    end;
  ppng_time = ^png_time;
  pppng_time = ^ppng_time;
  png_time_struct = png_time;
  png_timep = Ppng_time;
  PPNG_TIMEP = ^PNG_TIMEP;
  png_timepp = PPpng_time;

const
  PNG_CHUNK_NAME_LENGTH = 5; 
type
  (* png_unknown_chunk is a structure to hold queued chunks for which there is
   * no specific support.  The idea is that we can use this to queue
   * up private chunks for output even though the library doesn't actually
   * know about their semantics.
   *)
  png_unknown_chunk = record
       name : array[0..PNG_CHUNK_NAME_LENGTH-1] of png_byte;
       data : Ppng_byte;
       size : png_size_t;
       
       (* libpng-using applications should NOT directly modify this byte. *)
       location : png_byte; (* mode of operation at read time *)
    end;
  ppng_unknown_chunk = ^png_unknown_chunk;
  pppng_unknown_chunk = ^ppng_unknown_chunk;
  png_unknown_chunk_t = png_unknown_chunk;
  png_unknown_chunkp = Ppng_unknown_chunk;
  png_unknown_chunkpp = PPpng_unknown_chunk;
   
  (* png_info is a structure that holds the information in a PNG file so
   * that the application can find out the characteristics of the image.
   * If you are reading the file, this structure will tell you what is
   * in the PNG file.  If you are writing the file, fill in the information
   * you want to put into the PNG file, then call png_write_info().
   * The names chosen should be very close to the PNG specification, so
   * consult that document for information about the meaning of each field.
   *
   * With libpng < 0.95, it was only possible to directly set and read the
   * the values in the png_info_struct, which meant that the contents and
   * order of the values had to remain fixed.  With libpng 0.95 and later,
   * however, there are now functions that abstract the contents of
   * png_info_struct from the application, so this makes it easier to use
   * libpng with dynamic libraries, and even makes it possible to use
   * libraries that don't have all of the libpng ancillary chunk-handing
   * functionality.
   *
   * In any case, the order of the parameters in png_info_struct should NOT
   * be changed for as long as possible to keep compatibility with applications
   * that use the old direct-access method with png_info_struct.
   *
   * The following members may have allocated storage attached that should be
   * cleaned up before the structure is discarded: palette, trans, text,
   * pcal_purpose, pcal_units, pcal_params, hist, iccp_name, iccp_profile,
   * splt_palettes, scal_unit, row_pointers, and unknowns.   By default, these
   * are automatically freed when the info structure is deallocated, if they were
   * allocated internally by libpng.  This behavior can be changed by means
   * of the png_data_freer() function.
   *
   * More allocation details: all the chunk-reading functions that
   * change these members go through the corresponding png_set_*
   * functions.  A function to clear these members is available: see
   * png_free_data().  The png_set_* functions do not depend on being
   * able to point info structure members to any of the storage they are
   * passed (they make their own copies), EXCEPT that the png_set_text
   * functions use the same storage passed to them in the text_ptr or
   * itxt_ptr structure argument, and the png_set_rows and png_set_unknowns
   * functions do not make their own copies.
   *)
  png_info = record
       width : png_uint_32;       (* width of image in pixels (from IHDR) *)
       height : png_uint_32;      (* height of image in pixels (from IHDR) *)
       valid : png_uint_32;       (* valid chunk data (see PNG_INFO_ below) *)
       rowbytes : png_uint_32;    (* bytes needed to hold an untransformed row *)
       palette : png_colorp;      (* array of color values (valid & PNG_INFO_PLTE) *)
       num_palette : png_uint_16; (* number of color entries in "palette" (PLTE) *)
       num_trans : png_uint_16;   (* number of transparent palette color (tRNS) *)
       bit_depth : png_byte;      (* 1, 2, 4, 8, or 16 bits/channel (from IHDR) *)
       color_type : png_byte;     (* see PNG_COLOR_TYPE_ below (from IHDR) *)
       (* The following three should have been named *_method not *_type *)
       compression_type : png_byte; (* must be PNG_COMPRESSION_TYPE_BASE (IHDR) *)
       filter_type : png_byte;    (* must be PNG_FILTER_TYPE_BASE (from IHDR) *)
       interlace_type : png_byte; (* One of PNG_INTERLACE_NONE, PNG_INTERLACE_ADAM7 *)

       (* The following is informational only on read, and not used on writes. *)
       channels : png_byte;       (* number of data channels per pixel (1, 2, 3, 4) *)
       pixel_depth : png_byte;    (* number of bits per pixel *)
       spare_byte : png_byte;     (* to align the data, and for future use *)
       signature : array[0..7] of png_byte; (* magic bytes read by libpng from start of file *)

       (* The rest of the data is optional.  If you are reading, check the
        * valid field to see if the information in these are valid.  If you
        * are writing, set the valid field to those chunks you want written,
        * and initialize the appropriate fields below.
        *)
        
       gamma : cfloat;
       srgb_intent : png_byte;
       num_text : cint;
       max_text : cint;
       text : png_textp;
       mod_time : png_time;
       sig_bit : png_color_8;
       trans : png_bytep;
       trans_values : png_color_16;
       background : png_color_16;
       x_offset : png_int_32;
       y_offset : png_int_32;
       offset_unit_type : png_byte;
       x_pixels_per_unit : png_uint_32;
       y_pixels_per_unit : png_uint_32;
       phys_unit_type : png_byte;
       hist : png_uint_16p;
       x_white : cfloat;
       y_white : cfloat;
       x_red : cfloat;
       y_red : cfloat;
       x_green : cfloat;
       y_green : cfloat;
       x_blue : cfloat;
       y_blue : cfloat;
       pcal_purpose : png_charp;
       pcal_X0 : png_int_32;
       pcal_X1 : png_int_32;
       pcal_units : png_charp;
       pcal_params : png_charpp;
       pcal_type : png_byte;
       pcal_nparams : png_byte;
       free_me : png_uint_32;
       unknown_chunks : png_unknown_chunkp;
       unknown_chunks_num : png_size_t;
       iccp_name : png_charp;
       iccp_profile : png_charp;
       iccp_proflen : png_uint_32;
       iccp_compression : png_byte;
       splt_palettes : png_sPLT_tp;
       splt_palettes_num : png_uint_32;
       scal_unit : png_byte;
       scal_pixel_width : cdouble;
       scal_pixel_height : cdouble;
       scal_s_width : png_charp;
       scal_s_height : png_charp;
       row_pointers : png_bytepp;
       int_gamma : png_fixed_point;
       int_x_white : png_fixed_point;
       int_y_white : png_fixed_point;
       int_x_red : png_fixed_point;
       int_y_red : png_fixed_point;
       int_x_green : png_fixed_point;
       int_y_green : png_fixed_point;
       int_x_blue : png_fixed_point;
       int_y_blue : png_fixed_point;
    end;
  ppng_info = ^png_info;
  pppng_info = ^ppng_info;
  png_info_struct = png_info;
  png_infop = Ppng_info;
  png_infopp = PPpng_info;
  
  (* This is used for the transformation routines, as some of them
   * change these values for the row.  It also should enable using
   * the routines for other purposes.
   *)
  png_row_info = record
       width : png_uint_32;    (* width of row *)
       rowbytes : png_uint_32; (* number of bytes in row *)
       color_type : png_byte;  (* color type of row *)
       bit_depth : png_byte;   (* bit depth of row *)
       channels : png_byte;    (* number of channels (1, 2, 3, or 4) *)
       pixel_depth : png_byte; (* bits per pixel (depth * channels) *)
    end;
  ppng_row_info = ^png_row_info;
  pppng_row_info = ^ppng_row_info;
  png_row_info_struct = png_row_info;
  png_row_infop = Ppng_row_info;
  png_row_infopp = PPpng_row_info;
  png_structp = ^png_struct;


  (* These are the function types for the I/O functions and for the functions
   * that allow the user to override the default I/O functions with his or her
   * own.  The png_error_ptr type should match that of user-supplied warning
   * and error functions, while the png_rw_ptr type should match that of the
   * user read/write data functions.
   *)
  png_error_ptr = procedure(Arg1 : png_structp; Arg2 : png_const_charp); cdecl;
  png_rw_ptr = procedure(Arg1 : png_structp; Arg2 : png_bytep; Arg3 : png_size_t); cdecl;
  png_flush_ptr = procedure (Arg1 : png_structp); cdecl;
  png_read_status_ptr = procedure (Arg1 : png_structp; Arg2 : png_uint_32; Arg3: cint); cdecl;
  png_write_status_ptr = procedure (Arg1 : png_structp; Arg2:png_uint_32;Arg3 : cint); cdecl;
  png_progressive_info_ptr = procedure (Arg1 : png_structp; Arg2 : png_infop); cdecl;
  png_progressive_end_ptr = procedure (Arg1 : png_structp; Arg2 : png_infop); cdecl;
  png_progressive_row_ptr = procedure (Arg1 : png_structp; Arg2 : png_bytep; Arg3 : png_uint_32; Arg4 : cint); cdecl;
  png_user_transform_ptr = procedure (Arg1 : png_structp; Arg2 : png_row_infop; Arg3 : png_bytep); cdecl;
  png_user_chunk_ptr = function (Arg1 : png_structp; Arg2 : png_unknown_chunkp): cint; cdecl;
  png_unknown_chunk_ptr = procedure (Arg1 : png_structp); cdecl;
  png_malloc_ptr = function (Arg1 : png_structp; Arg2 : png_size_t) : png_voidp; cdecl;
  png_free_ptr = procedure (Arg1 : png_structp; Arg2 : png_voidp); cdecl;

  png_struct_def = record
        {$ifdef UsePngStruct}
        jmpbuf : jmp_buf;            (* used in png_error *)
        error_fn : png_error_ptr;    (* function for printing errors and aborting *)
        warning_fn : png_error_ptr;  (* function for printing warnings *)
        error_ptr : png_voidp;       (* user supplied struct for error functions *)
        write_data_fn : png_rw_ptr;  (* function for writing output data *)
        read_data_fn : png_rw_ptr;   (* function for reading input data *)
        io_ptr : png_voidp;          (* ptr to application struct for I/O functions *)

        read_user_transform_fn : png_user_transform_ptr;  (* user read transform *)

        write_user_transform_fn : png_user_transform_ptr; (* user write transform *)

        (* These were added in libpng-1.0.2 *)
        user_transform_ptr : png_voidp; (* user supplied struct for user transform *)
        user_transform_depth : png_byte;    (* bit depth of user transformed pixels *)
        user_transform_channels : png_byte; (* channels in user transformed pixels *)

        mode : png_uint_32;          (* tells us where we are in the PNG file *)
        flags : png_uint_32;         (* flags indicating various things to libpng *)
        transformations : png_uint_32; (* which transformations to perform *)

        zstream : z_stream;          (* pointer to decompression structure (below) *)
        zbuf : png_bytep;            (* buffer for zlib *)
        zbuf_size : png_size_t;      (* size of zbuf *)
        zlib_level : cint;        (* holds zlib compression level *)
        zlib_method : cint;       (* holds zlib compression method *)
        zlib_window_bits : cint;  (* holds zlib compression window bits *)
        zlib_mem_level : cint;    (* holds zlib compression memory level *)
        zlib_strategy : cint;     (* holds zlib compression strategy *)

        width : png_uint_32;         (* width of image in pixels *)
        height : png_uint_32;        (* height of image in pixels *)
        num_rows : png_uint_32;      (* number of rows in current pass *)
        usr_width : png_uint_32;     (* width of row at start of write *)
        rowbytes : png_uint_32;      (* size of row in bytes *)
        irowbytes : png_uint_32;     (* size of current interlaced row in bytes *)
        iwidth : png_uint_32;        (* width of current interlaced row in pixels *)
        row_number : png_uint_32;    (* current row in interlace pass *)
        prev_row : png_bytep;        (* buffer to save previous (unfiltered) row *)
        row_buf : png_bytep;         (* buffer to save current (unfiltered) row *)
        sub_row : png_bytep;         (* buffer to save "sub" row when filtering *)
        up_row : png_bytep;          (* buffer to save "up" row when filtering *)
        avg_row : png_bytep;         (* buffer to save "avg" row when filtering *)
        paeth_row : png_bytep;       (* buffer to save "Paeth" row when filtering *)
        row_info : png_row_info;     (* used for transformation routines *)
        
        idat_size : png_uint_32;     (* current IDAT size for read *)
        crc : png_uint_32;           (* current chunk CRC value *)
        palette : png_colorp;        (* palette from the input file *)
        num_palette : png_uint_16;   (* number of color entries in palette *)
        num_trans : png_uint_16;     (* number of transparency values *)
        chunk_name : array[0..4] of png_byte; (* null-terminated name of current chunk *)
        compression : png_byte;      (* file compression type (always 0) *)
        filter : png_byte;           (* file filter type (always 0) *)
        interlaced : png_byte;       (* PNG_INTERLACE_NONE, PNG_INTERLACE_ADAM7 *)
        pass : png_byte;             (* current interlace pass (0 - 6) *)
        do_filter : png_byte;        (* row filter flags (see PNG_FILTER_ below ) *)
        color_type : png_byte;       (* color type of file *)
        bit_depth : png_byte;        (* bit depth of file *)
        usr_bit_depth : png_byte;    (* bit depth of users row *)
        pixel_depth : png_byte;      (* number of bits per pixel *)
        channels : png_byte;         (* number of channels in file *)
        usr_channels : png_byte;     (* channels at start of write *)
        sig_bytes : png_byte;        (* magic bytes read/written from start of file *)

        filler : png_uint_16;

        background_gamma_type : png_byte;
        background_gamma : cfloat;
        background : png_color_16;
        background_1 : png_color_16;
        output_flush_fn : png_flush_ptr;
        flush_dist : png_uint_32;
        flush_rows : png_uint_32;
        gamma_shift : cint;
        gamma : cfloat;
        screen_gamma : cfloat;
        gamma_table : png_bytep;
        gamma_from_1 : png_bytep;
        gamma_to_1 : png_bytep;
        gamma_16_table : png_uint_16pp;
        gamma_16_from_1 : png_uint_16pp;
        gamma_16_to_1 : png_uint_16pp;
        sig_bit : png_color_8;
        shift : png_color_8;
        trans : png_bytep;
        trans_values : png_color_16;
        read_row_fn : png_read_status_ptr;
        write_row_fn : png_write_status_ptr;
        info_fn : png_progressive_info_ptr;
        row_fn : png_progressive_row_ptr;
        end_fn : png_progressive_end_ptr;
        save_buffer_ptr : png_bytep;
        save_buffer : png_bytep;
        current_buffer_ptr : png_bytep;
        current_buffer : png_bytep;
        push_length : png_uint_32;
        skip_length : png_uint_32;
        save_buffer_size : png_size_t;
        save_buffer_max : png_size_t;
        buffer_size : png_size_t;
        current_buffer_size : png_size_t;
        process_mode : cint;
        cur_palette : cint;
        current_text_size : png_size_t;
        current_text_left : png_size_t;
        current_text : png_charp;
        current_text_ptr : png_charp;
        palette_lookup : png_bytep;
        dither_index : png_bytep;
        hist : png_uint_16p;
        heuristic_method : png_byte;
        num_prev_filters : png_byte;
        prev_filters : png_bytep;
        filter_weights : png_uint_16p;
        inv_filter_weights : png_uint_16p;
        filter_costs : png_uint_16p;
        inv_filter_costs : png_uint_16p;
        time_buffer : png_charp;
        free_me : png_uint_32;
        user_chunk_ptr : png_voidp;
        read_user_chunk_fn : png_user_chunk_ptr;
        num_chunk_list : cint;
        chunk_list : png_bytep;
        rgb_to_gray_status : png_byte;
        rgb_to_gray_red_coeff : png_uint_16;
        rgb_to_gray_green_coeff : png_uint_16;
        rgb_to_gray_blue_coeff : png_uint_16;
        empty_plte_permitted : png_byte;
        int_gamma : png_fixed_point;
        {$endif UsePngStruct}
     end;
   ppng_struct_def = ^png_struct_def;
   pppng_struct_def = ^ppng_struct_def;
   png_struct = png_struct_def;
   ppng_struct = ^png_struct;
   pppng_struct = ^ppng_struct;

   version_1_0_8 = png_structp;
   png_structpp = PPpng_struct;

function png_access_version_number:png_uint_32; cdecl; external LibPng;

procedure png_set_sig_bytes(png_ptr:png_structp; num_bytes:cint); cdecl; external LibPng;
function png_sig_cmp(sig:png_bytep; start:png_size_t; num_to_check:png_size_t):cint; cdecl; external LibPng;
function png_check_sig(sig:png_bytep; num:cint):cint; cdecl; external LibPng;

(* Allocate and initialize png_ptr struct for reading, and any other memory. *)
function png_create_read_struct(user_png_ver:png_const_charp; error_ptr:png_voidp; error_fn:png_error_ptr; warn_fn:png_error_ptr):png_structp; cdecl; external LibPng;

(* Allocate and initialize png_ptr struct for writing, and any other memory *)
function png_create_write_struct(user_png_ver:png_const_charp; error_ptr:png_voidp; error_fn:png_error_ptr; warn_fn:png_error_ptr):png_structp; cdecl; external LibPng;

function png_get_compression_buffer_size(png_ptr:png_structp):png_uint_32; cdecl; external LibPng;
procedure png_set_compression_buffer_size(png_ptr:png_structp; size:png_uint_32); cdecl; external LibPng;
function png_reset_zstream(png_ptr:png_structp):cint; cdecl; external LibPng;

procedure png_write_chunk(png_ptr:png_structp; chunk_name:png_bytep; data:png_bytep; length:png_size_t); cdecl; external LibPng;
procedure png_write_chunk_start(png_ptr:png_structp; chunk_name:png_bytep; length:png_uint_32); cdecl; external LibPng;
procedure png_write_chunk_data(png_ptr:png_structp; data:png_bytep; length:png_size_t); cdecl; external LibPng;
procedure png_write_chunk_end(png_ptr:png_structp); cdecl; external LibPng;

(* Allocate and initialize the info structure *)
function png_create_info_struct(png_ptr:png_structp):png_infop; cdecl; external LibPng;

(* Initialize the info structure (old interface - DEPRECATED) *)
procedure png_info_init(info_ptr:png_infop); cdecl; external LibPng;

(* Writes all the PNG information before the image. *)
procedure png_write_info_before_PLTE(png_ptr:png_structp; info_ptr:png_infop); cdecl; external LibPng;
procedure png_write_info(png_ptr:png_structp; info_ptr:png_infop); cdecl; external LibPng;

(* read the information before the actual image data. *)
procedure png_read_info(png_ptr:png_structp; info_ptr:png_infop); cdecl; external LibPng;

function png_convert_to_rfc1123(png_ptr:png_structp; ptime:png_timep):png_charp; cdecl; external LibPng;
procedure png_convert_from_struct_tm(ptime:png_timep; ttime:Pointer); cdecl; external LibPng;
{$IFDEF UNIX}
procedure png_convert_from_time_t(ptime:png_timep; ttime:time_t); cdecl; external LibPng;
{$ENDIF}
procedure png_set_expand(png_ptr:png_structp); cdecl; external LibPng;
procedure png_set_gray_1_2_4_to_8(png_ptr:png_structp); cdecl; external LibPng;
procedure png_set_palette_to_rgb(png_ptr:png_structp); cdecl; external LibPng;
procedure png_set_tRNS_to_alpha(png_ptr:png_structp); cdecl; external LibPng;
procedure png_set_bgr(png_ptr:png_structp); cdecl; external LibPng;
procedure png_set_gray_to_rgb(png_ptr:png_structp); cdecl; external LibPng;
procedure png_set_rgb_to_gray(png_ptr:png_structp; error_action:cint; red:cdouble; green:cdouble); cdecl; external LibPng;
procedure png_set_rgb_to_gray_fixed(png_ptr:png_structp; error_action:cint; red:png_fixed_point; green:png_fixed_point); cdecl; external LibPng;
function png_get_rgb_to_gray_status(png_ptr:png_structp):png_byte; cdecl; external LibPng;
procedure png_build_grayscale_palette(bit_depth:cint; palette:png_colorp); cdecl; external LibPng;
procedure png_set_strip_alpha(png_ptr:png_structp); cdecl; external LibPng;
procedure png_set_swap_alpha(png_ptr:png_structp); cdecl; external LibPng;
procedure png_set_invert_alpha(png_ptr:png_structp); cdecl; external LibPng;
procedure png_set_filler(png_ptr:png_structp; filler:png_uint_32; flags:cint); cdecl; external LibPng;
procedure png_set_swap(png_ptr:png_structp); cdecl; external LibPng;
procedure png_set_packing(png_ptr:png_structp); cdecl; external LibPng;
procedure png_set_packswap(png_ptr:png_structp); cdecl; external LibPng;
procedure png_set_shift(png_ptr:png_structp; true_bits:png_color_8p); cdecl; external LibPng;
function png_set_interlace_handling(png_ptr:png_structp):cint; cdecl; external LibPng;
procedure png_set_invert_mono(png_ptr:png_structp); cdecl; external LibPng;
procedure png_set_background(png_ptr:png_structp; background_color:png_color_16p; background_gamma_code:cint; need_expand:cint; background_gamma:cdouble); cdecl; external LibPng;
procedure png_set_strip_16(png_ptr:png_structp); cdecl; external LibPng;
procedure png_set_dither(png_ptr:png_structp; palette:png_colorp; num_palette:cint; maximum_colors:cint; histogram:png_uint_16p;
            full_dither:cint); cdecl; external LibPng;
procedure png_set_gamma(png_ptr:png_structp; screen_gamma:cdouble; default_file_gamma:cdouble); cdecl; external LibPng;
procedure png_permit_empty_plte(png_ptr:png_structp; empty_plte_permitted:cint); cdecl; external LibPng;
procedure png_set_flush(png_ptr:png_structp; nrows:cint); cdecl; external LibPng;
procedure png_write_flush(png_ptr:png_structp); cdecl; external LibPng;
procedure png_start_read_image(png_ptr:png_structp); cdecl; external LibPng;
procedure png_read_update_info(png_ptr:png_structp; info_ptr:png_infop); cdecl; external LibPng;

(* read one or more rows of image data. *)
procedure png_read_rows(png_ptr:png_structp; row:png_bytepp; display_row:png_bytepp; num_rows:png_uint_32); cdecl; external LibPng;

(* read a row of data. *)
procedure png_read_row(png_ptr:png_structp; row:png_bytep; display_row:png_bytep); cdecl; external LibPng;

(* read the whole image into memory at once. *)
procedure png_read_image(png_ptr:png_structp; image:png_bytepp); cdecl; external LibPng;

(* write a row of image data *)
procedure png_write_row(png_ptr:png_structp; row:png_bytep); cdecl; external LibPng;

(* write a few rows of image data *)
procedure png_write_rows(png_ptr:png_structp; row:png_bytepp; num_rows:png_uint_32); cdecl; external LibPng;

(* write the image data *)
procedure png_write_image(png_ptr:png_structp; image:png_bytepp); cdecl; external LibPng;

(* writes the end of the PNG file. *)
procedure png_write_end(png_ptr:png_structp; info_ptr:png_infop); cdecl; external LibPng;

(* read the end of the PNG file. *)
procedure png_read_end(png_ptr:png_structp; info_ptr:png_infop); cdecl; external LibPng;

(* free any memory associated with the png_info_struct *)
procedure png_destroy_info_struct(png_ptr:png_structp; info_ptr_ptr:png_infopp); cdecl; external LibPng;

(* free any memory associated with the png_struct and the png_info_structs *)
procedure png_destroy_read_struct(png_ptr_ptr:png_structpp; info_ptr_ptr:png_infopp; end_info_ptr_ptr:png_infopp); cdecl; external LibPng;

(* free all memory used by the read (old method - NOT DLL EXPORTED) *)
procedure png_read_destroy(png_ptr:png_structp; info_ptr:png_infop; end_info_ptr:png_infop); cdecl; external LibPng;

(* free any memory associated with the png_struct and the png_info_structs *)
procedure png_destroy_write_struct(png_ptr_ptr:png_structpp; info_ptr_ptr:png_infopp); cdecl; external LibPng;

procedure png_write_destroy_info(info_ptr:png_infop); cdecl; external LibPng;
procedure png_write_destroy(png_ptr:png_structp); cdecl; external LibPng;

procedure png_set_crc_action(png_ptr:png_structp; crit_action:cint; ancil_action:cint); cdecl; external LibPng;

procedure png_set_filter(png_ptr:png_structp; method:cint; filters:cint); cdecl; external LibPng;
procedure png_set_filter_heuristics(png_ptr:png_structp; heuristic_method:cint; num_weights:cint; filter_weights:png_doublep; filter_costs:png_doublep); cdecl; external LibPng;

procedure png_set_compression_level(png_ptr:png_structp; level:cint); cdecl; external LibPng;
procedure png_set_compression_mem_level(png_ptr:png_structp; mem_level:cint); cdecl; external LibPng;
procedure png_set_compression_strategy(png_ptr:png_structp; strategy:cint); cdecl; external LibPng;
procedure png_set_compression_window_bits(png_ptr:png_structp; window_bits:cint); cdecl; external LibPng;
procedure png_set_compression_method(png_ptr:png_structp; method:cint); cdecl; external LibPng;

procedure png_init_io(png_ptr:png_structp; fp:png_FILE_p); cdecl; external LibPng;

(* Replace the (error and abort), and warning functions with user
 * supplied functions.  If no messages are to be printed you must still
 * write and use replacement functions. The replacement error_fn should
 * still do a longjmp to the last setjmp location if you are using this
 * method of error handling.  If error_fn or warning_fn is NULL, the
 * default function will be used.
 *)
procedure png_set_error_fn(png_ptr:png_structp; error_ptr:png_voidp; error_fn:png_error_ptr; warning_fn:png_error_ptr); cdecl; external LibPng;

(* Return the user pointer associated with the error functions *)
function png_get_error_ptr(png_ptr:png_structp):png_voidp; cdecl; external LibPng;

(* Replace the default data output functions with a user supplied one(s).
 * If buffered output is not used, then output_flush_fn can be set to NULL.
 * If PNG_WRITE_FLUSH_SUPPORTED is not defined at libpng compile time
 * output_flush_fn will be ignored (and thus can be NULL).
 *)
procedure png_set_write_fn(png_ptr:png_structp; io_ptr:png_voidp; write_data_fn:png_rw_ptr; output_flush_fn:png_flush_ptr); cdecl; external LibPng;

(* Replace the default data input function with a user supplied one. *)
procedure png_set_read_fn(png_ptr:png_structp; io_ptr:png_voidp; read_data_fn:png_rw_ptr); cdecl; external LibPng;

(* Return the user pointer associated with the I/O functions *)
function png_get_io_ptr(png_ptr:png_structp):png_voidp; cdecl; external LibPng;

procedure png_set_read_status_fn(png_ptr:png_structp; read_row_fn:png_read_status_ptr); cdecl; external LibPng;
procedure png_set_write_status_fn(png_ptr:png_structp; write_row_fn:png_write_status_ptr); cdecl; external LibPng;
procedure png_set_read_user_transform_fn(png_ptr:png_structp; read_user_transform_fn:png_user_transform_ptr); cdecl; external LibPng;
procedure png_set_write_user_transform_fn(png_ptr:png_structp; write_user_transform_fn:png_user_transform_ptr); cdecl; external LibPng;
procedure png_set_user_transform_info(png_ptr:png_structp; user_transform_ptr:png_voidp; user_transform_depth:cint; user_transform_channels:cint); cdecl; external LibPng;
function png_get_user_transform_ptr(png_ptr:png_structp):png_voidp; cdecl; external LibPng;
procedure png_set_read_user_chunk_fn(png_ptr:png_structp; user_chunk_ptr:png_voidp; read_user_chunk_fn:png_user_chunk_ptr); cdecl; external LibPng;
function png_get_user_chunk_ptr(png_ptr:png_structp):png_voidp; cdecl; external LibPng;
procedure png_set_progressive_read_fn(png_ptr:png_structp; progressive_ptr:png_voidp; info_fn:png_progressive_info_ptr; row_fn:png_progressive_row_ptr; end_fn:png_progressive_end_ptr); cdecl; external LibPng;
function png_get_progressive_ptr(png_ptr:png_structp):png_voidp; cdecl; external LibPng;
procedure png_process_data(png_ptr:png_structp; info_ptr:png_infop; buffer:png_bytep; buffer_size:png_size_t); cdecl; external LibPng;
procedure png_progressive_combine_row(png_ptr:png_structp; old_row:png_bytep; new_row:png_bytep); cdecl; external LibPng;
function png_malloc(png_ptr:png_structp; size:png_uint_32):png_voidp; cdecl; external LibPng;
procedure png_free(png_ptr:png_structp; ptr:png_voidp); cdecl; external LibPng;
procedure png_free_data(png_ptr:png_structp; info_ptr:png_infop; free_me:png_uint_32; num:cint); cdecl; external LibPng;
procedure png_data_freer(png_ptr:png_structp; info_ptr:png_infop; freer:cint; mask:png_uint_32); cdecl; external LibPng;
function png_memcpy_check(png_ptr:png_structp; s1:png_voidp; s2:png_voidp; size:png_uint_32):png_voidp; cdecl; external LibPng;
function png_memset_check(png_ptr:png_structp; s1:png_voidp; value:cint; size:png_uint_32):png_voidp; cdecl; external LibPng;
procedure png_error(png_ptr:png_structp; error:png_const_charp); cdecl; external LibPng;
procedure png_chunk_error(png_ptr:png_structp; error:png_const_charp); cdecl; external LibPng;
procedure png_warning(png_ptr:png_structp; message:png_const_charp); cdecl; external LibPng;
procedure png_chunk_warning(png_ptr:png_structp; message:png_const_charp); cdecl; external LibPng;
function png_get_valid(png_ptr:png_structp; info_ptr:png_infop; flag:png_uint_32):png_uint_32; cdecl; external LibPng;
function png_get_rowbytes(png_ptr:png_structp; info_ptr:png_infop):png_uint_32; cdecl; external LibPng;
function png_get_rows(png_ptr:png_structp; info_ptr:png_infop):png_bytepp; cdecl; external LibPng;
procedure png_set_rows(png_ptr:png_structp; info_ptr:png_infop; row_pointers:png_bytepp); cdecl; external LibPng;
function png_get_channels(png_ptr:png_structp; info_ptr:png_infop):png_byte; cdecl; external LibPng;
function png_get_image_width(png_ptr:png_structp; info_ptr:png_infop):png_uint_32; cdecl; external LibPng;
function png_get_image_height(png_ptr:png_structp; info_ptr:png_infop):png_uint_32; cdecl; external LibPng;
function png_get_bit_depth(png_ptr:png_structp; info_ptr:png_infop):png_byte; cdecl; external LibPng;
function png_get_color_type(png_ptr:png_structp; info_ptr:png_infop):png_byte; cdecl; external LibPng;
function png_get_filter_type(png_ptr:png_structp; info_ptr:png_infop):png_byte; cdecl; external LibPng;
function png_get_interlace_type(png_ptr:png_structp; info_ptr:png_infop):png_byte; cdecl; external LibPng;
function png_get_compression_type(png_ptr:png_structp; info_ptr:png_infop):png_byte; cdecl; external LibPng;
function png_get_pixels_per_meter(png_ptr:png_structp; info_ptr:png_infop):png_uint_32; cdecl; external LibPng;
function png_get_x_pixels_per_meter(png_ptr:png_structp; info_ptr:png_infop):png_uint_32; cdecl; external LibPng;
function png_get_y_pixels_per_meter(png_ptr:png_structp; info_ptr:png_infop):png_uint_32; cdecl; external LibPng;
function png_get_pixel_aspect_ratio(png_ptr:png_structp; info_ptr:png_infop):cfloat; cdecl; external LibPng;
function png_get_x_offset_pixels(png_ptr:png_structp; info_ptr:png_infop):png_int_32; cdecl; external LibPng;
function png_get_y_offset_pixels(png_ptr:png_structp; info_ptr:png_infop):png_int_32; cdecl; external LibPng;
function png_get_x_offset_microns(png_ptr:png_structp; info_ptr:png_infop):png_int_32; cdecl; external LibPng;
function png_get_y_offset_microns(png_ptr:png_structp; info_ptr:png_infop):png_int_32; cdecl; external LibPng;
function png_get_signature(png_ptr:png_structp; info_ptr:png_infop):png_bytep; cdecl; external LibPng;

function png_get_bKGD(png_ptr:png_structp; info_ptr:png_infop; background:Ppng_color_16p):png_uint_32; cdecl; external LibPng;
procedure png_set_bKGD(png_ptr:png_structp; info_ptr:png_infop; background:png_color_16p); cdecl; external LibPng;
function png_get_cHRM(png_ptr:png_structp; info_ptr:png_infop; white_x:PCdouble; white_y:PCdouble; red_x:PCdouble;
           red_y:PCdouble; green_x:PCdouble; green_y:PCdouble; blue_x:PCdouble; blue_y:PCdouble):png_uint_32; cdecl; external LibPng;
function png_get_cHRM_fixed(png_ptr:png_structp; info_ptr:png_infop; int_white_x:Ppng_fixed_point; int_white_y:Ppng_fixed_point; int_red_x:Ppng_fixed_point;
           int_red_y:Ppng_fixed_point; int_green_x:Ppng_fixed_point; int_green_y:Ppng_fixed_point; int_blue_x:Ppng_fixed_point; int_blue_y:Ppng_fixed_point):png_uint_32; cdecl; external LibPng;
procedure png_set_cHRM(png_ptr:png_structp; info_ptr:png_infop; white_x:cdouble; white_y:cdouble; red_x:cdouble;
            red_y:cdouble; green_x:cdouble; green_y:cdouble; blue_x:cdouble; blue_y:cdouble); cdecl; external LibPng;
procedure png_set_cHRM_fixed(png_ptr:png_structp; info_ptr:png_infop; int_white_x:png_fixed_point; int_white_y:png_fixed_point; int_red_x:png_fixed_point;
            int_red_y:png_fixed_point; int_green_x:png_fixed_point; int_green_y:png_fixed_point; int_blue_x:png_fixed_point; int_blue_y:png_fixed_point); cdecl; external LibPng;
function png_get_gAMA(png_ptr:png_structp; info_ptr:png_infop; file_gamma:PCdouble):png_uint_32; cdecl; external LibPng;
function png_get_gAMA_fixed(png_ptr:png_structp; info_ptr:png_infop; int_file_gamma:Ppng_fixed_point):png_uint_32; cdecl; external LibPng;
procedure png_set_gAMA(png_ptr:png_structp; info_ptr:png_infop; file_gamma:cdouble); cdecl; external LibPng;
procedure png_set_gAMA_fixed(png_ptr:png_structp; info_ptr:png_infop; int_file_gamma:png_fixed_point); cdecl; external LibPng;
function png_get_hIST(png_ptr:png_structp; info_ptr:png_infop; hist:Ppng_uint_16p):png_uint_32; cdecl; external LibPng;
procedure png_set_hIST(png_ptr:png_structp; info_ptr:png_infop; hist:png_uint_16p); cdecl; external LibPng;
function png_get_IHDR(png_ptr:png_structp; info_ptr:png_infop; width:Ppng_uint_32; height:Ppng_uint_32; bit_depth:PCint;
           color_type:PCint; interlace_type:PCint; compression_type:PCint; filter_type:PCint):png_uint_32; cdecl; external LibPng;
procedure png_set_IHDR(png_ptr:png_structp; info_ptr:png_infop; width:png_uint_32; height:png_uint_32; bit_depth:cint;
            color_type:cint; interlace_type:cint; compression_type:cint; filter_type:cint); cdecl; external LibPng;
function png_get_oFFs(png_ptr:png_structp; info_ptr:png_infop; offset_x:Ppng_int_32; offset_y:Ppng_int_32; unit_type:PCint):png_uint_32; cdecl; external LibPng;
procedure png_set_oFFs(png_ptr:png_structp; info_ptr:png_infop; offset_x:png_int_32; offset_y:png_int_32; unit_type:cint); cdecl; external LibPng;
function png_get_pCAL(png_ptr:png_structp; info_ptr:png_infop; purpose:Ppng_charp; X0:Ppng_int_32; X1:Ppng_int_32;
           atype:PCint; nparams:PCint; units:Ppng_charp; params:Ppng_charpp):png_uint_32; cdecl; external LibPng;
procedure png_set_pCAL(png_ptr:png_structp; info_ptr:png_infop; purpose:png_charp; X0:png_int_32; X1:png_int_32;
            atype:cint; nparams:cint; units:png_charp; params:png_charpp); cdecl; external LibPng;
function png_get_pHYs(png_ptr:png_structp; info_ptr:png_infop; res_x:Ppng_uint_32; res_y:Ppng_uint_32; unit_type:PCint):png_uint_32; cdecl; external LibPng;
procedure png_set_pHYs(png_ptr:png_structp; info_ptr:png_infop; res_x:png_uint_32; res_y:png_uint_32; unit_type:cint); cdecl; external LibPng;
function png_get_PLTE(png_ptr:png_structp; info_ptr:png_infop; palette:Ppng_colorp; num_palette:PCint):png_uint_32; cdecl; external LibPng;
procedure png_set_PLTE(png_ptr:png_structp; info_ptr:png_infop; palette:png_colorp; num_palette:cint); cdecl; external LibPng;
function png_get_sBIT(png_ptr:png_structp; info_ptr:png_infop; sig_bit:Ppng_color_8p):png_uint_32; cdecl; external LibPng;
procedure png_set_sBIT(png_ptr:png_structp; info_ptr:png_infop; sig_bit:png_color_8p); cdecl; external LibPng;
function png_get_sRGB(png_ptr:png_structp; info_ptr:png_infop; intent:PCint):png_uint_32; cdecl; external LibPng;
procedure png_set_sRGB(png_ptr:png_structp; info_ptr:png_infop; intent:cint); cdecl; external LibPng;
procedure png_set_sRGB_gAMA_and_cHRM(png_ptr:png_structp; info_ptr:png_infop; intent:cint); cdecl; external LibPng;
function png_get_iCCP(png_ptr:png_structp; info_ptr:png_infop; name:png_charpp; compression_type:PCint; profile:png_charpp;
           proflen:Ppng_uint_32):png_uint_32; cdecl; external LibPng;
procedure png_set_iCCP(png_ptr:png_structp; info_ptr:png_infop; name:png_charp; compression_type:cint; profile:png_charp;
            proflen:png_uint_32); cdecl; external LibPng;
function png_get_sPLT(png_ptr:png_structp; info_ptr:png_infop; entries:png_sPLT_tpp):png_uint_32; cdecl; external LibPng;
procedure png_set_sPLT(png_ptr:png_structp; info_ptr:png_infop; entries:png_sPLT_tp; nentries:cint); cdecl; external LibPng;

(* png_get_text also returns the number of text chunks in *num_text *)
function png_get_text(png_ptr:png_structp; info_ptr:png_infop; text_ptr:Ppng_textp; num_text:PCint):png_uint_32; cdecl; external LibPng;

(*
 *  Note while png_set_text() will accept a structure whose text,
 *  language, and  translated keywords are NULL pointers, the structure
 *  returned by png_get_text will always contain regular
 *  zero-terminated C strings.  They might be empty strings but
 *  they will never be NULL pointers.
 *)
procedure png_set_text(png_ptr:png_structp; info_ptr:png_infop; text_ptr:png_textp; num_text:cint); cdecl; external LibPng;

function png_get_tIME(png_ptr:png_structp; info_ptr:png_infop; mod_time:Ppng_timep):png_uint_32; cdecl; external LibPng;
procedure png_set_tIME(png_ptr:png_structp; info_ptr:png_infop; mod_time:png_timep); cdecl; external LibPng;
function png_get_tRNS(png_ptr:png_structp; info_ptr:png_infop; trans:Ppng_bytep; num_trans:PCint; trans_values:Ppng_color_16p):png_uint_32; cdecl; external LibPng;
procedure png_set_tRNS(png_ptr:png_structp; info_ptr:png_infop; trans:png_bytep; num_trans:cint; trans_values:png_color_16p); cdecl; external LibPng;
function png_get_sCAL(png_ptr:png_structp; info_ptr:png_infop; aunit:PCint; width:PCdouble; height:PCdouble):png_uint_32; cdecl; external LibPng;
procedure png_set_sCAL(png_ptr:png_structp; info_ptr:png_infop; aunit:cint; width:cdouble; height:cdouble); cdecl; external LibPng;
procedure png_set_sCAL_s(png_ptr:png_structp; info_ptr:png_infop; aunit:cint; swidth:png_charp; sheight:png_charp); cdecl; external LibPng;

procedure png_set_keep_unknown_chunks(png_ptr:png_structp; keep:cint; chunk_list:png_bytep; num_chunks:cint); cdecl; external LibPng;
procedure png_set_unknown_chunks(png_ptr:png_structp; info_ptr:png_infop; unknowns:png_unknown_chunkp; num_unknowns:cint); cdecl; external LibPng;
procedure png_set_unknown_chunk_location(png_ptr:png_structp; info_ptr:png_infop; chunk:cint; location:cint); cdecl; external LibPng;
function png_get_unknown_chunks(png_ptr:png_structp; info_ptr:png_infop; entries:png_unknown_chunkpp):png_uint_32; cdecl; external LibPng;

procedure png_set_invalid(png_ptr:png_structp; info_ptr:png_infop; mask:cint); cdecl; external LibPng;

procedure png_read_png(png_ptr:png_structp; info_ptr:png_infop; transforms:cint; params:png_voidp); cdecl; external LibPng;
procedure png_write_png(png_ptr:png_structp; info_ptr:png_infop; transforms:cint; params:png_voidp); cdecl; external LibPng;

function png_get_header_ver(png_ptr:png_structp):png_charp; cdecl; external LibPng;
function png_get_header_version(png_ptr:png_structp):png_charp; cdecl; external LibPng;
function png_get_libpng_ver(png_ptr:png_structp):png_charp; cdecl; external LibPng;

implementation

end.
