(*
 * AVOptions
 * copyright (c) 2005 Michael Niedermayer <michaelni@gmx.at>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 * This is a part of Pascal porting of ffmpeg.
 * - Originally by Victor Zinetz for Delphi and Free Pascal on Windows.
 * - For Mac OS X, some modifications were made by The Creative CAT, denoted as CAT
 *   in the source codes.
 * - Changes and updates by the UltraStar Deluxe Team
 *
 * Conversion of libavutil/opt.h
 * avutil version 51.34.101
 *
 *)

unit opt;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKENUM 4}    (* use 4-byte enums *)
  {$PACKRECORDS C} (* C/C++-compatible record packing *)
{$ELSE}
  {$MINENUMSIZE 4} (* use 4-byte enums *)
{$ENDIF}

interface

uses
  ctypes,
  dict,
  rational,
  UConfig;

(**
 * @defgroup avoptions AVOptions
 * @ingroup lavu_data
 * @{
 * AVOptions provide a generic system to declare options on arbitrary structs
 * ("objects"). An option can have a help text, a type and a range of possible
 * values. Options may then be enumerated, read and written to.
 *
 * @section avoptions_implement Implementing AVOptions
 * This section describes how to add AVOptions capabilities to a struct.
 *
 * All AVOptions-related information is stored in an AVClass. Therefore
 * the first member of the struct must be a pointer to an AVClass describing it.
 * The option field of the AVClass must be set to a NULL-terminated static array
 * of AVOptions. Each AVOption must have a non-empty name, a type, a default
 * value and for number-type AVOptions also a range of allowed values. It must
 * also declare an offset in bytes from the start of the struct, where the field
 * associated with this AVOption is located. Other fields in the AVOption struct
 * should also be set when applicable, but are not required.
 *
 * The following example illustrates an AVOptions-enabled struct:
 * @code
 * typedef struct test_struct {
 *     AVClass *class;
 *     int      int_opt;
 *     char    *str_opt;
 *     uint8_t *bin_opt;
 *     int      bin_len;
 * } test_struct;
 *
 * static const AVOption options[] = {
 *   { "test_int", "This is a test option of int type.", offsetof(test_struct, int_opt),
 *     AV_OPT_TYPE_INT, { -1 }, INT_MIN, INT_MAX },
 *   { "test_str", "This is a test option of string type.", offsetof(test_struct, str_opt),
 *     AV_OPT_TYPE_STRING },
 *   { "test_bin", "This is a test option of binary type.", offsetof(test_struct, bin_opt),
 *     AV_OPT_TYPE_BINARY },
 *   { NULL },
 * };
 *
 * static const AVClass test_class = {
 *     .class_name = "test class",
 *     .item_name  = av_default_item_name,
 *     .option     = options,
 *     .version    = LIBAVUTIL_VERSION_INT,
 * };
 * @endcode
 *
 * Next, when allocating your struct, you must ensure that the AVClass pointer
 * is set to the correct value. Then, av_opt_set_defaults() must be called to
 * initialize defaults. After that the struct is ready to be used with the
 * AVOptions API.
 *
 * When cleaning up, you may use the av_opt_free() function to automatically
 * free all the allocated string and binary options.
 *
 * Continuing with the above example:
 *
 * @code
 * test_struct *alloc_test_struct(void)
 * {
 *     test_struct *ret = av_malloc(sizeof(*ret));
 *     ret->class = &test_class;
 *     av_opt_set_defaults(ret);
 *     return ret;
 * }
 * void free_test_struct(test_struct **foo)
 * {
 *     av_opt_free(*foo);
 *     av_freep(foo);
 * }
 * @endcode
 *
 * @subsection avoptions_implement_nesting Nesting
 *      It may happen that an AVOptions-enabled struct contains another
 *      AVOptions-enabled struct as a member (e.g. AVCodecContext in
 *      libavcodec exports generic options, while its priv_data field exports
 *      codec-specific options). In such a case, it is possible to set up the
 *      parent struct to export a child's options. To do that, simply
 *      implement AVClass.child_next() and AVClass.child_class_next() in the
 *      parent struct's AVClass.
 *      Assuming that the test_struct from above now also contains a
 *      child_struct field:
 *
 *      @code
 *      typedef struct child_struct {
 *          AVClass *class;
 *          int flags_opt;
 *      } child_struct;
 *      static const AVOption child_opts[] = {
 *          { "test_flags", "This is a test option of flags type.",
 *            offsetof(child_struct, flags_opt), AV_OPT_TYPE_FLAGS, { 0 }, INT_MIN, INT_MAX },
 *          { NULL },
 *      };
 *      static const AVClass child_class = {
 *          .class_name = "child class",
 *          .item_name  = av_default_item_name,
 *          .option     = child_opts,
 *          .version    = LIBAVUTIL_VERSION_INT,
 *      };
 *
 *      void *child_next(void *obj, void *prev)
 *      {
 *          test_struct *t = obj;
 *          if (!prev && t->child_struct)
 *              return t->child_struct;
 *          return NULL
 *      }
 *      const AVClass child_class_next(const AVClass *prev)
 *      {
 *          return prev ? NULL : &child_class;
 *      }
 *      @endcode
 *      Putting child_next() and child_class_next() as defined above into
 *      test_class will now make child_struct's options accessible through
 *      test_struct (again, proper setup as described above needs to be done on
 *      child_struct right after it is created).
 *
 *      From the above example it might not be clear why both child_next()
 *      and child_class_next() are needed. The distinction is that child_next()
 *      iterates over actually existing objects, while child_class_next()
 *      iterates over all possible child classes. E.g. if an AVCodecContext
 *      was initialized to use a codec which has private options, then its
 *      child_next() will return AVCodecContext.priv_data and finish
 *      iterating. OTOH child_class_next() on AVCodecContext.av_class will
 *      iterate over all available codecs with private options.
 *
 * @subsection avoptions_implement_named_constants Named constants
 *      It is possible to create named constants for options. Simply set the unit
 *      field of the option the constants should apply to to a string and
 *      create the constants themselves as options of type AV_OPT_TYPE_CONST
 *      with their unit field set to the same string.
 *      Their default_val field should contain the value of the named
 *      constant.
 *      For example, to add some named constants for the test_flags option
 *      above, put the following into the child_opts array:
 *      @code
 *      { "test_flags", "This is a test option of flags type.",
 *        offsetof(child_struct, flags_opt), AV_OPT_TYPE_FLAGS, { 0 }, INT_MIN, INT_MAX, "test_unit" },
 *      { "flag1", "This is a flag with value 16", 0, AV_OPT_TYPE_CONST, { 16 }, 0, 0, "test_unit" },
 *      @endcode
 *
 * @section avoptions_use Using AVOptions
 * This section deals with accessing options in an AVOptions-enabled struct.
 * Such structs in FFmpeg are e.g. AVCodecContext in libavcodec or
 * AVFormatContext in libavformat.
 *
 * @subsection avoptions_use_examine Examining AVOptions
 * The basic functions for examining options are av_opt_next(), which iterates
 * over all options defined for one object, and av_opt_find(), which searches
 * for an option with the given name.
 *
 * The situation is more complicated with nesting. An AVOptions-enabled struct
 * may have AVOptions-enabled children. Passing the AV_OPT_SEARCH_CHILDREN flag
 * to av_opt_find() will make the function search children recursively.
 *
 * For enumerating there are basically two cases. The first is when you want to
 * get all options that may potentially exist on the struct and its children
 * (e.g.  when constructing documentation). In that case you should call
 * av_opt_child_class_next() recursively on the parent struct's AVClass.  The
 * second case is when you have an already initialized struct with all its
 * children and you want to get all options that can be actually written or read
 * from it. In that case you should call av_opt_child_next() recursively (and
 * av_opt_next() on each result).
 *
 * @subsection avoptions_use_get_set Reading and writing AVOptions
 * When setting options, you often have a string read directly from the
 * user. In such a case, simply passing it to av_opt_set() is enough. For
 * non-string type options, av_opt_set() will parse the string according to the
 * option type.
 *
 * Similarly av_opt_get() will read any option type and convert it to a string
 * which will be returned. Do not forget that the string is allocated, so you
 * have to free it with av_free().
 *
 * In some cases it may be more convenient to put all options into an
 * AVDictionary and call av_opt_set_dict() on it. A specific case of this
 * are the format/codec open functions in lavf/lavc which take a dictionary
 * filled with option as a parameter. This allows to set some options
 * that cannot be set otherwise, since e.g. the input file format is not known
 * before the file is actually opened.
 *)

type
  TAVOptionType = (
    AV_OPT_TYPE_FLAGS,
    AV_OPT_TYPE_INT,
    AV_OPT_TYPE_INT64,
    AV_OPT_TYPE_DOUBLE,
    AV_OPT_TYPE_FLOAT,
    AV_OPT_TYPE_STRING,
    AV_OPT_TYPE_RATIONAL,
    AV_OPT_TYPE_BINARY,  ///< offset must point to a pointer immediately followed by an int for the length
    AV_OPT_TYPE_CONST = 128
{$IFDEF FF_API_OLD_AVOPTIONS}
    FF_OPT_TYPE_FLAGS,
    FF_OPT_TYPE_INT,
    FF_OPT_TYPE_INT64,
    FF_OPT_TYPE_DOUBLE,
    FF_OPT_TYPE_FLOAT,
    FF_OPT_TYPE_STRING,
    FF_OPT_TYPE_RATIONAL,
    FF_OPT_TYPE_BINARY,  ///< offset must point to a pointer immediately followed by an int for the length
    FF_OPT_TYPE_CONST = 128
{$ENDIF}
  );

const
  AV_OPT_FLAG_ENCODING_PARAM  = 1;   ///< a generic parameter which can be set by the user for muxing or encoding
  AV_OPT_FLAG_DECODING_PARAM  = 2;   ///< a generic parameter which can be set by the user for demuxing or decoding
  AV_OPT_FLAG_METADATA        = 4;   ///< some data extracted or inserted into the file like title, comment, ...
  AV_OPT_FLAG_AUDIO_PARAM     = 8;
  AV_OPT_FLAG_VIDEO_PARAM     = 16;
  AV_OPT_FLAG_SUBTITLE_PARAM  = 32;

type
  (**
   * AVOption
   *)
  PAVOption = ^TAVOption;
  TAVOption = record
    name: {const} PAnsiChar;
    
    (**
     * short English help text
     * @todo What about other languages?
     *)
    help: {const} PAnsiChar;

    (**
     * The offset relative to the context structure where the option
     * value is stored. It should be 0 for named constants.
     *)
    offset: cint;
    type_: TAVOptionType;

    (**
     * the default value for scalar options
     *)
    default_val: record
      case cint of
        0: (dbl: cdouble);
        1: (str: PAnsiChar);
        (* TODO those are unused now *)
        2: (i64: cint64);
        3: (q: TAVRational);
      end;
    min: cdouble;                ///< minimum valid value for the option
    max: cdouble;                ///< maximum valid value for the option

    flags: cint;
//FIXME think about enc-audio, ... style flags

    (**
     * The logical unit to which the option belongs. Non-constant
     * options and corresponding named constants share the same
     * unit. May be NULL.
     *)
    unit_: {const} PAnsiChar;
  end;

{$IFDEF FF_API_FIND_OPT}
(**
 * Look for an option in obj. Look only for the options which
 * have the flags set as specified in mask and flags (that is,
 * for which it is the case that opt->flags & mask == flags).
 *
 * @param[in] obj a pointer to a struct whose first element is a
 * pointer to an AVClass
 * @param[in] name the name of the option to look for
 * @param[in] unit the unit of the option to look for, or any if NULL
 * @return a pointer to the option found, or NULL if no option
 * has been found
 *)
function av_find_opt(obj: Pointer; {const} name: {const} PAnsiChar; {const} unit_: PAnsiChar; mask: cint; flags: cint): {const} PAVOption;
  cdecl; external av__util; deprecated;
{$ENDIF}

{$IFDEF FF_API_OLD_AVOPTIONS}
(**
 * Set the field of obj with the given name to value.
 *
 * @param[in] obj A struct whose first element is a pointer to an
 * AVClass.
 * @param[in] name the name of the field to set
 * @param[in] val The value to set. If the field is not of a string
 * type, then the given string is parsed.
 * SI postfixes and some named scalars are supported.
 * If the field is of a numeric type, it has to be a numeric or named
 * scalar. Behavior with more than one scalar and +- infix operators
 * is undefined.
 * If the field is of a flags type, it has to be a sequence of numeric
 * scalars or named flags separated by '+' or '-'. Prefixing a flag
 * with '+' causes it to be set without affecting the other flags;
 * similarly, '-' unsets a flag.
 * @param[out] o_out if non-NULL put here a pointer to the AVOption
 * found
 * @param alloc this parameter is currently ignored
 * @return 0 if the value has been set, or an AVERROR code in case of
 * error:
 * AVERROR_OPTION_NOT_FOUND if no matching option exists
 * AVERROR(ERANGE) if the value is out of range
 * AVERROR(EINVAL) if the value is not valid
 *)
function av_set_string3(obj: Pointer; name: {const} PAnsiChar; val: {const} PAnsiChar; alloc: cint; out o_out: {const} PAVOption): cint;
  cdecl; external av__util; deprecated;

function av_set_double(obj: pointer; name: {const} PAnsiChar; n: cdouble): PAVOption;
  cdecl; external av__util; deprecated;
function av_set_q(obj: pointer; name: {const} PAnsiChar; n: TAVRational): PAVOption;
  cdecl; external av__util; deprecated;
function av_set_int(obj: pointer; name: {const} PAnsiChar; n: cint64): PAVOption;
  cdecl; external av__util; deprecated;
function av_get_double(obj: pointer; name: {const} PAnsiChar; var o_out: {const} PAVOption): cdouble;
  cdecl; external av__util; deprecated;
function av_get_q(obj: pointer; name: {const} PAnsiChar; var o_out: {const} PAVOption): TAVRational;
  cdecl; external av__util; deprecated;
function av_get_int(obj: pointer; name: {const} PAnsiChar; var o_out: {const} PAVOption): cint64;
  cdecl; external av__util; deprecated;
function av_get_string(obj: pointer; name: {const} PAnsiChar; var o_out: {const} PAVOption; buf: PAnsiChar; buf_len: cint): PAnsiChar;
  cdecl; external av__util; deprecated;
function av_next_option(obj: pointer; last: {const} PAVOption): PAVOption;
  cdecl; external av__util; deprecated;
{$ENDIF}

(**
 * Show the obj options.
 *
 * @param req_flags requested flags for the options to show. Show only the
 * options for which it is opt->flags & req_flags.
 * @param rej_flags rejected flags for the options to show. Show only the
 * options for which it is !(opt->flags & req_flags).
 * @param av_log_obj log context to use for showing the options
 *)
function av_opt_show2(obj: pointer; av_log_obj: pointer; req_flags: cint; rej_flags: cint): cint;
  cdecl; external av__util;

(**
 * Set the values of all AVOption fields to their default values.
 *
 * @param s an AVOption-enabled struct (its first member must be a pointer to AVClass)
 *)
procedure av_opt_set_defaults(s: pointer);
  cdecl; external av__util;

{$IFDEF FF_API_OLD_AVOPTIONS}
procedure av_opt_set_defaults2(s: Pointer; mask: cint; flags: cint);
  cdecl; external av__util; deprecated;
{$ENDIF}

(**
 * Parse the key/value pairs list in opts. For each key/value pair
 * found, stores the value in the field in ctx that is named like the
 * key. ctx must be an AVClass context, storing is done using
 * AVOptions.
 *
 * @param opts options string to parse, may be NULL
 * @param key_val_sep a 0-terminated list of characters used to
 * separate key from value
 * @param pairs_sep a 0-terminated list of characters used to separate
 * two pairs from each other
 * @return the number of successfully set key/value pairs, or a negative
 * value corresponding to an AVERROR code in case of error:
 * AVERROR(EINVAL) if opts cannot be parsed,
 * the error code issued by av_set_string3() if a key/value pair
 * cannot be set
*)
function av_set_options_string(ctx: pointer; opts: {const} PAnsiChar;
                      key_val_sep: {const} PAnsiChar; pairs_sep: {const} PAnsiChar): cint;
  cdecl; external av__util;

(**
 * Free all string and binary options in obj.
 *)
procedure av_opt_free(obj: pointer);
  cdecl; external av__util;

(**
 * Check whether a particular flag is set in a flags field.
 *
 * @param field_name the name of the flag field option
 * @param flag_name the name of the flag to check
 * @return non-zero if the flag is set, zero if the flag isn't set,
 *         isn't of the right type, or the flags field doesn't exist.
 *)
function av_opt_flag_is_set(obj: pointer; field_name: {const} PAnsiChar; flag_name: {const} PAnsiChar): cint;
  cdecl; external av__util;

(**
 * Set all the options from a given dictionary on an object.
 *
 * @param obj a struct whose first element is a pointer to AVClass
 * @param options options to process. This dictionary will be freed and replaced
 *                by a new one containing all options not found in obj.
 *                Of course this new dictionary needs to be freed by caller
 *                with av_dict_free().
 *
 * @return 0 on success, a negative AVERROR if some option was found in obj,
 *         but could not be set.
 *
 * @see av_dict_copy()
 *)
function av_opt_set_dict(obj: pointer; var options: PAVDictionary): cint;
  cdecl; external av__util;

(**
 * @defgroup opt_eval_funcs Evaluating option strings
 * @{
 * This group of functions can be used to evaluate option strings
 * and get numbers out of them. They do the same thing as av_opt_set(),
 * except the result is written into the caller-supplied pointer.
 *
 * @param obj a struct whose first element is a pointer to AVClass.
 * @param o an option for which the string is to be evaluated.
 * @param val string to be evaluated.
 * @param *_out value of the string will be written here.
 *
 * @return 0 on success, a negative number on failure.
 *)
function av_opt_eval_flags (obj: pointer; o: {const} PAVOption; {const} val: PAnsiChar; flags_out:  Pcint): cint;
  cdecl; external av__util;
function av_opt_eval_int   (obj: pointer; o: {const} PAVOption; {const} val: PAnsiChar; int_out:    Pcint): cint;
  cdecl; external av__util;
function av_opt_eval_int64 (obj: pointer; o: {const} PAVOption; {const} val: PAnsiChar; int64_out:  Pint64): cint;
  cdecl; external av__util;
function av_opt_eval_float (obj: pointer; o: {const} PAVOption; {const} val: PAnsiChar; float_out:  Pcfloat): cint;
  cdecl; external av__util;
function av_opt_eval_double(obj: pointer; o: {const} PAVOption; {const} val: PAnsiChar; double_out: Pcdouble): cint;
  cdecl; external av__util;
function av_opt_eval_q     (obj: pointer; o: {const} PAVOption; {const} val: PAnsiChar; q_out:      PAVRational): cint;
  cdecl; external av__util;
(**
 * @}
 *)

const
  AV_OPT_SEARCH_CHILDREN = 0001; (**< Search in possible children of the
                                      given object first.*)
(**
 *  The obj passed to av_opt_find() is fake -- only a double pointer to AVClass
 *  instead of a required pointer to a struct containing AVClass. This is
 *  useful for searching for options without needing to allocate the corresponding
 *  object.
 *)
  AV_OPT_SEARCH_FAKE_OBJ = 0002;

(**
 * Look for an option in an object. Consider only options which
 * have all the specified flags set.
 *
 * @param[in] obj A pointer to a struct whose first element is a
 *                pointer to an AVClass.
 * @param[in] name The name of the option to look for.
 * @param[in] unit When searching for named constants, name of the unit
 *                 it belongs to.
 * @param opt_flags Find only options with all the specified flags set (AV_OPT_FLAG).
 * @param search_flags A combination of AV_OPT_SEARCH_*.
 *
 * @return A pointer to the option found, or NULL if no option
 *         was found.
 *
 * @note Options found with AV_OPT_SEARCH_CHILDREN flag may not be settable
 * directly with av_set_string3(). Use special calls which take an options
 * AVDictionary (e.g. avformat_open_input()) to set options found with this
 * flag.
 *)
function av_opt_find(obj: pointer; name: {const} PAnsiChar; unit_: {const} PAnsiChar;
                     opt_flags: cint; search_flags: cint): PAVOption;
  cdecl; external av__util;

(**
 * Look for an option in an object. Consider only options which
 * have all the specified flags set.
 *
 * @param[in] obj A pointer to a struct whose first element is a
 *                pointer to an AVClass.
 *                Alternatively a double pointer to an AVClass, if
 *                AV_OPT_SEARCH_FAKE_OBJ search flag is set.
 * @param[in] name The name of the option to look for.
 * @param[in] unit When searching for named constants, name of the unit
 *                 it belongs to.
 * @param opt_flags Find only options with all the specified flags set (AV_OPT_FLAG).
 * @param search_flags A combination of AV_OPT_SEARCH_*.
 * @param[out] target_obj if non-NULL, an object to which the option belongs will be
 * written here. It may be different from obj if AV_OPT_SEARCH_CHILDREN is present
 * in search_flags. This parameter is ignored if search_flags contain
 * AV_OPT_SEARCH_FAKE_OBJ.
 *
 * @return A pointer to the option found, or NULL if no option
 *         was found.
 *)
function av_opt_find2(obj: pointer; name: {const} PAnsiChar; unit_: {const} PAnsiChar;
                      opt_flags: cint; search_flags: cint; target_obj: pointer): PAVOption;
  cdecl; external av__util;

(**
 * Iterate over all AVOptions belonging to obj.
 *
 * @param obj an AVOptions-enabled struct or a double pointer to an
 *            AVClass describing it.
 * @param prev result of the previous call to av_opt_next() on this object
 *             or NULL
 * @return next AVOption or NULL
 *)
function av_opt_next(obj: pointer; prev: {const} PAVOption): PAVOption;
  cdecl; external av__util;

(**
 * Iterate over AVOptions-enabled children of obj.
 *
 * @param prev result of a previous call to this function or NULL
 * @return next AVOptions-enabled child or NULL
 *)
procedure av_opt_child_next(obj: pointer; prev: pointer);
  cdecl; external av__util;

(**
 * Iterate over potential AVOptions-enabled children of parent.
 *
 * @param prev result of a previous call to this function or NULL
 * @return AVClass corresponding to next potential child or NULL
 *)
(*
function av_opt_child_class_next(parent: {const} PAVClass; prev: {const} PAVClass): PAVClass;
  cdecl; external av__util;
*)

(**
 * @defgroup opt_set_funcs Option setting functions
 * @{
 * Those functions set the field of obj with the given name to value.
 *
 * @param[in] obj A struct whose first element is a pointer to an AVClass.
 * @param[in] name the name of the field to set
 * @param[in] val The value to set. In case of av_opt_set() if the field is not
 * of a string type, then the given string is parsed.
 * SI postfixes and some named scalars are supported.
 * If the field is of a numeric type, it has to be a numeric or named
 * scalar. Behavior with more than one scalar and +- infix operators
 * is undefined.
 * If the field is of a flags type, it has to be a sequence of numeric
 * scalars or named flags separated by '+' or '-'. Prefixing a flag
 * with '+' causes it to be set without affecting the other flags;
 * similarly, '-' unsets a flag.
 * @param search_flags flags passed to av_opt_find2. I.e. if AV_OPT_SEARCH_CHILDREN
 * is passed here, then the option may be set on a child of obj.
 *
 * @return 0 if the value has been set, or an AVERROR code in case of
 * error:
 * AVERROR_OPTION_NOT_FOUND if no matching option exists
 * AVERROR(ERANGE) if the value is out of range
 * AVERROR(EINVAL) if the value is not valid
 *)
function av_opt_set       (obj: pointer; name: {const} PAnsiChar; val: {const} PAnsiChar; search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_int   (obj: pointer; name: {const} PAnsiChar; val: cint64; search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_double(obj: pointer; name: {const} PAnsiChar; val: cdouble; search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_q     (obj: pointer; name: {const} PAnsiChar; val: PAVRational; search_flags: cint): cint;
  cdecl; external av__util;
(**
 * @}
 *)

(**
 * @defgroup opt_get_funcs Option getting functions
 * @{
 * Those functions get a value of the option with the given name from an object.
 *
 * @param[in] obj a struct whose first element is a pointer to an AVClass.
 * @param[in] name name of the option to get.
 * @param[in] search_flags flags passed to av_opt_find2. I.e. if AV_OPT_SEARCH_CHILDREN
 * is passed here, then the option may be found in a child of obj.
 * @param[out] out_val value of the option will be written here
 * @return 0 on success, a negative error code otherwise
 *)
(**
 * @note the returned string will av_malloc()ed and must be av_free()ed by the caller
 *)
function av_opt_get       (obj: pointer; name: {const} PAnsiChar; search_flags: cint; out_val: Pcuint8): cint;
  cdecl; external av__util;
function av_opt_get_int   (obj: pointer; name: {const} PAnsiChar; search_flags: cint; out_val: Pcint64): cint;
  cdecl; external av__util;
function av_opt_get_double(obj: pointer; name: {const} PAnsiChar; search_flags: cint; out_val: Pcdouble): cint;
  cdecl; external av__util;
function av_opt_get_q     (obj: pointer; name: {const} PAnsiChar; search_flags: cint; var out_val: PAVRational): cint;
  cdecl; external av__util;
(**
 * @}
 *)

(**
 * Gets a pointer to the requested field in a struct.
 * This function allows accessing a struct even when its fields are moved or
 * renamed since the application making the access has been compiled,
 *
 * @returns a pointer to the field, it can be cast to the correct type and read
 *          or written to.
 *)
(*
procedure av_opt_ptr(avclass: {const} PAVClass; obj: pointer; name: {const} PAnsiChar);
  cdecl; external av__util;
*)

(**
 * @}
 *)

implementation

end.