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
 * avutil version 51.73.101
 *
 *)

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
 * @param alloc when 1 then the old value will be av_freed() and the
 *                     new av_strduped()
 *              when 0 then no av_free() nor av_strdup() will be used
 * @return 0 if the value has been set, or an AVERROR code in case of
 * error:
 * AVERROR(ENOENT) if no matching option exists
 * AVERROR(ERANGE) if the value is out of range
 * AVERROR(EINVAL) if the value is not valid
 *)
function av_set_string3(obj: Pointer; name: {const} PAnsiChar; val: {const} PAnsiChar; alloc: cint; out o_out: {const} PAVOption): cint;
  cdecl; external av__util; deprecated;

function av_set_double(obj: pointer; name: {const} PAnsiChar; n: cdouble):     PAVOption;
  cdecl; external av__util; deprecated;
function av_set_q     (obj: pointer; name: {const} PAnsiChar; n: TAVRational): PAVOption;
  cdecl; external av__util; deprecated;
function av_set_int   (obj: pointer; name: {const} PAnsiChar; n: cint64):      PAVOption;
  cdecl; external av__util; deprecated;

function av_get_double(obj: pointer; name: {const} PAnsiChar; out o_out: {const} PAVOption): cdouble;
  cdecl; external av__util;
function av_get_q     (obj: pointer; name: {const} PAnsiChar; out o_out: {const} PAVOption): TAVRational;
  cdecl; external av__util;
function av_get_int   (obj: pointer; name: {const} PAnsiChar; out o_out: {const} PAVOption): cint64;
  cdecl; external av__util;
function av_get_string(obj: pointer; name: {const} PAnsiChar; out o_out: {const} PAVOption; buf: PAnsiChar; buf_len: cint): PAnsiChar;
  cdecl; external av__util; deprecated;
function av_next_option(obj: pointer; last: {const} PAVOption): PAVOption;
  cdecl; external av__util; deprecated;

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

procedure av_opt_set_defaults2(s: Pointer; mask: cint; flags: cint);
  cdecl; external av__util; deprecated;

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
function av_opt_eval_flags (obj: pointer; o: {const} PAVOption; val: {const} PAnsiChar; flags_out:  Pcint):       cint;
  cdecl; external av__util;
function av_opt_eval_int   (obj: pointer; o: {const} PAVOption; val: {const} PAnsiChar; int_out:    Pcint):       cint;
  cdecl; external av__util;
function av_opt_eval_int64 (obj: pointer; o: {const} PAVOption; val: {const} PAnsiChar; int64_out:  Pcint64):     cint;
  cdecl; external av__util;
function av_opt_eval_float (obj: pointer; o: {const} PAVOption; val: {const} PAnsiChar; float_out:  Pcfloat):     cint;
  cdecl; external av__util;
function av_opt_eval_double(obj: pointer; o: {const} PAVOption; val: {const} PAnsiChar; double_out: Pcdouble):    cint;
  cdecl; external av__util;
function av_opt_eval_q     (obj: pointer; o: {const} PAVOption; val: {const} PAnsiChar; q_out:      PAVRational): cint;
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
                      opt_flags: cint; search_flags: cint; out target_obj: pointer): {const} PAVOption;
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
function av_opt_next(obj: pointer; prev: {const} PAVOption): {const} PAVOption;
  cdecl; external av__util;

(**
 * Iterate over AVOptions-enabled children of obj.
 *
 * @param prev result of a previous call to this function or NULL
 * @return next AVOptions-enabled child or NULL
 *)
function av_opt_child_next(obj: pointer; prev: pointer): pointer;
  cdecl; external av__util;

(**
 * Iterate over potential AVOptions-enabled children of parent.
 *
 * @param prev result of a previous call to this function or NULL
 * @return AVClass corresponding to next potential child or NULL
 *)
function av_opt_child_class_next(parent: {const} PAVClass; prev: {const} PAVClass): {const} PAVClass;
  cdecl; external av__util;

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
function av_opt_set_int   (obj: pointer; name: {const} PAnsiChar; val: cint64;            search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_double(obj: pointer; name: {const} PAnsiChar; val: cdouble;           search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_q     (obj: pointer; name: {const} PAnsiChar; val: TAVRational;       search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_bin   (obj: pointer; name: {const} PAnsiChar; val: {const} cuint8;    search_flags: cint): cint;
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
function av_opt_get       (obj: pointer; name: {const} PAnsiChar; search_flags: cint; out out_val: Pcuint8):     cint;
  cdecl; external av__util;
function av_opt_get_int   (obj: pointer; name: {const} PAnsiChar; search_flags: cint;     out_val: Pcint64):     cint;
  cdecl; external av__util;
function av_opt_get_double(obj: pointer; name: {const} PAnsiChar; search_flags: cint;     out_val: Pcdouble):    cint;
  cdecl; external av__util;
function av_opt_get_q     (obj: pointer; name: {const} PAnsiChar; search_flags: cint;     out_val: PAVRational): cint;
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
function av_opt_ptr(avclass: {const} PAVClass; obj: pointer; name: {const} PAnsiChar): pointer;
  cdecl; external av__util;
(**
 * @}
 *)
