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
 * avutil version 54.7.100
 *
 *)

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
 * @param alloc when 1 then the old value will be av_freed() and the
 *                     new av_strduped()
 *              when 0 then no av_free() nor av_strdup() will be used
 * @return 0 if the value has been set, or an AVERROR code in case of
 * error:
 * AVERROR_OPTION_NOT_FOUND if no matching option exists
 * AVERROR(ERANGE) if the value is out of range
 * AVERROR(EINVAL) if the value is not valid
 * @deprecated use av_opt_set()
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
 * Parse the key-value pairs list in opts. For each key=value pair found,
 * set the value of the corresponding option in ctx.
 *
 * @param ctx          the AVClass object to set options on
 * @param opts         the options string, key-value pairs separated by a
 *                     delimiter
 * @param shorthand    a NULL-terminated array of options names for shorthand
 *                     notation: if the first field in opts has no key part,
 *                     the key is taken from the first element of shorthand;
 *                     then again for the second, etc., until either opts is
 *                     finished, shorthand is finished or a named option is
 *                     found; after that, all options must be named
 * @param key_val_sep  a 0-terminated list of characters used to separate
 *                     key from value, for example '='
 * @param pairs_sep    a 0-terminated list of characters used to separate
 *                     two pairs from each other, for example ':' or ','
 * @return  the number of successfully set key=value pairs, or a negative
 *          value corresponding to an AVERROR code in case of error:
 *          AVERROR(EINVAL) if opts cannot be parsed,
 *          the error code issued by av_set_string3() if a key/value pair
 *          cannot be set
 *
 * Options names must use only the following characters: a-z A-Z 0-9 - . / _
 * Separators must use characters distinct from option names and from each
 * other.
 *)
function av_opt_set_from_string(ctx: pointer; opts: {const} PAnsiChar;
                           shorthand: {const} PAnsiChar;
                           key_val_sep: {const} PAnsiChar; pairs_sep: {const} PAnsiChar): cint;
  cdecl; external av__util;

(**
 * Free all allocated objects in obj.
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
 * Set all the options from a given dictionary on an object.
 *
 * @param obj a struct whose first element is a pointer to AVClass
 * @param options options to process. This dictionary will be freed and replaced
 *                by a new one containing all options not found in obj.
 *                Of course this new dictionary needs to be freed by caller
 *                with av_dict_free().
 * @param search_flags A combination of AV_OPT_SEARCH_*.
 *
 * @return 0 on success, a negative AVERROR if some option was found in obj,
 *         but could not be set.
 *
 * @see av_dict_copy()
 *)
function av_opt_set_dict2(obj: pointer; var options: PAVDictionary; search_flags: cint): cint;
  cdecl; external av__util;

(**
 * Extract a key-value pair from the beginning of a string.
 *
 * @param ropts        pointer to the options string, will be updated to
 *                     point to the rest of the string (one of the pairs_sep
 *                     or the final NUL)
 * @param key_val_sep  a 0-terminated list of characters used to separate
 *                     key from value, for example '='
 * @param pairs_sep    a 0-terminated list of characters used to separate
 *                     two pairs from each other, for example ':' or ','
 * @param flags        flags; see the AV_OPT_FLAG_* values below
 * @param rkey         parsed key; must be freed using av_free()
 * @param rval         parsed value; must be freed using av_free()
 *
 * @return  >=0 for success, or a negative value corresponding to an
 *          AVERROR code in case of error; in particular:
 *          AVERROR(EINVAL) if no key is present
 *
 *)
function av_opt_get_key_value(ropts: {const} PPAnsiChar;
                     key_val_sep: {const} PAnsiChar; pairs_sep: {const} PAnsiChar;
                     flags: byte;
                     rkey, rval: PPAnsiChar): cint;
  cdecl; external av__util;

const
  (**
   * Accept to parse a value without a key; the key will then be returned
   * as NULL.
   *)
  AV_OPT_FLAG_IMPLICIT_KEY = 1;

(**
 * @defgroup opt_eval_funcs Evaluating option strings
 * @
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
 * @
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
 *  Allows av_opt_query_ranges and av_opt_query_ranges_default to return more than
 *  one component for certain option types.
 *  @see AVOptionRanges for details.
 *)
  AV_OPT_MULTI_COMPONENT_RANGE = 1000;

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
 * @
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
function av_opt_set           (obj: pointer; name: {const} PAnsiChar; val: {const} PAnsiChar; search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_int       (obj: pointer; name: {const} PAnsiChar; val: cint64;            search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_double    (obj: pointer; name: {const} PAnsiChar; val: cdouble;           search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_q         (obj: pointer; name: {const} PAnsiChar; val: TAVRational;       search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_bin       (obj: pointer; name: {const} PAnsiChar; val: {const} cuint8;    search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_image_size(obj: pointer; name: {const} PAnsiChar; w, h,                   search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_pixel_fmt (obj: pointer; name: {const} PAnsiChar; fmt: TAVPixelFormat;    search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_sample_fmt(obj: pointer; name: {const} PAnsiChar; fmt: TAVSampleFormat;    search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_video_rate(obj: pointer; name: {const} PAnsiChar; val: TAVRational;       search_flags: cint): cint;
  cdecl; external av__util;
function av_opt_set_channel_layout(obj: pointer; name: {const} PAnsiChar; ch_layout: cint64; search_flags: cint): cint;
  cdecl; external av__util;

(**
 * @note Any old dictionary present is discarded and replaced with a copy of the new one. The
 * caller still owns val is and responsible for freeing it.
 *)
function av_opt_set_dict_val(obj: pointer; name: {const} PAnsiChar; val: {const} PAVDictionary; search_flags: cint): cint;
  cdecl; external av__util;

(**
 * Set a binary option to an integer list.
 *
 * @param obj    AVClass object to set options on
 * @param name   name of the binary option
 * @param val    pointer to an integer list (must have the correct type with
 *               regard to the contents of the list)
 * @param term   list terminator (usually 0 or -1)
 * @param flags  search flags
 *)
{to be translated
#define av_opt_set_int_list(obj, name, val, term, flags) \
    (av_int_list_length(val, term) > INT_MAX / sizeof(*(val)) ? \
     AVERROR(EINVAL) : \
     av_opt_set_bin(obj, name, (const uint8_t *)(val), \
                    av_int_list_length(val, term) * sizeof(*(val)), flags))
}
(**
 * @
 *)

(**
 * @defgroup opt_get_funcs Option getting functions
 * @
 * Those functions get a value of the option with the given name from an object.
 *
 * @param[in] obj a struct whose first element is a pointer to an AVClass.
 * @param[in] name name of the option to get.
 * @param[in] search_flags flags passed to av_opt_find2. I.e. if AV_OPT_SEARCH_CHILDREN
 * is passed here, then the option may be found in a child of obj.
 * @param[out] out_val value of the option will be written here
 * @return >=0 on success, a negative error code otherwise
 *)
(**
 * @note the returned string will be av_malloc()ed and must be av_free()ed by the caller
 *)
function av_opt_get           (obj: pointer; name: {const} PAnsiChar; search_flags: cint; out out_val: Pcuint8):     cint;
  cdecl; external av__util;
function av_opt_get_int       (obj: pointer; name: {const} PAnsiChar; search_flags: cint;     out_val: Pcint64):     cint;
  cdecl; external av__util;
function av_opt_get_double    (obj: pointer; name: {const} PAnsiChar; search_flags: cint;     out_val: Pcdouble):    cint;
  cdecl; external av__util;
function av_opt_get_q         (obj: pointer; name: {const} PAnsiChar; search_flags: cint;     out_val: PAVRational): cint;
  cdecl; external av__util;
function av_opt_get_image_size(obj: pointer; name: {const} PAnsiChar; search_flags: cint; w_out, h_out: Pcint):      cint;
  cdecl; external av__util;
function av_opt_get_pixel_fmt (obj: pointer; name: {const} PAnsiChar; search_flags: cint; out_fmt: PAVPixelFormat):  cint;
  cdecl; external av__util;
function av_opt_get_sample_fmt(obj: pointer; name: {const} PAnsiChar; search_flags: cint; out_fmt: PAVPixelFormat):  cint;
  cdecl; external av__util;
function av_opt_get_video_rate(obj: pointer; name: {const} PAnsiChar; search_flags: cint; out_val: PAVRational):     cint;
  cdecl; external av__util;
function av_opt_get_channel_layout(obj: pointer; name: {const} PAnsiChar; search_flags: cint; ch_layout: Pcint64):   cint;
  cdecl; external av__util;
(**
 * @param[out] out_val The returned dictionary is a copy of the actual value and must
 * be freed with av_dict_free() by the caller
 *)
function av_opt_get_dict_val  (obj: pointer; name: {const} PAnsiChar; search_flags: cint; out out_val: PAVDictionary): cint;
  cdecl; external av__util;
(**
 * @
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
 * Free an AVOptionRanges struct and set it to NULL.
 *)
procedure av_opt_freep_ranges(ranges: PPAVOptionRanges);
  cdecl; external av__util;

(**
 * Get a list of allowed ranges for the given option.
 *
 * The returned list may depend on other fields in obj like for example profile.
 *
 * @param flags is a bitmask of flags, undefined flags should not be set and should be ignored
 *              AV_OPT_SEARCH_FAKE_OBJ indicates that the obj is a double pointer to a AVClass instead of a full instance
 *              AV_OPT_MULTI_COMPONENT_RANGE indicates that function may return more than one component, @see AVOptionRanges
 *
 * The result must be freed with av_opt_freep_ranges.
 *
 * @return number of compontents returned on success, a negative errro code otherwise
 *)
function av_opt_query_ranges(P: PPAVOptionRanges; obj: pointer; key: {const} PAnsiChar; flags: cint): cint;
  cdecl; external av__util;

function av_opt_copy(dest: pointer;src: pointer): cint;
  cdecl; external av__util;

(**
 * Get a default list of allowed ranges for the given option.
 *
 * This list is constructed without using the AVClass.query_ranges() callback
 * and can be used as fallback from within the callback.
 *
 * @param flags is a bitmask of flags, undefined flags should not be set and should be ignored
 *              AV_OPT_SEARCH_FAKE_OBJ indicates that the obj is a double pointer to a AVClass instead of a full instance
 *              AV_OPT_MULTI_COMPONENT_RANGE indicates that function may return more than one component, @see AVOptionRanges
 *
 * The result must be freed with av_opt_free_ranges.
 *
 * @return number of compontents returned on success, a negative errro code otherwise
 *)
function av_opt_query_ranges_default(P: PPAVOptionRanges; obj: pointer; key: {const} PAnsiChar; flags: cint): cint;
  cdecl; external av__util;

(**
 * @
 *)
