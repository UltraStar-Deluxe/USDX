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
function av_opt_get       (obj: pointer; name: {const} PAnsiChar; search_flags: cint; outval: Pcuint8): cint;
  cdecl; external av__util;
function av_opt_get_int   (obj: pointer; name: {const} PAnsiChar; search_flags: cint; outval: Pcint64): cint;
  cdecl; external av__util;
function av_opt_get_double(obj: pointer; name: {const} PAnsiChar; search_flags: cint; outval: Pcdouble): cint;
  cdecl; external av__util;
function av_opt_get_q     (obj: pointer; name: {const} PAnsiChar; search_flags: cint; outval: PAVRational): cint;
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
