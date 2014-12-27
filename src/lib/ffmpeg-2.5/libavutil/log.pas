(*
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *
 * This is a part of the Pascal port of ffmpeg.
 * - Changes and updates by the UltraStar Deluxe Team
 *
 * Conversion of libavutil/log.h
 * avutil version 54.7.100
 *
 *)

(**
 * @file
 * log
 *)

type
  (* from opt.h *)
  TAVOptionType = (
{$IFDEF FF_API_OLD_AVOPTIONS}
    FF_OPT_TYPE_FLAGS = 0,
    FF_OPT_TYPE_INT,
    FF_OPT_TYPE_INT64,
    FF_OPT_TYPE_DOUBLE,
    FF_OPT_TYPE_FLOAT,
    FF_OPT_TYPE_STRING,
    FF_OPT_TYPE_RATIONAL,
    FF_OPT_TYPE_BINARY,  ///< offset must point to a pointer immediately followed by an int for the length
    FF_OPT_TYPE_CONST = 128
{$ELSE}
    AV_OPT_TYPE_FLAGS,
    AV_OPT_TYPE_INT,
    AV_OPT_TYPE_INT64,
    AV_OPT_TYPE_DOUBLE,
    AV_OPT_TYPE_FLOAT,
    AV_OPT_TYPE_STRING,
    AV_OPT_TYPE_RATIONAL,
    AV_OPT_TYPE_BINARY,  ///< offset must point to a pointer immediately followed by an int for the length
    AV_OPT_TYPE_DICT,
    AV_OPT_TYPE_CONST = 128,
    AV_OPT_TYPE_IMAGE_SIZE     = $53495A45,  ///< MKBETAG('S','I','Z','E'), offset must point to two consecutive integers    
    AV_OPT_TYPE_PIXEL_FMT      = $50464D54,  ///< MKBETAG('P','F','M','T') 
    AV_OPT_TYPE_SAMPLE_FMT     = $53464D54,  ///< MKBETAG('S','F','M','T') 
    AV_OPT_TYPE_VIDEO_RATE     = $56524154   ///< MKBETAG('V','R','A','T'), offset must point to TAVRational   
    AV_OPT_TYPE_DURATION       = $44555220,  ///< MKBETAG('D','U','R',' '),
    AV_OPT_TYPE_COLOR          = $434F4C52,  ///< MKBETAG('C','O','L','R'),    
    AV_OPT_TYPE_CHANNEL_LAYOUT = $43484C41,  ///< MKBETAG('C','H','L','A'),
{$ENDIF}
  );

const
  AV_OPT_FLAG_ENCODING_PARAM  = 1;   ///< a generic parameter which can be set by the user for muxing or encoding
  AV_OPT_FLAG_DECODING_PARAM  = 2;   ///< a generic parameter which can be set by the user for demuxing or decoding
{$IFDEF FF_API_OPT_TYPE_METADATA}
  AV_OPT_FLAG_METADATA        = 4;   ///< some data extracted or inserted into the file like title, comment, ...
{$ENDIF}
  AV_OPT_FLAG_AUDIO_PARAM     = 8;
  AV_OPT_FLAG_VIDEO_PARAM     = 16;
  AV_OPT_FLAG_SUBTITLE_PARAM  = 32;
  (**
   * The option is inteded for exporting values to the caller.
   *)
  AV_OPT_FLAG_EXPORT          = 64;
  (**
   * The option may not be set through the AVOptions API, only read.
   * This flag only makes sense when AV_OPT_FLAG_EXPORT is also set.
   *)
  AV_OPT_FLAG_READONLY        = 128;
  AV_OPT_FLAG_FILTERING_PARAM = 1 shl 16; ///< a generic parameter which can be set by the user for filtering

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
        0: (i64: cint64);
        1: (dbl: cdouble);
        2: (str: PAnsiChar);
        (* TODO those are unused now *)
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

type
  PAVClassCategory = ^TAVClassCategory;
  TAVClassCategory = (
    AV_CLASS_CATEGORY_NA = 0,
    AV_CLASS_CATEGORY_INPUT,
    AV_CLASS_CATEGORY_OUTPUT,
    AV_CLASS_CATEGORY_MUXER,
    AV_CLASS_CATEGORY_DEMUXER,
    AV_CLASS_CATEGORY_ENCODER,
    AV_CLASS_CATEGORY_DECODER,
    AV_CLASS_CATEGORY_FILTER,
    AV_CLASS_CATEGORY_BITSTREAM_FILTER,
    AV_CLASS_CATEGORY_SWSCALER,
    AV_CLASS_CATEGORY_SWRESAMPLER,
    AV_CLASS_CATEGORY_DEVICE_VIDEO_OUTPUT = 40,
    AV_CLASS_CATEGORY_DEVICE_VIDEO_INPUT,
    AV_CLASS_CATEGORY_DEVICE_AUDIO_OUTPUT,
    AV_CLASS_CATEGORY_DEVICE_AUDIO_INPUT,
    AV_CLASS_CATEGORY_DEVICE_OUTPUT,
    AV_CLASS_CATEGORY_DEVICE_INPUT,
    AV_CLASS_CATEGORY_NB ///< not part of ABI/API
  );

(*
#define AV_IS_INPUT_DEVICE(category) \
    (((category) == AV_CLASS_CATEGORY_DEVICE_VIDEO_INPUT) || \
     ((category) == AV_CLASS_CATEGORY_DEVICE_AUDIO_INPUT) || \
     ((category) == AV_CLASS_CATEGORY_DEVICE_INPUT))

#define AV_IS_OUTPUT_DEVICE(category) \
    (((category) == AV_CLASS_CATEGORY_DEVICE_VIDEO_OUTPUT) || \
     ((category) == AV_CLASS_CATEGORY_DEVICE_AUDIO_OUTPUT) || \
     ((category) == AV_CLASS_CATEGORY_DEVICE_OUTPUT))
*)

// struct AVOptionRanges;

  (**
   * A single allowed range of values, or a single allowed value.
   *)
  PAVOptionRange  = ^TAVOptionRange;
  PPAVOptionRange = ^PAVOptionRange;
  TAVOptionRange = record
    str: {const} PAnsiChar;
    (**
     * Value range.
     * For string ranges this represents the min/max length.
     * For dimensions this represents the min/max pixel count or width/height in multi-component case.
     *)
    value_min, value_max: cdouble;
		(**
     * Value's component range.
     * For string this represents the unicode range for chars, 0-127 limits to ASCII.
     *)
    component_min, component_max: cdouble;
		(**
     * Range flag.
     * If set to 1 the struct encodes a range, if set to 0 a single value.
     *)
    is_range: cint;
  end;

  (**
   * List of AVOptionRange structs.
   *)
  PAVOptionRanges  = ^TAVOptionRanges;
  PPAVOptionRanges = ^PAVOptionRanges;
  TAVOptionRanges = record
    (**
     * Array of option ranges.
     *
     * Most of option types use just one component.
     * Following describes multi-component option types:
     *
     * AV_OPT_TYPE_IMAGE_SIZE:
     * component index 0: range of pixel count (width * height).
     * component index 1: range of width.
     * component index 2: range of height.
     *
     * @note To obtain multi-component version of this structure, user must
     *       provide AV_OPT_MULTI_COMPONENT_RANGE to av_opt_query_ranges or
     *       av_opt_query_ranges_default function.
     *
     * Multi-component range can be read as in following example:
     *
     * @code
     * int range_index, component_index;
     * AVOptionRanges *ranges;
     * AVOptionRange *range[3]; //may require more than 3 in the future.
     * av_opt_query_ranges(&ranges, obj, key, AV_OPT_MULTI_COMPONENT_RANGE);
     * for (range_index = 0; range_index < ranges->nb_ranges; range_index++) {
     *     for (component_index = 0; component_index < ranges->nb_components; component_index++)
     *         range[component_index] = ranges->range[ranges->nb_ranges * component_index + range_index];
     *     //do something with range here.
     * }
     * av_opt_freep_ranges(&ranges);
     * @endcode
     *)
    range:     PPAVOptionRange;
    (**
     * Number of ranges per component.
     *)
    nb_ranges: cint;
    (**
     * Number of componentes.
     *)
    nb_components: cint;
  end;

(**
 * Describe the class of an AVClass context structure. That is an
 * arbitrary struct of which the first field is a pointer to an
 * AVClass struct (e.g. AVCodecContext, AVFormatContext etc.).
 *)
  PAVClass = ^TAVClass;
  TAVClass = record
    (**
     * The name of the class; usually it is the same name as the
     * context structure type to which the AVClass is associated.
     *)
    class_name: PAnsiChar;

    (**
     * A pointer to a function which returns the name of a context
     * instance ctx associated with the class.
     *)
    item_name: function(ctx: pointer): PAnsiChar; cdecl;

    (**
     * a pointer to the first option specified in the class if any or NULL
     *
     * @see av_set_default_options()
     *)
    option: PAVOption;

    (**
     * LIBAVUTIL_VERSION with which this structure was created.
     * This is used to allow fields to be added without requiring major
     * version bumps everywhere.
     *)
    version: cint;

    (**
     * Offset in the structure where log_level_offset is stored.
     * 0 means there is no such variable
     *)
    log_level_offset_offset: cint;

    (**
     * Offset in the structure where a pointer to the parent context for
     * logging is stored. For example a decoder could pass its AVCodecContext
     * to eval as such a parent context, which an av_log() implementation
     * could then leverage to display the parent context.
     * The offset can be NULL.
     *)
    parent_log_context_offset: cint;
    
    (**
     * Return next AVOptions-enabled child or NULL
     *)
    child_next: function (obj: pointer; prev: pointer): pointer; cdecl;

    (**
     * Return an AVClass corresponding to the next potential
     * AVOptions-enabled child.
     *
     * The difference between child_next and this is that
     * child_next iterates over _already existing_ objects, while
     * child_class_next iterates over _all possible_ children.
     *)
    child_class_next: function (prev: {const} PAVClass): {const} PAVClass; cdecl;

    (**
     * Category used for visualization (like color)
     * This is only set if the category is equal for all objects using this class.
     * available since version (51 << 16 | 56 << 8 | 100)
     *)
    category: TAVClassCategory;

    (**
     * Callback to return the category.
     * available since version (51 << 16 | 59 << 8 | 100)
     *)
    get_category: function (ctx: pointer): PAVClassCategory; cdecl;

    (**
     * Callback to return the supported/allowed ranges.
     * available since version (52.12)
     *)
    query_ranges: function (P: PPAVOptionRanges; obj: pointer; key: {const} PAnsiChar; flags: cint): cint; cdecl;
end;

const
(**
 * Print no output.
 *)
  AV_LOG_QUIET   = -8;

(**
 * Something went really wrong and we will crash now.
 *)
  AV_LOG_PANIC   =  0;

(**
 * Something went wrong and recovery is not possible.
 * For example, no header was found for a format which depends
 * on headers or an illegal combination of parameters is used.
 *)
  AV_LOG_FATAL   =  8;

(**
 * Something went wrong and cannot losslessly be recovered.
 * However, not all future data is affected.
 *)
  AV_LOG_ERROR   = 16;

(**
 * Something somehow does not look correct. This may or may not
 * lead to problems. An example would be the use of '-vstrict -2'.
 *)
  AV_LOG_WARNING = 24;

(**
 * Standard information.
 *)
  AV_LOG_INFO    = 32;

(**
 * Detailed information.
 *)
	AV_LOG_VERBOSE = 40;

(**
 * Stuff which is only useful for libav* developers.
 *)
  AV_LOG_DEBUG   = 48;

  AV_LOG_MAX_OFFSET = (AV_LOG_DEBUG - AV_LOG_QUIET);

(**
 * Sets additional colors for extended debugging sessions.
 * @code
   av_log(ctx, AV_LOG_DEBUG|AV_LOG_C(134), "Message in purple\n");
   @endcode
 * Requires 256color terminal support. Uses outside debugging is not
 * recommended.
 *)
{** to be translated if needed
  AV_LOG_C(x) (x << 8)
**}

(**
 * Send the specified message to the log if the level is less than or equal
 * to the current av_log_level. By default, all logging messages are sent to
 * stderr. This behavior can be altered by setting a different logging callback
 * function.
 * @see av_log_set_callback
 *
 * @param avcl A pointer to an arbitrary struct of which the first field is a
 *        pointer to an AVClass struct.
 * @param level The importance level of the message expressed using a @ref
 *        lavu_log_constants "Logging Constant".
 * @param fmt The format string (printf-compatible) that specifies how
 *        subsequent arguments are converted to output.
 *)
{** to be translated if needed
void av_log(void *avcl, int level, const char *fmt, ...) av_printf_format(3, 4);
**}

type
  va_list = pointer;

(**
 * Send the specified message to the log if the level is less than or equal
 * to the current av_log_level. By default, all logging messages are sent to
 * stderr. This behavior can be altered by setting a different logging callback
 * function.
 * @see av_log_set_callback
 *
 * @param avcl A pointer to an arbitrary struct of which the first field is a
 *        pointer to an AVClass struct.
 * @param level The importance level of the message expressed using a @ref
 *        lavu_log_constants "Logging Constant".
 * @param fmt The format string (printf-compatible) that specifies how
 *        subsequent arguments are converted to output.
 * @param vl The arguments referenced by the format string.
 *)
procedure av_vlog(avcl: pointer; level: cint; fmt: {const} PAnsiChar; vl: va_list);
  cdecl; external av__util;

(**
 * Get the current log level
 *
 * @see lavu_log_constants
 *
 * @return Current log level
 *)
function av_log_get_level(): cint;
  cdecl; external av__util;

(**
 * Set the log level
 *
 * @see lavu_log_constants
 *
 * @param level Logging level
 *)
procedure av_log_set_level(level: cint);
  cdecl; external av__util;

(**
 * Set the logging callback
 *
 * @note The callback must be thread safe, even if the application does not use
 *       threads itself as some codecs are multithreaded.
 *
 * @see av_log_default_callback
 *
 * @param callback A logging function with a compatible signature.
 *)
{** to be translated if needed
void av_log_set_callback(void (*callback)(void*, int, const char*, va_list));
**}

(**
 * Default logging callback
 *
 * It prints the message to stderr, optionally colorizing it.
 *
 * @param avcl A pointer to an arbitrary struct of which the first field is a
 *        pointer to an AVClass struct.
 * @param level The importance level of the message expressed using a @ref
 *        lavu_log_constants "Logging Constant".
 * @param fmt The format string (printf-compatible) that specifies how
 *        subsequent arguments are converted to output.
 * @param vl The arguments referenced by the format string.
 *)
{** to be translated if needed
void av_log_default_callback(void* avcl, int level, const char *fmt, va_list vl);
**}

(**
 * Return the context name
 *
 * @param  ctx The AVClass context
 *
 * @return The AVClass class_name
 *)
function av_default_item_name(ctx: pointer): PAnsiChar;
  cdecl; external av__util;
function av_default_get_category(ptr: pointer): TAVClassCategory;
  cdecl; external av__util;

(**
 * Format a line of log the same way as the default callback.
 * @param line          buffer to receive the formated line
 * @param line_size     size of the buffer
 * @param print_prefix  used to store whether the prefix must be printed;
 *                      must point to a persistent integer initially set to 1
 *)
procedure av_log_format_line(ptr: pointer; level: cint; fmt: {const} PAnsiChar; vl: va_list;
                        line: PAnsiChar; line_size: cint; print_prefix: Pcint);
  cdecl; external av__util;

(**
 * av_dlog macros
 * Useful to print debug messages that shouldn't get compiled in normally.
 *)
(** to be translated if needed
#ifdef DEBUG
#    define av_dlog(pctx, ...) av_log(pctx, AV_LOG_DEBUG, __VA_ARGS__)
#else
#    define av_dlog(pctx, ...) do { if (0) av_log(pctx, AV_LOG_DEBUG, __VA_ARGS__); } while (0)
#endif
**)

const
(**
 * Skip repeated messages, this requires the user app to use av_log() instead of
 * (f)printf as the 2 would otherwise interfere and lead to
 * "Last message repeated x times" messages below (f)printf messages with some
 * bad luck.
 * Also to receive the last, "last repeated" line if any, the user app must
 * call av_log(NULL, AV_LOG_QUIET, "%s", ""); at the end
 *)
  AV_LOG_SKIP_REPEATED = 1;

(**
 * Include the log severity in messages originating from codecs.
 *
 * Results in messages such as:
 * [rawvideo @ 0xDEADBEEF] [error] encode did not produce valid pts
 *)
  AV_LOG_PRINT_LEVEL = 2;

procedure av_log_set_flags(arg: cint);
  cdecl; external av__util;

function av_log_get_flags: cint;
  cdecl; external av__util;
