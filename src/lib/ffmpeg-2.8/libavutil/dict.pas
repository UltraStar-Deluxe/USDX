(*
 * AVDictionary
 * copyright (c) 2011 Karl-Michael Schindler <karl-michael.schindler@web.de>
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
 * This is a part of the Pascal port of ffmpeg.
 *
 * Conversion of libavutil/dict.h
 * avutil version 54.7.100
 *
 *)

const
  AV_DICT_MATCH_CASE      = 1;
  AV_DICT_IGNORE_SUFFIX   = 2;
  AV_DICT_DONT_STRDUP_KEY = 4;    (**< Take ownership of a key that's been
                                       allocated with av_malloc() or another memory allocation function. *)
  AV_DICT_DONT_STRDUP_VAL = 8;    (**< Take ownership of a value that's been
                                       allocated with av_malloc() or another memory allocation function. *)
  AV_DICT_DONT_OVERWRITE  = 16;   (**< Don't overwrite existing entries. *)
  AV_DICT_APPEND          = 32;   (**< If the entry already exists, append to it.  Note that no
                                    delimiter is added, the strings are simply concatenated. *)

type
  PAVDictionaryEntry = ^TAVDictionaryEntry;
  TAVDictionaryEntry = record
    key:   PAnsiChar;
    value: PAnsiChar;
  end;
  
(* with the "help" of libavutil/internal.h: *)

  PPAVDictionary = ^PAVDictionary;
  PAVDictionary = ^TAVDictionary;
  TAVDictionary = record
    count: cint;
    elems: PAVDictionaryEntry;
  end;

(**
 * Get a dictionary entry with matching key.
 *
 * The returned entry key or value must not be changed, or it will
 * cause undefined behavior.
 *
 * To iterate through all the dictionary entries, you can set the matching key
 * to the null string "" and set the AV_DICT_IGNORE_SUFFIX flag.
 *  
 * @param key matching key  
 * @param prev Set to the previous matching element to find the next.
 *             If set to NULL the first matching element is returned.
 * @param flags a collection of AV_DICT_* flags controlling how the entry is retrieved
 * @return found entry or NULL in case no matching entry was found in the dictionary
 *)
function av_dict_get({const} m: PAVDictionary; {const} key: PAnsiChar; {const} prev: PAVDictionaryEntry; flags: cint): PAVDictionaryEntry;
  cdecl; external av__util;

(**
 * Get number of entries in dictionary.
 *
 * @param m dictionary
 * @return  number of entries in dictionary
 *)
function av_dict_count({const} m: PAVDictionary): cint;
  cdecl; external av__util;

(**
 * Set the given entry in *pm, overwriting an existing entry.
 *
 * Note: If AV_DICT_DONT_STRDUP_KEY or AV_DICT_DONT_STRDUP_VAL is set,
 * these arguments will be freed on error.
 *
 * @param pm pointer to a pointer to a dictionary struct. If *pm is NULL
 * a dictionary struct is allocated and put in *pm.
 * @param key entry key to add to *pm (will be av_strduped depending on flags)
 * @param value entry value to add to *pm (will be av_strduped depending on flags).
 *        Passing a NULL value will cause an existing entry to be deleted.
 * @return >= 0 on success otherwise an error code <0
 *)
function av_dict_set(var pm: PAVDictionary; {const} key: PAnsiChar; {const} value: PAnsiChar; flags: cint): cint;
  cdecl; external av__util;

(**
 * Convenience wrapper for av_dict_set that converts the value to a string
 * and stores it.
 *
 * Note: If AV_DICT_DONT_STRDUP_KEY is set, key will be freed on error.
 *)
function av_dict_set_int(var pm: PAVDictionary; {const} key: PAnsiChar;
                         value: cint64; flags: cint): cint;
  cdecl; external av__util;

(**
 * Parse the key/value pairs list and add the parsed entries to a dictionary.
 *
 * In case of failure, all the successfully set entries are stored in
 * *pm. You may need to manually free the created dictionary.
 *
 * @param key_val_sep  a 0-terminated list of characters used to separate
 *                     key from value
 * @param pairs_sep    a 0-terminated list of characters used to separate
 *                     two pairs from each other
 * @param flags        flags to use when adding to dictionary.
 *                     AV_DICT_DONT_STRDUP_KEY and AV_DICT_DONT_STRDUP_VAL
 *                     are ignored since the key/value tokens will always
 *                     be duplicated.
 * @return             0 on success, negative AVERROR code on failure
 *)
function av_dict_parse_string(var pm: PAVDictionary; {const} str: PAnsiChar;
                         {const} key_val_sep: PAnsiChar; {const} pairs_sep: PAnsiChar;
                         flags: cint): cint;
  cdecl; external av__util;

(**
 * Copy entries from one AVDictionary struct into another.
 * @param dst pointer to a pointer to a AVDictionary struct. If *dst is NULL,
 *            this function will allocate a struct for you and put it in *dst
 * @param src pointer to source AVDictionary struct
 * @param flags flags to use when setting entries in *dst
 * @note metadata is read using the AV_DICT_IGNORE_SUFFIX flag
 *)
procedure av_dict_copy(var dst: PAVDictionary; {const} src: PAVDictionary; flags: cint);
  cdecl; external av__util;

(**
 * Get dictionary entries as a string.
 *
 * Create a string containing dictionary's entries.
 * Such string may be passed back to av_dict_parse_string().
 * @note String is escaped with backslashes ('\').
 *
 * @param[in]  m             dictionary
 * @param[out] buffer        Pointer to buffer that will be allocated with string containg entries.
 *                           Buffer must be freed by the caller when is no longer needed.
 * @param[in]  key_val_sep   character used to separate key from value
 * @param[in]  pairs_sep     character used to separate two pairs from each other
 * @return                   >= 0 on success, negative on error
 * @warning Separators cannot be neither '\\' nor '\0'. They also cannot be the same.
 *)
function av_dict_get_string({const} m: PAVDictionary; buffer: PPAnsiChar;
                       {const} key_val_sep: AnsiChar; {const} pairs_sep: AnsiChar): cint;
  cdecl; external av__util;

(**
 * Free all the memory allocated for an AVDictionary struct
 * and all keys and values.
 *)
procedure av_dict_free(var m: PAVDictionary);
  cdecl; external av__util;
