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
 * avutil version 51.73.101
 *
 *)

const
  AV_DICT_MATCH_CASE      = 1;
  AV_DICT_IGNORE_SUFFIX   = 2;
  AV_DICT_DONT_STRDUP_KEY = 4;    (**< Take ownership of a key that's been
                                       allocated with av_malloc() and children. *)
  AV_DICT_DONT_STRDUP_VAL = 8;    (**< Take ownership of a value that's been
                                       allocated with av_malloc() and chilren. *)
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
 * @param prev Set to the previous matching element to find the next.
 *             If set to NULL the first matching element is returned.
 * @param flags Allows case as well as suffix-insensitive comparisons.
 * @return Found entry or NULL, changing key or value leads to undefined behavior.
 *)
function av_dict_get(m: PAVDictionary; {const} key: PAnsiChar; {const} prev: PAVDictionaryEntry; flags: cint): PAVDictionaryEntry;
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
 * Copy entries from one AVDictionary struct into another.
 * @param dst pointer to a pointer to a AVDictionary struct. If *dst is NULL,
 *            this function will allocate a struct for you and put it in *dst
 * @param src pointer to source AVDictionary struct
 * @param flags flags to use when setting entries in *dst
 * @note metadata is read using the AV_DICT_IGNORE_SUFFIX flag
 *)
procedure av_dict_copy(var dst: PAVDictionary; src: PAVDictionary; flags: cint);
  cdecl; external av__util;

(**
 * Free all the memory allocated for an AVDictionary struct
 * and all keys and values.
 *)
procedure av_dict_free(var m: PAVDictionary);
  cdecl; external av__util;
