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
 * Conversion of libavutil/error.h
 * Max. avutil version:  50.21.0, revision 24190, Wed Jul 21 01:00:00 2010 CET
 *
 *)

{$IF LIBAVUTIL_VERSION >= 50012000} // >= 50.12.0

{* error handling *}

const
{$IFDEF UNIX}
  ENOENT = ESysENOENT;
  EIO    = ESysEIO;
  ENOMEM = ESysENOMEM;
  EINVAL = ESysEINVAL;
  EDOM   = ESysEDOM;
  ENOSYS = ESysENOSYS;
  EILSEQ = ESysEILSEQ;
  EPIPE  = ESysEPIPE;
{$ELSE}
  ENOENT = 2;
  EIO    = 5;
  ENOMEM = 12;
  EINVAL = 22;
  EPIPE  = 32;  // just an assumption. needs to be checked.
  EDOM   = 33;
  {$IFDEF MSWINDOWS}
  // Note: we assume that ffmpeg was compiled with MinGW.
  // This must be changed if DLLs were compiled with cygwin.
  ENOSYS = 40;  // MSVC/MINGW: 40, CYGWIN: 88,  LINUX/FPC: 38
  EILSEQ = 42;  // MSVC/MINGW: 42, CYGWIN: 138, LINUX/FPC: 84
  {$ENDIF}
{$ENDIF}

(**
 * We need the sign of the error, because some platforms have 
 * E* and errno already negated. The previous version failed
 * with Delphi, because it needed EINVAL defined.
 * Warning: This code is platform dependent and assumes constants 
 * to be 32 bit.
 * This version does the following steps:
 * 1) shr 30:        shifts the sign bit to bit position 2
 * 2) and $00000002: sets all other bits to zero
 *                   positive EINVAL gives 0, negative gives 2
 * 3) not:           inverts all bits. This gives -1 and -3
 * 3) - 1:           positive EINVAL gives -1, negative 1
 *)
const
  AVERROR_SIGN = (EINVAL shr 30) and $00000002 - 1;

(*
#if EINVAL > 0
#define AVERROR(e) (-(e)) {**< Returns a negative error code from a POSIX error code, to return from library functions. *}
#define AVUNERROR(e) (-(e)) {**< Returns a POSIX error code from a library function error return value. *}
#else
{* Some platforms have E* and errno already negated. *}
#define AVERROR(e) (e)
#define AVUNERROR(e) (e)
#endif
*)

const
  AVERROR_UNKNOWN     = AVERROR_SIGN * EINVAL;  (**< unknown error *)
  AVERROR_IO          = AVERROR_SIGN * EIO;     (**< I/O error *)
  AVERROR_NUMEXPECTED = AVERROR_SIGN * EDOM;    (**< Number syntax expected in filename. *)
  AVERROR_INVALIDDATA = AVERROR_SIGN * EINVAL;  (**< invalid data found *)
  AVERROR_NOMEM       = AVERROR_SIGN * ENOMEM;  (**< not enough memory *)
  AVERROR_NOFMT       = AVERROR_SIGN * EILSEQ;  (**< unknown format *)
  AVERROR_NOTSUPP     = AVERROR_SIGN * ENOSYS;  (**< Operation not supported. *)
  AVERROR_NOENT       = AVERROR_SIGN * ENOENT;  (**< No such file or directory. *)
{$IF LIBAVCODEC_VERSION >= 52017000} // 52.17.0
  AVERROR_EOF         = AVERROR_SIGN * EPIPE;   (**< End of file. *)
{$IFEND}
  // Note: function calls as constant-initializers are invalid
  //AVERROR_PATCHWELCOME = -MKTAG('P','A','W','E'); {**< Not yet implemented in FFmpeg. Patches welcome. *}
  AVERROR_PATCHWELCOME = -(ord('P') or (ord('A') shl 8) or (ord('W') shl 16) or (ord('E') shl 24));
{$IFEND}

{$IF LIBAVUTIL_VERSION >= 50013000} // >= 50.13.0
(*
 * Put a description of the AVERROR code errnum in errbuf.
 * In case of failure the global variable errno is set to indicate the
 * error. Even in case of failure av_strerror() will print a generic
 * error message indicating the errnum provided to errbuf.
 *
 * @param errnum      error code to describe
 * @param errbuf      buffer to which description is written
 * @param errbuf_size the size in bytes of errbuf
 * @return 0 on success, a negative value if a description for errnum
 * cannot be found
 *)

function av_strerror(errnum: cint; errbuf: Pchar; errbuf_size: cint): cint;
  cdecl; external av__util;
{$IFEND}
