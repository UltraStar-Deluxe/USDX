# This file is part of UltraStar Deluxe
# Created by the UltraStar Deluxe Team

# SYNOPSIS
#
#   AC_FFMPEG_VERSION
#
# DESCRIPTION
#
#   Determines version of ffmpeg.
#
#   Description of the steps of the command line
#
# 1) redirect stderr and stdout to stdout
#    I did not manage in sh, therefore invoke bash
# 2) grep for the lines with "ffmpeg"
# 3) grep for the line without "version"
# 4) cut out the 2nd word, with ' ' as a separator.
#

AC_DEFUN([AC_FFMPEG_VERSION],
[
    AC_MSG_CHECKING([for ffmpeg version])
    FFMPEG_VERSION=$(bash -c 'ffmpeg -version 2>&1' | grep ffmpeg | grep -v version | cut -f2 -d\ )
    AX_EXTRACT_VERSION(FFMPEG, $FFMPEG_VERSION)
    AC_MSG_RESULT(@<:@$FFMPEG_VERSION@:>@)
    AC_SUBST(FFMPEG_VERSION)
])

# FFMPEG_LIB_COMPATIBLE(LIBRARY, MIN-VERSION, [FIRST-INCOMPATIBLE-VERSION])
# -------------------------------------------------------------------------
# Tests if the FFmpeg libaray LIBRARY is at least version MIN-VERSION.
# If FIRST-INCOMPATIBLE-VERSION is passed, it is also tested if the version
# is below FIRST-INCOMPATIBLE-VERSION. If it is not passed, the major version
# of the library must be equal to the one in MIN-VERSION.
AC_DEFUN([FFMPEG_LIB_COMPATIBLE],
    [(test x$[$1][_HAVE] != xyes || (test $[$1][_VERSION_INT] -ge $2 && test m4_ifnblank([$3],
	[$[$1][_VERSION_INT] -lt $3], [$[$1][_VERSION_MAJOR] -eq m4_eval([$2/1000000])])))])

# FFMPEG_LIBS_COMPATIBLE(AVUTIL-VERSION-RANGE, AVCODEC-VERSION-RANGE,
#                        AVFORMAT-VERSION-RANGE, [SWSCALE-VERSION.RANGE],
#                        [SWRESAMPLE-VERSION-RANGE])
# -----------------------------------------------------------------------
# Tests if all 3-5 libraries fall into the passed version ranges.
AC_DEFUN([FFMPEG_LIBS_COMPATIBLE],
    [FFMPEG_LIB_COMPATIBLE(libavutil,$1) &&
    FFMPEG_LIB_COMPATIBLE(libavcodec,$2) &&
    FFMPEG_LIB_COMPATIBLE(libavformat,$3)m4_ifnblank([$4], [ &&
    FFMPEG_LIB_COMPATIBLE(libswscale,$4)])m4_ifnblank([$5], [ &&
    FFMPEG_LIB_COMPATIBLE(libswresample,$5)])])
