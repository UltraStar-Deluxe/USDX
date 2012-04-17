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
