# This file is part of UltraStar Deluxe
# Created by the UltraStar Deluxe Team

# SYNOPSIS
#
#   AC_MACOSX_VERSION
#
# DESCRIPTION
#
#   Determines the Mac OS X and Darwin version.
#
#   +----------+---------+
#   | Mac OS X |  Darwin |
#   +----------+---------+
#   |     10.4 |       8 |
#   |     10.5 |       9 |
#   +----------+---------+

AC_DEFUN([AC_MACOSX_VERSION],
[
    AC_MSG_CHECKING([for Mac OS X version])
    MACOSX_VERSION=`sw_vers -productVersion`
    AX_EXTRACT_VERSION(MACOSX, $MACOSX_VERSION)
    AC_MSG_RESULT(@<:@$MACOSX_VERSION@:>@)
    AC_SUBST(MACOSX_VERSION)

    AC_MSG_CHECKING([for Darwin version])
    DARWIN_VERSION=`uname -r | cut -f1 -d.`
    AC_MSG_RESULT(@<:@$DARWIN_VERSION@:>@)
    AC_SUBST(DARWIN_VERSION)
])
