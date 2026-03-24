# This file is part of UltraStar Deluxe
# Created by the UltraStar Deluxe Team

# SYNOPSIS
#
#   AC_MACOS_VERSION
#
# DESCRIPTION
#
#   Determines the Mac OS X / OS X / macOS and Darwin version.
#
#   +----------+---------+------------------------+
#   | Darwin   | OS   | Name                      |
#   +----------+---------+------------------------+
#   |   8.0    | 10.4.0  | Mac OS X Tiger         |
#   |   9.0    | 10.5.0  | Mac OS X Leopard       |
#   |  10.0    | 10.6.0  | Mac OS X Snow Leopard  |
#   |  11.0.0  | 10.7.0  | Mac OS X Lion          |
#   |  12.0.0  | 10.8.0  | OS X Mountain Lion     |
#   |  13.0.0  | 10.9.0  | OS X Mavericks         |
#   |  14.0.0  | 10.10.0 | OS X Yosemite          |
#   |  15.0.0  | 10.11.0 | OS X El Capitan        |
#   |  16.0.0  | 10.12.0 | macOS Sierra           |
#   |  17.0.0  | 10.13.0 | macOS High Sierra      |
#   |  18.0.0  | 10.14.0 | macOS Mojave           |
#   |  19.0.0  | 10.15.0 | macOS Catalina         |
#   |  20.0.0  | 11.0.0  | macOS Big Sur          |
#   |  21.0.0  | 12.0.0  | macOS Monterey         |
#   |  22.0.0  | 13.0.0  | macOS Ventura          |
#   |  23.0.0  | 14.0.0  | macOS Sonoma           |
#   |  24.0.0  | 15.0.0  | macOS Sequoia          |
#   |  25.0.0  | 26.0.0  | macOS Tahoe            |
#   +----------+---------+------------------------+

AC_DEFUN([AC_MACOS_VERSION],
[
    AC_MSG_CHECKING([for macOS version])
    MACOS_VERSION=`sw_vers -productVersion`
    AX_EXTRACT_VERSION(MACOS, $MACOS_VERSION)
    AC_MSG_RESULT(@<:@$MACOS_VERSION@:>@)
    AC_SUBST(MACOS_VERSION)

    AC_MSG_CHECKING([for Darwin version])
    DARWIN_VERSION=`uname -r | cut -f1 -d.`
    AC_MSG_RESULT(@<:@$DARWIN_VERSION@:>@)
    AC_SUBST(DARWIN_VERSION)
])
