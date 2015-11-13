# This file is part of UltraStar Deluxe
# Created by the UltraStar Deluxe Team

# SYNOPSIS
#
#   AX_EXTRACT_VERSION(VARIABLE_PREFIX, VERSION)
#
# DESCRIPTION
#
#   Splits a version number ("major.minor.release") into its components.
#   The resulting components of the version are guaranteed to be
#   numeric. All non-numeric chars are removed.
#
# Sets
#   [$VARIABLE_PREFIX]_VERSION_MAJOR
#   [$VARIABLE_PREFIX]_VERSION_MINOR
#   [$VARIABLE_PREFIX]_VERSION_RELEASE
#
# This function calls
#   AC_SUBST([$VARIABLE_PREFIX]_VERSION_type] for each type

AC_DEFUN([AX_EXTRACT_VERSION],
[
    version=[$2]    

    # strip leading non-numeric tokens 
    # (necessary for some ffmpeg-packages in ubuntu)
    # example: 0d.51.1.0 -> 51.1.0
    version=`echo $version | sed 's/^[[^.]]*[[^0-9.]][[^.]]*\.//'`

    # replace "." and "-" with " " and ignore trailing tokens.
    # 1.23.4-r2 will be splitted to [maj=1, min=23, rel=4].
    # In addition we delete everything after the first character 
    # which is not 0-9.
    # 1.3a4-r32 will be [maj=1, min=3, rel=0].
    read major minor release ignore <<eof
        `echo $version | tr '.-' ' ' | sed 's/[[^0-9\ ]].*//'` 
eof
    # Note: Do NOT indent the eof-delimiter
    # We use a here-document (<<< here-strings not POSIX compatible)

    test -z $major && major=0
    test -z $minor && minor=0
    test -z $release && release=0

    # strip preceding 0s and set unset version-parts to 0
    [$1][_VERSION_MAJOR]=$(($major))
    [$1][_VERSION_MINOR]=$(($minor))
    [$1][_VERSION_RELEASE]=$(($release))
    # integer representation: MMMmmmrrr (M:major,m:minor,r:release)
    # can be used if pkg-config's comparison fails
    [$1][_VERSION_INT]=$(($[$1][_VERSION_MAJOR]*1000000+$[$1][_VERSION_MINOR]*1000+$[$1][_VERSION_RELEASE]))

    AC_SUBST([$1][_VERSION_MAJOR])
    AC_SUBST([$1][_VERSION_MINOR])
    AC_SUBST([$1][_VERSION_RELEASE])
    AC_SUBST([$1][_VERSION_INT])
])
