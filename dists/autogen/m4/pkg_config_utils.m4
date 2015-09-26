# This file is part of UltraStar Deluxe
# Created by the UltraStar Deluxe Team


# OVERVIEW
#
#   PKG_VALUE(VARIABLE_PREFIX, POSTFIX, COMMAND, MODULE, HELP-STRING)
#   PKG_VERSION(VARIABLE_PREFIX, MODULE)
#   PKG_HAVE(VARIABLE_PREFIX, MODULE, [REQUIRED])
#   AX_TRIM(STRING)

# SYNOPSIS
#
#   PKG_VALUE(VARIABLE_PREFIX, POSTFIX, COMMAND, MODULE, HELP-STRING)
#
# DESCRIPTION
#
#   Calls pkg-config with a given command and stores the result.
#   If the variable was already defined by the user or the package
#   is not present on the system ([$VARIABLE_PREFIX]_HAVE <> yes) 
#   pkg-config will not be executed and the old value remains.
#   In addition the variable will be shown on "./configure --help"
#   described by a given help-string.
#
#   Parameters:
#     - VARIABLE_PREFIX: the prefix for the variables storing 
#                        information about the package.
#     - POSTFIX:         [$VARIABLE_PREFIX]_[$POSTFIX] will contain the value
#     - COMMAND:         a pkg-config command, e.g. "variable=prefix"
#     - MODULE:          the package pkg-config will retrieve info from
#     - HELP-STRING:     description of the variable
#
#   Sets:
#     [$VARIABLE_PREFIX]_[$POSTFIX]   # value (AC_SUBST)

AC_DEFUN([PKG_VALUE],
[
    AC_ARG_VAR([$1]_[$2], [$5, overriding pkg-config])   
    # check if variable was defined by the user
    if test -z "$[$1]_[$2]"; then
        # if not, get it from pkg-config
        if test x$[$1][_HAVE] = xyes; then
            PKG_CHECK_EXISTS([$4],
                [[$1]_[$2]=`$PKG_CONFIG --[$3] --silence-errors "$4"`],
                [# print error message and quit
                 err_msg=`$PKG_CONFIG --errors-to-stdout --print-errors "$4"`
                 AC_MSG_ERROR(
[

$err_msg

If --with-[$1]=nocheck is defined the environment variable 
[$1]_[$2]
must be set to avoid the need to call pkg-config.

See the pkg-config man page for more details.
])

                ])
        fi
    fi
    AC_SUBST([$1]_[$2])
])

# SYNOPSIS
#
#   PKG_VERSION(VARIABLE_PREFIX, MODULE)
#
# DESCRIPTION
#
#   Retrieves the version of a package
#
#   Parameters:
#     - VARIABLE_PREFIX: the prefix for the variables storing 
#                        information about the package.
#     - MODULE:          package name according to pkg-config
#
#   Sets:
#     [$VARIABLE_PREFIX]_VERSION         # full version string 
#                                        #   (format: "major.minor.release")
#
#     [$VARIABLE_PREFIX]_VERSION_MAJOR   # major version number
#     [$VARIABLE_PREFIX]_VERSION_MINOR   # minor version number
#     [$VARIABLE_PREFIX]_VERSION_RELEASE # release version number
#
#     [$VARIABLE_PREFIX]_VERSION_INT     # integer representation: 
#                                        #   MMMmmmrrr (M:major,m:minor,r:release)

AC_DEFUN([PKG_VERSION],
[
    if test x$[$1][_HAVE] = xyes; then
        AC_MSG_CHECKING([version of $1])
        PKG_VALUE([$1], [VERSION], [modversion], [$2], [version of $1])   
        AC_MSG_RESULT(@<:@$[$1][_VERSION]@:>@)
    else
        [$1][_VERSION]="0.0.0"
    fi
    AX_EXTRACT_VERSION([$1], $[$1][_VERSION])

    # for avutil: map library version to ffmpeg version
    if test $1 = "libavutil"; then
        AC_MSG_CHECKING([version of ffmpeg])
    	if test $[$1][_VERSION_INT] -le 60000000; then
		if   test $[$1][_VERSION_INT] -ge 54031100; then
			FFMPEG_VERSION="2.8"    	    	
		elif test $[$1][_VERSION_INT] -ge 54027100; then
			FFMPEG_VERSION="2.7"    	    	
		elif test $[$1][_VERSION_INT] -ge 54020100; then
			FFMPEG_VERSION="2.6"    	    	
		elif test $[$1][_VERSION_INT] -ge 54015100; then
			FFMPEG_VERSION="2.5"    	    	
		elif test $[$1][_VERSION_INT] -ge 54007001; then
			FFMPEG_VERSION="2.4"    	
		elif test $[$1][_VERSION_INT] -ge 52066100; then
			FFMPEG_VERSION="2.2"    	
		elif test $[$1][_VERSION_INT] -ge 52048100; then
			FFMPEG_VERSION="2.1"
		elif test $[$1][_VERSION_INT] -ge 52038100; then
			FFMPEG_VERSION="2.0"
		elif test $[$1][_VERSION_INT] -ge 52018100; then
			FFMPEG_VERSION="1.2"
		elif test $[$1][_VERSION_INT] -ge 52013100; then
			FFMPEG_VERSION="1.1"
		elif test $[$1][_VERSION_INT] -ge 51073101; then
			FFMPEG_VERSION="1.0"
		elif test $[$1][_VERSION_INT] -ge 51054100; then
			FFMPEG_VERSION="0.11"
		elif test $[$1][_VERSION_INT] -ge 51034101; then
			FFMPEG_VERSION="0.10"
		elif test $[$1][_VERSION_INT] -ge 51032000; then
			FFMPEG_VERSION="0.9"
		elif test $[$1][_VERSION_INT] -ge 51009001; then
			FFMPEG_VERSION="0.8"
		elif test $[$1][_VERSION_INT] -ge 50043000; then
			FFMPEG_VERSION="0.7"
		else
			FFMPEG_VERSION="0"
		fi
	else
		FFMPEG_VERSION="0"
	fi
        AX_EXTRACT_VERSION(FFMPEG, $FFMPEG_VERSION)
        AC_SUBST(FFMPEG_VERSION)
        AC_MSG_RESULT(@<:@$FFMPEG_VERSION@:>@)
    fi
])

# SYNOPSIS
#
#   AX_TRIM(STRING)
#
# DESCRIPTION
#
#   Removes surrounding whitespace

AC_DEFUN([AX_TRIM],
[
    echo "[$1]" | $SED 's/^[[ \t]]*//' | $SED 's/[[ \t]]*$//'
])

# SYNOPSIS
#
#   PKG_HAVE(VARIABLE_PREFIX, MODULE, [REQUIRED])
#
# DESCRIPTION
#
#   Checks with pkg-config if a package exists and retrieves 
#   information about it.
#
#   Parameters:
#     - VARIABLE_PREFIX: the prefix for the variables storing information about the package.
#     - MODULE:   package name according to pkg-config
#     - REQUIRED: if true, the configure-script is aborted if the package was not found
# 
#   Uses:
#     with_[$VARIABLE_PREFIX]: whether and how the package should be checked for
#       "check":   check for the package but do not abort if it does not exist (default)
#       "no":      do not check for the package (sets _HAVE to "no" and _VERSION to "0.0.0")
#       "yes":     check for the package and abort if it does not exist
#       "nocheck": do not check for the package (sets _HAVE to "yes")
# 
#   Sets:
#     [$VARIABLE_PREFIX]_HAVE       # package is available (values: "yes"|"no")
#     [$VARIABLE_PREFIX]_LIBS       # linker flags (e.g. -Lmylibdir -lmylib)
#     [$VARIABLE_PREFIX]_LIBDIRS    # library dirs (e.g. -Lmylibdir)

AC_DEFUN([PKG_HAVE],
[
    have_lib="no"
    AC_MSG_CHECKING([for $2])
    if test x"$with_[$1]" = xnocheck; then
        # do not call pkg-config, use user settings
        have_lib="yes"
    elif test x"$with_[$1]" != xno; then
        # check if package exists
	PKG_CHECK_EXISTS([$2], [
            have_lib="yes"
            [$1][_LIBS]=`$PKG_CONFIG --libs --silence-errors "$2"`
            [$1][_LIBDIRS]=`$PKG_CONFIG --libs-only-L --silence-errors "$2"`
            [$1][_LIBDIRS]=`AX_TRIM($[$1][_LIBDIRS])`
            # add library directories to LIBS (ignore *_LIBS for now)
	    if test -n "$[$1][_LIBDIRS]"; then
                LIBS="$LIBS $[$1][_LIBDIRS]"
            fi
        ])
    fi
    if test x$have_lib = xyes; then
        [$1][_HAVE]="yes"
        if test -n "$[$1][_LIBDIRS]"; then
            # show additional lib-dirs
            AC_MSG_RESULT(yes [(]$[$1][_LIBDIRS][)])
        else
            AC_MSG_RESULT(yes)
        fi
    else
        [$1][_HAVE]="no"
        AC_MSG_RESULT(no)

        # check if package is required
        if test x$3 = xyes -o x"$with_[$1]" = xyes ; then
            # print error message and quit
            err_msg=`$PKG_CONFIG --errors-to-stdout --print-errors "$2"`
            AC_MSG_ERROR(
[

$err_msg

Alternatively, you may set --with-[$1]=nocheck and the environment
variables [$1]_[[...]] (see configure --help) 
to appropriate values to avoid the need to call pkg-config.

See the pkg-config man page for more details.
])
        fi
    fi
])
