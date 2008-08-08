#
# UltraStar-DX configure.ac script
#
# by UltraStar Deluxe Team
#
# Execute "autogen.sh" to create the configure script.
#

# Require autoconf >= 2.61
AC_PREREQ(2.61)

# Init autoconf
AC_INIT([ultrastardx], 
        [1.1-alpha], 
        [http://sourceforge.net/tracker/?group_id=191560&atid=937872])
# specify the website here
PACKAGE_WEBSITE="http://www.ultrastardeluxe.org/"
AC_SUBST(PACKAGE_WEBSITE)
# specify the IRC-channel here
PACKAGE_IRC="#ultrastardx at quakenet.org"
AC_SUBST(PACKAGE_IRC)

# Specify a source-file so autoconf can check if the source-dir exists
AC_CONFIG_SRCDIR(UltraStar.dpr)

# This one is not used by autoconf at the moment.
# When it is used maybe we don't need aclocal's -I parameter anymore.
AC_CONFIG_MACRO_DIR(m4)

# show features and packages in one list
AC_PRESERVE_HELP_ORDER

# set root directory (= trunk dir)
# ..resolved:   used by configure.ac
usdxroot=../..
# ..unresolved: used by .in files
usdxrootdir=\${top_srcdir}/../..
AC_SUBST(usdxrootdir)

# set sharerootdir to the resolved dataroot-dir for the config-*.inc file.
# Pascal cannot handle shell-variables like ${prefix}
AC_DEFINE_DIR(sharerootdir, datarootdir)

# -----------------------------------------
# find tools
# -----------------------------------------

# options for make command
AC_PROG_MAKE_SET
# find tool for ln -s (e.g. uses cp -p for FAT-filesystems)
AC_LN_S
# find a program for recursive dir creation
AC_PROG_MKDIR_P
# find the best install tool
AC_PROG_INSTALL
# some other useful tools
#AC_PROG_AWK
AC_PROG_SED
AC_PROG_GREP
#AC_PROG_EGREP

# -----------------------------------------
# macro declarations
# -----------------------------------------

# AC_TRIM(STRING)
# removes surrounding whitespace
# -------------------------------------------
AC_DEFUN([AC_TRIM],
[echo "[$1]" | $SED 's/^[[ \t]]*//' | $SED 's/[[ \t]]*$//'
])

# AC_SUBST_DEFINE(DEFINE_SUFFIX, IS_DEFINED)
# used to enable/disable pascal defines
AC_DEFUN([AC_SUBST_DEFINE],
[
    if [[ x$2 = xyes ]]; then
        DEFINE_[$1]=DEFINE
    else
	DEFINE_[$1]=UNDEF
    fi
    AC_SUBST(DEFINE_[$1])
])

# AC_SUBST_COMMENT(DEFINE_SUFFIX, IS_ENABLED)
# used to enable/disable lines in a Makefile
AC_DEFUN([AC_SUBST_COMMENT],
[
    if [[ x$2 = xyes ]]; then
        COMMENT_[$1]=""
    else
	COMMENT_[$1]="#"
    fi
    AC_SUBST(COMMENT_[$1])
])

# AC_SPLIT_VERSION(VARIABLE_PREFIX, VERSION)
# Splits version number ("major.minor.release") into its components.
# Sets
#   [$VARIABLE_PREFIX]_VERSION_MAJOR
#   [$VARIABLE_PREFIX]_VERSION_MINOR
#   [$VARIABLE_PREFIX]_VERSION_RELEASE
# This function calls
#   AC_SUBST([$VARIABLE_PREFIX]_VERSION_type] for each type
AC_DEFUN([AC_SPLIT_VERSION],
[
    version=[$2]    

    # strip leading non-numeric tokens 
    # (necessary for some ffmpeg-packages in ubuntu)
    # example: 0d.51.1.0 -> 51.1.0
    version=`echo $version | $SED 's/^[[^.]]*[[^0-9.]][[^.]]*\.//'`

    # replace "." and "-" with " " and ignore trailing tokens.
    # 1.23.4-r2 will be splitted to [maj=1, min=23, rel=4].
    # In addition we delete every character which is not 0-9.
    # 1.3a4-r32 will be [maj=1, min=34, rel=32].
    read major minor release ignore <<eof
        `echo $version | tr '.-' ' ' | $SED 's/[[^0-9\ ]].*//'` 
eof
    # Note: Do NOT indent the eof-delimiter
    # We use a here-document (<<< here-strings not POSIX compatible)

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

# PKG_VALUE(VARIABLE_PREFIX, POSTFIX, COMMAND, MODULE, HELP-STRING)
# -----------------------------------------------------
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

# PKG_VERSION(VARIABLE_PREFIX, MODULE)
#   Checks version of a package
# Parameters:
# - VARIABLE_PREFIX: the prefix for the variables storing information about the package.
# - MODULE:          package name according to pkg-config
# Sets:
#   [$VARIABLE_PREFIX]_VERSION         # full version string (format: "major.minor.release")
#   [$VARIABLE_PREFIX]_VERSION_MAJOR   # major version number
#   [$VARIABLE_PREFIX]_VERSION_MINOR   # minor version number
#   [$VARIABLE_PREFIX]_VERSION_RELEASE # release version number
#   [$VARIABLE_PREFIX]_VERSION_INT     # integer representation: MMMmmmrrr (M:major,m:minor,r:release)
AC_DEFUN([PKG_VERSION],
[
    if test x$[$1][_HAVE] = xyes; then
        AC_MSG_CHECKING([version of $1])
        PKG_VALUE([$1], [VERSION], [modversion], [$2], [version of $1])   
        AC_MSG_RESULT(@<:@$[$1][_VERSION]@:>@)
    else
        [$1][_VERSION]="0.0.0"
    fi
    AC_SPLIT_VERSION([$1], $[$1][_VERSION])
])

# PKG_HAVE(VARIABLE_PREFIX, MODULE, [REQUIRED])
#   Checks with pkg-config if a package exists and retrieves information 
#   about it.
# Parameters:
# - VARIABLE_PREFIX: the prefix for the variables storing information about the package.
# - MODULE:   package name according to pkg-config
# - REQUIRED: if true, the configure-script is aborted if the package was not found
# Uses:
#   with_[$VARIABLE_PREFIX]: whether and how the package should be checked for
#     "check":   check for the package but do not abort if it does not exist (default)
#     "no":      do not check for the package (sets _HAVE to "no" and _VERSION to "0.0.0")
#     "yes":     check for the package and abort if it does not exist
#     "nocheck": do not check for the package (sets _HAVE to "yes")
# Sets:
#   [$VARIABLE_PREFIX]_HAVE       # package is available (values: "yes"|"no")
#   [$VARIABLE_PREFIX]_LIBS       # linker flags (e.g. -Lmylibdir -lmylib)
#   [$VARIABLE_PREFIX]_LIBDIRS    # library dirs (e.g. -Lmylibdir)
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
            [$1][_LIBDIRS]=`AC_TRIM($[$1][_LIBDIRS])`
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


# -----------------------------------------
# define switches
# -----------------------------------------

# print library options header
AC_ARG_WITH([cfg-dummy1], [
External Libraries:])

# add portmixer option
AC_ARG_WITH([portmixer],
    [AS_HELP_STRING([--with-portmixer],
      [enable portmixer audio-mixer support @<:@default=check@:>@])],
    [with_portmixer=$withval], [with_portmixer="check"])

# add projectM option
AC_ARG_WITH([libprojectM], 
    [AS_HELP_STRING([--with-libprojectM],
      [enable projectM visualization support @<:@default=no@:>@])],
    [with_libprojectM=$withval], [with_libprojectM="no"])

# print path options header
AC_ARG_WITH([cfg-dummy2], [
Additional directories:])

# add suffix option
AC_ARG_WITH([suffix], 
    [AS_HELP_STRING([--with-suffix=SUFFIX],
      [path suffix @<:@default=UltraStarDeluxe@:>@])],
    [suffix=$withval], [suffix="UltraStarDeluxe"])
if [[ x$suffix = xno -o x$suffix = xyes ]] ; then
    AC_MSG_ERROR([Invalid suffix.]);
fi
AC_SUBST(suffix)

# print misc options header
AC_ARG_WITH([cfg-dummy3], [
Development options:])

LOCAL_BUILD="no"

# add global option
AC_ARG_ENABLE(global,
    [AS_HELP_STRING([--enable-global],
      [install into global folders (PREFIX/...) @<:@default=yes@:>@])],
    [test $enableval = "no" && LOCAL_BUILD="yes"], [])

# add local option
AC_ARG_ENABLE(local,
    [AS_HELP_STRING([--enable-local],
      [install into local folders (../../...) (same as --disable-global) @<:@default=no@:>@]))],
    [test $enableval = "yes" && LOCAL_BUILD="yes"], [])

# add dev_layout option
AC_ARG_ENABLE(dev-build,
    [AS_HELP_STRING([--enable-dev-build],
      [development build (implies local)  @<:@default=no@:>@])],
    [enable_dev_build=$enableval], [enable_dev_build="no"])
if [[ x$enable_dev_build = xyes ]]; then
    LOCAL_BUILD="yes"
fi

# set default Makefile install-target according to local/global build-type
AC_SUBST_DEFINE(USE_LOCAL_DIRS, $LOCAL_BUILD)
if [[ x$LOCAL_BUILD = xyes ]]; then
    AC_SUBST(install_type, ["local"])
else
    AC_SUBST(install_type, ["global"])    
fi


# -----------------------------------------
# check for compilers
# -----------------------------------------

# find and test the freepascal compiler
# sets PFLAGS, FPC_VERSION, FPC_DEBUG, etc.
AC_PROG_FPC
# FPC_VERSION is already defined by FPC, use
# PPC as prefix instead.
AC_SPLIT_VERSION(PPC, $FPC_VERSION)

# find and test the C compiler (for C-libs and wrappers)
AC_PROG_CC
AC_LANG([C])

# find and test the C++ compiler (for C-libs and wrappers)
AC_PROG_CXX
AC_LANG([C++])

AC_PROG_RANLIB

# find pkg-config
PKG_PROG_PKG_CONFIG()
if [[ x$PKG_CONFIG = x ]]; then
   AC_MSG_ERROR([
!!! pkg-config was not found on your system. 
!!! It is needed to determine the versions of your libraries.
!!! Install it and try again.])
fi


# -----------------------------------------
# check for OS
# -----------------------------------------

if [[ x$FPC_PLATFORM = xdarwin ]]; then
   AC_MSG_CHECKING([for Mac OS X version])
   MACOSX_VERSION=`sw_vers -productVersion`
   AC_SPLIT_VERSION(MACOSX, $MACOSX_VERSION)
   AC_MSG_RESULT(@<:@$MACOSX_VERSION@:>@)
fi

# -----------------------------------------
# check for libraries
# -----------------------------------------

# libpng
PKG_HAVE([libpng], [libpng], yes)

# find sdl
PKG_HAVE([sdl], [sdl], yes)

# find sqlite3
PKG_HAVE([sqlite3], [sqlite3], yes)

# find FFMpeg
# Note: do not use the min/max version parameters with ffmpeg
# otherwise it might fail in ubuntu due to a wrong version number 
# format in ffmpeg's .pc-files.
# For example: 0d.51.1.2 instead of the correct 51.1.2.
#   A check for version >=52.0.0 will return version 0d.51.1.2 
#   although it is lower because pkg-config is confused by the 0d.
# Use [mylib]_VERSION_INT for version-checking instead
PKG_HAVE([libavcodec], [libavcodec], yes)
PKG_VERSION([libavcodec], [libavcodec])
AC_CHECK_LIB([avcodec], [avcodec_decode_audio], [HAVE_AVCODEC_DECODE_AUDIO="yes"])
AC_CHECK_LIB([avcodec], [avcodec_decode_audio2], [HAVE_AVCODEC_DECODE_AUDIO2="yes"])
AC_CHECK_LIB([avcodec], [img_convert], [HAVE_IMG_CONVERT="yes"])
PKG_HAVE([libavformat], [libavformat], yes)
PKG_VERSION([libavformat], [libavformat])
PKG_HAVE([libavutil], [libavutil], yes)
PKG_VERSION([libavutil], [libavutil])
if [[ x$libavcodec_HAVE = xyes -a x$libavformat_HAVE = xyes -a x$libavutil_HAVE = xyes ]]; then
    ffmpeg_HAVE=yes
else
    ffmpeg_HAVE=no
fi
AC_SUBST_DEFINE(HAVE_FFMPEG, $ffmpeg_HAVE)

# find FFMpeg's swscale lib (just if FFMpeg is compiled in GPL mode)
PKG_HAVE([libswscale], [libswscale], no)
PKG_VERSION([libswscale], [libswscale])
AC_SUBST_DEFINE(HAVE_SWSCALE, $libswscale_HAVE)


# find projectM version
libprojectM_PKG="libprojectM >= 0.98"
PKG_HAVE([libprojectM], [$libprojectM_PKG], no)
PKG_VERSION([libprojectM], [$libprojectM_PKG])
AC_SUBST_DEFINE(HAVE_PROJECTM, $libprojectM_HAVE)
# get projectM include-dir
PKG_VALUE([libprojectM], [INCLUDEDIR], [variable=includedir], [$libprojectM_PKG], 
          [C-Header include-dir (e.g. /usr/include)])
# get projectM data-dir (for preset- and font-dir)
PKG_VALUE([libprojectM], [DATADIR], [variable=pkgdatadir], [$libprojectM_PKG], 
          [projectM data-directory for presets etc. (e.g. /usr/share/projectM)])
# check if we need the c-wrapper
if [[ "$libprojectM_VERSION_MAJOR" -ge 1 ]]; then
    libprojectM_NEEDS_CWRAPPER=yes
else
    libprojectM_NEEDS_CWRAPPER=no
fi
AC_SUBST_COMMENT(PROJECTM_CWRAPPER, $libprojectM_NEEDS_CWRAPPER)

# find portaudio
PKG_HAVE([portaudio], [portaudio-2.0], yes)
PKG_VERSION([portaudio], [portaudio-2.0])
AC_SUBST_DEFINE(HAVE_PORTAUDIO, $portaudio_HAVE)
# find portmixer
PKG_HAVE([portmixer], [portmixer], no)
AC_SUBST_DEFINE(HAVE_PORTMIXER, $portmixer_HAVE)

# determine linker-flags
#LDFLAGS=
#LIBS=
AC_SUBST(LDFLAGS)
AC_SUBST(LIBS)

# -----------------------------------------
# create output files
# -----------------------------------------

AC_CONFIG_FILES([config-$FPC_PLATFORM.inc:config.inc.in])
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([$usdxroot/Tools/ResourceExtractor/Makefile])
if [[ x$libprojectM_NEEDS_CWRAPPER = xyes ]]; then
    AC_CONFIG_FILES([lib/projectM/cwrapper/Makefile])
fi
AC_OUTPUT

# -----------------------------------------
# show results
# -----------------------------------------

AC_MSG_NOTICE([

!!!
!!! Configuration of $PACKAGE_NAME $PACKAGE_VERSION done!
!!!
!!! Type "make" to compile and 
!!!      "make install" to install it afterwards.
!!!
!!! For further information on $PACKAGE_NAME visit:
!!!   $PACKAGE_WEBSITE
!!!
!!! IMPORTANT:
!!! This is an UNSUPPORTED ALPHA release for developers only.
!!!
!!! DO NOT EXPECT THE MAKEFILE OR THE PROGRAM ITSELF TO WORK
!!!
!!! If you want to contribute, visit the IRC-Channel instead:
!!!   $PACKAGE_IRC
!!!
!!! PLEASE DO NOT SEND BUGREPORTS FOR THIS VERSION.
!!!
])

# TODO: insert this in the public beta release
#!!! In case you find a bug send a bugreport to:
#!!!   $PACKAGE_BUGREPORT
#!!! You might as well ask for help at the IRC-Channel
#!!!   $PACKAGE_IRC


