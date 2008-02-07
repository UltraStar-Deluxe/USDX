#
# UltraStar-DX configure.in script
#
# by UltraStar Deluxe Team
#
# Call "aclocal -I m4" to create the aclocal.m4 file.
# Process this file with autoconf to produce a configure script.
#

# Require autoconf >= 2.61
AC_PREREQ(2.61)

# Init autoconf
AC_INIT([UltraStar Deluxe], 
        [1.1 alpha], 
        [http://sourceforge.net/tracker/?group_id=191560&atid=937872])
# specify the website here
PACKAGE_WEBSITE="http://www.ultrastardeluxe.org/"
# specify the IRC-channel here
PACKAGE_IRC="#ultrastardx at quakenet.org"

# Specify a source-file so autoconf can check if the source-dir exists
AC_CONFIG_SRCDIR(UltraStar.lpr)

# This one is not used by autoconf at the moment.
# When it is used maybe we don't need aclocal's -I parameter anymore.
AC_CONFIG_MACRO_DIR(m4)

# show features and packages in one list
AC_PRESERVE_HELP_ORDER

#echo $LIBS

#define([maco], [])
#echo maco
#m4_foreach_w([arg], [-L/usr/local -l/bin/bash -L/usr/local],
#    [m4_append_uniq([maco], "arg", [ ])
#])
#echo maco

#echo m4_split([hallo o])
#AC_MSG_ERROR(oi)

# -----------------------------------------
# define switches
# -----------------------------------------

# print library options header
AC_ARG_WITH([cfg-dummy1], [
External Libraries:])

# add lazarus option
AC_ARG_WITH([lazarus], 
    [AS_HELP_STRING([--with-lazarus=DIR],
      [Directory of lazarus @<:@LAZARUSDIR@:>@])],
    [with_lazarus=$withval], [with_lazarus="yes"])
if [[ x$with_lazarus = xno ]] ; then
    AC_MSG_ERROR([Lazarus is required. It is impossible to build without it.]);
fi

# add lazarus widget-type option
AC_ARG_WITH([lcl_widget_type], 
    [AS_HELP_STRING([--with-lcl-widget-type=TYPE],
      [Lazarus LCL Widget  Type @<:@default=gtk2@:>@])],
    [with_lcl_widget_type=$withval], [with_lcl_widget_type="gtk2"])
if [[ x$with_lcl_widget_type = xno -o x$with_lcl_widget_type = xyes ]] ; then
    AC_MSG_ERROR([Invalid LCL Widget Type (try one of gtk2/gtk/qt)]);
fi

# add portaudio option
AC_ARG_WITH([portaudio], 
    [AS_HELP_STRING([--with-portaudio=DIR],
      [Directory of portaudio library @<:@PORTAUDIODIR@:>@])],
    [with_portaudio=$withval], [with_portaudio="yes"])

# add portmixer option
AC_ARG_WITH([portmixer],
    [AS_HELP_STRING([--with-portmixer@<:@=DIR@:>@],
      [Enable portmixer audio-mixer support @<:@default=check@:>@])],
    [with_portmixer=$withval], [with_portmixer="check"])

# add projectM option
AC_ARG_WITH([projectM], 
    [AS_HELP_STRING([--with-projectM@<:@=DIR@:>@],
      [Enable projectM visualization support @<:@default=check@:>@])],
    [with_projectM=$withval], [with_projectM="check"])

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

# add logdir option
AC_ARG_WITH([logrootdir], 
    [AS_HELP_STRING([--with-logrootdir=DIR],
      [logging root directory @<:@default=/var/log@:>@])],
    [with_logrootdir=$withval], [with_logrootdir="/var/log"])

# print misc options header
AC_ARG_WITH([cfg-dummy3], [
Development options:])

# add dev_layout option
AC_ARG_ENABLE(dev_build,
    [AS_HELP_STRING([--enable-dev-build],
      [local development build])],
    [enable_dev_build="yes"], [enable_dev_build="no"])

# -----------------------------------------
# tools
# -----------------------------------------

# options for make command
AC_PROG_MAKE_SET
# find tool for ln -s
AC_LN_S
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
    # replace "." and "-" with " " and ignore trailing tokens.
    # 1.23.4-r2 will be splitted to [maj=1, min=23, rel=4].
    # In addition we delete every character which is not 0-9.
    # 1.3a4-r32 will be [maj=1, min=34, rel=32].
    # 
    # Note: We use a here-document (<<< here-strings not POSIX compatible)
    # Do NOT indent the eof-delimiter
    read major minor release ignore <<eof
        `echo [$2] | tr '.-' ' ' | $SED 's/[[^0-9\ ]].*//'` 
eof

    # strip preceding 0s and set unset version-parts to 0
    [$1][_VERSION_MAJOR]=$(($major))
    [$1][_VERSION_MINOR]=$(($minor))
    [$1][_VERSION_RELEASE]=$(($release))

    AC_SUBST([$1][_VERSION_MAJOR])
    AC_SUBST([$1][_VERSION_MINOR])
    AC_SUBST([$1][_VERSION_RELEASE])
])

# AC_PKG_CHECK_VERSION(VARIABLE_PREFIX, MODULE, REQUIRED, PATH, MIN, MAX)
# Aborts if lib was not found and REQUIRED="yes".
# The PATH can be "check", "no" or "yes"
# Sets 
#   [$VARIABLE_PREFIX]_HAVE=("yes"|"no") and
#   [$VARIABLE_PREFIX]_VERSION=major.minor.release
#   [$VARIABLE_PREFIX]_VERSION_MAJOR
#   [$VARIABLE_PREFIX]_VERSION_MINOR
#   [$VARIABLE_PREFIX]_VERSION_RELEASE
AC_DEFUN([AC_PKG_CHECK_VERSION],
[
    have_lib=no
    if test x$4 != xno; then
	# on empty PATH or PATH="yes"/"check" use the default path
	if test x$4 = x -o x$4 = xyes -o x$4 = xcheck; then
            # search in default path if no path is given
	    lib_path=""	    
	else
	    # all PKG_* names are forbidden by pkg.m4 so we have to allow this first
	    m4_pattern_allow(PKG_CONFIG_LIBDIR)
            # search only in the path given
	    lib_path="PKG_CONFIG_LIBDIR=$4/pkgconfig"
        fi

	minmax_flags=""
	minmax_text=""

	# min version
	if test x$5 != x; then
	    minmax_flags="$minmax_flags --atleast-version=$5"
	    minmax_text="$minmax_text >= $5"
	fi

	# max version
	if test x$6 != x; then
	    minmax_flags="$minmax_flags --max-version=$6"
	    minmax_text="$minmax_text <= $6"
	fi

	# call pkg-config
        AC_MSG_CHECKING(for [$2]$minmax_text)
        [$1][_VERSION]=`eval $lib_path $PKG_CONFIG $minmax_flags --modversion [$2] --silence-errors`
	if test $? -eq 0; then
            have_lib=yes
	    [$1][_HAVE]="yes"
	    AC_SPLIT_VERSION([$1], $[$1][_VERSION])
	    AC_MSG_RESULT(yes @<:@$[$1][_VERSION]@:>@)
	fi
    else
        AC_MSG_CHECKING(for [$2])
    fi
    if test x$have_lib = xno; then
        [$1][_HAVE]="no"
        AC_SPLIT_VERSION([$1], [0.0.0])
        AC_MSG_RESULT(no)
	if test x$3 = xyes -o x$4 = xyes; then
	    AC_MSG_ERROR([Could not find required library $2. Please install $2 and try again.]);
	fi
    fi
])

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

# find pkg-config
PKG_PROG_PKG_CONFIG()
if [[ x$PKG_CONFIG = x ]]; then
   AC_MSG_ERROR([
!!! pkg-config was not found on your system. 
!!! It is needed to determine the versions of your libraries.
!!! Install it and try again.])
fi

# -----------------------------------------
# check for libraries
# -----------------------------------------

# set dirs to check for lazarus
if [[ x$with_lazarus = xyes ]]; then
   # use default path (ignore the standard path (PATH) because the lazarus executable might
   # be in /usr/bin, but what we want is the program directory with the libs)   
   LAZARUS_CHECK_DIRS="/usr/bin/lazarus:/usr/lib/lazarus:/usr/share/lazarus:/opt/lazarus:/usr/local/bin/lazarus:/usr/local/lib/lazarus:/usr/local/share/lazarus"
else
    # check if dir is valid
    if [[ -d $with_lazarus ]] ; then
        LAZARUS_CHECK_DIRS=$with_lazarus
    else
        AC_MSG_ERROR([LAZARUSDIR is not a directory.]);
    fi
fi

# find lazarus 
AC_PATH_PROG(LAZARUS, lazarus, no, [$LAZARUS_CHECK_DIRS])
if [[ $LAZARUS = "no" ]] ; then
    AC_MSG_ERROR([Could not find lazarus. Please install lazarus and try again.]);
fi
LAZARUS_DIR=`AS_DIRNAME(["$LAZARUS"])`

# get lazarus version
AC_MSG_CHECKING(for version of lazarus)
# (do this in a temporary shell to prevent a change of directory)
LAZARUS_VERSION=`(cd "$LAZARUS_DIR/tools/install"; ./get_lazarus_version.sh)`
#LAZARUS_VERSION=`cat ide/version.inc | tr -d "' \t"` 
AC_SPLIT_VERSION([LAZARUS], [$LAZARUS_VERSION])
AC_MSG_RESULT(@<:@$LAZARUS_VERSION@:>@)

# check if LCL Widget type is valid
LCL_WIDGET_TYPE=$with_lcl_widget_type
LCL_UNIT_DIR="$LAZARUS_DIR/lcl/units/$FPC_PROCESSOR-$FPC_PLATFORM"
LCL_WIDGET_DIR="$LCL_UNIT_DIR/$LCL_WIDGET_TYPE"
AC_MSG_CHECKING([whether LCL Widget Type "$LCL_WIDGET_TYPE" is valid])
if [[ -d "$LCL_WIDGET_DIR" ]]; then
    AC_MSG_RESULT(yes)
else
    AC_MSG_RESULT(no)
    AC_MSG_ERROR(LCL Widget Type not available: directory "$LCL_WIDGET_DIR" not found.)
fi

# find sdl
AC_PKG_CHECK_VERSION(SDL, [sdl], yes)

# find sqlite3
AC_PKG_CHECK_VERSION(SQLITE3, [sqlite3], yes)

# find ffmpeg
AC_PKG_CHECK_VERSION(AVCODEC, [libavcodec], yes)
AC_CHECK_LIB(avcodec, avcodec_decode_audio)
AC_CHECK_LIB(avcodec, avcodec_decode_audio2)
AC_CHECK_LIB(avcodec, img_convert)
AC_PKG_CHECK_VERSION(AVFORMAT, [libavformat], yes)
AC_PKG_CHECK_VERSION(AVUTIL, [libavutil], yes)
AC_PKG_CHECK_VERSION(SWSCALE, [libswscale], no)

if [[ x$AVCODEC_HAVE = xyes -a x$AVFORMAT_HAVE = xyes -a x$AVUTIL_HAVE = xyes ]]; then
    FFMPEG_HAVE=yes
else
    FFMPEG_HAVE=no
fi

# find projectM version
AC_PKG_CHECK_VERSION(PROJECTM, [libprojectM], no, $with_projectM, [], 0.99)

# find portaudio version
AC_PKG_CHECK_VERSION(PORTAUDIO, [portaudio-2.0], yes, $with_portaudio)
AC_PKG_CHECK_VERSION(PORTMIXER, [portmixer], no, $with_portmixer)

# -----------------------------------------
# defines for config.inc/versions.pas 
# -----------------------------------------

# AC_SUBST_DEFINE(DEFINE_SUFFIX, IS_DEFINED)
AC_DEFUN([AC_SUBST_DEFINE],
[
    if [[ x$2 = xyes ]]; then
        DEFINE_[$1]=DEFINE
    else
	DEFINE_[$1]=UNDEF
    fi
    AC_SUBST(DEFINE_[$1])
])

AC_SUBST_DEFINE(DEBUG, $FPC_DEBUG)

AC_SUBST(AVCODEC_LIB)
AC_SUBST(AVFORMAT_LIB)
AC_SUBST(AVUTIL_LIB)
AC_SUBST_DEFINE(HAVE_FFMPEG, $FFMPEG_HAVE)

AC_SUBST(SWSCALE_LIB)
AC_SUBST_DEFINE(HAVE_SWSCALE, $SWSCALE_HAVE)

AC_SUBST(PROJECTM_LIB)
AC_SUBST_DEFINE(HAVE_PROJECTM_0_9, $PROJECTM_HAVE)
AC_SUBST_DEFINE(HAVE_PROJECTM_1_0_PLUS, $PROJECTM_HAVE)

AC_SUBST(PORTAUDIO_LIB)
AC_SUBST_DEFINE(HAVE_PORTAUDIO, $PORTAUDIO_HAVE)

AC_SUBST(PORTMIXER_LIB)
AC_SUBST_DEFINE(HAVE_PORTMIXER, $PORTMIXER_HAVE)

AC_SUBST(LAZARUS_DIR)
AC_SUBST(LCL_WIDGET_TYPE)

AC_SUBST_DEFINE(USE_LOCAL_DIRS, $enable_dev_build)
if [[ x$enable_dev_build = xyes ]]; then
    AC_SUBST(install_type, ["dev"])
else
    AC_SUBST(install_type, ["release"])    
fi
AC_SUBST(suffix)
AC_SUBST(logrootdir, [$with_logrootdir])
AC_DEFINE_DIR(sharerootdir, datarootdir)

AC_SUBST(PLIBS)
AC_SUBST(PACKAGE_WEBSITE)

# -----------------------------------------
# create output files
# -----------------------------------------

AC_CONFIG_FILES([config-linux.inc:config.inc.in])
AC_CONFIG_FILES([Makefile])
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
!!! and do not waste your time with bugfixing this version.
!!! Bugs might already be fixed in the developers' working copies.
!!! If you want to contribute, visit the IRC-Channel instead:
!!!   $PACKAGE_IRC
!!!
!!! Known issues:
!!! - Audio-output and -input are not working yet
!!! - The editor is broken
!!!
!!! PLEASE DO NOT SEND BUGREPORTS FOR THIS VERSION.
!!!
])

# TODO: insert this in the public beta release
#!!! In case you find a bug send a bugreport to:
#!!!   $PACKAGE_BUGREPORT
#!!! You might as well ask for help at the IRC-Channel
#!!!   $PACKAGE_IRC
