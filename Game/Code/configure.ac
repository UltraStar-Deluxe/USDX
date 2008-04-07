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
AC_INIT([UltraStar-Deluxe], 
        [1.1-alpha], 
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

# -----------------------------------------
# define switches
# -----------------------------------------

# print library options header
AC_ARG_WITH([cfg-dummy1], [
External Libraries:])

# add lazarus option
#AC_ARG_WITH([lazarus], 
#    [AS_HELP_STRING([--with-lazarus=DIR],
#      [Directory of lazarus @<:@LAZARUSDIR@:>@])],
#    [with_lazarus=$withval], [with_lazarus="yes"])
#if [[ x$with_lazarus = xno ]] ; then
#    AC_MSG_ERROR([Lazarus is required. It is impossible to build without it.]);
#fi

# add lazarus widget-type option
# the result is not used at the moment
#AC_ARG_WITH([lcl-widget-type], 
#    [AS_HELP_STRING([--with-lcl-widget-type=TYPE],
#      [Lazarus LCL Widget  Type @<:@default=check@:>@])],
#    [with_lcl_widget_type=$withval], [with_lcl_widget_type=""])
#if [[ x$with_lcl_widget_type = xno -o x$with_lcl_widget_type = xyes ]] ; then
#    AC_MSG_ERROR([Invalid LCL Widget Type (try one of gtk2/gtk/qt)]);
#fi

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

# add skip pkg-config error option
AC_ARG_ENABLE([skip-pkgconfig-errors], 
    [AS_HELP_STRING([--enable-skip-pkgconfig-errors],
      [Continue if pkg-config does not find a package @<:@default=no@:>@])],
    [skip_pkgconfig_errors=$withval], [skip_pkgconfig_errors="no"])

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
AC_ARG_ENABLE(dev-build,
    [AS_HELP_STRING([--enable-dev-build],
      [local development build])],
    [enable_dev_build="yes"], [enable_dev_build="no"])

# -----------------------------------------
# tools
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

# set to yes if a pkg-config error was skipped
pkgconfig_error_skipped="no"

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
    have_lib="no"
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
            have_lib="yes"
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
            AC_MSG_WARN([
!!! Could not find the required library $2.
]);
            # do not abort if skip-pkgconfig-errors is set
            if test x$skip_pkgconfig_errors = xno; then
               AC_MSG_ERROR([
!!! Please install $2 and try again.
!!! 
!!! If it is already installed check if "[$2].pc" is present in one 
!!! of the pkg-config search directories (e.g. /usr/lib/pkgconfig). 
!!! 
!!! If the file is present but not in the standard search path you may add
!!! its directory by prepending "PKG_CONFIG_PATH=mylibs_dir/pkgconfig/" to 
!!! your configure call.
!!! See the man-page on pkg-config for further information.
!!! 
!!! Some linux distributions do not provide a .pc-file by the default
!!! package so you might have to install a special dev-package.
!!! 
!!! If all of this does not help you may consider to skip this error and 
!!! adjust the configuration file yourself.
!!! To do this call "./configure --enable-skip-pkgconfig-errors ...".
])
            else
                pkgconfig_error_skipped="yes"
            fi
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
# check for lazarus
# -----------------------------------------

# set dirs to check for lazarus
#if [[ x$with_lazarus = xyes ]]; then
   # use default path (ignore the standard path (PATH) because the lazarus executable might
   # be in /usr/bin, but what we want is the program directory with the libs)   
#   LAZARUS_CHECK_DIRS="/usr/bin/lazarus:/usr/lib/lazarus:/usr/share/lazarus:/opt/lazarus:/usr/local/bin/lazarus:/usr/local/lib/lazarus:/usr/local/share/lazarus"
#else
    # check if dir is valid
#    if [[ -d $with_lazarus ]] ; then
#        LAZARUS_CHECK_DIRS=$with_lazarus
#    else
#        AC_MSG_ERROR([LAZARUSDIR is not a directory.]);
#    fi
#fi

# find lazarus 
#AC_PATH_PROG(LAZARUS, lazarus, no, [$LAZARUS_CHECK_DIRS])
#if [[ $LAZARUS = "no" ]] ; then
#    AC_MSG_ERROR([Could not find lazarus. Please install lazarus and try again.]);
#fi
#LAZARUS_DIR=`AS_DIRNAME(["$LAZARUS"])`

# get lazarus version
#AC_MSG_CHECKING(for version of lazarus)
# (do this in a temporary shell to prevent a change of directory)
#LAZARUS_VERSION=`(cd "$LAZARUS_DIR/tools/install"; ./get_lazarus_version.sh)`
#LAZARUS_VERSION=`cat ide/version.inc | tr -d "' \t"` 
#AC_SPLIT_VERSION([LAZARUS], [$LAZARUS_VERSION])
#AC_MSG_RESULT(@<:@$LAZARUS_VERSION@:>@)

# check if LCL Widget type is valid
#if [[ x$with_lcl_widget_type = x ]]; then
#    # search for standard types
#    LCL_CHECK_WIDGET_TYPES="gtk2 gtk qt win32 carbon"
#else
    # search for user defined type only
#    LCL_CHECK_WIDGET_TYPES="$with_lcl_widget_type"
#fi

# LCL widget specific base dir
#LCL_UNIT_DIR="$LAZARUS_DIR/lcl/units/$FPC_PROCESSOR-$FPC_PLATFORM"

# check for a supported widget type
#LCL_WIDGET_TYPE=""
#for widget_type in $LCL_CHECK_WIDGET_TYPES; do
#    widget_dir="$LCL_UNIT_DIR/$widget_type"
#    AC_MSG_CHECKING([whether LCL Widget Type "$widget_type" is supported])
#    if [[ -d "$widget_dir" ]]; then
#        LCL_WIDGET_TYPE=$widget_type
#        AC_MSG_RESULT(yes)
#        break
#    else
#        AC_MSG_RESULT(no)
#    fi
#done

# check if a widget type was found
#if [[ x$LCL_WIDGET_TYPE = x ]]; then
#    AC_MSG_ERROR([
#!!! Could not detect the LCL Widget Type. 
#!!! Specify the widget type with the --with-lcl-widget-type option.])
#fi

# -----------------------------------------
# check for libraries
# -----------------------------------------

# find sdl
AC_PKG_CHECK_VERSION(SDL, [sdl], yes)

# find sqlite3
AC_PKG_CHECK_VERSION(SQLITE3, [sqlite3], yes)

# find ffmpeg
# Note: do not use the min/max version parameters with ffmpeg
# otherwise it might fail in ubuntu due to a wrong version number 
# format in ffmpeg's .pc-files.
# For example: 0d.51.1.2 instead of the correct 51.1.2.
#   A check for a version >=52.0.0 will return version 0d.51.1.2 
#   although it is lower because pkg-config is confused by the 0d.
# Use [mylib]_VERSION_INT instead
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

#AC_SUBST(LAZARUS_DIR)
#AC_SUBST(LCL_WIDGET_TYPE)

AC_SUBST_DEFINE(USE_LOCAL_DIRS, $enable_dev_build)
if [[ x$enable_dev_build = xyes ]]; then
    AC_SUBST(install_type, ["dev"])
else
    AC_SUBST(install_type, ["release"])    
fi
AC_SUBST(suffix)
AC_SUBST(logrootdir, [$with_logrootdir])
AC_DEFINE_DIR(sharerootdir, datarootdir)

#AC_SUBST(LDFLAGS)
#AC_SUBST(LIBS)

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

if [[ x$pkgconfig_error_skipped = xyes ]]; then
   AC_MSG_WARN([
??? pkg-config did not find all of the required libraries.
??? Edit "config-linux.inc" to add the missing parts.
??? If one of your libraries' directories is not in the linker's 
??? search-path add it to the LDFLAGS environment variable and 
??? rerun configure (e.g. ./configure LDFLAGS="-Lmylibdir" ...).
])
fi

