#
# ultrastardx configure.ac script
#
# by UltraStar Deluxe Team
#
# Execute "autogen.sh" or "make reconf"
# to create the configure script.
#
# Helper macros have been separated to
#   ax_extract_version.m4 (AX_EXTRACT_VERSION)
#   pkg_config_utils.m4   (PKG_VALUE, PKG_VERSION, PKG_HAVE)
#

# Require autoconf >= 2.61
AC_PREREQ(2.61)

# Init autoconf
AC_INIT([ultrastardx],
        m4_esyscmd_s([cat VERSION]),
        [http://sourceforge.net/tracker/?group_id=191560&atid=937872])
# specify the website here
PACKAGE_WEBSITE="http://www.ultrastardeluxe.org/"
AC_SUBST(PACKAGE_WEBSITE)
# specify the IRC-channel here
PACKAGE_IRC="#ultrastardx at quakenet.org"
AC_SUBST(PACKAGE_IRC)

AC_CONFIG_MACRO_DIR([dists/autogen/m4])
# Specify a source-file so autoconf can check if the source-dir exists
AC_CONFIG_SRCDIR(src/ultrastardx.dpr)

# Set the path to install-sh
AC_CONFIG_AUX_DIR(dists/autogen)

# show features and packages in one list
AC_PRESERVE_HELP_ORDER

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

# print misc options header
AC_ARG_WITH([cfg-dummy2], [
Development options:])

# add DEPRECATED global and local options
AC_ARG_ENABLE(global, [AS_HELP_STRING([--enable-global], [(DEPRECATED, DO NOT USE]))])
AC_ARG_ENABLE(local, [AS_HELP_STRING([--enable-local], [(DEPRECATED, DO NOT USE]))])

if [[ x$enable_global != x -o x$enable_local != x ]]; then
   AC_MSG_NOTICE([

!!! NOTE: --enable-global and --enable-local are deprecated:
!!! - global build: just type "make" and "make install"
!!! - local  build: just type "make" and start "game/ultrastardx"
])
   sleep 2
fi

AC_ARG_ENABLE(debug,
  [AS_HELP_STRING([--enable-debug],
    [Enable debug build @<:@default=no@:>@])],
  [test $enableval = "yes" && ENABLE_DEBUG="yes"], [])
AC_SUBST(ENABLE_DEBUG)

# OSX Packaging
AC_ARG_ENABLE(osx-fink, [AS_HELP_STRING([--enable-osx-fink], [Enable fink packaging for OSX])])
AC_ARG_ENABLE(osx-brew, [AS_HELP_STRING([--enable-osx-brew], [Enable homebrew packaging for OSX])])

if [[ x$enable_osx_fink != x -a x$enable_osx_brew != x ]]; then
    AC_MSG_ERROR([
!!! You can't use --enable-osx-fink with --enable-osx-brew at the same time
])
fi

# Default (homebrew)
USE_OSX_PACKAGING="brew"
USE_OSX_PACKAGING_LIBDIR="/usr/local/lib"

if [[ x$enable_osx_fink != x ]]; then
    USE_OSX_PACKAGING="fink"
    USE_OSX_PACKAGING_LIBDIR="/sw/lib"
fi

AC_SUBST(USE_OSX_PACKAGING, $USE_OSX_PACKAGING)
AC_SUBST(USE_OSX_PACKAGING_LIBDIR, $USE_OSX_PACKAGING_LIBDIR)

# -----------------------------------------
# check for compilers
# -----------------------------------------

AC_CANONICAL_HOST

# find and test the freepascal compiler
# sets PFLAGS, FPC_VERSION, FPC_DEBUG, etc.
AC_PROG_FPC

if [[ "$FPC_VERSION_MAJOR" -lt 3 ]]; then
   AC_MSG_ERROR([
!!! fpc (the free Pascal compiler) is required in at least
!!! version 3.0.0, but only version $FPC_VERSION was found.
!!! Please install a newer fpc version.
]) 
fi

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

if test x$FPC_PLATFORM = xdarwin; then
   AC_MACOSX_VERSION
fi

# -----------------------------------------
# check for libraries
# -----------------------------------------

# find libpng
# pkgconfig name for 1.2 is "libpng12", for 1.4 "libpng"
PKG_HAVE([libpng], [libpng12], no)
if [[ x$libpng_HAVE = xyes ]]; then
    PKG_VERSION([libpng], [libpng12])
else
    # check for the generic .pc file
    PKG_HAVE([libpng], [libpng >= 1.2], yes)
    PKG_VERSION([libpng], [libpng >= 1.2])
fi

# find sdl2
PKG_HAVE([sdl2], [sdl2], yes)

# find SDL2_image
PKG_HAVE([sdl2_image], [SDL2_image], yes)

# find freetype
PKG_HAVE([freetype], [freetype2], yes)

# find sqlite3
PKG_HAVE([sqlite3], [sqlite3], yes)

# find lua 5.3, 5.2 or 5.11
# (K)Ubuntu uses lua5.2.pc and lua5.1.pc, 
# Mac OS X and other linux distributions use lua.pc
# Archlinux aur package for 5.3 use lua5.3.pc in order
# to make it work alongside the 5.2 version.
PKG_HAVE([lua], [lua5.3], check)
lua_LIB_NAME="lua5.3"
if [[ x$lua_HAVE = xyes ]]; then
	PKG_VERSION([lua], [lua5.3])
else
	PKG_HAVE([lua], [lua5.2], check)
	lua_LIB_NAME="lua5.2"
	if [[ x$lua_HAVE = xyes ]]; then
		PKG_VERSION([lua], [lua5.2])
	else
		PKG_HAVE([lua], [lua5.1], check)
		lua_LIB_NAME="lua5.1"
		if [[ x$lua_HAVE = xyes ]]; then
			PKG_VERSION([lua], [lua5.1])
		else
    PKG_HAVE([lua], [lua >= 5.1], yes)
    lua_LIB_NAME="lua"
			PKG_VERSION([lua], [lua])
		fi
	fi
fi
AC_SUBST(lua_LIB_NAME)

# find FFMpeg
# Note: do not use the min/max version parameters with ffmpeg
# otherwise it might fail in ubuntu due to a wrong version number 
# format in ffmpeg's .pc-files.
# For example: 0d.51.1.2 instead of the correct 51.1.2.
#   A check for version >=52.0.0 will return version 0d.51.1.2 
#   although it is lower because pkg-config is confused by the 0d.
# Use [mylib]_VERSION_INT for version-checking instead
PKG_HAVE([libavcodec], [libavcodec], check)
# The following finds libavcodec.pc in its new location on Mac OS X.
# It only takes the last from the list, assuming that this is the latest 
# version.
if [[ x$libavcodec_HAVE = xno -a x$FPC_PLATFORM = xdarwin ]]; then
    PKG_CONFIG_PATH=`find $FPCDIR/lib/ffmpeg* -name libavcodec.pc | tail -n 1 | xargs dirname`:$PKG_CONFIG_PATH export PKG_CONFIG_PATH
    PKG_HAVE([libavcodec], [libavcodec], yes)
fi
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

PKG_HAVE([libswresample], [libswresample], no)
PKG_VERSION([libswresample], [libswresample])
AC_SUBST_DEFINE(HAVE_SWRESAMPLE, $libswresample_HAVE)


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
    libprojectM_USE_CWRAPPER=yes
else
    libprojectM_USE_CWRAPPER=no
fi
AC_SUBST(USE_PROJECTM_CWRAPPER, $libprojectM_USE_CWRAPPER)

# find portaudio
PKG_HAVE([portaudio], [portaudio-2.0], yes)
PKG_VERSION([portaudio], [portaudio-2.0])
AC_SUBST_DEFINE(HAVE_PORTAUDIO, $portaudio_HAVE)

# find pcre
libpcre_PKG="libpcre"
PKG_HAVE([libpcre], [$libpcre_PKG], no)
PKG_VERSION([libpcre], [$libpcre_PKG])
AC_SUBST_DEFINE(HAVE_LIBPCRE, $libpcre_HAVE)
# get libpcre library dir
PKG_VALUE([libpcre], [LIBDIR], [variable=libdir], [$libpcre_PKG], 
          [library dir (e.g. /usr/lib, /sw/lib, ...)])
# we need the soname of the pcre library
if test x"$libpcre_HAVE" = xyes; then
    old_LIBS="$LIBS"
    LIBS="-lpcre"
    OBJDUMP="objdump"
    if test x$FPC_PLATFORM = xdarwin; then
        OBJDUMP="gobjdump"
    fi
    AC_LINK_IFELSE([AC_LANG_PROGRAM([],[])],
                   [libpcre_LIBNAME=`$OBJDUMP -p "conftest$EXEEXT" | grep pcre | awk '{print $2}'` dnl '
                    AC_SUBST([libpcre_LIBNAME],[$libpcre_LIBNAME])],
                   [AC_MSG_ERROR([failed to link])])
    LIBS="$old_LIBS"
fi

# find portmixer
PKG_HAVE([portmixer], [portmixer], no)
AC_SUBST_DEFINE(HAVE_PORTMIXER, $portmixer_HAVE)

# determine linker-flags
if test x$FPC_PLATFORM = xdarwin; then
  LDFLAGS="-macosx_version_min 10.4 -undefined dynamic_lookup -headerpad_max_install_names -L/usr/X11/lib"
fi
#LIBS=
AC_SUBST(LDFLAGS)
AC_SUBST(LIBS)

# -----------------------------------------
# create output files
# -----------------------------------------

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([src/Makefile])
AC_CONFIG_FILES([src/config-$FPC_PLATFORM.inc:src/config.inc.in])
if [[ x$libprojectM_USE_CWRAPPER = xyes ]]; then
    AC_CONFIG_FILES([src/lib/projectM/cwrapper/Makefile])
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
!!! In case you find a bug send a bugreport to:
!!!   $PACKAGE_BUGREPORT
!!! You might as well ask for help at the IRC-Channel
!!!   $PACKAGE_IRC
!!!
])

if test x$FPC_PLATFORM = xdarwin; then
    AC_MSG_NOTICE([
!!! Special target for OSX are:
!!! "make macosx-standalone-app" to create a standalone .app
!!! "make macosx-dmg" to create a dmg archive with standalone .app
!!! Selected packaging: $USE_OSX_PACKAGING with libdir $USE_OSX_PACKAGING_LIBDIR
                   ])
fi


