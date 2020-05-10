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
        [https://github.com/UltraStar-Deluxe/USDX/issues/])
# specify the website here
PACKAGE_WEBSITE="http://usdx.eu/"
AC_SUBST(PACKAGE_WEBSITE)
# specify the IRC-channel here
PACKAGE_IRC="#usdx at freenode.net"
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
CMAKE_FIND_BINARY
[[ -n "$CMAKE_BINARY" ]] || CMAKE_BINARY=false

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
AC_ARG_WITH([portaudio],
    [AS_HELP_STRING([--with-portaudio],
      [enable audio capture with PortAudio @<:@default=check@:>@])],
    [with_portmixer=$withval], [with_portaudio="check"])

AC_ARG_WITH([portmixer],
    [AS_HELP_STRING([--with-portmixer],
      [enable portmixer audio-mixer support @<:@default=check@:>@])],
    [with_portmixer=$withval], [with_portmixer="check"])

# add projectM option
AC_ARG_WITH([libprojectM], 
    [AS_HELP_STRING([--with-libprojectM],
      [enable projectM visualization support @<:@default=no@:>@])],
    [with_libprojectM=$withval], [with_libprojectM="no"])

# add OpenCV option
AC_ARG_WITH([opencv-cxx-api],
    [AS_HELP_STRING([--with-opencv-cxx-api],
      [use current OpenCV C++ API @<:@default=no@:>@])],
    [with_opencv_cxx_api=$withval], [with_opencv_cxx_api="no"])

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
AC_MSG_CHECKING([size of lua_Integer])
PKG_VALUE([lua],[CFLAGS],[cflags],[$lua_LIB_NAME],[C compiler flags for Lua])
old_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS $lua_CFLAGS"
AC_COMPUTE_INT([LUA_INTEGER_BITS],[(long)sizeof(lua_Integer)*8],[#include <lua.h>])
CFLAGS="$old_CFLAGS"
AC_SUBST(LUA_INTEGER_BITS)
AC_MSG_RESULT([$LUA_INTEGER_BITS bits])
lua_lib_candidates=
for i in $lua_LIBS ; do
	case "$i" in
	-l*) lua_lib_candidates="$lua_lib_candidates ${i#-l}"
	esac
done
old_LIBS=$LIBS
AC_SEARCH_LIBS([luaL_newstate], [$lua_lib_candidates], [lua_LIB_NAME=${ac_cv_search_luaL_newstate#-l}], [], [])
LIBS=$old_LIBS
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
    if [[ x$enable_osx_fink != x ]]; then
        PKG_CONFIG_PATH=`find /sw/lib -name libavcodec.pc | tail -n 1 | xargs dirname`:$PKG_CONFIG_PATH
    elif [[ x$enable_osx_brew != x ]]; then
        PKG_CONFIG_PATH=`find /usr/local/opt/ffmpeg*/lib /usr/local/lib -name libavcodec.pc | tail -n 1 | xargs dirname`:$PKG_CONFIG_PATH
    else
        PKG_CONFIG_PATH=`find $FPCDIR/lib/ffmpeg* -name libavcodec.pc | tail -n 1 | xargs dirname`:$PKG_CONFIG_PATH
    fi
    export PKG_CONFIG_PATH
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

# map avutil library version to ffmpeg version
AC_MSG_CHECKING([version of ffmpeg])
# the order is ([avutil], [avcodec], [avformat], [swscale], [swresample])
if   FFMPEG_LIBS_COMPATIBLE([56014100], [58018100], [58012100], [5001100], [3001100]); then
    FFMPEG_VERSION="4.0"
elif FFMPEG_LIBS_COMPATIBLE([55078100], [57107100], [57083100], [4008100], [2009100]); then
    FFMPEG_VERSION="3.4"
elif FFMPEG_LIBS_COMPATIBLE([55034100], [57064100,57093100], [57056100], [4002100], [2003100]); then
    FFMPEG_VERSION="3.2" # and 3.3
elif FFMPEG_LIBS_COMPATIBLE([55027100,55028100], [57048101], [57040101], [4001100], [2001100]); then
    FFMPEG_VERSION="3.1"
elif FFMPEG_LIBS_COMPATIBLE([55017103,55018100], [57024102,57037100], [57025100], [4000100], [2000101]); then
    FFMPEG_VERSION="3.0"
elif FFMPEG_LIBS_COMPATIBLE([54030100], [56060100], [56040101], [3001101], [1002101]); then
    FFMPEG_VERSION="2.8"
elif test $libavutil_VERSION_INT -le 54030100 -a $libavutil_VERSION_INT -ge 54027100; then
    FFMPEG_VERSION="2.7"
elif test $libavutil_VERSION_INT -eq 54020100; then
    FFMPEG_VERSION="2.6"
elif test $libavutil_VERSION_INT -eq 54015100; then
    FFMPEG_VERSION="2.5"
elif test $libavutil_VERSION_INT -eq 54007100; then
    FFMPEG_VERSION="2.4"
elif test $libavutil_VERSION_INT -eq 52066100; then
    FFMPEG_VERSION="2.2"
elif test $libavutil_VERSION_INT -le 52048101 -a $libavutil_VERSION_INT -ge 52048100; then
    FFMPEG_VERSION="2.1"
elif test $libavutil_VERSION_INT -le 52038100 -a $libavutil_VERSION_INT -ge 52038000; then
    FFMPEG_VERSION="2.0"
elif test $libavutil_VERSION_INT -le 52018100 -a $libavutil_VERSION_INT -ge 52018000; then
    FFMPEG_VERSION="1.2"
elif test $libavutil_VERSION_INT -le 52013100 -a $libavutil_VERSION_INT -ge 52013000; then
    FFMPEG_VERSION="1.1"
elif test $libavutil_VERSION_INT -le 51073101 -a $libavutil_VERSION_INT -ge 51073000; then
    FFMPEG_VERSION="1.0"
elif test $libavutil_VERSION_INT -le 51054100 -a $libavutil_VERSION_INT -ge 51054000; then
    FFMPEG_VERSION="0.11"
elif test $libavutil_VERSION_INT -le 51035100 -a $libavutil_VERSION_INT -ge 51034101; then
    FFMPEG_VERSION="0.10"
elif test $libavutil_VERSION_INT -eq 51032000; then
    FFMPEG_VERSION="0.9"
elif test $libavutil_VERSION_INT -le 51022002 -a $libavutil_VERSION_INT -ge 51009001; then
    FFMPEG_VERSION="0.8"
elif test $libavutil_VERSION_INT -eq 50043000; then
    FFMPEG_VERSION="0.7"
elif test $libavutil_VERSION_INT -le 50024000 -a $libavutil_VERSION_INT -ge 49000001; then
    FFMPEG_VERSION="0"
else
    AC_MSG_ERROR([

Unsupported ffmpeg version, most recent version supported is 3.3.
])
fi
AX_EXTRACT_VERSION(FFMPEG, $FFMPEG_VERSION)
AC_SUBST(FFMPEG_VERSION)
AC_MSG_RESULT(@<:@$FFMPEG_VERSION@:>@)

AC_MSG_NOTICE([Checking for C++ compiler])
AC_PROG_CXX
AC_LANG_PUSH([C++])
AC_MSG_CHECKING([if C++ toolchain works])
cxx_works=no
use_cxx=no
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],[])],
		  [AC_LINK_IFELSE([AC_LANG_PROGRAM([],[])],
				  [AC_MSG_RESULT([yes])
				   cxx_works=yes],
				  [AC_MSG_RESULT([linking C++ programs does not work])])],
		  [AC_MSG_RESULT([C++ compiler does not work])])
AC_LANG_POP([C++])

# find projectM version
libprojectM_PKG="libprojectM >= 0.98"
PKG_HAVE([libprojectM], [$libprojectM_PKG], no)
PKG_VERSION([libprojectM], [$libprojectM_PKG])
if [[ "$libprojectM_VERSION_MAJOR" -ge 3 ]] ; then
	AC_MSG_FAILURE([libprojectM >= 3 not yet supported])
fi
AC_SUBST_DEFINE(HAVE_PROJECTM, $libprojectM_HAVE)
# get projectM include-dir
PKG_VALUE([libprojectM], [INCLUDEDIR], [variable=includedir], [$libprojectM_PKG], 
          [C-Header include-dir (e.g. /usr/include)])
# get projectM data-dir (for preset- and font-dir)
PKG_VALUE([libprojectM], [DATADIR], [variable=pkgdatadir], [$libprojectM_PKG], 
          [projectM data-directory for presets etc. (e.g. /usr/share/projectM)])
# check if we need the c-wrapper
if [[ "$libprojectM_VERSION_MAJOR" -ge 1 ]]; then
	if [[ "$cxx_works" = yes ]] ; then
		libprojectM_USE_CWRAPPER=yes
		use_cxx=yes
	else
		AC_MSG_FAILURE([Need C++ compiler to build libprojectM wrapper])
	fi
else
	libprojectM_USE_CWRAPPER=no
fi
AC_SUBST(USE_PROJECTM_CWRAPPER, $libprojectM_USE_CWRAPPER)

opencv_core_standalone=no
opencv_imgproc_standalone=no
opencv_videoio_standalone=no
if [[ "$with_opencv_cxx_api" = yes ]] ; then
	if [[ "$cxx_works" = yes ]] ; then
		AX_CXX_COMPILE_STDCXX([11])
		CMAKE_FIND_PACKAGE([OpenCV], [CXX], [GNU], , [OpenCV_HAVE=yes], [OpenCV_HAVE=no])
		if [[ "$OpenCV_HAVE" != yes ]] ; then
			PKG_HAVE([OpenCV], [opencv4], no)
			if [[ "$OpenCV_HAVE" != yes ]] ; then
				PKG_HAVE([OpenCV], [opencv >= 3.0.0], yes)
				OpenCV_CXXFLAGS=`$PKG_CONFIG --cflags "opencv >= 3.0.0"`
			else
				OpenCV_CXXFLAGS=`$PKG_CONFIG --cflags opencv4`
			fi
		fi
		opencv_USE_CWRAPPER=yes
		use_cxx=yes
		for lib in $OpenCV_LIBS ; do
			case "${lib##*/}" in
			*opencv_core*)    opencv_core_standalone=yes ;;
			*opencv_imgproc*) opencv_imgproc_standalone=yes ;;
			*opencv_videoio*) opencv_videoio_standalone=yes ;;
			*) continue ;;
			esac
			case "$lib" in
			-L)  LIBS="$LIBS $lib" ;;
			*/*) LIBS="$LIBS -L${lib%/*}" ;;
			esac
		done
		AC_SUBST(OpenCV_CXXFLAGS)
	else
		AC_MSG_FAILURE([Need C++ compiler to build OpenCV wrapper])
	fi
else
	opencv_USE_CWRAPPER=no
fi
AC_SUBST_DEFINE(USE_OPENCV_CWRAPPER, $opencv_USE_CWRAPPER)
AC_SUBST(USE_OPENCV_CWRAPPER, $opencv_USE_CWRAPPER)
AC_SUBST_DEFINE(OPENCV_CORE_STANDALONE, $opencv_core_standalone)
AC_SUBST_DEFINE(OPENCV_IMGPROC_STANDALONE, $opencv_imgproc_standalone)
AC_SUBST_DEFINE(OPENCV_VIDEOIO_STANDALONE, $opencv_videoio_standalone)

if [[ "$use_cxx" = yes ]] ; then
    # Hack to link to libstdc++ through libprojectM where needed
    # Of course only works if libprojectM is a shared library
    # --no-copy-dt-needed-entries is the default since Binutils 2.22
    if test x$ac_cv_prog_ppc_copy_dt_needed_entries = xyes ; then
        PFLAGS_EXTRA="$PFLAGS_EXTRA -k\"--copy-dt-needed-entries\""
    fi
fi

# find portaudio
PKG_HAVE([portaudio], [portaudio-2.0], no)
PKG_VERSION([portaudio], [portaudio-2.0])
AC_SUBST_DEFINE(HAVE_PORTAUDIO, $portaudio_HAVE)

# find portmixer
PKG_HAVE([portmixer], [portmixer], no)
AC_SUBST_DEFINE(HAVE_PORTMIXER, $portmixer_HAVE)

AC_ARG_ENABLE(portmidi, [AS_HELP_STRING([--disable-portmidi], [Disable PortTime Midi loop])])
AC_ARG_WITH(porttime, [AS_HELP_STRING([--with-porttime], [Use PortTime instead of SDL Midi loop])])

if [[ x$enable_portmidi != xno ]] ; then
    AC_CHECK_LIB([portmidi], [Pm_Initialize], [
        portmidi_HAVE=yes
        if [[ x$with_porttime = xyes ]] ; then
            porttime_HAVE=yes
            AC_CHECK_LIB([portmidi], [Pt_Start], [porttime_LIB_NAME=portmidi],
                [AC_CHECK_LIB([porttime], [Pt_Start], [porttime_LIB_NAME=porttime],
                     [AC_MSG_ERROR([PortTime library not found])])])
        else
            porttime_HAVE=no
        fi
        ], [
        if [[ x$enable_portmidi = xyes ]] ; then
            AC_MSG_ERROR([PortMidi library not found])
        else
            portmidi_HAVE=no
        fi
        ])
else
	portmidi_HAVE=no
fi
AC_SUBST_DEFINE(HAVE_PORTMIDI, $portmidi_HAVE)
AC_SUBST_DEFINE(HAVE_PORTTIME, $porttime_HAVE)
AC_SUBST(porttime_LIB_NAME)

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
if [[ x$opencv_USE_CWRAPPER = xyes ]]; then
    AC_CONFIG_FILES([src/lib/openCV3/Makefile])
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


