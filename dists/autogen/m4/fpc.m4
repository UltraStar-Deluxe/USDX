# Based on fpc.m4 Version 1.1 provided with
#   J Sound System (http://jss.sourceforge.net)
# 
# Originally written by
#   Matti "ccr/TNSP" Hamalainen
#   (C) Copyright 2000-2001 Tecnic Software productions (TNSP)
# 
# Mostly rewritten by 
#   UltraStar Deluxe Team

# SYNOPSIS
#
#   AC_PROG_FPC
#
# DESCRIPTION
#
#   Checks for Free Pascal Compiler
#
#   Sets:
#     PPC            : fpc command          
#     FPCMAKE        : fpcmake command       
#
#     PFLAGS         : flags passed to fpc (overwrite default)
#     PFLAGS_BASE    : base flags (release + debug)
#     PFLAGS_EXTRA   : additional flags (appended to default PFLAGS)
#     PFLAGS_DEBUG   : flags used in debug build
#     PFLAGS_RELEASE : flags used in release build
#
#     Note: 
#       all PFLAGS/PFLAGS_XYZ vars are set to $(PFLAGS_XYZ_DEFAULT)
#       if not set by the user, so the Makefile can assign default
#       values to them.
#
#     FPC_VERSION        : fpc version string, e.g. 2.3.1
#     FPC_VERSION_MAJOR  : major version (here 2) 
#     FPC_VERSION_MINOR  : minor version (here 3)
#     FPC_VERSION_RELEASE: release version (here 1)
#
#     FPC_PLATFORM   : platform of the target (linux/darwin/win32/...)
#     FPC_PROCESSOR  : processor of the target, (i386/...)
#     FPC_CPLATFORM  : platform of the compiler host, (linux/darwin/win32/...)
#     FPC_CPROCESSOR : processor of the compiler host, (i386/...)
#     FPC_TARGET     : FPC_PROCESSOR-FPC_PLATFORM (e.g. i386-linux)
#
#     FPC_PREFIX     : prefix of fpc install path, (default: /usr)
#     FPC_BASE_PATH  : $FPC_PREFIX/lib/fpc/$FPC_VERSION
#     FPC_UNIT_PATH  : $FPC_BASE_PATH/units/$FPC_TARGET
#
# See "fpc -i" for a list of supported platforms and processors

AC_DEFUN([AC_PROG_FPC], [

##
# User PFLAGS
##

AC_ARG_VAR(PFLAGS, [Free Pascal Compiler flags (replaces all other flags)])
AC_ARG_VAR(PFLAGS_BASE,    [Free Pascal Compiler base flags, e.g. -Si])
AC_ARG_VAR(PFLAGS_DEBUG,   [Free Pascal Compiler debug flags, e.g. -gl])
AC_ARG_VAR(PFLAGS_RELEASE, [Free Pascal Compiler release flags, e.g. -O2])
AC_ARG_VAR(PFLAGS_EXTRA,   [Free Pascal Compiler additional flags])

##
# Compiler options
##

AC_ARG_ENABLE(dummy_fpc1,[
Free Pascal Compiler specific options:])

# fpc path
AC_ARG_WITH(fpc,
  [AS_HELP_STRING([--with-fpc=DIR],
    [Directory of the FPC executable @<:@PATH@:>@])],
  [PPC_PATH=$withval], [])

# verbose
AC_ARG_ENABLE(verbose,
  [AS_HELP_STRING([--disable-verbose],
    [Disable verbose compiler output @<:@default=no@:>@])],
  [test x$enableval = xno && PFLAGS_EXTRA="$PFLAGS_EXTRA -v0Bew"], [])

# gprof
AC_ARG_ENABLE(gprof,
  [AS_HELP_STRING([--enable-gprof],
    [Enable profiling with gprof @<:@default=no@:>@])],
  [test x$enableval = xyes && PFLAGS_EXTRA="$PFLAGS_EXTRA -pg"], [])

# valgrind
AC_ARG_ENABLE(valgrind,
  [AS_HELP_STRING([--enable-valgrind],
    [Enable debugging with valgrind @<:@default=no@:>@])],
  [test x$enableval = xyes && PFLAGS_EXTRA="$PFLAGS_EXTRA -gv"], [])

# heaptrace
AC_ARG_ENABLE(heaptrace,
  [AS_HELP_STRING([--enable-heaptrace],
    [Enable heaptrace (memory corruption detection) @<:@default=no@:>@])],
  [test x$enableval = xyes && PFLAGS_EXTRA="$PFLAGS_EXTRA -gh"], [])

# range-checks
AC_ARG_ENABLE(rangechecks,
  [AS_HELP_STRING([--enable-rangechecks],
    [Enables range-checks @<:@default=no@:>@])],
  [test x$enableval = xyes && PFLAGS_EXTRA="$PFLAGS_EXTRA -Crtoi"], [])

# allow execstack (see noexecstack compiler check below)
AC_ARG_ENABLE(noexecstack,
  [AS_HELP_STRING([--disable-noexecstack],
    [Allow executable stacks @<:@default=no@:>@])],
  [], [enable_noexecstack="yes"])

###
# Find compiler executable
###

PPC_CHECK_PROGS="fpc FPC ppc386 ppc PPC386"

if test -z "$PPC_PATH"; then
    PPC_PATH=$PATH
    AC_CHECK_PROGS(PPC, $PPC_CHECK_PROGS)
    AC_CHECK_PROGS(FPCMAKE, [fpcmake])
else
    AC_PATH_PROGS(PPC, $PPC_CHECK_PROGS, [], $PPC_PATH)
    AC_PATH_PROGS(FPCMAKE, [fpcmake], [], $PPC_PATH)
fi
if test -z "$PPC"; then
    AC_MSG_ERROR([no Free Pascal Compiler found in $PPC_PATH])
fi

###
# Get the FPC compiler info
###

AC_MSG_CHECKING([version of fpc])
FPC_VERSION=`${PPC} -iV`
AX_EXTRACT_VERSION(FPC, $FPC_VERSION)
AC_SUBST(FPC_VERSION)
AC_MSG_RESULT([@<:@$FPC_VERSION@:>@])

FPC_PLATFORM=`${PPC} -iTO`
FPC_PROCESSOR=`${PPC} -iTP`
FPC_CPLATFORM=`${PPC} -iSO`
FPC_CPROCESSOR=`${PPC} -iSP`

FPC_TARGET=${FPC_PROCESSOR}-${FPC_PLATFORM}


AC_SUBST(FPC_PLATFORM)
AC_SUBST(FPC_PROCESSOR)
AC_SUBST(FPC_CPLATFORM)
AC_SUBST(FPC_CPROCESSOR)
AC_SUBST(FPC_TARGET)

###
# Get paths
###

if test "x$prefix" != xNONE; then
    FPC_PREFIX=$prefix
else
    FPC_PREFIX=$ac_default_prefix
fi

FPC_BASE_PATH="${FPC_PREFIX}/lib/fpc/${FPC_VERSION}"
FPC_UNIT_PATH="${FPC_BASE_PATH}/units/${FPC_TARGET}"

AC_SUBST(FPC_PREFIX)
AC_SUBST(FPC_BASE_PATH)
AC_SUBST(FPC_UNIT_PATH)

###
# Compiler checks
###

SIMPLE_PROGRAM="program foo; begin writeln; end."

# Check if FPC works and can compile a program
AC_CACHE_CHECK([whether the Free Pascal Compiler works], ac_cv_prog_ppc_works,
[
    AC_PROG_FPC_CHECK([ac_cv_prog_ppc_works], [], [$SIMPLE_PROGRAM])
])
if test x$ac_cv_prog_ppc_works = xno; then
    AC_MSG_ERROR([installation or configuration problem: Cannot create executables.])
fi

# Check if FPC can link with standard libraries
AC_CACHE_CHECK([whether the Free Pascal Compiler can link], ac_cv_prog_ppc_links,
[
    AC_PROG_FPC_CHECK([ac_cv_prog_ppc_links], [],
        [program foo; uses crt; begin writeln; end.]
    )
])
if test x$ac_cv_prog_ppc_links = xno; then
    AC_MSG_ERROR([installation or configuration problem: Cannot link with some standard libraries.])
fi

# Check whether FPC's linker knows "-z noexecstack"
# FPC does not set the NX-flag on stack memory. Binaries generated with FPC
# might crash on platforms that require the stack to be non-executable.
# So we will try to find a workaround here.
# See http://bugs.freepascal.org/view.php?id=11563

AC_CACHE_CHECK([whether FPC supports -k"-z noexecstack"], ac_cv_prog_ppc_noexecstack,
[
    AC_PROG_FPC_CHECK([ac_cv_prog_ppc_noexecstack], [-k"-z noexecstack"], [$SIMPLE_PROGRAM])
])
if test x$enable_noexecstack = xyes; then
    if test x$ac_cv_prog_ppc_noexecstack = xyes; then
        PFLAGS_EXTRA="$PFLAGS_EXTRA -k\"-z noexecstack\""
    fi
fi

# Finally substitute PFLAGS

# set unset PFLAGS_XYZ vars to $(PFLAGS_XYZ_DEFAULT)
# so the Makefile can define default values to it.
true ${PFLAGS:=\$(PFLAGS_DEFAULT)}
true ${PFLAGS_BASE:=\$(PFLAGS_BASE_DEFAULT)}
true ${PFLAGS_EXTRA:=\$(PFLAGS_EXTRA_DEFAULT)}
true ${PFLAGS_DEBUG:=\$(PFLAGS_DEBUG_DEFAULT)}
true ${PFLAGS_RELEASE:=\$(PFLAGS_RELEASE_DEFAULT)}

AC_SUBST(PFLAGS)
AC_SUBST(PFLAGS_BASE)
AC_SUBST(PFLAGS_EXTRA)
AC_SUBST(PFLAGS_DEBUG)
AC_SUBST(PFLAGS_RELEASE)

])

#######################################
# Helper functions
#######################################

# SYNOPSIS
# 
#   AC_PROG_FPC_CHECK(RESULT, FPC_FLAGS, CODE)
#
# DESCRIPTION
#
#   Checks if FPC is able to compile CODE with FPC_FLAGS.
#   The result ("yes" on success, "no" otherwise) is
#   stored in [$RESULT]
#
#   Parameters:
#     RESULT:    Name of result variable
#     FPC_FLAGS: Flags passed to FPC
#     CODE:      

AC_DEFUN([AC_PROG_FPC_CHECK],
[
    # create test file
    rm -f conftest*
    echo "[$3]" > conftest.pp

    # compile test file
    ${PPC} [$2] conftest.pp >> config.log 2>&1

    # check if test file was compiled
    if test -f conftest || test -f conftest.exe; then
        [$1]="yes"
    else
        [$1]="no"
    fi

    # remove test file
    rm -f conftest*
])
