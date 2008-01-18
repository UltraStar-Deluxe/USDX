dnl ** Version 1.1 of file is part of the LGPLed 
dnl **   J Sound System (http://jss.sourceforge.net)
dnl **
dnl ** Checks for Free Pascal Compiler by Matti "ccr/TNSP" Hamalainen
dnl ** (C) Copyright 2000-2001 Tecnic Software productions (TNSP)
dnl **
dnl ** Versions
dnl ** --------
dnl ** 1.0 - Created
dnl **
dnl ** 1.1 - Added stuff to enable unix -> win32
dnl **       cross compilation.
dnl **
dnl ** 1.x - A few fixes (by the UltraStar Deluxe Team)
dnl **

AC_DEFUN([AC_PROG_FPC], [

AC_ARG_VAR(PFLAGS, [Free Pascal Compiler flags])

AC_ARG_ENABLE(dummy_fpc1,[
Free Pascal Compiler specific options:])

AC_ARG_WITH(fpc,
  [AS_HELP_STRING([--with-fpc],
    [Directory of the FPC executable @<:@PATH@:>@])],
  [PPC_PATH=$withval], [])

FPC_DEBUG="no"

AC_ARG_ENABLE(debug,
  [AS_HELP_STRING([--enable-debug],
    [Enable FPC debug options @<:@default=no@:>@])],
  [FPC_DEBUG="yes"], [])


AC_ARG_ENABLE(release,
  [AS_HELP_STRING([--enable-release],
    [Enable FPC release options (same as --enable-debug=no)])],
  [FPC_DEBUG="no"], [])

dnl use -dDEBUG (instead of -g) so it uses the fpc.cfg defaults
AC_ARG_WITH(debug-flags,
  [AS_HELP_STRING([--with-debug-flags],
    [FPC debug flags @<:@-dDEBUG@:>@])],
  [fpc_debugflags="$withval"], 
  [fpc_debugflags="-dDEBUG"])

dnl use -dDEBUG (instead of e.g. -O2) so it uses the fpc.cfg defaults
AC_ARG_WITH(release-flags,
  [AS_HELP_STRING([--with-release-flags],
    [FPC release flags @<:@-dRELEASE@:>@])],
  [fpc_releaseflags="$withval"], 
  [fpc_releaseflags="-dRELEASE"])

if test x$FPC_DEBUG = xyes; then 
	PFLAGS="$PFLAGS $fpc_debugflags"
else
	PFLAGS="$PFLAGS $fpc_releaseflags"
fi

AC_ARG_ENABLE(profile,
  [AS_HELP_STRING([--enable-profile],
    [Enable FPC profiling options])],
  [PFLAGS="$PFLAGS -pg"], [])

PPC_CHECK_PROGS="fpc FPC ppc386 ppc PPC386 ppos2"

if test -z "$PPC_PATH"; then
	PPC_PATH=$PATH
	AC_CHECK_PROGS(PPC, $PPC_CHECK_PROGS)
else
	AC_PATH_PROGS(PPC, $PPC_CHECK_PROGS, [], $PPC_PATH)
fi
if test -z "$PPC"; then
	AC_MSG_ERROR([no Free Pascal Compiler found in $PPC_PATH])
fi

AC_PROG_FPC_WORKS
AC_PROG_FPC_LINKS

dnl *** Get the FPC version and some paths
FPC_VERSION=`${PPC} ${PFLAGS} -iV`
FPC_PLATFORM=`${PPC} ${PFLAGS} -iTO`
FPC_PROCESSOR=`${PPC} ${PFLAGS} -iTP`
if test "x$prefix" != xNONE; then
	FPC_PREFIX=$prefix
else
	FPC_PREFIX=$ac_default_prefix
fi
FPC_BASE_PATH="${FPC_PREFIX}/lib/fpc/${FPC_VERSION}"
FPC_UNIT_PATH="${FPC_BASE_PATH}/units/${FPC_PLATFORM}"
AC_SUBST(PFLAGS)
AC_SUBST(FPC_VERSION)
AC_SUBST(FPC_PLATFORM)
AC_SUBST(FPC_PROCESSOR)
AC_SUBST(FPC_PREFIX)
AC_SUBST(FPC_BASE_PATH)
AC_SUBST(FPC_UNIT_PATH)
])


dnl ***
dnl *** Check if FPC works and can compile a program
dnl ***
AC_DEFUN([AC_PROG_FPC_WORKS],
[AC_CACHE_CHECK([whether the Free Pascal Compiler ($PPC $PFLAGS) works], ac_cv_prog_ppc_works,
[
rm -f conftest*
echo "program foo; begin writeln; end." > conftest.pp
${PPC} ${PFLAGS} conftest.pp >> config.log

if test -f conftest || test -f conftest.exe; then
dnl *** It works!
	ac_cv_prog_ppc_works="yes"

else
	ac_cv_prog_ppc_works="no"
fi
rm -f conftest*
dnl AC_MSG_RESULT($ac_cv_prog_ppc_works)
if test x$ac_cv_prog_ppc_works = xno; then
	AC_MSG_ERROR([installation or configuration problem: Cannot create executables.])
fi
])])


dnl ***
dnl *** Check if FPC can link with standard libraries
dnl ***
AC_DEFUN([AC_PROG_FPC_LINKS],
[AC_CACHE_CHECK([whether the Free Pascal Compiler ($PPC $PFLAGS) can link], ac_cv_prog_ppc_works,
[
rm -f conftest*
echo "program foo; uses crt; begin writeln; end." > conftest.pp
${PPC} ${PFLAGS} conftest.pp >> config.log
if test -f conftest || test -f conftest.exe; then
	ac_cv_prog_ppc_links="yes"
else
	ac_cv_prog_ppc_links="no"
fi
rm -f conftest*
AC_MSG_RESULT($ac_cv_prog_ppc_links)
if test x$ac_cv_prog_ppc_links = xno; then
	AC_MSG_ERROR([installation or configuration problem: Cannot link with some standard libraries.])
fi
])])

