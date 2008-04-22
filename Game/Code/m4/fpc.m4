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

AC_ARG_VAR(PFLAGS, [Free Pascal Compiler flags (replaces all other flags)])
AC_ARG_VAR(PFLAGS_DEBUG, [Free Pascal Compiler debug flags @<:@-gl -Coi -Xs- -vew -dDEBUG_MODE@:>@])
AC_ARG_VAR(PFLAGS_RELEASE, [Free Pascal Compiler release flags @<:@-O2 -Xs -vew@:>@])
AC_ARG_VAR(PFLAGS_EXTRA, [Free Pascal Compiler additional flags])

dnl set DEBUG/RELEASE flags to default-values if unset

dnl - Do not use -dDEBUG because this will enable range-checks that will fail with USDX.
dnl - Disable -Xs which is defined in fpc.cfg (TODO: is this necessary?).
dnl - For FPC we have to use DEBUG_MODE instead of DEBUG to enable the apps debug-mode 
dnl   because DEBUG enables some additional compiler-flags in fpc.cfg too
PFLAGS_DEBUG=${PFLAGS_DEBUG-"-gl -Cit -Xs- -vew -dDEBUG_MODE"}
dnl -dRELEASE works too but we define our own settings
PFLAGS_RELEASE=${PFLAGS_RELEASE-"-O2 -Xs -vew"}


AC_ARG_ENABLE(dummy_fpc1,[
Free Pascal Compiler specific options:])

AC_ARG_WITH(fpc,
  [AS_HELP_STRING([--with-fpc=DIR],
    [Directory of the FPC executable @<:@PATH@:>@])],
  [PPC_PATH=$withval], [])

FPC_DEBUG="no"

AC_ARG_ENABLE(release,
  [AS_HELP_STRING([--enable-release],
    [Enable FPC release options @<:@default=yes@:>@])],
  [test $enableval = "no" && FPC_DEBUG="yes"], [])

AC_ARG_ENABLE(debug,
  [AS_HELP_STRING([--enable-debug],
    [Enable FPC debug options (= --disable-release) @<:@default=no@:>@])],
  [test $enableval = "yes" && FPC_DEBUG="yes"], [])

AC_ARG_ENABLE(profile,
  [AS_HELP_STRING([--enable-profile],
    [Enable FPC profiling options @<:@default=no@:>@])],
  [PFLAGS_EXTRA="$PFLAGS_EXTRA -pg"], [])


dnl ** set PFLAGS depending on whether it is already set by the user
dnl Note: the user's PFLAGS must *follow* this script's flags
dnl   to enable the user to overwrite the settings.
if test x${PFLAGS+assigned} = x; then
dnl PFLAGS not set by the user
	if test x$FPC_DEBUG = xyes; then 
		PFLAGS="$PFLAGS_DEBUG"
		PFLAGS_MAKE="[\$](PFLAGS_DEBUG)"
	else
		PFLAGS="$PFLAGS_RELEASE"
		PFLAGS_MAKE="[\$](PFLAGS_RELEASE)"
	fi
else
dnl PFLAGS set by the user, just add additional flags
	PFLAGS="$PFLAGS"
	PFLAGS_MAKE="$PFLAGS"
fi

dnl ** find compiler executable

PPC_CHECK_PROGS="fpc FPC ppc386 ppc PPC386 ppos2"

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
if test -z "$FPCMAKE"; then
	AC_MSG_ERROR([fpcmake not found in $PPC_PATH])
fi

AC_PROG_FPC_WORKS
AC_PROG_FPC_LINKS

dnl *** Get the FPC version and some paths
FPC_VERSION=`${PPC} -iV`
FPC_PLATFORM=`${PPC} -iTO`
FPC_PROCESSOR=`${PPC} -iTP`

if test "x$prefix" != xNONE; then
	FPC_PREFIX=$prefix
else
	FPC_PREFIX=$ac_default_prefix
fi
FPC_BASE_PATH="${FPC_PREFIX}/lib/fpc/${FPC_VERSION}"
FPC_UNIT_PATH="${FPC_BASE_PATH}/units/${FPC_PLATFORM}"

AC_SUBST(PFLAGS)
AC_SUBST(PFLAGS_MAKE)
AC_SUBST(PFLAGS_EXTRA)
AC_SUBST(PFLAGS_DEBUG)
AC_SUBST(PFLAGS_RELEASE)

AC_SUBST(FPC_VERSION)
AC_SUBST(FPC_PLATFORM)
AC_SUBST(FPC_PROCESSOR)

AC_SUBST(FPC_PREFIX)
AC_SUBST(FPC_BASE_PATH)
AC_SUBST(FPC_UNIT_PATH)
])

PFLAGS_TEST="$PFLAGS $PFLAGS_EXTRA"

dnl ***
dnl *** Check if FPC works and can compile a program
dnl ***
AC_DEFUN([AC_PROG_FPC_WORKS],
[AC_CACHE_CHECK([whether the Free Pascal Compiler ($PPC $PFLAGS_TEST) works], ac_cv_prog_ppc_works,
[
rm -f conftest*
echo "program foo; begin writeln; end." > conftest.pp
${PPC} ${PFLAGS_TEST} conftest.pp >> config.log

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
[AC_CACHE_CHECK([whether the Free Pascal Compiler ($PPC $PFLAGS_TEST) can link], ac_cv_prog_ppc_works,
[
rm -f conftest*
echo "program foo; uses crt; begin writeln; end." > conftest.pp
${PPC} ${PFLAGS_TEST} conftest.pp >> config.log
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

