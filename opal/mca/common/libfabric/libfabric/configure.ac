dnl Process this file with autoconf to produce a configure script.

AC_PREREQ(2.57)
AC_INIT([libfabric], [1.0.0rc5], [ofiwg@lists.openfabrics.org])
AC_CONFIG_SRCDIR([src/fabric.c])
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)
AC_CONFIG_HEADERS(config.h)
AM_INIT_AUTOMAKE([1.11 dist-bzip2 foreign -Wall -Werror subdir-objects parallel-tests])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

AC_CANONICAL_HOST

macos=0
linux=0

case $host_os in
*darwin*)
	macos=1
	;;
*linux*)
	linux=1
	;;
*)
	AC_MSG_ERROR([libfabric only builds on Linux & OS X])
	;;
esac

AM_CONDITIONAL([MACOS], [test "x$macos" = "x1"])
AM_CONDITIONAL([LINUX], [test "x$linux" = "x1"])

AC_ARG_ENABLE([debug],
	      [AS_HELP_STRING([--enable-debug],
			      [Enable debugging @<:@default=no@:>@])
	      ],
	      [CFLAGS="$CFLAGS -g -O0 -Wall"
	       dbg=1],
	      [enable_debug=no
               dbg=0])

AC_DEFINE_UNQUOTED([ENABLE_DEBUG],[$dbg],
                   [defined to 1 if libfabric was configured with --enable-debug, 0 otherwise])

dnl Fix autoconf's habit of adding -g -O2 by default
AS_IF([test -z "$CFLAGS"],
      [CFLAGS='-fvisibility=hidden -O2 -DNDEBUG -Wall'])

# AM PROG_AR did not exist pre AM 1.11.x (where x is somewhere >0 and
# <3), but it is necessary in AM 1.12.x.
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])

AC_ARG_WITH([valgrind],
    AC_HELP_STRING([--with-valgrind],
		   [Enable valgrind annotations @<:@default=no@:>@]))

if test "$with_valgrind" != "" && test "$with_valgrind" != "no"; then
	AC_DEFINE([INCLUDE_VALGRIND], 1,
		  [Define to 1 to enable valgrind annotations])
	if test -d $with_valgrind; then
		CPPFLAGS="$CPPLFAGS -I$with_valgrind/include"
	fi
fi

AC_ARG_ENABLE([direct],
	[AS_HELP_STRING([--enable-direct=@<:@provider@:>@],
		[Enable direct calls to a fabric provider @<:@default=no@:>@])
	],
	[],
	[enable_direct=no])

dnl Checks for programs
AC_PROG_CC_C99

dnl Checks for header files.
AC_HEADER_STDC

LT_INIT

dnl dlopen support is optional
AC_ARG_WITH([dlopen],
	AC_HELP_STRING([--with-dlopen],
		       [dl-loadable provider support @<:@default=yes@:>@]),
	)

AS_IF([test x"$with_dlopen" != x"no"], [
AC_CHECK_LIB(dl, dlopen, [],
    AC_MSG_ERROR([dlopen not found.  libfabric requires libdl.]))
])

dnl Checks for libraries
AC_CHECK_LIB(pthread, pthread_mutex_init, [],
    AC_MSG_ERROR([pthread_mutex_init() not found.  libfabric requires libpthread.]))

AC_CHECK_FUNC([pthread_spin_init],
	[have_spinlock=1],
	[have_spinlock=0])

AC_DEFINE_UNQUOTED([PT_LOCK_SPIN], [$have_spinlock],
	[Define to 1 if pthread_spin_init is available.])

have_clock_gettime=0
have_host_get_clock_service=0

AC_SEARCH_LIBS([clock_gettime],[rt],
		[have_clock_gettime=1],
		[AC_CHECK_FUNCS([host_get_clock_service],
			[have_host_get_clock_service=1],
			[AC_MSG_ERROR([clock_gettime or host_get_clock_service
			 not found.])])])

AC_DEFINE_UNQUOTED(HAVE_CLOCK_GETTIME, [$have_clock_gettime],
		   [Define to 1 if clock_gettime is available.])
AC_DEFINE_UNQUOTED(HAVE_HOST_GET_CLOCK_SERVICE, [$have_host_get_clock_service],
		   [Define to 1 if host_clock_get_service is available.])

dnl Check for gcc atomic intrinsics
AC_MSG_CHECKING(compiler support for c11 atomics)
AC_TRY_LINK([#include <stdatomic.h>],
    [atomic_int a;
     atomic_init(&a, 0);
     #ifdef __STDC_NO_ATOMICS__
       #error c11 atomics are not supported
     #else
       return 0;
     #endif
    ],
    [
	AC_MSG_RESULT(yes)
        AC_DEFINE(HAVE_ATOMICS, 1, [Set to 1 to use c11 atomic functions])
    ],
    [AC_MSG_RESULT(no)])

if test "$with_valgrind" != "" && test "$with_valgrind" != "no"; then
AC_CHECK_HEADER(valgrind/memcheck.h, [],
    AC_MSG_ERROR([valgrind requested but <valgrind/memcheck.h> not found.]))
fi

AC_CACHE_CHECK(whether ld accepts --version-script, ac_cv_version_script,
    if test -n "`$LD --help < /dev/null 2>/dev/null | grep version-script`"; then
        ac_cv_version_script=yes
    else
        ac_cv_version_script=no
    fi)

AM_CONDITIONAL(HAVE_LD_VERSION_SCRIPT, test "$ac_cv_version_script" = "yes")

dnl Disable symbol versioning with icc + ipo, with it ipo is disabled by icc.
dnl The gcc equivalent ipo (-fwhole-program) seems to work fine.
AS_IF([case "$CFLAGS" in
         *-ipo*) true ;;
         *)      false ;;
        esac],
      [AC_MSG_NOTICE([disabling symbol versioning support with -ipo CFLAG])],
[

AC_CACHE_CHECK(for .symver assembler support, ac_cv_asm_symver_support,
	[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]],
		[[asm("symbol:\n.symver symbol, api@ABI\n");]])],
		[ac_cv_asm_symver_support=yes],
		[ac_cv_asm_symver_support=no])])

]) dnl AS_IF

if test x$ac_cv_asm_symver_support = xyes; then
	AC_DEFINE([HAVE_SYMVER_SUPPORT], 1, [assembler has .symver support])
fi

AC_LINK_IFELSE(
	[AC_LANG_SOURCE[
		int foo(int arg);
		int foo(int arg) { return arg + 3; };
		int foo2(int arg) __attribute__ (( __alias__("foo")));
	]],
	[ac_cv_prog_cc_alias_symbols=1],
	[ac_cv_prog_cc_alias_symbols=0])

AC_DEFINE_UNQUOTED([HAVE_ALIAS_ATTRIBUTE], [$ac_cv_prog_cc_alias_symbols],
	  	   [Define to 1 if the linker supports alias attribute.])

dnl Provider-specific checks
FI_PROVIDER_INIT
FI_PROVIDER_SETUP([psm])
FI_PROVIDER_SETUP([sockets])
FI_PROVIDER_SETUP([verbs])
FI_PROVIDER_SETUP([usnic])
FI_PROVIDER_FINI

# If the user requested to build in direct mode, but
# we have more than one provider, error.
AS_IF([test x"$enable_direct" != x"no"],
      [AS_IF([test "$PROVIDERS_COUNT" -eq "1"],
	     [AC_SUBST(PROVIDER_DIRECT, "$enable_direct")],
	     [AC_MSG_NOTICE([Only one provider can be chosen when using --enable-direct])
	     AC_MSG_ERROR(Cannot continue)])])

AM_CONDITIONAL([HAVE_DIRECT], [test x"$enable_direct" != x"no"])

AC_CONFIG_FILES([Makefile libfabric.spec])
AC_OUTPUT

dnl helpful output
if test "$PROVIDERS_TO_BUILD" = ""; then
	echo "***"
	echo "*** No providers were configured. This may not be what you wanted."
	echo "***"
	exit 1
fi

for i in $PROVIDERS_TO_BUILD; do
	v=${i}_dl
	if test `eval echo \\$${v}` == "1"; then
		dso="$i ${dso}"
	else
		builtin="$i ${builtin}"
	fi
done
cat <<EOF
***
*** Built-in providers:	${builtin}
*** DSO providers:	${dso}
***
EOF
