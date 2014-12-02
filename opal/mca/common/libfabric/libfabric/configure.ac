dnl Process this file with autoconf to produce a configure script.

AC_PREREQ(2.57)
AC_INIT([libfabric], [0.0.2], [linux-rdma@vger.kernel.org])
AC_CONFIG_SRCDIR([src/fabric.c])
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)
AC_CONFIG_HEADERS(config.h)
AM_INIT_AUTOMAKE([1.11 dist-bzip2 foreign -Wall -Werror subdir-objects parallel-tests])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

AC_ARG_ENABLE([debug],
	      [AS_HELP_STRING([--enable-debug],
			      [Enable debugging @<:@default=no@:>@])
	      ],
	      [CFLAGS="$CFLAGS -g -O0 -Wall"],
	      [enable_debug=no])

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
AC_PROG_CC

dnl Checks for header files.
AC_HEADER_STDC

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_CHECK_SIZEOF(long)

dnl Only build on Linux
AC_CHECK_HEADER([linux/types.h], [],
	[AC_MSG_ERROR([libfabric only builds on Linux])])

LT_INIT

AC_CHECK_HEADERS([fcntl.h sys/socket.h])
AC_CHECK_DECLS([O_CLOEXEC],,[AC_DEFINE([O_CLOEXEC],[0],
	[Defined to 0 if not provided])],
	[[
		#ifdef HAVE_FCNTL_H
		#include <fcntl.h>
		#endif
	]])

AC_CHECK_DECLS([SOCK_CLOEXEC],,[AC_DEFINE([SOCK_CLOEXEC],[0],
	[Defined to 0 if not provided])],
	[[
		#ifdef HAVE_SYS_SOCKET_H
		#include <sys/socket.h>
		#endif
	]])

AC_CACHE_CHECK(for close on exec modifier for fopen(),
	ac_cv_feature_stream_cloexec_flag,
	[if test $ac_cv_have_decl_O_CLOEXEC = yes ; then
		if test $ac_cv_have_decl_SOCK_CLOEXEC = yes ; then
			ac_cv_feature_stream_cloexec_flag="e"
		fi
	fi])
AC_DEFINE_UNQUOTED([STREAM_CLOEXEC], "$ac_cv_feature_stream_cloexec_flag",
	[fopen() modifier for setting close on exec flag])

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
AC_CHECK_LIB(rt, clock_gettime, [],
    AC_MSG_ERROR([clock_gettime() not found.  libfabric requires librt.]))

dnl Check for gcc atomic intrinsics
AC_MSG_CHECKING(compiler support for c11 atomics)
AC_TRY_LINK([#include <stdatomic.h>],
    [#ifdef __STDC_NO_ATOMICS__
       return 1;
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

AC_CACHE_CHECK(for .symver assembler support, ac_cv_asm_symver_support,
	[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]],
		[[asm("symbol:\n.symver symbol, api@ABI\n");]])],
		[ac_cv_asm_symver_support=yes],
		[ac_cv_asm_symver_support=no])])

if test $ac_cv_asm_symver_support = yes; then
	AC_DEFINE([HAVE_SYMVER_SUPPORT], 1, [assembler has .symver support])
fi

dnl Provider-specific checks
FI_PROVIDER_INIT
FI_PROVIDER_SETUP([psm])
FI_PROVIDER_SETUP([sockets])
FI_PROVIDER_SETUP([verbs])
FI_PROVIDER_SETUP([usnic])
FI_PROVIDER_FINI

# Make sure at least one provider was setup
AS_IF([test x"$PROVIDERS_TO_BUILD" = "x"],
      [AC_MSG_NOTICE([No providers were configured])
       AC_MSG_ERROR([Cannot continue])])

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
