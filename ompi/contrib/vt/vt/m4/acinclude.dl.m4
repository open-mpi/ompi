AC_DEFUN([ACVT_DL],
[
	dl_error="no"
	have_dl="no"

	have_rtld_next="no"

	DLDIR=
	DLINCDIR=
	DLLIBDIR=
	DLLIB=

	AC_ARG_WITH(dl-dir,
		AC_HELP_STRING([--with-dl-dir=DLDIR], [give the path for libdl, default: /usr]),
	[DLDIR="$withval/"])

	AC_ARG_WITH(dl-inc-dir,
		AC_HELP_STRING([--with-dl-inc-dir=DLINCDIR],
		[give the path for libdl-include files, default: DLDIR/include]),
	[DLINCDIR="-I$withval/"],
	[AS_IF([test x"$DLDIR" != x], [DLINCDIR="-I$DLDIR"include/])])

	AC_ARG_WITH(dl-lib-dir,
		AC_HELP_STRING([--with-dl-lib-dir=DLLIBDIR],
		[give the path for libdl-libraries, default: DLDIR/lib]),
	[DLLIBDIR="-L$withval/"],
	[AS_IF([test x"$DLDIR" != x], [DLLIBDIR="-L$DLDIR"lib/])])

	AC_ARG_WITH(dl-lib,
		AC_HELP_STRING([--with-dl-lib=DLLIB], [use given libdl lib, default: -ldl]),
	[DLLIB="$withval"])

	sav_CPPFLAGS=$CPPFLAGS
	CPPFLAGS="$CPPFLAGS $DLINCDIR"
	AC_CHECK_HEADER([dlfcn.h], [],
	[
		AC_MSG_NOTICE([error: no dlfcn.h found; check path for libdl package first...])
		dl_error="yes"
	])
	CPPFLAGS=$sav_CPPFLAGS

	AS_IF([test x"$DLLIB" = x -a x"$dl_error" = "xno"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $DLLIBDIR -ldl"
		AC_MSG_CHECKING([whether linking with -ldl works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); DLLIB=-ldl],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$DLLIB" = x -a x"$dl_error" = "xno"],
	[
		AC_MSG_NOTICE([error: no libdl found; check path for libdl package first...])
		dl_error="yes"
	])

	AS_IF([test x"$dl_error" = "xno"],
	[
		AC_CHECK_DECL([RTLD_NEXT], [have_rtld_next="yes"], [], [#include <dlfcn.h>])

		AS_IF([test x"$have_rtld_next" = "xno"],
		[
			AC_MSG_CHECKING([whether we need to define _GNU_SOURCE to get RTLD_NEXT])
			AC_TRY_COMPILE(
			[
#define _GNU_SOURCE
#include <dlfcn.h>
			],
			[
#ifndef RTLD_NEXT
  (void) RTLD_NEXT;
#endif
			],
			[
				AC_MSG_RESULT([yes])
				have_rtld_next="yes"
				CPPFLAGS="$CPPFLAGS -D_GNU_SOURCE"
			],
			[
				AC_MSG_RESULT([no])
			])
		])
	])

	AS_IF([test x"$DLLIB" != x -a x"$dl_error" = "xno"], [have_dl="yes"])

	AC_SUBST(DLINCDIR)
	AC_SUBST(DLLIBDIR)
	AC_SUBST(DLLIB)
])

