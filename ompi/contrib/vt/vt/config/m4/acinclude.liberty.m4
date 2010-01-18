AC_DEFUN([ACVT_LIBERTY],
[
	liberty_error="no"
	have_liberty="no"

	LIBERTYDIR=
	LIBERTYINCDIR=
	LIBERTYLIBDIR=
	LIBERTYLIB=

	AC_ARG_WITH(liberty-dir,
		AC_HELP_STRING([--with-liberty-dir=LIBERTYDIR], [give the path for LIBERTY, default: /usr]),
	[LIBERTYDIR="$withval/"])

	AC_ARG_WITH(liberty-inc-dir,
		AC_HELP_STRING([--with-liberty-inc-dir=LIBERTYINCDIR],
		[give the path for LIBERTY-include files, default: LIBERTYDIR/include]),
	[LIBERTYINCDIR="-I$withval/"],
	[AS_IF([test x"$LIBERTYDIR" != x], [LIBERTYINCDIR="-I$LIBERTYDIR"include/])])

	AC_ARG_WITH(liberty-lib-dir,
		AC_HELP_STRING([--with-liberty-lib-dir=LIBERTYLIBDIR],
		[give the path for LIBERTY-libraries, default: LIBERTYDIR/lib]),
	[LIBERTYLIBDIR="-L$withval/"],
	[AS_IF([test x"$LIBERTYDIR" != x], [LIBERTYLIBDIR="-L$LIBERTYDIR"lib/])])

	AC_ARG_WITH(liberty-lib,
		AC_HELP_STRING([--with-liberty-lib=LIBERTYLIB], [use given liberty lib, default: -liberty]),
	[LIBERTYLIB="$withval"])

	sav_CPPFLAGS=$CPPFLAGS
	CPPFLAGS="$CPPFLAGS $LIBERTYINCDIR"
	AC_CHECK_HEADER([libiberty.h], [],
	[
		AC_MSG_NOTICE([error: no libiberty.h found; check path for LIBERTY package first...])
		liberty_error="yes"
	])
	CPPFLAGS=$sav_CPPFLAGS

	AS_IF([test x"$LIBERTYLIB" = x -a x"$liberty_error" = "xno"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $LIBERTYLIBDIR -liberty"
		AC_MSG_CHECKING([whether linking with -liberty works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); LIBERTYLIB=-liberty],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$LIBERTYLIB" = x -a x"$liberty_error" = "xno"],
	[
		AC_MSG_NOTICE([error: no libiberty found; check path for LIBERTY package first...])
		liberty_error="yes"
	])

	AS_IF([test x"$LIBERTYLIB" != x -a x"$liberty_error" = "xno"],
	[have_liberty="yes"])

	AC_SUBST(LIBERTYINCDIR)
	AC_SUBST(LIBERTYLIBDIR)
	AC_SUBST(LIBERTYLIB)
])

