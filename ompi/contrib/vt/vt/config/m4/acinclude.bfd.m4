AC_DEFUN([ACVT_BFD],
[
	bfd_error="no"
	check_bfd="yes"
        force_bfd="no"
	have_bfd="no"

	BFDDIR=
	BFDINCDIR=
	BFDLIBDIR=
	BFDLIB=

	AC_ARG_WITH(bfd,
		AC_HELP_STRING([--with-bfd], [use BFD to get symbol information of an executable instrumented with GNU, Intel, or Pathscale compiler, default: yes]),
	[AS_IF([test x"$withval" = "xyes"], [force_bfd="yes"], [check_bfd="no"])])

	AC_ARG_WITH(bfd-dir,
		AC_HELP_STRING([--with-bfd-dir=BFDDIR], [give the path for BFD, default: /usr]),
	[BFDDIR="$withval/"])

	AC_ARG_WITH(bfd-inc-dir,
		AC_HELP_STRING([--with-bfd-inc-dir=BFDINCDIR],
		[give the path for BFD-include files, default: BFDDIR/include]),
	[BFDINCDIR="-I$withval/"],
	[AS_IF([test x"$BFDDIR" != x], [BFDINCDIR="-I$BFDDIR"include/])])

	AC_ARG_WITH(bfd-lib-dir,
		AC_HELP_STRING([--with-bfd-lib-dir=BFDLIBDIR],
		[give the path for BFD-libraries, default: BFDDIR/lib]),
	[BFDLIBDIR="-L$withval/"],
	[AS_IF([test x"$BFDDIR" != x], [BFDLIBDIR="-L$BFDDIR"lib/])])

	AC_ARG_WITH(bfd-lib,
		AC_HELP_STRING([--with-bfd-lib=BFDLIB], [use given bfd lib, default: -lbfd]),
	[BFDLIB="$withval"])

	AS_IF([test x"$check_bfd" = "xyes"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $BFDINCDIR"
		AC_CHECK_HEADER([bfd.h], [],
		[
			AC_MSG_NOTICE([error: no bfd.h found; check path for BFD package first...])
			bfd_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS

		AS_IF([test x"$BFDLIB" = x -a "$bfd_error" = "no"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $BFDLIBDIR -lbfd"
			AC_MSG_CHECKING([whether linking with -lbfd works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); BFDLIB=-lbfd],[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AS_IF([test x"$BFDLIB" = x -a "$bfd_error" = "no"],
		[
			AC_MSG_NOTICE([error: no libbfd found; check path for BFD package first...])
			bfd_error="yes"
		])

		AS_IF([test "$bfd_error" = "no"], [have_bfd="yes"])
	])

	AC_SUBST(BFDINCDIR)
	AC_SUBST(BFDLIBDIR)
	AC_SUBST(BFDLIB)
])

