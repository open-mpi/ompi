AC_DEFUN([ACVT_DYNINST],
[
	dyninst_error="no"
	dynattlib_error="no"
	check_dyninst="yes"
	force_dyninst="no"
	check_dynattlib="yes"
	force_dynattlib="no"
	build_dynattlib="no"
	have_dyninst="no"

	DYNIDIR=
	DYNIINCDIR=
	DYNILIBDIR=
	DYNILIB=
	VTDYNATTLIB=

	AC_ARG_ENABLE(dyninst,
		AC_HELP_STRING([--enable-dyninst],
		[enable support for binary instrumentation by using Dyninst, default: enable if found by configure]),
	[AS_IF([test x"$enableval" = "xyes"], [force_dyninst="yes"], [check_dyninst="no"])])

	AC_ARG_ENABLE(dyninst-attlib,
		AC_HELP_STRING([--enable-dyninst-attlib],
		[build shared library which attach Dyninst to running user's application, default: enable if Dyninst found by configure and system supports shared libraries]),
	[AS_IF([test x"$enableval" = "xyes"], [force_dyninst="yes"; check_dyninst="yes"; force_dynattlib="yes"; build_dynattlib="yes"], [check_dynattlib="no"])])

	AC_ARG_WITH(dyninst-dir,
		AC_HELP_STRING([--with-dyninst-dir=DYNIDIR], [give the path for Dyninst, default: /usr]),
	[DYNIDIR="$withval/"])

	AC_ARG_WITH(dyninst-inc-dir,
		AC_HELP_STRING([--with-dyninst-inc-dir=DYNIINCDIR],
		[give the path for Dyninst-include files, default: DYNIDIR/include]),
	[DYNIINCDIR="-I$withval/"],
	[AS_IF([test x"$DYNIDIR" != x], [DYNIINCDIR="-I$DYNIDIR"include/])])

	AC_ARG_WITH(dyninst-lib-dir,
		AC_HELP_STRING([--with-dyninst-lib-dir=DYNILIBDIR],
		[give the path for Dyninst-libraries, default: DYNIDIR/lib]),
	[DYNILIBDIR="-L$withval/"],
	[AS_IF([test x"$DYNIDIR" != x], [DYNILIBDIR="-L$DYNIDIR"lib/])])

	AC_ARG_WITH(dyninst-lib,
		AC_HELP_STRING([--with-dyninst-lib=DYNILIB], [use given Dyninst lib, default: -ldyninstAPI]),
	[DYNILIB="$withval"])

	AS_IF([test "$check_dyninst" = "yes"],
	[
		AC_LANG([C++])

		AS_IF([test x"$dyninst_error" = "xno"],
		[
			sav_CPPFLAGS=$CPPFLAGS
			CPPFLAGS="$CPPFLAGS $DYNIINCDIR"
			AC_CHECK_HEADER([BPatch.h], [],
			[
				AC_MSG_NOTICE([error: no BPatch.h found; check path for Dyninst package first...])
				dyninst_error="yes"
			])
			CPPFLAGS=$sav_CPPFLAGS
		])

		AS_IF([test x"$DYNILIB" = x -a x"$dyninst_error" = "xno"],
		[
			sav_LIBS=$LIBS
			LIBS="$LIBS $DYNILIBDIR -ldyninstAPI"
			AC_MSG_CHECKING([whether linking with -ldyninstAPI works])
			AC_TRY_LINK([],[],
			[AC_MSG_RESULT([yes]); DYNILIB="-ldyninstAPI"],
			[AC_MSG_RESULT([no])])
			LIBS=$sav_LIBS
		])

		AC_LANG([C])

		AS_IF([test x"$DYNILIB" = x -a x"$dyninst_error" = "xno"],
		[
			AC_MSG_NOTICE([error: no libdyninstAPI found; check path for Dyninst package first...])
			dyninst_error="yes"
		])

		AS_IF([test x"$DYNILIB" != x -a x"$dyninst_error" = "xno"],
		[
			have_dyninst="yes"
			AS_IF([test x"$check_dynattlib" = "xyes"],
			[
				ACVT_LDSHFLAG
				AS_IF([test x"$ldshflag_error" = "xno"],
				[
					build_dynattlib="yes"
					VTDYNATTLIB="-lvt.dynatt"
				],
				[
					AC_MSG_NOTICE([error: could not determine linker flag to create shared libraries!])
					dynattlib_error="yes"
				])
			])
		])
	])

	AC_SUBST(DYNIINCDIR)
	AC_SUBST(DYNILIBDIR)
	AC_SUBST(DYNILIB)
	AC_SUBST(VTDYNATTLIB)
])

