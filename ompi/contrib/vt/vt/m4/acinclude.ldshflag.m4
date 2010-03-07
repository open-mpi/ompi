AC_DEFUN([ACVT_LDSHFLAG],
[
	ldshflag_error="no"

	AC_REQUIRE([ACVT_PLATFORM])

	AC_MSG_CHECKING([for linker flag to create shared libraries])

	AC_ARG_VAR(LDSHFLAG, [linker flag to create shared libraries])

	AS_IF([test x"$LDSHFLAG" != x],
	[
		AC_MSG_RESULT([skipped (LDSHFLAG=$LDSHFLAG)])
	],
	[
		LDSHFLAG=

		ldshflag=
		base_CC=`basename $CC`
		case $base_CC in
			gcc* | scgcc* | icc* | pgcc* | pathcc* | scpathcc* | suncc*)
				ldshflag="-shared"
				;;
			xlc* | blrts_xlc*)
				ldshflag="-G"
				;;
			cc*)
				AS_IF([test x"$PLATFORM" = "xmips"],
				[ldshflag="-shared"])
				;;
		esac

		AS_IF([test x"$ldshflag" != x],
		[AC_MSG_RESULT([$ldshflag])], [AC_MSG_RESULT([unknown])])

		LDSHFLAG=$ldshflag
	])

	AS_IF([test x"$LDSHFLAG" = x], [ldshflag_error="yes"])
])

