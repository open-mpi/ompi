AC_DEFUN([ACVT_OMP],
[
	omp_error="no"
	have_omp="no"

	AC_REQUIRE([ACVT_PLATFORM])

	AC_ARG_VAR(OMPFLAG, [compiler flag to enable OpenMP parallelizer])

	AS_IF([test "$PLATFORM" = "bgl"],
	[
		AC_MSG_CHECKING([for OpenMP flag of C compiler])
		AC_MSG_RESULT([skipped (not supported on BlueGene/L)])
		omp_error="yes"
	])

	AS_IF([test "$omp_error" = "no"],
	[
		AS_IF([test x"$OMPFLAG" != x],
		[
			AC_MSG_CHECKING([for OpenMP flag of C compiler])
			AC_MSG_RESULT([skipped (OMPFLAG=$OMPFLAG)])
		],
		[
			AX_OPENMP
			AS_IF([test x"$OPENMP_CFLAGS" = x],
			[omp_error="yes"], [OMPFLAG=$OPENMP_CFLAGS])
		])
	])

	AS_IF([test "$omp_error" = "no"],
	[
		sav_CFLAGS=$CFLAGS
		CFLAGS="$CFLAGS $OMPFLAG"
		AC_CHECK_HEADER([omp.h], [have_omp="yes"], [omp_error="yes"])
		CFLAGS=$sav_CFLAGS
	])
])

