AC_DEFUN([ACVT_OMP],
[
	omp_error="no"
	build_opari="no"
	have_omp="no"

	VTPOMPLIB=

	AC_ARG_VAR(OPENMP_CFLAGS, [C compiler flags to enable support for OpenMP])

	AX_OPENMP([], [omp_error="yes"])

	AS_IF([test x"$omp_error" = "xno"],
	[
		sav_CFLAGS=$CFLAGS
		CFLAGS="$CFLAGS $OPENMP_CFLAGS"
		AC_CHECK_HEADER([omp.h], [], [omp_error="yes"])
		CFLAGS=$sav_CFLAGS
	])

	AS_IF([test x"$omp_error" = "xno"],
	[
		VTPOMPLIB="-lvt-pomp"
		build_opari="yes"
		have_omp="yes"
		AC_DEFINE([HAVE_OMP], [1], [Define to 1 if VT is configured with OpenMP support.])
	])

	AC_SUBST(VTPOMPLIB)

	# Extract version from Open64, do not use -dumpversion, as this does not
	# uniquely identify the Open64 compiler
	opencc=`$CC -E -dM -x c /dev/null 2>/dev/null | sed -n -e 's/#define __OPENCC__ // p'`
	AS_IF([test x"$opencc" != x"" ],
	[
		opencc_minor=`$CC -E -dM -x c /dev/null | sed -n -e 's/#define __OPENCC_MINOR__ // p'`
		opencc_patchlevel=`$CC -E -dM -x c /dev/null | sed -n -e 's/#define __OPENCC_PATCHLEVEL__ // p'`
		opencc_subpatchlevel=0
		AS_CASE([$opencc_patchlevel],
		[""],    [opencc_patchlevel=0],
		[*.*],
		[
			opencc_subpatchlevel=${opencc_patchlevel##*.}
			opencc_patchlevel=${opencc_patchlevel%.*}
		])
		opencc_version=`expr $opencc "*" 1000 + $opencc_minor "*" 100 + $opencc_patchlevel "*" 10 + $opencc_subpatchlevel`
		AC_DEFINE_UNQUOTED([VT_OPENCC_VERSION], [$opencc_version], [Version of Open64 compiler as number.])
		AS_UNSET([opencc_minor])
		AS_UNSET([opencc_patchlevel])
		AS_UNSET([opencc_subpatchlevel])
		AS_UNSET([opencc_version])
	])
	AS_UNSET([opencc])
])

