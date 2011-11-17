AC_DEFUN([ACVT_CUDAWRAP],
[
	have_cudawrap="no"
	have_cudartwrap="no"

	cudalib_pathname=
	cudartlib_pathname=

	AC_REQUIRE([ACVT_CUDA])

	AS_IF([test x"$cudart_error" = "xno"],
	[
		AC_ARG_WITH(cudart-shlib,
			AC_HELP_STRING([--with-cudart-shlib=CUDARTSHLIB], [give the pathname for the shared CUDA runtime library, default: automatically by configure]),
		[
			AS_IF([test x"$withval" = "xyes" -o x"$withval" = "xno"],
			[AC_MSG_ERROR([value of '--with-cudart-shlib' not properly set])])
			cudartlib_pathname=$withval
		])
	])

	AS_IF([test x"$cudart_error" = "xno"],
	[
		AC_MSG_CHECKING([for pathname of CUDA runtime library])

		AS_IF([test x"$cudartlib_pathname" != x],
		[
			AC_MSG_RESULT([skipped (--with-cudart-shlib=$cudartlib_pathname)])
		],
		[
			AS_IF([test x"$have_rtld_next" = "xyes"],
			[
				AC_MSG_RESULT([not needed])
			],
			[
				AS_IF([test x"$CUDATKLIBDIR" != x],
				[cudartlib_dir=`echo $CUDATKLIBDIR | sed s/\-L//`])
				cudartlib_pathname=$cudartlib_dir`echo $CUDARTLIB | sed s/\-l/lib/`".so"

				AS_IF([! test -f $cudartlib_pathname],
				[
					AC_MSG_RESULT([unknown])
					AC_MSG_NOTICE([error: could not determine pathname of CUDA runtime library])
					cudart_error="yes"
				],
				[
					AC_MSG_RESULT([$cudartlib_pathname])
				])
			])
		])
	])

	AS_IF([test x"$cudart_error" = "xno"],
	[
		AS_IF([test x"$cudartlib_pathname" != x],
		[
			AC_DEFINE_UNQUOTED([DEFAULT_CUDARTLIB_PATHNAME],
			["$cudartlib_pathname"], [pathname of CUDA runtime library])
		])
		have_cudartwrap="yes"
	])


	AS_IF([test x"$cuda_error" = "xno"],
	[
		AC_ARG_WITH(cuda-shlib,
			AC_HELP_STRING([--with-cuda-shlib=CUDASHLIB], [give the pathname for the shared CUDA driver library, default: automatically by configure]),
		[
			AS_IF([test x"$withval" = "xyes" -o x"$withval" = "xno"],
			[AC_MSG_ERROR([value of '--with-cuda-shlib' not properly set])])
			cudalib_pathname=$withval
		])
	])

	AS_IF([test x"$cuda_error" = "xno"],
	[
		AC_MSG_CHECKING([for pathname of CUDA driver library])

		AS_IF([test x"$cudalib_pathname" != x],
		[
			AC_MSG_RESULT([skipped (--with-cuda-shlib=$cudalib_pathname)])
		],
		[
			AS_IF([test x"$have_rtld_next" = "xyes"],
			[
				AC_MSG_RESULT([not needed])
			],
			[
				AS_IF([test x"$CUDATKLIBDIR" != x],
				[cudalib_dir=`echo $CUDATKLIBDIR | sed s/\-L//`])
				cudalib_pathname=$cudalib_dir`echo $CUDALIB | sed s/\-l/lib/`".so"

				AS_IF([! test -f $cudalib_pathname],
				[
					AC_MSG_RESULT([unknown])
					AC_MSG_NOTICE([error: could not determine pathname of CUDA driver library])
					cuda_error="yes"
				],
				[
					AC_MSG_RESULT([$cudalib_pathname])
				])
			])
		])
	])

	AS_IF([test x"$cuda_error" = "xno"],
	[
		AS_IF([test x"$cudalib_pathname" != x],
		[
			AC_DEFINE_UNQUOTED([DEFAULT_CUDALIB_PATHNAME],
			["$cudalib_pathname"], [pathname of CUDA driver library])
		])
		have_cudawrap="yes"
	])

])
