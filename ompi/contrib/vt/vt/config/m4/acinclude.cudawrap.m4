AC_DEFUN([ACVT_CUDAWRAP],
[
	cudawrap_error="no"
	cudartwrap_error="no"
	have_cudawrap="no"
	have_cudartwrap="no"

	CUDATKDIR=
	CUDATKINCDIR=
	CUDATKLIBDIR=
	CUDALIB=
	CUDARTLIB=

	cudalib_pathname=
	cudartlib_pathname=

	AC_ARG_VAR(NVCC, [NVIDIA CUDA compiler command])

	AC_ARG_WITH(cuda-dir,
		AC_HELP_STRING([--with-cuda-dir=CUDATKDIR],
		[give the path for CUDA Toolkit, default: /usr/local/cuda]),
	[CUDATKDIR="$withval/"], [CUDATKDIR="/usr/local/cuda/"])

	AC_ARG_WITH(cuda-inc-dir,
		AC_HELP_STRING([--with-cuda-inc-dir=CUDATKINCDIR],
		[give the path for CUDA-Toolkit-include files, default: CUDATKDIR/include]),
	[CUDATKINCDIR="-I$withval/"],
	[AS_IF([test x"$CUDATKDIR" != x], [CUDATKINCDIR="-I$CUDATKDIR"include/])])

	AC_ARG_WITH(cuda-lib-dir,
		AC_HELP_STRING([--with-cuda-lib-dir=CUDATKLIBDIR],
		[give the path for CUDA-Toolkit-libraries, default: CUDATKDIR/lib64]),
	[CUDATKLIBDIR="-L$withval/"],
	[AS_IF([test x"$CUDATKDIR" != x], [CUDATKLIBDIR="-L$CUDATKDIR"lib64/])])

	AC_ARG_WITH(cuda-lib,
		AC_HELP_STRING([--with-cuda-lib=CUDALIB], [use given CUDA driver library, default: -lcuda]),
	[CUDALIB="$withval"])

	AC_ARG_WITH(cudart-lib,
		AC_HELP_STRING([--with-cudart-lib=CUDARTLIB], [use given CUDA runtime library, default: -lcudart]),
	[CUDARTLIB="$withval"])

	AC_ARG_WITH(cuda-shlib,
		AC_HELP_STRING([--with-cuda-shlib=CUDASHLIB], [give the pathname for the shared CUDA driver library, default: automatically by configure]),
	[
		AS_IF([test x"$withval" = "xyes" -o x"$withval" = "xno"],
		[AC_MSG_ERROR([value of '--with-cuda-shlib' not properly set])])
		cudalib_pathname=$withval
	])

	AC_ARG_WITH(cudart-shlib,
		AC_HELP_STRING([--with-cudart-shlib=CUDARTSHLIB], [give the pathname for the shared CUDA runtime library, default: automatically by configure]),
	[
		AS_IF([test x"$withval" = "xyes" -o x"$withval" = "xno"],
		[AC_MSG_ERROR([value of '--with-cudart-shlib' not properly set])])
                cudartlib_pathname=$withval
	])

	AS_IF([test x"$cudawrap_error" = "xno"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $CUDATKINCDIR"
		AC_CHECK_HEADER([cuda.h], [],
		[
			AC_MSG_NOTICE([error: no cuda.h found; check path for CUDA Toolkit first...])
			cudawrap_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS
	])

	AS_IF([test x"$CUDALIB" = x -a x"$cudawrap_error" = "xno"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $CUDATKLIBDIR -lcuda"
		AC_MSG_CHECKING([whether linking with -lcuda works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); CUDALIB=-lcuda],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$CUDALIB" = x -a x"$cudawrap_error" = "xno"],
	[
		AC_MSG_NOTICE([error: no libcuda found; check path for CUDA Toolkit first...])
		cudawrap_error="yes"
	])

	AS_IF([test x"$cudawrap_error" = "xno"],
	[
		AC_MSG_CHECKING([whether CUDA driver version >= 3.0])

		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $CUDATKINCDIR"
		AC_TRY_COMPILE([#include "cuda.h"],
		[
#ifndef CUDA_VERSION
#  error "CUDA_VERSION not defined"
#elif CUDA_VERSION < 3000
#  error "CUDA_VERSION < 3000"
#endif
		],
		[AC_MSG_RESULT([yes])],
		[
			AC_MSG_RESULT([no])
			AC_MSG_NOTICE([error: CUDA driver version could not be determined and/or is incompatible (< 3.0)
 See \`config.log' for more details.])
			cudawrap_error="yes"
                ])
                CPPFLAGS=$sav_CPPFLAGS
	])

	AS_IF([test x"$cudawrap_error" = "xno"],
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
					cudawrap_error="yes"
				],
				[
					AC_MSG_RESULT([$cudalib_pathname])
				])
			])
		])
	])

	AS_IF([test x"$cudawrap_error" = "xno"],
	[
		AS_IF([test x"$cudalib_pathname" != x],
		[
			AC_DEFINE_UNQUOTED([DEFAULT_CUDALIB_PATHNAME],
			["$cudalib_pathname"], [pathname of CUDA driver library])
		])

		have_cudawrap="yes"
	])

	AS_IF([test x"$cudartwrap_error" = "xno"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $CUDATKINCDIR"
		AC_CHECK_HEADER([cuda_runtime_api.h], [],
		[
			AC_MSG_NOTICE([error: no cuda_runtime_api.h found; check path for CUDA Toolkit first...])
			cudartwrap_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS
	])

	AS_IF([test x"$CUDARTLIB" = x -a x"$cudartwrap_error" = "xno"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $CUDATKLIBDIR -lcudart"
		AC_MSG_CHECKING([whether linking with -lcudart works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); CUDARTLIB=-lcudart],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$CUDARTLIB" = x -a x"$cudartwrap_error" = "xno"],
	[
		AC_MSG_NOTICE([error: no libcudart found; check path for CUDA Toolkit first...])
		cudartwrap_error="yes"
	])

	AS_IF([test x"$cudartwrap_error" = "xno"],
	[
		AC_MSG_CHECKING([whether CUDA runtime version >= 3.0])

		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $CUDATKINCDIR"
		AC_TRY_COMPILE([#include "cuda_runtime_api.h"],
		[
#ifndef CUDART_VERSION
#  error "CUDART_VERSION not defined"
#elif CUDART_VERSION < 3000
#  error "CUDART_VERSION < 3000"
#endif
		],
		[AC_MSG_RESULT([yes])],
		[
			AC_MSG_RESULT([no])
			AC_MSG_NOTICE([error: CUDA runtime version could not be determined and/or is incompatible (< 3.0)
See \`config.log' for more details.])
			cudartwrap_error="yes"
		])
		CPPFLAGS=$sav_CPPFLAGS
	])

	AS_IF([test x"$cudartwrap_error" = "xno"],
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
					cudartwrap_error="yes"
				],
				[
					AC_MSG_RESULT([$cudartlib_pathname])
				])
			])
		])
	])

	AS_IF([test x"$cudartwrap_error" = "xno"],
	[
		AC_CHECK_PROG(NVCC, nvcc, nvcc, , [$PATH$PATH_SEPARATOR$CUDATKDIR"bin/"])

		AS_IF([test x"$cudartlib_pathname" != x],
		[
			AC_DEFINE_UNQUOTED([DEFAULT_CUDARTLIB_PATHNAME],
			["$cudartlib_pathname"], [pathname of CUDA runtime library])
		])

		have_cudartwrap="yes"
	])

	AS_IF([test x"$cudawrap_error" = "xno" -a x"$cudartwrap_error" = "xno"],
	[
		ACVT_CUPTI
	])

	AC_SUBST(CUDATKINCDIR)
	AC_SUBST(CUDATKLIBDIR)
	AC_SUBST(CUDATKLIB)
])
