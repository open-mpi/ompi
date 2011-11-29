AC_DEFUN([ACVT_CUPTI],
[
	cupti_error="no"
	have_cupti="no"

	CUPTIDIR=
	CUPTIINCDIR=
	CUPTILIBDIR=
	CUPTILIB=

	AC_ARG_WITH(cupti-dir,
		AC_HELP_STRING([--with-cupti-dir=CUPTIDIR],
		[give the path for CUPTI, default: /usr]),
	[CUPTIDIR="$withval/"])

	AC_ARG_WITH(cupti-inc-dir,
		AC_HELP_STRING([--with-cupti-inc-dir=CUPTIINCDIR],
		[give the path for CUPTI-include files, default: CUPTIDIR/include]),
	[CUPTIINCDIR="-I$withval/"],
	[AS_IF([test x"$CUPTIDIR" != x], [CUPTIINCDIR="-I$CUPTIDIR"include/])])

	AC_ARG_WITH(cupti-lib-dir,
		AC_HELP_STRING([--with-cupti-lib-dir=CUPTILIBDIR],
		[give the path for CUPTI-libraries, default: CUPTIDIR/lib]),
	[CUPTILIBDIR="-L$withval/"],
	[AS_IF([test x"$CUPTIDIR" != x], [CUPTILIBDIR="-L$CUPTIDIR"lib/])])

	AC_ARG_WITH(cupti-lib,
		AC_HELP_STRING([--with-cupti-lib=CUPTILIB], [use given cupti lib, default: -lcupti CUDALIB]),
	[CUPTILIB="$withval"])

	sav_CPPFLAGS=$CPPFLAGS
	CPPFLAGS="$CPPFLAGS $CUPTIINCDIR $CUDATKINCDIR"
	AC_CHECK_HEADER([cupti_events.h], [],
	[
		AC_MSG_NOTICE([error: no cupti_events.h found; check path for CUPTI package first...])
		cupti_error="yes"
	])
	CPPFLAGS=$sav_CPPFLAGS

	AS_IF([test x"$CUPTILIB" = x -a x"$cupti_error" = "xno"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $CUPTILIBDIR -lcupti $CUDATKLIBDIR $CUDALIB"
		AC_MSG_CHECKING([whether linking with -lcupti works])
		AC_TRY_LINK([],[],
		[AC_MSG_RESULT([yes]); CUPTILIB="-lcupti $CUDATKLIBDIR $CUDALIB"],[AC_MSG_RESULT([no])])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$CUPTILIB" = x -a x"$cupti_error" = "xno"],
	[
		AC_MSG_NOTICE([error: no libcupti found; check path for CUPTI package first...])
		cupti_error="yes"
	])

	AC_MSG_CHECKING([whether CUDA runtime version >= 4.0])

	sav_CPPFLAGS=$CPPFLAGS
	CPPFLAGS="$CPPFLAGS $CUDATKINCDIR"
	AC_TRY_COMPILE([#include "cuda_runtime_api.h"],
[
#ifndef CUDART_VERSION
#  error "CUDART_VERSION not defined"
#elif CUDART_VERSION < 4000
#  error "CUDART_VERSION < 4000"
#endif
],
	[AC_MSG_RESULT([yes])],
	[
		AC_MSG_RESULT([no])
		AC_MSG_NOTICE([error: CUDA runtime version could not be determined and/or is incompatible (< 4.0)
See \`config.log' for more details.])
		cupti_error="yes"
	])
	CPPFLAGS=$sav_CPPFLAGS

	AS_IF([test x"$cupti_error" = "xno"], [have_cupti="yes"])

	AC_SUBST(CUPTIINCDIR)
	AC_SUBST(CUPTILIBDIR)
	AC_SUBST(CUPTILIB)
])

