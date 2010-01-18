AC_DEFUN([ACVT_PTHREAD],
[
	pthread_error="no"
	have_pthread="no"

	AC_ARG_VAR(PTHREAD_CFLAGS, [C compiler flags to enable support for POSIX threads])
	AC_ARG_VAR(PTHREAD_LIBS, [POSIX threads libraries])

	ACX_PTHREAD([], [pthread_error="yes"])

	AS_IF([test x"$pthread_error" = "xno"],
	[
		sav_CFLAGS=$CFLAGS
		CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
		AC_CHECK_HEADER([pthread.h], [], [pthread_error="yes"])
		CFLAGS=$sav_CFLAGS
	])

	AS_IF([test x"$pthread_error" = "xno"],
	[
		have_pthread="yes"
		AC_DEFINE([HAVE_PTHREAD], [1], [Define to 1 if VT is configured with Pthreads support.])
	])
])

