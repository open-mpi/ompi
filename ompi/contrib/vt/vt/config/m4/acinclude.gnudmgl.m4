AC_DEFUN([ACVT_GNUDMGL],
[
	gnudmgl_error="no"
	have_gnudmgl="no"

	ACVT_LIBERTY
	AS_IF([test x"$LIBERTYLIB" = x], [gnudmgl_error="yes"])

	AS_IF([test x"$gnudmgl_error" = "xno"],
	[
		sav_LIBS=$LIBS
		LIBS="$LIBS $LIBERTYLIBDIR $LIBERTYLIB"
		AC_CHECK_FUNC([cplus_demangle], [], [gnudmgl_error="yes"])
		LIBS=$sav_LIBS
	])

	AS_IF([test x"$gnudmgl_error" = "xno"],
	[
		sav_CPPFLAGS=$CPPFLAGS
		CPPFLAGS="$CPPFLAGS $LIBERTYINCDIR"
		AC_CHECK_HEADERS([demangle.h])
		CPPFLAGS=$sav_CPPFLAGS
	])

	AS_IF([test x"$gnudmgl_error" = "xno"],
	[
		have_gnudmgl="yes"
		AC_DEFINE([HAVE_GNU_DEMANGLE],
			[1], [Define to 1 if you can use GNU demangling of C++ names.])
	])
])

