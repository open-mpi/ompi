
dnl PAC_SET_HEADER_LIB_PATH(with_option,[default_path])
dnl This macro looks for the --with-xxx=, --with-xxx-include and --with-xxx-lib=
dnl options and sets the library and include paths.
dnl
dnl TODO as written, this macro cannot handle a "with_option" arg that has "-"
dnl characters in it.  Use AS_TR_SH (and possibly AS_VAR_* macros) to handle
dnl this case if it ever arises.
AC_DEFUN([PAC_SET_HEADER_LIB_PATH],[
    AC_ARG_WITH([$1],
                [AC_HELP_STRING([--with-$1=PATH],
                                [specify path where $1 include directory and lib directory can be found])],

                [AS_CASE(["$withval"],
                         [yes|no|''],
                         [AC_MSG_WARN([--with[out]-$1=PATH expects a valid PATH])
                          with_$1=""])],
                [with_$1=$2])
    AC_ARG_WITH([$1-include],
                [AC_HELP_STRING([--with-$1-include=PATH],
                                [specify path where $1 include directory can be found])],
                [AS_CASE(["$withval"],
                         [yes|no|''],
                         [AC_MSG_WARN([--with[out]-$1-include=PATH expects a valid PATH])
                          with_$1_include=""])],
                [])
    AC_ARG_WITH([$1-lib],
                [AC_HELP_STRING([--with-$1-lib=PATH],
                                [specify path where $1 lib directory can be found])],
                [AS_CASE(["$withval"],
                         [yes|no|''],
                         [AC_MSG_WARN([--with[out]-$1-lib=PATH expects a valid PATH])
                          with_$1_lib=""])],
                [])

    # The args have been sanitized into empty/non-empty values above.
    # Now append -I/-L args to CPPFLAGS/LDFLAGS, with more specific options
    # taking priority

    AS_IF([test -n "${with_$1_include}"],
          [PAC_APPEND_FLAG([-I${with_$1_include}],[CPPFLAGS])
	   PAC_APPEND_FLAG([-I${with_$1_include}],[WRAPPER_CPPFLAGS])],
          [AS_IF([test -n "${with_$1}"],
                 [PAC_APPEND_FLAG([-I${with_$1}/include],[CPPFLAGS])
		  PAC_APPEND_FLAG([-I${with_$1}/include],[WRAPPER_CPPFLAGS])])])

    AS_IF([test -n "${with_$1_lib}"],
          [PAC_APPEND_FLAG([-L${with_$1_lib}],[LDFLAGS])
	   PAC_APPEND_FLAG([-L${with_$1_lib}],[WRAPPER_LDFLAGS])],
          [AS_IF([test -n "${with_$1}"],
                 dnl is adding lib64 by default really the right thing to do?  What if
                 dnl we are on a 32-bit host that happens to have both lib dirs available?
                 [PAC_APPEND_FLAG([-L${with_$1}/lib64],[LDFLAGS])
		  PAC_APPEND_FLAG([-L${with_$1}/lib64],[WRAPPER_LDFLAGS])
                  PAC_APPEND_FLAG([-L${with_$1}/lib],[LDFLAGS])
		  PAC_APPEND_FLAG([-L${with_$1}/lib],[WRAPPER_LDFLAGS])])])
])


dnl PAC_CHECK_HEADER_LIB(header.h, libname, function, action-if-yes, action-if-no)
dnl This macro checks for a header and lib.  It is assumed that the
dnl user can specify a path to the includes and libs using --with-xxx=.
dnl The xxx is specified in the "with_option" parameter.
dnl
dnl NOTE: This macro expects a corresponding PAC_SET_HEADER_LIB_PATH
dnl macro (or equivalent logic) to be used before this macro is used.
AC_DEFUN([PAC_CHECK_HEADER_LIB],[
    failure=no
    AC_CHECK_HEADER([$1],,failure=yes)
    AC_CHECK_LIB($2,$3,,failure=yes)
    if test "$failure" = "no" ; then
       $4
    else
       $5
    fi
])

dnl PAC_CHECK_HEADER_LIB_FATAL(with_option, header.h, libname, function)
dnl Similar to PAC_CHECK_HEADER_LIB, but errors out on failure
AC_DEFUN([PAC_CHECK_HEADER_LIB_FATAL],[
	PAC_CHECK_HEADER_LIB($2,$3,$4,success=yes,success=no)
	if test "$success" = "no" ; then
	   AC_MSG_ERROR(['$2 or lib$3 library not found. Did you specify --with-$1= or --with-$1-include= or --with-$1-lib=?'])
	fi
])

dnl PAC_CHECK_PREFIX(with_option,prefixvar)
AC_DEFUN([PAC_CHECK_PREFIX],[
	AC_ARG_WITH([$1-prefix],
            [AS_HELP_STRING([[--with-$1-prefix[=DIR]]], [use the $1
                            library installed in DIR, rather than the
                            one included in the distribution.  Pass
                            "embedded" to force usage of the included
                            $1 source.])],
            [if test "$withval" = "system" ; then
                 :
             elif test "$withval" = "embedded" ; then
                 :
             else
                 PAC_APPEND_FLAG([-I${with_$1_prefix}/include],[CPPFLAGS])
                 if test -d "${with_$1_prefix}/lib64" ; then
                     PAC_APPEND_FLAG([-L${with_$1_prefix}/lib64],[LDFLAGS])
                 fi
                 PAC_APPEND_FLAG([-L${with_$1_prefix}/lib],[LDFLAGS])
             fi
             ],
            [with_$1_prefix="embedded"])
	]
)
