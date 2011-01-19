
dnl PAC_SET_HEADER_LIB_PATH(with_option)
dnl This macro looks for the --with-xxx=, --with-xxx-include and --with-xxx-lib=
dnl options and sets the library and include paths.
AC_DEFUN([PAC_SET_HEADER_LIB_PATH],[
    AC_ARG_WITH($1, 
    		AC_HELP_STRING([--with-$1=path],
			       [specify path where $1 include directory and lib directory can be found]),
        if test "${with_$1}" != "yes" -a "${with_$1}" != "no" ; then
            # is adding lib64 by default really the right thing to do?  What if
            # we are on a 32-bit host that happens to have both lib dirs available?
            LDFLAGS="$LDFLAGS -L${with_$1}/lib64 -L${with_$1}/lib"
            CPPFLAGS="$CPPFLAGS -I${with_$1}/include"
	    WRAPPER_CFLAGS="$WRAPPER_CFLAGS -I${with_$1}/include"
        fi,
    )
    AC_ARG_WITH($1-include, 
    		AC_HELP_STRING([--with-$1-include=path],
			       [specify path where $1 include directory can be found]),
        if test "${with_$1_include}" != "yes" -a "${with_$1_include}" != "no" ; then
            CPPFLAGS="$CPPFLAGS -I${with_$1_include}"
            WRAPPER_CFLAGS="$WRAPPER_CFLAGS -I${with_$1_include}"
        fi,
    )
    AC_ARG_WITH($1-lib, 
    		AC_HELP_STRING([--with-$1-lib=path],
			       [specify path where $1 lib directory can be found]),
        if test "${with_$1_lib}" != "yes" -a "${with_$1_lib}" != "no" ; then
            LDFLAGS="$LDFLAGS -L${with_$1_lib}"
        fi,
    )
])


dnl PAC_CHECK_HEADER_LIB(with_option, header.h, libname, function, action-if-yes, action-if-no)
dnl This macro checks for a header and lib.  It is assumed that the
dnl user can specify a path to the includes and libs using --with-xxx=.
dnl The xxx is specified in the "with_option" parameter.
AC_DEFUN([PAC_CHECK_HEADER_LIB],[
    failure=no
    AC_CHECK_HEADER([$2],,failure=yes)
    AC_CHECK_LIB($3,$4,,failure=yes)
    if test "$failure" = "no" ; then
       $5
    else
       $6
    fi
])

dnl PAC_CHECK_HEADER_LIB_FATAL(with_option, header.h, libname, function)
dnl Similar to PAC_CHECK_HEADER_LIB, but errors out on failure
AC_DEFUN([PAC_CHECK_HEADER_LIB_FATAL],[
	PAC_CHECK_HEADER_LIB($1,$2,$3,$4,success=yes,success=no)
	if test "$success" = "no" ; then
	   AC_MSG_ERROR(['$2 or lib$3 library not found. Did you specify --with-$1= or --with-$1-include= or --with-$1-lib=?'])
	fi
])
