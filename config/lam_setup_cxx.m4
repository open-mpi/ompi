dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl
dnl $Id: lam_setup_cxx.m4,v 1.1 2004/01/07 07:43:28 jsquyres Exp $
dnl

AC_DEFUN(LAM_SETUP_CXX,[

# Modularize this setup so that sub-configure.in scripts can use this
# same setup code.

lam_show_subtitle "C++ compiler and preprocessor" 

lam_cxxflags_save="$CXXFLAGS"
AC_PROG_CXX
AC_PROG_CXXCPP
BASECXX="`basename $CXX`"
CXXFLAGS="$lam_cxxflags_save"
AC_DEFINE_UNQUOTED(LAM_CXX, "$CXX", [LAM underlying C++ compiler])

# Do we want debugging?

if test "$WANT_DEBUG" = "1"; then
    if test "$GXX" = "yes"; then
	add="-g -Wall -Wundef -Wno-long-long"
	add="$add -Wmissing-prototypes -Wstrict-prototypes"

        # see if -Wno-long-double works...
        CXXFLAGS_orig="$CXXFLAGS"
        CXXFLAGS="$CXXFLAGS -Wno-long-double"
	AC_LANG_PUSH(C++)
        AC_TRY_COMPILE([], [], add="$add -Wno-long-double")
	AC_LANG_POP
        CXXFLAGS="$CXXFLAGS_orig"
    else
	add="-g"
    fi
    CXXFLAGS="$CXXFLAGS $add"
    LAM_UNIQ(CXXFLAGS)
    AC_MSG_WARN([$add has been added to CXXFLAGS (--with-debug)])
    unset add
fi

# Check for special things due to C++ exceptions

WRAPPER_EXTRA_CFLAGS=
WRAPPER_EXTRA_FFLAGS=
WRAPPER_EXTRA_CXXFLAGS=
WRAPPER_EXTRA_LDFLAGS=
WRAPPER_EXTRA_LIBS=
WANT_CXXEXCEPTIONS=no
HAVE_CXXEXCEPTIONS=0
AC_ARG_ENABLE(cxx-exceptions, 
  AC_HELP_STRING([--enable-cxx-exceptions],
		 [enable support for C++ exceptions]),
  [WANT_CXXEXCEPTIONS="$withval"])

AC_MSG_CHECKING([if want C++ exception handling])
AC_MSG_RESULT([$enable_cxx_exceptions])
if test "$enable_cxx_exceptions" = "yes"; then
    # config/cxx_have_exceptions.m4
    AC_MSG_ERROR([Yell at Jeff to finish this section])
    LAM_CXX_HAVE_EXCEPTIONS
    # config/cxx_find_exception_flags.m4
    LAM_CXX_FIND_EXCEPTION_FLAGS
    if test "$LAM_CXX_EXCEPTIONS" = "1"; then
	HAVE_CXXEXCEPTIONS=1
	CFLAGS="$CFLAGS $LAM_CXX_EXCEPTIONS_CXXFLAGS"
	FFLAGS="$FFLAGS $LAM_CXX_EXCEPTIONS_CXXFLAGS"
	CXXFLAGS="$CXXFLAGS $LAM_CXX_EXCEPTIONS_CXXFLAGS"
	LDFLAGS="$LDFLAGS $LAM_CXX_EXCEPTIONS_LDFLAGS"

	WRAPPER_EXTRA_CFLAGS="$LAM_CXX_EXCEPTIONS_CXXFLAGS"
	WRAPPER_EXTRA_FFLAGS="$LAM_CXX_EXCEPTIONS_CXXFLAGS"
	WRAPPER_EXTRA_CXXFLAGS="$LAM_CXX_EXCEPTIONS_CXXFLAGS"
    fi
fi
AC_DEFINE_UNQUOTED(LAM_HAVE_CXX_EXCEPTION_SUPPORT, $HAVE_CXXEXCEPTIONS,
    [Whether or not we have compiled with C++ exceptions support])

# Find some more characteristics of the C++ compiler

# config/cxx_find_template_repository.m4
LAM_CXX_FIND_TEMPLATE_REPOSITORY
# config/cxx_find_template_parameters.m4
LAM_CXX_FIND_TEMPLATE_PARAMETERS

# If we are on HP-UX, ensure that we're using aCC
case "$host" in
*hpux*)
    if test "$BASECXX" = "CC"; then
	AC_MSG_WARN([*** You will probably have problems compiling the MPI 2])
	AC_MSG_WARN([*** C++ bindings with the HP-UX CC compiler.  You should])
	AC_MSG_WARN([*** probably be using the aCC compiler.  Re-run configure])
	AC_MSG_WARN([*** with "--with-cxx=aCC".])
    fi
    ;;
esac

# Same rationale for g++ as with gcc, above.
if test "$GXX" = yes; then
    OPTFLAGS="-O3"
else
    OPTFLAGS="-O"
fi
# config/lam_check_optflags.m4
LAM_CHECK_OPTFLAGS("$CXXFLAGS")
AC_MSG_CHECKING([for C++ optimization flags])
AC_MSG_RESULT([$co_result])
CXXFLAGS="$co_result"
])
