dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

AC_DEFUN([LAM_SETUP_CXX],[

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
    CXXFLAGS="$CXXFLAGS -g"
    LAM_UNIQ(CXXFLAGS)
    AC_MSG_WARN([-g has been added to CXXFLAGS (--enable-debug)])
fi

LAM_CXXFLAGS_BEFORE_PICKY="$CXXFLAGS"
if test "$GCC" = "yes" -a "$WANT_PICKY_COMPILER" = 1; then
    add="-g -Wall -Wundef -Wno-long-long"
    add="$add -Wmissing-prototypes -Wstrict-prototypes"

    # see if -Wno-long-double works...
    AC_LANG_PUSH(C++)
    CXXFLAGS_orig="$CXXFLAGS"
    CXXFLAGS="$CXXFLAGS -Wno-long-double"
    AC_TRY_COMPILE([], [], add="$add -Wno-long-double")
    CXXFLAGS="$CXXFLAGS_orig"
    AC_LANG_POP(C++)

    CXXFLAGS="$CXXFLAGS $add"
    LAM_UNIQ(CXXFLAGS)
    AC_MSG_WARN([$add has been added to CXXFLAGS (developer copy)])
    unset add
fi

# See if this version of gcc allows -finline-functions
if test "$GCC" = "yes"; then
    CXXFLAGS_orig="$CXXFLAGS"
    CXXFLAGS="$CXXFLAGS -finline-functions"
    add=
    AC_TRY_COMPILE([], [], add=" -finline-functions")
    CXXFLAGS="$CXXFLAGS_orig$add"
    LAM_UNIQ(CXXFLAGS)
    AC_MSG_WARN([$add has been added to CXXFLAGS])
    unset add
fi

# Check for special things due to C++ exceptions

WRAPPER_EXTRA_CFLAGS=
WRAPPER_EXTRA_FFLAGS=
WRAPPER_EXTRA_CXXFLAGS=
WRAPPER_EXTRA_LDFLAGS=
WRAPPER_EXTRA_LIBS=
ENABLE_CXX_EXCEPTIONS=no
HAVE_CXX_EXCEPTIONS=0
AC_ARG_ENABLE(cxx-exceptions, 
  AC_HELP_STRING([--enable-cxx-exceptions],
		 [enable support for C++ exceptions]),
  [ENABLE_CXX_EXCEPTIONS="$enableval"])

AC_MSG_CHECKING([if want C++ exception handling])
AC_MSG_RESULT([$ENABLE_CXX_EXCEPTIONS])
if test "$ENABLE_CXX_EXCEPTIONS" = "yes"; then
    # config/cxx_have_exceptions.m4
    LAM_CXX_HAVE_EXCEPTIONS
    # config/cxx_find_exception_flags.m4
    LAM_CXX_FIND_EXCEPTION_FLAGS
    if test "$LAM_CXX_EXCEPTIONS" = "1"; then
	HAVE_CXX_EXCEPTIONS=1
	CFLAGS="$CFLAGS $LAM_CXX_EXCEPTIONS_CFLAGS"
	FFLAGS="$FFLAGS $LAM_CXX_EXCEPTIONS_FFLAGS"
	CXXFLAGS="$CXXFLAGS $LAM_CXX_EXCEPTIONS_CXXFLAGS"
	LDFLAGS="$LDFLAGS $LAM_CXX_EXCEPTIONS_LDFLAGS"

	WRAPPER_EXTRA_CFLAGS="$LAM_CXX_EXCEPTIONS_CFLAGS"
	WRAPPER_EXTRA_FFLAGS="$LAM_CXX_EXCEPTIONS_FFLAGS"
	WRAPPER_EXTRA_CXXFLAGS="$LAM_CXX_EXCEPTIONS_CXXFLAGS"
    fi
fi
AC_DEFINE_UNQUOTED(LAM_HAVE_CXX_EXCEPTION_SUPPORT, $HAVE_CXX_EXCEPTIONS,
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
	AC_MSG_WARN([*** with the environment variable "CXX=aCC".])
    fi
    ;;
esac

# Same rationale for g++ as with gcc in LAM_SETUP_CC.

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
