dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2006 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2006 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
dnl                         reserved. 
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_SETUP_CXX()
# ----------------
# Do everything required to setup the C++ compiler.  Safe to AC_REQUIRE
# this macro.
AC_DEFUN([OMPI_SETUP_CXX],[

    AC_REQUIRE([_OMPI_START_SETUP_CXX])
    AC_REQUIRE([_OMPI_PROG_CXX])

    # Do we want code coverage
    if test "$WANT_COVERAGE" = "1"; then 
        if test "$ompi_cxx_vendor" = "gnu" ; then
            AC_MSG_WARN([$OMPI_COVERAGE_FLAGS has been added to CFLAGS (--enable-coverage)])
            WANT_DEBUG=1
            CXXFLAGS="${CXXFLAGS} $OMPI_COVERAGE_FLAGS"
            WRAPPER_EXTRA_CXXFLAGS="${WRAPPER_EXTRA_CXXFLAGS} $OMPI_COVERAGE_FLAGS"
        else
            AC_MSG_WARN([Code coverage functionality is currently available only with GCC suite])
            AC_MSG_ERROR([Configure: cannot continue])
        fi
    fi

    # Do we want debugging?
    if test "$WANT_DEBUG" = "1" -a "$enable_debug_symbols" != "no" ; then
        CXXFLAGS="$CXXFLAGS -g"
        OMPI_UNIQ(CXXFLAGS)
        AC_MSG_WARN([-g has been added to CXXFLAGS (--enable-debug)])
    fi

    # These flags are generally g++-specific; even the g++-impersonating
    # compilers won't accept them.
    OMPI_CXXFLAGS_BEFORE_PICKY="$CXXFLAGS"
    if test "$WANT_PICKY_COMPILER" = 1 -a "$ompi_cxx_vendor" = "gnu"; then
        add="-Wall -Wundef -Wno-long-long"

        # see if -Wno-long-double works...
        AC_LANG_PUSH(C++)
        CXXFLAGS_orig="$CXXFLAGS"
        CXXFLAGS="$CXXFLAGS -Wno-long-double"
        AC_CACHE_CHECK([if $CXX supports -Wno-long-double],
                   [ompi_cv_cxx_wno_long_double],
                   [AC_TRY_COMPILE([], [], 
                                   [ompi_cv_cxx_wno_long_double="yes"],
                                   [ompi_cv_cxx_wno_long_double="no"])])
        CXXFLAGS="$CXXFLAGS_orig"
        AC_LANG_POP(C++)
        if test "$ompi_cv_cxx_wno_long_double" = "yes" ; then
            add="$add -Wno-long-double"
        fi

        CXXFLAGS="$CXXFLAGS $add"
        OMPI_UNIQ(CXXFLAGS)
        if test "$add" != "" ; then
            AC_MSG_WARN([$add has been added to CXXFLAGS (--enable-picky)])
        fi
        unset add
    fi

    # See if this version of g++ allows -finline-functions
    if test "$GXX" = "yes"; then
        CXXFLAGS_orig="$CXXFLAGS"
        CXXFLAGS="$CXXFLAGS -finline-functions"
        add=
        AC_CACHE_CHECK([if $CXX supports -finline-functions],
                   [ompi_cv_cxx_finline_functions],
                   [AC_TRY_COMPILE([], [],
                                   [ompi_cv_cxx_finline_functions="yes"],
                                   [ompi_cv_cxx_finline_functions="no"])])
        if test "$ompi_cv_cxx_finline_functions" = "yes" ; then
            add=" -finline-functions"
        fi
        CXXFLAGS="$CXXFLAGS_orig$add"
        OMPI_UNIQ(CXXFLAGS)
        if test "$add" != "" ; then
            AC_MSG_WARN([$add has been added to CXXFLAGS])
        fi
        unset add
    fi

    # Check for special things due to C++ exceptions
    ENABLE_CXX_EXCEPTIONS=no
    HAVE_CXX_EXCEPTIONS=0
    AC_ARG_ENABLE([cxx-exceptions], 
        [AC_HELP_STRING([--enable-cxx-exceptions],
	                [enable support for C++ exceptions])],
        [ENABLE_CXX_EXCEPTIONS="$enableval"])

    AC_MSG_CHECKING([if want C++ exception handling])
    AC_MSG_RESULT([$ENABLE_CXX_EXCEPTIONS])
    if test "$ENABLE_CXX_EXCEPTIONS" = "yes"; then
        # config/cxx_have_exceptions.m4
        OMPI_CXX_HAVE_EXCEPTIONS
        # config/cxx_find_exception_flags.m4
        OMPI_CXX_FIND_EXCEPTION_FLAGS
        if test "$OMPI_CXX_EXCEPTIONS" = "1"; then
            HAVE_CXX_EXCEPTIONS=1
            CFLAGS="$CFLAGS $OMPI_CXX_EXCEPTIONS_CFLAGS"
            FFLAGS="$FFLAGS $OMPI_CXX_EXCEPTIONS_FFLAGS"
            CXXFLAGS="$CXXFLAGS $OMPI_CXX_EXCEPTIONS_CXXFLAGS"
            LDFLAGS="$LDFLAGS $OMPI_CXX_EXCEPTIONS_LDFLAGS"

            WRAPPER_EXTRA_CFLAGS="$OMPI_CXX_EXCEPTIONS_CFLAGS ${WRAPPER_EXTRA_CFLAGS}"
            WRAPPER_EXTRA_FFLAGS="$OMPI_CXX_EXCEPTIONS_FFLAGS ${WRAPPER_EXTRA_FFLAGS}"
            WRAPPER_EXTRA_CXXFLAGS="$OMPI_CXX_EXCEPTIONS_CXXFLAGS ${WRAPPER_EXTRA_CXXFLAGS}"
        fi
    fi
    AC_DEFINE_UNQUOTED(OMPI_HAVE_CXX_EXCEPTION_SUPPORT, $HAVE_CXX_EXCEPTIONS,
        [Whether or not we have compiled with C++ exceptions support])

    # Make sure we can link with the C compiler
    if[ test "$ompi_cv_cxx_compiler_vendor" != "microsoft" ]; then
      OMPI_LANG_LINK_WITH_C([C++], [],
        [cat <<EOF >&2
**********************************************************************
* It appears that your C++ compiler is unable to link against object
* files created by your C compiler.  This generally indicates either
* a conflict between the options specified in CFLAGS and CXXFLAGS
* or a problem with the local compiler installation.  More
* information (including exactly what command was given to the 
* compilers and what error resulted when the commands were executed) is
* available in the config.log file in this directory.
**********************************************************************
EOF
         AC_MSG_ERROR([C and C++ compilers are not link compatible.  Can not continue.])])
    fi

    # Find some more characteristics of the C++ compiler

    # config/cxx_find_template_repository.m4
    OMPI_CXX_FIND_TEMPLATE_REPOSITORY
    # config/cxx_find_template_parameters.m4
    OMPI_CXX_FIND_TEMPLATE_PARAMETERS

    # see if the C++ compiler supports __builtin_expect
    AC_LANG_PUSH(C++)
    AC_CACHE_CHECK([if $CXX supports __builtin_expect],
        [ompi_cv_cxx_supports___builtin_expect],
        [AC_TRY_LINK([],
          [void *ptr = (void*) 0;
           if (__builtin_expect (ptr != (void*) 0, 1)) return 0;],
          [ompi_cv_cxx_supports___builtin_expect="yes"],
          [ompi_cv_cxx_supports___builtin_expect="no"])])
    if test "$ompi_cv_cxx_supports___builtin_expect" = "yes" ; then
        have_builtin_expect=1
    else
        have_builtin_expect=0
    fi
    AC_DEFINE_UNQUOTED([OMPI_CXX_HAVE_BUILTIN_EXPECT], [$have_builtin_expect],
          [Whether C++ compiler supports __builtin_expect])
    AC_LANG_POP(C++)

    # see if the C compiler supports __builtin_prefetch
    AC_LANG_PUSH(C++)
    AC_CACHE_CHECK([if $CXX supports __builtin_prefetch],
        [ompi_cv_cxx_supports___builtin_prefetch],
        [AC_TRY_LINK([],
          [int ptr;
           __builtin_prefetch(&ptr,0,0);],
          [ompi_cv_cxx_supports___builtin_prefetch="yes"],
          [ompi_cv_cxx_supports___builtin_prefetch="no"])])
    if test "$ompi_cv_cxx_supports___builtin_prefetch" = "yes" ; then
        have_builtin_prefetch=1
    else
        have_builtin_prefetch=0
    fi
    AC_DEFINE_UNQUOTED([OMPI_CXX_HAVE_BUILTIN_PREFETCH], [$have_builtin_prefetch],
          [Whether C++ compiler supports __builtin_prefetch])
    AC_LANG_POP(C++)

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

    # see if the compiler supports const_cast of 2-dimensional arrays
    AC_LANG_PUSH(C++)
    AC_CACHE_CHECK([if $CXX supports const_cast<> properly],
       [ompi_cv_cxx_supports_2d_const_cast],
       [AC_TRY_COMPILE([int non_const_func(int ranges[][3]);
int cast_test(const int ranges[][3]) {
  return non_const_func(const_cast<int(*)[3]>(ranges));  
}],
            [],
            [ompi_cv_cxx_supports_2d_const_cast="yes"],
            [ompi_cv_cxx_supports_2d_const_cast="no"])])
    if test "$ompi_cv_cxx_supports_2d_const_cast" = "yes" ; then
        use_2d_const_cast=1
    else
        use_2d_const_cast=0
    fi
    AC_DEFINE_UNQUOTED([OMPI_CXX_SUPPORTS_2D_CONST_CAST],
                       [$use_2d_const_cast],
                       [Whether a const_cast on a 2-d array will work with the C++ compiler])
    unset use_2d_const_cast
    AC_LANG_POP(C++)

    # Note: gcc-imperonating compilers accept -O3
    if test "$WANT_DEBUG" = "1"; then
        OPTFLAGS=
    else
        if test "$GXX" = yes; then
            OPTFLAGS="-O3"
        else
            OPTFLAGS="-O"
        fi
    fi

    # config/ompi_check_optflags.m4
    OMPI_CHECK_OPTFLAGS(["$CXXFLAGS"])
    AC_MSG_CHECKING([for C++ optimization flags])
    AC_MSG_RESULT([$co_result])
    CXXFLAGS="$co_result"
])


AC_DEFUN([_OMPI_START_SETUP_CXX],[
    ompi_show_subtitle "C++ compiler and preprocessor" 
])


AC_DEFUN([_OMPI_PROG_CXX],[
    ompi_cxxflags_save="$CXXFLAGS"
    AC_PROG_CXX
    AC_PROG_CXXCPP
    BASECXX="`basename $CXX`"
    CXXFLAGS="$ompi_cxxflags_save"
    AC_DEFINE_UNQUOTED(OMPI_CXX, "$CXX", [OMPI underlying C++ compiler])
    OMPI_CXX_ABSOLUTE="`which $CXX`"
    AC_SUBST(OMPI_CXX_ABSOLUTE)

    OMPI_CXX_COMPILER_VENDOR([ompi_cxx_vendor])

    # Make sure that the C++ compiler both works and is actually a C++
    # compiler (if not cross-compiling).  Don't just use the AC macro
    # so that we can have a pretty message.  Do something here that
    # should force the linking of C++-specific things (e.g., STL
    # strings) so that we can force a hard check of compiling,
    # linking, and running a C++ application Note that some C
    # compilers, such as at least some versions of the GNU and Intel
    # compilers, will detect that the file extension is ".cc" and
    # therefore switch into a pseudo-C++ personality which works for
    # *compiling*, but does not work for *linking*.  So in this test,
    # we want to cover the entire spectrum (compiling, linking,
    # running).

	if[ test "$ompi_cv_cxx_compiler_vendor" != "microsoft" ]; then
		OMPI_CHECK_COMPILER_WORKS([C++], [#include <string>], 
				[std::string foo = "Hello, world"], [], 
				[AC_MSG_ERROR([Could not run a simple C++ program.  Aborting.])])
	fi
])
