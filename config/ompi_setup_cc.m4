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
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_SETUP_CC()
# ---------------
# Do everything required to setup the C compiler.  Safe to AC_REQUIRE
# this macro.
AC_DEFUN([OMPI_SETUP_CC],[
    # AM_PROG_CC_C_O AC_REQUIREs AC_PROG_CC, so we have to be a little
    # careful about ordering here, and AC_REQUIRE these things so that
    # they get stamped out in the right order.

    AC_REQUIRE([_OMPI_START_SETUP_CC])
    AC_REQUIRE([_OMPI_PROG_CC])
    AC_REQUIRE([AM_PROG_CC_C_O])

    OMPI_C_COMPILER_VENDOR([ompi_c_vendor])

    # Do we want code coverage
    if test "$WANT_COVERAGE" = "1"; then 
        if test "$ompi_c_vendor" = "gnu" ; then
            CLEANFILES="*.bb *.bbg *.da *.*.gcov ${CLEANFILES}"
             AC_MSG_WARN([-fprofile-arcs -ftest-coverage has been added to CFLAGS (--enable-coverage)])
            WANT_DEBUG=1
            CFLAGS="-ftest-coverage -fprofile-arcs ${CFLAGS}"
            WRAPPER_EXTRA_CFLAGS="-ftest-coverage -fprofile-arcs ${WRAPPER_EXTRA_CFLAGS}"
        else
            AC_MSG_WARN([Code coverage functionality is currently available only with GCC])
            AC_MSG_ERROR([Configure: Cannot continue])
       fi
    fi

    # Do we want debugging?
    if test "$WANT_DEBUG" = "1" -a "$enable_debug_symbols" != "no" ; then
        if test "$ompi_c_vendor" = "gnu"; then
            CFLAGS="$CFLAGS -g3"
        else
            CFLAGS="$CFLAGS -g"
        fi

        OMPI_UNIQ(CFLAGS)
        AC_MSG_WARN([-g has been added to CFLAGS (--enable-debug)])
    fi

    # These flags are generally gcc-specific; even the
    # gcc-impersonating compilers won't accept them.
    OMPI_CFLAGS_BEFORE_PICKY="$CFLAGS"
    if test "$WANT_PICKY_COMPILER" = 1 -a "$ompi_c_vendor" = "gnu" ; then
        add="-Wall -Wundef -Wno-long-long -Wsign-compare"
        add="$add -Wmissing-prototypes -Wstrict-prototypes"
        add="$add -Wcomment -pedantic"

        # see if -Wno-long-double works...
        CFLAGS_orig="$CFLAGS"
        CFLAGS="$CFLAGS -Wno-long-double"
        AC_CACHE_CHECK([if $CC supports -Wno-long-double],
                   [ompi_cv_cc_wno_long_double],
                   [AC_TRY_COMPILE([], [], 
                                   [ompi_cv_cc_wno_long_double="yes"],
                                   [ompi_cv_cc_wno_long_double="no"])])

        CFLAGS="$CFLAGS_orig"
        if test "$ompi_cv_cc_wno_long_double" = "yes" ; then
            add="$add -Wno-long-double"
        fi

        add="$add -Werror-implicit-function-declaration "

        CFLAGS="$CFLAGS $add"
        OMPI_UNIQ(CFLAGS)
        AC_MSG_WARN([$add has been added to CFLAGS (--enable-picky)])
        unset add
    fi

    # See if this version of gcc allows -finline-functions and/or
    # -fno-strict-aliasing.  Even check the gcc-impersonating compilers.
    if test "$GCC" = "yes"; then
        CFLAGS_orig="$CFLAGS"

        CFLAGS="$CFLAGS_orig -finline-functions"
        add=
        AC_CACHE_CHECK([if $CC supports -finline-functions],
                   [ompi_cv_cc_finline_functions],
                   [AC_TRY_COMPILE([], [],
                                   [ompi_cv_cc_finline_functions="yes"],
                                   [ompi_cv_cc_finline_functions="no"])])
        if test "$ompi_cv_cc_finline_functions" = "yes" ; then
            add=" -finline-functions"
        fi
        CFLAGS="$CFLAGS_orig$add"

        CFLAGS="$CFLAGS_orig -fno-strict-aliasing"
        add=
        AC_CACHE_CHECK([if $CC supports -fno-strict-aliasing],
                   [ompi_cv_cc_fno_strict_aliasing],
                   [AC_TRY_COMPILE([], [],
                                   [ompi_cv_cc_fno_strict_aliasing="yes"],
                                   [ompi_cv_cc_fno_strict_aliasing="no"])])
        if test "$ompi_cv_cc_fno_strict_aliasing" = "yes" ; then
            add=" -fno-strict-aliasing"
        fi
        CFLAGS="$CFLAGS_orig$add"

        OMPI_UNIQ(CFLAGS)
        AC_MSG_WARN([$add has been added to CFLAGS])
        unset add
    fi

    # Try to enable restrict keyword
    RESTRICT_CFLAGS=
    case "$ompi_c_vendor" in
        intel)
            RESTRICT_CFLAGS="-restrict"
        ;;
        sgi)
            RESTRICT_CFLAGS="-LANG:restrict=ON"
        ;;
    esac
    if test ! -z "$RESTRICT_CFLAGS" ; then
        CFLAGS_orig="$CFLAGS"
        CFLAGS="$CFLAGS_orig $RESTRICT_CFLAGS"
        add=
        AC_CACHE_CHECK([if $CC supports $RESTRICT_CFLAGS],
                   [ompi_cv_cc_restrict_cflags],
                   [AC_TRY_COMPILE([], [], 
                                   [ompi_cv_cc_restrict_cflags="yes"],
                                   [ompi_cv_cc_restrict_cflags="no"])])
        if test "ompi_cv_cc_restruct_cflags" = "yes" ; then
            add="$RESTRUCT_CFLAGS"
        fi

        CFLAGS="${CFLAGS_orig}${add}"
        OMPI_UNIQ([CFLAGS])
        if test "$add" != "" ; then
            AC_MSG_WARN([$add has been added to CFLAGS])
        fi
        unset add
    fi

    # Preload the optflags for the case where the user didn't specify
    # any.  If we're using GNU compilers, use -O3 (since it GNU
    # doesn't require all compilation units to be compiled with the
    # same level of optimization -- selecting a high level of
    # optimization is not prohibitive).  If we're using anything else,
    # be conservative and just use -O.
    #
    # Note: gcc-impersonating compilers accept -O3
    if test "$WANT_DEBUG" = "1"; then
        OPTFLAGS=
    else
        if test "$GCC" = yes; then
            OPTFLAGS="-O3"
        else
            OPTFLAGS="-O"
        fi
    fi

    OMPI_CHECK_OPTFLAGS("$OMPI_CFLAGS_BEFORE_PICKY")
    OMPI_CFLAGS_BEFORE_PICKY="$co_result"

    AC_MSG_CHECKING([for C optimization flags])
    OMPI_CHECK_OPTFLAGS(["$CFLAGS"])
    AC_MSG_RESULT([$co_result])
    CFLAGS="$co_result"
])


AC_DEFUN([_OMPI_START_SETUP_CC],[
    ompi_show_subtitle "C compiler and preprocessor" 

    # $%@#!@#% AIX!!  This has to be called before anything invokes the C
    # compiler.
    dnl AC_AIX
])


AC_DEFUN([_OMPI_PROG_CC],[
    #
    # Check for the compiler
    #
    ompi_cflags_save="$CFLAGS"
    AC_PROG_CC
    BASECC="`basename $CC`"
    CFLAGS="$ompi_cflags_save"
    AC_DEFINE_UNQUOTED(OMPI_CC, "$CC", [OMPI underlying C compiler])
    OMPI_CC_ABSOLUTE="`which $CC`"
    AC_SUBST(OMPI_CC_ABSOLUTE)
])

