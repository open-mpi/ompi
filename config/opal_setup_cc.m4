dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2006 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2006 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2007-2009 Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2008-2013 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2012      Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OPAL_SETUP_CC()
# ---------------
# Do everything required to setup the C compiler.  Safe to AC_REQUIRE
# this macro.
AC_DEFUN([OPAL_SETUP_CC],[
    # AM_PROG_CC_C_O AC_REQUIREs AC_PROG_CC, so we have to be a little
    # careful about ordering here, and AC_REQUIRE these things so that
    # they get stamped out in the right order.

    AC_REQUIRE([_OMPI_START_SETUP_CC])
    AC_REQUIRE([_OMPI_PROG_CC])
    AC_REQUIRE([AM_PROG_CC_C_O])

    # AC_PROG_CC_C99 changes CC (instead of CFLAGS) so save CC (without c99
    # flags) for use in our wrappers.
    WRAPPER_CC="$CC"
    AC_SUBST([WRAPPER_CC])

    # From Open MPI 1.7 on we require a C99 compiant compiler
    AC_PROG_CC_C99
    # The result of AC_PROG_CC_C99 is stored in ac_cv_prog_cc_c99
    if test "x$ac_cv_prog_cc_c99" = xno ; then
        AC_MSG_WARN([Open MPI requires a C99 compiler])
        AC_MSG_ERROR([Aborting.])
    fi


    OMPI_C_COMPILER_VENDOR([ompi_c_vendor])

    # Check for standard headers, needed here because needed before
    # the types checks.
    AC_HEADER_STDC

    # GNU C and autotools are inconsistent about whether this is
    # defined so let's make it true everywhere for now...  However, IBM
    # XL compilers on PPC Linux behave really badly when compiled with
    # _GNU_SOURCE defined, so don't define it in that situation.
    #
    # Don't use AC_GNU_SOURCE because it requires that no compiler
    # tests are done before setting it, and we need to at least do
    # enough tests to figure out if we're using XL or not.
    AS_IF([test "$ompi_cv_c_compiler_vendor" != "ibm"],
          [AH_VERBATIM([_GNU_SOURCE],
                       [/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# undef _GNU_SOURCE
#endif])
           AC_DEFINE([_GNU_SOURCE])])

    # Do we want code coverage
    if test "$WANT_COVERAGE" = "1"; then
        if test "$ompi_c_vendor" = "gnu" ; then
            # For compilers > gcc-4.x, use --coverage for
            # compiling and linking to circumvent trouble with
            # libgcov.
            CFLAGS_orig="$CFLAGS"
            LDFLAGS_orig="$LDFLAGS"

            CFLAGS="$CFLAGS_orig --coverage"
            LDFLAGS="$LDFLAGS_orig --coverage"
            OMPI_COVERAGE_FLAGS=

            AC_CACHE_CHECK([if $CC supports --coverage],
                      [ompi_cv_cc_coverage],
                      [AC_TRY_COMPILE([], [],
                                      [ompi_cv_cc_coverage="yes"],
                                      [ompi_cv_cc_coverage="no"])])

            if test "$ompi_cv_cc_coverage" = "yes" ; then
                OMPI_COVERAGE_FLAGS="--coverage"
                CLEANFILES="*.gcno ${CLEANFILES}"
                CONFIG_CLEAN_FILES="*.gcda *.gcov ${CONFIG_CLEAN_FILES}"
            else
                OMPI_COVERAGE_FLAGS="-ftest-coverage -fprofile-arcs"
                CLEANFILES="*.bb *.bbg ${CLEANFILES}"
                CONFIG_CLEAN_FILES="*.da *.*.gcov ${CONFIG_CLEAN_FILES}"
            fi
            CFLAGS="$CFLAGS_orig $OMPI_COVERAGE_FLAGS"
            LDFLAGS="$LDFLAGS_orig $OMPI_COVERAGE_FLAGS"
            OPAL_WRAPPER_FLAGS_ADD([CFLAGS], [$OMPI_COVERAGE_FLAGS])
            OPAL_WRAPPER_FLAGS_ADD([LDFLAGS], [$OMPI_COVERAGE_FLAGS])

            OPAL_FLAGS_UNIQ(CFLAGS)
            OPAL_FLAGS_UNIQ(LDFLAGS)
            AC_MSG_WARN([$OMPI_COVERAGE_FLAGS has been added to CFLAGS (--enable-coverage)])

            WANT_DEBUG=1
        else
            AC_MSG_WARN([Code coverage functionality is currently available only with GCC])
            AC_MSG_ERROR([Configure: Cannot continue])
       fi
    fi

    # Do we want debugging?
    if test "$WANT_DEBUG" = "1" -a "$enable_debug_symbols" != "no" ; then
        if test "$ompi_c_vendor" = "gnu"; then
            CFLAGS="$CFLAGS -g"  # keep the -g3 for when it will become a standard option.
        else
            CFLAGS="$CFLAGS -g"
        fi

        OPAL_FLAGS_UNIQ(CFLAGS)
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
        # CFLAGS="$CFLAGS -Wno-long-double"
        # Starting with GCC-4.4, the compiler complains about not
        # knowing -Wno-long-double, only if -Wstrict-prototypes is set, too.
        #
        # Actually, this is not real fix, as GCC will pass on any -Wno- flag,
        # have fun with the warning: -Wno-britney
        CFLAGS="$CFLAGS $add -Wno-long-double -Wstrict-prototypes"

        AC_CACHE_CHECK([if $CC supports -Wno-long-double],
            [ompi_cv_cc_wno_long_double],
            [AC_TRY_COMPILE([], [], 
                [
                 dnl So -Wno-long-double did not produce any errors...
                 dnl We will try to extract a warning regarding 
                 dnl unrecognized or ignored options
                 AC_TRY_COMPILE([], [long double test;], 
                     [
                      ompi_cv_cc_wno_long_double="yes"
                      if test -s conftest.err ; then
                          dnl Yes, it should be "ignor", in order to catch ignoring and ignore
                          for i in unknown invalid ignor unrecognized ; do
                              $GREP -iq $i conftest.err
                              if test "$?" = "0" ; then
                                  ompi_cv_cc_wno_long_double="no"
                                  break;
                              fi
                          done
                      fi
                     ],
                     [ompi_cv_cc_wno_long_double="no"])],
                [ompi_cv_cc_wno_long_double="no"])
            ])

        CFLAGS="$CFLAGS_orig"
        if test "$ompi_cv_cc_wno_long_double" = "yes" ; then
            add="$add -Wno-long-double"
        fi

        add="$add -Werror-implicit-function-declaration "

        CFLAGS="$CFLAGS $add"
        OPAL_FLAGS_UNIQ(CFLAGS)
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

        CFLAGS_orig="$CFLAGS"
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

        OPAL_FLAGS_UNIQ(CFLAGS)
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
        if test "$ompi_cv_cc_restrict_cflags" = "yes" ; then
            add=" $RESTRICT_CFLAGS"
        fi

        CFLAGS="${CFLAGS_orig}${add}"
        OPAL_FLAGS_UNIQ([CFLAGS])
        if test "$add" != "" ; then
            AC_MSG_WARN([$add has been added to CFLAGS])
        fi
        unset add
    fi

    # see if the C compiler supports __builtin_expect
    AC_CACHE_CHECK([if $CC supports __builtin_expect],
        [ompi_cv_cc_supports___builtin_expect],
        [AC_TRY_LINK([],
          [void *ptr = (void*) 0;
           if (__builtin_expect (ptr != (void*) 0, 1)) return 0;],
          [ompi_cv_cc_supports___builtin_expect="yes"],
          [ompi_cv_cc_supports___builtin_expect="no"])])
    if test "$ompi_cv_cc_supports___builtin_expect" = "yes" ; then
        have_cc_builtin_expect=1
    else
        have_cc_builtin_expect=0
    fi
    AC_DEFINE_UNQUOTED([OPAL_C_HAVE_BUILTIN_EXPECT], [$have_cc_builtin_expect],
        [Whether C compiler supports __builtin_expect])

    # see if the C compiler supports __builtin_prefetch
    AC_CACHE_CHECK([if $CC supports __builtin_prefetch],
        [ompi_cv_cc_supports___builtin_prefetch],
        [AC_TRY_LINK([],
          [int ptr;
           __builtin_prefetch(&ptr,0,0);],
          [ompi_cv_cc_supports___builtin_prefetch="yes"],
          [ompi_cv_cc_supports___builtin_prefetch="no"])])
    if test "$ompi_cv_cc_supports___builtin_prefetch" = "yes" ; then
        have_cc_builtin_prefetch=1
    else
        have_cc_builtin_prefetch=0
    fi
    AC_DEFINE_UNQUOTED([OPAL_C_HAVE_BUILTIN_PREFETCH], [$have_cc_builtin_prefetch],
        [Whether C compiler supports __builtin_prefetch])

    # see if the C compiler supports __builtin_clz
    AC_CACHE_CHECK([if $CC supports __builtin_clz],
        [ompi_cv_cc_supports___builtin_clz],
        [AC_TRY_LINK([],
            [int value = 0xffff; /* we know we have 16 bits set */
             if ((8*sizeof(int)-16) != __builtin_clz(value)) return 0;],
            [ompi_cv_cc_supports___builtin_clz="yes"],
            [ompi_cv_cc_supports___builtin_clz="no"])])
    if test "$ompi_cv_cc_supports___builtin_clz" = "yes" ; then
        have_cc_builtin_clz=1
    else
        have_cc_builtin_clz=0
    fi
    AC_DEFINE_UNQUOTED([OPAL_C_HAVE_BUILTIN_CLZ], [$have_cc_builtin_clz],
        [Whether C compiler supports __builtin_clz])

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

    OMPI_ENSURE_CONTAINS_OPTFLAGS("$OMPI_CFLAGS_BEFORE_PICKY")
    OMPI_CFLAGS_BEFORE_PICKY="$co_result"

    AC_MSG_CHECKING([for C optimization flags])
    OMPI_ENSURE_CONTAINS_OPTFLAGS(["$CFLAGS"])
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
    OPAL_VAR_SCOPE_PUSH([ompi_cflags_save dummy ompi_cc_arvgv0])
    ompi_cflags_save="$CFLAGS"
    AC_PROG_CC
    BASECC="`basename $CC`"
    CFLAGS="$ompi_cflags_save"
    AC_DEFINE_UNQUOTED(OPAL_CC, "$CC", [OMPI underlying C compiler])
    set dummy $CC
    ompi_cc_argv0=[$]2
    OPAL_WHICH([$ompi_cc_argv0], [OPAL_CC_ABSOLUTE])
    AC_SUBST(OPAL_CC_ABSOLUTE)
    OPAL_VAR_SCOPE_POP
])
