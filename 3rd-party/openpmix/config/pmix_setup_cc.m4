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
dnl Copyright (c) 2008-2015 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2012-2017 Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2015-2019 Research Organization for Information Science
dnl                         and Technology (RIST).  All rights reserved.
dnl Copyright (c) 2018-2020 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2020-2023 Triad National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2021      IBM Corporation.  All rights reserved.
dnl
dnl Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([PMIX_CC_HELPER],[
    PMIX_VAR_SCOPE_PUSH([pmix_cc_helper_result])
    AC_MSG_CHECKING([$1])

    AC_LINK_IFELSE([AC_LANG_PROGRAM([$3],[$4])],
                   [$2=1
                    pmix_cc_helper_result=yes],
                   [$2=0
                    pmix_cc_helper_result=no])

    AC_MSG_RESULT([$pmix_cc_helper_result])
    PMIX_VAR_SCOPE_POP
])


AC_DEFUN([PMIX_PROG_CC_C11_HELPER],[
    PMIX_VAR_SCOPE_PUSH([pmix_prog_cc_c11_helper_CFLAGS_save])

    pmix_prog_cc_c11_helper_CFLAGS_save=$CFLAGS
    CFLAGS="$CFLAGS $1"

    PMIX_CC_HELPER([if $CC $1 supports C11 _Thread_local], [pmix_prog_cc_c11_helper__Thread_local_available],
                   [],[[static _Thread_local int  foo = 1;++foo;]])

    PMIX_CC_HELPER([if $CC $1 supports C11 atomic variables], [pmix_prog_cc_c11_helper_atomic_var_available],
                   [[#include <stdatomic.h>]], [[static atomic_long foo = 1;++foo;]])

    PMIX_CC_HELPER([if $CC $1 supports C11 _Atomic keyword], [pmix_prog_cc_c11_helper__Atomic_available],
                   [[#include <stdatomic.h>]],[[static _Atomic long foo = 1;++foo;]])

   PMIX_CC_HELPER([if $CC $1 supports C11 _c11_atomic functions], [pmix_prog_cc_c11_atomic_function],
                   [[#include <stdatomic.h>]],[[atomic_int acnt = 0; __c11_atomic_fetch_add(&acnt, 1, memory_order_relaxed);]])
   if test $pmix_prog_cc_c11_atomic_function -eq 1; then
      AC_DEFINE_UNQUOTED([PMIX_HAVE_CLANG_BUILTIN_ATOMIC_C11_FUNC], [$pmix_prog_cc_c11_atomic_function], [Whether we have Clang __c11 atomic functions])
   fi;

    PMIX_CC_HELPER([if $CC $1 supports C11 _Generic keyword], [pmix_prog_cc_c11_helper__Generic_available],
                   [[#define FOO(x) (_Generic (x, int: 1))]], [[static int x, y; y = FOO(x);]])

    PMIX_CC_HELPER([if $CC $1 supports C11 _Static_assert], [pmix_prog_cc_c11_helper__static_assert_available],
                   [[#include <stdint.h>]],[[_Static_assert(sizeof(int64_t) == 8, "WTH");]])

    PMIX_CC_HELPER([if $CC $1 supports C11 atomic_fetch_xor_explicit], [pmix_prog_cc_c11_helper_atomic_fetch_xor_explicit_available],
	           [[#include <stdatomic.h>
#include <stdint.h>]],[[_Atomic uint32_t a; uint32_t b; atomic_fetch_xor_explicit(&a, b, memory_order_relaxed);]])


    AS_IF([test $pmix_prog_cc_c11_helper__Thread_local_available -eq 1 && test $pmix_prog_cc_c11_helper_atomic_var_available -eq 1 && test $pmix_prog_cc_c11_helper_atomic_fetch_xor_explicit_available -eq 1],
          [$2],
          [$3])

    CFLAGS=$pmix_prog_cc_c11_helper_CFLAGS_save

    PMIX_VAR_SCOPE_POP
])

AC_DEFUN([PMIX_PROG_CC_C11],[
    PMIX_VAR_SCOPE_PUSH([pmix_prog_cc_c11_flags])
    if test -z "$pmix_cv_c11_supported" ; then
        pmix_cv_c11_supported=no
        pmix_cv_c11_flag_required=yes

        AC_MSG_CHECKING([if $CC requires a flag for C11])

        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#if __STDC_VERSION__ < 201112L
#error "Without any CLI flags, this compiler does not support C11"
#endif
                                           ]],[])],
                          [pmix_cv_c11_flag_required=no])

        AC_MSG_RESULT([$pmix_cv_c11_flag_required])

        if test "x$pmix_cv_c11_flag_required" = "xno" ; then
            AC_MSG_NOTICE([verifying $CC supports C11 without a flag])
            PMIX_PROG_CC_C11_HELPER([], [], [pmix_cv_c11_flag_required=yes])
        fi

        if test "x$pmix_cv_c11_flag_required" = "xyes" ; then
            pmix_prog_cc_c11_flags="-std=gnu11 -std=c11 -c11"

            AC_MSG_NOTICE([checking if $CC supports C11 with a flag])
            pmix_cv_c11_flag=
            for flag in $(echo $pmix_prog_cc_c11_flags | tr ' ' '\n') ; do
                PMIX_PROG_CC_C11_HELPER([$flag],[pmix_cv_c11_flag=$flag],[])
                if test "x$pmix_cv_c11_flag" != "x" ; then
                    PMIX_APPEND_UNIQ([CFLAGS], ["$pmix_cv_c11_flag"])
                    AC_MSG_NOTICE([using $flag to enable C11 support])
                    pmix_cv_c11_supported=yes
                    break
                fi
            done
        else
            AC_MSG_NOTICE([no flag required for C11 support])
            pmix_cv_c11_supported=yes
        fi
    fi

    PMIX_VAR_SCOPE_POP
])


# PMIX_CHECK_CC_IQUOTE()
# ----------------------
# Check if the compiler supports the -iquote option. This options
# removes the specified directory from the search path when using
# #include <>. This check works around an issue caused by C++20
# which added a <version> header. This conflicts with the
# VERSION file at the base of our source directory on case-
# insensitive filesystems.
AC_DEFUN([PMIX_CHECK_CC_IQUOTE],[
    PMIX_VAR_SCOPE_PUSH([pmix_check_cc_iquote_CFLAGS_save])
    pmix_check_cc_iquote_CFLAGS_save=${CFLAGS}
    CFLAGS="${CFLAGS} -iquote ."
    AC_MSG_CHECKING([for $CC option to add a directory only to the search path for the quote form of include])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]],[])],
              [pmix_cc_iquote="-iquote"],
              [pmix_cc_iquote="-I"])
    CFLAGS=${pmix_check_cc_iquote_CFLAGS_save}
    PMIX_VAR_SCOPE_POP
    AC_MSG_RESULT([$pmix_cc_iquote])
])


# PMIX_SETUP_CC()
# ---------------
# Do everything required to setup the C compiler.  Safe to AC_REQUIRE
# this macro.
AC_DEFUN([PMIX_SETUP_CC],[
    # AM_PROG_CC_C_O AC_REQUIREs AC_PROG_CC, so we have to be a little
    # careful about ordering here, and AC_REQUIRE these things so that
    # they get stamped out in the right order.

    AC_REQUIRE([_PMIX_START_SETUP_CC])
    AC_REQUIRE([_PMIX_PROG_CC])
    AC_REQUIRE([AM_PROG_CC_C_O])

    PMIX_VAR_SCOPE_PUSH([pmix_prog_cc_c11_helper__Thread_local_available pmix_prog_cc_c11_helper_atomic_var_available pmix_prog_cc_c11_helper__Atomic_available pmix_prog_cc_c11_helper__static_assert_available pmix_prog_cc_c11_helper__Generic_available pmix_prog_cc__thread_available pmix_prog_cc_c11_helper_atomic_fetch_xor_explicit_available pmix_prog_cc_c11_atomic_function])

    # AC_PROG_CC_C99 changes CC (instead of CFLAGS) so save CC (without c99
    # flags) for use in our wrappers.
    WRAPPER_CC="$CC"
    AC_SUBST([WRAPPER_CC])

    PMIX_PROG_CC_C11
    PMIX_CHECK_CC_IQUOTE

    if test $pmix_cv_c11_supported = no ; then
        # It is not currently an error if C11 support is not available. Uncomment the
        # following lines and update the warning when we require a C11 compiler.
        # AC_MSG_WARNING([PMIx requires a C11 (or newer) compiler])
        # AC_MSG_ERROR([Aborting.])
        # We require a C99 compliant compiler
        # with autoconf 2.70 AC_PROG_CC makes AC_PROG_CC_C99 obsolete
        m4_version_prereq([2.70],
            [],
            [AC_PROG_CC_C99])
        # The C99 result of AC_PROG_CC is stored in ac_cv_prog_cc_c99
        if test "x$ac_cv_prog_cc_c99" = xno ; then
            AC_MSG_WARN([PMIx requires a C99 (or newer) compiler. C11 is recommended.])
            AC_MSG_ERROR([Aborting.])
        fi

        # Get the correct result for C11 support flags now that the compiler flags have
        # changed
        PMIX_PROG_CC_C11_HELPER([],[],[])
    fi

    # Check if compiler support __thread
    PMIX_CC_HELPER([if $CC $1 supports __thread], [pmix_prog_cc__thread_available],
                    [],[[static __thread int  foo = 1;++foo;]])


    PMIX_CC_HELPER([if $CC $1 supports C11 _Thread_local], [pmix_prog_cc_c11_helper__Thread_local_available],
                   [],[[static _Thread_local int  foo = 1;++foo;]])

    dnl At this time, PMIx only needs thread local and the atomic convenience types for C11 support. These
    dnl will likely be required in the future.
    AC_DEFINE_UNQUOTED([PMIX_C_HAVE__THREAD_LOCAL], [$pmix_prog_cc_c11_helper__Thread_local_available],
                       [Whether C compiler supports __Thread_local])

    AC_DEFINE_UNQUOTED([PMIX_C_HAVE_ATOMIC_CONV_VAR], [$pmix_prog_cc_c11_helper_atomic_var_available],
                       [Whether C compiler supports atomic convenience variables in stdatomic.h])

    AC_DEFINE_UNQUOTED([PMIX_C_HAVE__ATOMIC], [$pmix_prog_cc_c11_helper__Atomic_available],
                       [Whether C compiler supports __Atomic keyword])

    AC_DEFINE_UNQUOTED([PMIX_C_HAVE__GENERIC], [$pmix_prog_cc_c11_helper__Generic_available],
                       [Whether C compiler supports __Generic keyword])

    AC_DEFINE_UNQUOTED([PMIX_C_HAVE__STATIC_ASSERT], [$pmix_prog_cc_c11_helper__static_assert_available],
                       [Whether C compiler supports _Static_assert keyword])

    AC_DEFINE_UNQUOTED([PMIX_C_HAVE___THREAD], [$pmix_prog_cc__thread_available],
                       [Whether C compiler supports __thread])

    OAC_C_COMPILER_VENDOR()

    # GNU C and autotools are inconsistent about whether this is
    # defined so let's make it true everywhere for now...  However, IBM
    # XL compilers on PPC Linux behave really badly when compiled with
    # _GNU_SOURCE defined, so don't define it in that situation.
    #
    # Don't use AC_GNU_SOURCE because it requires that no compiler
    # tests are done before setting it, and we need to at least do
    # enough tests to figure out if we're using XL or not.
    AS_IF([test "$oac_cv_c_compiler_vendor" != "ibm"],
          [AH_VERBATIM([_GNU_SOURCE],
                       [/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# undef _GNU_SOURCE
#endif])
           AC_DEFINE([_GNU_SOURCE])])

    # Do we want code coverage
    if test "$WANT_COVERAGE" = "1"; then
        # For compilers > gcc-4.x, use --coverage for
        # compiling and linking to circumvent trouble with
        # libgcov.
        LDFLAGS_orig="$LDFLAGS"
        PMIX_APPEND_UNIQ([LDFLAGS], ["$--coverage"])
        PMIX_COVERAGE_FLAGS=

        _PMIX_CHECK_SPECIFIC_CFLAGS(--coverage, coverage)
        if test "$pmix_cv_cc_coverage" = "1" ; then
            PMIX_COVERAGE_FLAGS="--coverage"
            CLEANFILES="*.gcno ${CLEANFILES}"
            CONFIG_CLEAN_FILES="*.gcda *.gcov ${CONFIG_CLEAN_FILES}"
            AC_MSG_WARN([$PMIX_COVERAGE_FLAGS has been added to CFLAGS (--enable-coverage)])
        else
            _PMIX_CHECK_SPECIFIC_CFLAGS(-ftest-coverage, ftest_coverage)
            _PMIX_CHECK_SPECIFIC_CFLAGS(-fprofile-arcs, fprofile_arcs)
            if test "$pmix_cv_cc_ftest_coverage" = "0" || test "pmix_cv_cc_fprofile_arcs" = "0" ; then
                AC_MSG_WARN([Code coverage functionality is not currently available with $CC])
                AC_MSG_ERROR([Configure: Cannot continue])
            fi
            CLEANFILES="*.bb *.bbg ${CLEANFILES}"
            PMIX_COVERAGE_FLAGS="-ftest-coverage -fprofile-arcs"
        fi
        WANT_DEBUG=1
   fi

    # Do we want debugging?
    if test "$WANT_DEBUG" = "1" && test "$enable_debug_symbols" != "no" ; then
        PMIX_APPEND_UNIQ([CFLAGS], ["-g"])

        AC_MSG_WARN([-g has been added to CFLAGS (--enable-debug)])
    fi

    # These flags are generally gcc-specific; even the
    # gcc-impersonating compilers won't accept them.
    PMIX_CFLAGS_BEFORE_PICKY="$CFLAGS"

    # Note: Some versions of clang (at least >= 3.5 -- perhaps
    # older versions, too?) and xlc with -g (v16.1, perhaps older)
    # will *warn* about -finline-functions, but still allow it.
    # This is very annoying, so check for that warning, too.
    # The clang warning looks like this:
    # clang: warning: optimization flag '-finline-functions' is not supported
    # clang: warning: argument unused during compilation: '-finline-functions'
    # the xlc warning looks like this:
    # warning: "-qinline" is not compatible with "-g". "-qnoinline" is being set.
    _PMIX_CHECK_SPECIFIC_CFLAGS(-finline-functions, finline_functions)

    # Try to enable restrict keyword
    RESTRICT_CFLAGS=
    case "$oac_cv_c_compiler_vendor" in
        intel)
            RESTRICT_CFLAGS="-restrict"
        ;;
        sgi)
            RESTRICT_CFLAGS="-LANG:restrict=ON"
        ;;
    esac
    if test ! -z "$RESTRICT_CFLAGS" ; then
        _PMIX_CHECK_SPECIFIC_CFLAGS($RESTRICT_CFLAGS, restrict)
    fi

    AC_MSG_RESULT(CFLAGS result: $CFLAGS)

    # see if the C compiler supports __builtin_expect
    AC_CACHE_CHECK([if $CC supports __builtin_expect],
        [pmix_cv_cc_supports___builtin_expect],
        [AC_LINK_IFELSE([AC_LANG_PROGRAM([[ ]],
          [[void *ptr = (void*) 0;
           if (__builtin_expect (ptr != (void*) 0, 1)) return 0;
          ]])],
          [pmix_cv_cc_supports___builtin_expect="yes"],
          [pmix_cv_cc_supports___builtin_expect="no"])])
    if test "$pmix_cv_cc_supports___builtin_expect" = "yes" ; then
        have_cc_builtin_expect=1
    else
        have_cc_builtin_expect=0
    fi
    AC_DEFINE_UNQUOTED([PMIX_C_HAVE_BUILTIN_EXPECT], [$have_cc_builtin_expect],
        [Whether C compiler supports __builtin_expect])

    # see if the C compiler supports __builtin_prefetch
    AC_CACHE_CHECK([if $CC supports __builtin_prefetch],
        [pmix_cv_cc_supports___builtin_prefetch],
        [AC_LINK_IFELSE([AC_LANG_PROGRAM([
          [int ptr;
           __builtin_prefetch(&ptr,0,0);]],
          [pmix_cv_cc_supports___builtin_prefetch="yes"],
          [pmix_cv_cc_supports___builtin_prefetch="no"])])])
    if test "$pmix_cv_cc_supports___builtin_prefetch" = "yes" ; then
        have_cc_builtin_prefetch=1
    else
        have_cc_builtin_prefetch=0
    fi
    AC_DEFINE_UNQUOTED([PMIX_C_HAVE_BUILTIN_PREFETCH], [$have_cc_builtin_prefetch],
        [Whether C compiler supports __builtin_prefetch])

    # see if the C compiler supports __builtin_clz
    AC_CACHE_CHECK([if $CC supports __builtin_clz],
        [pmix_cv_cc_supports___builtin_clz],
        [AC_LINK_IFELSE([AC_LANG_PROGRAM([
            [int value = 0xffff; /* we know we have 16 bits set */
             if ((8*sizeof(int)-16) != __builtin_clz(value)) return 0;]],
            [pmix_cv_cc_supports___builtin_clz="yes"],
            [pmix_cv_cc_supports___builtin_clz="no"])])])
    if test "$pmix_cv_cc_supports___builtin_clz" = "yes" ; then
        have_cc_builtin_clz=1
    else
        have_cc_builtin_clz=0
    fi
    AC_DEFINE_UNQUOTED([PMIX_C_HAVE_BUILTIN_CLZ], [$have_cc_builtin_clz],
        [Whether C compiler supports __builtin_clz])

    # Preload the optflags for the case where the user didn't specify
    # any.  If we're using GNU compilers, use -O3 (since it GNU
    # doesn't require all compilation units to be compiled with the
    # same level of optimization -- selecting a high level of
    # optimization is not prohibitive).  If we're using anything else,
    # be conservative and just use -O.
    #
    if test "$WANT_DEBUG" = "1"; then
        OPTFLAGS=
    else
        if test "$GCC" = yes; then
            OPTFLAGS="-O3"
        else
            OPTFLAGS="-O"
        fi
    fi

    PMIX_ENSURE_CONTAINS_OPTFLAGS("$PMIX_CFLAGS_BEFORE_PICKY")
    PMIX_CFLAGS_BEFORE_PICKY="$co_result"

    AC_MSG_CHECKING([for C optimization flags])
    PMIX_ENSURE_CONTAINS_OPTFLAGS(["$CFLAGS"])
    AC_MSG_RESULT([$co_result])
    CFLAGS="$co_result"
    PMIX_VAR_SCOPE_POP
])


AC_DEFUN([_PMIX_START_SETUP_CC],[
    pmix_show_subtitle "C compiler and preprocessor"

	# $%@#!@#% AIX!!  This has to be called before anything invokes the C
    # compiler.
    dnl AC_AIX
])


AC_DEFUN([_PMIX_PROG_CC],[
    #
    # Check for the compiler
    #
    PMIX_VAR_SCOPE_PUSH([pmix_cflags_save dummy pmix_cc_arvgv0])
    pmix_cflags_save="$CFLAGS"
    AC_PROG_CC
    BASECC="`basename $CC`"
    CFLAGS="$pmix_cflags_save"
    AC_DEFINE_UNQUOTED(PMIX_CC, "$CC", [PMIx underlying C compiler])
    set dummy $CC
    pmix_cc_argv0=[$]2
    PMIX_WHICH([$pmix_cc_argv0], [PMIX_CC_ABSOLUTE])
    AC_SUBST(PMIX_CC_ABSOLUTE)
    PMIX_VAR_SCOPE_POP
])

AC_DEFUN([PMIX_SETUP_PICKY_COMPILERS],[
    if test $WANT_PICKY_COMPILER -eq 1 && test "$oac_cv_c_compiler_vendor" != "portland group"; then
        _PMIX_CHECK_SPECIFIC_CFLAGS(-Wundef, Wundef)
        _PMIX_CHECK_SPECIFIC_CFLAGS(-Wno-long-long, Wno_long_long, int main() { long long x; })
        _PMIX_CHECK_SPECIFIC_CFLAGS(-Wsign-compare, Wsign_compare)
        _PMIX_CHECK_SPECIFIC_CFLAGS(-Wmissing-prototypes, Wmissing_prototypes)
        _PMIX_CHECK_SPECIFIC_CFLAGS(-Wstrict-prototypes, Wstrict_prototypes)
        _PMIX_CHECK_SPECIFIC_CFLAGS(-Wcomment, Wcomment)
        _PMIX_CHECK_SPECIFIC_CFLAGS(-Wshadow, Wshadow)
        _PMIX_CHECK_SPECIFIC_CFLAGS(-Werror-implicit-function-declaration, Werror_implicit_function_declaration)
        _PMIX_CHECK_SPECIFIC_CFLAGS(-Wno-long-double, Wno_long_double, int main() { long double x; })
        _PMIX_CHECK_SPECIFIC_CFLAGS(-fno-strict-aliasing, fno_strict_aliasing, int main () { int x; })
        _PMIX_CHECK_SPECIFIC_CFLAGS(-pedantic, pedantic)
        _PMIX_CHECK_SPECIFIC_CFLAGS(-Wall, Wall)
        _PMIX_CHECK_SPECIFIC_CFLAGS(-Wextra, Wextra)
        _PMIX_CHECK_SPECIFIC_CFLAGS(-Werror, Werror)
        if test $WANT_MEMORY_SANITIZERS -eq 1 && test "$oac_cv_c_compiler_vendor" != "portland group"; then
            _PMIX_CHECK_SPECIFIC_CFLAGS(-fsanitize=address, fsanaddress)
            _PMIX_CHECK_SPECIFIC_CFLAGS(-fsanitize=undefined, fsanundefined)
        fi
    fi

])
