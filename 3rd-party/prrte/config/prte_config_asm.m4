dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2018 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2008-2018 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
dnl Copyright (c) 2015-2018 Research Organization for Information Science
dnl                         and Technology (RIST).  All rights reserved.
dnl Copyright (c) 2014-2018 Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2017-2021 Amazon.com, Inc. or its affiliates.  All Rights
dnl                         reserved.
dnl Copyright (c) 2020      Google, LLC. All rights reserved.
dnl Copyright (c) 2020      Intel, Inc.  All rights reserved.
dnl Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


AC_DEFUN([PRTE_CHECK_GCC_ATOMIC_BUILTINS], [
  if test -z "$prte_cv_have___atomic" ; then
    AC_MSG_CHECKING([for 32-bit GCC built-in atomics])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <stdint.h>
]],[[
uint32_t tmp, old = 0;
uint64_t tmp64, old64 = 0;
__atomic_thread_fence(__ATOMIC_SEQ_CST);
__atomic_compare_exchange_n(&tmp, &old, 1, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
__atomic_add_fetch(&tmp, 1, __ATOMIC_RELAXED);
__atomic_compare_exchange_n(&tmp64, &old64, 1, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
__atomic_add_fetch(&tmp64, 1, __ATOMIC_RELAXED);]])],
        [prte_cv_have___atomic=yes],
        [prte_cv_have___atomic=no])
    AC_MSG_RESULT([$prte_cv_have___atomic])

    if test "$prte_cv_have___atomic" = "yes" ; then
      AC_MSG_CHECKING([for 64-bit GCC built-in atomics])
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[
#include <stdint.h>
]],[[
uint64_t tmp64, old64 = 0;
__atomic_compare_exchange_n(&tmp64, &old64, 1, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
__atomic_add_fetch(&tmp64, 1, __ATOMIC_RELAXED);]])],
            [prte_cv_have___atomic_64=yes],
            [prte_cv_have___atomic_64=no])
      AC_MSG_RESULT([$prte_cv_have___atomic_64])
      if test "$prte_cv_have___atomic_64" = "yes" ; then
        AC_MSG_CHECKING([if 64-bit GCC built-in atomics are lock-free])
        AC_RUN_IFELSE([AC_LANG_PROGRAM([], [if (!__atomic_is_lock_free (8, 0)) { return 1; }])],
              [AC_MSG_RESULT([yes])],
              [AC_MSG_RESULT([no])
               prte_cv_have___atomic_64=no],
              [AC_MSG_RESULT([cannot test -- assume yes (cross compiling)])])
      fi
    else
      prte_cv_have___atomic_64=no
    fi
  fi
])

dnl #################################################################
dnl
dnl PRTE_CONFIG_ASM()
dnl
dnl DEFINE PRTE_ATOMIC_C11 if C11 atomics should be used
dnl DEFINE PRTE_ATOMIC_GCC_BUILTIN if gcc builtin atomics should be used
dnl DEFINE PRTE_ATOMIC_X86_64 if we're on an x86_64 platform
dnl
dnl #################################################################
AC_DEFUN([PRTE_CONFIG_ASM],[
    AC_REQUIRE([PRTE_SETUP_CC])

    AC_ARG_ENABLE([c11-atomics],[AS_HELP_STRING([--enable-c11-atomics],
                  [Enable use of C11 atomics if available (default: enabled)])])
    AC_ARG_ENABLE([builtin-atomics],
      [AS_HELP_STRING([--enable-builtin-atomics],
         [Enable use of GCC built-in atomics (default: autodetect)])])

    prte_atomic_c11=0
    prte_atomic_gcc_builtin=0

    PRTE_CHECK_GCC_ATOMIC_BUILTINS
    if test "x$enable_c11_atomics" != "xno" && test "$prte_cv_c11_supported" = "yes" ; then
        prte_atomic_style="c11"
        prte_atomic_c11=1
    elif test "x$enable_c11_atomics" = "xyes"; then
        AC_MSG_WARN([C11 atomics were requested but are not supported])
        AC_MSG_ERROR([Cannot continue])
    elif test "$enable_builtin_atomics" != "no" && test "$prte_cv_have___atomic" = "yes" ; then
        prte_atomic_style="gcc"
        prte_atomic_gcc_builtin=1
    elif test "$enable_builtin_atomics" = "yes" ; then
        AC_MSG_WARN([GCC built-in atomics requested but not found.])
        AC_MSG_ERROR([Cannot continue])
    else
        AC_MSG_WARN([Neither C11 nor the built-in atomics are available,])
        AC_MSG_WARN([either because they were disabled on the configure])
        AC_MSG_WARN([command line or they were not found. PRRTE requires])
        AC_MSG_WARN([atomic support, so either a compiler with C11 atomics])
        AC_MSG_WARN([must be used OR the built-in atomics must not be disabled.])
        AC_MSG_ERROR([Cannot continue])
    fi

    AC_MSG_CHECKING([for x86_64 architecture])
    case "${host}" in
    x86_64-*x32|i?86-*|x86_64*|amd64*)
        AC_MSG_RESULT([yes])
        AC_DEFINE([PRTE_ATOMIC_X86_64], [1], [whether building on x86_64 platform])
        ;;
    *)
        AC_MSG_RESULT([no])
        ;;
    esac

    AC_MSG_CHECKING([for atomics style])
    AC_MSG_RESULT([$prte_atomic_style])

    AC_DEFINE_UNQUOTED([PRTE_ATOMIC_C11], [$prte_atomic_c11],
        [Use C11 style atomics])

    AC_DEFINE_UNQUOTED([PRTE_ATOMIC_GCC_BUILTIN], [$prte_atomic_gcc_builtin],
        [Use GCC builtin style atomics])
])dnl
