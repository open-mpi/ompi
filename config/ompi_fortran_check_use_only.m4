dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2010-2015 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl
dnl Check whether Fortran compiler supports the "only" clause properly
dnl when using modules.  Specifically, if we "use a :: only foo" and "use
dnl b :: only bar", and modules a and b have a conflicting "yow"
dnl definition, it *should* be ignored because of the "only" clauses.  PGI
dnl 15.7 (and probably prior versions) does not -- but only when
dnl compiling with -g (!).
dnl

dnl OMPI_FORTRAN_CHECK_USE_ONLY([action if supported],
dnl                                [action if not supported])
dnl ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_USE_ONLY],[
    AS_VAR_PUSHDEF([use_only_var], [ompi_cv_fortran_use_only])

    AC_CACHE_CHECK([if Fortran compiler supports USE...ONLY], use_only_var,
       [AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[MODULE aaa
INTEGER :: aaa_unique
COMMON/common_to_both/COMMON_NAME_TO_BOTH
END MODULE aaa

MODULE bbb
INTEGER :: bbb_unique
INTEGER, BIND(C, name="common_to_both_") :: COMMON_NAME_TO_BOTH
END MODULE bbb

PROGRAM test_proc
  USE :: aaa, ONLY: aaa_unique
  USE :: bbb, ONLY: bbb_unique
END PROGRAM]])],
             [AS_VAR_SET(use_only_var, yes)],
             [AS_VAR_SET(use_only_var, no)])
        touch conftest_foo.mod
        rm -rf *.mod 2>/dev/null
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(use_only_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([use_only_var])dnl
])
