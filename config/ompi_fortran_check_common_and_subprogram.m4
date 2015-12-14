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

dnl OMPI_FORTRAN_CHECK_COMMON_AND_SUBPROGRAM([action if supported],
dnl                                [action if not supported])
dnl ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_COMMON_AND_SUBPROGRAM],[
    AS_VAR_PUSHDEF([common_and_subprogram_var], [ompi_cv_fortran_common_and_subprogram])

    AC_CACHE_CHECK([if Fortran compiler supports both common and subprogram], common_and_subprogram_var,
       [AC_LANG_PUSH([Fortran])
        cat > aaa.f90 << EOF
MODULE aaa
INTEGER :: CMON(1)
COMMON/CMMON/CMON
INTEGER :: global_aaa
END MODULE aaa
EOF
        OPAL_LOG_COMMAND([$FC $FCFLAGS -c aaa.f90],
                         [AC_COMPILE_IFELSE([AC_LANG_SOURCE([[ MODULE bbb
integer, bind(C, name="cmmon_") :: CMON
INTEGER :: global_bbb
END MODULE bbb

PROGRAM test
USE aaa, ONLY : global_aaa
USE bbb, ONLY : global_bbb
implicit none
END PROGRAM]])],
                                            [AS_VAR_SET(common_and_subprogram_var, yes)],
                                            [AS_VAR_SET(common_and_subprogram_var, no)])],
                         [AS_VAR_SET(common_and_subprogram_var, no)])
        rm -rf aaa.f90 aaa.o *.mod 2>/dev/null
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(common_and_subprogram_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([common_and_subprogram_var])dnl
])
