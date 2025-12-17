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
dnl Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2022-2025 Triad National Security, LLC. All rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# Check whether or not the Fortran compiler can build mpi_f08 using
# the given compiler options.  This test is intended to
# trap cases where default INTEGER KIND is equivalent to MPI_COUNT_KIND.

# OMPI_FORTRAN_CHECK_BIG_COUNT([action if found],
#                            [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_BIG_COUNT],[
    AS_VAR_PUSHDEF([big_count_var], [ompi_cv_big_count_var])

    AC_CACHE_CHECK([if Fortran compiler can compile interface containing MPI_KIND_COUNT arguments ], big_count_var,
       [AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[module sendit_interfaces
INTERFACE SendIt
 subroutine SendIt_int(X)
 INTEGER :: X
 end subroutine SendIt_int
 subroutine SendIt_big(x)
 integer(KIND=$OMPI_MPI_COUNT_KIND)::X
 end subroutine SendIt_big
 end interface SendIt
 end module sendit_interfaces]])],
             [AS_VAR_SET(big_count_var, yes)],
             [AS_VAR_SET(big_count_var, no)])
        touch conftest_foo.mod
        rm -rf *.mod 2>/dev/null
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(big_count_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([big_count_var])dnl
])
