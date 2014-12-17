dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2014      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# Check whether or not the Fortran compiler implements
# TS 29113 subclause 8.1:
# Removed restrictions on ISO_C_BINDING module procedures.

# OMPI_FORTRAN_CHECK_C_FUNLOC([action if found],
#                             [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_C_FUNLOC],[
    AS_VAR_PUSHDEF([c_funloc], [ompi_cv_fortran_c_funloc])

    AC_CACHE_CHECK([if Fortran compiler supports C_FUNLOC/TS 29113], c_funloc,
       [AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[program main
abstract interface
  subroutine MPI_Comm_errhandler_function(comm, error_code)
    implicit none
    integer :: comm, error_code
  end subroutine
end interface

contains

subroutine  mysub(fn)
    use, intrinsic :: iso_c_binding, only : c_funloc, c_funptr
    procedure(MPI_Comm_errhandler_function) :: comm_errhandler_fn
    type(c_funptr) :: comm_errhandler_fn_c
    comm_errhandler_fn_c = c_funloc(comm_errhandler_fn)
end subroutine mysub
end program]])],
             [AS_VAR_SET(c_funloc, yes)],
             [AS_VAR_SET(c_funloc, no)])
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(c_funloc, [yes], [$1], [$2])
    AS_VAR_POPDEF([c_funloc])dnl
])
