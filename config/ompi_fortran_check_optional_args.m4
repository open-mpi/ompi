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
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# Check whether or not the Fortran compiler supports the "optional"
# keyword or not.

# OMPI_FORTRAN_CHECK_OPTIONAL_ARGS([action if found],
#                                  [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_OPTIONAL_ARGS],[
    AS_VAR_PUSHDEF([optional_var], [ompi_cv_fortran_optional])

    AC_CACHE_CHECK([if Fortran compiler supports optional arguments], optional_var,
       [AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[program check_for_optional
   use, intrinsic :: iso_c_binding

   interface
      subroutine check_op(i, ierror)
         integer, intent(in), value :: i
         integer, intent(out), optional :: ierror
      end subroutine check_op
   end interface

   integer :: ierror

   call check_op(0)
   call check_op(1, ierror)
end program]])],
             [AS_VAR_SET(optional_var, yes)],
             [AS_VAR_SET(optional_var, no)])
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(optional_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([optional_var])dnl
])
