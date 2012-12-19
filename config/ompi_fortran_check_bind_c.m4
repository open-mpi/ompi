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
dnl Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# Check whether or not the Fortran compiler supports BIND(C) or not

# OMPI_FORTRAN_CHECK_BIND_C([action if found], [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_BIND_C],[
    AS_VAR_PUSHDEF([bind_c_var], [ompi_cv_fortran_have_bind_c])

    AC_CACHE_CHECK([if Fortran compiler supports BIND(C)], bind_c_var,
       [AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[program check_for_bind_c
   use, intrinsic :: iso_c_binding
   type CType
      integer(C_INT) :: i
   end type
end program]])],
             [AS_VAR_SET(bind_c_var, yes)],
             [AS_VAR_SET(bind_c_var, no)])
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(bind_c_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([bind_c_var])dnl
])
