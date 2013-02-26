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
dnl Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# Per https://svn.open-mpi.org/trac/ompi/ticket/3523 and
# http://lists.mpi-forum.org/mpi-comments/2013/02/0076.php, the
# Fortran compiler may not support default LOGICAL parameters with
# BIND(C).  OMPI's F08 bindings currently require this.  
#
# So at least for the moment, test to see if the compiler allows
# default LOGICAL parameters with BIND(C).

# OMPI_FORTRAN_CHECK_BIND_C_LOGICAL([action if found], [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_BIND_C_LOGICAL],[
    AS_VAR_PUSHDEF([bind_c_var], [ompi_cv_fortran_bind_c_likes_logical])

    AC_CACHE_CHECK([if Fortran compiler supports BIND(C) with LOGICAL params], bind_c_var,
       [AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[module bindc_logical
   use, intrinsic :: iso_c_binding
   interface
       subroutine foo(flag) bind(c)
           logical :: flag
       end subroutine foo
   end interface
end module bindc_logical]])],
             [AS_VAR_SET(bind_c_var, yes)],
             [AS_VAR_SET(bind_c_var, no)])
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(bind_c_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([bind_c_var])dnl
])
