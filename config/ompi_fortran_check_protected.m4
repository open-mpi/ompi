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

# Check whether or not the Fortran compiler supports the "protected"
# keyword in derived types or not.

# OMPI_FORTRAN_CHECK_PROTECTED([action if found], 
#                              [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_PROTECTED],[
    AS_VAR_PUSHDEF([protected_var], [ompi_cv_fortran_protected])

    AC_CACHE_CHECK([if Fortran compiler supports PROTECTED], protected_var,
       [AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[module test_for_protected
   integer, protected :: bogus
end module test_for_protected]])],
             [AS_VAR_SET(protected_var, yes)],
             [AS_VAR_SET(protected_var, no)])
        touch conftest_foo.mod
        rm -rf *.mod 2>/dev/null
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(protected_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([protected_var])dnl
])
