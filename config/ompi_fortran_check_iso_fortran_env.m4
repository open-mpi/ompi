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
dnl Copyright (c) 2020      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# Check whether or not the Fortran compiler supports iso_fortran_env or not
#
# OMPI_FORTRAN_CHECK_ISO_FORTRAN_ENV([action if found], [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_ISO_FORTRAN_ENV],[
    AS_VAR_PUSHDEF([iso_fortran_env_var], [ompi_cv_fortran_have_iso_fortran_env])
    OPAL_VAR_SCOPE_PUSH([iso_fortran_env_kind])

    AC_CACHE_CHECK([if Fortran compiler supports ISO_FORTRAN_ENV], iso_fortran_env_var,
       [AS_IF([test $OMPI_HAVE_FORTRAN_REAL16 -eq 1],
              [iso_fortran_env_kind=real128],
              [iso_fortran_env_kind=real32])
        AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[program check_for_iso_fortran_env
   use, intrinsic :: iso_fortran_env
   real($iso_fortran_env_kind) :: var
   var = 12.34
end program]])],
             [AS_VAR_SET(iso_fortran_env_var, yes)],
             [AS_VAR_SET(iso_fortran_env_var, no)])
        AC_LANG_POP([Fortran])
       ])

    OPAL_VAR_SCOPE_POP
    AS_VAR_IF(iso_fortran_env_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([iso_fortran_env_var])
])
