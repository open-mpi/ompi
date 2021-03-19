dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2021      Research Organization for Information Science
dnl                         and Technology (RIST).  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# Check whether or not the Fortran compiler supports the non standard
# iso_fortran_env:real16 or not
#
# OMPI_FORTRAN_CHECK_ISO_FORTRAN_ENV_REAL16([action if found], [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_ISO_FORTRAN_ENV_REAL16],[
    AS_VAR_PUSHDEF([iso_fortran_env_var_real16], [ompi_cv_fortran_have_iso_fortran_env_real16])

    AC_CACHE_CHECK([if Fortran compiler supports ISO_FORTRAN_ENV:REAL16], iso_fortran_env_var_real16,
       [AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[program check_for_iso_fortran_env_real16
   use, intrinsic :: iso_fortran_env
   real(REAL16) :: var
   var = 12.34
end program]])],
             [AS_VAR_SET(iso_fortran_env_var_real16, yes)],
             [AS_VAR_SET(iso_fortran_env_var_real16, no)])
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(iso_fortran_env_var_real16, [yes], [$1], [$2])
    AS_VAR_POPDEF([iso_fortran_env_var_real16])
])
