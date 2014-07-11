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

# Check whether or not the Fortran compiler supports iso_c_binding or not
#
# OMPI_FORTRAN_CHECK_ISO_C_BINDING([action if found], [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_ISO_C_BINDING],[
    AS_VAR_PUSHDEF([iso_c_binding_var], [ompi_cv_fortran_have_iso_c_binding])

    AC_CACHE_CHECK([if Fortran compiler supports ISO_C_BINDING], iso_c_binding_var,
       [AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[program check_for_iso_c_binding
   use, intrinsic :: iso_c_binding
   type CType
      integer(C_INT) :: i
   end type
end program]])],
             [AS_VAR_SET(iso_c_binding_var, yes)],
             [AS_VAR_SET(iso_c_binding_var, no)])
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(iso_c_binding_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([iso_c_binding_var])
])

# Check for SUBROUTINE ... BIND(C)
# OMPI_FORTRAN_CHECK_BIND_C_SUB([action if found], [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_BIND_C_SUB],[
    AS_VAR_PUSHDEF([bind_c_sub_var], [ompi_cv_fortran_have_bind_c_sub])

    AC_CACHE_CHECK([if Fortran compiler supports SUBROUTINE BIND(C)], bind_c_sub_var,
       [AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[program check_for_bind_c_sub
   use, intrinsic :: iso_c_binding
   interface
       subroutine foo(i) bind(c)
           integer :: i
       end subroutine foo
   end interface
end program]])],
             [AS_VAR_SET(bind_c_sub_var, yes)],
             [AS_VAR_SET(bind_c_sub_var, no)])
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(bind_c_sub_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([bind_c_sub_var])
])

# Check for TYPE, BIND(C) :: derived_type
# OMPI_FORTRAN_CHECK_BIND_C_TYPE([action if found], [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_BIND_C_TYPE],[
    AS_VAR_PUSHDEF([bind_c_type_var], [ompi_cv_fortran_have_bind_c_type])

    AC_CACHE_CHECK([if Fortran compiler supports TYPE, BIND(C)], bind_c_type_var,
       [AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[module bindc_test
   use, intrinsic :: iso_c_binding
   type, bind(c) :: foo
       integer :: value
   end type foo
end module]])],
             [AS_VAR_SET(bind_c_type_var, yes)],
             [AS_VAR_SET(bind_c_type_var, no)])
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(bind_c_type_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([bind_c_type_var])dnl
])

# Check for TYPE(type), BIND(C, name="name")
# OMPI_FORTRAN_CHECK_BIND_C_TYPE_NAME([action if found], [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_BIND_C_TYPE_NAME],[
    AS_VAR_PUSHDEF([bind_c_type_name_var], [ompi_cv_fortran_have_bind_c_type_name])

    # See comment in ompi_setup_mpi_fortran.m4: it is important that
    # the bind(c) name in this text is longer than 32 characters.

    AC_CACHE_CHECK([if Fortran compiler supports TYPE(type), BIND(C, NAME="name")], bind_c_type_name_var,
       [AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[module bindc_test
   use, intrinsic :: iso_c_binding
   type, bind(c) :: foo
       integer :: value
   end type foo

   type(foo), bind(c, name="really_long_name_longer_than_32_chars") :: bar
end module]])],
             [AS_VAR_SET(bind_c_type_name_var, yes)],
             [AS_VAR_SET(bind_c_type_name_var, no)])
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(bind_c_type_name_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([bind_c_type_name_var])dnl
])
