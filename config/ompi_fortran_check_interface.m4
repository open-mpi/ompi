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

# Check whether or not the Fortran compiler supports the "interface"
# keyword or not.

# OMPI_FORTRAN_CHECK_INTERFACE([action if found],
#                              [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_INTERFACE],[
    AS_VAR_PUSHDEF([interface_var], [ompi_cv_fortran_interface])

    AC_CACHE_CHECK([if Fortran compiler supports INTERFACE], interface_var,
       [AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[MODULE my_module
INTERFACE MPI_Foo
SUBROUTINE inky(a)
  DOUBLE PRECISION A
END SUBROUTINE inky
SUBROUTINE blinky(a)
  INTEGER A
END SUBROUTINE blinky
END INTERFACE MPI_Foo
END MODULE my_module]])],
             [AS_VAR_SET(interface_var, yes)],
             [AS_VAR_SET(interface_var, no)])
        touch conftest_foo.mod
        rm -rf *.mod 2>/dev/null
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(interface_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([interface_var])dnl
])
