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

# Check whether or not the Fortran compiler supports the "procedure"
# keyword in derived types or not.

# OMPI_FORTRAN_CHECK_PROCEDURE([action if found], 
#                            [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_PROCEDURE],[
    AS_VAR_PUSHDEF([procedure_var], [ompi_cv_fortran_procedure])

    AC_CACHE_CHECK([if Fortran compiler supports PROCEDURE], procedure_var,
       [AC_LANG_PUSH([Fortran])
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[MODULE proc_mod
INTERFACE
SUBROUTINE MPI_User_function
END SUBROUTINE
END INTERFACE
END MODULE proc_mod

PROGRAM test_proc
INTERFACE
SUBROUTINE binky(user_fn)
  USE proc_mod
  PROCEDURE(MPI_User_function) :: user_fn
END SUBROUTINE
END INTERFACE
END PROGRAM]])],
             [AS_VAR_SET(procedure_var, yes)],
             [AS_VAR_SET(procedure_var, no)])
        touch conftest_foo.mod
        rm -rf *.mod 2>/dev/null
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_IF(procedure_var, [yes], [$1], [$2])
    AS_VAR_POPDEF([procedure_var])dnl
])
