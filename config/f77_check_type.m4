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
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_F77_CHECK_TYPE],[
# Determine FORTRAN datatype size.
# First arg is type, 2nd arg is config var to define

AC_MSG_CHECKING([if FORTRAN compiler supports $1])

AC_LANG_PUSH(Fortran 77)
AC_COMPILE_IFELSE(AC_LANG_SOURCE([[C
        program main
        $1 bogus_variable
        end]]), 
    [HAPPY=1 
     AC_MSG_RESULT([yes])],
    [HAPPY=0 
     AC_MSG_RESULT([no])])
AC_LANG_POP

str="$2=$HAPPY"
eval $str

unset HAPPY])dnl
