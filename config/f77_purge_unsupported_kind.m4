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

AC_DEFUN([OMPI_F77_PURGE_UNSUPPORTED_KIND],[
# Purge F77 types (such as INTEGER*16) that don't match exptected datatype size.
# First arg is type, 2nd arg is config var to define.
AC_MSG_CHECKING([whether Fortran $1 has expected size])

  val=`echo $1 | cut -f2 -d'*'` 
  type=`echo $1 | cut -f1 -d'*'`
  if test "x$((OMPI_SIZEOF_FORTRAN_$type$val))" != "x$val" ; then
    eval "OMPI_SIZEOF_FORTRAN_$type$val=0"
    # eval "OMPI_ALIGNMENT_FORTRAN_$type$val=0"
    eval "OMPI_HAVE_FORTRAN_$type$val=0"
    AC_MSG_RESULT([no])
  else
    AC_MSG_RESULT([yes])
  fi
  unset val type
])dnl
