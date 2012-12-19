dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
dnl                         reserved. 
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$

# Does this compiler support the Fortran 2008 assumed rank syntax?

# OMPI_FORTRAN_CHECK_F08_ASSUMED_RANK([action if found], 
#                                      [action if not found])
# ----------------------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_F08_ASSUMED_RANK], [
    AS_VAR_PUSHDEF([fortran_f08_assumed_rank], 
                   [ompi_cv_fortran_f08_assumed_rank])

    AC_CACHE_CHECK([Fortran compiler F08 assumed rank syntax],
                    fortran_f08_assumed_rank,
                    [_OMPI_FORTRAN_CHECK_F08_ASSUMED_RANK])

    AS_VAR_IF(fortran_f08_assumed_rank, [yes], [$1], [$2])
])

###################################

AC_DEFUN([_OMPI_FORTRAN_CHECK_F08_ASSUMED_RANK], [
    OPAL_VAR_SCOPE_PUSH([happy])

    # If we were called here, it means that the value was not cached,
    # so we need to check several different things.  Since CACHE_CHECK
    # puts up a MSG_CHECKING, we need to terminate it with a bogus
    # answer before doing the individual checks.
    AC_MSG_RESULT([not cached; checking])

    # Check for the F08 type(*),dimension(..) syntax
    OMPI_FORTRAN_CHECK_IGNORE_TKR_SUB([!], [TYPE(*), DIMENSION(..)],
                                      [TYPE(*), DIMENSION(..)],
                                      [happy=yes], [happy=no])

    AS_VAR_SET(fortran_f08_assumed_rank, [$happy]);

    # Now put the orignal CACHE_CHECK MSG_CHECKING back so that it can
    # output the MSG_RESULT.
    AC_MSG_CHECKING([Fortran compiler F08 assumed rank syntax])
    OPAL_VAR_SCOPE_POP
])
