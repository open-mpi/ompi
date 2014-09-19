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

# Check the max array rank that the Fortran compiler supports.
#
# OMPI_FORTRAN_CHECK_MAX_ARRAY_RANK
#
# Sets $OMPI_FORTRAN_MAX_ARRAY_RANK, AC_SUBSTs it, and AC_DEFINEs
# OMPI_FORTRAN_MAX_ARRAY_RANK.
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_MAX_ARRAY_RANK],[
    AS_VAR_PUSHDEF([max_array_rank_var], [ompi_cv_fortran_max_array_rank])

    OPAL_VAR_SCOPE_PUSH([f_max_rank f_fail f_rank f_i f_dim])
    AC_CACHE_CHECK([max supported Fortran array rank], max_array_rank_var,
       [AC_LANG_PUSH([Fortran])
        f_max_rank=0
        f_fail=0

        # Realistically, this will only be 7 or 15.  But what the heck
        # -- try them all.  Note that we don't test above 15, because
        # that's the max value from the F2008 spec (and some compilers
        # will let you go above rank=16, e.g., Intel ifort).
        for f_rank in 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
            if test $f_fail -eq 0; then
                f_i=1
                f_dim=2
                while test `expr $f_i + 1` -le $f_rank; do
                    f_dim="$f_dim,2"
                    f_i=`expr $f_i + 1`
                done
                OPAL_LOG_MSG([testing Fortran dimension $f_rank / $f_dim])
                AC_COMPILE_IFELSE([AC_LANG_SOURCE([[PROGRAM test_program
    INTEGER, DIMENSION($f_dim) :: var
    var($f_dim) = 3
END PROGRAM test_program]])],
                                  [f_max_rank=$f_rank], [f_fail=1])
            fi
        done
        AS_VAR_SET(max_array_rank_var, $f_max_rank)
        AC_LANG_POP([Fortran])
       ])

    AS_VAR_COPY([OMPI_FORTRAN_MAX_ARRAY_RANK], [max_array_rank_var])
    AC_SUBST(OMPI_FORTRAN_MAX_ARRAY_RANK)
    AC_DEFINE_UNQUOTED([OMPI_FORTRAN_MAX_ARRAY_RANK],
                       [$OMPI_FORTRAN_MAX_ARRAY_RANK],
                       [Max dimension rank of Fortran arrays])

    OPAL_VAR_SCOPE_POP
    AS_VAR_POPDEF([max_array_rank_var])dnl
])
