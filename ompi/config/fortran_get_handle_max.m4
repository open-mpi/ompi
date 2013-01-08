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
dnl Copyright (c) 2011-2013 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_FORTRAN_GET_HANDLE_MAX()
# ---------------------------------------------------------------
# Find the maximum value of fortran integers, then calculate
# min(INT_MAX, max fortran INTEGER).  This represents the maximum
# number of fortran MPI handle index.
AC_DEFUN([OMPI_FORTRAN_GET_HANDLE_MAX],[
    AS_VAR_PUSHDEF([fortran_handle_max_var], 
                   [ompi_cv_fortran_handle_max])

    AC_CACHE_CHECK([for max Fortran MPI handle index],
        fortran_handle_max_var,
        [ # Find max fortran INTEGER value.  Set to sentinel value if we don't
         # have a Fortran compiler (e.g., if --disable-fortran was given). 
         if test $OMPI_WANT_FORTRAN_BINDINGS -eq 0; then
             ompi_fint_max=0
         else
             OPAL_COMPUTE_MAX_VALUE([$OMPI_SIZEOF_FORTRAN_INTEGER], [ompi_fint_max])
         fi

         # Get INT_MAX.  Compute a SWAG if we are cross compiling or something
         # goes wrong.
         rm -f conftest.out >/dev/null 2>&1
         AC_RUN_IFELSE([AC_LANG_PROGRAM([[
#include <stdio.h>
#include <limits.h>
]],[[FILE *fp = fopen("conftest.out", "w");
long cint = INT_MAX;
fprintf(fp, "%ld", cint);
fclose(fp);]])], 
             [ompi_cint_max=`cat conftest.out`], 
             [ompi_cint_max=0],
             [ #cross compiling is fun.  compute INT_MAX same as INTEGER max
              OPAL_COMPUTE_MAX_VALUE([$ac_cv_sizeof_int], [ompi_cint_max])])

         # Use string comparisons with "test"; see comment above for
         # rationale.
         if test "$ompi_cint_max" = "0" ; then
             # wow - something went really wrong.  Be conservative
             value=32767
         elif test "$ompi_fint_max" = "0" ; then
             # we aren't compiling Fortran - just set it to C INT_MAX
             value=$ompi_cint_max
         else
             # Take the lesser of C INT_MAX and Fortran INTEGER max.
             # The resulting value will then be storable in either
             # type.  Use expr (instead of "test -lt"), because it can
             # handle 8-byte integer values.
             value=$ompi_fint_max
             if test "`expr $ompi_cint_max \< $value`" = "1"; then
                 value=$ompi_cint_max
             fi
          fi
          AS_VAR_SET(fortran_handle_max_var, [$value])
          rm -f conftest.out > /dev/null 2>&1 
          unset value])

    AS_VAR_COPY([ompi_fortran_handle_max], [fortran_handle_max_var])
    AC_DEFINE_UNQUOTED([OMPI_FORTRAN_HANDLE_MAX],
        [$ompi_fortran_handle_max],
        [Max handle value for fortran MPI handles, effectively min(INT_MAX, max fortran INTEGER value)])
    AS_VAR_POPDEF([fortran_handle_max_var])
])dnl
