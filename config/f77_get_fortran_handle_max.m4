dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_F77_GET_FORTRAN_HANDLE_MAX],[
# Find the maximum value of fortran integers, then calculate
# min(INT_MAX, max fortran INTEGER).  This represents the maximum
# number of fortran MPI handle index.

AC_MSG_CHECKING([for max fortran MPI handle index])

# Find max fortran INTEGER value.  Set to sentinel value if we don't
# have a Fortran compiler (e.g., if --disable-f77 was given)

if test "$OMPI_WANT_F77_BINDINGS" = "0" ; then
    ompi_fint_max=-1
else
    ompi_sizeof_fint=`expr $OMPI_SIZEOF_FORTRAN_INT \* 8 - 1`
    ompi_fint_max=1
    while test "$ompi_sizeof_fint" != "0"; do
        ompi_fint_max=`expr $ompi_fint_max \* 2`
        ompi_sizeof_fint=`expr $ompi_sizeof_fint - 1`
    done
    ompi_fint_max=`expr $ompi_fint_max - 1`
fi

# Compare to C MAX_INT.

rm -f conftest.out > /dev/null 2>&1
AC_RUN_IFELSE(AC_LANG_PROGRAM([[
#include <stdio.h>
#include <limits.h>
]],[[FILE *fp = fopen("conftest.out", "w");
long fint = $ompi_fint_max;
long cint = INT_MAX;
if (-1 == fint) fint = cint;
fprintf(fp, "%ld", (cint < fint) ? cint : fint);
fclose(fp);]]), HAPPY=1, HAPPY=0)
if test "$HAPPY" = "1"; then
    OMPI_FORTRAN_HANDLE_MAX=`cat conftest.out`
else
    # Something went wrong, so be conservative
    OMPI_FORTRAN_HANDLE_MAX=32767
fi
rm -f conftest.out > /dev/null 2>&1

AC_DEFINE_UNQUOTED(OMPI_FORTRAN_HANDLE_MAX,
    $OMPI_FORTRAN_HANDLE_MAX,
    [Max handle value for fortran MPI handles, effectively min(INT_MAX, max fortran INTEGER value)])

AC_MSG_RESULT([$OMPI_FORTRAN_HANDLE_MAX])
])dnl
