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
dnl sets:
dnl  F77                   : full pathname to compiler
dnl  BASEF77               : compiler name (no path)
dnl  OMPI_WANT_F77_BINDINGS : (actually set by ompi_configure_options, may be
dnl                          redefined here)
dnl  FC                    : Same as F77.  Side effect of AC_PROG_FC.  Should
dnl                          not be used
dnl defines:
dnl  OMPI_F77               : same as F77
dnl  OMPI_WANT_F77_BINDINGS :
dnl am_conditional:
dnl  OMPI_WANT_F77_BINDINGS :

AC_DEFUN([OMPI_SETUP_F77],[

# Modularize this setup so that sub-configure.in scripts can use this
# same setup code.

ompi_show_subtitle "Fortran 77 compiler" 

#
# Check for the compiler
#
# Note that we don't actually *use* the fortran compiler to build
# anything in OMPI; it's only used here in configure to find out
# symbol conventions, type sizes, etc.  We also pass it down to
# the wrapper compiler mpif77.
#
# Always run this test, even if fortran isn't wanted so that F77 has
# value for the Fint tests
#
ompi_fflags_save="$FFLAGS"
AC_PROG_F77
FFLAGS="$ompi_fflags_save"
if test -z "$F77"; then
    AC_MSG_WARN([*** Fortran 77 bindings disabled (could not find compiler)])
    OMPI_WANT_F77_BINDINGS=0
    OMPI_F77="none"
    BASEF77="none"
else
    if test "$OMPI_WANT_F77_BINDINGS" = "0" ; then
        AC_MSG_WARN([*** Fortran 77 bindings disabled by user])
        OMPI_WANT_F77_BINDINGS=0
        OMPI_F77="$F77"
        BASEF77="`basename $OMPI_F77`"
    else
        OMPI_WANT_F77_BINDINGS=1
        OMPI_F77="$F77"
        BASEF77="`basename $OMPI_F77`"
    fi
fi

AC_DEFINE_UNQUOTED(OMPI_WANT_F77_BINDINGS, $OMPI_WANT_F77_BINDINGS,
    [Whether we want the MPI f77 bindings or not])
AC_DEFINE_UNQUOTED(OMPI_F77, "$OMPI_F77", [OMPI underlying F77 compiler])
AM_CONDITIONAL(OMPI_WANT_F77_BINDINGS, test "$OMPI_WANT_F77_BINDINGS" = "1")
])
