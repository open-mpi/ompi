dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl
dnl sets:
dnl  F77                   : full pathname to compiler
dnl  BASEF77               : compiler name (no path)
dnl  LAM_WANT_F77_BINDINGS : (actually set by lam_configure_options, may be
dnl                          redefined here)
dnl  FC                    : Same as F77.  Side effect of AC_PROG_FC.  Should
dnl                          not be used
dnl defines:
dnl  LAM_F77               : same as F77
dnl  LAM_WANT_F77_BINDINGS :
dnl am_conditional:
dnl  LAM_WANT_F77_BINDINGS :

AC_DEFUN([LAM_SETUP_F77],[

# Modularize this setup so that sub-configure.in scripts can use this
# same setup code.

lam_show_subtitle "Fortran 77 compiler" 

#
# Check for the compiler
#
# Note that we don't actually *use* the fortran compiler to build
# anything in LAM; it's only used here in configure to find out
# symbol conventions, type sizes, etc.  We also pass it down to
# the wrapper compiler mpif77.
#
# Always run this test, even if fortran isn't wanted so that F77 has
# value for the Fint tests
#
lam_fflags_save="$FFLAGS"
AC_PROG_F77
FFLAGS="$lam_fflags_save"
if test -z "$F77"; then
    AC_MSG_WARN([*** Fortran 77 bindings disabled (could not find compiler)])
    LAM_WANT_F77_BINDINGS=0
    F77="none"
    BASEF77="none"
else
    if test "$LAM_WANT_F77_BINDINGS" = "0" ; then
        AC_MSG_WARN([*** Fortran 77 bindings disabled by user])
        LAM_WANT_F77_BINDINGS=0
        BASEF77="`basename $F77`"
    else
        LAM_WANT_F77_BINDINGS=1
        BASEF77="`basename $F77`"
    fi
fi

AC_DEFINE_UNQUOTED(LAM_WANT_F77_BINDINGS, $LAM_WANT_F77_BINDINGS,
    [Whether we want the MPI f77 bindings or not])
AC_DEFINE_UNQUOTED(LAM_F77, "$F77", [LAM underlying F77 compiler])
AM_CONDITIONAL(LAM_WANT_F77_BINDINGS, test "$LAM_WANT_F77_BINDINGS" = "1")
])
