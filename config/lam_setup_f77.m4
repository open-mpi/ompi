dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

AC_DEFUN([LAM_SETUP_F77],[

# Modularize this setup so that sub-configure.in scripts can use this
# same setup code.

lam_show_subtitle "Fortran 77 compiler" 

if test "$enable_fortran" = "no"; then
    AC_MSG_WARN([*** Fortran disabled by user])
    LAM_WANT_F77=0
    F77=none
    BASEF77=none
elif test "$enable_f77" = "no"; then
    AC_MSG_WARN([*** Fortran 77 disabled by user])
    LAM_WANT_F77=0
    F77=none
    BASEF77=none
else

    #
    # Check for the compiler
    #
    # Note that we don't actually *use* the fortran compiler to build
    # anything in LAM; it's only used here in configure to find out
    # symbol conventions, type sizes, etc.  We also pass it down to
    # the wrapper compiler mpif77.
    #

    lam_fflags_save="$FFLAGS"
    AC_PROG_F77
    FFLAGS="$lam_fflags_save"
    if test -z "$F77"; then
        AC_MSG_WARN([*** Could not find Fortran 77 compiler])
        LAM_WANT_F77=0
        F77=none
        BASEF77=none
    else
        LAM_WANT_F77=1
        BASEF77="`basename $F77`"
    fi
fi

AC_DEFINE_UNQUOTED(LAM_ENABLE_MPI_F77, $LAM_WANT_F77,
    [Whether we want the MPI f77 bindings or not])
AC_DEFINE_UNQUOTED(LAM_F77, "$F77", [LAM underlying F77 compiler])
AM_CONDITIONAL(BUILD_MPI_F77, test "$LAM_WANT_F77" = "1")
])
