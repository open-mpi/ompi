dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

AC_DEFUN(LAM_SETUP_F90,[

# Modularize this setup so that sub-configure.in scripts can use this
# same setup code.

lam_show_subtitle "Fortran 90/95 compiler" 

if test "$enable_fortran" = "no"; then
    AC_MSG_WARN([*** Fortran disabled by user])
    LAM_WANT_F90=0
elif test "$enable_f90" = "no"; then
    AC_MSG_WARN([*** Fortran 90/95 disabled by user])
    LAM_WANT_F90=0
else

    #
    # Check for the compiler
    #
    # Note that we don't actually *use* the fortran compiler to build
    # anything in LAM; it's only used here in configure to find out
    # symbol conventions, type sizes, etc.  We also pass it down to
    # the wrapper compiler mpif90.
    #

    lam_fflags_save="$FFLAGS"
    AC_PROG_FC
    FFLAGS="$lam_fflags_save"
    if test -z "$FC"; then
	AC_MSG_WARN([*** Could not find Fortran 90/95 compiler])
	LAM_WANT_F90=0
    elif test "$FC" = "$F77"; then
	AC_MSG_WARN([*** Found same compiler for Fortran 77 and 90/95])
	AC_MSG_WARN([*** Assuming no Fortran 90/95 compiler; disabling])
	AC_MSG_WARN([*** Fortran 90 MPI bindings])
	LAM_WANT_F90=0
    else
	LAM_WANT_F90=1
	BASEF90="`basename $FC`"
    fi
fi

AM_CONDITIONAL(BUILD_MPI_F90, test "$LAM_WANT_F90" = "1")
])
