dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl
dnl LAM_SETUP_F90
dnl
dnl sets:
dnl  F90                   : full pathname to compiler
dnl  BASEF90               : compiler name (no path)
dnl  LAM_WANT_F90_BINDINGS : (actually set by lam_configure_options, may be
dnl                          redefined here)
dnl  FC                    : Same as F90.  Side effect of AC_PROG_FC.  Should
dnl                          not be used
dnl defines:
dnl  LAM_F90               : same as F90
dnl  LAM_WANT_F90_BINDINGS :
dnl am_conditional:
dnl  LAM_WANT_F90_BINDINGS :

AC_DEFUN([LAM_SETUP_F90],[

# Modularize this setup so that sub-configure.in scripts can use this
# same setup code.

lam_show_subtitle "Fortran 90/95 compiler" 

if test "$LAM_WANT_F77_BINDINGS" = "0" ; then
    AC_MSG_WARN([*** Fortran 90/95 bindings disabled (Fortran 77 was disabled)])
    LAM_WANT_F90_BINDINGS=0
    LAM_F90="none"
    BASEF90="none"
elif test "$LAM_WANT_F90_BINDINGS" = "0" ; then
    AC_MSG_WARN([*** Fortran 90/95 bindings disabled by user])
    LAM_WANT_F90_BINDINGS=0
    LAM_F90="none"
    BASEF90="none"
else

    #
    # Check for the compiler
    #
    # Note that we don't actually *use* the fortran compiler to build
    # anything in LAM; it's only used here in configure to find out
    # symbol conventions, type sizes, etc.  We also pass it down to
    # the wrapper compiler mpif90.
    #

    lam_fcflags_save="$FCFLAGS"
    AC_PROG_FC
    FCFLAGS="$lam_fcflags_save"
    if test -z "$FC"; then
        AC_MSG_WARN([*** Fortran 90/95 bindings disabled (could not find compiler)])
        LAM_WANT_F90_BINDINGS=0
        LAM_F90="none"
        BASEF90="none"
    elif test "$FC" = "$F77"; then
        AC_MSG_WARN([*** Found same compiler for Fortran 77 and 90/95.])
        AC_MSG_WARN([*** Assuming no Fortran 90/95 compiler; disabling])
        AC_MSG_WARN([*** Fortran 90/95 MPI bindings.])
        LAM_WANT_F90_BINDINGS=0
        LAM_F90="none"
        BASEF90="none"
    else
        LAM_F90="$FC"
        BASEF90="`basename $FC`"

        AC_LANG_PUSH(Fortran)
        AC_FC_SRCEXT(f)
        AC_FC_SRCEXT(f90)
        AC_FC_SRCEXT(f95)
        AC_LANG_POP(Fortran)

        AC_MSG_CHECKING([whether $LAM_F77 and $LAM_F90 compilers are compatible])
        LAM_INTL_F90_F77_INTERACTION(fortran_goodness=1, fortran_goodness=0)
        if test "$fortran_goodness" = "0" ; then
            AC_MSG_RESULT([no])
            AC_MSG_WARN([*** Fortran 77 and Fortran 90 compilers are not link compatible])
            AC_MSG_WARN([*** Disabling Fortran 90/95 bindings])
            LAM_WANT_F90_BINDINGS=0
        else
            AC_MSG_RESULT([yes])
            LAM_WANT_F90_BINDINGS=1
        fi
    fi
fi

AC_DEFINE_UNQUOTED(LAM_WANT_F90_BINDINGS, $LAM_WANT_F90_BINDINGS,
    [Whether we want the MPI f90 bindings or not])
AC_DEFINE_UNQUOTED(LAM_F90, "$LAM_F90", [LAM underlying F90 compiler])
AM_CONDITIONAL(LAM_WANT_F90_BINDINGS, test "$LAM_WANT_F90_BINDINGS" = "1")
unset fortran_goodness
])


AC_DEFUN([LAM_INTL_F90_F77_INTERACTION], [
# make sure that we can combine F90 and F77 code
AC_LANG_PUSH(Fortran)
# Fortran module
cat > conftestf77.f <<EOF
        subroutine Conf1_test()
        end
EOF
cat > conftestf90.f <<EOF
        program main
        call Conf1_test()
        end
EOF

# Try the compile
LAM_LOG_COMMAND(
    [$LAM_F90 $FCFLAGS $FCFLAGS_f -c conftestf90.f],
    LAM_LOG_COMMAND(
        [$LAM_F77 $FFLAGS -c conftestf77.f],
        LAM_LOG_COMMAND(
            [$LAM_F90 $FCFLAGS -o conftest conftestf90.o conftestf77.o $LIBS],
            [HAPPY=1],
            [HAPPY=0]),
	[HAPPY=0]),
    [HAPPY=0])

if test "$HAPPY" = "1"; then
   $1
else
    LAM_LOG_MSG([here is the F77 program:], 1)
    LAM_LOG_FILE([conftestf77.f])
    LAM_LOG_MSG([here is the F90 program:], 1)
    LAM_LOG_FILE([conftestf90.f90])
    $2
fi

unset HAPPY lam_conftest_h
/bin/rm -f conftest*

AC_LANG_POP(Fortran)
])dnl
