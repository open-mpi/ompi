dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

AC_DEFUN(LAM_CONFIGURE_OPTIONS,[
lam_show_subtitle "Configuration options"

#
# Purify clean
#

AC_MSG_CHECKING([whether to enable memory zeroing])
AC_ARG_ENABLE(memzero, 
    AC_HELP_STRING([--enable-memzero],
                   [enable memory zeroing (debugging only) (default: disabled)]))
if test "$enable_memzero" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_MEMZERO=1
else
    AC_MSG_RESULT([no])
    WANT_MEMZERO=0
fi
AC_DEFINE_UNQUOTED(LAM_ENABLE_MEMZERO, $WANT_MEMZERO,
    [Whether we want the LAM_MEMZERO macro to memset or not])

#
# Developer debugging
#

AC_MSG_CHECKING([if want developer-level debugging code])
AC_ARG_ENABLE(debug, 
    AC_HELP_STRING([--enable-debug],
                   [enable developer-level debugging code (not for general MPI users!) (default: disabled)]))
if test "$enable_debug" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_DEBUG=1
else
    AC_MSG_RESULT([no])
    WANT_DEBUG=0
fi
AC_DEFINE_UNQUOTED(LAM_ENABLE_DEBUG, $WANT_DEBUG,
    [Whether we want developer-level debugging code or not])

#
# Fortran 77
#

AC_MSG_CHECKING([if want Fortran 77 bindings])
AC_ARG_ENABLE(f77, 
    AC_HELP_STRING([--enable-f77],
                   [enable f77 MPI bindings (default: enabled)]))
if test "$enable_f77" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_MPI_F77=1
else
    AC_MSG_RESULT([no])
    WANT_MPI_F77=0
fi
AC_DEFINE_UNQUOTED(LAM_ENABLE_MPI_F77, $WANT_MPI_F77,
    [Whether we want the MPI f77 bindings or not])

#
# Fortran 90
#

AC_MSG_CHECKING([if want Fortran 90 bindings])
AC_ARG_ENABLE(f90, 
    AC_HELP_STRING([--enable-f90],
                   [enable f90 MPI bindings (default: enabled)]))
if test "$enable_f90" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_MPI_F90=1
else
    AC_MSG_RESULT([no])
    WANT_MPI_F90=0
fi
AC_DEFINE_UNQUOTED(LAM_ENABLE_MPI_F90, $WANT_MPI_F90,
    [Whether we want the MPI f90 bindings or not])

#
# Do we want -llam/-lmpi, or just -lmpi?
#

AC_MSG_CHECKING([if want consolidated MPI library (not recommended)])
AC_ARG_ENABLE(single-lib,
    AC_HELP_STRING([--enable-single-lib],
                   [those who use the MPI wrapper compilers (mpicc, mpif77, etc.) do not care about this option.  This option is *only* if you insist on using underlying compilers to compile MPI applications and only want to have -lmpi as your $LIBS) (default: disabled)]))
if test "$enable_single_lib" != "yes"; then
    AC_MSG_RESULT([no])
    WANT_SINGLE_MPI_LIBRARY=1
else
    AC_MSG_RESULT([no])
    WANT_SINGLE_MPI_LIBRARY=0
fi
AM_CONDITIONAL(WANT_SINGLE_MPI_LIBRARY, test "$WANT_SINGLE_MPI_LIBRARY" = 1)

#
# Do we want profiling?
#

AC_MSG_CHECKING([if want MPI profiling layers])
AC_ARG_ENABLE(profiling,
    AC_HELP_STRING([--enable-profiling],
                   [build the MPI profiling layers for all available languages (default: enabled)]))
if test "$enable_profiling" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_MPI_PROFILING=1
else
    AC_MSG_RESULT([no])
    WANT_MPI_PROFILING=0
fi
AM_CONDITIONAL(WANT_MPI_PROFILING, test "$WANT_MPI_PROFILING" = 1)

#
# Do we want to disable weak symbols for some reason?
#

AC_MSG_CHECKING([if want to enable weak symbol support])
AC_ARG_ENABLE(weak-symbols,
    AC_HELP_STRING([--enable-weak-symbols],
                   [use weak symbols, if available (default: enabled)]))
if test "$enable_weak-symbols" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_WEAK_SYMBOLS=1
else
    AC_MSG_RESULT([no])
    WANT_WEAK_SYMBOLS=0
fi

# --enable-dist
# ...?

# amorphous, seem-to-be-good-idea options
# --with-lam=maintainer_options
# --with-mca-*
# ...?
])
