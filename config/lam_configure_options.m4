dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

AC_DEFUN(LAM_CONFIGURE_OPTIONS,[
lam_show_subtitle "Configuration options"

#
# "Purify" clean
#

AC_MSG_CHECKING([whether to enable memory zeroing])
AC_ARG_ENABLE(mem-zero, 
    AC_HELP_STRING([--enable-mem-zero],
                   [enable memory zeroing for memory-checking debuggers (debugging only) (default: disabled)]))
if test "$enable_mem-zero" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_MEM_ZERO=1
else
    AC_MSG_RESULT([no])
    WANT_MEM_ZERO=0
fi
AC_DEFINE_UNQUOTED(LAM_ENABLE_MEM_ZERO, $WANT_MEM_ZERO,
    [Whether we want the LAM_MEM_ZERO macro to memset or not])

#
# Memory profiling
#

AC_MSG_CHECKING([whether to profile memory usage])
AC_ARG_ENABLE(mem-profile, 
    AC_HELP_STRING([--enable-mem-profile],
                   [enable memory profiling (debugging only) (default: disabled)]))
if test "$enable_mem_profile" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_MEM_PROFILE=1
else
    AC_MSG_RESULT([no])
    WANT_MEM_PROFILE=0
fi
AC_DEFINE_UNQUOTED(LAM_ENABLE_MEM_PROFILE, $WANT_MEM_PROFILE,
    [Whether we want the memory profiling or not])

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
    WANT_SINGLE_MPI_LIBRARY=0
else
    AC_MSG_RESULT([yes])
    WANT_SINGLE_MPI_LIBRARY=1
fi
AM_CONDITIONAL(WANT_SINGLE_MPI_LIBRARY, test "$WANT_SINGLE_MPI_LIBRARY" = 1)

#
# Do we want to install all of LAM's header files?
#

AC_MSG_CHECKING([if want to install LAM header files])
AC_ARG_WITH(devel-headers,
    AC_HELP_STRING([--with-devel-headers],
                   [normal MPI users/applications do not need this (mpi.h and mpif.h are ALWAYS installed).  Developer headers are only necessary for MCA module authors (default: disabled).]))
if test "$with_devel_headers" != "yes"; then
    AC_MSG_RESULT([no])
    WANT_INSTALL_HEADERS=0
else
    AC_MSG_RESULT([yes])
    WANT_INSTALL_HEADERS=1
fi
AM_CONDITIONAL(WANT_INSTALL_HEADERS, test "$WANT_INSTALL_HEADERS" = 1)

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

#
# Do we want deprecated executable names ?
# 
WANT_DEN=yes
AC_MSG_CHECKING(if want deprecated executable names)
AC_ARG_ENABLE(deprecated-executable-names,
    AC_HELP_STRING([--disable-deprecated-executable-names],
        [do not make sym links to deprecated LAM executables
         (e.g., hcc, hcp, hf77, hf90, wipe)]),
    [if test "$enableval" = "yes"; then
    WANT_DEN=yes
    else
    WANT_DEN=no
    fi])
AM_CONDITIONAL(WANT_DEPRECATED_EXECUTABLE_NAMES, test "$WANT_DEN" = "yes")
AC_MSG_RESULT([$WANT_DEN])

#
])
