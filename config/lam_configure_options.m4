dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

AC_DEFUN([LAM_CONFIGURE_OPTIONS],[
lam_show_subtitle "Configuration options"

#
# Memory debugging
#

AC_MSG_CHECKING([whether to debug memory usage])
AC_ARG_ENABLE(mem-debug, 
    AC_HELP_STRING([--enable-mem-debug],
                   [enable memory debugging (debugging only) (default: disabled)]))
if test "$enable_mem_debug" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_MEM_DEBUG=1
else
    AC_MSG_RESULT([no])
    WANT_MEM_DEBUG=0
fi
#################### Early development override ####################
if test "$WANT_MEM_DEBUG" = "0" -a -z "$enable_mem_zero" -a -d CVS; then
    WANT_MEM_DEBUG=1
    echo "--> developer override: enable mem profiling by default"
fi
#################### Early development override ####################
AC_DEFINE_UNQUOTED(LAM_ENABLE_MEM_DEBUG, $WANT_MEM_DEBUG,
    [Whether we want the memory profiling or not])

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
#################### Early development override ####################
if test "$WANT_MEM_PROFILE" = "0" -a -z "$enable_mem_zero" -a -d CVS; then
    WANT_MEM_PROFILE=1
    echo "--> developer override: enable mem profiling by default"
fi
#################### Early development override ####################
AC_DEFINE_UNQUOTED(LAM_ENABLE_MEM_PROFILE, $WANT_MEM_PROFILE,
    [Whether we want the memory profiling or not])

#
# Developer picky compiler options
#

AC_MSG_CHECKING([if want developer-level compiler pickyness])
AC_ARG_ENABLE(picky, 
    AC_HELP_STRING([--enable-picky],
                   [enable developer-level compiler pickyness (not for general MPI users!) (default: enabled)]))
if test "$enable_picky" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_PICKY_COMPILER=1
else
    AC_MSG_RESULT([no])
    WANT_PICKY_COMPILER=0
fi
#################### Early development override ####################
if test "$WANT_PICKY_COMPILER" = "0" -a -z "$enable_picky" -a -d CVS; then
    WANT_PICKY_COMPILER=1
    echo "--> developer override: enable picky compiler by default"
fi
#################### Early development override ####################

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
#################### Early development override ####################
if test "$WANT_DEBUG" = "0" -a -z "$enable_debug" -a -d CVS; then
    WANT_DEBUG=1
    echo "--> developer override: enable debugging code by default"
fi
#################### Early development override ####################
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

#
# MPI profiling
#

AC_MSG_CHECKING([whether to enable PMPI])
AC_ARG_ENABLE(mpi-profile, 
    AC_HELP_STRING([--enable-mpi-profile],
                   [enable MPI profiling (default: enabled)]))
if test "$enable_mpi_profile" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_MPI_PROFILE=1
else
    AC_MSG_RESULT([no])
    WANT_MPI_PROFILE=0
fi
AC_DEFINE_UNQUOTED(LAM_ENABLE_MPI_PROFILING, $WANT_MPI_PROFILE,
    [Do we want MPI profiling or not?])


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
    LIBLAM_LA='$(top_builddir)/src/liblam.la'
    LIBMPI_LA='$(top_builddir)/src/libmpi.la'" $LIBLAM_LA"
else
    AC_MSG_RESULT([yes])
    WANT_SINGLE_MPI_LIBRARY=1
    LIBLAM_LA='$(top_builddir)/src/libmpi.la'
    LIBMPI_LA="$LIBLAM_LA"
fi
AC_SUBST(LIBLAM_LA)
AC_SUBST(LIBMPI_LA)
AC_DEFINE_UNQUOTED(WANT_SINGLE_MPI_LIBRARY, $WANT_SINGLE_MPI_LIBRARY,
    [Do we want libmpi or libmpi and liblam?])
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
# Do we want to disable weak symbols for some reason?
#

AC_MSG_CHECKING([if want to enable weak symbol support])
AC_ARG_ENABLE(weak-symbols,
    AC_HELP_STRING([--enable-weak-symbols],
                   [use weak symbols, if available (default: enabled)]))
if test "$enable_weak_symbols" != "no"; then
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
AC_MSG_CHECKING(if want deprecated executable names)
AC_ARG_ENABLE(deprecated-executable-names,
    AC_HELP_STRING([--enable-deprecated-executable-names], [make sym links to deprecated LAM executables (e.g., hcc, hcp, hf77, wipe) (default: disabled)]))
if test "$enable_deprecated_executable_names" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_DEN=1
else
    AC_MSG_RESULT([no])
    WANT_DEN=0
fi

AM_CONDITIONAL(WANT_DEPRECATED_EXECUTABLE_NAMES, test "$WANT_DEN" = "1")

#
])
