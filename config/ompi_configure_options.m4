dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CONFIGURE_OPTIONS],[
ompi_show_subtitle "Configuration options"

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
if test "$WANT_MEM_DEBUG" = "0" -a -z "$enable_mem_debug" -a -d .svn; then
    WANT_MEM_DEBUG=1
    echo "--> developer override: enable mem profiling by default"
fi
#################### Early development override ####################
AC_DEFINE_UNQUOTED(OMPI_ENABLE_MEM_DEBUG, $WANT_MEM_DEBUG,
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
if test "$WANT_MEM_PROFILE" = "0" -a -z "$enable_mem_profile" -a -d .svn; then
    WANT_MEM_PROFILE=1
    echo "--> developer override: enable mem profiling by default"
fi
#################### Early development override ####################
AC_DEFINE_UNQUOTED(OMPI_ENABLE_MEM_PROFILE, $WANT_MEM_PROFILE,
    [Whether we want the memory profiling or not])

#
# Developer picky compiler options
#

AC_MSG_CHECKING([if want developer-level compiler pickyness])
AC_ARG_ENABLE(picky, 
    AC_HELP_STRING([--enable-picky],
                   [enable developer-level compiler pickyness when building Open MPI (default: disabled)]))
if test "$enable_picky" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_PICKY_COMPILER=1
else
    AC_MSG_RESULT([no])
    WANT_PICKY_COMPILER=0
fi
#################### Early development override ####################
if test "$WANT_PICKY_COMPILER" = "0" -a -z "$enable_picky" -a -d .svn; then
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
if test "$WANT_DEBUG" = "0" -a -z "$enable_debug" -a -d .svn; then
    WANT_DEBUG=1
    echo "--> developer override: enable debugging code by default"
fi
#################### Early development override ####################
if test "$WANT_DEBUG" = "0"; then
    CFLAGS="-DNDEBUG $CFLAGS"
    CXXFLAGS="-DNDEBUG $CFLAGS"
fi
AC_DEFINE_UNQUOTED(OMPI_ENABLE_DEBUG, $WANT_DEBUG,
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
    OMPI_WANT_F77_BINDINGS=1
else
    AC_MSG_RESULT([no])
    OMPI_WANT_F77_BINDINGS=0
fi


#
# Fortran 90
#

AC_MSG_CHECKING([if want Fortran 90 bindings])
AC_ARG_ENABLE(f90, 
    AC_HELP_STRING([--enable-f90],
                   [enable f90 MPI bindings (default: disabled)]))
if test "$enable_f90" = "yes"; then
    AC_MSG_RESULT([yes])
    OMPI_WANT_F90_BINDINGS=1
else
    AC_MSG_RESULT([no])
    OMPI_WANT_F90_BINDINGS=0
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
    WANT_MPI_PROFILING=1
else
    AC_MSG_RESULT([no])
    WANT_MPI_PROFILING=0
fi


#                                                                              
# C++                                                                          
#                                                                              

AC_MSG_CHECKING([if want C++ bindings])
AC_ARG_ENABLE(cxx,
    AC_HELP_STRING([--enable-cxx],
                   [enable C++ MPI bindings (default: enabled)]))
if test "$enable_cxx" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_MPI_CXX_SUPPORT=1
else
    AC_MSG_RESULT([no])
    WANT_MPI_CXX_SUPPORT=0
fi


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

#
# Do we want to disable MPI parameter checking at run-time?
#

AC_MSG_CHECKING([if want run-time MPI parameter checking])
AC_ARG_WITH(mpi-param-check,
    AC_HELP_STRING([--with-mpi-param-check(=VALUE)],
                   [behavior of MPI function parameter checking.  Valid values are: always, never, runtime.  No VALUE specified is equivalent to "always"; --without is equivalent to "never" (default: runtime).]))
mpi_param_check=ompi_mpi_param_check
if test "$with_mpi_param_check" = "no" -o \
    "$with_mpi_param_check" = "never"; then
    mpi_param_check=0
    AC_MSG_RESULT([never])
elif test "$with_mpi_param_check" = "yes" -o \
    "$with_mpi_param_check" = "always"; then
    mpi_param_check=1
    AC_MSG_RESULT([always])
elif test "$with_mpi_param_check" = "runtime" -o \
    -z "$with_mpi_params_check"; then
    AC_MSG_RESULT([runtime])
else
    AC_MSG_RESULT([unknown])
    AC_MSG_WARN([*** Unrecognized --with-mpi-param-check value])
    AC_MSG_WARN([*** See "configure --help" output])
    AC_MSG_WARN([*** Defaulting to "runtime"])
fi
AC_DEFINE_UNQUOTED(MPI_PARAM_CHECK, $mpi_param_check,
    [Whether we want to check MPI parameters always, never, or decide at run-time])


#
# Do we want to install all of OMPI's header files?
#

AC_MSG_CHECKING([if want to install OMPI header files])
AC_ARG_WITH(devel-headers,
    AC_HELP_STRING([--with-devel-headers],
                   [normal MPI users/applications do not need this (mpi.h and mpif.h are ALWAYS installed).  Developer headers are only necessary for MCA module authors (default: disabled).]))
if test "$with_devel_headers" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_INSTALL_HEADERS=1
else
    AC_MSG_RESULT([no])
    WANT_INSTALL_HEADERS=0
fi
AM_CONDITIONAL(WANT_INSTALL_HEADERS, test "$WANT_INSTALL_HEADERS" = 1)

# --enable-dist
# ...?

# amorphous, seem-to-be-good-idea options
# --with-ompi=maintainer_options
# --with-mca-*
# ...?

#
# Do we want deprecated executable names ?
# 
AC_MSG_CHECKING(if want deprecated executable names)
AC_ARG_ENABLE(deprecated-executable-names,
    AC_HELP_STRING([--enable-deprecated-executable-names], [make sym links to deprecated OMPI executables (e.g., hcc, hcp, hf77, wipe) (default: disabled)]))
if test "$enable_deprecated_executable_names" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_DEN=1
else
    AC_MSG_RESULT([no])
    WANT_DEN=0
fi

AM_CONDITIONAL(WANT_DEPRECATED_EXECUTABLE_NAMES, test "$WANT_DEN" = "1")


#
# Do we want to build MPI-2 one-sided functions?  Currently, they are
# empty shell functions that simply invoke an MPI exception (i.e., a
# run-time error vs. a compile/link-time error).
#
AC_MSG_CHECKING([if want MPI-2 one-sided empty shell functions])
AC_ARG_ENABLE(mpi2-one-sided,
    AC_HELP_STRING([--enable-mpi2-one-sided],
                   [Do we want to build empty shell functions for the MPI-2 one-sided functionality?  (these functions are currently unimplemented -- all they do is invoke a run-time MPI exception)]))
if test "$enable_mpi2_one_sided" = "yes"; then
    mpi2_one_sided=yes
    value=1

    # Need to set values that will be in mpif.h

    OMPI_F77_WIN_ATTR_KEYS="integer MPI_WIN_BASE, MPI_WIN_SIZE, MPI_WIN_DISP_UNIT"
    OMPI_F77_WIN_ATTR_BASE_VALUE="parameter (MPI_WIN_BASE=6)"
    OMPI_F77_WIN_ATTR_SIZE_VALUE="parameter (MPI_WIN_SIZE=7)"
    OMPI_F77_WIN_ATTR_DISP_VALUE="parameter (MPI_WIN_DISP_UNIT=8)"
    OMPI_F77_WIN_NULL_COPY_FN="external MPI_WIN_NULL_COPY_FN"
    OMPI_F77_WIN_NULL_DELETE_FN="external MPI_WIN_NULL_DELETE_FN"
    OMPI_F77_WIN_DUP_FN="external MPI_WIN_DUP_FN"
else
    mpi2_one_sided=no
    value=0

    # Many values in mpif.h are now blank

    OMPI_F77_WIN_ATTR_KEYS=
    OMPI_F77_WIN_ATTR_BASE_VALUE=
    OMPI_F77_WIN_ATTR_SIZE_VALUE=
    OMPI_F77_WIN_ATTR_DISP_VALUE=
    OMPI_F77_WIN_NULL_COPY_FN=
    OMPI_F77_WIN_NULL_DELETE_FN=
    OMPI_F77_WIN_DUP_FN=
fi
AC_MSG_RESULT([$mpi2_one_sided])
AC_DEFINE_UNQUOTED(OMPI_WANT_MPI2_ONE_SIDED, $value,
    [Do we want the MPI-2 one-sided functions to be compiled in or left out altogether (i.e., unlinkable)?])
AM_CONDITIONAL(WANT_MPI2_ONE_SIDED, test "$mpi2_one_sided" = "yes")
AC_SUBST(OMPI_F77_WIN_ATTR_KEYS)
AC_SUBST(OMPI_F77_WIN_ATTR_BASE_VALUE)
AC_SUBST(OMPI_F77_WIN_ATTR_SIZE_VALUE)
AC_SUBST(OMPI_F77_WIN_ATTR_DISP_VALUE)
AC_SUBST(OMPI_F77_WIN_NULL_COPY_FN)
AC_SUBST(OMPI_F77_WIN_NULL_DELETE_FN)
AC_SUBST(OMPI_F77_WIN_DUP_FN)
#
])
