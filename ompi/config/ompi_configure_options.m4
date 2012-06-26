dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006-2012 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2009      Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2009-2012 Oak Ridge National Labs.  All rights reserved.
dnl
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CONFIGURE_OPTIONS],[
ompi_show_subtitle "OMPI Configuration options"

#
# Do we want to enable MPI interface warnings (e.g. deprecated
# functionality and others)?
#
# This was disabled by default in v1.5, but will be enabled by default
# in 1.7 and beyond.
#

AC_MSG_CHECKING([if want compile-time warnings inside of mpi.h])
AC_ARG_ENABLE(mpi-interface-warning,
    AC_HELP_STRING([--enable-mpi-interface-warning],
                   [enable compile-time warnings when deprecated MPI functions are used (default: enabled)]))
if test "$enable_mpi_interface_warning" != "no"; then
    AC_MSG_RESULT([yes])
    OMPI_WANT_MPI_INTERFACE_WARNING=1
else
    AC_MSG_RESULT([no])
    OMPI_WANT_MPI_INTERFACE_WARNING=0
fi
AC_DEFINE_UNQUOTED([OMPI_WANT_MPI_INTERFACE_WARNING], [$OMPI_WANT_MPI_INTERFACE_WARNING],
    [Enable warnings when using deprecated MPI functions])

#
# Sparse Groups
#

AC_MSG_CHECKING([if want sparse process groups])
AC_ARG_ENABLE(sparse-groups,
    AC_HELP_STRING([--enable-sparse-groups],
                   [enable sparse process groups (default: not enabled)]))
if test "$enable_sparse_groups" = "yes"; then
    AC_MSG_RESULT([yes])
    GROUP_SPARSE=1
else
    AC_MSG_RESULT([no])
    GROUP_SPARSE=0
fi
AC_DEFINE_UNQUOTED([OMPI_GROUP_SPARSE],$GROUP_SPARSE,
    [Wether we want sparse process groups])


#
# Do we want to enable peruse interface?
#

AC_MSG_CHECKING([if want peruse support])
AC_ARG_ENABLE(peruse,
    AC_HELP_STRING([--enable-peruse],
                   [enable PERUSE interface (default: disabled)]))
if test "$enable_peruse" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_PERUSE=1
else
    AC_MSG_RESULT([no])
    WANT_PERUSE=0
fi
AC_DEFINE_UNQUOTED([OMPI_WANT_PERUSE],
                   [$WANT_PERUSE],
                   [if the peruse interface should be enabled])
AM_CONDITIONAL(WANT_PERUSE, test "$WANT_PERUSE" = "1")

#
# Fortran MPI bindings
#
# JMS Add more here for granulatiry of specifically which bindings to build
#
AC_MSG_CHECKING([if want Fortran MPI bindings])
AC_ARG_ENABLE(mpi-fortran,
    AC_HELP_STRING([--enable-mpi-fortran],
                   [enable Fortran MPI bindings (default: enabled if Fortran compiler found)]))
if test "$enable_mpi_fortran" != "no"; then
    AC_MSG_RESULT([yes])
    OMPI_WANT_FORTRAN_MPIFH_BINDINGS=1
    OMPI_WANT_FORTRAN_USEMPI_BINDINGS=1
    OMPI_WANT_FORTRAN_USEMPIF08_BINDINGS=1
else
    AC_MSG_RESULT([no])
    OMPI_WANT_FORTRAN_MPIFH_BINDINGS=0
    OMPI_WANT_FORTRAN_USEMPI_BINDINGS=0
    OMPI_WANT_FORTRAN_USEMPIF08_BINDINGS=0
fi
AS_IF([test $OMPI_WANT_FORTRAN_MPIFH_BINDINGS -eq 1 -o \
            $OMPI_WANT_FORTRAN_USEMPI_BINDINGS -eq 1 -o \
            $OMPI_WANT_FORTRAN_USEMPIF08_BINDINGS -eq 1],
      [OMPI_WANT_FORTRAN_BINDINGS=1],
      [OMPI_WANT_FORTRAN_BINDINGS=0])


#
# MPI profiling
#

AC_MSG_CHECKING([if want PMPI])
AC_ARG_ENABLE(mpi-profile,
    AC_HELP_STRING([--enable-mpi-profile],
                   [enable MPI profiling (default: enabled)]))
if test "$enable_mpi_profile" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_MPI_PROFILING=1
    MPIF_H_PMPI_W_FUNCS=", PMPI_WTICK, PMPI_WTIME"
else
    AC_MSG_RESULT([no])
    WANT_MPI_PROFILING=0
    MPIF_H_PMPI_W_FUNCS=
fi
AC_SUBST(MPIF_H_PMPI_W_FUNCS)


#
# C++
#

AC_MSG_CHECKING([if want C++ bindings])
AC_ARG_ENABLE(mpi-cxx,
    AC_HELP_STRING([--enable-mpi-cxx],
                   [enable C++ MPI bindings (default: enabled)]))
if test "$enable_mpi_cxx" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_MPI_CXX_SUPPORT=1
else
    AC_MSG_RESULT([no])
    WANT_MPI_CXX_SUPPORT=0
fi

AC_MSG_CHECKING([if want MPI::SEEK_SET support])
AC_ARG_ENABLE([mpi-cxx-seek],
    [AC_HELP_STRING([--enable-mpi-cxx-seek],
                   [enable support for MPI::SEEK_SET, MPI::SEEK_END, and MPI::SEEK_POS in C++ bindings (default: enabled)])])
if test "$enable_mpi_cxx_seek" != "no" ; then
  AC_MSG_RESULT([yes])
  OMPI_WANT_MPI_CXX_SEEK=1
else
  AC_MSG_RESULT([no])
  OMPI_WANT_MPI_CXX_SEEK=0
fi
AC_DEFINE_UNQUOTED([OMPI_WANT_MPI_CXX_SEEK], [$OMPI_WANT_MPI_CXX_SEEK],
    [do we want to try to work around C++ bindings SEEK_* issue?])


#
# Do we want to disable MPI parameter checking at run-time?
#

AC_MSG_CHECKING([if want run-time MPI parameter checking])
AC_ARG_WITH(mpi-param-check,
    AC_HELP_STRING([--with-mpi-param-check(=VALUE)],
                   [behavior of MPI function parameter checking.  Valid values are: always, never, runtime.  If --with-mpi-param-check is specified with no VALUE argument, it is equivalent to a VALUE of "always"; --without-mpi-param-check is equivalent to "never" (default: runtime).]))
mpi_param_check=ompi_mpi_param_check
if test "$with_mpi_param_check" = "no" -o \
    "$with_mpi_param_check" = "never"; then
    mpi_param_check=0
    ompi_param_check=0
    AC_MSG_RESULT([never])
elif test "$with_mpi_param_check" = "yes" -o \
    "$with_mpi_param_check" = "always"; then
    mpi_param_check=1
    ompi_param_check=1
    AC_MSG_RESULT([always])
elif test "$with_mpi_param_check" = "runtime" -o \
    -z "$with_mpi_params_check"; then
    ompi_param_check=1
    AC_MSG_RESULT([runtime])
else
    AC_MSG_RESULT([unknown])
    AC_MSG_WARN([*** Unrecognized --with-mpi-param-check value])
    AC_MSG_WARN([*** See "configure --help" output])
    AC_MSG_WARN([*** Defaulting to "runtime"])
fi
AC_DEFINE_UNQUOTED(MPI_PARAM_CHECK, $mpi_param_check,
    [Whether we want to check MPI parameters always, never, or decide at run-time])
AC_DEFINE_UNQUOTED(OMPI_PARAM_CHECK, $ompi_param_check,
    [Whether we want to check MPI parameters never or possible (an integer constant)])

#
# Do we want the prototype "use mpi_f08" implementation that uses
# Fortran descriptors?
#

AC_MSG_CHECKING([which 'use mpi_f08' implementation to use])
AC_ARG_ENABLE(mpi-f08-subarray-prototype,
    AC_HELP_STRING([--enable-mpi-f08-subarray-prototype],
                   [Use the PROTOTYPE and SEVERLY FUNCTIONALITY-LIMITED Fortran 08 'use mpi_f08' implementation that supports subarrrays (via Fortran descriptors).  This option will disable the normal 'use mpi_f08' implementation and *only* build the prototype implementation.]))
OMPI_BUILD_FORTRAN_F08_SUBARRAYS=0
AS_IF([test $OMPI_WANT_FORTRAN_USEMPIF08_BINDINGS -eq 0],
      [AC_MSG_RESULT([none (use mpi_f08 disabled)])],
      [AS_IF([test "$enable_mpi_f08_subarray_prototype" = "yes"],
             [OMPI_BUILD_FORTRAN_F08_SUBARRAYS=1
              AC_MSG_RESULT([extra crispy (subarray prototype)])],
             [AC_MSG_RESULT([regular (no subarray support)])])
      ])
AC_DEFINE_UNQUOTED([OMPI_BUILD_FORTRAN_F08_SUBARRAYS],
                   [$OMPI_BUILD_FORTRAN_F08_SUBARRAYS],
                   [Whether we built the 'use mpi_f08' prototype subarray-based implementation or not (i.e., whether to build the use-mpi-f08-desc prototype or the regular use-mpi-f08 implementation)])

#
# What is the max array rank that we want to support in the f90
# bindings?  Now only relevant for the ompi/mpi/fortran/use-mpi dir,
# which is now gfortran-only (because all other Fortran compilers will
# compile ompi/mpi/fortran/use-mpi-ignore-tkr).
#

OMPI_FORTRAN_MAX_ARRAY_RANK=4
AC_MSG_CHECKING([max supported gfortran array dimension in the "use mpi" Fortran module])
AC_ARG_WITH(gfortran-max-array-dim,
    AC_HELP_STRING([--with-gfortran-max-array-dim=<DIM>],
                   [The maximum array dimension supported in the gfortran-only "use mpi" module (default: $OMPI_FORTRAN_MAX_ARRAY_RANK).  This option is ignored when using other Fortran compilers]))
with_f90_max_array_dim=$gfortran_max_array_dim
if test ! -z "$with_f90_max_array_dim" -a "$with_f90_max_array_dim" != "no"; then
    # Ensure it's a number (hopefully an integer!), and >=1 and <=7
    happy=1
    expr $with_f90_max_array_dim + 1 > /dev/null 2> /dev/null
    AS_IF([test "$?" != "0"], [happy=0],
          [expr $with_f90_max_array_dim \>= 1 \& $with_f90_max_array_dim \<= 7 > /dev/null 2>/dev/null
           AS_IF([test "$?" != "0"], [happy=0])])

    # If badness in the above tests, bail
    AS_IF([test "$happy" = "0"],
          [AC_MSG_RESULT([bad value ($with_f90_max_array_dim)])
           AC_MSG_WARN([--with-f90-max-array-dim value must be >=1 and <=7])
           AC_MSG_ERROR([Cannot continue])])
    OMPI_FORTRAN_MAX_ARRAY_RANK="$with_f90_max_array_dim"
fi
AC_MSG_RESULT([$OMPI_FORTRAN_MAX_ARRAY_RANK])
AC_SUBST(OMPI_FORTRAN_MAX_ARRAY_RANK)


#
# A global check for ORTE to amke it easier to support the tools
# See note in orca_configure_options.m4
#
AC_MSG_CHECKING([if want ORTE supported OMPI tools])
if test "$ORCA_WITH_ORTE_SUPPORT" = "0"; then
    list_of_frameworks="pubsub-pmi,dpm-orte,pubsub-orte"
    if test -z $enable_mca_no_build ; then
        enable_mca_no_build="$list_of_frameworks"
    else
        enable_mca_no_build="$enable_mca_no_build,$list_of_frameworks"
    fi
    OMPI_WITH_ORTE_SUPPORTED_TOOLS=0
    AC_MSG_RESULT([no])
else
    OMPI_WITH_ORTE_SUPPORTED_TOOLS=1
    AC_MSG_RESULT([yes])
fi

AC_DEFINE_UNQUOTED([OMPI_WITH_ORTE_SUPPORTED_TOOLS],
    [$OMPI_WITH_ORTE_SUPPORTED_TOOLS],
    [Whether we want ORTE supported OMPI tools])
AM_CONDITIONAL(OMPI_WITH_ORTE_SUPPORTED_TOOLS, test "$OMPI_WITH_ORTE_SUPPORTED_TOOLS" = "1")

])dnl

