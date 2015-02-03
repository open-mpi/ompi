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
dnl Copyright (c) 2006-2014 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2009      Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2013      Intel, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CONFIGURE_OPTIONS],[
opal_show_subtitle "OMPI Configuration options"

#
# Disable MPI layer?
#
AC_ARG_ENABLE([mpi],
  [AC_HELP_STRING([--disable-mpi],
     [Disable building the MPI layer (default:enabled)])])

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
AC_MSG_CHECKING([if want Fortran MPI bindings])
AC_ARG_ENABLE(mpi-fortran,
    AC_HELP_STRING([--enable-mpi-fortran],
                   [specify which Fortran MPI bindings to build: all (or yes), none (or no), mpifh (build only mpif.h support), usempi (build mpif.h and the mpi module), or usempif08 (build mpifh, the mpi module, and the mpi_f08 module) (default: "all" if Fortran compiler found)]))

OMPI_FORTRAN_USER_REQUESTED=0
case "x$enable_mpi_fortran" in
    x)
        AC_MSG_RESULT([yes (all/default)])
        OMPI_WANT_FORTRAN_MPIFH_BINDINGS=1
        OMPI_WANT_FORTRAN_USEMPI_BINDINGS=1
        OMPI_WANT_FORTRAN_USEMPIF08_BINDINGS=1
        ;;

    xyes|xall)
        AC_MSG_RESULT([yes (all)])
        OMPI_FORTRAN_USER_REQUESTED=1
        OMPI_WANT_FORTRAN_MPIFH_BINDINGS=1
        OMPI_WANT_FORTRAN_USEMPI_BINDINGS=1
        OMPI_WANT_FORTRAN_USEMPIF08_BINDINGS=1
        ;;

    xno|xnone)
        AC_MSG_RESULT([no (none)])
        OMPI_WANT_FORTRAN_MPIFH_BINDINGS=0
        OMPI_WANT_FORTRAN_USEMPI_BINDINGS=0
        OMPI_WANT_FORTRAN_USEMPIF08_BINDINGS=0
        ;;

    xmpifh)
        AC_MSG_RESULT([yes (mpif.h)])
        OMPI_FORTRAN_USER_REQUESTED=1
        OMPI_WANT_FORTRAN_MPIFH_BINDINGS=1
        OMPI_WANT_FORTRAN_USEMPI_BINDINGS=0
        OMPI_WANT_FORTRAN_USEMPIF08_BINDINGS=0
        ;;

    xusempi)
        AC_MSG_RESULT([yes (mpif.h, mpi module)])
        OMPI_FORTRAN_USER_REQUESTED=1
        OMPI_WANT_FORTRAN_MPIFH_BINDINGS=1
        OMPI_WANT_FORTRAN_USEMPI_BINDINGS=1
        OMPI_WANT_FORTRAN_USEMPIF08_BINDINGS=0
        ;;

    xusempif08)
        AC_MSG_RESULT([yes (mpif.h, mpi and mpi_f08 modules)])
        OMPI_FORTRAN_USER_REQUESTED=1
        OMPI_WANT_FORTRAN_MPIFH_BINDINGS=1
        OMPI_WANT_FORTRAN_USEMPI_BINDINGS=1
        OMPI_WANT_FORTRAN_USEMPIF08_BINDINGS=1
        ;;

    *)
        AC_MSG_RESULT([unknown: $binding])
        AC_MSG_WARN([--enable-mpi-fortran only one of the following values: all, none, mpifh, usempi, or usempif08])
        AC_MSG_ERROR([Cannot continue])
        ;;
esac

AS_IF([test $OMPI_WANT_FORTRAN_MPIFH_BINDINGS -eq 1 || \
       test $OMPI_WANT_FORTRAN_USEMPI_BINDINGS -eq 1 || \
       test $OMPI_WANT_FORTRAN_USEMPIF08_BINDINGS -eq 1],
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
AM_CONDITIONAL(WANT_MPI_PROFILING, test "$WANT_MPI_PROFILING" = 1)


#
# C++
#

AC_MSG_CHECKING([if want C++ bindings])
AC_ARG_ENABLE(mpi-cxx,
    AC_HELP_STRING([--enable-mpi-cxx],
                   [enable C++ MPI bindings (default: disabled)]))
if test "$enable_mpi_cxx" = "yes"; then
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
if test "$with_mpi_param_check" = "no" || \
   test "$with_mpi_param_check" = "never"; then
    mpi_param_check=0
    ompi_param_check=0
    AC_MSG_RESULT([never])
elif test "$with_mpi_param_check" = "yes" || \
     test "$with_mpi_param_check" = "always"; then
    mpi_param_check=1
    ompi_param_check=1
    AC_MSG_RESULT([always])
elif test "$with_mpi_param_check" = "runtime" || \
     test -z "$with_mpi_params_check"; then
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
              AC_MSG_RESULT([extra crispy (subarray prototype)])
              AC_MSG_WARN([Sorry, the subarray prototype is no longer available])
              AC_MSG_WARN([Contact your favorite OMPI developer and ask for it to be re-enabled])
              AC_MSG_ERROR([Cannot continue])],
             [AC_MSG_RESULT([regular (no subarray support)])])
      ])
AC_DEFINE_UNQUOTED([OMPI_BUILD_FORTRAN_F08_SUBARRAYS],
                   [$OMPI_BUILD_FORTRAN_F08_SUBARRAYS],
                   [Whether we built the 'use mpi_f08' prototype subarray-based implementation or not (i.e., whether to build the use-mpi-f08-desc prototype or the regular use-mpi-f08 implementation)])

])dnl

