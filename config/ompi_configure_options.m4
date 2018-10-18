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
dnl Copyright (c) 2006-2017 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2009-2018 Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2013      Intel, Inc.  All rights reserved.
dnl Copyright (c) 2015-2018 Research Organization for Information Science
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
    [Whether we want sparse process groups])


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
                   [specify which Fortran MPI bindings to build: yes, none (or no), best-effort, mpifh (build only mpif.h support), usempi (build mpif.h and the mpi module), or usempif08 (or all, build mpifh, the mpi module, and the mpi_f08 module) (default: "yes" if Fortran compiler found)]))

# These are the 4 monotonically-rising values indicating levels of
# Fortran bindings support.
OMPI_FORTRAN_NO_BINDINGS=0
OMPI_FORTRAN_MPIFH_BINDINGS=1
OMPI_FORTRAN_USEMPI_BINDINGS=2
OMPI_FORTRAN_USEMPIF08_BINDINGS=3

# Set this variable to minimum the level of Fortran bindings support
# that is *required* (i.e., if we can't achieve this level, then
# configure should abort).
OMPI_MIN_REQUIRED_FORTRAN_BINDINGS=$OMPI_FORTRAN_NO_BINDINGS
# Set this variable to the highest level of Fortran bindings support
# that should be attempted.  This value will never be <=
# $OMPI_MIN_REQUIRED_FORTRAN_BINDINGS.
OMPI_TRY_FORTRAN_BINDINGS=$OMPI_FORTRAN_NO_BINDINGS

case "x$enable_mpi_fortran" in
    x|xbest-effort)
        AC_MSG_RESULT([ (try)])
        OMPI_MIN_REQUIRED_FORTRAN_BINDINGS=$OMPI_FORTRAN_NO_BINDINGS
        OMPI_TRY_FORTRAN_BINDINGS=$OMPI_FORTRAN_USEMPIF08_BINDINGS
        ;;

    xyes)
        AC_MSG_RESULT([yes (default)])
        OMPI_MIN_REQUIRED_FORTRAN_BINDINGS=$OMPI_FORTRAN_MPIFH_BINDINGS
        OMPI_TRY_FORTRAN_BINDINGS=$OMPI_FORTRAN_USEMPIF08_BINDINGS
        ;;

    xall|xusempif08)
        AC_MSG_RESULT([all (usempif08)])
        OMPI_MIN_REQUIRED_FORTRAN_BINDINGS=$OMPI_FORTRAN_USEMPIF08_BINDINGS
        OMPI_TRY_FORTRAN_BINDINGS=$OMPI_FORTRAN_USEMPIF08_BINDINGS
        ;;

    xno|xnone)
        AC_MSG_RESULT([no (none)])
        OMPI_MIN_REQUIRED_FORTRAN_BINDINGS=$OMPI_FORTRAN_NO_BINDINGS
        OMPI_TRY_FORTRAN_BINDINGS=$OMPI_FORTRAN_NO_BINDINGS
        ;;

    xmpifh)
        AC_MSG_RESULT([yes (mpif.h)])
        OMPI_MIN_REQUIRED_FORTRAN_BINDINGS=$OMPI_FORTRAN_MPIFH_BINDINGS
        OMPI_TRY_FORTRAN_BINDINGS=$OMPI_FORTRAN_MPIFH_BINDINGS
        ;;

    xusempi)
        AC_MSG_RESULT([yes (mpif.h, mpi module)])
        OMPI_MIN_REQUIRED_FORTRAN_BINDINGS=$OMPI_FORTRAN_USEMPI_BINDINGS
        OMPI_TRY_FORTRAN_BINDINGS=$OMPI_FORTRAN_USEMPI_BINDINGS
        ;;

    *)
        AC_MSG_RESULT([unknown: $binding])
        AC_MSG_WARN([--enable-mpi-fortran supports only one of the following values: yes, all, none, best-effort, mpifh, usempi, or usempif08])
        AC_MSG_ERROR([Cannot continue])
        ;;
esac

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

# Remove these when we finally kill them once and for all
AC_ARG_ENABLE([mpi1-compatibility],
    [AC_HELP_STRING([--enable-mpi1-compatibility],
                    [Enable support for MPI-1.x functions removed from the current MPI standard (MPI-3.x). This option will go away in a future release of Open MPI (default: disabled)])])

if test "x$enable_mpi1_compatibility" = "xyes" ; then
ompi_mpi1_support=1
else
ompi_mpi1_support=0
fi


AC_DEFINE_UNQUOTED([OMPI_ENABLE_MPI1_COMPAT], [$ompi_mpi1_support], [whether we want MPI-1.x support])
AC_SUBST([OMPI_ENABLE_MPI1_COMPAT], [$ompi_mpi1_support])
AM_CONDITIONAL([OMPI_ENABLE_MPI1_COMPAT],[test $ompi_mpi1_support = 1])

AC_ARG_ENABLE([grequest-extensions],
    [AC_HELP_STRING([--enable-grequest-extensions],
                    [Enable support for Grequest extensions (default: disabled)])])

if test "x$enable_grequest_extensions" = "xyes" ; then
ompi_grequest_extensions=1
else
ompi_grequest_extensions=0
fi


AC_DEFINE_UNQUOTED([OMPI_ENABLE_GREQUEST_EXTENSIONS], [$ompi_grequest_extensions], [whether we support Grequest extensions])
AC_SUBST([OMPI_ENABLE_GREQUEST_EXTENSIONS], [$ompi_grequest_extensions])
AM_CONDITIONAL([OMPI_ENABLE_GREQUEST_EXTENSIONS],[test $ompi_grequest_extensions = 1])

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

AC_ARG_ENABLE([io-ompio],
    [AC_HELP_STRING([--disable-io-ompio],
        [Disable the ompio MPI-IO component])])

])dnl

