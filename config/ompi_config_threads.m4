dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CONFIG_THREADS],[
#
# Arguments: none
#
# Dependencies: None
#
# Modifies:
#  none - see called tests
#
# configure threads
#

#
# MPI_THREAD_MULTIPLE
#
# --enable-mpi-thread-multiple
#  #if OMPI_ENABLE_THREAD_MULTIPLE == 0 /* Not available */
#  #if OMPI_ENABLE_THREAD_MULTIPLE == 1 /* Available  */
#
AC_MSG_CHECKING([if want MPI_THREAD_MULTIPLE support])
AC_ARG_ENABLE([mpi_thread_multiple],
    [AC_HELP_STRING([--enable-mpi-thread-multiple],
                    [Enable MPI_THREAD_MULTIPLE support (default: disabled)])])

if test "$enable_mpi_thread_multiple" = "yes" ; then
    ompi_want_mpi_threads=1
    OMPI_ENABLE_THREAD_MULTIPLE=1
    AC_MSG_RESULT([Enabled])
else
    # Default: disable
    ompi_want_mpi_threads=0
    OMPI_ENABLE_THREAD_MULTIPLE=0
    AC_MSG_RESULT([Disabled])
fi
AC_DEFINE_UNQUOTED([OMPI_ENABLE_THREAD_MULTIPLE], [$ompi_want_mpi_threads],
                   [Enable MPI_THREAD_MULTIPLE])

])dnl

