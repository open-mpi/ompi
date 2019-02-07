# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
# Copyright (c) 2019      Triad National Security, LLC. All rights
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
AC_DEFUN([MCA_opal_threads_pthreads_PRIORITY], [30])

AC_DEFUN([MCA_opal_threads_pthreads_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

AC_DEFUN([MCA_opal_threads_pthreads_POST_CONFIG],[
    AS_IF([test "$1" = "1"], [threads_base_include="pthreads/threads_pthreads_threads.h"])
])dnl

AC_DEFUN([MCA_opal_mutex_pthreads_POST_CONFIG],[
    AS_IF([test "$1" = "1"], [mutex_base_include="pthreads/threads_pthreads_mutex.h"])
    AC_MSG_CHECKING([mutex_base_include = $mutex_base_include])
])dnl

AC_DEFUN([MCA_opal_tsd_pthreads_POST_CONFIG],[
    AS_IF([test "$1" = "1"], [threads_base_include="pthreads/threads_pthreads_tsd.h"])
    AC_MSG_CHECKING([threads_base_include = $threads_base_include])
])dnl

AC_DEFUN([MCA_opal_wait_sync_pthreads_POST_CONFIG],[
    AS_IF([test "$1" = "1"], [wait_sync_base_include="pthreads/threads_pthreads_wait_sync.h"])
    AC_MSG_CHECKING([wait_sync_includenclude = $wait_sync_base_include])
])dnl

# MCA_threads_pthreads_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_threads_pthreads_CONFIG],[
    AC_CONFIG_FILES([opal/mca/threads/pthreads/Makefile])

    AC_MSG_CHECKING([HAVE_THREAD_PKG_TYPE = $HAVE_THREAD_PKG_TYPE])

    AS_IF([test "$HAVE_THREAD_PKG_TYPE" = "pthreads"],
          [$1],
          [$2])
])
