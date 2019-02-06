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
])dnl

AC_DEFUN([MCA_opal_tsd_pthreads_POST_CONFIG],[
    AS_IF([test "$1" = "1"], [threads_base_include="pthreads/threads_pthreads_tsd.h"])
])dnl

AC_DEFUN([MCA_opal_wait_sync_pthreads_POST_CONFIG],[
    AS_IF([test "$1" = "1"], [mutex_base_include="pthreads/threads_pthreads_wait_sync.h"])
])dnl

# MCA_threads_pthreads_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_threads_pthreads_CONFIG],[
    AC_CONFIG_FILES([opal/mca/threads/pthreads/Makefile])

    AS_IF([test "$with_threads" = "pthreads"],
          [threads_pthreads_happy="yes"
           threads_pthreads_should_use=1],
          [threads_pthreads_should_use=0
           AS_IF([test "$with_threads" = ""],
                 [threads_pthreads_happy="yes"],
                 [threads_pthreads_happy="no"])])

    AS_IF([test "$threads_pthreads_happy" = "yes"],
          [OPAL_CONFIG_POSIX_THREADS([threads_pthreads_happy="yes"],
                                     [threads_pthreads_happy="no"])])

   AS_IF([test "$threads_pthreads_happy" = "no" && \
          test "$threads_pthreads_should_use" = "1"],
         [AC_MSG_ERROR([pthreads threads requested but not available.  Aborting.])])

    AS_IF([test "$threads_pthreads_happy" = "yes"],
          [$1],
          [$2])
])
