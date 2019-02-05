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
# Copyright (c) 2019      Triad National Security, LLC. All rights
#                         reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([OPAL_CONFIG_QTHREADS],[

    AC_CHECK_HEADERS([qthread/qthread.h],
                     [AC_CHECK_LIB([qthread],[qthread_initialize],
                                    [threads_qthreads_happy="yes"],
                                    [threads_qthreads_happy="no"])],
                     [threads_qthreads_happy="no"])

    AS_IF([test "$threads_qthreads_happy" = "yes"],
          [$1],
          [$2])
])dnl

AC_DEFUN([MCA_opal_threads_qthreads_PRIORITY], [30])

AC_DEFUN([MCA_opal_threads_qthreads_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# If component was selected, $1 will be 1 and we should set the base header
AC_DEFUN([MCA_opal_threads_qthreads_POST_CONFIG],[
    AS_IF([test "$1" = "1"], 
          [opal_thread_type_found="qthreads"
           AC_DEFINE_UNQUOTED([MCA_threads_base_include_HEADER],
                              ["opal/mca/threads/qthreads/threads_qthreads_threads.h"],
                              [Header to include for threads implementation])
           AC_DEFINE_UNQUOTED([MCA_threads_mutex_base_include_HEADER],
                              ["opal/mca/threads/qthreads/threads_qthreads_mutex.h"],
                              [Header to include for mutex implementation])
           AC_DEFINE_UNQUOTED([MCA_threads_tsd_base_include_HEADER],
                              ["opal/mca/threads/qthreads/threads_qthreads_tsd.h"],
                              [Header to include for tsd implementation])
           AC_DEFINE_UNQUOTED([MCA_threads_wait_sync_base_include_HEADER],
                              ["opal/mca/threads/qthreads/threads_qthreads_wait_sync.h"],
                              [Header to include for wait_sync implementation])
         ])
])dnl


# MCA_threads_qthreads_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_threads_qthreads_CONFIG],[
    AC_CONFIG_FILES([opal/mca/threads/qthreads/Makefile])

    AS_IF([test "$with_threads" = "qthreads"],
          [OPAL_CONFIG_QTHREADS([qthreads_works=1],[qthreads_works=0])],
          [qthreads_works=0])

    AS_IF([test "$qthreads_works" = "1"],
          [$1
           opal_thread_type_found="qthreads"],
          [$2])
])
