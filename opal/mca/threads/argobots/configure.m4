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
#                         Reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([OPAL_CONFIG_ARGOBOTS_THREADS],[
    AC_CHECK_HEADERS([abt.h],
                     [AC_CHECK_LIB([abt],[ABT_init],
                                    [threads_argobots_happy="yes"],
                                    [threads_argobots_happy="no"])],
                     [threads_argobots_happy="no"])

    AS_IF([test "$threads_argobots_happy" = "yes"],
          [$1],
          [$2])
])dnl


AC_DEFUN([MCA_opal_threads_argobots_PRIORITY], [30])

AC_DEFUN([MCA_opal_threads_argobots_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

# If component was selected, $1 will be 1 and we should set the base header
AC_DEFUN([MCA_opal_threads_argobots_POST_CONFIG],[
    AS_IF([test "$1" = "1"], 
          [opal_thread_type_found="argobots"
           AC_DEFINE_UNQUOTED([MCA_threads_base_include_HEADER],
                              ["opal/mca/threads/argobots/threads_argobots_threads.h"],
                              [Header to include for threads implementation])
           AC_DEFINE_UNQUOTED([MCA_threads_mutex_base_include_HEADER],
                              ["opal/mca/threads/argobots/threads_argobots_mutex.h"],
                              [Header to include for mutex implementation])
           AC_DEFINE_UNQUOTED([MCA_threads_tsd_base_include_HEADER],
                              ["opal/mca/threads/argobots/threads_argobots_tsd.h"],
                              [Header to include for tsd implementation])
           AC_DEFINE_UNQUOTED([MCA_threads_wait_sync_base_include_HEADER],
                              ["opal/mca/threads/argobots/threads_argobots_wait_sync.h"],
                              [Header to include for wait_sync implementation])
          ])

])dnl

# MCA_threads_argobots_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_threads_argobots_CONFIG],[
    AC_CONFIG_FILES([opal/mca/threads/argobots/Makefile])

    AS_IF([test "$with_threads" = "argobots"],
          [OPAL_CONFIG_ARGOBOTS_THREADS([argobots_threads_works=1], [argobots_threads_works=0])],
          [argobots_threads_works=0])

    AS_IF([test "$argobots_threads_works" = "1"],
          [$1
           opal_thread_type_found="argobots"],
          [$2])
])
