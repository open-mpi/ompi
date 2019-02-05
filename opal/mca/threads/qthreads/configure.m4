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
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#
AC_DEFUN([MCA_opal_threads_qthreads_PRIORITY], [30])

AC_DEFUN([MCA_opal_threads_qthreads_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])

AC_DEFUN([MCA_opal_threads_qthreads_POST_CONFIG],[
    AS_IF([test "$1" = "1"], [threads_base_include="qthreads/threads_qthreads.h"])
])dnl

# MCA_threads_qthreads_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_threads_qthreads_CONFIG],[
    AC_CONFIG_FILES([opal/mca/threads/qthreads/Makefile])

    AS_IF([test "$with_threads" = "qthreads"],
          [threads_qthreads_happy="yes"
           threads_qthreads_should_use=1],
          [threads_qthreads_should_use=0
           AS_IF([test "$with_threads" = ""],
                 [threads_qthreads_happy="yes"],
                 [threads_qthreads_happy="no"])])

    AS_IF([test "$threads_qthreads_happy" = "yes"],
          [AC_CHECK_HEADERS([mach/mach_time.h])
           AC_CHECK_FUNC([mach_absolute_time],
                         [threads_qthreads_happy="yes"],
                         [threads_qthreads_happy="no"])])

   AS_IF([test "$threads_qthreads_happy" = "no" && \
          test "$threads_qthreads_should_use" = "1"],
         [AC_MSG_ERROR([qthreads threads requested but not available.  Aborting.])])

    AS_IF([test "$threads_qthreads_happy" = "yes"],
          [$1],
          [$2])
])
