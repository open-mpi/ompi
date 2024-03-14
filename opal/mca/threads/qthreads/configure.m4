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
# Copyright (c) 2019-2022 Triad National Security, LLC. All rights
#                         reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([OPAL_CONFIG_QTHREADS],[

    AC_ARG_WITH([qthreads],
                [AS_HELP_STRING([--with-qthreads=DIR],
                                [Specify location of qthreads installation.  Error if qthreads support cannot be found.])])

    AC_ARG_WITH([qthreads-libdir],
                [AS_HELP_STRING([--with-qthreads-libdir=DIR],
                                [Search for qthreads libraries in DIR])])

    opal_check_qthreads_save_CPPFLAGS=$CPPFLAGS
    opal_check_qthreads_save_LDFLAGS=$LDFLAGS
    opal_check_qthreads_save_LIBS=$LIBS

    OAC_CHECK_PACKAGE([qthreads],
                      [opal_qthreads],
                      [qthread.h],
                      [qthread],
                      [qthread_initialize],
                      [opal_qthreads_happy=yes],
                      [opal_qthreads_happy=no])

    AS_IF([test $opal_qthreads_happy = yes],
          [TPKG_CFLAGS="$opal_qthreads_CPPFLAGS"
           TPKG_FCFLAGS="$opal_qthreads_CPPFLAGS"
           TPKG_CXXFLAGS="$opal_qthreads_CPPFLAGS"
           TPKG_CPPFLAGS="$opal_qthreads_CPPFLAGS"
           TPKG_CXXCPPFLAGS="$opal_qthreads_CPPFLAGS"
           TPKG_LDFLAGS="$opal_qthreads_LDFLAGS"
           TPKG_LIBS="$opal_qthreads_LIBS"])

    AC_SUBST([opal_qthreads_CPPFLAGS])
    AC_SUBST([opal_qthreads_LDFLAGS])
    AC_SUBST([opal_qthreads_LIBS])

    CPPFLAGS="${opal_check_argo_save_CPPFLAGS} ${opal_qthreads_CPPFLAGS}"
    LDFLAGS=$opal_check_qthreads_save_LDFLAGS
    LIBS=$opal_check_qthreads_save_LIBS

    AS_IF([test "$opal_qthreads_happy" = "yes"],
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
           THREAD_CFLAGS="$TPKG_CFLAGS"
           THREAD_FCFLAGS="$TPKG_FCFLAGS"
           THREAD_CXXFLAGS="$TPKG_CXXFLAGS"
           THREAD_CPPFLAGS="$TPKG_CPPFLAGS"
           THREAD_CXXCPPFLAGS="$TPKG_CXXCPPFLAGS"
           THREAD_LDFLAGS="$TPKG_LDFLAGS"
           THREAD_LIBS="$TPKG_LIBS"
           LIBS="$LIBS $THREAD_LIBS"
           LDFLAGS="$LDFLAGS $THREAD_LDFLAGS"
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
