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

    AC_ARG_WITH([argobots],
                [AC_HELP_STRING([--with-argobots=DIR],
                                [Specify location of argobots installation.  Error if argobots support cannot be found.])])

    AC_ARG_WITH([argobots-libdir],
                [AC_HELP_STRING([--with-argobots-libdir=DIR],
                                [Search for argobots libraries in DIR])])

    opal_check_argo_save_CPPFLAGS=$CPPFLAGS
    opal_check_argo_save_LDFLAGS=$LDFLAGS
    opal_check_argo_save_LIBS=$LIBS

    opal_argo_happy=yes
    AS_IF([test "$with_argo" = "no"],
          [opal_argo_happy=no])

    AS_IF([test $opal_argo_happy = yes],
          [AC_MSG_CHECKING([looking for argobots in])
           AS_IF([test "$with_argobots" != "yes"],
                 [opal_argo_dir=$with_argobots
                  AC_MSG_RESULT([($opal_argo_dir)])],
                 [AC_MSG_RESULT([(default search paths)])])
           AS_IF([test ! -z "$with_argobots_libdir" && \
                         test "$with_argobots_libdir" != "yes"],
                 [opal_argo_libdir=$with_argobots_libdir])
          ])

    AS_IF([test $opal_argo_happy = yes],
          [OPAL_CHECK_PACKAGE([opal_argo],
                              [abt.h],
                              [abt],
                              [ABT_init],
                              [],
                              [$opal_argo_dir],
                              [$opal_argo_libdir],
                              [],
                              [opal_argo_happy=no])])

    AS_IF([test $opal_argo_happy = yes && test -n "$opal_argo_dir"],
          [OPAL_ARGO_INCLUDE_PATH="$opal_argo_dir/include/"],
          [OPAL_ARGO_INCLUDE_PATH=""])

    AS_IF([test $opal_argo_happy = yes],
          [AC_CONFIG_FILES([opal/mca/threads/argobots/threads_argobots.h])
           AC_SUBST([OPAL_ARGO_INCLUDE_PATH])
           AC_SUBST([opal_argo_CPPFLAGS])
           AC_SUBST([opal_argo_LDFLAGS])
           AC_SUBST([opal_argo_LIBS])
           TPKG_CFLAGS="$opal_argo_CPPFLAGS"
           TPKG_FCFLAGS="$opal_argo_CPPFLAGS"
           TPKG_CXXFLAGS="$opal_argo_CPPFLAGS"
           TPKG_CPPFLAGS="$opal_argo_CPPFLAGS"
           TPKG_CXXCPPFLAGS="$opal_argo_CPPFLAGS"
           TPKG_LDFLAGS="$opal_argo_LDFLAGS"
           TPKG_LIBS="$opal_argo_LIBS"])

    CPPFLAGS=$opal_check_argo_save_CPPFLAGS
    LDFLAGS=$opal_check_argo_save_LDFLAGS
    LIBS=$opal_check_argo_save_LIBS

    AS_IF([test "$opal_argo_happy" = "yes"],
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
