dnl -*- shell-script -*-
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
dnl Copyright (c) 2019      Sandia National Laboratories.  All rights reserved.
dnl Copyright (c) 2019      Triad National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl need in the core library
AC_DEFUN([MCA_opal_threads_CORE_LIB], [1])

dnl we only want one :)
m4_define(MCA_opal_threads_CONFIGURE_MODE, STOP_AT_FIRST)

AC_DEFUN([MCA_opal_threads_CONFIG],[
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
# First see what kind of threads we are going to use
#

AC_ARG_WITH([threads],
            [AS_HELP_STRING([--with-threads=TYPE],
                        [Specify thread TYPE to use. default:pthreads. Other options are qthreads and argobots.])])

#
# Configure components
#

MCA_CONFIGURE_FRAMEWORK($1, $2, 1)

AS_IF([test x"$opal_thread_type_found" = x""],
      [AC_MSG_ERROR([Did not find a suitable threads component])])

AC_MSG_RESULT([Found thread type $opal_thread_type_found])

AC_SUBST(THREAD_CFLAGS)
AC_SUBST(THREAD_FCFLAGS)
AC_SUBST(THREAD_CXXFLAGS)
AC_SUBST(THREAD_CPPFLAGS)
AC_SUBST(THREAD_LDFLAGS)
AC_SUBST(THREAD_LIBS)

OPAL_SUMMARY_ADD([Miscellaneous], [Threading Package], [], [$opal_thread_type_found])
])dnl
