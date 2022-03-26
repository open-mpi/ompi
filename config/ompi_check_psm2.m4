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
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      QLogic Corp. All rights reserved.
# Copyright (c) 2009-2021 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016      Intel Corporation. All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
#  Copyright (c) 2021     Triad National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_CHECK_PSM2(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if PSM2 support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_PSM2],[
    OPAL_VAR_SCOPE_PUSH([opal_psm2_CPPFLAGS_save opal_psm2_LDFLAGS_save opal_psm2_LIBS_save])

    AC_ARG_WITH([psm2],
                [AS_HELP_STRING([--with-psm2(=DIR)],
                    [Build PSM2 (Intel PSM2) support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    AC_ARG_WITH([psm2-libdir],
                [AS_HELP_STRING([--with-psm2-libdir=DIR],
                    [Search for PSM (Intel PSM2) libraries in DIR])])

    AC_ARG_ENABLE([psm2-version-check],
                  [AS_HELP_STRING([--disable-psm2-version-check],
                      [Disable PSM2 version checking.  Not recommended to disable. (default: enabled)])])

    OAC_CHECK_PACKAGE([psm2],
                      [$1],
                      [psm2.h],
                      [psm2],
                      [psm2_mq_irecv2],
                      [ompi_check_psm2_happy="yes"],
                      [ompi_check_psm2_happy="no"])

    AS_IF([test "$ompi_check_psm2_happy" = "yes" && test "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([PSM2 driver does not currently support progress threads.  Disabling MTL.])
           $1_SUMMARY="no (progress threads not supported)"
           ompi_check_psm2_happy="no"])

    opal_psm2_CPPFLAGS_save="$CPPFLAGS"
    CPPFLAGS="${$1_CPPFLAGS} ${CPPFLAGS}"

    AS_IF([test "$ompi_check_psm2_happy" = "yes"],
          [AC_CHECK_HEADERS([glob.h], []
               [AC_MSG_WARN([glob.h not found.  Can not build component.])
                $1_SUMMARY="no (glob.h not found))"
                ompi_check_psm2_happy="no"])])

    AS_IF([test "$ompi_check_psm2_happy" = "yes"],
          [AC_CHECK_DECL([PSM2_LIB_REFCOUNT_CAP],
                         [AC_DEFINE([HAVE_PSM2_LIB_REFCOUNT_CAP], [1],
                                    [have PSM2_LIB_REFCOUNT_CAP in psm2.h])],
                         [AS_IF([test "x$enable_psm2_version_check" != "xno"],
                                [ompi_check_psm2_happy="no"
                                 $1_SUMMARY="no (version too old)"
                                 AC_MSG_WARN([PSM2 needs to be version 11.2.173 or later. Disabling MTL.])]
                         )],
                          [#include <psm2.h>])])

    CPPFLAGS="${opal_psm2_CPPFLAGS_save}"

    OPAL_SUMMARY_ADD([Transports], [Intel Omnipath (PSM2)], [], [${$1_SUMMARY}])

    AS_IF([test "$ompi_check_psm2_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_psm2" && test "$with_psm2" != "no"],
                 [AC_MSG_ERROR([PSM2 support requested but not found.  Aborting])])
           $3])
])
