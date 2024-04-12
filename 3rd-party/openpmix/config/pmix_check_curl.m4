# -*- autoconf -*-
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
# Copyright (c) 2009-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# PMIX_CHECK_CURL
# --------------------------------------------------------
# check if CURL support can be found.  sets curl_{CPPFLAGS,
# LDFLAGS, LIBS}
AC_DEFUN([PMIX_CHECK_CURL],[
    dnl Intentionally disable CURL unless explicitly requested
    AC_ARG_WITH([curl],
                [AS_HELP_STRING([--with-curl(=DIR)],
                   [Build curl support (default=no), optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])],
                [], [with_curl=no])
    AC_ARG_WITH([curl-libdir],
            [AS_HELP_STRING([--with-curl-libdir=DIR],
                    [Search for Curl libraries in DIR])])

    dnl We set the curl configure arguments before pkg-config support,
    dnl so we need to explicitly set the module name.
    m4_define([curl_pkgconfig_module], [libcurl])
    OAC_CHECK_PACKAGE([curl],
                      [pmix_check_curl],
                      [curl/curl.h],
		      [curl],
		      [curl_easy_getinfo],
		      [pmix_check_curl_happy="yes"],
		      [pmix_check_curl_happy="no"])

    AS_IF([test "$pmix_check_curl_happy" = "no" -a "$with_curl" != "no"],
          [AC_MSG_ERROR([curl support requested but not found.  Aborting])])

    AC_MSG_CHECKING([libcurl support available])
    AC_MSG_RESULT([$pmix_check_curl_happy])

    PMIX_SUMMARY_ADD([External Packages], [Curl], [], [$pmix_check_curl_SUMMARY])
])
