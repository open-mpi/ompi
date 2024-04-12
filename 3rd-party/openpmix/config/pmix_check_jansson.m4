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

# PMIX_CHECK_JANSSON
# --------------------------------------------------------
# check if JANSSON support can be found.  sets jansson_{CPPFLAGS,
# LDFLAGS, LIBS}
AC_DEFUN([PMIX_CHECK_JANSSON],[
    PMIX_VAR_SCOPE_PUSH([pmix_check_jansson_save_CPPFLAGS])

    dnl Intentionally disable Jansson unless explicitly requested
    AC_ARG_WITH([jansson],
                [AS_HELP_STRING([--with-jansson(=DIR)],
                   [Build jansson support (default=no), optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])],
                [], [with_jansson=no])
    AC_ARG_WITH([jansson-libdir],
            [AS_HELP_STRING([--with-jansson-libdir=DIR],
                    [Search for Jansson libraries in DIR])])

    OAC_CHECK_PACKAGE([jansson],
                      [pmix_check_jansson],
                      [jansson.h],
	 	      [jansson],
		      [json_loads],
                      [pmix_check_jansson_happy="yes"],
		      [pmix_check_jansson_happy="no"])

    if test "$pmix_check_jansson_happy" = "yes"; then
        AC_MSG_CHECKING([if libjansson version is 2.11 or greater])
        pmix_check_jansson_save_CPPFLAGS="${CPPFLAGS}"
        PMIX_FLAGS_APPEND_UNIQ([CPPFLAGS], [${pmix_check_jansson_CPPFLAGS}])
        AC_COMPILE_IFELSE(
              [AC_LANG_PROGRAM([[#include <jansson.h>]],
              [[
        #if JANSSON_VERSION_HEX < 0x00020b00
        #error "jansson API version is less than 2.11"
        #endif
                  ]])],
              [AC_MSG_RESULT([yes])],
              [AC_MSG_RESULT([no])
               pmix_check_jansson_happy=no])
    	CPPFLAGS="$pmix_check_jansson_save_CPPFLAGS"
    fi

    AS_IF([test "$pmix_check_jansson_happy" = "no" -a "$with_jansson" != "no"],
          [AC_MSG_ERROR([Jansson support requested but not found.  Aborting])])

    AC_MSG_CHECKING([Jansson support available])
    AC_MSG_RESULT([$pmix_check_jansson_happy])

    AM_CONDITIONAL([HAVE_JANSSON], [test "$pmix_check_jansson_happy" = "yes"])

    PMIX_SUMMARY_ADD([External Packages], [Jansson], [], [${pmix_check_jansson_SUMMARY}])

    PMIX_VAR_SCOPE_POP
])
