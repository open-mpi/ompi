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
# Copyright (c) 2006-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2019      Intel, Inc.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# 1. if --with-sge is given, always build
# 2. if --without-sge is given, never build
# 3. if neither is given, build if-and-only-if you find either qrsh in path or
# sge_root in environment

# PRTE_CHECK_GRIDENGINE(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([PRTE_CHECK_GRIDENGINE],[
    if test -z "$prte_gridengine_build" ; then
	AC_ARG_WITH([sge],
                    [AS_HELP_STRING([--with-sge],
                                    [Build SGE or Grid Engine support (default: no)])])

	AC_MSG_CHECKING([if user requested SGE build])
	prte_gridengine_build="no"
	AS_IF([test "$with_sge" = "yes"],
              [AC_MSG_RESULT([yes])
               prte_gridengine_build=yes],
              [AS_IF([test "$with_sge" = "no"],
                     [AC_MSG_RESULT([no])],
                     [AC_MSG_RESULT([not specified; checking environment])
                      AC_CHECK_PROG([QRSH], [qrsh], [qrsh])
                      AS_IF([test "$QRSH" != ""],
                            [prte_gridengine_build=yes],
                            [AC_MSG_CHECKING([for SGE_ROOT environment variable])
                             AS_IF([test "$SGE_ROOT" != ""],
				   [AC_MSG_RESULT([found])
                                    prte_gridengine_build=yes],
				   [AC_MSG_RESULT([not found])])])])])

	PRTE_SUMMARY_ADD([Resource Managers], [Grid Engine], [], [$prte_gridengine_build])
    fi

    AS_IF([test "$prte_gridengine_build" = "yes"],
          [$2],
          [$3])
])
