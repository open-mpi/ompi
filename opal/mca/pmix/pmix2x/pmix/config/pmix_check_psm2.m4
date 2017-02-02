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
# Copyright (c) 2009-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2016-2017 Intel, Inc.  All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# PMIX_CHECK_PSM2(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if PSM2 support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([PMIX_CHECK_PSM2],[
    if test -z "$pmix_check_psm2_happy" ; then
	AC_ARG_WITH([psm2],
		    [AC_HELP_STRING([--with-psm2(=DIR)],
				    [Build PSM2 (Intel PSM2) support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
	PMIX_CHECK_WITHDIR([psm2], [$with_psm2], [include/psm2.h])
	AC_ARG_WITH([psm2-libdir],
		    [AC_HELP_STRING([--with-psm2-libdir=DIR],
				    [Search for PSM (Intel PSM2) libraries in DIR])])
	PMIX_CHECK_WITHDIR([psm2-libdir], [$with_psm2_libdir], [libpsm2.*])

	pmix_check_psm2_$1_save_CPPFLAGS="$CPPFLAGS"
	pmix_check_psm2_$1_save_LDFLAGS="$LDFLAGS"
	pmix_check_psm2_$1_save_LIBS="$LIBS"

	AS_IF([test "$with_psm2" != "no"],
              [AS_IF([test ! -z "$with_psm2" && test "$with_psm2" != "yes"],
                     [pmix_check_psm2_dir="$with_psm2"])
               AS_IF([test ! -z "$with_psm2_libdir" && test "$with_psm2_libdir" != "yes"],
                     [pmix_check_psm2_libdir="$with_psm2_libdir"])

               PMIX_CHECK_PACKAGE([pmix_check_psm2],
				  [psm2.h],
				  [psm2],
				  [psm_mq_irecv],
				  [],
				  [$pmix_check_psm2_dir],
				  [$pmix_check_psm2_libdir],
				  [pmix_check_psm2_happy="yes"],
				  [pmix_check_psm2_happy="no"])],
              [pmix_check_psm2_happy="no"])

	CPPFLAGS="$pmix_check_psm2_$1_save_CPPFLAGS"
	LDFLAGS="$pmix_check_psm2_$1_save_LDFLAGS"
	LIBS="$pmix_check_psm2_$1_save_LIBS"

	AS_IF([test "$pmix_check_psm2_happy" = "yes" && test "$enable_progress_threads" = "yes"],
              [AC_MSG_WARN([PSM2 driver does not currently support progress threads.  Disabling MTL.])
               pmix_check_psm2_happy="no"])

        AS_IF([test "$pmix_check_psm2_happy" = "yes"],
              [AC_CHECK_HEADERS(
               glob.h,
               [],
               [AC_MSG_WARN([glob.h not found.  Can not build component.])
               pmix_check_psm2_happy="no"])])

    fi

    AS_IF([test "$pmix_check_psm2_happy" = "yes"],
          [$1_LDFLAGS="[$]$1_LDFLAGS $pmix_check_psm2_LDFLAGS"
	   $1_CPPFLAGS="[$]$1_CPPFLAGS $pmix_check_psm2_CPPFLAGS"
	   $1_LIBS="[$]$1_LIBS $pmix_check_psm2_LIBS"
	   $2],
          [AS_IF([test ! -z "$with_psm2" && test "$with_psm2" != "no"],
                 [AC_MSG_ERROR([PSM2 support requested but not found.  Aborting])])
           $3])
])
