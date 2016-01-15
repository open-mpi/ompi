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
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2014      Intel Corporation. All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
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
    AC_ARG_WITH([psm2],
        [AC_HELP_STRING([--with-psm2(=DIR)],
             [Build PSM2 (Intel PSM2) support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OPAL_CHECK_WITHDIR([psm2], [$with_psm2], [include/psm2.h])
    AC_ARG_WITH([psm2-libdir],
        [AC_HELP_STRING([--with-psm2-libdir=DIR],
             [Search for PSM (Intel PSM2) libraries in DIR])])
    OPAL_CHECK_WITHDIR([psm2-libdir], [$with_psm2_libdir], [libpsm2.*])

    ompi_check_psm2_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_psm2_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_psm2_$1_save_LIBS="$LIBS"

    AS_IF([test "$with_psm2" != "no"],
          [AS_IF([test ! -z "$with_psm2" && test "$with_psm2" != "yes"],
                 [ompi_check_psm2_dir="$with_psm2"])
           AS_IF([test ! -z "$with_psm2_libdir" && test "$with_psm2_libdir" != "yes"],
                 [ompi_check_psm2_libdir="$with_psm2_libdir"])

           OPAL_CHECK_PACKAGE([$1],
                              [psm2.h],
                              [psm2],
                              [psm2_mq_irecv2],
			      [],
                              [$ompi_check_psm2_dir],
                              [$ompi_check_psm2_libdir],
                              [ompi_check_psm2_happy="yes"],
                              [ompi_check_psm2_happy="no"])],
          [ompi_check_psm2_happy="no"])

    CPPFLAGS="$ompi_check_psm2_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_psm2_$1_save_LDFLAGS"
    LIBS="$ompi_check_psm2_$1_save_LIBS"

    AS_IF([test "$ompi_check_psm2_happy" = "yes" && test "$enable_progress_threads" = "yes"],
          [AC_MSG_WARN([PSM2 driver does not currently support progress threads.  Disabling MTL.])
           ompi_check_psm2_happy="no"])

    AS_IF([test "$ompi_check_psm2_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_psm2" && test "$with_psm2" != "no"],
                 [AC_MSG_ERROR([PSM2 support requested but not found.  Aborting])])
           $3])
])
