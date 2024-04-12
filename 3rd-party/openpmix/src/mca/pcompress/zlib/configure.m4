# -*- shell-script -*-
#
# Copyright (c) 2009-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
# Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
#                         All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pcompress_zlib_CONFIG([action-if-can-compile],
#                           [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_pcompress_zlib_CONFIG],[
    AC_CONFIG_FILES([src/mca/pcompress/zlib/Makefile])

    AC_ARG_WITH([zlib],
                [AS_HELP_STRING([--with-zlib=DIR],
                                [Search for zlib headers and libraries in DIR ])])
    AC_ARG_WITH([zlib-libdir],
                [AS_HELP_STRING([--with-zlib-libdir=DIR],
                                [Search for zlib libraries in DIR ])])

    pmix_zlib_support=0

    OAC_CHECK_PACKAGE([zlib],
                      [pcompress_zlib],
                      [zlib.h],
                      [z],
                      [deflate],
                      [pmix_zlib_support=1],
                      [pmix_zlib_support=0])

    if test ! -z "$with_zlib" && test "$with_zlib" != "no" && test "$pmix_zlib_support" != "1"; then
        AC_MSG_WARN([ZLIB SUPPORT REQUESTED AND NOT FOUND])
        AC_MSG_ERROR([CANNOT CONTINUE])
    fi

    AC_MSG_CHECKING([will zlib support be built])
    if test "$pmix_zlib_support" != "1"; then
        AC_MSG_RESULT([no])
        AC_MSG_WARN([*************************************************])
        AC_MSG_WARN([* PMIx was unable to find a usable version      *])
        AC_MSG_WARN([* of zlib and zlib-devel on the system. We will *])
        AC_MSG_WARN([* be unable to compress large data streams.     *])
        AC_MSG_WARN([* This may result in longer-than-normal startup *])
        AC_MSG_WARN([* times and larger memory footprints. We will   *])
        AC_MSG_WARN([* continue, but strongly recommend installing   *])
        AC_MSG_WARN([* zlib for better user experience.              *])
        AC_MSG_WARN([*************************************************])
    else
        AC_MSG_RESULT([yes])
    fi

    AS_IF([test "$pmix_zlib_support" = "1"],
          [$1],
          [$2])

    PMIX_SUMMARY_ADD([External Packages], [ZLIB], [], [${pcompress_zlib_SUMMARY}])

    # substitute in the things needed to build pcompress/zlib
    AC_SUBST([pcompress_zlib_CPPFLAGS])
    AC_SUBST([pcompress_zlib_LDFLAGS])
    AC_SUBST([pcompress_zlib_LIBS])

    PMIX_EMBEDDED_LIBS="$PMIX_EMBEDDED_LIBS $pcompress_zlib_LIBS"
    PMIX_EMBEDDED_LDFLAGS="$PMIX_EMBEDDED_LDFLAGS $pcompress_zlib_LDFLAGS"
    PMIX_EMBEDDED_CPPFLAGS="$PMIX_EMBEDDED_CPPFLAGS $pcompress_zlib_CPPFLAGS"

])dnl
