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
dnl Copyright (c) 2004-2006 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2008-2016 University of Houston. All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OMPI_CHECK_PVFS2(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if PVFS2 support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_PVFS2],[

    check_pvfs2_CPPFLAGS=
    check_pvfs2_LDFLAGS=
    check_pvfs2_LIBS=

    check_pvfs2_configuration="none"
    ompi_check_pvfs2_happy="yes"


    # Get some configuration information
    AC_ARG_WITH([pvfs2],
        [AC_HELP_STRING([--with-pvfs2(=DIR)],
             [Build Pvfs2 support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OPAL_CHECK_WITHDIR([pvfs2], [$with_pvfs2], [include/pvfs2.h])

    AS_IF([test -z "$with_pvfs2"],
          [ompi_check_pvfs2_dir="/usr/local"],
          [ompi_check_pvfs2_dir=$with_pvfs2])

    if test -e "$ompi_check_pvfs2_dir/lib64" ; then
        ompi_check_pvfs2_libdir="$ompi_check_pvfs2_dir/lib64"
    else
        ompi_check_pvfs2_libdir="$ompi_check_pvfs2_dir/lib"
    fi

    # Add correct -I and -L flags
    OPAL_CHECK_PACKAGE([$1], [pvfs2.h], [pvfs2], [PVFS_util_resolve], [],
                       [$ompi_check_pvfs2_dir], [$ompi_check_pvfs2_libdir], [ompi_check_pvfs2_happy="yes"],
                       [ompi_check_pvfs2_happy="no"])


    AS_IF([test "$ompi_check_pvfs2_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_pvfs2" && test "$with_pvfs2" != "no"],
                  [echo PVFS2 support not found])
              $3])

])

