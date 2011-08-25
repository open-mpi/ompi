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
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008-2011 University of Houston. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_CHECK_LUSTRE(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if LUSTRE support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_LUSTRE],[

    check_lustre_CPPFLAGS=
    check_lustre_LDFLAGS=
    check_lustre_LIBS=

    check_lustre_configuration="none"
    ompi_check_lustre_happy="yes"

    # Get some configuration information
    AC_ARG_WITH([lustre],
        [AC_HELP_STRING([--with-lustre(=DIR)],
             [Build Lustre support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OMPI_CHECK_WITHDIR([lustre], [$with_lustre], [include/lustre/liblustreapi.h])

    AC_ARG_WITH([lustre-libs], 
        [AC_HELP_STRING([--with-lustre-libs=LIBS],
                       [Libraries to link with for lustre])])

    # Add correct -I and -L flags
    temp_lustre=$with_lustre
    AS_IF([test -n "$with_lustre"],
          [AS_IF([test -d "$with_lustre/include"],
                 [check_lustre_CPPFLAGS="-I$with_lustre/include"
                  CPPFLAGS="$CPPFLAGS $check_lustre_CPPFLAGS"], [])
           AS_IF([test -d "$with_lustre/lib"],
                 [check_lustre_LDFLAGS="-L$with_lustre/lib"
                  LDFLAGS="$LDFLAGS $check_lustre_LDFLAGS"], [])], 
           with_lustre="/usr/local")

    # Try to find all the lustre libraries (this is not fun!)
    if test -n "$with_lustre_libs" ; then
        check_lustre_LIBS="-llustre -llustreapi"
        for lib in $with_lustre_libs ; do
            check_lustre_LIBS="$check_lustre_LIBS -l$lib"
        done
    fi

    # check for lustre
    LIBS="$LIBS $check_lustre_LIBS"
    AC_CHECK_HEADERS([${check_lustre_header_prefix}lustre/liblustreapi.h],
        [AC_MSG_CHECKING([if possible to link LUSTRE])
            AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <${check_LUSTRE_header_prefix}lustre.h>], 
                        [int i;])],
                [AC_MSG_RESULT([yes])
                    ompi_check_lustre_happy="yes"],
                [AC_MSG_RESULT([no])
                    ompi_check_lustre_happy="no"])],
        [ompi_check_lustre_happy="no"])

    AS_IF([test "$ompi_check_lustre_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_lustre" -a "$with_lustre" != "no"],
                  [echo LUSTRE support not found])
              $3])
    with_lustre="$temp_lustre"
])
