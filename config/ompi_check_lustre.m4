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
dnl Copyright (c) 2008-2012 University of Houston. All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OMPI_CHECK_LUSTRE(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if LUSTRE support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_LUSTRE],[

    check_lustre_CPPFLAGS=
    check_lustre_LDFLAGS=
    check_lustre_LIBS=

    check_lustre_save_LIBS="$LIBS" 
    check_lustre_save_LDFLAGS="$LDFLAGS"
    check_lustre_save_CPPFLAGS="$CPPFLAGS"

    check_lustre_configuration="none"
    ompi_check_lustre_happy="yes"


    # Get some configuration information
    AC_ARG_WITH([lustre],
        [AC_HELP_STRING([--with-lustre(=DIR)],
             [Build Lustre support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OPAL_CHECK_WITHDIR([lustre], [$with_lustre], [include/lustre/liblustreapi.h])

    AS_IF([test -z "$with_lustre"],
          [ompi_check_lustre_dir="/usr"],
          [ompi_check_lustre_dir="$with_lustre"])

    if test -e "$ompi_check_lustre_dir/lib64" ; then
        ompi_check_lustre_libdir="$ompi_check_lustre_dir/lib64"
    else
        ompi_check_lustre_libdir="$ompi_check_lustre_dir/lib"
    fi

    # Add correct -I and -L flags
    OPAL_CHECK_PACKAGE([$1], [lustre/liblustreapi.h], [lustreapi], [llapi_file_create], [],
                       [$ompi_check_lustre_dir], [$ompi_check_lustre_libdir], [ompi_check_lustre_happy="yes"],
                       [ompi_check_lustre_happy="no"])

    AS_IF([test "$ompi_check_lustre_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_lustre" && test "$with_lustre" != "no"],
                  [echo LUSTRE support not found])
              $3])
])
