# -*- shell-script ; indent-tabs-mode:nil -*-
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
# Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2014      Intel, Inc. All rights reserved.
# Copyright (c) 2014-2015 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# special check for cray xpmem, uses macro(s) from pkg.m4
#
# OPAL_CHECK_CRAY_XPMEM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OPAL_CHECK_CRAY_XPMEM],[
    AC_ARG_WITH([cray_xpmem],
                [AC_HELP_STRING([--with-cray-xpmem(=yes/no)],
                [Build Cray XPMEM support(default: auto)])],
                 [], with_cray_xpmem=auto)

   AC_MSG_CHECKING([for Cray XPMEM support])
   AS_IF([test "$with_cray_xpmem" = "no"],
         [AC_MSG_RESULT([no])
          $3],
         [AS_IF([test "$with_cray_xpmem" = "auto" || test "$with_cray_xpmem" = "yes"],
                 [PKG_CHECK_MODULES_STATIC([CRAY_XPMEM], [cray-xpmem],
                                    [opal_check_cray_xpmem_happy="yes"],
                                    [opal_check_cray_xpmem_happy="no"]
                                    [AS_IF([test "$with_cray_xpmem" = "yes"],
                                           [AC_MSG_WARN([Cray XPMEM support requested but pkg-config failed.])
                                            AC_MSG_ERROR([Aborting])],[])]
                                     )],
                 [])
         ])

    AS_IF([test "$opal_check_cray_xpmem_happy" = "yes" && test "$enable_static" = "yes"],
          [CRAY_XPMEM_LIBS = $CRAY_XPMEM_STATIC_LIBS],[])

    AS_IF([test "$opal_check_cray_xpmem_happy" = "yes"],
          [$1_LDFLAGS="$CRAY_XPMEM_LIBS"
           $1_CPPFLAGS="$CRAY_XPMEM_CFLAGS"
           $1_LIBS="$CRAY_XPMEM_LIBS"
           AC_DEFINE_UNQUOTED([HAVE_XPMEM_H], [1],[is xpmem.h available])
           $2], [$3])
])



