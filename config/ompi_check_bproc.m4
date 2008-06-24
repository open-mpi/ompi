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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# new bproc is LANL versions >= 3.2.0
# old bproc is all Scyld versions and LANL version < 3.2.0
# OMPI_CHECK_BPROC(prefix, [action-if-new-bproc], [action-if-old-bproc],
#                  [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_BPROC],[
    AC_ARG_WITH([bproc],
                [AC_HELP_STRING([--with-bproc],
                                [Directory where the BProc software is installed])])

    AS_IF([test ! -z "$with_bproc" -a "$with_bproc" = "no"],[$4], [ 
        ompi_check_bproc_save_CPPFLAGS="$CPPFLAGS"
        ompi_check_bproc_save_LDFLAGS="$LDFLAGS"
        ompi_check_bproc_save_LIBS="$LIBS"

        AS_IF([test ! -z "$with_bproc" -a "$with_bproc" != "yes"], 
              [CPPFLAGS="$CPPFLAGS -I$with_bproc/include"
               LDFLAGS="$LDFLAGS -L$with_bproc/lib"])
        AC_CHECK_HEADERS([sys/bproc.h],
                         [AC_CHECK_LIB([bproc], 
                                       [bproc_numnodes],
                                       [ompi_check_bproc_happy="yes"],
                                       [ompi_check_bproc_happy="no"])],
                         [ompi_check_bproc_happy="no"])

        # Check for Scyld bproc or an old version of LANL Bproc (pre 3.2.0)
        AS_IF([test "$ompi_check_bproc_happy" = "yes"], 
            [AC_CHECK_HEADERS([sys/bproc_common.h],[ompi_check_bproc_happy="new"],
                              [ompi_check_bproc_happy="old"],
                              [#include <stdint.h>
                               #include <sys/socket.h>])])

        CPPFLAGS="$ompi_check_bproc_save_CPPFLAGS"
        LDFLAGS="$ompi_check_bproc_save_LDFLAGS"
        LIBS="$ompi_check_bproc_save_LIBS"

        AS_IF([test "$ompi_check_bproc_happy" != "no"], 
              [AS_IF([test ! -z "$with_bproc" -a "$with_bproc" != "yes"], 
                     [$1_CPPFLAGS="$$1_CPPFLAGS -I$with_bproc/include"
                      $1_LDFLAGS="$$1_LDFLAGS -L$with_bproc/lib"])
               $1_LIBS="$$1_LIBS -lbproc"
               AS_IF([test "$ompi_check_bproc_happy" = "new"], [$2], [$3])], 
              [AS_IF([test ! -z "$with_bproc"],
                     [AC_MSG_ERROR([BProc support request but not found.  Perhaps
you need to specify the location of the BProc libraries.])])
               $4])
    ])
])
