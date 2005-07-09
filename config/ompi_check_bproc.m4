# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
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


# OMPI_CHECK_BPROC(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_BPROC],[
    AC_ARG_WITH([bproc],
                [AC_HELP_STRING([--with-bproc],
                                [Directory where the bproc software is installed])])

    ompi_check_bproc_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_bproc_save_LDFLAGS="$LDFLAGS"
    ompi_check_bproc_save_LIBS="$LIBS"

    AS_IF([test ! -z "$with_bproc"], 
          [CPPFLAGS="$CPPFLAGS -I$with_bproc/include"
           LDFLAGS="$LDFLAGS -L$with_bproc/lib"])
    AC_CHECK_HEADERS([sys/bproc.h],
                     [AC_CHECK_LIB([bproc], 
                                   [bproc_numnodes],
                                   [ompi_check_bproc_happy="yes"],
                                   [ompi_check_bproc_happy="no"])],
                     [ompi_check_bproc_happy="no"])

    CPPFLAGS="$ompi_check_bproc_save_CPPFLAGS"
    LDFLAGS="$ompi_check_bproc_save_LDFLAGS"
    LIBS="$ompi_check_bproc_save_LIBS"

    AS_IF([test "$ompi_check_bproc_happy" = "yes"], 
          [AS_IF([test ! -z "$with_bproc"], 
                 [$1_CPPFLAGS="$$1_CPPFLAGS -I$with_bproc/include"
                  $1_LDFLAGS="$$1_LDFLAGS -L$with_bproc/lib"
                  $1_LIBS="$$1_LIBS -lbproc"])
           $2], 
          [$3])
])
