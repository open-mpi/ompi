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

# OMPI_CHECK_XCPU(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_XCPU],[
    AC_ARG_WITH([xcpu],
                [AC_HELP_STRING([--with-xcpu],
                                [Path to xcpu installation])])

    AS_IF([test ! -z "$with_xcpu" -a "$with_xcpu" = "no"],[$4], [ 
        ompi_check_xcpu_save_CPPFLAGS="$CPPFLAGS"
        ompi_check_xcpu_save_LDFLAGS="$LDFLAGS"
        ompi_check_xcpu_save_LIBS="$LIBS"

        AS_IF([test ! -z "$with_xcpu" -a "$with_xcpu" != "yes"], 
              [CPPFLAGS="$CPPFLAGS -I$with_xcpu/include"
               LDFLAGS="$LDFLAGS -L$with_xcpu/lib"])
        AC_CHECK_HEADERS([sys/xcpu.h],
                         [AC_CHECK_LIB([xcpu], 
                                       [check_for_xcpu],
                                       [ompi_check_xcpu_works="yes"],
                                       [ompi_check_xcpu_works="no"])],
                         [AC_CHECK_LIB([xcpu], 
                                       [check_for_xcpu],
                                       [ompi_check_xcpu_works="yes"],
                                       [ompi_check_xcpu_works="no"])])
                         # check for library irrespective of if xcpu.h is there or not
                         # 'cause  I am not sure 
                         # if we need to check for xcpu.h

        CPPFLAGS="$ompi_check_xcpu_save_CPPFLAGS"
        LDFLAGS="$ompi_check_xcpu_save_LDFLAGS"
        LIBS="$ompi_check_xcpu_save_LIBS"

        AS_IF([test "$ompi_check_xcpu_works" != "no"], 
              [AS_IF([test ! -z "$with_xcpu" -a "$with_xcpu" != "yes"], 
                     [$1_CPPFLAGS="$$1_CPPFLAGS -I$with_xcpu/include"
                      $1_LDFLAGS="$$1_LDFLAGS -L$with_xcpu/lib"])
               $1_LIBS="$$1_LIBS -lxcpu"
               AS_IF([test "$ompi_check_xcpu_works" = "yes"], [$2], [$3])], 
              [AS_IF([test ! -z "$with_xcpu"],
                     [AC_MSG_ERROR([xcpu support requested but not found.  Perhaps
you need to specify the location of the xcpu libraries.])])
               $4])
    ])
])
