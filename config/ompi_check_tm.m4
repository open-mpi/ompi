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


# OMPI_CHECK_TM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_TM],[
    AC_ARG_WITH([tm],
                [AC_HELP_STRING([--with-tm],
                                [Directory where the tm software is installed])])


    ompi_check_tm_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_tm_save_LDFLAGS="$LDFLAGS"
    ompi_check_tm_save_LIBS="$LIBS"

    AS_IF([test ! -z "$with_tm"], 
          [CPPFLAGS="$CPPFLAGS -I$with_tm/include"
           LDFLAGS="$LDFLAGS -L$with_tm/lib"])
    AC_CHECK_HEADERS([tm.h],
                     [AC_CHECK_LIB([pbs], 
                                   [tm_init],
                                   [ompi_check_tm_happy="yes"],
                                   [ompi_check_tm_happy="no"])],
                     [ompi_check_tm_happy="no"])

    CPPFLAGS="$ompi_check_tm_save_CPPFLAGS"
    LDFLAGS="$ompi_check_tm_save_LDFLAGS"
    LIBS="$ompi_check_tm_save_LIBS"


    AS_IF([test "$ompi_check_tm_happy" = "yes"], 
          [AS_IF([test ! -z "$with_tm"], 
                 [$1_CPPFLAGS="$$1_CPPFLAGS -I$with_tm/include"
                  $1_LDFLAGS="$$1_LDFLAGS -L$with_tm/lib"
                  $1_LIBS="$$1_LIBS -lpbs"])
           $2], 
          [$3])
])
