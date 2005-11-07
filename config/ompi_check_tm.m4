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


# OMPI_CHECK_TM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_TM],[
    AC_ARG_WITH([tm],
                [AC_HELP_STRING([--with-tm],
                                [Directory where the tm software is installed])])

    AS_IF([test "$with_tm" = "no"],
          [ompi_check_tm_hapy="no"],
          [ompi_check_tm_happy="yes"
           AS_IF([test ! -z "$with_tm" -a "$with_tm" != "yes"],
                 [ompi_check_tm_dir="$with_tm"],
                 [ompi_check_tm_dir=""])])

    AS_IF([test "$ompi_check_tm_happy" = "yes"],
          [OMPI_CHECK_PACKAGE([$1],
                              [tm.h],
                              [pbs],
                              [tm_init],
                              [],
                              [$ompi_check_tm_dir],
                              [],
                              [ompi_check_tm_happy="yes"],
                              [ompi_check_tm_happy="no"])])

    AS_IF([test "$ompi_check_tm_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_tm" -a "$with_tm" != "no"],
                 [AC_MSG_ERROR([TM support requested but not found.  Aborting])])
           $3])
])
