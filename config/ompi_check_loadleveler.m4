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
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_CHECK_LOADLEVELER(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([OMPI_CHECK_LOADLEVELER],[
    AC_ARG_WITH([loadleveler],
                [AC_HELP_STRING([--with-loadleveler],
                                [Directory where the loadleveler software is installed])])

    AS_IF([test "$with_loadleveler" = "no"],
          [ompi_check_loadleveler_happy="no"],
          [ompi_check_loadleveler_happy="yes"
           AS_IF([test ! -z "$with_loadleveler" -a "$with_loadleveler" != "yes"],
                 [ompi_check_loadleveler_dir="$with_loadleveler"],
                 [ompi_check_loadleveler_dir=""])])

    AS_IF([test "$ompi_check_loadleveler_happy" = "yes"],
          [OMPI_CHECK_PACKAGE([$1],
                              [llapi.h],
                              [llapi],
                              [ll_query],
                              [],
                              [$ompi_check_loadleveler_dir],
                              [],
                              [ompi_check_loadleveler_happy="yes"],
                              [ompi_check_loadleveler_happy="no"])])

    AS_IF([test "$ompi_check_loadleveler_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_loadleveler" -a "$with_loadleveler" != "no"],
                 [AC_MSG_ERROR([LOADLEVELER support requested but not found.  Aborting])])
           $3])
])
