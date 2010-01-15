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
# Copyright (c) 2006-2009 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# ORTE_CHECK_LOADLEVELER(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([ORTE_CHECK_LOADLEVELER],[
    AC_ARG_WITH([loadleveler],
                [AC_HELP_STRING([--with-loadleveler(=DIR)],
                                [Build LoadLeveler support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OMPI_CHECK_WITHDIR([loadleveler], [$with_loadleveler], [include/llapi.h])

    AS_IF([test "$with_loadleveler" = "no"],
          [orte_check_loadleveler_happy="no"],
          [orte_check_loadleveler_happy="yes"
           AS_IF([test ! -z "$with_loadleveler" -a "$with_loadleveler" != "yes"],
                 [orte_check_loadleveler_dir="$with_loadleveler"],
                 [orte_check_loadleveler_dir=""])])

    AS_IF([test "$orte_check_loadleveler_happy" = "yes"],
          [OMPI_CHECK_PACKAGE([$1],
                              [llapi.h],
                              [llapi],
                              [ll_query],
                              [],
                              [$orte_check_loadleveler_dir],
                              [],
                              [orte_check_loadleveler_happy="yes"],
                              [orte_check_loadleveler_happy="no"])])

    AS_IF([test "$orte_check_loadleveler_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_loadleveler" -a "$with_loadleveler" != "no"],
                 [AC_MSG_ERROR([LOADLEVELER support requested but not found.  Aborting])])
           $3])
])
