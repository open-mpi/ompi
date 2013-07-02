# -*- shell-script -*-
#
# Copyright (c) 2012-2013 Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# MCA_rmaps_mindist_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_rmaps_mindist_CONFIG], [
    AC_CONFIG_FILES([orte/mca/rmaps/mindist/Makefile])

    AS_IF([test "$OPAL_HAVE_HWLOC" = 1],
          [$1],
          [$2])
])
