# -*- shell-script -*-
#
# Copyright (c) 2011      Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# MCA_routed_direct_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_routed_direct_CONFIG], [
    AC_CONFIG_FILES([orte/mca/routed/direct/Makefile])

    AS_IF([test "$orte_without_full_support" = 0],
          [$1],
          [$2])
])
