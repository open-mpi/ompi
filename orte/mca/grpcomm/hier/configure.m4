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
AC_DEFUN([MCA_orte_grpcomm_hier_PRIORITY], [10])

# MCA_grpcomm_hier_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_grpcomm_hier_CONFIG], [
    AC_CONFIG_FILES([orte/mca/grpcomm/hier/Makefile])

    AS_IF([test "$orte_without_full_support" = 0],
          [$1],
          [$2])
])
