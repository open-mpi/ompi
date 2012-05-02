# -*- shell-script -*-
#
# Copyright (c) 2012      Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# MCA_iof_mr_hnp_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_iof_mr_hnp_CONFIG], [
    AC_CONFIG_FILES([orte/mca/iof/mr_hnp/Makefile])

    AS_IF([test "$orte_without_full_support" = 0],
          [$1],
          [$2])
])
