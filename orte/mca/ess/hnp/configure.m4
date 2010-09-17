# -*- shell-script -*-
#
# Copyright (c) 2007      Sandia National Laboratories. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
AC_DEFUN([MCA_orte_ess_hnp_PRIORITY], [10])

# MCA_ess_hnp_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_ess_hnp_CONFIG], [
    AC_CONFIG_FILES([orte/mca/ess/hnp/Makefile])
])
