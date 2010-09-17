# -*- shell-script -*-
#
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
AC_DEFUN([MCA_orte_ess_generic_PRIORITY], [10])

# MCA_ess_generic_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_ess_generic_CONFIG], [
    AC_CONFIG_FILES([orte/mca/ess/generic/Makefile])
])
