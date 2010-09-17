# -*- shell-script -*-
#
# Copyright (c) 2009      Los Alamos National Security, LLC.
#			  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
AC_DEFUN([MCA_orte_ess_slave_PRIORITY], [10])

# MCA_ess_slave_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_ess_slave_CONFIG], [
    AC_CONFIG_FILES([orte/mca/ess/slave/Makefile])
])
