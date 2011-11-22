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
# MCA_ras_simulator_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_ras_simulator_CONFIG], [
    AC_CONFIG_FILES([orte/mca/ras/simulator/Makefile])

    AS_IF([test "$orte_without_full_support" = 0],
          [$1],
          [$2])
])
