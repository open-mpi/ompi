# -*- shell-script -*-
#
# Copyright (c) 2011      Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_dpm_orte_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_ompi_dpm_orte_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/dpm/orte/Makefile])

    ORCA_CHECK_ORTE([dpm_orte],
        [dpm_orte_happy="yes"],
        [dpm_orte_happy="no"])

    AS_IF([test "$dpm_orte_happy" = "yes" -a "$orca_without_orte_full_support" = 0],
        [AC_SUBST([dpm_orte_CFLAGS])
         AC_SUBST([dpm_orte_CPPFLAGS])
         AC_SUBST([dpm_orte_LDFLAGS])
         AC_SUBST([dpm_orte_LIBS])
         $1],
        [$2])
])dnl
