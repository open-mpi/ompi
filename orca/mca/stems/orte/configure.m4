# -*- shell-script -*-
#
# Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


# MCA_stems_orte_CONFIG([action-if-can-compile],
#                       [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_orca_stems_orte_CONFIG],[
    AC_CONFIG_FILES([orca/mca/stems/orte/Makefile])

    ORCA_CHECK_ORTE([stems_orte],
        [stems_orte_happy="yes"],
        [stems_orte_happy="no"])

    AS_IF([test "$stems_orte_happy" = "yes"],
        [AC_SUBST([stems_orte_CFLAGS])
         AC_SUBST([stems_orte_CPPFLAGS])
         AC_SUBST([stems_orte_LDFLAGS])
         AC_SUBST([stems_orte_LIBS])
         $1],
        [$2]
    )

 ])dnl

