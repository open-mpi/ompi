# -*- shell-script -*-
#
# Copyright (c) 2016      Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_schizo_singularity_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_schizo_singularity_CONFIG],[
    AC_CONFIG_FILES([orte/mca/schizo/singularity/Makefile])

    OPAL_CHECK_SINGULARITY([schizo_singularity], [schizo_singularity_happy="yes"], [schizo_singularity_happy="no"])

    AS_IF([test "$schizo_singularity_happy" = "yes"], [$1], [$2])
])dnl
