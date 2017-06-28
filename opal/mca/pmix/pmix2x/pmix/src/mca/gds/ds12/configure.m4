# -*- shell-script -*-
#
# Copyright (c) 2015-2017 Intel, Inc. All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_gds_ds12_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_pmix_gds_ds12_CONFIG],[
    AC_CONFIG_FILES([src/mca/gds/ds12/Makefile])

    AS_IF([test "$enable_dstore" == "yes"], [$1], [$2])

])dnl
