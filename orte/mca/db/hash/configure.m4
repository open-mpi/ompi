# -*- shell-script -*-
#
# Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
# Copyright (c) 2012      Los Alamos National Security, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_db_hash_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_db_hash_CONFIG], [
    AC_CONFIG_FILES([orte/mca/db/hash/Makefile])

    # do not build if rte is disabled
    AS_IF([test "$orte_without_full_support" = 0],
          [$1],
          [$2])
])dnl
