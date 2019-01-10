# -*- shell-script -*-
#
# Copyright (c) 2018      Mellanox Technologies.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_common_dstore_CONFIG([action-if-can-compile],
#                           [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_common_dstore_CONFIG], [
    AC_CONFIG_FILES([src/mca/common/dstore/Makefile])
    $1
])dnl
