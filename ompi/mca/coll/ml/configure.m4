# -*- shell-script -*-
#
# Copyright (c) 2014      Intel, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# MCA_coll_ml_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_ompi_coll_ml_CONFIG], [
    AC_CONFIG_FILES([ompi/mca/coll/ml/Makefile])

    AS_IF([test "$OPAL_HAVE_HWLOC" = 1],
          [$1],
          [$2])
])
