# -*- shell-script -*-
#
# Copyright (c) 2007-2011 Cisco Systems, Inc. All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_maffinity_hwloc_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_maffinity_hwloc_CONFIG],[
    AC_CONFIG_FILES([opal/mca/maffinity/hwloc/Makefile])

    # All we check for is the results of opal/mca/common/hwloc's
    # configury
    AC_MSG_CHECKING([if common hwloc was happy])
    AC_MSG_RESULT([$opal_common_hwloc_support])

    AS_IF([test "$opal_common_hwloc_support" = "yes"], [$1], [$2])
])dnl
