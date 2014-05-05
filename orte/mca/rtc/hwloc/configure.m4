# -*- shell-script -*-
#
# Copyright (c) 2014      Intel, Inc. All rights reserved
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# MCA_rtc_hwloc_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_rtc_hwloc_CONFIG], [
    AC_CONFIG_FILES([orte/mca/rtc/hwloc/Makefile])

    AS_IF([test "$OPAL_HAVE_HWLOC" = 1],
          [$1],
          [$2])
])
