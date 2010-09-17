# -*- shell-script -*-
#
# Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
AC_DEFUN([MCA_orte_grpcomm_mcast_PRIORITY], [10])

# MCA_grpcomm_mcast_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_grpcomm_mcast_CONFIG], [
    AC_CONFIG_FILES([orte/mca/grpcomm/mcast/Makefile])

    # if we don't want reliable multicast, don't compile
    # this component
    AS_IF([test "$orte_want_multicast" = "1"],
        [$1], [$2])
])dnl
