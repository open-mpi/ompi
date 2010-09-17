# -*- shell-script -*-
#
# Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_rmcast_tcp_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_rmcast_tcp_CONFIG], [
    AC_CONFIG_FILES([orte/mca/rmcast/tcp/Makefile])

    # if we don't want reliable multicast, don't compile
    # this component
    AS_IF([test "$orte_want_multicast" = "1"],
        [$1], [$2])
])dnl

