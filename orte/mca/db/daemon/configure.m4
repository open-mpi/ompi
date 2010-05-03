# -*- shell-script -*-
#
# Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_db_daemon_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_db_daemon_CONFIG], [
    # cant run this component without multicast
    AS_IF([test "$orte_want_multicast" = "1"],
        [$1], [$2])
])dnl
