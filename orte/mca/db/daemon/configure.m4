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
AC_DEFUN([MCA_orte_db_daemon_CONFIG], [
    AC_CONFIG_FILES([orte/mca/db/daemon/Makefile])

    # cant run this component without multicast
    AS_IF([test "$orte_want_multicast" = "1"],
        [$1], [$2])
])dnl
