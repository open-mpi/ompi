# -*- shell-script -*-
#
# Copyright (c) 2010      Cisco Systems, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_sensor_heartbeat_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_sensor_heartbeat_CONFIG], [
    AC_CONFIG_FILES([orte/mca/sensor/heartbeat/Makefile])

    # if we don't want heartbeats, don't compile
    # this component
    AS_IF([test "$orte_want_heartbeats" = "1"],
        [$1], [$2])
])dnl

