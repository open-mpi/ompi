# -*- shell-script -*-
#
# Copyright (c) 2011      Cisco Systems, Inc. All rights reserved.
# Copyright (c) 2011-2013 Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_sensor_ft_tester_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_sensor_ft_tester_CONFIG], [
    AC_CONFIG_FILES([orte/mca/sensor/ft_tester/Makefile])

    # if we don't want sensors, don't compile
    # this component
    AS_IF([test "$orte_want_sensors" = "1"],
        [$1], [$2])
])dnl

