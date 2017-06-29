# -*- shell-script -*-
#
# Copyright (c) 2016 Inria.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_osc_monitoring_CONFIG()
# ------------------------------------------------
AC_DEFUN([MCA_ompi_osc_monitoring_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/osc/monitoring/Makefile])

    AS_IF([test MCA_BUILD_ompi_common_monitoring_DSO_TRUE == ''],
          [$1],
          [$2])
    OPAL_CHECK_PORTALS4([osc_monitoring],
                        [AC_DEFINE([OMPI_WITH_OSC_PORTALS4], [1], [Whether or not to generate template for osc_portals4])],
                        [])
])dnl
