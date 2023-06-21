# -*- shell-script -*-
#
# Copyright (c) 2017      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_common_monitoring_CONFIG([action-if-can-compile],
#                                   [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_common_monitoring_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/common/monitoring/Makefile])

    m4_ifdef([project_ompi],
             [AC_CONFIG_LINKS(test/monitoring/profile2mat.pl:ompi/mca/common/monitoring/profile2mat.pl
                              test/monitoring/aggregate_profile.pl:ompi/mca/common/monitoring/aggregate_profile.pl)])


    [$1]
])dnl
