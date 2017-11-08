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

    m4_ifdef([project_ompi], [
          m4_ifdef([MCA_BUILD_ompi_common_monitoring_DSO_TRUE],
                   [AC_CONFIG_LINKS(profile2mat.pl:test/monitoring/profile2mat.pl
                                    aggregate_profile.pl:test/monitoring/aggregate_profile.pl)])])


    AS_IF([test "$MCA_BUILD_ompi_common_monitoring_DSO_TRUE" = ''],
          [$1],
          [$2])
])dnl
