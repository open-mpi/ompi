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

# MCA_ompi_coll_monitoring_CONFIG([action-if-can-compile],
#                                 [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_coll_monitoring_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/coll/monitoring/Makefile])
    [$1]
])dnl

