# -*- shell-script -*-
#
# Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
# Copyright (c) 2014      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_osc_pt2pt_CONFIG([action-if-can-compile],
#                           [action-if-cant-compile])
# ------------------------------------------------
# We can always build, unless we were explicitly disabled.
AC_DEFUN([MCA_ompi_osc_pt2pt_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/osc/pt2pt/Makefile])
    [$1]
])dnl
