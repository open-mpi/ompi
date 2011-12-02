# -*- shell-script -*-
#
# Copyright (c) 2011 Cisco Systems, Inc.  All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_ompi_fbtl_CONFIG(project_name, framework_name)
# -------------------------------------------
AC_DEFUN([MCA_ompi_fbtl_CONFIG], 
[
    # An AC-ARG-ENABLE for mpi-io was set in ompi/mca/io/configure.m4.
    # If it's no, we shouldn't bother building anything in fcoll.
    AS_IF([test "$enable_mpi_io" != "no"],
          [want_mpi_io=1],
          [want_mpi_io=0])
    MCA_CONFIGURE_FRAMEWORK([$1], [$2], [$want_mpi_io])
])
