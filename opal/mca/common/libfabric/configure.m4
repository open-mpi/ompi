# -*- shell-script -*-
#
# Copyright (c) 2011-2013 NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2013      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

#
# If LIBFABRIC support was requested, then build the LIBFABRIC support library.
# This code checks just makes sure the check was done earlier by the
# opal_check_libfabric.m4 code.
#

AC_DEFUN([MCA_opal_common_libfabric_CONFIG],[
    AC_CONFIG_FILES([opal/mca/common/libfabric/Makefile])

    # check for libfabric request
    OPAL_CHECK_LIBFABRIC([opal_common_libfabric],
                         [opal_common_libfabric_happy=yes
                          $1],
                         [opal_common_libfabric_happy=no
                          $2])

])dnl
