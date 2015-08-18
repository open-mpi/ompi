# -*- shell-script -*-
#
# Copyright (c) 2011-2013 NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2013      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved.
# Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_opal_common_libfabric_CONFIG],[
    AC_CONFIG_FILES([opal/mca/common/libfabric/Makefile])

    # Check for libfabric.  Note that $opal_common_libfabric_happy is
    # used in other configure.m4's to know if libfabric configured
    # successfully.
    OPAL_CHECK_LIBFABRIC([opal_common_libfabric],
                         [opal_common_libfabric_happy=yes
                          common_libfabric_WRAPPER_EXTRA_LDFLAGS=$opal_common_libfabric_LDFLAGS
                          common_libfabric_WRAPPER_EXTRA_LIBS=$opal_common_libfabric_LIBS
                          $1],
                         [opal_common_libfabric_happy=no
                          $2])

])dnl
