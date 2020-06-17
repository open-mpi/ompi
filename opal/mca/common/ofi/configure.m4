# -*- shell-script -*-
#
# Copyright (c) 2011-2013 NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2013      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved.
# Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017       Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2019      Hewlett Packard Enterprise. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_opal_common_ofi_CONFIG],[
    AC_CONFIG_FILES([opal/mca/common/ofi/Makefile])

    OPAL_CHECK_OFI

    # Note that $opal_common_ofi_happy is
    # used in other configure.m4's to know if ofi configured
    # successfully.
    AS_IF([test "$opal_ofi_happy" = "yes"],
          [opal_common_ofi_happy=yes
           common_ofi_WRAPPER_EXTRA_LDFLAGS=$opal_ofi_LDFLAGS
           common_ofi_WRAPPER_EXTRA_LIBS=$opal_ofi_LIBS
           $1],
          [opal_common_ofi_happy=no
           $2])

])dnl
