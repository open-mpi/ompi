# -*- autoconf -*-
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
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

AC_DEFUN([MCA_opal_common_ofi_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([common_ofi_happy])

    AC_CONFIG_FILES([opal/mca/common/ofi/Makefile])

    OPAL_CHECK_OFI([common_ofi], [common_ofi_happy=1], [common_ofi_happy=0])

    AS_IF([test ${common_ofi_happy} -eq 1],
          [$1],
          [$2])

    AC_SUBST([common_ofi_CPPFLAGS])
    AC_SUBST([common_ofi_LDFLAGS])
    AC_SUBST([common_ofi_LIBS])

    OPAL_VAR_SCOPE_POP
])dnl
