# -*- shell-script -*-
#
# Copyright (c) 2009-2013 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2015 NVIDIA Corporation.  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_btl_smcuda_CONFIG([action-if-can-compile],
#                   [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_btl_smcuda_CONFIG],[
    AC_CONFIG_FILES([opal/mca/btl/smcuda/Makefile])

    OPAL_CHECK_CUDA([btl_smcuda])

    # Only build if CUDA support is available
    AS_IF([test "x$CUDA_SUPPORT" = "x1"],
          [$1
           OPAL_MCA_CHECK_DEPENDENCY([opal], [btl], [smcuda], [opal], [common], [sm])],
          [$2])

    AC_SUBST([btl_smcuda_CPPFLAGS])
    AC_SUBST([btl_smcuda_LDFLAGS])
    AC_SUBST([btl_smcuda_LIBS])
])dnl
