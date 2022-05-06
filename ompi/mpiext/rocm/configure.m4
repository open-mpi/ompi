# -*- shell-script -*-
#
# Copyright (c) 2004-2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2012-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Intel, Inc. All rights reserved.
# Copyright (c) 2015      NVIDIA, Inc.  All rights reserved.
# Copyright (c) 2022      Advanced Micro Devices, Inc.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_MPIEXT_rocm_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([OMPI_MPIEXT_rocm_CONFIG],[
    AC_CONFIG_FILES([ompi/mpiext/rocm/Makefile])
    AC_CONFIG_FILES([ompi/mpiext/rocm/c/Makefile])
    AC_CONFIG_HEADERS([ompi/mpiext/rocm/c/mpiext_rocm_c.h])

    AC_DEFINE_UNQUOTED([MPIX_ROCM_AWARE_SUPPORT], [$ROCM_SUPPORT],
                       [Macro that is set to 1 when ROCM-aware support is configured in and 0 when it is not])

    
    # We compile this whether ROCm support was requested or not. It allows
    # us to to detect if we have ROCm support.
    AS_IF([test "$ENABLE_rocm" = "1" || \
           test "$ENABLE_EXT_ALL" = "1"],
          [$1],
          [$2])
    
])
