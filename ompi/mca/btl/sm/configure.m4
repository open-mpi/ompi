# -*- shell-script -*-
#
# Copyright (c) 2009      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010-2012 IBM Corporation.  All rights reserved.
# Copyright (c) 2014      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_btl_sm_CONFIG([action-if-can-compile],
#                   [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_btl_sm_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/btl/sm/Makefile])

    OPAL_VAR_SCOPE_PUSH([btl_sm_cma_happy])
    OMPI_CHECK_CMA([btl_sm], [btl_sm_cma_happy=1], [btl_sm_cma_happy=0])

    AC_DEFINE_UNQUOTED([OMPI_BTL_SM_HAVE_CMA],
        [$btl_sm_cma_happy],
        [If CMA support can be enabled])

    OPAL_VAR_SCOPE_POP

    OPAL_VAR_SCOPE_PUSH([btl_sm_knem_happy])
    OMPI_CHECK_KNEM([btl_sm],
        [btl_sm_knem_happy=1],
        [btl_sm_knem_happy=0])

    AC_DEFINE_UNQUOTED([OMPI_BTL_SM_HAVE_KNEM],
        [$btl_sm_knem_happy],
        [If knem support can be enabled])
    [$1]
    # substitute in the things needed to build KNEM
    AC_SUBST([btl_sm_CPPFLAGS])
    OPAL_VAR_SCOPE_POP
])dnl
