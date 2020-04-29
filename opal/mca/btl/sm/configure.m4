# -*- shell-script -*-
#
# Copyright (c) 2009      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2009-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2014 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_btl_sm_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_btl_sm_CONFIG],[
    AC_CONFIG_FILES([opal/mca/btl/sm/Makefile])

    OPAL_VAR_SCOPE_PUSH([btl_sm_xpmem_happy btl_sm_cma_happy btl_sm_knem_happy])

    # Check for single-copy APIs

    OPAL_CHECK_XPMEM([btl_sm], [btl_sm_xpmem_happy=1], [btl_sm_xpmem_happy=0])
    OPAL_CHECK_KNEM([btl_sm], [btl_sm_knem_happy=1],[btl_sm_knem_happy=0])
    OPAL_CHECK_CMA([btl_sm], [AC_CHECK_HEADER([sys/prctl.h]) btl_sm_cma_happy=1], [btl_sm_cma_happy=0])

    AC_DEFINE_UNQUOTED([OPAL_BTL_SM_HAVE_XPMEM], [$btl_sm_xpmem_happy],
        [If XPMEM support can be enabled within sm])

    AC_DEFINE_UNQUOTED([OPAL_BTL_SM_HAVE_CMA], [$btl_sm_cma_happy],
        [If CMA support can be enabled within sm])

    AC_DEFINE_UNQUOTED([OPAL_BTL_SM_HAVE_KNEM], [$btl_sm_knem_happy],
	[If KNEM support can be enabled within sm])

    OPAL_VAR_SCOPE_POP

    # always happy
    [$1]

    OPAL_SUMMARY_ADD([[Transports]],[[Shared memory/copy in+copy out]],[$1],[yes])

    # substitute in the things needed to build with XPMEM support
    AC_SUBST([btl_sm_CFLAGS])
    AC_SUBST([btl_sm_CPPFLAGS])
    AC_SUBST([btl_sm_LDFLAGS])
    AC_SUBST([btl_sm_LIBS])
])dnl
