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

    # always happy
    [$1]

    OPAL_SUMMARY_ADD([[Transports]],[[Shared memory/copy in+copy out]],[$1],[yes])

    # substitute in the things needed to build with XPMEM support
    AC_SUBST([btl_sm_CFLAGS])
    AC_SUBST([btl_sm_CPPFLAGS])
    AC_SUBST([btl_sm_LDFLAGS])
    AC_SUBST([btl_sm_LIBS])
])dnl
