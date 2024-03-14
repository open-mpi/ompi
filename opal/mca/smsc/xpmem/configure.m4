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
# Copyright (c) 2021      Google, LLC. All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_smsc_xpmem_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_smsc_xpmem_CONFIG],[
    AC_CONFIG_FILES([opal/mca/smsc/xpmem/Makefile])

    OPAL_CHECK_XPMEM([smsc_xpmem], [$1], [$2])

    AC_SUBST([smsc_xpmem_CPPFLAGS])
    AC_SUBST([smsc_xpmem_LDFLAGS])
    AC_SUBST([smsc_xpmem_LIBS])
])dnl
