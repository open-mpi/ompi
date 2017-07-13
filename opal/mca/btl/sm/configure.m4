# -*- shell-script -*-
#
# Copyright (c) 2009      The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2009-2017 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2010-2012 IBM Corporation.  All rights reserved.
# Copyright (c) 2014      Los Alamos National Security, LLC. All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# The "sm" BTL is effectively dead; it has been wholly replaced
# by the "vader" BTL.  This BTL now only exists to provide a help
# message to users advising them to use the "vader" BTL.

# MCA_btl_sm_CONFIG([action-if-can-compile],
#                   [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_btl_sm_CONFIG],[
    AC_CONFIG_FILES([opal/mca/btl/sm/Makefile])
])dnl
