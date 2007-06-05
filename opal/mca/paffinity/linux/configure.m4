# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2007      Cisco Systems, Inc. All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_paffinity_linux_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
m4_include(opal/mca/paffinity/linux/plpa/config/plpa.m4)

AC_DEFUN([MCA_paffinity_linux_CONFIG],[
    paffinity_linux_CPPFLAGS="-Iopal/mca/paffinity/linux/plpa/src/libplpa"
    AC_SUBST([paffinity_linux_CPPFLAGS])
    PLPA_SET_SYMBOL_PREFIX([opal_paffinity_linux_plpa_])
    PLPA_INCLUDED([opal/mca/paffinity/linux/plpa])
    PLPA_INIT([$1],[$2])
])dnl
