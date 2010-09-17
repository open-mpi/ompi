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
# Copyright (c) 2008-2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_<framework>_<component>_CONFIG([action-if-can-compile], 
#                                    [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_opal_maffinity_libnuma_CONFIG],[
    AC_CONFIG_FILES([opal/mca/maffinity/libnuma/Makefile])

    OPAL_SETUP_COMPONENT_PACKAGE([maffinity],
                              [libnuma],
                              [libnuma],
                              [include/numa.h],
                              [libnuma*],
                              [numa.h],
                              [numa],
                              [numa_available],
                              [],
                              [AC_CHECK_DECLS([MPOL_MF_MOVE])
                               $1],
                              [$2])
])
