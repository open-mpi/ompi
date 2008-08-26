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

AC_DEFUN([MCA_paffinity_linux_POST_CONFIG],[
    PLPA_DO_AM_CONDITIONALS
])dnl

AC_DEFUN([MCA_paffinity_linux_CONFIG],[
    OMPI_VAR_SCOPE_PUSH([PLPA_VERSION])

    # Setup PLPA
    PLPA_SET_SYMBOL_PREFIX([opal_paffinity_linux_plpa_])
    PLPA_INCLUDED
    PLPA_INIT([opal/mca/paffinity/linux/plpa], 
              [AC_MSG_CHECKING([for PLPA version])
               PLPA_VERSION=`$srcdir/opal/mca/paffinity/linux/plpa/config/plpa_get_version.sh $srcdir/opal/mca/paffinity/linux/plpa/VERSION`
               AC_DEFINE_UNQUOTED([PAFFINITY_LINUX_PLPA_VERSION], 
                                  ["$PLPA_VERSION"], 
                                  [Version of PLPA embedded in OMPI])
               AC_MSG_RESULT([$PLPA_VERSION])
               $1], 
              [$2])

    OMPI_VAR_SCOPE_POP
])dnl
