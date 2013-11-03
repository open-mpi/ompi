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
# Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
#                         Use is subject to license terms.
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2013 Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_ras_sge_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_ras_gridengine_CONFIG],[
    AC_CONFIG_FILES([orte/mca/ras/gridengine/Makefile])

    ORTE_CHECK_GRIDENGINE([ras_gridengine], [ras_gridengine_happy="yes"], [ras_gridengine_happy="no"])

    AS_IF([test "$ras_gridengine_happy" = "yes"], [$1], [$2])
])dnl
