# -*- shell-script -*-
#
# Copyright (c) 2004-2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_crs_blcr_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_crs_blcr_CONFIG],[
    AC_CONFIG_FILES([opal/mca/crs/blcr/Makefile])

    check_crs_blcr_good="no"

    # Turn OFF BLCR as C/R is no longer supported
    AS_IF([test "$check_crs_blcr_good" = "no"],
          [$2
           check_crs_blcr_good="no"],
          [$1
           check_crs_blcr_good="yes"])
])dnl
