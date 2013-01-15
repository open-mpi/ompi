# -*- shell-script -*-
#
# Copyright (c)      2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c)      2010 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_opal_crs_dmtcp_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_crs_dmtcp_CONFIG],[
    AC_CONFIG_FILES([opal/mca/crs/dmtcp/Makefile])

    opal_check_crs_dmtcp_good="no"

    # Turn OFF DMTCP as C/R is no longer supported
    #
    AS_IF([test "$opal_check_crs_dmtcp_good" = "no"],
          [$2],
          [$1])
])dnl
