dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# MCA_rmcast_spread_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_rmcast_spread_CONFIG],[
    AC_CONFIG_FILES([orte/mca/rmcast/spread/Makefile])

   OPAL_SETUP_COMPONENT_PACKAGE([rmcast],
                              [spread],
                              [spread],
                              [include/sp.h],
                              [libspread*],
                              [sp.h],
                              [spread],
                              [SP_version],
                              [],
                              [$1],
                              [$2])
])dnl
