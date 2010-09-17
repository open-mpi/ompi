# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_odls_process_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_odls_process_CONFIG],[
    AC_CONFIG_FILES([orte/mca/odls/process/Makefile])

    AC_CHECK_FUNC([CreateProcess], [$1], [$2])
])dnl
