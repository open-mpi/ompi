# -*- shell-script -*-
#
# Copyright (c) 2004-2006 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2011      Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_plm_submit_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_plm_submit_CONFIG],[
    AC_CONFIG_FILES([orte/mca/plm/submit/Makefile])

    AC_CHECK_FUNC([fork], [plm_submit_happy="yes"], [plm_submit_happy="no"])

    AS_IF([test "$plm_submit_happy" = "yes" -a "$orte_without_full_support" = 0], [$1], [$2])
])dnl
