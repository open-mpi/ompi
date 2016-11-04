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
# Copyright (c) 2011-2013 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2010-2016 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_plm_rsh_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_plm_rsh_CONFIG],[
    AC_CONFIG_FILES([orte/mca/plm/rsh/Makefile])

    AC_CHECK_FUNC([fork], [plm_rsh_happy="yes"], [plm_rsh_happy="no"])

    OPAL_SUMMARY_ADD([[Resource Managers]],[[ssh/rsh]],[$1],[$plm_rsh_happy])
    AS_IF([test "$plm_rsh_happy" = "yes"], [$1], [$2])
])dnl
