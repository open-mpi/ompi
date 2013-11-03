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
# Copyright (c) 2008      UT-Battelle, LLC
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2013 Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_ras_alps_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_ras_alps_CONFIG],[
    AC_CONFIG_FILES([orte/mca/ras/alps/Makefile])

    ORTE_CHECK_ALPS([ras_alps], [ras_alps_happy="yes"], [ras_alps_happy="no"])

    # check for alps/apInfo.h
    # save current CPPFLAGS
    MCA_orte_ras_save_CPPFLAGS="$CPPFLAGS"

    # add flags obtained from ORTE_CHECK_ALPS
    CPPFLAGS="$CPPFLAGS $ras_alps_CPPFLAGS"

    AC_CHECK_HEADERS([alps/apInfo.h], [], [ras_alps_happy="no"])

    # restore CPPFLAGS
    CPPFLAGS="$MCA_orte_ras_save_CPPFLAGS"

    AC_SUBST([ras_alps_CPPFLAGS])

    AS_IF([test "$ras_alps_happy" = "yes"], [$1], [$2])
])dnl
