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
# Copyright (c) 2011-2016 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2016      Intel, Inc. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_schizo_alps_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_schizo_alps_CONFIG],[
    AC_CONFIG_FILES([orte/mca/schizo/alps/Makefile])

    OPAL_CHECK_ALPS([schizo_alps], [schizo_alps_happy="yes"], [schizo_alps_happy="no"])

    # check for alps/apInfo.h
    # save current CPPFLAGS
    MCA_orte_schizo_save_CPPFLAGS="$CPPFLAGS"

    # add flags obtained from OPAL_CHECK_ALPS
    CPPFLAGS="$CPPFLAGS $schizo_alps_CPPFLAGS"

    AC_CHECK_HEADERS([alps/apInfo.h], [], [schizo_alps_happy="no"])

    # restore CPPFLAGS
    CPPFLAGS="$MCA_orte_schizo_save_CPPFLAGS"

    AC_SUBST([schizo_alps_CPPFLAGS])

    AS_IF([test "$schizo_alps_happy" = "yes"], [$1], [$2])
])dnl
