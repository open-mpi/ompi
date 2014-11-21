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
# Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011      Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ess_alps_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_ess_alps_CONFIG],[
    AC_CONFIG_FILES([orte/mca/ess/alps/Makefile])

    ess_alps_lli_happy="no"
    ess_alps_util_happy="no"

    PKG_CHECK_MODULES([CRAY_ALPS_LLI], [cray-alpslli],
                      [ess_alps_CPPFLAGS=$CRAY_ALPS_LLI_CFLAGS
                       ess_alps_LDFLAGS=$CRAY_ALPS_LLI_LIBS
                       ess_alps_LIBS=$CRAY_ALPS_LLI_LIBS
                       ess_alps_lli_happy="yes"],
                      [AC_MSG_RESULT([no])])

    PKG_CHECK_MODULES([CRAY_ALPS_UTIL], [cray-alpsutil],
                      [ess_alps_CPPFLAGS="$ess_alps_CPPFLAGS $CRAY_ALPS_UTIL_CFLAGS"
                       ess_alps_LDFLAGS="$ess_alps_LDFLAGS  $CRAY_ALPS_UTIL_LIBS"
                       ess_alps_LIBS="$ess_alps_LIBS $CRAY_ALPS_LLI_LIBS"
                       ess_alps_util_happy="yes"],
                      [AC_MSG_RESULT([no])])

    AS_IF([test "$ess_alps_lli_happy" = "yes" -a "$ess_alps_util_happy" = "yes"], 
          [$1
           AC_SUBST([ess_alps_CPPFLAGS])
           AC_SUBST([ess_alps_LDFLAGS])
           AC_SUBST([ess_alps_LIBS])], 
          [$2])

])dnl
