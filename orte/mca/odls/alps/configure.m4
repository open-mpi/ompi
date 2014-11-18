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
# Copyright (c) 2011-2014 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_odls_alps_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_odls_alps_CONFIG],[
    AC_CONFIG_FILES([orte/mca/odls/alps/Makefile])

    odls_alps_lli_happy="no"
    odls_alps_util_happy="no"

    PKG_CHECK_MODULES([CRAY_ALPS_LLI], [cray-alpslli],
                      [odls_alps_CPPFLAGS=$CRAY_ALPS_LLI_CFLAGS
                       odls_alps_LDFLAGS=$CRAY_ALPS_LLI_LIBS
                       odls_alps_LIBS=$CRAY_ALPS_LLI_LIBS
                       odls_alps_lli_happy="yes"],
                      [AC_MSG_RESULT([no])])

    PKG_CHECK_MODULES([CRAY_ALPS_UTIL], [cray-alpsutil],
                      [odls_alps_CPPFLAGS="$odls_alps_CPPFLAGS $CRAY_ALPS_UTIL_CFLAGS"
                       odls_alps_LDFLAGS="$odls_alps_LDFLAGS  $CRAY_ALPS_UTIL_LIBS"
                       odls_alps_LIBS="$odls_alps_LIBS $CRAY_ALPS_LLI_LIBS"
                       odls_alps_util_happy="yes"],
                      [AC_MSG_RESULT([hey, no alps lli found])])

    AS_IF([test "$odls_alps_lli_happy" = "yes" -a "$odls_alps_util_happy" = "yes"], 
          [$1
           AC_SUBST([odls_alps_CPPFLAGS])
           AC_SUBST([odls_alps_LDFLAGS])
           AC_SUBST([odls_alps_LIBS])], 
          [$2])

])dnl

