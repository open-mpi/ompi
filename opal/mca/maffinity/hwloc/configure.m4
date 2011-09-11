# -*- shell-script -*-
#
# Copyright (c) 2007-2011 Cisco Systems, Inc. All rights reserved.
#
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_maffinity_hwloc_CONFIG([action-if-found], [action-if-not-found])
# --------------------------------------------------------------------
AC_DEFUN([MCA_opal_maffinity_hwloc_CONFIG],[
    AC_CONFIG_FILES([opal/mca/maffinity/hwloc/Makefile])

    # All we check for is whether --without-hwloc was given
    # configury.  See big comment in opal/mca/hwloc/configure.m4.
    AC_MSG_CHECKING([if hwloc is enabled])
    AS_IF([test "$with_hwloc" != "no"],
          [AC_MSG_RESULT([yes])
           $1],
          [AC_MSG_RESULT([no])
           $2])
])dnl
