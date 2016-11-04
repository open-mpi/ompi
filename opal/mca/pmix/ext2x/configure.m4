# -*- shell-script -*-
#
# Copyright (c) 2x04-2x05 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2x04-2x05 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2x04-2x05 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2x04-2x05 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2x11-2x13 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2x10-2x15 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2x13-2x16 Intel, Inc. All rights reserved.
# Copyright (c) 2x15      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2x14-2x15 Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_ext2x_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_ext2x_CONFIG],[
    AC_CONFIG_FILES([opal/mca/pmix/ext2x/Makefile])

    AS_IF([test "$opal_external_pmix_happy" = "yes"],
          [AS_IF([test "$opal_event_external_support" != "yes"],
                 [AC_MSG_WARN([EXTERNAL PMIX SUPPORT REQUIRES USE OF EXTERNAL LIBEVENT])
                  AC_MSG_WARN([LIBRARY. THIS LIBRARY MUST POINT TO THE SAME ONE USED])
                  AC_MSG_WARN([TO BUILD PMIX OR ELSE UNPREDICTABLE BEHAVIOR MAY RESULT])
                  AC_MSG_ERROR([PLEASE CORRECT THE CONFIGURE COMMAND LINE AND REBUILD])])
           AS_IF([test "$opal_hwloc_external_support" != "yes"],
                 [AC_MSG_WARN([EXTERNAL PMIX SUPPORT REQUIRES USE OF EXTERNAL HWLOC])
                  AC_MSG_WARN([LIBRARY THIS LIBRARY MUST POINT TO THE SAME ONE USED ])
                  AC_MSG_WARN([TO BUILD PMIX OR ELSE UNPREDICTABLE BEHAVIOR MAY RESULT])
                  AC_MSG_ERROR([PLEASE CORRECT THE CONFIGURE COMMAND LINE AND REBUILD])])
           external_WRAPPER_EXTRA_CPPFLAGS='-I${includedir}/openmpi/$opal_pmix_external_basedir/pmix -I${includedir}/openmpi/$opal_pmix_external_basedir/pmix/include'

           AC_MSG_CHECKING([if external component is version 2.x])
           AS_IF([test "$opal_external_pmix_version" = "2X"],
                 [AC_MSG_RESULT([yes])
                  opal_pmix_ext2x_happy=yes],
                 [AC_MSG_RESULT([no])
                  opal_pmix_ext2x_happy=no])

           opal_pmix_ext2x_CPPFLAGS=$opal_external_pmix_CPPFLAGS
           opal_pmix_ext2x_LDFLAGS=$opal_external_pmix_LDFLAGS
           opal_pmix_ext2x_LIBS=$opal_external_pmix_LIBS

           AC_SUBST(opal_pmix_ext2x_CPPFLAGS)
           AC_SUBST(opal_pmix_ext2x_LDFLAGS)
           AC_SUBST(opal_pmix_ext2x_LIBS)

           AS_IF([test "$opal_pmix_ext2x_happy" = "yes"],
                 [$1],
                 [$2])],
          [$2])
])dnl
