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
# Copyright (c) 2010-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013-2015 Intel, Inc. All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2014-2015 Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_external_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_external_CONFIG],[
    AC_CONFIG_FILES([opal/mca/pmix/external/Makefile])

    AC_REQUIRE([OPAL_CHECK_PMIX])

    # We have a different external comment for the 1.1.4 series, so check that
    AS_IF([test "$opal_external_pmix_happy" = "yes" ],
          [
           OPAL_CHECK_VERSION([opal_pmix_external],
                              [$opal_external_pmix_version],
                              ["1.1.4"],
                              [opal_external_pmix_version_happy="yes"],
                              [AC_MSG_WARN([Defer to the 1.1.4 external component])
                               opal_external_pmix_version_happy="no"],
                              [AC_MSG_WARN([Defer to the 1.1.4 external component])
                               opal_external_pmix_version_happy="no"],
                              [opal_external_pmix_version_happy="yes"])
           ])

    AS_IF([test "$opal_external_pmix_happy" = "yes" && test "$opal_external_pmix_version_happy" = "yes" ],
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
           $1],
          [$2])
])dnl
