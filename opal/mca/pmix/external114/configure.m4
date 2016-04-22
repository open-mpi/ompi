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

# MCA_pmix_external114_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_external114_CONFIG],[
    AC_CONFIG_FILES([opal/mca/pmix/external114/Makefile])

    AC_REQUIRE([OPAL_CHECK_PMIX])

    opal_pmix_external_114_happy="no"
    opal_pmix_external_114_version_good="no"

    AS_IF([test "$opal_external_pmix_happy" = "yes"],
        [OPAL_CHECK_VERSION([opal_pmix_external_114],
                            [$opal_external_pmix_version],
                            ["1.1.4"],
                            [opal_pmix_external_114_version_good="no"],
                            [opal_pmix_external_114_version_good="yes"],
                            [opal_pmix_external_114_version_good="yes"],
                            [opal_pmix_external_114_version_good="no"])

         AS_IF([test "$opal_pmix_external_114_version_good" = "no"],
             [AC_MSG_WARN([External PMIx 1.1.4 component disabled. Incorrect library version ($opal_external_pmix_version)])
              $2],
             [
              # Make sure we have an external libevent
              AS_IF([test "$opal_event_external_support" != "yes"],
                  [AC_MSG_WARN([EXTERNAL PMIX SUPPORT REQUIRES USE OF EXTERNAL LIBEVENT])
                   AC_MSG_WARN([LIBRARY. THIS LIBRARY MUST POINT TO THE SAME ONE USED])
                   AC_MSG_WARN([TO BUILD PMIX OR ELSE UNPREDICTABLE BEHAVIOR MAY RESULT])
                   AC_MSG_ERROR([PLEASE CORRECT THE CONFIGURE COMMAND LINE AND REBUILD])])

              # Make sure we have an external hwloc
              AS_IF([test "$opal_hwloc_external_support" != "yes"],
                  [AC_MSG_WARN([EXTERNAL PMIX SUPPORT REQUIRES USE OF EXTERNAL HWLOC])
                   AC_MSG_WARN([LIBRARY THIS LIBRARY MUST POINT TO THE SAME ONE USED ])
                   AC_MSG_WARN([TO BUILD PMIX OR ELSE UNPREDICTABLE BEHAVIOR MAY RESULT])
                   AC_MSG_ERROR([PLEASE CORRECT THE CONFIGURE COMMAND LINE AND REBUILD])])
             opal_pmix_external_114_happy="yes"
             $1]
        )
        ],
        [$2]
    )

    # External wrapper compiler flags
    #external114_WRAPPER_EXTRA_CPPFLAGS='-I${includedir}/openmpi/$opal_pmix_external114_basedir/pmix -I${includedir}/openmpi/$opal_pmix_external114_basedir/pmix/include'

    OPAL_VAR_SCOPE_POP
])dnl
