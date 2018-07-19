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
# Copyright (c) 2013-2018 Intel, Inc. All rights reserved.
# Copyright (c) 2015-2017 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2014-2015 Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_ext1x_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_ext1x_CONFIG],[
    AC_CONFIG_FILES([opal/mca/pmix/ext1x/Makefile])

    AS_IF([test "$opal_external_pmix_happy" = "yes"],
          [ # check for the 1.x version ( >= 1.1.4 ?)
           AC_MSG_CHECKING([if external component is version 1.x])
           AS_IF([test "$opal_external_pmix_version" = "1x"],
                 [AC_MSG_RESULT([yes])
                  AS_IF([test "$opal_event_external_support" != "yes"],
                        [AC_MSG_WARN([EXTERNAL PMIX SUPPORT REQUIRES USE OF EXTERNAL LIBEVENT])
                         AC_MSG_WARN([LIBRARY. THIS LIBRARY MUST POINT TO THE SAME ONE USED])
                         AC_MSG_WARN([TO BUILD PMIX OR ELSE UNPREDICTABLE BEHAVIOR MAY RESULT])
                         AC_MSG_ERROR([PLEASE CORRECT THE CONFIGURE COMMAND LINE AND REBUILD])])
                  AS_IF([test "$opal_hwloc_external_support" != "yes"],
                        [AC_MSG_WARN([EXTERNAL PMIX SUPPORT REQUIRES USE OF EXTERNAL HWLOC])
                         AC_MSG_WARN([LIBRARY THIS LIBRARY MUST POINT TO THE SAME ONE USED ])
                         AC_MSG_WARN([TO BUILD PMIX OR ELSE UNPREDICTABLE BEHAVIOR MAY RESULT])
                         AC_MSG_ERROR([PLEASE CORRECT THE CONFIGURE COMMAND LINE AND REBUILD])])
                  opal_pmix_external_1x_happy=yes],
                 [AC_MSG_RESULT([no])
                  opal_pmix_external_1x_happy=no])

           AS_IF([test "$opal_pmix_external_1x_happy" = "yes"],
                 [$1
                  # need to set the wrapper flags for static builds
                  pmix_ext1x_WRAPPER_EXTRA_LDFLAGS=$opal_external_pmix_LDFLAGS
                  pmix_ext1x_WRAPPER_EXTRA_LIBS=$opal_external_pmix_LIBS],
                 [$2])],
          [$2])

    opal_pmix_ext1x_CPPFLAGS=$opal_external_pmix_CPPFLAGS
    opal_pmix_ext1x_LDFLAGS=$opal_external_pmix_LDFLAGS
    opal_pmix_ext1x_LIBS=$opal_external_pmix_LIBS

    AC_SUBST([opal_pmix_ext1x_CPPFLAGS])
    AC_SUBST([opal_pmix_ext1x_LDFLAGS])
    AC_SUBST([opal_pmix_ext1x_LIBS])

])dnl
