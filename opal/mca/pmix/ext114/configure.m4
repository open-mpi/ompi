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
# Copyright (c) 2013-2016 Intel, Inc. All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2014-2015 Mellanox Technologies, Inc.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pmix_ext114_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_opal_pmix_ext114_CONFIG],[
    AC_CONFIG_FILES([opal/mca/pmix/ext114/Makefile])

    AC_REQUIRE([OPAL_CHECK_PMIX])

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
           # check for the 1.1.4 version by looking for a function
           # which was later removed
           AC_MSG_CHECKING([if external component is version 1.1.4])
           OPAL_CHECK_PACKAGE([opal_pmix_ext114],
                              [pmix.h],
                              [pmix],
                              [PMIx_Register_errhandler],
                              [-lpmix],
                              [$pmix_ext_install_dir],
                              [$pmix_ext_install_dir/lib],
                              [AC_MSG_RESULT([yes])
                               opal_pmix_external_114_happy=yes],
                              [AC_MSG_RESULT([no])
                               opal_pmix_external_114_happy=no])

           AC_SUBST(opal_pmix_ext114_CPPFLAGS)
           AC_SUBST(opal_pmix_ext114_LDFLAGS)
           AC_SUBST(opal_pmix_ext114_LIBS)

           AS_IF([test "$opal_pmix_external_114_happy" = "yes"],
                 [$1
                 # need to set the wrapper flags for static builds
                 pmix_ext114_WRAPPER_EXTRA_LDFLAGS="$opal_pmix_ext114_LDFLAGS"
                 pmix_ext114_WRAPPER_EXTRA_LIBS="$opal_pmix_ext114_LIBS"],
                 [$2])],
          [$2])
])dnl
