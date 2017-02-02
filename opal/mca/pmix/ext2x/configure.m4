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
# Copyright (c) 2010-2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2013-2016 Intel, Inc. All rights reserved.
# Copyright (c) 2015-2016 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
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

    # check to see
    # if we are linking to an external v2.x library. If not, then
    # do not use this component.
    AC_MSG_CHECKING([if external v2.x component is to be used])
    AS_IF([test "$opal_external_pmix_happy" = "yes"],
          [AS_IF([test "$opal_external_pmix_version" = "2x"],
                 [AC_MSG_RESULT([yes - using an external v2.x library])
                  opal_pmix_ext2x_happy=1
                  # Build flags for our Makefile.am
                  opal_pmix_ext2x_CPPFLAGS=$opal_external_pmix_CPPFLAGS
                  opal_pmix_ext2x_LDFLAGS=$opal_external_pmix_LDFLAGS
                  opal_pmix_ext2x_LIBS=$opal_external_pmix_LIBS
                  # setup wrapper flags
                  pmix_ext2x_WRAPPER_EXTRA_LDFLAGS=$opal_external_pmix_LDFLAGS
                  pmix_ext2x_WRAPPER_EXTRA_LIBS=$opal_external_pmix_LIBS],
                 [AC_MSG_RESULT([no - disqualifying this component])
                  opal_pmix_ext2x_happy=0])],
          [AC_MSG_RESULT([no - disqualifying this component])
           opal_pmix_ext2x_happy=0])

   AC_SUBST([opal_pmix_ext2x_LIBS])
   AC_SUBST([opal_pmix_ext2x_CPPFLAGS])
   AC_SUBST([opal_pmix_ext2x_LDFLAGS])
   AC_SUBST([opal_pmix_ext2x_DEPENDENCIES])

    AS_IF([test $opal_pmix_ext2x_happy -eq 1],
          [$1],
          [$2])

])dnl
