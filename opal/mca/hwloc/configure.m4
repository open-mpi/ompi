dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2010-2017 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2020      Amazon.com, Inc. or its affiliates.  All Rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

#
# In days of old, hwloc was packaged as multiple MCA components, and
# grew an extensive set of base code to support Open MPI's use of
# hwloc.  When internal builds of libevent, hwloc, and hwloc were moved
# out of components into base code so that they could be shared
# between Open MPI and PRRTE without incurring linking hell, we left
# the base code active.  This MCA framework is essentially defunct;
# its only purpose is to allow continued use of the base code.
#
# We do not expect to find any components, nor do we allow any to be
# configured.  The top-level configure will set all the flags in all
# the right places needed to link hwloc.
#
AC_DEFUN([MCA_opal_hwloc_CONFIG],[
    AC_CONFIG_FILES([$1/mca/$2/Makefile])
])
