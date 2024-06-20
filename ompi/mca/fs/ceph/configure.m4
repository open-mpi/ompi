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
# Copyright (c) 2010-2017 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2008-2012 University of Houston. All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# Copyright (c) 2022      Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


# MCA_fs_ceph_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_fs_ceph_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([fs_ceph_happy])

    AC_CONFIG_FILES([ompi/mca/fs/ceph/Makefile])

    OMPI_CHECK_CEPH([fs_ceph],
                    [fs_ceph_happy="yes"],
                    [fs_ceph_happy="no"])

    AS_IF([test "$fs_ceph_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build ceph
    AC_SUBST([fs_ceph_CPPFLAGS])
    AC_SUBST([fs_ceph_LDFLAGS])
    AC_SUBST([fs_ceph_LIBS])

    OPAL_VAR_SCOPE_POP
])dnl
