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
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


# MCA_fs_lustre_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_fs_lustre_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([fs_lustre_happy])

    AC_CONFIG_FILES([ompi/mca/fs/lustre/Makefile])

    OMPI_CHECK_LUSTRE([fs_lustre],
                     [fs_lustre_happy="yes"],
                     [fs_lustre_happy="no"])

    AS_IF([test "$fs_lustre_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build lustre
    AC_SUBST([fs_lustre_CPPFLAGS])
    AC_SUBST([fs_lustre_LDFLAGS])
    AC_SUBST([fs_lustre_LIBS])

    OPAL_VAR_SCOPE_POP
])dnl
