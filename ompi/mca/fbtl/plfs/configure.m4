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
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2008-2014 University of Houston. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#


# MCA_fbtl_plfs_CONFIG(action-if-can-compile,
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_fbtl_plfs_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/fbtl/plfs/Makefile])

    OMPI_CHECK_PLFS([fbtl_plfs],
                     [fbtl_plfs_happy="yes"],
                     [fbtl_plfs_happy="no"])

    AS_IF([test "$fbtl_plfs_happy" = "yes"],
          [$1],
          [$2])


    # substitute in the things needed to build plfs
    AC_SUBST([fbtl_plfs_CPPFLAGS])
    AC_SUBST([fbtl_plfs_LDFLAGS])
    AC_SUBST([fbtl_plfs_LIBS])
])dnl
