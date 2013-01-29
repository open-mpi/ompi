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
# Copyright (c) 2008-2012 University of Houston. All rights reserved.
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
    AC_CONFIG_FILES([ompi/mca/fs/lustre/Makefile])

    OMPI_CHECK_LUSTRE([fs_lustre],
                     [fs_lustre_happy="yes"],
                     [fs_lustre_happy="no"])

    AS_IF([test "$fs_lustre_happy" = "yes"],
          [$1],
          [$2])

#    AC_CHECK_HEADERS([lustre/liblustreapi.h], [],
#                      [AC_CHECK_HEADERS([lustre/liblustreapi.h], [], [$2],
#                          [AC_INCLUDES_DEFAULT])],
#                      [AC_INCLUDES_DEFAULT])


    # substitute in the things needed to build lustre
    AC_SUBST([fs_lustre_CPPFLAGS])
    AC_SUBST([fs_lustre_LDFLAGS])
    AC_SUBST([fs_lustre_LIBS])
])dnl
