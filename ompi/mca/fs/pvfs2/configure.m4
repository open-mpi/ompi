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


# MCA_fs_pvfs2_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_fs_pvfs2_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/fs/pvfs2/Makefile])

    OMPI_CHECK_PVFS2([fs_pvfs2],
                     [fs_pvfs2_happy="yes"],
                     [fs_pvfs2_happy="no"])

    AS_IF([test "$fs_pvfs2_happy" = "yes"],
          [$1],
          [$2])

#    AC_CHECK_HEADERS([pvfs2.h], [],
#                      [AC_CHECK_HEADERS([pvfs2.h], [], [$2],
#                          [AC_INCLUDES_DEFAULT])],
#                      [AC_INCLUDES_DEFAULT])


    # substitute in the things needed to build pvfs2
    AC_SUBST([fs_pvfs2_CPPFLAGS])
    AC_SUBST([fs_pvfs2_LDFLAGS])
    AC_SUBST([fs_pvfs2_LIBS])
])dnl
