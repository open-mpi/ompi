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


# MCA_fbtl_pvfs2_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_fbtl_pvfs2_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/fbtl/pvfs2/Makefile])

    OMPI_CHECK_PVFS2([fbtl_pvfs2],
                     [fbtl_pvfs2_happy="yes"],
                     [fbtl_pvfs2_happy="no"])

    AS_IF([test "$fbtl_pvfs2_happy" = "yes"],
          [$1],
          [$2])



    # substitute in the things needed to build pvfs2
    AC_SUBST([fbtl_pvfs2_CPPFLAGS])
    AC_SUBST([fbtl_pvfs2_LDFLAGS])
    AC_SUBST([fbtl_pvfs2_LIBS])
])dnl
