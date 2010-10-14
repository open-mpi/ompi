# -*- shell-script -*-
#
# Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_mtl_portals4_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_mtl_portals4_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/mtl/portals4/Makefile])

    OMPI_CHECK_PORTALS4([mtl_portals4],
                     [mtl_portals4_happy="yes"],
                     [mtl_portals4_happy="no"])

    AS_IF([test "$mtl_portals4_happy" = "yes"],
          [mtl_portals4_WRAPPER_EXTRA_LDFLAGS="$mtl_portals4_LDFLAGS"
           mtl_portals4_WRAPPER_EXTRA_LIBS="$mtl_portals4_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build portals4
    AC_SUBST([mtl_portals4_CPPFLAGS])
    AC_SUBST([mtl_portals4_LDFLAGS])
    AC_SUBST([mtl_portals4_LIBS])
])dnl
