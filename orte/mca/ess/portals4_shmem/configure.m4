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
# Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2010      Sandia National Laboratories. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
AC_DEFUN([MCA_orte_ess_portals4_shmem_PRIORITY], [30])

# MCA_ess_portals4_shmem_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_ess_portals4_shmem_CONFIG],[
    AC_CONFIG_FILES([orte/mca/ess/portals4_shmem/Makefile])

    OMPI_CHECK_PORTALS4([ess_portals4_shmem],
                     [ess_portals4_shmem_happy="yes"],
                     [ess_portals4_shmem_happy="no"])

    AS_IF([test "$ess_portals4_shmem_happy" = "yes"],
          [ess_portals4_shmem_WRAPPER_EXTRA_LDFLAGS="$ess_portals4_shmem_LDFLAGS"
           ess_portals4_shmem_WRAPPER_EXTRA_LIBS="$ess_portals4_shmem_LIBS"
           $1],
          [$2])

    ess_portals4_shmem_LDFLAGS_SAVE="$LDFLAGS"
    ess_portals4_shmem_LIBS_SAVE="$LIBS"
    LDFLAGS="$LDFLAGS $ess_portals4_shmem_LDFLAGS"
    LIBS="$LIBS $ess_portals4_shmem_LIBS"

    AC_CHECK_FUNC([runtime_get_size], [$1], [$2])

    LDFLAGS="$ess_portals4_shmem_LDFLAGS_SAVE"
    LIBS="$ess_portals4_shmem_LIBS_SAVE"

    # substitute in the things needed to build portals4_shmem
    AC_SUBST([ess_portals4_shmem_CPPFLAGS])
    AC_SUBST([ess_portals4_shmem_LDFLAGS])
    AC_SUBST([ess_portals4_shmem_LIBS])
])dnl
