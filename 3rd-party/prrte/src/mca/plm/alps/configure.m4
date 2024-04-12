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
# Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2011-2016 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2019      Intel, Inc.  All rights reserved.
# Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_plm_alps_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_prte_plm_alps_CONFIG],[
    AC_CONFIG_FILES([src/mca/plm/alps/Makefile])

    PRTE_CHECK_ALPS([plm_alps], [plm_alps_happy="yes"], [plm_alps_happy="no"])

    AS_IF([test "$plm_alps_happy" = "yes"],
          [$1
           AC_SUBST([plm_alps_CPPFLAGS])
           AC_SUBST([plm_alps_LDFLAGS])
           AC_SUBST([plm_alps_LIBS])],
          [$2])

])dnl
