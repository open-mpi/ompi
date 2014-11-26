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
# Copyright (c) 2007-2012 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# Copyright (c) 2014      Los Alamos National Security, LLC.
#                         All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_orte_common_alps_CONFIG([action-if-can-compile], 
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_orte_common_alps_CONFIG],[
    AC_CONFIG_FILES([orte/mca/common/alps/Makefile])

    ORTE_CHECK_ALPS([common_alps], [common_alps_happy="yes"], [common_alps_happy="no"])

    AS_IF([test "$common_alps_happy" = "yes"],
          [$1
           AC_SUBST([common_alps_CPPFLAGS])
           AC_SUBST([common_alps_LDFLAGS])
           AC_SUBST([common_alps_LIBS])],
          [$2])
    #
])dnl
