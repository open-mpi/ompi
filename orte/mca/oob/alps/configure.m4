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
# Copyright (c) 2011-2015 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_oob_alps_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_orte_oob_alps_CONFIG],[
    AC_CONFIG_FILES([orte/mca/oob/alps/Makefile])

    ORTE_CHECK_ALPS([oob_alps], [oob_alps_happy="yes"], [oob_alps_happy="no"])

    AS_IF([test "$oob_alps_happy" = "yes"], 
          [$1
           AC_SUBST([oob_alps_CPPFLAGS])
           AC_SUBST([oob_alps_LDFLAGS])
           AC_SUBST([oob_alps_LIBS])], 
          [$2])
])dnl

