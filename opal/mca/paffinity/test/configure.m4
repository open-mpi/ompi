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
# Copyright (c) 2007-2008 Cisco, Inc. All rights reserved.
# Copyright (c) 2008      Sun Microsystems, Inc. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# MCA_paffinity_test_CONFIG([action-if-found], [action-if-not-found])
# -----------------------------------------------------------
AC_DEFUN([MCA_paffinity_test_CONFIG],[
    OMPI_VAR_SCOPE_PUSH([paff_test_happy])
    # check to see if we have <mach/mach_host.h>
    # as this is a Darwin-specific thing and
    # we are a test module for that environment
    AC_CHECK_HEADER([mach/mach_host.h], [paff_test_happy=yes], [paff_test_happy=no])

    AS_IF([test "$paff_test_happy" = "yes"], [$1], [$2])
    OMPI_VAR_SCOPE_POP
])dnl

