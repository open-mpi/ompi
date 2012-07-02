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
# Copyright (c) 2007-2009 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_common_ofautils_CONFIG([action-if-can-compile], 
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_common_ofautils_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/common/ofautils/Makefile])
    common_ofautils_happy="no"
    OMPI_CHECK_OPENFABRICS([common_ofautils],
                           [common_ofautils_happy="yes"])

    AS_IF([test "$common_ofautils_happy" = "yes"],
          [common_ofautils_WRAPPER_EXTRA_LDFLAGS="$common_ofautils_LDFLAGS"
           common_ofautils_WRAPPER_EXTRA_LIBS="$common_ofautils_LIBS"
           $1],
          [$2])

    # substitute in the things needed to build openib
    AC_SUBST([common_ofautils_CFLAGS])
    AC_SUBST([common_ofautils_CPPFLAGS])
    AC_SUBST([common_ofautils_LDFLAGS])
    AC_SUBST([common_ofautils_LIBS])
])dnl
