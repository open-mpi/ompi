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
# Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
# Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_pnet_opa_CONFIG([action-if-can-compile],
#                     [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_pmix_pnet_opa_CONFIG],[
    AC_CONFIG_FILES([src/mca/pnet/opa/Makefile])

    PMIX_CHECK_PSM2([pnet_opa],
                    [pnet_opa_happy="yes"],
                    [pnet_opa_happy="no"])

    AS_IF([test "$pnet_opa_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build psm2
    AC_SUBST([pnet_opa_CFLAGS])
    AC_SUBST([pnet_opa_CPPFLAGS])
    AC_SUBST([pnet_opa_LDFLAGS])
    AC_SUBST([pnet_opa_LIBS])
])dnl
