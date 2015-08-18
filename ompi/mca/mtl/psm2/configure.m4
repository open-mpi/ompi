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
# Copyright (c) 2014      Intel Corporation. All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_mtl_psm2_POST_CONFIG(will_build)
# ----------------------------------------
# Only require the tag if we're actually going to be built
AC_DEFUN([MCA_ompi_mtl_psm2_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([MTL])])
])dnl

# MCA_mtl_psm2_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_mtl_psm2_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/mtl/psm2/Makefile])

    OMPI_CHECK_PSM2([mtl_psm2],
                     [mtl_psm2_happy="yes"],
                     [mtl_psm2_happy="no"])

    AS_IF([test "$mtl_psm2_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build psm2
    AC_SUBST([mtl_psm2_CFLAGS])
    AC_SUBST([mtl_psm2_CPPFLAGS])
    AC_SUBST([mtl_psm2_LDFLAGS])
    AC_SUBST([mtl_psm2_LIBS])
])dnl
