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
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_mtl_psm_POST_CONFIG(will_build)
# ----------------------------------------
# Only require the tag if we're actually going to be built
AC_DEFUN([MCA_ompi_mtl_psm_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([MTL])])
])dnl

# MCA_mtl_psm_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_mtl_psm_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/mtl/psm/Makefile])

    OMPI_CHECK_PSM([mtl_psm],
                     [mtl_psm_happy="yes"],
                     [mtl_psm_happy="no"])

    AS_IF([test "$mtl_psm_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build psm
    AC_SUBST([mtl_psm_CFLAGS])
    AC_SUBST([mtl_psm_CPPFLAGS])
    AC_SUBST([mtl_psm_LDFLAGS])
    AC_SUBST([mtl_psm_LIBS])
])dnl

