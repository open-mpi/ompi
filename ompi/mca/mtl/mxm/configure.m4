# -*- shell-script -*-
#
# Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
# Copyright (c) 2013      Sandia National Laboratories.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_mtl_mxm_POST_CONFIG(will_build)
# ----------------------------------------
# Only require the tag if we're actually going to be built
AC_DEFUN([MCA_ompi_mtl_mxm_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([MTL])])
])dnl

# MCA_mtl_mxm_CONFIG([action-if-can-compile],
#                    [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_mtl_mxm_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/mtl/mxm/Makefile])

    OMPI_CHECK_MXM([mtl_mxm],
                   [mtl_mxm_happy="yes"],
                   [mtl_mxm_happy="no"])

    AS_IF([test "$mtl_mxm_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build mxm
    AC_SUBST([mtl_mxm_CFLAGS])
    AC_SUBST([mtl_mxm_CPPFLAGS])
    AC_SUBST([mtl_mxm_LDFLAGS])
    AC_SUBST([mtl_mxm_LIBS])
])dnl

