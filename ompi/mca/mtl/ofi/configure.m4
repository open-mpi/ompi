# -*- shell-script -*-
#
# Copyright (c) 2013-2015 Intel, Inc. All rights reserved
#
# Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_mtl_ofi_POST_CONFIG(will_build)
# ----------------------------------------
# Only require the tag if we're actually going to be built
AC_DEFUN([MCA_ompi_mtl_ofi_POST_CONFIG], [
    AS_IF([test "$1" = "1"], [OMPI_REQUIRE_ENDPOINT_TAG([MTL])])
])dnl

# MCA_mtl_ofi_CONFIG([action-if-can-compile],
#                    [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_mtl_ofi_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/mtl/ofi/Makefile])

    OPAL_CHECK_LIBFABRIC([ompi_mtl_ofi],
          [$1
           mtl_ofi_WRAPPER_EXTRA_LDFLAGS="$ompi_mtl_ofi_LDFLAGS"
           mtl_ofi_WRAPPER_EXTRA_LIBS="$ompi_mtl_ofi_LIBS"],
          [$2])

    AC_SUBST(ompi_mtl_ofi_LDFLAGS)
    AC_SUBST(ompi_mtl_ofi_LIBS)
])dnl
