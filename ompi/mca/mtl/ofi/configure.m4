# -*- shell-script -*-
#
# Copyright (c) 2013-2014 Intel, Inc. All rights reserved
#
# Copyright (c) 2014-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
#                         reserved.
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

    # Check for OFI
    OPAL_CHECK_OFI

    # The OFI MTL requires at least OFI libfabric v1.5.
    AS_IF([test "$opal_ofi_happy" = "yes"],
          [OPAL_CHECK_OFI_VERSION_GE([1,5],
                                     [],
                                     [opal_ofi_happy=no])])

    AS_IF([test "$opal_ofi_happy" = "yes"],
          [$1],
          [$2])
])dnl
