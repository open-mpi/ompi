# -*- shell-script -*-
#
# Copyright (c) 2013-2014 Intel, Inc. All rights reserved
#
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

    AS_IF([test $opal_common_libfabric_happy -eq 1],
          [$1],
          [$2])

    # substitute in the things needed to build ofi
    AC_SUBST([opal_common_libfabric_CPPFLAGS])
    AC_SUBST([opal_common_libfabric_LIBADD])
])dnl
