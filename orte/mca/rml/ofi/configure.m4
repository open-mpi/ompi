# -*- shell-script -*-
#
# Copyright (c) 2013-2014 Intel, Inc. All rights reserved
#
# Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_orte_rml_ofi_POST_CONFIG(will_build)
# ----------------------------------------
# Only require the tag if we're actually going to be built

# MCA_mtl_ofi_CONFIG([action-if-can-compile],
#                    [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_orte_rml_ofi_CONFIG],[
    AC_CONFIG_FILES([orte/mca/rml/ofi/Makefile])

    # ensure we already ran the common OFI libfabric config
    AC_REQUIRE([MCA_opal_common_ofi_CONFIG])

    AS_IF([test "$opal_common_ofi_happy" = "yes"],
          [$1],
          [$2])
])dnl
