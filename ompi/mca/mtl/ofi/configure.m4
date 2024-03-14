# -*- autoconf -*-
#
# Copyright (c) 2013-2014 Intel, Inc. All rights reserved
#
# Copyright (c) 2014-2020 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
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
    OPAL_VAR_SCOPE_PUSH([mtl_ofi_happy])

    AC_CONFIG_FILES([ompi/mca/mtl/ofi/Makefile])

    OPAL_CHECK_OFI([mtl_ofi],
                   [mtl_ofi_happy=1],
                   [mtl_ofi_happy=0])

    dnl The OFI MTL requires at least OFI libfabric v1.9.
    AS_IF([test ${mtl_ofi_happy} -eq 1],
          [OPAL_CHECK_OFI_VERSION_GE([1,9],
                                     [],
                                     [mtl_ofi_happy=0])])

    AS_IF([test ${mtl_ofi_happy} -eq 1],
          [OPAL_MCA_CHECK_DEPENDENCY([opal], [btl], [ofi], [opal], [common], [ofi])])

    AS_IF([test ${mtl_ofi_happy} -eq 1],
          [$1],
          [AS_IF([test -n "${with_ofi}" -a "${with_ofi}" != "no"],
                 [AC_MSG_WARN([OFI libfabric support requested (via --with-ofi or --with-libfabric), but not found.])
                  AC_MSG_ERROR([Cannot continue.])])
           $2])

    AC_SUBST([mtl_ofi_CPPFLAGS])
    AC_SUBST([mtl_ofi_LDFLAGS])
    AC_SUBST([mtl_ofi_LIBS])

    OPAL_VAR_SCOPE_POP
])dnl
