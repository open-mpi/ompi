# -*- autoconf -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      QLogic Corp. All rights reserved.
# Copyright (c) 2009-2019 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2011-2018 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2018      Intel, inc. All rights reserved
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OPAL_CHECK_OFI(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if OFI support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found

AC_DEFUN([MCA_opal_btl_ofi_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([btl_ofi_happy CPPFLAGS_save])

    AC_CONFIG_FILES([opal/mca/btl/ofi/Makefile])

    # Check for OFI
    OPAL_CHECK_OFI([btl_ofi], [btl_ofi_happy=1], [btl_ofi_happy=0])

    AS_IF([test ${btl_ofi_happy} -eq 1],
          [CPPFLAGS_save=${CPPFLAGS}
           OPAL_FLAGS_APPEND_UNIQ([CPPFLAGS], [${btl_ofi_CPPFLAGS}])
           AC_CHECK_DECL([FI_MR_VIRT_ADDR], [], [btl_ofi_happy=0],
                         [#include <rdma/fabric.h>])
           CPPFLAGS=${CPPFLAGS_save}])

    AS_IF([test ${btl_ofi_happy} -eq 1],
          [OPAL_MCA_CHECK_DEPENDENCY([opal], [btl], [ofi], [opal], [common], [ofi])])

    AS_IF([test ${btl_ofi_happy} -eq 1],
          [$1],
          [AS_IF([test -n "${with_ofi}" -a "${with_ofi}" != "no"],
                 [AC_MSG_WARN([OFI libfabric support requested (via --with-ofi or --with-libfabric), but not found.])
                  AC_MSG_ERROR([Cannot continue.])])
           $2])

    AC_SUBST([btl_ofi_CPPFLAGS])
    AC_SUBST([btl_ofi_LDFLAGS])
    AC_SUBST([btl_ofi_LIBS])

    OPAL_VAR_SCOPE_POP
])dnl
