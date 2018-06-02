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
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      QLogic Corp. All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011-2018 Los Alamos National Security, LLC.
#                         All rights reserved.
# Copyright (c) 2018      Intel, inc. All rights reserved
#
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
    AC_CONFIG_FILES([opal/mca/btl/ofi/Makefile])

    AC_REQUIRE([MCA_opal_common_ofi_CONFIG])

    AS_IF([test "$opal_common_ofi_happy" = "yes"],
          [$1],
          [$2])

    # substitute in the things needed to build ofi
    AC_SUBST([btl_ofi_CPPFLAGS])
    AC_SUBST([btl_ofi_LDFLAGS])
    AC_SUBST([btl_ofi_LIBS])
])dnl
