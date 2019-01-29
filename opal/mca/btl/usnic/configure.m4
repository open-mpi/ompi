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
# Copyright (c) 2006      Sandia National Laboratories. All rights
#                         reserved.
# Copyright (c) 2010-2019 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2017      Los Alamos National Security, LLC.  All rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_opal_btl_usnic_POST_CONFIG([should_build])
# ------------------------------------------
AC_DEFUN([MCA_opal_btl_usnic_POST_CONFIG], [
    AM_CONDITIONAL([OPAL_BTL_USNIC_BUILD_UNIT_TESTS],
                   [test "$1" -eq 1 && test "X$enable_opal_btl_usnic_unit_tests" = "Xyes"])
])

# MCA_btl_usnic_CONFIG([action-if-can-copalle],
#                      [action-if-cant-copalle])
# ------------------------------------------------
AC_DEFUN([MCA_opal_btl_usnic_CONFIG],[
    AC_CONFIG_FILES([opal/mca/btl/usnic/Makefile])

    AC_ARG_WITH([usnic],
                [AS_HELP_STRING([--with-usnic],
                                [If specified, cause an error if usNIC
                                 support cannot be built])])

    # If --without-usnic was specified, then gracefully exit.
    # Otherwise, do the rest of the config.
    AS_IF([test "x$with_usnic" = "xno"],
          [AC_MSG_WARN([--without-usnic specified; skipping usnic BTL])
           $2],
          [_OPAL_BTL_USNIC_DO_CONFIG($1, $2)])
])

AC_DEFUN([_OPAL_BTL_USNIC_DO_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([unit_tests])

    # see README.test for information about this scheme
    AC_ARG_ENABLE([opal-btl-usnic-unit-tests],
                  [AS_HELP_STRING([--enable-opal-btl-usnic-unit-tests],
                                  [build unit tests for the usnic BTL,
                                   including the test runner program,
                                   opal_btl_usnic_run_tests])])
    AS_IF([test "X$enable_opal_btl_usnic_unit_tests" = "Xyes"],
          [unit_tests=1
           AC_MSG_NOTICE([enabling usnic BTL unit tests])],
          [unit_tests=0])
    AC_DEFINE_UNQUOTED([OPAL_BTL_USNIC_UNIT_TESTS], [$unit_tests],
                       [define to 1 if usnic BTL unit tests are enabled, 0 otherwise])
    unset unit_tests

    # The current logic in btl_usnic_compat.h checks the OPAL version as a
    # proxy for the top-level OPAL version.  Unfortunately this does the wrong
    # thing for other top-level projects that might use the usnic BTL, such as
    # ORCM.  ORCM's versioning is totally unrelated to OPAL's.  As a short term
    # workaround, just disqualify ourselves if the OPAL version seems too old.
    # In the longer term we should be doing something else, like versioning
    # OPAL and OPAL separately.
    opal_btl_usnic_happy=yes
    AS_IF([test "$OPAL_MAJOR_VERSION" -eq "1" && \
           test "$OPAL_MINOR_VERSION" -lt "7"],
          [AC_MSG_NOTICE([OPAL version appears to be too old, disabling the usnic BTL])
           opal_btl_usnic_happy=no])

    # We only want to build on 64 bit Linux.
    AS_IF([test "$opal_btl_usnic_happy" = "yes"],
          [AC_CHECK_SIZEOF([void *])
           AC_MSG_CHECKING([for 64 bit Linux])
           case $host_os in
               *linux*)
                   AS_IF([test $ac_cv_sizeof_void_p -eq 8],
                         [],
                         [opal_btl_usnic_happy=no])
                   ;;
               *)
                   opal_btl_usnic_happy=no
                   ;;
           esac
           AC_MSG_RESULT([$opal_btl_usnic_happy])
          ])

    AS_IF([test "$opal_btl_usnic_happy" = "yes"],
          [ # The usnic BTL requires OFI libfabric support
           OPAL_CHECK_OFI
           opal_btl_usnic_happy=$opal_ofi_happy])

    # The usnic BTL requires at least OFI libfabric v1.1 (there was a
    # critical bug in libfabric v1.0).
    AS_IF([test "$opal_btl_usnic_happy" = "yes"],
          [AC_MSG_CHECKING([whether OFI libfabric is >= v1.1])
           opal_btl_usnic_CPPFLAGS_save=$CPPFLAGS
           CPPFLAGS="$opal_ofi_CPPFLAGS $CPPFLAGS"
           AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <rdma/fabric.h>]],
[[
#if !defined(FI_MAJOR_VERSION)
#error your version of OFI libfabric is too old
#elif FI_VERSION(FI_MAJOR_VERSION, FI_MINOR_VERSION) < FI_VERSION(1, 1)
#error your version of OFI libfabric is too old
#endif
]])],
                 [opal_btl_usnic_happy=yes],
                 [opal_btl_usnic_happy=no])
           AC_MSG_RESULT([$opal_btl_usnic_happy])
           CPPFLAGS=$opal_btl_usnic_CPPFLAGS_save
          ])

    # Make sure we can find the OFI libfabric usnic extensions header
    AS_IF([test "$opal_btl_usnic_happy" = "yes" ],
          [opal_btl_usnic_CPPFLAGS_save=$CPPFLAGS
           CPPFLAGS="$opal_ofi_CPPFLAGS $CPPFLAGS"
           AC_CHECK_HEADER([rdma/fi_ext_usnic.h],
                            [],
                            [opal_btl_usnic_happy=no])
           CPPFLAGS=$opal_btl_usnic_CPPFLAGS_save
          ])

    # All done
    AS_IF([test "$opal_btl_usnic_happy" = "yes"],
          [$1],
          [AS_IF([test "$with_usnic" = "yes"],
                 [AC_MSG_WARN([--with-usnic was specified, but Cisco usNIC support cannot be built])
                  AC_MSG_ERROR([Cannot continue])],
                 [$2])
          ])

    OPAL_SUMMARY_ADD([[Transports]],[[Cisco usNIC]],[[btl_usnic]],[$opal_btl_usnic_happy])
    OPAL_VAR_SCOPE_POP
])dnl
