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
# Copyright (c) 2010-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# MCA_ompi_btl_usnic_POST_CONFIG([should_build])
# ------------------------------------------
AC_DEFUN([MCA_ompi_btl_usnic_POST_CONFIG], [
    AM_CONDITIONAL([OMPI_BTL_USNIC_BUILD_UNIT_TESTS],
                   [test "$1" -eq 1 && test "X$enable_ompi_btl_usnic_unit_tests" = "Xyes"])
])

# MCA_btl_usnic_CONFIG([action-if-can-compile],
#                      [action-if-cant-compile])
# ------------------------------------------------
AC_DEFUN([MCA_ompi_btl_usnic_CONFIG],[
    AC_CONFIG_FILES([ompi/mca/btl/usnic/Makefile])

    AC_ARG_WITH([usnic],
                [AS_HELP_STRING([--with-usnic],
                                [If specified, cause an error if usNIC
                                 support cannot be built])])

    # If --without-usnic was specified, then gracefully exit.
    # Otherwise, do the rest of the config.
    AS_IF([test "x$with_usnic" = "xno"],
          [AC_MSG_WARN([--without-usnic specified; skipping usnic BTL])
           $2],
          [_OMPI_BTL_USNIC_DO_CONFIG($1, $2)])
])

AC_DEFUN([_OMPI_BTL_USNIC_DO_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([unit_tests])

    # see README.test for information about this scheme
    AC_ARG_ENABLE([ompi-btl-usnic-unit-tests],
                  [AS_HELP_STRING([--enable-ompi-btl-usnic-unit-tests],
                                  [build unit tests for the usnic BTL,
                                   including the test runner program,
                                   ompi_btl_usnic_run_tests])])
    AS_IF([test "X$enable_ompi_btl_usnic_unit_tests" = "Xyes"],
          [unit_tests=1
           AC_MSG_NOTICE([enabling usnic BTL unit tests])],
          [unit_tests=0])
    AC_DEFINE_UNQUOTED([OMPI_BTL_USNIC_UNIT_TESTS], [$unit_tests],
                       [define to 1 if usnic BTL unit tests are enabled, 0 otherwise])
    unset unit_tests

    # The current logic in btl_usnic_compat.h checks the OMPI version as a
    # proxy for the top-level OMPI version.  Unfortunately this does the wrong
    # thing for other top-level projects that might use the usnic BTL, such as
    # ORCM.  ORCM's versioning is totally unrelated to OMPI's.  As a short term
    # workaround, just disqualify ourselves if the OMPI version seems too old.
    # In the longer term we should be doing something else, like versioning
    # OMPI and OMPI separately.
    ompi_btl_usnic_happy=yes
    AS_IF([test "$OMPI_MAJOR_VERSION" -eq "1" && \
           test "$OMPI_MINOR_VERSION" -lt "7"],
          [AC_MSG_NOTICE([OMPI version appears to be too old, disabling the usnic BTL])
           ompi_btl_usnic_happy=no])

    # We only want to build on 64 bit Linux.
    AS_IF([test "$ompi_btl_usnic_happy" = "yes"],
          [AC_CHECK_SIZEOF([void *])
           AC_MSG_CHECKING([for 64 bit Linux])
           case $host_os in
               *linux*)
                   AS_IF([test $ac_cv_sizeof_void_p -eq 8],
                         [],
                         [ompi_btl_usnic_happy=no])
                   ;;
               *)
                   ompi_btl_usnic_happy=no
                   ;;
           esac
           AC_MSG_RESULT([$ompi_btl_usnic_happy])
          ])

    # The usnic BTL requires libfabric support.
    AS_IF([test "$ompi_btl_usnic_happy" = "yes"],
          [OPAL_CHECK_LIBFABRIC([ompi_btl_usnic],
              [ompi_btl_usnic_happy=yes],
              [ompi_btl_usnic_happy=no])])

    # The usnic BTL requires at least libfabric v1.1 (there was a
    # critical bug in libfabric v1.0).
    AS_IF([test "$opal_btl_usnic_happy" = "yes"],
          [AC_MSG_CHECKING([whether libfabric is >= v1.1])
           opal_btl_usnic_CPPFLAGS_save=$CPPFLAGS
           CPPFLAGS="$opal_common_libfabric_CPPFLAGS $CPPFLAGS"
           AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <rdma/fabric.h>]],
[[
#if !defined(FI_MAJOR_VERSION)
#error your version of libfabric is too old
#elif FI_VERSION(FI_MAJOR_VERSION, FI_MINOR_VERSION) < FI_VERSION(1, 1)
#error your version of libfabric is too old
#endif
]])],
                 [opal_btl_usnic_happy=yes],
                 [opal_btl_usnic_happy=no])
           AC_MSG_RESULT([$opal_btl_usnic_happy])
           CPPFLAGS=$opal_btl_usnic_CPPFLAGS_save
          ])

    # Make sure we can find the libfabric usnic extensions header
    AS_IF([test "$ompi_btl_usnic_happy" = "yes" ],
          [ompi_btl_usnic_CPPFLAGS_save=$CPPFLAGS
           CPPFLAGS="$ompi_btl_usnic_CPPFLAGS $CPPFLAGS"
           AC_CHECK_HEADER([rdma/fi_ext_usnic.h],
                            [],
                            [ompi_btl_usnic_happy=no])
           CPPFLAGS=$ompi_btl_usnic_CPPFLAGS_save
          ])

    # All done
    AS_IF([test "$ompi_btl_usnic_happy" = "yes"],
          [$1
           btl_usnic_WRAPPER_EXTRA_LDFLAGS=$ompi_btl_usnic_LDFLAGS
           btl_usnic_WRAPPER_EXTRA_LIBS=$ompi_btl_usnic_LIBS],
          [AS_IF([test "$with_usnic" = "yes"],
                 [AC_MSG_WARN([--with-usnic was specified, but Cisco usNIC support cannot be built])
                  AC_MSG_ERROR([Cannot continue])],
                 [$2])
          ])

    OPAL_VAR_SCOPE_POP
])dnl
