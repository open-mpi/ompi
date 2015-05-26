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
    OPAL_VAR_SCOPE_PUSH([unit_tests ompi_btl_usnic_CPPFLAGS_save])

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
    btl_usnic_happy=yes
    AS_IF([test "$OMPI_MAJOR_VERSION" -eq "1" && \
           test "$OMPI_MINOR_VERSION" -lt "7"],
          [AC_MSG_NOTICE([OMPI version appears to be too old, disabling the usnic BTL])
           btl_usnic_happy=no])

    # We only want to build on 64 bit Linux.
    AS_IF([test "$btl_usnic_happy" = "yes"],
          [AC_CHECK_SIZEOF([void *])
           AC_MSG_CHECKING([for 64 bit Linux])
           case $host_os in
               *linux*)
                   AS_IF([test $ac_cv_sizeof_void_p -eq 8],
                         [],
                         [btl_usnic_happy=no])
                   ;;
               *)
                   btl_usnic_happy=no
                   ;;
           esac
           AC_MSG_RESULT([$btl_usnic_happy])
          ])

    # The usnic BTL requires libfabric support.  libfabric should
    # already have been configured, so just see if it was happy.
    AS_IF([test "$btl_usnic_happy" = "yes"],
          [AC_MSG_CHECKING([if libfabric support is available])
           AS_IF([test $opal_common_libfabric_happy -eq 1],
                 [AC_MSG_RESULT([yes])],
                 [AC_MSG_RESULT([no])
                  btl_usnic_happy=no])
          ])

    # Are we building embedded or external libfabric?  (this is really
    # just for output / user info purposes)
    AS_IF([test "$btl_usnic_happy" = "yes"],
          [AC_MSG_CHECKING([if building embedded or external libfabric])
           AS_IF([test $opal_common_libfabric_build_embedded -eq 1],
                 [AC_MSG_RESULT([embedded])],
                 [AC_MSG_RESULT([external])])
          ])

    AH_TEMPLATE([OPAL_BTL_USNIC_FI_EXT_USNIC_H],
                [Path by which to include fi_ext_usnic.h])

    # If we're building the embedded libfabric, see if
    # it contains usnic support.
    AS_IF([test "$btl_usnic_happy" = "yes" && \
           test $opal_common_libfabric_build_embedded -eq 1],
          [AC_MSG_CHECKING([if embedded libfabric has usnic support])
           AS_IF([test $opal_common_libfabric_usnic_happy -eq 1],
                 [AC_MSG_RESULT([yes])
                  AC_DEFINE([OPAL_BTL_USNIC_FI_EXT_USNIC_H],
                            ["fi_ext_usnic.h"])],
                 [AC_MSG_RESULT([no])
                  btl_usnic_happy=no])
          ])


    # If we're building external libfabric, see if it has fi_ext_usnic.h
    AS_IF([test "$btl_usnic_happy" = "yes" && \
           test $opal_common_libfabric_build_embedded -eq 0],
          [ompi_btl_usnic_CPPFLAGS_save=$CPPFLAGS
           CPPFLAGS="$opal_common_libfabric_CPPFLAGS $CPPFLAGS"
           AC_CHECK_HEADER([rdma/fi_ext_usnic.h],
                            [AC_DEFINE([OPAL_BTL_USNIC_FI_EXT_USNIC_H],
                                       ["rdma/fi_ext_usnic.h"])],
                            [btl_usnic_happy=no])
           CPPFLAGS=$ompi_btl_usnic_CPPFLAGS_save
          ])

    # All done
    AS_IF([test "$btl_usnic_happy" = "yes"],
          [$1],
          [AS_IF([test "$with_usnic" = "yes"],
                 [AC_MSG_WARN([--with-usnic was specified, but Cisco usNIC support cannot be built])
                  AC_MSG_ERROR([Cannot continue])],
                 [$2])
          ])

    OPAL_VAR_SCOPE_POP
])dnl
