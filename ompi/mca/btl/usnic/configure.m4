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
# Copyright (c) 2010-2014 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OMPI_CHECK_LIBNL3(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if libnl3 support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
#
# libnl3 changed its default header location as of v3.2 (released ca. September
# 2011).  It was previously "${prefix}/include/netlink/...".  It now is
# "${prefix}/libnl3/include/netlink/...".  The logic below only supports
# >=v3.2, under the assumption that it is not widely deployed.
AC_DEFUN([OMPI_CHECK_LIBNL3],[
    AC_ARG_WITH([libnl3],
        [AC_HELP_STRING([--with-libnl3(=DIR)],
             [Build libnl3 support])])
    OMPI_CHECK_WITHDIR([libnl3], [$with_libnl3], [include/libnl3/netlink/netlink.h])
    AC_ARG_WITH([libnl3-libdir],
        [AC_HELP_STRING([--with-libnl3-libdir=DIR],
             [Search for libnl3 libraries in DIR])])
    OMPI_CHECK_WITHDIR([libnl3-libdir], [$with_libnl3_libdir], [libnl-3.*])

    ompi_check_libnl3_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_libnl3_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_libnl3_$1_save_LIBS="$LIBS"

    ompi_check_libnl3_$1_orig_CPPFLAGS="$$1_CPPFLAGS"
    ompi_check_libnl3_$1_orig_LDFLAGS="$$1_LDFLAGS"
    ompi_check_libnl3_$1_orig_LIBS="$$1_LIBS"

    AS_IF([test "$with_libnl3" != "no"],
          [AS_IF([test ! -z "$with_libnl3" -a "$with_libnl3" != "yes"],
                 [ompi_check_libnl3_dir="$with_libnl3"])
           AS_IF([test ! -z "$with_libnl3_libdir" -a "$with_libnl3_libdir" != "yes"],
                 [ompi_check_libnl3_libdir="$with_libnl3_libdir"])

           # OMPI_CHECK_PACKAGE unfortunately can't handle this weird include
           # dir layout
           AS_IF([test -n "$ompi_check_libnl3_dir"],
                 [ompi_check_libnl3_includedir="$ompi_check_libnl3_dir/include/libnl3"],
                 [ompi_check_libnl3_includedir="/usr/include/libnl3"])
           $1_CPPFLAGS="$$1_CPPFLAGS -I$ompi_check_libnl3_includedir"
           CPPFLAGS="$CPPFLAGS -I$ompi_check_libnl3_includedir"

           AC_CHECK_HEADER([netlink/netlink.h],
                           [# nl_recvmsgs_report appears to be a symbol which
                            # is present in libnl-3 but not libnl (v1)
                            _OMPI_CHECK_PACKAGE_LIB([$1],
                                                    [nl-3],
                                                    [nl_recvmsgs_report],
                                                    [],
                                                    [$ompi_check_libnl3_dir],
                                                    [$ompi_check_libnl3_libdir],
                                                    [ompi_check_libnl3_happy="yes"],
                                                    [ompi_check_libnl3_happy="no"])],
                           [ompi_check_libnl3_happy=no])

           # make sure that we don't pollute the cache with the results of a
           # test performed under different CPPFLAGS
           AS_UNSET([ac_cv_header_netlink_netlink_h])],
          [ompi_check_libnl3_happy="no"])

    # restore global flags
    CPPFLAGS="$ompi_check_libnl3_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_libnl3_$1_save_LDFLAGS"
    LIBS="$ompi_check_libnl3_$1_save_LIBS"

    AS_IF([test "$ompi_check_libnl3_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_libnl3" -a "$with_libnl3" != "no"],
                 [AC_MSG_ERROR([libnl3 support requested but not found.  Aborting])])
           # restore prefixed flags on failure
           $1_CPPFLAGS="$ompi_check_package_$1_orig_CPPFLAGS"
           $1_LDFLAGS="$ompi_check_package_$1_orig_LDFLAGS"
           $1_LIBS="$ompi_check_package_$1_orig_LIBS"
           $3])
])

# MCA_ompi_btl_usnic_POST_CONFIG([should_build])
# ------------------------------------------
AC_DEFUN([MCA_ompi_btl_usnic_POST_CONFIG], [
    AM_CONDITIONAL([OMPI_BTL_USNIC_BUILD_UNIT_TESTS],
                   [test "$1" -eq 1 && test "X$enable_ompi_btl_usnic_unit_tests" = "Xyes"])
    AM_CONDITIONAL([OMPI_BTL_USNIC_BUILD_LIBNL1_UTILS],
                   [test "$1" -eq 1 && test "X$enable_ompi_btl_usnic_libnl1_utils" = "Xyes"])
    AM_CONDITIONAL([OMPI_BTL_USNIC_BUILD_LIBNL3_UTILS],
                   [test "$1" -eq 1 && test "X$enable_ompi_btl_usnic_libnl3_utils" = "Xyes"])
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
          [OMPI_BTL_USNIC_DO_CONFIG($1, $2)])
])

AC_DEFUN([OMPI_BTL_USNIC_DO_CONFIG],[
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

    # for if_nametoindex(3)
    AC_CHECK_HEADERS([net/if.h])

    OMPI_CHECK_OPENFABRICS([btl_usnic],
                        [btl_usnic_happy="yes"],
                        [btl_usnic_happy="no"])

    # We only want to build on 64 bit Linux.
    AS_IF([test "$btl_usnic_happy" = "yes"],
          [AC_CHECK_SIZEOF([void *])
           AC_MSG_CHECKING([for 64 bit Linux])
           case $host_os in
               *linux*)
                   AS_IF([test $ac_cv_sizeof_void_p -eq 8],
                         [btl_usnic_happy=yes],
                         [btl_usnic_happy=no])
                   ;;
               *)
                   btl_usnic_happy=no
                   ;;
           esac
           AC_MSG_RESULT([$btl_usnic_happy])
          ]
    )

    AS_IF([test "$btl_usnic_happy" = "yes"],
          [AC_CHECK_DECLS([IBV_EVENT_GID_CHANGE, ibv_event_type_str], [], [],
                          [#include <infiniband/verbs.h>
])
          ]
    )

    # Search for libnl so we can query routing information.  We need to
    # distinguish between v1 and v3.
    enable_ompi_btl_usnic_libnl1_utils=no
    enable_ompi_btl_usnic_libnl3_utils=no
    AS_IF([test "$btl_usnic_happy" = "yes"],
          [OMPI_CHECK_LIBNL3([btl_usnic_libnl],
                             [enable_ompi_btl_usnic_libnl3_utils=yes],
                             [enable_ompi_btl_usnic_libnl3_utils=no])

           # fall back to libnl1 if libnl3 could not be found
           AS_IF([test "X$enable_ompi_btl_usnic_libnl3_utils" = "Xno"],
                 [OMPI_CHECK_PACKAGE([btl_usnic_libnl],
                                     [netlink/netlink.h],
                                     [nl],
                                     [nl_recvmsgs_default],
                                     [],
                                     [],
                                     [],
                                     [enable_ompi_btl_usnic_libnl1_utils=yes],
                                     [enable_ompi_btl_usnic_libnl1_utils=no])])

           AS_IF([test "X$enable_ompi_btl_usnic_libnl3_utils" = "Xno" &&
                  test "X$enable_ompi_btl_usnic_libnl1_utils" = "Xno"],
                 [AC_MSG_NOTICE([could not find a libnl or libnl-3, disabling the usnic BTL])
                  btl_usnic_happy="no"])

           btl_usnic_CPPFLAGS="$btl_usnic_CPPFLAGS $btl_usnic_libnl_CPPFLAGS"
           btl_usnic_CFLAGS="$btl_usnic_CFLAGS $btl_usnic_libnl_CFLAGS"
           btl_usnic_LDFLAGS="$btl_usnic_LDFLAGS $btl_usnic_libnl_LDFLAGS"
           btl_usnic_LIBS="$btl_usnic_libnl_LIBS $btl_usnic_LIBS"
           ])

    AS_IF([test "$btl_usnic_happy" = "yes"],
          [btl_usnic_WRAPPER_EXTRA_LDFLAGS="$btl_usnic_LDFLAGS"
           btl_usnic_WRAPPER_EXTRA_LIBS="$btl_usnic_LIBS"
           $1],
          [AS_IF([test "$with_usnic" = "yes"],
                 [AC_MSG_WARN([--with-usnic specified, but usNIC support cannot be built])
                  AC_MSG_ERROR([Cannot continue])],
                 [$2])
          ])

    # Substitute in the things needed to build USNIC
    AC_SUBST([btl_usnic_CPPFLAGS])
    AC_SUBST([btl_usnic_CFLAGS])
    AC_SUBST([btl_usnic_LDFLAGS])
    AC_SUBST([btl_usnic_LIBS])
])dnl
