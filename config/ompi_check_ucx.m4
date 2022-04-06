# -*- shell-script -*-
#
# Copyright (C) 2015-2017 Mellanox Technologies, Inc.
#                         All rights reserved.
# Copyright (c) 2015      Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# Copyright (c) 2016      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2016 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_CHECK_UCX(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if UCX support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_UCX],[
    OPAL_VAR_SCOPE_PUSH([ompi_check_ucx_dir])

    AS_IF([test -z "$ompi_check_ucx_happy"],
          [AC_ARG_WITH([ucx],
		       [AS_HELP_STRING([--with-ucx(=DIR)],
				       [Build with Unified Communication X library support])])
	   OPAL_CHECK_WITHDIR([ucx], [$with_ucx], [include/ucp/api/ucp.h])
	   AC_ARG_WITH([ucx-libdir],
		       [AS_HELP_STRING([--with-ucx-libdir=DIR],
				       [Search for Unified Communication X libraries in DIR])])
	   OPAL_CHECK_WITHDIR([ucx-libdir], [$with_ucx_libdir], [libucp.*])

	   AS_IF([test "$with_ucx" != "no"],
                 [AS_IF([test -n "$with_ucx" && test "$with_ucx" != "yes"],
                        [ompi_check_ucx_dir="$with_ucx"],
                        [PKG_CHECK_MODULES_STATIC([ucx],[ucx],
                                             [ompi_check_ucx_dir=`$PKG_CONFIG --variable=prefix ucx`
                                              AS_IF([test "$ompi_check_ucx_dir" = "/usr"],
                                                    [ompi_check_ucx_dir=])],
                                             [true])])
                  ompi_check_ucx_happy="no"
                  AS_IF([test -z "$ompi_check_ucx_dir"],
                        [OPAL_CHECK_PACKAGE([ompi_check_ucx],
                                   [ucp/api/ucp.h],
                                   [ucp],
                                   [ucp_cleanup],
                                   [-luct -lucm -lucs],
                                   [],
                                   [],
                                   [ompi_check_ucx_happy="yes"],
                                   [ompi_check_ucx_happy="no"])
                         AS_IF([test "$ompi_check_ucx_happy" = yes],
                               [AC_MSG_CHECKING(for UCX version compatibility)
                                AC_REQUIRE_CPP
                                AC_COMPILE_IFELSE(
                                    [AC_LANG_PROGRAM([[#include <uct/api/version.h>]],[[]])],
                                    [ompi_check_ucx_happy="yes"],
                                    [ompi_check_ucx_happy="no"])

                                AC_MSG_RESULT([$ompi_check_ucx_happy])])
                         AS_IF([test "$ompi_check_ucx_happy" = "no"],
                               [ompi_check_ucx_dir=/opt/ucx])])
                  AS_IF([test "$ompi_check_ucx_happy" != yes],
                        [AS_IF([test -n "$with_ucx_libdir"],
                               [ompi_check_ucx_libdir="$with_ucx_libdir"],
                               [files=`ls $ompi_check_ucx_dir/lib64/libucp.* 2> /dev/null | wc -l`
                                AS_IF([test "$files" -gt 0],
                                      [ompi_check_ucx_libdir=$ompi_check_ucx_dir/lib64],
                                      [ompi_check_ucx_libdir=$ompi_check_ucx_dir/lib])])

                         ompi_check_ucx_$1_save_CPPFLAGS="$CPPFLAGS"
                         ompi_check_ucx_$1_save_LDFLAGS="$LDFLAGS"
                         ompi_check_ucx_$1_save_LIBS="$LIBS"

                         OPAL_CHECK_PACKAGE([ompi_check_ucx],
                                            [ucp/api/ucp.h],
                                            [ucp],
                                            [ucp_cleanup],
                                            [-luct -lucm -lucs],
                                            [$ompi_check_ucx_dir],
                                            [$ompi_check_ucx_libdir],
                                            [ompi_check_ucx_happy="yes"],
                                            [ompi_check_ucx_happy="no"])

                         CPPFLAGS="$ompi_check_ucx_$1_save_CPPFLAGS"
                         LDFLAGS="$ompi_check_ucx_$1_save_LDFLAGS"
                         LIBS="$ompi_check_ucx_$1_save_LIBS"

                         AS_IF([test "$ompi_check_ucx_happy" = yes],
                               [AC_MSG_CHECKING(for UCX version compatibility)
                                AC_REQUIRE_CPP
                                old_CPPFLAGS="$CPPFLAGS"
                                CPPFLAGS="$CPPFLAGS -I$ompi_check_ucx_dir/include"
                                AC_COMPILE_IFELSE(
                                    [AC_LANG_PROGRAM([[#include <uct/api/version.h>]],[[]])],
                                    [ompi_check_ucx_happy="yes"],
                                    [ompi_check_ucx_happy="no"])

                                AC_MSG_RESULT([$ompi_check_ucx_happy])
                                CPPFLAGS=$old_CPPFLAGS])])

                  old_CPPFLAGS="$CPPFLAGS"
                  AS_IF([test -n "$ompi_check_ucx_dir"],
                        [CPPFLAGS="$CPPFLAGS -I$ompi_check_ucx_dir/include"])
                  # Turn off UCX version v1.8 due to issue #8321
                  AC_MSG_CHECKING([UCX version])
                  AC_PREPROC_IFELSE([AC_LANG_PROGRAM([#include <ucp/api/ucp_version.h>
                                                      #if (UCP_API_MAJOR == 1) && (UCP_API_MINOR == 8)
                                                      #error "Invalid version"
                                                      #endif], [])],
                                    [AC_MSG_RESULT([ok (not 1.8.x)])],
                                    [AC_MSG_RESULT([bad (1.8.x)])
                                     AC_MSG_WARN([UCX support skipped because version 1.8.x was found, which has a known catastrophic issue.])
                                     AC_MSG_WARN([Please upgrade to UCX version 1.9 or higher.])
                                     ompi_check_ucx_happy=no])
                  AS_IF([test "$ompi_check_ucx_happy" = yes],
                        [
                         AC_CHECK_DECLS([ucp_tag_send_nbr],
                                        [AC_DEFINE([HAVE_UCP_TAG_SEND_NBR],[1],
                                                   [have ucp_tag_send_nbr()])], [],
                                        [#include <ucp/api/ucp.h>])
                         AC_CHECK_DECLS([ucp_ep_flush_nb, ucp_worker_flush_nb,
                                         ucp_request_check_status, ucp_put_nb, ucp_get_nb,
                                         ucp_put_nbx, ucp_get_nbx, ucp_atomic_op_nbx,
                                         ucp_ep_flush_nbx],
                                        [], [],
                                        [#include <ucp/api/ucp.h>])
                         AC_CHECK_DECLS([ucm_test_events,
                                         ucm_test_external_events],
                                        [], [],
                                        [#include <ucm/api/ucm.h>])
                         AC_CHECK_DECLS([UCP_ATOMIC_POST_OP_AND,
                                         UCP_ATOMIC_POST_OP_OR,
                                         UCP_ATOMIC_POST_OP_XOR,
                                         UCP_ATOMIC_FETCH_OP_FAND,
                                         UCP_ATOMIC_FETCH_OP_FOR,
                                         UCP_ATOMIC_FETCH_OP_FXOR,
                                         UCP_PARAM_FIELD_ESTIMATED_NUM_PPN,
                                         UCP_WORKER_FLAG_IGNORE_REQUEST_LEAK,
                                         UCP_OP_ATTR_FLAG_MULTI_SEND],
                                        [], [],
                                        [#include <ucp/api/ucp.h>])
                         AC_CHECK_DECLS([UCP_WORKER_ATTR_FIELD_ADDRESS_FLAGS],
                                        [AC_DEFINE([HAVE_UCP_WORKER_ADDRESS_FLAGS], [1],
                                                   [have worker address attribute])], [],
                                        [#include <ucp/api/ucp.h>])
                         AC_CHECK_DECLS([UCP_ATTR_FIELD_MEMORY_TYPES],
                                        [AC_DEFINE([HAVE_UCP_ATTR_MEMORY_TYPES], [1],
                                                   [have memory types attribute])], [],
                                        [#include <ucp/api/ucp.h>])
                         AC_CHECK_DECLS([ucp_tag_send_nbx,
                                         ucp_tag_send_sync_nbx,
                                         ucp_tag_recv_nbx],
                                        [], [],
                                        [#include <ucp/api/ucp.h>])
                         AC_CHECK_TYPES([ucp_request_param_t],
                                        [], [],
                                        [[#include <ucp/api/ucp.h>]])
                        ])
                  CPPFLAGS=$old_CPPFLAGS

                  OPAL_SUMMARY_ADD([Transports], [Open UCX], [], [$ompi_check_ucx_happy])])])

    AS_IF([test "$ompi_check_ucx_happy" = "yes"],
          [$1_CPPFLAGS="[$]$1_CPPFLAGS $ompi_check_ucx_CPPFLAGS"
           $1_LDFLAGS="[$]$1_LDFLAGS $ompi_check_ucx_LDFLAGS"
           $1_LIBS="[$]$1_LIBS $ompi_check_ucx_LIBS"
           $2],
          [AS_IF([test ! -z "$with_ucx" && test "$with_ucx" != "no"],
                 [AC_MSG_ERROR([UCX support requested but not found.  Aborting])])
           $3])

    OPAL_VAR_SCOPE_POP
])

