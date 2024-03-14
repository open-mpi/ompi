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
    OPAL_VAR_SCOPE_PUSH([ompi_check_ucx_happy ompi_check_ucx_CPPFLAGS_save ompi_check_ucx_LDFLAGS_save ompi_check_ucx_LIBS_save])

    m4_ifblank([$1], [m4_fatal([First argument to OMPI_CHECK_UCX cannot be blank])])

    AC_ARG_WITH([ucx],
                [AS_HELP_STRING([--with-ucx(=DIR)],
                                [Build with Unified Communication X library support])])
    AC_ARG_WITH([ucx-libdir],
                [AS_HELP_STRING([--with-ucx-libdir=DIR],
                                [Search for Unified Communication X libraries in DIR])])

    OAC_CHECK_PACKAGE([ucx],
                      [$1],
                      [ucp/api/ucp.h],
                      [ucp -luct -lucm -lucs],
                      [ucp_cleanup],
                      [ompi_check_ucx_happy="yes"],
                      [ompi_check_ucx_happy="no"])

    ompi_check_ucx_CPPFLAGS_save=${CPPFLAGS}
    ompi_check_ucx_LDFLAGS_save=${LDFLAGS}
    ompi_check_ucx_LIBS_save=${LIBS}

    OPAL_FLAGS_APPEND_UNIQ([CPPFLAGS], [${$1_CPPFLAGS}])
    OPAL_FLAGS_APPEND_UNIQ([LDFLAGS], [${$1_LDFLAGS}])
    OPAL_FLAGS_APPEND_MOVE([LIBS], [${$1_LIBS}])

    AS_IF([test "$ompi_check_ucx_happy" = yes],
          [AC_CACHE_CHECK([for UCX version header],
               [ompi_check_ucx_cv_have_version_header],
               [AC_REQUIRE_CPP
                AC_COMPILE_IFELSE(
                   [AC_LANG_PROGRAM([[#include <uct/api/version.h>]],[[]])],
                   [ompi_check_ucx_cv_have_version_header="yes"],
                   [ompi_check_ucx_cv_have_version_header="no"])])
           AS_IF([test "${ompi_check_ucx_cv_have_version_header}" != "yes"],
                 [ompi_check_ucx_happy=no])])

    AS_IF([test "$ompi_check_ucx_happy" = yes],
          [# Turn off UCX version v1.8 due to issue #8321
           AC_CACHE_CHECK([UCX version 1.8.x],
               [ompi_check_ucx_cv_have_version_1_8],
               [AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#include <ucp/api/ucp_version.h>
                                 ]], [[
#if (UCP_API_MAJOR == 1) && (UCP_API_MINOR == 8)
#error "Invalid version"
#endif
                                 ]])],
                             [ompi_check_ucx_cv_have_version_1_8=no],
                             [ompi_check_ucx_cv_have_version_1_8=yes])])
           AS_IF([test "${ompi_check_ucx_cv_have_version_1_8}" = "yes"],
                 [AC_MSG_WARN([UCX support skipped because version 1.8.x was found, which has a known catastrophic issue.])
                  ompi_check_ucx_happy=no])])
           AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
#include <ucp/api/ucp_version.h>
                             ]], [[
#if (UCP_API_MAJOR < 1) || ((UCP_API_MAJOR == 1) && (UCP_API_MINOR < 9))
#error "Version too low"
#endif
                             ]])],
                             [], [AC_MSG_WARN([UCX version is too old, please upgrade to 1.9 or higher.])])

    AS_IF([test "$ompi_check_ucx_happy" = yes],
          [AC_CHECK_DECLS([ucp_tag_send_nbr],
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
                           UCP_OP_ATTR_FLAG_MULTI_SEND,
                           UCS_MEMORY_TYPE_RDMA,
                           UCP_MEM_MAP_SYMMETRIC_RKEY],
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
           AC_CHECK_DECLS([UCP_EP_ATTR_FIELD_TRANSPORTS],
                          [], [],
                          [#include <ucp/api/ucp.h>])
           AC_CHECK_DECLS([ucp_tag_send_nbx,
                           ucp_tag_send_sync_nbx,
                           ucp_tag_recv_nbx,
                           ucp_rkey_compare],
                          [], [],
                          [#include <ucp/api/ucp.h>])
           AC_CHECK_TYPES([ucp_request_param_t],
                          [], [],
                          [[#include <ucp/api/ucp.h>]])
           ])

    CPPFLAGS=${ompi_check_ucx_CPPFLAGS_save}
    LDFLAGS=${ompi_check_ucx_LDFLAGS_save}
    LIBS=${ompi_check_ucx_LIBS_save}

    OPAL_SUMMARY_ADD([Transports], [Open UCX], [], [$ompi_check_ucx_happy])

    AS_IF([test "$ompi_check_ucx_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_ucx" && test "$with_ucx" != "no"],
                 [AC_MSG_ERROR([UCX support requested but not found.  Aborting])])
           $3])

    OPAL_VAR_SCOPE_POP
])

