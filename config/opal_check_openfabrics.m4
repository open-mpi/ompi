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
# Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2015 Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2006-2009 Mellanox Technologies. All rights reserved.
# Copyright (c) 2010-2012 Oracle and/or its affiliates.  All rights reserved.
# Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
# Copyright (c) 2014      Bull SAS.  All rights reserved.
# Copyright (c) 2014-2015 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

# OPAL_CHECK_OPENFABRICS(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if OPENIB support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OPAL_CHECK_OPENFABRICS],[
    OPAL_VAR_SCOPE_PUSH([$1_msg])

    # Setup the --with switches to allow users to specify where
    # verbs stuff lives.
    AC_REQUIRE([OPAL_CHECK_VERBS_DIR])

    #
    # Add padding to OpenIB header
    #
    AC_ARG_ENABLE([openib-control-hdr-padding],
        [AC_HELP_STRING([--enable-openib-control-hdr-padding],
            [Add padding bytes to the openib BTL control header (default:disabled)])])
    AC_MSG_CHECKING([if want to add padding to the openib BTL control header])
    if test "$enable_openib_control_hdr_padding" = "yes"; then
        AC_MSG_RESULT([yes])
        ompi_openib_pad_hdr=1
    elif test "$enable_openib_control_hdr_padding" = "no"; then
        AC_MSG_RESULT([no])
        ompi_openib_pad_hdr=0
    else
        #
        # Enable padding for SPARC platforms by default  because the
        # btl will segv otherwise.  Keep padding disabled for other 
        # platforms since there are some performance implications with
        #  padding on for those plaforms.
        #
        case "${host}" in
        sparc*)
            AC_MSG_RESULT([yes (enabled by default on SPARC)])
            ompi_openib_pad_hdr=1
            ;;
        *)
            AC_MSG_RESULT([no])
            ompi_openib_pad_hdr=0
            ;;
        esac
    fi
    AC_DEFINE_UNQUOTED([OPAL_OPENIB_PAD_HDR], [$ompi_openib_pad_hdr],
                       [Add padding bytes to the openib BTL control header])

    AS_IF([test "$opal_want_verbs" = "no"],
          [ompi_check_openib_happy="no"],
          [ompi_check_openib_happy="yes"])

    ompi_check_openib_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_openib_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_openib_$1_save_LIBS="$LIBS"

    AS_IF([test "$ompi_check_openib_happy" = "yes"], 
            [AC_CHECK_HEADERS(
                fcntl.h sys/poll.h,
                    [],
                    [AC_MSG_WARN([fcntl.h sys/poll.h not found.  Can not build component.])
                    ompi_check_openib_happy="no"])]) 

    AS_IF([test "$ompi_check_openib_happy" = "yes"], 
          [OPAL_CHECK_PACKAGE([$1],
                              [infiniband/verbs.h],
                              [ibverbs],
                              [ibv_open_device],
                              [],
                              [$opal_verbs_dir],
                              [$opal_verbs_libdir],
                              [ompi_check_openib_happy="yes"],
                              [ompi_check_openib_happy="no"])])

    CPPFLAGS="$CPPFLAGS $$1_CPPFLAGS"
    LDFLAGS="$LDFLAGS $$1_LDFLAGS"
    LIBS="$LIBS $$1_LIBS"

    AS_IF([test "$ompi_check_openib_happy" = "yes"],
          [AC_CACHE_CHECK(
              [number of arguments to ibv_create_cq],
              [ompi_cv_func_ibv_create_cq_args],
              [AC_LINK_IFELSE(
                 [AC_LANG_PROGRAM(
                    [[#include <infiniband/verbs.h> ]],
                    [[ibv_create_cq(NULL, 0, NULL, NULL, 0);]])],
                 [ompi_cv_func_ibv_create_cq_args=5],
                 [AC_LINK_IFELSE(
                    [AC_LANG_PROGRAM(
                       [[#include <infiniband/verbs.h> ]],
                       [[ibv_create_cq(NULL, 0, NULL);]])],
                    [ompi_cv_func_ibv_create_cq_args=3],
                    [ompi_cv_func_ibv_create_cq_args="unknown"])])])
           AS_IF([test "$ompi_cv_func_ibv_create_cq_args" = "unknown"],
                 [AC_MSG_WARN([Can not determine number of args to ibv_create_cq.])
                  AC_MSG_WARN([Not building component.])
                  ompi_check_openib_happy="no"],
                 [AC_DEFINE_UNQUOTED([OPAL_IBV_CREATE_CQ_ARGS],
                                     [$ompi_cv_func_ibv_create_cq_args],
                                     [Number of arguments to ibv_create_cq])])])

    #
    # OpenIB dynamic SL
    #
    AC_ARG_ENABLE([openib-dynamic-sl],
        [AC_HELP_STRING([--enable-openib-dynamic-sl],
                        [Enable openib BTL to query Subnet Manager for IB SL (default: enabled)])])

    # Set these up so that we can do an AC_DEFINE below
    # (unconditionally)
    $1_have_xrc=0
    $1_have_xrc_domains=0
    $1_have_opensm_devel=0

    # If we have the openib stuff available, find out what we've got
    AS_IF([test "$ompi_check_openib_happy" = "yes"],
          [AC_CHECK_DECLS([IBV_EVENT_CLIENT_REREGISTER, IBV_ACCESS_SO, IBV_ATOMIC_HCA], [], [],
                          [#include <infiniband/verbs.h>])
           AC_CHECK_FUNCS([ibv_get_device_list ibv_resize_cq])

           # struct ibv_device.transport_type was added in OFED v1.2
           AC_CHECK_MEMBERS([struct ibv_device.transport_type], [], [],
                            [#include <infiniband/verbs.h>])

           # ibv_create_xrc_rcv_qp was added in OFED 1.3
           # ibv_cmd_open_xrcd (aka XRC Domains) was added in  OFED 3.12
           if test "$enable_connectx_xrc" = "yes"; then
               $1_have_xrc=1
               AC_CHECK_FUNCS([ibv_create_xrc_rcv_qp ibv_cmd_open_xrcd],
                              [], [$1_have_xrc=0])
               AC_CHECK_DECLS([IBV_SRQT_XRC],
                              [], [$1_have_xrc=0])
                              [#include <infiniband/verbs.h>])
           fi
           if test "$enable_connectx_xrc" = "yes" \
               && test $$1_have_xrc -eq 1; then
               AC_CHECK_FUNCS([ibv_cmd_open_xrcd], [$1_have_xrc_domains=1])
           fi


           if test "no" != "$enable_openib_dynamic_sl"; then
               # We need ib_types.h file, which is installed with opensm-devel
               # package. However, ib_types.h has a bad include directive,
               # which will cause AC_CHECK_HEADER to fail.
               # So instead, we will look for another file that is also
               # installed as part of opensm-devel package and included in
               # ib_types.h, but it doesn't include any other IB-related files.
               AC_CHECK_HEADER([infiniband/complib/cl_types_osd.h],
                               [AC_CHECK_LIB([osmcomp], [cl_map_init],
                                             [$1_have_opensm_devel=1],[])],
                               [],
                               [])
               # Abort if dynamic SL support was explicitly requested but opensm-devel
               # package wasn't found. Otherwise, OMPI will be built w/o dynamic SL.
               AC_MSG_CHECKING([if can use dynamic SL support])
               AS_IF([test "$$1_have_opensm_devel" = "1"],
                     [AC_MSG_RESULT([yes])],
                     [AC_MSG_RESULT([no])
                      AS_IF([test "$enable_openib_dynamic_sl" = "yes"],
                            [AC_MSG_WARN([--enable-openib-dynamic-sl was specified but the])
                             AC_MSG_WARN([appropriate header/library files could not be found])
                             AC_MSG_WARN([Please install opensm-devel if you need dynamic SL support])
                             AC_MSG_ERROR([Cannot continue])])])
           fi


           # Check support for RDMAoE devices
           $1_have_rdmaoe=0
           AC_CHECK_DECLS([IBV_LINK_LAYER_ETHERNET],
                          [$1_have_rdmaoe=1], [],
                          [#include <infiniband/verbs.h>])

           AC_MSG_CHECKING([if RDMAoE support is enabled])
           AC_DEFINE_UNQUOTED([OPAL_HAVE_RDMAOE], [$$1_have_rdmaoe], [Enable RDMAoE support])
           if test "1" = "$$1_have_rdmaoe"; then
                AC_MSG_RESULT([yes])
           else
                AC_MSG_RESULT([no])
           fi

          ])

    # Check to see if <infiniband/driver.h> works.  It is known to
    # create problems on some platforms with some compilers (e.g.,
    # RHEL4U3 with the PGI 32 bit compiler).  Use undocumented (in AC
    # 2.63) feature of AC_CHECK_HEADERS: if you explicitly pass in
    # AC_INCLUDES_DEFAULT as the 4th arg to AC_CHECK_HEADERS, the test
    # will fail if the header is present but not compilable, *but it
    # will not print the big scary warning*.  See
    # http://lists.gnu.org/archive/html/autoconf/2008-10/msg00143.html.
    AS_IF([test "$ompi_check_openib_happy" = "yes"],
          [AC_CHECK_HEADERS([infiniband/driver.h], [], [], 
                            [AC_INCLUDES_DEFAULT])])

    AC_MSG_CHECKING([if ConnectX XRC support is enabled])
    AC_DEFINE_UNQUOTED([OPAL_HAVE_CONNECTX_XRC], [$$1_have_xrc],
        [Enable features required for ConnectX XRC support])
    if test "1" = "$$1_have_xrc"; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi

    AC_MSG_CHECKING([if ConnectIB XRC support is enabled])
    AC_DEFINE_UNQUOTED([OPAL_HAVE_CONNECTX_XRC_DOMAINS], [$$1_have_xrc_domains],
        [Enable features required for XRC domains support])
    if test "1" = "$$1_have_xrc_domains"; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi

    AC_MSG_CHECKING([if dynamic SL is enabled])
    AC_DEFINE_UNQUOTED([OPAL_ENABLE_DYNAMIC_SL], [$$1_have_opensm_devel],
        [Enable features required for dynamic SL support])
    if test "1" = "$$1_have_opensm_devel"; then
        AC_MSG_RESULT([yes])
        $1_LIBS="-losmcomp $$1_LIBS"
    else
        AC_MSG_RESULT([no])
    fi

    AS_IF([test -z "$opal_verbs_dir"],
          [openib_include_dir="/usr/include"],
          [openib_include_dir="$opal_verbs_dir/include"])
    $1_CPPFLAGS="$$1_CPPFLAGS -I$openib_include_dir/infiniband"

    CPPFLAGS="$ompi_check_openib_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_openib_$1_save_LDFLAGS"
    LIBS="$ompi_check_openib_$1_save_LIBS"

    AS_IF([test "$ompi_check_openib_happy" = "yes"],
          [$2],
          [AS_IF([test "$opal_want_verbs" = "yes"],
                 [AC_MSG_WARN([Verbs support requested (via --with-verbs) but not found.])
                  AC_MSG_WARN([If you are using libibverbs v1.0 (i.e., OFED v1.0 or v1.1), you *MUST* have both the libsysfs headers and libraries installed.  Later versions of libibverbs do not require libsysfs.])
                  AC_MSG_ERROR([Aborting.])])
           $3])

     OPAL_VAR_SCOPE_POP
])

AC_DEFUN([OPAL_CHECK_OPENFABRICS_CM_ARGS],[
    #
    # ConnectX XRC support
    #
    AC_ARG_ENABLE([openib-connectx-xrc],
        [AC_HELP_STRING([--enable-openib-connectx-xrc],
                        [Enable ConnectX XRC support in the openib BTL. If you do not have InfiniBand ConnectX adapters, you may disable the ConnectX XRC support. If you do not know which InfiniBand adapter is installed on your cluster, leave this option enabled (default: enabled)])],
                        [enable_connectx_xrc="$enableval"], [enable_connectx_xrc="yes"])
    #
    # Unconnect Datagram (UD) based connection manager
    #
    AC_ARG_ENABLE([openib-udcm],
        [AC_HELP_STRING([--enable-openib-udcm],
                        [Enable datagram connection support in openib BTL (default: enabled)])], 
                        [enable_openib_udcm="$enableval"], [enable_openib_udcm="yes"])
    # Per discussion with Ralph and Nathan, disable UDCM for now.
    # It's borked and needs some surgery to get back on its feet.
    # enable_openib_udcm=no

    #
    # Openfabrics RDMACM
    #
    AC_ARG_ENABLE([openib-rdmacm],
        [AC_HELP_STRING([--enable-openib-rdmacm],
                        [Enable Open Fabrics RDMACM support in openib BTL (default: enabled)])])
    AC_ARG_ENABLE([openib-rdmacm-ibaddr],
        [AC_HELP_STRING([--enable-openib-rdmacm-ibaddr],
                        [Enable Open Fabrics RDMACM with IB addressing support in openib BTL (default: disabled)])],
        [enable_openib_rdmacm=yes])
])dnl

AC_DEFUN([OPAL_CHECK_OPENFABRICS_CM],[
    AC_REQUIRE([OPAL_CHECK_OPENFABRICS_CM_ARGS])
    $1_have_udcm=0
    $1_have_rdmacm=0

    ompi_check_openib_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_openib_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_openib_$1_save_LIBS="$LIBS"

    # add back in all the InfiniBand flags so that these tests might work...
    CPPFLAGS="$CPPFLAGS $$1_CPPFLAGS"
    LDFLAGS="$LDFLAGS $$1_LDFLAGS"
    LIBS="$LIBS $$1_LIBS"

    AS_IF([test "$ompi_check_openib_happy" = "yes"],
          [# Do we have a recent enough RDMA CM?  Need to have the
           # rdma_get_peer_addr (inline) function (originally appeared
           # in OFED v1.3).
           if test "$enable_openib_rdmacm" != "no"; then
                 AC_CHECK_HEADERS([rdma/rdma_cma.h],
                     [AC_CHECK_LIB([rdmacm], [rdma_create_id],
                         [AC_MSG_CHECKING([for rdma_get_peer_addr])
                         $1_msg=no
                         AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include "rdma/rdma_cma.h"
                                 ]], [[void *ret = (void*) rdma_get_peer_addr((struct rdma_cm_id*)0);]])],
                             [$1_have_rdmacm=1 
                             $1_msg=yes])
                         AC_MSG_RESULT([$$1_msg])])])

                 if test "1" = "$$1_have_rdmacm"; then
                     $1_LIBS="-lrdmacm $$1_LIBS"
                 else
                     AS_IF([test "$enable_openib_rdmacm" = "yes"],
                           [AC_MSG_WARN([--enable-openib-rdmacm was specified but the])
                            AC_MSG_WARN([appropriate files could not be found])
                            AC_MSG_WARN([Please install librdmacm and librdmacm-devel or disable rdmacm support])
                            AC_MSG_ERROR([Cannot continue])])
                 fi
           fi

           # is udcm enabled
           if test "$enable_openib_udcm" = "yes"; then
               $1_have_udcm=1
           fi
           ])

    CPPFLAGS="$ompi_check_openib_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_openib_$1_save_LDFLAGS"
    LIBS="$ompi_check_openib_$1_save_LIBS"

    AC_MSG_CHECKING([if UD CM is enabled])
    AC_DEFINE_UNQUOTED([OPAL_HAVE_UDCM], [$$1_have_udcm],
        [Whether UD CM is available or not])
    if test "1" = "$$1_have_udcm"; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi

    AC_MSG_CHECKING([if OpenFabrics RDMACM support is enabled])
    AC_DEFINE_UNQUOTED([OPAL_HAVE_RDMACM], [$$1_have_rdmacm],
        [Whether RDMA CM is available or not])
    if test "1" = "$$1_have_rdmacm"; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi
])dnl

AC_DEFUN([OPAL_CHECK_MLNX_OPENFABRICS],[
     $1_have_mverbs=0
     $1_have_mqe=0

    AS_IF([test "$ompi_check_openib_happy" = "yes"], 
           [OPAL_CHECK_PACKAGE([$1],
                               [infiniband/mverbs.h],
                               [mverbs],
                               [ibv_m_query_device],
                               ["$$1_LIBS"],
                               [$opal_verbs_dir],
                               [$opal_verbs_libdir],
                               [$1_have_mverbs=1],
                               [])])

    AS_IF([test "$ompi_check_openib_happy" = "yes"], 
           [OPAL_CHECK_PACKAGE([$1],
                               [infiniband/mqe.h],
                               [mqe],
                               [mqe_context_create],
                               ["$$1_LIBS"],
                               [$opal_verbs_dir],
                               [$opal_verbs_libdir],
                               [$1_have_mqe=1],
                               [])])

    AC_MSG_CHECKING([if Mellanox OpenFabrics VERBS is enabled])
    AC_DEFINE_UNQUOTED([OPAL_HAVE_MVERBS], [$$1_have_mverbs],
        [Whether MVERBS is available or not])
    AS_IF([test "1" = "$$1_have_mverbs"],
          [AC_MSG_RESULT([yes])],
          [AC_MSG_RESULT([no])])

    # save the CPPFLAGS since we would have to update it for next test
    ompi_check_mellanox_openfabrics_$1_save_CPPFLAGS="$CPPFLAGS"

    # If openfabrics custom directory have been defined, we have
    # to use it for MACRO test that uses mverbs.h file.
    #
    if test ! -z "$ompi_check_verbs_dir" ; then
        CPPFLAGS="-I${opal_verbs_dir}/include $CPPFLAGS"
    fi

    AS_IF([test "1" = "$$1_have_mverbs"],
          [AC_CHECK_DECLS([IBV_M_WR_CALC_RDMA_WRITE_WITH_IMM], 
                          [AC_DEFINE_UNQUOTED([OPAL_HAVE_IBOFFLOAD_CALC_RDMA], [1],
                                              [Whether IBV_M_WR_CALC_SEND is defined or not])], 
                          [AC_DEFINE_UNQUOTED([OPAL_HAVE_IBOFFLOAD_CALC_RDMA], [0],
                                              [Whether IBV_M_WR_CALC_SEND is defined or not])], 
                          [#include <infiniband/mverbs.h>])])

    # restoring the CPPFLAGS
    CPPFLAGS="$ompi_check_mellanox_openfabrics_$1_save_CPPFLAGS"

    AC_MSG_CHECKING([if Mellanox OpenFabrics MQE is enabled])
    AC_DEFINE_UNQUOTED([OPAL_HAVE_MQE], [$$1_have_mqe],
        [Whether MQE is available or not])
    AS_IF([test "1" = "$$1_have_mqe"],
          [AC_MSG_RESULT([yes])],
          [AC_MSG_RESULT([no])])

    AS_IF([test "1" = "$$1_have_mverbs" && test "1" = $$1_have_mqe],
            [$2], [$3])
])dnl
