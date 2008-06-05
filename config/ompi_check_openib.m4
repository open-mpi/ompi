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
# Copyright (c) 2006-2008 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2006-2007 Los Alamos National Security, LLC.  All rights
#                         reserved.
# Copyright (c) 2006-2008 Mellanox Technologies. All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI_CHECK_OPENIB(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if OPENIB support can be found.  sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_OPENIB],[
    OMPI_VAR_SCOPE_PUSH([$1_msg])

    AC_ARG_WITH([openib],
        [AC_HELP_STRING([--with-openib(=DIR)],
             [Build OpenFabrics support, searching for libraries in DIR])])
    AC_ARG_WITH([openib-libdir],
       [AC_HELP_STRING([--with-openib-libdir=DIR],
             [Search for OpenFabrics libraries in DIR])])

    AS_IF([test ! -z "$with_openib" -a "$with_openib" != "yes"],
          [ompi_check_openib_dir="$with_openib"])
    AS_IF([test ! -z "$with_openib_libdir" -a "$with_openib_libdir" != "yes"],
          [ompi_check_openib_libdir="$with_openib_libdir"])
    AS_IF([test "$with_openib" = "no"],
          [ompi_check_openib_happy="no"],
          [ompi_check_openib_happy="yes"])

    ompi_check_openib_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_openib_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_openib_$1_save_LIBS="$LIBS"

    AS_IF([test "$ompi_check_openib_happy" = "yes"],
          [AS_IF([test "$THREAD_TYPE" != "posix" -a "$memory_ptmalloc2_happy" = "yes"],
                 [AC_MSG_WARN([POSIX Threads disabled but PTMalloc2 enabled.])
                  AC_MSG_WARN([This will cause memory corruption with OpenFabrics.])
                  AC_MSG_WARN([Not building component.])
                  ompi_check_openib_happy="no"])])

    AS_IF([test "$ompi_check_openib_happy" = "yes"], 
            [AC_CHECK_HEADERS(
                fcntl.h sys/poll.h,
                    [],
                    [AC_MSG_WARN([fcntl.h sys/poll.h not found.  Can not build component.])
                    ompi_check_openib_happy="no"])]) 

    AS_IF([test "$ompi_check_openib_happy" = "yes"], 
          [OMPI_CHECK_PACKAGE([$1],
                              [infiniband/verbs.h],
                              [ibverbs],
                              [ibv_open_device],
                              [],
                              [$ompi_check_openib_dir],
                              [$ompi_check_openib_libdir],
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
                 [AC_DEFINE_UNQUOTED([OMPI_IBV_CREATE_CQ_ARGS],
                                     [$ompi_cv_func_ibv_create_cq_args],
                                     [Number of arguments to ibv_create_cq])])])

    # Set these up so that we can do an AC_DEFINE below
    # (unconditionally)
    $1_have_rdmacm=0
    $1_have_ibcm=0

    # If we have the openib stuff available, find out what we've got
    AS_IF([test "$ompi_check_openib_happy" = "yes"],
          [AC_CHECK_DECLS([IBV_EVENT_CLIENT_REREGISTER], [], [], 
                          [#include <infiniband/verbs.h>])
           AC_CHECK_FUNCS([ibv_get_device_list ibv_resize_cq])

           # struct ibv_device.transport_type was added in OFED v1.2
           AC_CHECK_MEMBERS([struct ibv_device.transport_type], [], [],
                            [#include <infiniband/verbs.h>])

           # ibv_create_xrc_rcv_qp was added in OFED 1.3
           AC_CHECK_FUNCS([ibv_create_xrc_rcv_qp], [$1_have_xrc=1])

           # Do we have a recent enough RDMA CM?  Need to have the
           # rdma_get_peer_addr (inline) function (originally appeared
           # in OFED v1.3).
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
           fi

           # Do we have IB CM? (note that OFED IB CM depends on RDMA
           # CM, so no need to add it into the other-libraries
           # argument to AC_CHECK_ LIB).  Note that we only want IBCM
           # starting with OFED 1.2 or so, so check for
           # ib_cm_open_device (introduced in libibcm 1.0/OFED 1.2).
           AC_CHECK_HEADERS([infiniband/cm.h],
               [AC_CHECK_LIB([ibcm], [ib_cm_open_device],
                   [$1_have_ibcm=1
                    $1_LIBS="-libcm $$1_LIBS"])])
          ])

    AC_DEFINE_UNQUOTED([OMPI_HAVE_RDMACM], [$$1_have_rdmacm],
        [Whether RDMA CM is available or not])
    AC_DEFINE_UNQUOTED([OMPI_HAVE_IBCM], [$$1_have_ibcm],
        [Whether IB CM is available or not])

    CPPFLAGS="$ompi_check_openib_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_openib_$1_save_LDFLAGS"
    LIBS="$ompi_check_openib_$1_save_LIBS"

    AS_IF([test "$ompi_check_openib_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_openib" -a "$with_openib" != "no"],
                 [AC_MSG_WARN([OpenFabrics support requested (via --with-openib) but not found.])
                  AC_MSG_WARN([If you are using libibverbs v1.0 (i.e., OFED v1.0 or v1.1), you *MUST* have both the libsysfs headers and libraries installed.  Later versions of libibverbs do not require libsysfs.])
                  AC_MSG_ERROR([Aborting.])])
           $3])

     OMPI_VAR_SCOPE_POP
])

