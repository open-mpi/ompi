# -*- makefile -*-
#
# Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2012-2013 Inria.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015-2019 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2016      IBM Corporation.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules


if OMPI_FORTRAN_HAVE_TS

AM_CPPFLAGS = -DOMPI_COMPILING_FORTRAN_WRAPPERS=1

noinst_LTLIBRARIES = libusempif08_ts.la

endif

libusempif08_ts_la_SOURCES = \
        bindings.h \
        ts.h \
        ts.c \
        \
        accumulate_ts.c \
        allgather_ts.c \
        allgatherv_ts.c \
        allreduce_ts.c \
        alltoall_ts.c \
        alltoallv_ts.c \
        alltoallw_ts.c \
        bcast_ts.c \
        bsend_ts.c \
        bsend_init_ts.c \
        buffer_attach_ts.c \
        compare_and_swap_ts.c \
        exscan_ts.c \
        f_sync_reg_ts.c \
        fetch_and_op_ts.c \
        free_mem_ts.c \
        gather_ts.c \
        gatherv_ts.c \
        get_address_ts.c \
        get_accumulate_ts.c \
        get_ts.c \
        iallgather_ts.c \
        iallgatherv_ts.c \
        iallreduce_ts.c \
        ialltoall_ts.c \
        ialltoallv_ts.c \
        ialltoallw_ts.c \
        ibcast_ts.c \
        ibsend_ts.c \
        iexscan_ts.c \
        igather_ts.c \
        igatherv_ts.c \
        imrecv_ts.c \
        ineighbor_allgather_ts.c \
        ineighbor_allgatherv_ts.c \
        ineighbor_alltoall_ts.c \
        ineighbor_alltoallv_ts.c \
        ineighbor_alltoallw_ts.c \
        irecv_ts.c \
        ireduce_ts.c \
        ireduce_scatter_block_ts.c \
        ireduce_scatter_ts.c \
        irsend_ts.c \
        iscan_ts.c \
        iscatter_ts.c \
        iscatterv_ts.c \
        isend_ts.c \
        issend_ts.c \
        mrecv_ts.c \
        neighbor_allgather_ts.c \
        neighbor_allgatherv_ts.c \
        neighbor_alltoall_ts.c \
        neighbor_alltoallv_ts.c \
        neighbor_alltoallw_ts.c \
        pack_ts.c \
        pack_external_ts.c \
        put_ts.c \
        raccumulate_ts.c \
        recv_ts.c \
        recv_init_ts.c \
        reduce_ts.c \
        reduce_local_ts.c \
        reduce_scatter_block_ts.c \
        reduce_scatter_ts.c \
        rget_accumulate_ts.c \
        rget_ts.c \
        rput_ts.c \
        rsend_ts.c \
        rsend_init_ts.c \
        scan_ts.c \
        scatter_ts.c \
        scatterv_ts.c \
        send_ts.c \
        send_init_ts.c \
        sendrecv_ts.c \
        sendrecv_replace_ts.c \
        ssend_ts.c \
        ssend_init_ts.c \
        unpack_ts.c \
        unpack_external_ts.c \
        win_attach_ts.c \
        win_create_ts.c \
        win_detach_ts.c
