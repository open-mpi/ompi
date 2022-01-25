#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2013 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009-2021 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2011      Sandia National Laboratories. All rights reserved.
# Copyright (c) 2012      Oak Rigde National Laboratory. All rights reserved.
# Copyright (c) 2012-2013 Inria.  All rights reserved.
# Copyright (c) 2013-2018 Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015-2020 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
#                         reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# The purpose of the profiling layer is to allow intercept libraries
# which override the MPI_ namespace symbols.  We potentially compile
# every MPI function twice.  We always build the profiling layer,
# because the symbols that are always implemented as functions are the
# PMPI_ namespace symbols.  We sometimes also build the non-profiling
# layer, if weak symbols can't be used to alias the MPI_ namespace
# into the PMPI_ namespace.

noinst_LTLIBRARIES = libmpi_c.la libmpi_c_profile.la
if BUILD_MPI_BINDINGS_LAYER
noinst_LTLIBRARIES += libmpi_c_noprofile.la
endif

headers = bindings.h

# attr_fn.c contains attribute manipulation functions which do not
# profiling implications, and so are always built.
libmpi_c_la_SOURCES = \
        attr_fn.c
libmpi_c_la_LIBADD = libmpi_c_profile.la
if BUILD_MPI_BINDINGS_LAYER
libmpi_c_la_LIBADD += libmpi_c_noprofile.la
endif

# Conditionally install the header files
if WANT_INSTALL_HEADERS
ompidir = $(ompiincludedir)/$(subdir)
ompi_HEADERS = $(headers)
endif

#
# List of all C files that have profile versions
#
interface_profile_sources = \
        abort.c \
        add_error_class.c \
        add_error_code.c \
        add_error_string.c \
        allgather.c \
        iallgather.c \
        allgather_init.c \
        allgatherv.c \
        iallgatherv.c \
        allgatherv_init.c \
        alloc_mem.c \
        allreduce.c \
        iallreduce.c \
        allreduce_init.c \
        alltoall.c \
        ialltoall.c \
        alltoall_init.c \
        alltoallv.c \
        ialltoallv.c \
        alltoallv_init.c \
        alltoallw.c \
        ialltoallw.c \
        alltoallw_init.c \
        attr_delete.c \
        attr_get.c \
        attr_put.c \
        barrier.c \
        ibarrier.c \
        barrier_init.c \
        bcast.c \
        ibcast.c \
        bcast_init.c \
        bsend.c \
        bsend_init.c \
        buffer_attach.c \
        buffer_detach.c \
        cancel.c \
        cart_coords.c \
        cart_create.c \
        cartdim_get.c \
        cart_get.c \
        cart_map.c \
        cart_rank.c \
        cart_shift.c \
        cart_sub.c \
        close_port.c \
        comm_accept.c \
        comm_c2f.c \
        comm_call_errhandler.c \
        comm_compare.c \
        comm_connect.c \
        comm_create.c \
        comm_create_errhandler.c \
        comm_create_group.c \
        comm_create_keyval.c \
        comm_delete_attr.c \
        comm_disconnect.c \
        comm_dup.c \
        comm_dup_with_info.c \
        comm_idup.c \
        comm_idup_with_info.c \
        comm_f2c.c \
        comm_free.c \
        comm_free_keyval.c \
        comm_get_attr.c \
        comm_get_errhandler.c \
        comm_get_info.c \
        comm_get_name.c \
        comm_get_parent.c \
        comm_group.c \
        comm_join.c \
        comm_rank.c \
        comm_remote_group.c \
        comm_remote_size.c \
        comm_set_attr.c \
        comm_set_info.c \
        dist_graph_create.c \
        dist_graph_create_adjacent.c \
        dist_graph_neighbors.c \
        dist_graph_neighbors_count.c \
        comm_set_errhandler.c \
        comm_set_name.c \
        comm_size.c \
        comm_spawn.c \
        comm_spawn_multiple.c \
        comm_split.c \
        comm_split_type.c \
        comm_test_inter.c \
        compare_and_swap.c \
        dims_create.c \
        errhandler_c2f.c \
        errhandler_f2c.c \
        errhandler_free.c \
        error_class.c \
        error_string.c \
        exscan.c \
        iexscan.c \
        exscan_init.c \
        fetch_and_op.c \
        file_c2f.c \
        file_call_errhandler.c \
        file_close.c \
        file_create_errhandler.c \
        file_delete.c \
        file_f2c.c \
        file_get_amode.c \
        file_get_atomicity.c \
        file_get_byte_offset.c \
        file_get_errhandler.c \
        file_get_group.c \
        file_get_info.c \
        file_get_position.c \
        file_get_position_shared.c \
        file_get_size.c \
        file_get_type_extent.c \
        file_get_view.c \
        file_iread_at.c \
        file_iread_at_all.c \
        file_iread.c \
        file_iread_all.c \
        file_iread_shared.c \
        file_iwrite_at.c \
        file_iwrite_at_all.c \
        file_iwrite.c \
        file_iwrite_all.c \
        file_iwrite_shared.c \
        file_open.c \
        file_preallocate.c \
        file_read_all_begin.c \
        file_read_all.c \
        file_read_all_end.c \
        file_read_at_all_begin.c \
        file_read_at_all.c \
        file_read_at_all_end.c \
        file_read_at.c \
        file_read.c \
        file_read_ordered_begin.c \
        file_read_ordered.c \
        file_read_ordered_end.c \
        file_read_shared.c \
        file_seek.c \
        file_seek_shared.c \
        file_set_atomicity.c \
        file_set_errhandler.c \
        file_set_info.c \
        file_set_size.c \
        file_set_view.c \
        file_sync.c \
        file_write_all_begin.c \
        file_write_all.c \
        file_write_all_end.c \
        file_write_at_all_begin.c \
        file_write_at_all.c \
        file_write_at_all_end.c \
        file_write_at.c \
        file_write.c \
        file_write_ordered_begin.c \
        file_write_ordered.c \
        file_write_ordered_end.c \
        file_write_shared.c \
        finalize.c \
        finalized.c \
        free_mem.c \
        gather.c \
        igather.c \
        gather_init.c \
        gatherv.c \
        igatherv.c \
        gatherv_init.c \
        get_address.c \
        get_count.c \
        get_elements.c \
        get_elements_x.c \
        get_accumulate.c \
        get_library_version.c \
        get_processor_name.c \
        get_version.c \
        graph_create.c \
        graph_get.c \
        graph_map.c \
        graph_neighbors_count.c \
        graph_neighbors.c \
        graphdims_get.c \
        grequest_complete.c \
        grequest_start.c \
        group_c2f.c \
        group_compare.c \
        group_difference.c \
        group_excl.c \
        group_f2c.c \
        group_free.c \
        group_incl.c \
        group_intersection.c \
        group_range_excl.c \
        group_range_incl.c \
        group_rank.c \
        group_size.c \
        group_translate_ranks.c \
        group_union.c \
        ibsend.c \
        improbe.c \
        imrecv.c \
        info_c2f.c \
        info_create.c \
        info_delete.c \
        info_dup.c \
        info_f2c.c \
        info_free.c \
        info_get.c \
        info_get_nkeys.c \
        info_get_nthkey.c \
        info_get_string.c \
        info_get_valuelen.c \
        info_set.c \
        init.c \
        init_thread.c \
        initialized.c \
        intercomm_create.c \
        intercomm_merge.c \
        iprobe.c \
        irecv.c \
        irsend.c \
        is_thread_main.c \
        isend.c \
        isendrecv.c \
        isendrecv_replace.c \
        issend.c \
        lookup_name.c \
        message_f2c.c \
        message_c2f.c \
        mprobe.c \
        mrecv.c \
        neighbor_allgather.c \
        ineighbor_allgather.c \
        neighbor_allgather_init.c \
        neighbor_allgatherv.c \
        ineighbor_allgatherv.c \
        neighbor_allgatherv_init.c \
        neighbor_alltoall.c \
        ineighbor_alltoall.c \
        neighbor_alltoall_init.c \
        neighbor_alltoallv.c \
        ineighbor_alltoallv.c \
        neighbor_alltoallv_init.c \
        neighbor_alltoallw.c \
        ineighbor_alltoallw.c \
        neighbor_alltoallw_init.c \
        keyval_create.c \
        keyval_free.c \
        op_c2f.c \
        op_commutative.c \
        op_create.c \
        op_f2c.c \
        op_free.c \
        open_port.c \
        pack_external.c \
        pack_external_size.c \
        pack.c \
        pack_size.c \
        parrived.c \
        pcontrol.c \
        pready.c \
        pready_list.c \
        pready_range.c \
        precv_init.c \
        probe.c \
        psend_init.c \
        publish_name.c \
        query_thread.c \
        raccumulate.c \
        recv_init.c \
        recv.c \
        reduce.c \
        ireduce.c \
        reduce_init.c \
        register_datarep.c \
        reduce_local.c \
        reduce_scatter.c \
        ireduce_scatter.c \
        reduce_scatter_init.c \
        reduce_scatter_block.c \
        ireduce_scatter_block.c \
        reduce_scatter_block_init.c \
        request_c2f.c \
        request_f2c.c \
        request_free.c \
        request_get_status.c \
        rget.c \
        rget_accumulate.c \
        rput.c \
        rsend_init.c \
        rsend.c \
        scan.c \
        iscan.c \
        scan_init.c \
        scatter.c \
        iscatter.c \
        scatter_init.c \
        scatterv.c \
        iscatterv.c \
        scatterv_init.c \
        send.c \
        send_init.c \
        sendrecv.c \
        sendrecv_replace.c \
        ssend_init.c \
        ssend.c \
        start.c \
        startall.c \
        status_c2f.c \
        status_c2f08.c \
        status_f082c.c \
        status_f082f.c \
        status_f2c.c \
        status_f2f08.c \
        status_set_cancelled.c \
        status_set_elements.c \
        status_set_elements_x.c \
        testall.c \
        testany.c \
        test.c \
        test_cancelled.c \
        testsome.c \
        topo_test.c \
        type_c2f.c \
        type_commit.c \
        type_contiguous.c \
        type_create_darray.c \
        type_create_f90_complex.c \
        type_create_f90_integer.c \
        type_create_f90_real.c \
        type_create_hindexed.c \
        type_create_hvector.c \
        type_create_indexed_block.c \
        type_create_hindexed_block.c \
        type_create_keyval.c \
        type_create_resized.c \
        type_create_struct.c \
        type_create_subarray.c \
        type_delete_attr.c \
        type_dup.c \
        type_f2c.c \
        type_free.c \
        type_free_keyval.c \
        type_get_attr.c \
        type_get_contents.c \
        type_get_envelope.c \
        type_get_extent.c \
        type_get_extent_x.c \
        type_get_name.c \
        type_get_true_extent.c \
        type_get_true_extent_x.c \
        type_indexed.c \
        type_match_size.c \
        type_set_attr.c \
        type_set_name.c \
        type_size.c \
        type_size_x.c \
        type_vector.c \
        unpack_external.c \
        unpack.c \
        unpublish_name.c \
        wait.c \
        waitall.c \
        waitany.c \
        waitsome.c \
        wtime.c \
        wtick.c \
        accumulate.c \
        get.c \
        put.c \
        win_allocate.c \
        win_allocate_shared.c \
        win_attach.c \
        win_c2f.c \
        win_call_errhandler.c \
        win_complete.c  \
        win_create_errhandler.c \
        win_create_keyval.c \
        win_create.c \
        win_create_dynamic.c \
        win_delete_attr.c \
        win_detach.c \
        win_f2c.c \
        win_fence.c \
        win_flush.c \
        win_flush_all.c \
        win_flush_local.c \
        win_flush_local_all.c \
        win_free_keyval.c \
        win_free.c \
        win_get_attr.c \
        win_get_errhandler.c \
        win_get_group.c \
        win_get_info.c \
        win_get_name.c  \
        win_lock.c \
        win_lock_all.c \
        win_post.c \
        win_set_attr.c \
        win_set_errhandler.c \
        win_set_info.c \
        win_set_name.c \
        win_shared_query.c \
        win_sync.c \
        win_start.c \
        win_test.c \
        win_unlock.c \
        win_unlock_all.c \
        win_wait.c

if OMPI_ENABLE_MPI1_COMPAT
interface_profile_sources += \
        address.c \
        errhandler_create.c \
        errhandler_get.c \
        errhandler_set.c \
        type_extent.c \
        type_hindexed.c \
        type_hvector.c \
        type_lb.c \
        type_struct.c \
        type_ub.c
endif

libmpi_c_profile_la_SOURCES = $(interface_profile_sources)
libmpi_c_profile_la_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=1

libmpi_c_noprofile_la_SOURCES = $(interface_profile_sources)
libmpi_c_noprofile_la_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=0
