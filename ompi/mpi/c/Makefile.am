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
# Copyright (c) 2025      Advanced Micro Devices, Inc. All Rights reserved.
# Copyright (c) 2025-2026 Triad National Security, LLC.  All rights reserved.
# Copyright (c) 2025      UT-Battelle, LLC.  All rights reserved.
# Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
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

include $(top_srcdir)/Makefile.ompi-rules

noinst_LTLIBRARIES = libmpi_c.la libmpi_c_profile.la
if BUILD_MPI_BINDINGS_LAYER
noinst_LTLIBRARIES += libmpi_c_noprofile.la
endif

headers = bindings.h

#
# Template/prototype source files used for generating MPI functions
#

prototype_sources = \
        abort.c.in \
        accumulate.c.in \
        add_error_class.c.in \
        add_error_code.c.in \
        add_error_string.c.in \
        allgather.c.in \
        allgather_init.c.in \
        allgatherv.c.in \
        allgatherv_init.c.in \
        alloc_mem.c.in \
        allreduce.c.in \
        allreduce_init.c.in \
        alltoall.c.in \
        alltoall_init.c.in \
        alltoallv.c.in \
        alltoallv_init.c.in \
        alltoallw.c.in \
        alltoallw_init.c.in \
        attr_delete.c.in \
        attr_get.c.in \
        attr_put.c.in \
        barrier.c.in \
        barrier_init.c.in \
        bcast.c.in \
        bcast_init.c.in \
        bsend.c.in \
        bsend_init.c.in \
        buffer_attach.c.in \
        buffer_detach.c.in \
        buffer_flush.c.in \
        buffer_iflush.c.in \
        cancel.c.in \
        cart_coords.c.in \
        cart_create.c.in \
        cartdim_get.c.in \
        cart_get.c.in \
        cart_map.c.in \
        cart_rank.c.in \
        cart_shift.c.in \
        cart_sub.c.in \
        close_port.c.in \
        comm_accept.c.in \
        comm_attach_buffer.c.in \
        comm_c2f.c.in \
        comm_call_errhandler.c.in \
	comm_compare.c.in \
        comm_connect.c.in \
	comm_create.c.in \
	comm_create_errhandler.c.in \
	comm_create_group.c.in \
	comm_create_from_group.c.in \
	comm_create_keyval.c.in \
        comm_delete_attr.c.in \
        comm_detach_buffer.c.in \
        comm_disconnect.c.in \
        comm_dup.c.in \
        comm_dup_with_info.c.in \
        comm_f2c.c.in \
        comm_flush_buffer.c.in \
        comm_free.c.in \
        comm_free_keyval.c.in \
        comm_get_attr.c.in \
	comm_get_errhandler.c.in \
	comm_get_info.c.in \
	comm_get_name.c.in \
	comm_get_parent.c.in \
	comm_group.c.in \
        comm_idup.c.in \
        comm_idup_with_info.c.in \
        comm_iflush_buffer.c.in \
	comm_join.c.in \
        comm_rank.c.in \
	comm_remote_group.c.in \
	comm_remote_size.c.in \
	comm_set_attr.c.in \
	comm_set_errhandler.c.in \
	comm_set_info.c.in \
	comm_set_name.c.in \
        comm_size.c.in \
	comm_spawn.c.in \
	comm_spawn_multiple.c.in \
        comm_split.c.in \
        comm_split_type.c.in \
	comm_test_inter.c.in \
	compare_and_swap.c.in \
	dims_create.c.in \
	dist_graph_create_adjacent.c.in \
	dist_graph_create.c.in \
	dist_graph_neighbors.c.in \
	dist_graph_neighbors_count.c.in \
        errhandler_c2f.c.in \
        errhandler_f2c.c.in \
	errhandler_free.c.in \
	error_class.c.in \
	error_string.c.in \
	exscan.c.in \
	exscan_init.c.in \
	fetch_and_op.c.in \
	file_c2f.c.in \
	file_call_errhandler.c.in \
	file_close.c.in \
	file_create_errhandler.c.in \
	file_delete.c.in \
	file_f2c.c.in \
	file_get_amode.c.in \
	file_get_atomicity.c.in \
	file_get_byte_offset.c.in \
	file_get_errhandler.c.in \
	file_get_group.c.in \
	file_get_info.c.in \
	file_get_position.c.in \
	file_get_position_shared.c.in \
	file_get_size.c.in \
	file_get_type_extent.c.in \
	file_get_view.c.in \
	file_iread_all.c.in \
	file_iread_at_all.c.in \
	file_iread_at.c.in \
	file_iread.c.in \
	file_iread_shared.c.in \
	file_iwrite_all.c.in \
	file_iwrite_at_all.c.in \
	file_iwrite_at.c.in \
	file_iwrite.c.in \
	file_iwrite_shared.c.in \
        file_open.c.in \
	file_preallocate.c.in \
	file_read_all_begin.c.in \
	file_read_all.c.in \
	file_read_all_end.c.in \
	file_read_at_all_begin.c.in \
	file_read_at_all.c.in \
	file_read_at_all_end.c.in \
	file_read_at.c.in \
	file_read.c.in \
	file_read_ordered_begin.c.in \
	file_read_ordered.c.in \
	file_read_ordered_end.c.in \
	file_read_shared.c.in \
	file_seek.c.in \
	file_seek_shared.c.in \
	file_set_atomicity.c.in \
	file_set_errhandler.c.in \
	file_set_info.c.in \
	file_set_size.c.in \
	file_set_view.c.in \
	file_sync.c.in \
	file_write_all_begin.c.in \
	file_write_all.c.in \
	file_write_all_end.c.in \
	file_write_at_all_begin.c.in \
	file_write_at_all.c.in \
	file_write_at_all_end.c.in \
	file_write_at.c.in \
	file_write.c.in \
	file_write_ordered_begin.c.in \
	file_write_ordered.c.in \
	file_write_ordered_end.c.in \
	file_write_shared.c.in \
        finalize.c.in \
        finalized.c.in \
	free_mem.c.in \
	gather.c.in \
	gather_init.c.in \
	gatherv.c.in \
	gatherv_init.c.in \
	get_accumulate.c.in \
	get_address.c.in \
	get.c.in \
	get_count.c.in \
	get_elements.c.in \
	get_elements_x.c.in \
	get_hw_resource_info.c.in \
	get_library_version.c.in \
	get_processor_name.c.in \
	get_version.c.in \
	graph_create.c.in \
	graphdims_get.c.in \
	graph_get.c.in \
	graph_map.c.in \
	graph_neighbors.c.in \
	graph_neighbors_count.c.in \
	grequest_complete.c.in \
	grequest_start.c.in \
	group_c2f.c.in \
	group_compare.c.in \
	group_difference.c.in \
	group_excl.c.in \
	group_f2c.c.in \
	group_free.c.in \
	group_from_session_pset.c.in \
	group_incl.c.in \
	group_intersection.c.in \
	group_range_excl.c.in \
	group_range_incl.c.in \
	group_rank.c.in \
	group_size.c.in \
	group_translate_ranks.c.in \
	group_union.c.in \
        iallgather.c.in \
        iallgatherv.c.in \
        ialltoall.c.in \
        ialltoallv.c.in \
        ialltoallw.c.in \
        ibarrier.c.in \
        ibcast.c.in \
	ibsend.c.in \
	iexscan.c.in \
	igather.c.in \
	igatherv.c.in \
	improbe.c.in \
	imrecv.c.in \
        ineighbor_allgather.c.in \
        ineighbor_allgatherv.c.in \
        ineighbor_alltoall.c.in \
	ineighbor_alltoallv.c.in \
	ineighbor_alltoallw.c.in \
	info_c2f.c.in \
	info_create.c.in \
	info_create_env.c.in \
	info_delete.c.in \
	info_dup.c.in \
	info_f2c.c.in \
	info_free.c.in \
	info_get.c.in \
	info_get_nkeys.c.in \
	info_get_nthkey.c.in \
	info_get_string.c.in \
	info_get_valuelen.c.in \
	info_set.c.in \
        init.c.in \
        initialized.c.in \
	init_thread.c.in \
	intercomm_create.c.in \
	intercomm_create_from_groups.c.in \
	intercomm_merge.c.in \
	iprobe.c.in \
        iallreduce.c.in \
        irecv.c.in \
	ireduce.c.in \
	ireduce_scatter_block.c.in \
	ireduce_scatter.c.in \
	irsend.c.in \
	iscan.c.in \
	iscatter.c.in \
	iscatterv.c.in \
        isend.c.in \
	isendrecv.c.in \
	isendrecv_replace.c.in \
	issend.c.in \
	is_thread_main.c.in \
	keyval_create.c.in \
	keyval_free.c.in \
	lookup_name.c.in \
        message_c2f.c.in \
        message_f2c.c.in \
	mprobe.c.in \
	mrecv.c.in \
	neighbor_allgather.c.in \
        neighbor_allgather_init.c.in \
        neighbor_allgatherv.c.in \
        neighbor_allgatherv_init.c.in \
        neighbor_alltoall.c.in \
        neighbor_alltoall_init.c.in \
        neighbor_alltoallv.c.in \
        neighbor_alltoallv_init.c.in\
	neighbor_alltoallw.c.in \
	neighbor_alltoallw_init.c.in \
        open_port.c.in \
	op_c2f.c.in \
	op_commutative.c.in \
	op_create.c.in \
	op_free.c.in \
	op_f2c.c.in \
	pack.c.in \
	pack_external.c.in \
	pack_external_size.c.in \
	pack_size.c.in \
	parrived.c.in \
	pready.c.in \
	pready_list.c.in \
	pready_range.c.in \
	precv_init.c.in \
	probe.c.in \
	psend_init.c.in \
	publish_name.c.in \
	put.c.in \
	query_thread.c.in \
	raccumulate.c.in \
        recv.c.in \
	recv_init.c.in \
	reduce.c.in \
	reduce_init.c.in \
	reduce_local.c.in \
	reduce_scatter_block.c.in \
	reduce_scatter_block_init.c.in \
	reduce_scatter.c.in \
	reduce_scatter_init.c.in \
	register_datarep.c.in \
        request_c2f.c.in \
        request_f2c.c.in \
	remove_error_class.c.in \
	remove_error_code.c.in \
	remove_error_string.c.in \
	request_free.c.in \
	request_get_status.c.in \
	request_get_status_all.c.in \
	request_get_status_any.c.in \
	request_get_status_some.c.in \
	rget_accumulate.c.in \
	rget.c.in \
	rput.c.in \
	rsend.c.in \
	rsend_init.c.in \
        scan.c.in \
	scan_init.c.in \
	scatter.c.in \
	scatter_init.c.in \
	scatterv.c.in \
	scatterv_init.c.in \
        send.c.in \
	send_init.c.in \
	sendrecv.c.in \
	sendrecv_replace.c.in \
	session_attach_buffer.c.in \
	session_c2f.c.in \
	session_call_errhandler.c.in \
	session_create_errhandler.c.in \
	session_detach_buffer.c.in \
	session_f2c.c.in \
	session_finalize.c.in \
	session_flush_buffer.c.in \
	session_get_errhandler.c.in \
	session_get_info.c.in \
	session_get_nth_pset.c.in \
	session_get_num_psets.c.in \
	session_get_pset_info.c.in \
	session_iflush_buffer.c.in \
	session_init.c.in \
	session_set_errhandler.c.in \
	ssend.c.in \
	ssend_init.c.in \
	startall.c.in \
	start.c.in \
	status_c2f08.c.in \
	status_c2f.c.in \
	status_f082c.c.in \
	status_f082f.c.in \
	status_f2c.c.in \
	status_f2f08.c.in \
        status_get_error.c.in \
        status_get_source.c.in \
        status_get_tag.c.in \
	status_set_cancelled.c.in \
	status_set_elements.c.in \
	status_set_elements_x.c.in \
        status_set_error.c.in \
        status_set_source.c.in \
        status_set_tag.c.in \
	testall.c.in \
	testany.c.in \
	test.c.in \
	test_cancelled.c.in \
	testsome.c.in \
	topo_test.c.in \
	type_c2f.c.in \
	type_commit.c.in \
	type_contiguous.c.in \
	type_create_darray.c.in \
	type_create_f90_complex.c.in \
	type_create_f90_integer.c.in \
	type_create_f90_real.c.in \
        type_create_hindexed.c.in \
	type_create_hindexed_block.c.in \
	type_create_hvector.c.in \
	type_create_indexed_block.c.in \
	type_create_keyval.c.in \
	type_create_resized.c.in \
        type_create_struct.c.in \
	type_create_subarray.c.in \
	type_delete_attr.c.in \
	type_dup.c.in \
	type_f2c.c.in \
	type_free.c.in \
	type_free_keyval.c.in \
	type_get_attr.c.in \
	type_get_extent.c.in \
	type_get_extent_x.c.in \
	type_get_name.c.in \
	type_get_true_extent.c.in \
	type_get_true_extent_x.c.in \
	type_get_value_index.c.in \
	type_indexed.c.in \
	type_match_size.c.in \
	type_set_attr.c.in \
	type_set_name.c.in \
	type_size.c.in \
	type_size_x.c.in \
	type_vector.c.in \
	unpack.c.in \
	unpack_external.c.in \
	unpublish_name.c.in \
        waitall.c.in \
	waitany.c.in \
	wait.c.in \
        waitsome.c.in \
	win_allocate.c.in \
	win_allocate_shared.c.in \
        win_attach.c.in \
        win_c2f.c.in \
        win_call_errhandler.c.in \
        win_complete.c.in  \
	win_create.c.in \
	win_create_dynamic.c.in \
	win_create_errhandler.c.in \
	win_create_keyval.c.in \
        win_delete_attr.c.in \
        win_detach.c.in \
        win_fence.c.in \
        win_flush_all.c.in \
        win_flush.c.in \
        win_flush_local_all.c.in \
        win_flush_local.c.in \
	win_free.c.in \
        win_free_keyval.c.in \
	win_get_attr.c.in \
	win_get_errhandler.c.in \
	win_get_group.c.in \
	win_get_info.c.in \
	win_get_name.c.in \
	win_lock_all.c.in \
	win_lock.c.in \
	win_post.c.in \
	win_set_attr.c.in \
	win_set_errhandler.c.in \
	win_set_info.c.in \
	win_set_name.c.in \
	win_shared_query.c.in \
	win_start.c.in \
	win_sync.c.in \
	win_test.c.in \
	win_unlock_all.c.in \
	win_unlock.c.in \
	win_wait.c.in \
        wtime.c.in

# Include template files in case someone wants to update them
EXTRA_DIST = $(prototype_sources) mpi_bindings_generated.stamp

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
# List of all C files that have profile versions (generated_*.c files were
# generated from prototype_sources above).
#
#
interface_profile_sources = \
	abort_generated.c \
	accumulate_generated.c \
	add_error_class_generated.c \
	add_error_code_generated.c \
	add_error_string_generated.c \
	allgather_generated.c \
	allgather_init_generated.c \
	allgatherv_generated.c \
	allgatherv_init_generated.c \
	alloc_mem_generated.c \
	allreduce_generated.c \
	allreduce_init_generated.c \
	alltoall_generated.c \
	alltoall_init_generated.c \
	alltoallv_generated.c \
	alltoallv_init_generated.c \
	alltoallw_generated.c \
	alltoallw_init_generated.c \
	attr_delete_generated.c \
	attr_get_generated.c \
	attr_put_generated.c \
	barrier_generated.c \
	barrier_init_generated.c \
	bcast_generated.c \
	bcast_init_generated.c \
	bsend_generated.c \
	bsend_init_generated.c \
	buffer_attach_generated.c \
	buffer_detach_generated.c \
	buffer_flush_generated.c \
	buffer_iflush_generated.c \
	cancel_generated.c \
	cart_coords_generated.c \
	cart_create_generated.c \
	cartdim_get_generated.c \
	cart_get_generated.c \
	cart_map_generated.c \
	cart_rank_generated.c \
	cart_shift_generated.c \
	cart_sub_generated.c \
	close_port_generated.c \
	comm_accept_generated.c \
	comm_attach_buffer_generated.c \
	comm_c2f_generated.c \
	comm_call_errhandler_generated.c \
	comm_compare_generated.c \
	comm_connect_generated.c \
	comm_create_generated.c \
	comm_create_errhandler_generated.c \
	comm_create_group_generated.c \
	comm_create_from_group_generated.c \
	comm_create_keyval_generated.c \
	comm_delete_attr_generated.c \
	comm_detach_buffer_generated.c \
	comm_disconnect_generated.c \
	comm_dup_generated.c \
	comm_dup_with_info_generated.c \
	comm_f2c_generated.c \
	comm_flush_buffer_generated.c \
	comm_free_generated.c \
	comm_free_keyval_generated.c \
	comm_get_attr_generated.c \
	comm_get_errhandler_generated.c \
	comm_get_info_generated.c \
	comm_get_name_generated.c \
	comm_get_parent_generated.c \
	comm_group_generated.c \
	comm_idup_generated.c \
	comm_idup_with_info_generated.c \
	comm_iflush_buffer_generated.c \
	comm_join_generated.c \
	comm_rank_generated.c \
	comm_remote_group_generated.c \
	comm_remote_size_generated.c \
	comm_set_attr_generated.c \
	comm_set_errhandler_generated.c \
	comm_set_info_generated.c \
	comm_set_name_generated.c \
	comm_size_generated.c \
	comm_spawn_generated.c \
	comm_spawn_multiple_generated.c \
	comm_split_generated.c \
	comm_split_type_generated.c \
	comm_test_inter_generated.c \
	compare_and_swap_generated.c \
	dims_create_generated.c \
	dist_graph_create_adjacent_generated.c \
	dist_graph_create_generated.c \
	dist_graph_neighbors_generated.c \
	dist_graph_neighbors_count_generated.c \
	errhandler_c2f_generated.c \
	errhandler_f2c_generated.c \
	errhandler_free_generated.c \
	error_class_generated.c \
	error_string_generated.c \
	exscan_generated.c \
	exscan_init_generated.c \
	fetch_and_op_generated.c \
	file_c2f_generated.c \
	file_call_errhandler_generated.c \
	file_close_generated.c \
	file_create_errhandler_generated.c \
	file_delete_generated.c \
	file_f2c_generated.c \
	file_get_amode_generated.c \
	file_get_atomicity_generated.c \
	file_get_byte_offset_generated.c \
	file_get_errhandler_generated.c \
	file_get_group_generated.c \
	file_get_info_generated.c \
	file_get_position_generated.c \
	file_get_position_shared_generated.c \
	file_get_size_generated.c \
	file_get_type_extent_generated.c \
	file_get_view_generated.c \
	file_iread_all_generated.c \
	file_iread_at_all_generated.c \
	file_iread_at_generated.c \
	file_iread_generated.c \
	file_iread_shared_generated.c \
	file_iwrite_all_generated.c \
	file_iwrite_at_all_generated.c \
	file_iwrite_at_generated.c \
	file_iwrite_generated.c \
	file_iwrite_shared_generated.c \
	file_open_generated.c \
	file_preallocate_generated.c \
	file_read_all_begin_generated.c \
	file_read_all_generated.c \
	file_read_all_end_generated.c \
	file_read_at_all_begin_generated.c \
	file_read_at_all_generated.c \
	file_read_at_all_end_generated.c \
	file_read_at_generated.c \
	file_read_generated.c \
	file_read_ordered_begin_generated.c \
	file_read_ordered_generated.c \
	file_read_ordered_end_generated.c \
	file_read_shared_generated.c \
	file_seek_generated.c \
	file_seek_shared_generated.c \
	file_set_atomicity_generated.c \
	file_set_errhandler_generated.c \
	file_set_info_generated.c \
	file_set_size_generated.c \
	file_set_view_generated.c \
	file_sync_generated.c \
	file_write_all_begin_generated.c \
	file_write_all_generated.c \
	file_write_all_end_generated.c \
	file_write_at_all_begin_generated.c \
	file_write_at_all_generated.c \
	file_write_at_all_end_generated.c \
	file_write_at_generated.c \
	file_write_generated.c \
	file_write_ordered_begin_generated.c \
	file_write_ordered_generated.c \
	file_write_ordered_end_generated.c \
	file_write_shared_generated.c \
	finalize_generated.c \
	finalized_generated.c \
	free_mem_generated.c \
	gather_generated.c \
	gather_init_generated.c \
	gatherv_generated.c \
	gatherv_init_generated.c \
	get_accumulate_generated.c \
	get_address_generated.c \
	get_generated.c \
	get_count_generated.c \
	get_elements_generated.c \
	get_elements_x_generated.c \
	get_hw_resource_info_generated.c \
	get_library_version_generated.c \
	get_processor_name_generated.c \
	get_version_generated.c \
	graph_create_generated.c \
	graphdims_get_generated.c \
	graph_get_generated.c \
	graph_map_generated.c \
	graph_neighbors_generated.c \
	graph_neighbors_count_generated.c \
	grequest_complete_generated.c \
	grequest_start_generated.c \
	group_c2f_generated.c \
	group_compare_generated.c \
	group_difference_generated.c \
	group_excl_generated.c \
	group_f2c_generated.c \
	group_free_generated.c \
	group_from_session_pset_generated.c \
	group_incl_generated.c \
	group_intersection_generated.c \
	group_range_excl_generated.c \
	group_range_incl_generated.c \
	group_rank_generated.c \
	group_size_generated.c \
	group_translate_ranks_generated.c \
	group_union_generated.c \
	iallgather_generated.c \
	iallgatherv_generated.c \
	ialltoall_generated.c \
	ialltoallv_generated.c \
	ialltoallw_generated.c \
	ibarrier_generated.c \
	ibcast_generated.c \
	ibsend_generated.c \
	iexscan_generated.c \
	igather_generated.c \
	igatherv_generated.c \
	improbe_generated.c \
	imrecv_generated.c \
	ineighbor_allgather_generated.c \
	ineighbor_allgatherv_generated.c \
	ineighbor_alltoall_generated.c \
	ineighbor_alltoallv_generated.c \
	ineighbor_alltoallw_generated.c \
	info_c2f_generated.c \
	info_create_generated.c \
	info_create_env_generated.c \
	info_delete_generated.c \
	info_dup_generated.c \
	info_f2c_generated.c \
	info_free_generated.c \
	info_get_generated.c \
	info_get_nkeys_generated.c \
	info_get_nthkey_generated.c \
	info_get_string_generated.c \
	info_get_valuelen_generated.c \
	info_set_generated.c \
	init_generated.c \
	initialized_generated.c \
	init_thread_generated.c \
	intercomm_create_generated.c \
	intercomm_create_from_groups_generated.c \
	intercomm_merge_generated.c \
	iprobe_generated.c \
	iallreduce_generated.c \
	irecv_generated.c \
	ireduce_generated.c \
	ireduce_scatter_block_generated.c \
	ireduce_scatter_generated.c \
	irsend_generated.c \
	iscan_generated.c \
	iscatter_generated.c \
	iscatterv_generated.c \
	isend_generated.c \
	isendrecv_generated.c \
	isendrecv_replace_generated.c \
	issend_generated.c \
	is_thread_main_generated.c \
	keyval_create_generated.c \
	keyval_free_generated.c \
	lookup_name_generated.c \
	message_c2f_generated.c \
	message_f2c_generated.c \
	mprobe_generated.c \
	mrecv_generated.c \
	neighbor_allgather_generated.c \
	neighbor_allgather_init_generated.c \
	neighbor_allgatherv_generated.c \
	neighbor_allgatherv_init_generated.c \
	neighbor_alltoall_generated.c \
	neighbor_alltoall_init_generated.c \
	neighbor_alltoallv_generated.c \
	neighbor_alltoallv_init_generated.c\
	neighbor_alltoallw_generated.c \
	neighbor_alltoallw_init_generated.c \
	open_port_generated.c \
	op_c2f_generated.c \
	op_commutative_generated.c \
	op_create_generated.c \
	op_free_generated.c \
	op_f2c_generated.c \
	pack_generated.c \
	pack_external_generated.c \
	pack_external_size_generated.c \
	pack_size_generated.c \
	parrived_generated.c \
	pready_generated.c \
	pready_list_generated.c \
	pready_range_generated.c \
	precv_init_generated.c \
	probe_generated.c \
	psend_init_generated.c \
	publish_name_generated.c \
	put_generated.c \
	query_thread_generated.c \
	raccumulate_generated.c \
	recv_generated.c \
	recv_init_generated.c \
	reduce_generated.c \
	reduce_init_generated.c \
	reduce_local_generated.c \
	reduce_scatter_block_generated.c \
	reduce_scatter_block_init_generated.c \
	reduce_scatter_generated.c \
	reduce_scatter_init_generated.c \
	register_datarep_generated.c \
	request_c2f_generated.c \
	request_f2c_generated.c \
	remove_error_class_generated.c \
	remove_error_code_generated.c \
	remove_error_string_generated.c \
	request_free_generated.c \
	request_get_status_generated.c \
	request_get_status_all_generated.c \
	request_get_status_any_generated.c \
	request_get_status_some_generated.c \
	rget_accumulate_generated.c \
	rget_generated.c \
	rput_generated.c \
	rsend_generated.c \
	rsend_init_generated.c \
	scan_generated.c \
	scan_init_generated.c \
	scatter_generated.c \
	scatter_init_generated.c \
	scatterv_generated.c \
	scatterv_init_generated.c \
	send_generated.c \
	send_init_generated.c \
	sendrecv_generated.c \
	sendrecv_replace_generated.c \
	session_attach_buffer_generated.c \
	session_c2f_generated.c \
	session_call_errhandler_generated.c \
	session_create_errhandler_generated.c \
	session_detach_buffer_generated.c \
	session_f2c_generated.c \
	session_finalize_generated.c \
	session_flush_buffer_generated.c \
	session_get_errhandler_generated.c \
	session_get_info_generated.c \
	session_get_nth_pset_generated.c \
	session_get_num_psets_generated.c \
	session_get_pset_info_generated.c \
	session_iflush_buffer_generated.c \
	session_init_generated.c \
	session_set_errhandler_generated.c \
	ssend_generated.c \
	ssend_init_generated.c \
	startall_generated.c \
	start_generated.c \
	status_c2f08_generated.c \
	status_c2f_generated.c \
	status_f082c_generated.c \
	status_f082f_generated.c \
	status_f2c_generated.c \
	status_f2f08_generated.c \
	status_get_error_generated.c \
	status_get_source_generated.c \
	status_get_tag_generated.c \
	status_set_cancelled_generated.c \
	status_set_elements_generated.c \
	status_set_elements_x_generated.c \
	status_set_error_generated.c \
	status_set_source_generated.c \
	status_set_tag_generated.c \
	testall_generated.c \
	testany_generated.c \
	test_generated.c \
	test_cancelled_generated.c \
	testsome_generated.c \
	topo_test_generated.c \
	type_c2f_generated.c \
	type_commit_generated.c \
	type_contiguous_generated.c \
	type_create_darray_generated.c \
	type_create_f90_complex_generated.c \
	type_create_f90_integer_generated.c \
	type_create_f90_real_generated.c \
	type_create_hindexed_generated.c \
	type_create_hindexed_block_generated.c \
	type_create_hvector_generated.c \
	type_create_indexed_block_generated.c \
	type_create_keyval_generated.c \
	type_create_resized_generated.c \
	type_create_struct_generated.c \
	type_create_subarray_generated.c \
	type_delete_attr_generated.c \
	type_dup_generated.c \
	type_f2c_generated.c \
	type_free_generated.c \
	type_free_keyval_generated.c \
	type_get_attr_generated.c \
	type_get_extent_generated.c \
	type_get_extent_x_generated.c \
	type_get_name_generated.c \
	type_get_true_extent_generated.c \
	type_get_true_extent_x_generated.c \
	type_get_value_index_generated.c \
	type_indexed_generated.c \
	type_match_size_generated.c \
	type_set_attr_generated.c \
	type_set_name_generated.c \
	type_size_generated.c \
	type_size_x_generated.c \
	type_vector_generated.c \
	unpack_generated.c \
	unpack_external_generated.c \
	unpublish_name_generated.c \
	waitall_generated.c \
	waitany_generated.c \
	wait_generated.c \
	waitsome_generated.c \
	win_allocate_generated.c \
	win_allocate_shared_generated.c \
	win_attach_generated.c \
	win_c2f_generated.c \
	win_call_errhandler_generated.c \
	win_complete_generated.c  \
	win_create_generated.c \
	win_create_dynamic_generated.c \
	win_create_errhandler_generated.c \
	win_create_keyval_generated.c \
	win_delete_attr_generated.c \
	win_detach_generated.c \
	win_fence_generated.c \
	win_flush_all_generated.c \
	win_flush_generated.c \
	win_flush_local_all_generated.c \
	win_flush_local_generated.c \
	win_free_generated.c \
	win_free_keyval_generated.c \
	win_get_attr_generated.c \
	win_get_errhandler_generated.c \
	win_get_group_generated.c \
	win_get_info_generated.c \
	win_get_name_generated.c \
	win_lock_all_generated.c \
	win_lock_generated.c \
	win_post_generated.c \
	win_set_attr_generated.c \
	win_set_errhandler_generated.c \
	win_set_info_generated.c \
	win_set_name_generated.c \
	win_shared_query_generated.c \
	win_start_generated.c \
	win_sync_generated.c \
	win_test_generated.c \
	win_unlock_all_generated.c \
	win_unlock_generated.c \
	win_wait_generated.c \
	wtime_generated.c

# The following are special case functions where we
# have to deal manually
#
interface_profile_sources += \
        pcontrol.c \
	type_get_contents.c \
	type_get_contents_c.c \
        type_get_envelope.c \
        type_get_envelope_c.c \
        win_f2c.c \
        wtick.c

# The following functions were removed from the MPI standard, but are
# retained for ABI compliance reasons.  They are listed independently
# of the other MPI functions in case we one day change behavior around
# ABI compliance.
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

libmpi_c_profile_la_SOURCES = $(interface_profile_sources)
libmpi_c_profile_la_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=1

libmpi_c_noprofile_la_SOURCES = $(interface_profile_sources)
libmpi_c_noprofile_la_CPPFLAGS = -DOMPI_BUILD_MPI_PROFILING=0

# ABI generation rules
mpi_bindings_generated.stamp: $(prototype_sources)
	@set -e; \
	for src in $(prototype_sources); do \
	    base=`echo "$$src" | sed 's/\\.c\\.in$$//'`; \
	    out="$$base"_generated.c; \
	    $(PYTHON) $(top_srcdir)/ompi/mpi/bindings/bindings.py \
	        --builddir $(abs_builddir) \
	        --srcdir $(abs_srcdir) \
	        --output "$$out" \
	        c \
	        source \
	        ompi \
	        "$$src"; \
	done; \
	touch $@

$(interface_profile_sources): mpi_bindings_generated.stamp

# Delete generated files on maintainer-clean
MAINTAINERCLEANFILES = *_generated.c mpi_bindings_generated.stamp
