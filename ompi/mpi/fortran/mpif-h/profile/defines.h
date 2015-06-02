/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Inria.  All rights reserved.
 * Copyright (c) 2011-2013 Universite Bordeaux 1
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#ifndef OMPI_F77_PROFILE_DEFINES_H
#define OMPI_F77_PROFILE_DEFINES_H

#define ompi_abort_f pompi_abort_f
#define ompi_accumulate_f pompi_accumulate_f
#define ompi_add_error_class_f pompi_add_error_class_f
#define ompi_add_error_code_f pompi_add_error_code_f
#define ompi_add_error_string_f pompi_add_error_string_f
#define ompi_address_f pompi_address_f
#define ompi_allgather_f pompi_allgather_f
#define ompi_allgatherv_f pompi_allgatherv_f
#define ompi_alloc_mem_f pompi_alloc_mem_f
#define ompi_allreduce_f pompi_allreduce_f
#define ompi_alltoall_f pompi_alltoall_f
#define ompi_alltoallv_f pompi_alltoallv_f
#define ompi_alltoallw_f pompi_alltoallw_f
#define ompi_attr_delete_f pompi_attr_delete_f
#define ompi_attr_get_f pompi_attr_get_f
#define ompi_attr_put_f pompi_attr_put_f
#define ompi_barrier_f pompi_barrier_f
#define ompi_bcast_f pompi_bcast_f
#define ompi_bsend_f pompi_bsend_f
#define ompi_bsend_init_f pompi_bsend_init_f
#define ompi_buffer_attach_f pompi_buffer_attach_f
#define ompi_buffer_detach_f pompi_buffer_detach_f
#define ompi_cancel_f pompi_cancel_f
#define ompi_cart_coords_f pompi_cart_coords_f
#define ompi_cart_create_f pompi_cart_create_f
#define ompi_cart_get_f pompi_cart_get_f
#define ompi_cart_map_f pompi_cart_map_f
#define ompi_cart_rank_f pompi_cart_rank_f
#define ompi_cart_shift_f pompi_cart_shift_f
#define ompi_cart_sub_f pompi_cart_sub_f
#define ompi_cartdim_get_f pompi_cartdim_get_f
#define ompi_close_port_f pompi_close_port_f
#define ompi_comm_accept_f pompi_comm_accept_f
#define ompi_comm_call_errhandler_f pompi_comm_call_errhandler_f
#define ompi_comm_compare_f pompi_comm_compare_f
#define ompi_comm_connect_f pompi_comm_connect_f
#define ompi_comm_create_errhandler_f pompi_comm_create_errhandler_f
#define ompi_comm_create_keyval_f pompi_comm_create_keyval_f
#define ompi_comm_create_group_f pompi_comm_create_group_f
#define ompi_comm_create_f pompi_comm_create_f
#define ompi_comm_delete_attr_f pompi_comm_delete_attr_f
#define ompi_comm_disconnect_f pompi_comm_disconnect_f
#define ompi_comm_dup_with_info_f pompi_comm_dup_with_info_f
#define ompi_comm_dup_f pompi_comm_dup_f
#define ompi_comm_idup_f pompi_comm_idup_f
#define ompi_comm_free_keyval_f pompi_comm_free_keyval_f
#define ompi_comm_free_f pompi_comm_free_f
#define ompi_comm_get_attr_f pompi_comm_get_attr_f
#define ompi_comm_get_info_f pompi_comm_get_info_f
#define ompi_comm_get_errhandler_f pompi_comm_get_errhandler_f
#define ompi_comm_get_name_f pompi_comm_get_name_f
#define ompi_comm_get_parent_f pompi_comm_get_parent_f
#define ompi_comm_group_f pompi_comm_group_f
#define ompi_comm_join_f pompi_comm_join_f
#define ompi_comm_rank_f pompi_comm_rank_f
#define ompi_comm_remote_group_f pompi_comm_remote_group_f
#define ompi_comm_remote_size_f pompi_comm_remote_size_f
#define ompi_comm_set_attr_f pompi_comm_set_attr_f
#define ompi_comm_set_info_f pompi_comm_set_info_f
#define ompi_comm_set_errhandler_f pompi_comm_set_errhandler_f
#define ompi_comm_set_name_f pompi_comm_set_name_f
#define ompi_comm_size_f pompi_comm_size_f
#define ompi_comm_spawn_f pompi_comm_spawn_f
#define ompi_comm_spawn_multiple_f pompi_comm_spawn_multiple_f
#define ompi_comm_split_f pompi_comm_split_f
#define ompi_comm_split_type_f pompi_comm_split_type_f
#define ompi_comm_test_inter_f pompi_comm_test_inter_f
#define ompi_compare_and_swap_f pompi_compare_and_swap_f
#define ompi_dims_create_f pompi_dims_create_f
#define ompi_dist_graph_create_f pompi_dist_graph_create_f
#define ompi_dist_graph_create_adjacent_f pompi_dist_graph_create_adjacent_f
#define ompi_dist_graph_neighbors_f pompi_dist_graph_neighbors_f
#define ompi_dist_graph_neighbors_count_f pompi_dist_graph_neighbors_count_f
#define ompi_errhandler_create_f pompi_errhandler_create_f
#define ompi_errhandler_free_f pompi_errhandler_free_f
#define ompi_errhandler_get_f pompi_errhandler_get_f
#define ompi_errhandler_set_f pompi_errhandler_set_f
#define ompi_error_class_f pompi_error_class_f
#define ompi_error_string_f pompi_error_string_f
#define ompi_exscan_f pompi_exscan_f
#define ompi_f_sync_reg_f pompi_f_sync_reg_f
#define ompi_fetch_and_op_f pompi_fetch_and_op_f
#define ompi_file_call_errhandler_f pompi_file_call_errhandler_f
#define ompi_file_create_errhandler_f pompi_file_create_errhandler_f
#define ompi_file_set_errhandler_f pompi_file_set_errhandler_f
#define ompi_file_get_errhandler_f pompi_file_get_errhandler_f
#define ompi_file_open_f pompi_file_open_f
#define ompi_file_close_f pompi_file_close_f
#define ompi_file_delete_f pompi_file_delete_f
#define ompi_file_set_size_f pompi_file_set_size_f
#define ompi_file_preallocate_f pompi_file_preallocate_f
#define ompi_file_get_size_f pompi_file_get_size_f
#define ompi_file_get_group_f pompi_file_get_group_f
#define ompi_file_get_amode_f pompi_file_get_amode_f
#define ompi_file_set_info_f pompi_file_set_info_f
#define ompi_file_get_info_f pompi_file_get_info_f
#define ompi_file_set_view_f pompi_file_set_view_f
#define ompi_file_get_view_f pompi_file_get_view_f
#define ompi_file_read_at_f pompi_file_read_at_f
#define ompi_file_read_at_all_f pompi_file_read_at_all_f
#define ompi_file_write_at_f pompi_file_write_at_f
#define ompi_file_write_at_all_f pompi_file_write_at_all_f
#define ompi_file_iread_at_f pompi_file_iread_at_f
#define ompi_file_iwrite_at_f pompi_file_iwrite_at_f
#define ompi_file_read_f pompi_file_read_f
#define ompi_file_read_all_f pompi_file_read_all_f
#define ompi_file_write_f pompi_file_write_f
#define ompi_file_write_all_f pompi_file_write_all_f
#define ompi_file_iread_f pompi_file_iread_f
#define ompi_file_iwrite_f pompi_file_iwrite_f
#define ompi_file_seek_f pompi_file_seek_f
#define ompi_file_get_position_f pompi_file_get_position_f
#define ompi_file_get_byte_offset_f pompi_file_get_byte_offset_f
#define ompi_file_read_shared_f pompi_file_read_shared_f
#define ompi_file_write_shared_f pompi_file_write_shared_f
#define ompi_file_iread_shared_f pompi_file_iread_shared_f
#define ompi_file_iwrite_shared_f pompi_file_iwrite_shared_f
#define ompi_file_read_ordered_f pompi_file_read_ordered_f
#define ompi_file_write_ordered_f pompi_file_write_ordered_f
#define ompi_file_seek_shared_f pompi_file_seek_shared_f
#define ompi_file_get_position_shared_f pompi_file_get_position_shared_f
#define ompi_file_read_at_all_begin_f pompi_file_read_at_all_begin_f
#define ompi_file_read_at_all_end_f pompi_file_read_at_all_end_f
#define ompi_file_write_at_all_begin_f pompi_file_write_at_all_begin_f
#define ompi_file_write_at_all_end_f pompi_file_write_at_all_end_f
#define ompi_file_read_all_begin_f pompi_file_read_all_begin_f
#define ompi_file_read_all_end_f pompi_file_read_all_end_f
#define ompi_file_write_all_begin_f pompi_file_write_all_begin_f
#define ompi_file_write_all_end_f pompi_file_write_all_end_f
#define ompi_file_read_ordered_begin_f pompi_file_read_ordered_begin_f
#define ompi_file_read_ordered_end_f pompi_file_read_ordered_end_f
#define ompi_file_write_ordered_begin_f pompi_file_write_ordered_begin_f
#define ompi_file_write_ordered_end_f pompi_file_write_ordered_end_f
#define ompi_file_get_type_extent_f pompi_file_get_type_extent_f
#define ompi_file_set_atomicity_f pompi_file_set_atomicity_f
#define ompi_file_get_atomicity_f pompi_file_get_atomicity_f
#define ompi_file_sync_f pompi_file_sync_f
#define ompi_finalize_f pompi_finalize_f
#define ompi_finalized_f pompi_finalized_f
#define ompi_free_mem_f pompi_free_mem_f
#define ompi_gather_f pompi_gather_f
#define ompi_gatherv_f pompi_gatherv_f
#define ompi_get_address_f pompi_get_address_f
#define ompi_get_count_f pompi_get_count_f
#define ompi_get_elements_f pompi_get_elements_f
#define ompi_get_elements_x_f pompi_get_elements_x_f
#define ompi_get_f pompi_get_f
#define ompi_get_accumulate_f pompi_get_accumulate_f
#define ompi_get_library_version_f pompi_get_library_version_f
#define ompi_get_processor_name_f pompi_get_processor_name_f
#define ompi_get_version_f pompi_get_version_f
#define ompi_graph_create_f pompi_graph_create_f
#define ompi_graph_get_f pompi_graph_get_f
#define ompi_graph_map_f pompi_graph_map_f
#define ompi_graph_neighbors_count_f pompi_graph_neighbors_count_f
#define ompi_graph_neighbors_f pompi_graph_neighbors_f
#define ompi_graphdims_get_f pompi_graphdims_get_f
#define ompi_grequest_complete_f pompi_grequest_complete_f
#define ompi_grequest_start_f pompi_grequest_start_f
#define ompi_group_compare_f pompi_group_compare_f
#define ompi_group_difference_f pompi_group_difference_f
#define ompi_group_excl_f pompi_group_excl_f
#define ompi_group_free_f pompi_group_free_f
#define ompi_group_incl_f pompi_group_incl_f
#define ompi_group_intersection_f pompi_group_intersection_f
#define ompi_group_range_excl_f pompi_group_range_excl_f
#define ompi_group_range_incl_f pompi_group_range_incl_f
#define ompi_group_rank_f pompi_group_rank_f
#define ompi_group_size_f pompi_group_size_f
#define ompi_group_translate_ranks_f pompi_group_translate_ranks_f
#define ompi_group_union_f pompi_group_union_f
#define ompi_iallgather_f pompi_iallgather_f
#define ompi_iallgatherv_f pompi_iallgatherv_f
#define ompi_iallgather_f pompi_iallgather_f
#define ompi_iallreduce_f pompi_iallreduce_f
#define ompi_ialltoall_f pompi_ialltoall_f
#define ompi_ialltoallv_f pompi_ialltoallv_f
#define ompi_ialltoallw_f pompi_ialltoallw_f
#define ompi_ibarrier_f pompi_ibarrier_f
#define ompi_ibcast_f pompi_ibcast_f
#define ompi_ibsend_f pompi_ibsend_f
#define ompi_iexscan_f pompi_iexscan_f
#define ompi_igather_f pompi_igather_f
#define ompi_igatherv_f pompi_igatherv_f
#define ompi_improbe_f pompi_improbe_f
#define ompi_imrecv_f pompi_imrecv_f
#define ompi_ineighbor_allgather_f pompi_ineighbor_allgather_f
#define ompi_ineighbor_allgatherv_f pompi_ineighbor_allgatherv_f
#define ompi_ineighbor_alltoall_f pompi_ineighbor_alltoall_f
#define ompi_ineighbor_alltoallv_f pompi_ineighbor_alltoallv_f
#define ompi_ineighbor_alltoallw_f pompi_ineighbor_alltoallw_f
#define ompi_ireduce_f pompi_ireduce_f
#define ompi_ireduce_scatter_f pompi_ireduce_scatter_f
#define ompi_ireduce_scatter_block_f pompi_ireduce_scatter_block_f
#define ompi_iscan_f pompi_iscan_f
#define ompi_iscatter_f pompi_iscatter_f
#define ompi_iscatterv_f pompi_iscatterv_f
#define ompi_info_create_f pompi_info_create_f
#define ompi_info_delete_f pompi_info_delete_f
#define ompi_info_dup_f pompi_info_dup_f
#define ompi_info_free_f pompi_info_free_f
#define ompi_info_get_f pompi_info_get_f
#define ompi_info_get_nkeys_f pompi_info_get_nkeys_f
#define ompi_info_get_nthkey_f pompi_info_get_nthkey_f
#define ompi_info_get_valuelen_f pompi_info_get_valuelen_f
#define ompi_info_set_f pompi_info_set_f
#define ompi_init_f pompi_init_f
#define ompi_initialized_f pompi_initialized_f
#define ompi_init_thread_f pompi_init_thread_f
#define ompi_intercomm_create_f pompi_intercomm_create_f
#define ompi_intercomm_merge_f pompi_intercomm_merge_f
#define ompi_iprobe_f pompi_iprobe_f
#define ompi_irecv_f pompi_irecv_f
#define ompi_irsend_f pompi_irsend_f
#define ompi_isend_f pompi_isend_f
#define ompi_issend_f pompi_issend_f
#define ompi_is_thread_main_f pompi_is_thread_main_f
#define ompi_keyval_create_f pompi_keyval_create_f
#define ompi_keyval_free_f pompi_keyval_free_f
#define ompi_lookup_name_f pompi_lookup_name_f
#define ompi_mprobe_f pompi_mprobe_f
#define ompi_mrecv_f pompi_mrecv_f
#define ompi_neighbor_allgather_f pompi_neighbor_allgather_f
#define ompi_neighbor_allgatherv_f pompi_neighbor_allgatherv_f
#define ompi_neighbor_alltoall_f pompi_neighbor_alltoall_f
#define ompi_neighbor_alltoallv_f pompi_neighbor_alltoallv_f
#define ompi_neighbor_alltoallw_f pompi_neighbor_alltoallw_f
#define ompi_op_commutative_f pompi_op_commutative_f
#define ompi_op_create_f pompi_op_create_f
#define ompi_open_port_f pompi_open_port_f
#define ompi_op_free_f pompi_op_free_f
#define ompi_pack_external_f pompi_pack_external_f
#define ompi_pack_external_size_f pompi_pack_external_size_f
#define ompi_pack_f pompi_pack_f
#define ompi_pack_size_f pompi_pack_size_f
#define ompi_pcontrol_f pompi_pcontrol_f
#define ompi_probe_f pompi_probe_f
#define ompi_publish_name_f pompi_publish_name_f
#define ompi_put_f pompi_put_f
#define ompi_query_thread_f pompi_query_thread_f
#define ompi_raccumulate_f pompi_raccumulate_f
#define ompi_recv_init_f pompi_recv_init_f
#define ompi_recv_f pompi_recv_f
#define ompi_reduce_f pompi_reduce_f
#define ompi_reduce_local_f pompi_reduce_local_f
#define ompi_reduce_scatter_f pompi_reduce_scatter_f
#define ompi_reduce_scatter_block_f pompi_reduce_scatter_block_f
#define ompi_register_datarep_f pompi_register_datarep_f
#define ompi_request_free_f pompi_request_free_f
#define ompi_request_get_status_f pompi_request_get_status_f
#define ompi_rget_f pompi_rget_f
#define ompi_rget_accumulate_f pompi_rget_accumulate_f
#define ompi_rput_f pompi_rput_f
#define ompi_rsend_f pompi_rsend_f
#define ompi_rsend_init_f pompi_rsend_init_f
#define ompi_scan_f pompi_scan_f
#define ompi_scatter_f pompi_scatter_f
#define ompi_scatterv_f pompi_scatterv_f
#define ompi_send_init_f pompi_send_init_f
#define ompi_send_f pompi_send_f
#define ompi_sendrecv_f pompi_sendrecv_f
#define ompi_sendrecv_replace_f pompi_sendrecv_replace_f
#define ompi_ssend_init_f pompi_ssend_init_f
#define ompi_ssend_f pompi_ssend_f
#define ompi_start_f pompi_start_f
#define ompi_startall_f pompi_startall_f
#define ompi_status_set_cancelled_f pompi_status_set_cancelled_f
#define ompi_status_set_elements_f pompi_status_set_elements_f
#define ompi_status_set_elements_x_f pompi_status_set_elements_x_f
#define ompi_testall_f pompi_testall_f
#define ompi_testany_f pompi_testany_f
#define ompi_test_f pompi_test_f
#define ompi_test_cancelled_f pompi_test_cancelled_f
#define ompi_testsome_f pompi_testsome_f
#define ompi_topo_test_f pompi_topo_test_f
#define ompi_type_commit_f pompi_type_commit_f
#define ompi_type_contiguous_f pompi_type_contiguous_f
#define ompi_type_create_darray_f pompi_type_create_darray_f
#define ompi_type_create_f90_complex_f pompi_type_create_f90_complex_f
#define ompi_type_create_f90_integer_f pompi_type_create_f90_integer_f
#define ompi_type_create_f90_real_f pompi_type_create_f90_real_f
#define ompi_type_create_hindexed_f pompi_type_create_hindexed_f
#define ompi_type_create_hvector_f pompi_type_create_hvector_f
#define ompi_type_create_keyval_f pompi_type_create_keyval_f
#define ompi_type_create_indexed_block_f pompi_type_create_indexed_block_f
#define ompi_type_create_hindexed_block_f pompi_type_create_hindexed_block_f
#define ompi_type_create_struct_f pompi_type_create_struct_f
#define ompi_type_create_subarray_f pompi_type_create_subarray_f
#define ompi_type_create_resized_f pompi_type_create_resized_f
#define ompi_type_delete_attr_f pompi_type_delete_attr_f
#define ompi_type_dup_f pompi_type_dup_f
#define ompi_type_extent_f pompi_type_extent_f
#define ompi_type_free_f pompi_type_free_f
#define ompi_type_free_keyval_f pompi_type_free_keyval_f
#define ompi_type_get_attr_f pompi_type_get_attr_f
#define ompi_type_get_contents_f pompi_type_get_contents_f
#define ompi_type_get_envelope_f pompi_type_get_envelope_f
#define ompi_type_get_extent_f pompi_type_get_extent_f
#define ompi_type_get_extent_x_f pompi_type_get_extent_x_f
#define ompi_type_get_name_f pompi_type_get_name_f
#define ompi_type_get_true_extent_f pompi_type_get_true_extent_f
#define ompi_type_get_true_extent_x_f pompi_type_get_true_extent_x_f
#define ompi_type_hindexed_f pompi_type_hindexed_f
#define ompi_type_hvector_f pompi_type_hvector_f
#define ompi_type_indexed_f pompi_type_indexed_f
#define ompi_type_lb_f pompi_type_lb_f
#define ompi_type_match_size_f pompi_type_match_size_f
#define ompi_type_set_attr_f pompi_type_set_attr_f
#define ompi_type_set_name_f pompi_type_set_name_f
#define ompi_type_size_f pompi_type_size_f
#define ompi_type_size_x_f pompi_type_size_x_f
#define ompi_type_struct_f pompi_type_struct_f
#define ompi_type_ub_f pompi_type_ub_f
#define ompi_type_vector_f pompi_type_vector_f
#define ompi_unpack_f pompi_unpack_f
#define ompi_unpublish_name_f pompi_unpublish_name_f
#define ompi_unpack_external_f pompi_unpack_external_f
#define ompi_waitall_f pompi_waitall_f
#define ompi_waitany_f pompi_waitany_f
#define ompi_wait_f pompi_wait_f
#define ompi_waitsome_f pompi_waitsome_f
#define ompi_win_allocate_f pompi_win_allocate_f
#define ompi_win_allocate_cptr_f pompi_win_allocate_cptr_f
#define ompi_win_allocate_shared_f pompi_win_allocate_shared_f
#define ompi_win_allocate_shared_cptr_f pompi_win_allocate_shared_cptr_f
#define ompi_win_call_errhandler_f pompi_win_call_errhandler_f
#define ompi_win_complete_f pompi_win_complete_f
#define ompi_win_create_f pompi_win_create_f
#define ompi_win_create_dynamic_f pompi_win_create_dynamic_f
#define ompi_win_create_errhandler_f pompi_win_create_errhandler_f
#define ompi_win_create_keyval_f pompi_win_create_keyval_f
#define ompi_win_delete_attr_f pompi_win_delete_attr_f
#define ompi_win_fence_f pompi_win_fence_f
#define ompi_win_flush_f pompi_win_flush_f
#define ompi_win_flush_all_f pompi_win_flush_all_f
#define ompi_win_flush_local_f pompi_win_flush_local_f
#define ompi_win_flush_local_all_f pompi_win_flush_local_all_f
#define ompi_win_free_f pompi_win_free_f
#define ompi_win_free_keyval_f pompi_win_free_keyval_f
#define ompi_win_get_attr_f pompi_win_get_attr_f
#define ompi_win_get_errhandler_f pompi_win_get_errhandler_f
#define ompi_win_get_group_f pompi_win_get_group_f
#define ompi_win_get_name_f pompi_win_get_name_f
#define ompi_win_lock_f pompi_win_lock_f
#define ompi_win_lock_all_f pompi_win_lock_all_f
#define ompi_win_post_f pompi_win_post_f
#define ompi_win_set_attr_f pompi_win_set_attr_f
#define ompi_win_set_errhandler_f pompi_win_set_errhandler_f
#define ompi_win_set_name_f pompi_win_set_name_f
#define ompi_win_shared_query_f pompi_win_shared_query_f
#define ompi_win_shared_query_cptr_f pompi_win_shared_query_cptr_f
#define ompi_win_start_f pompi_win_start_f
#define ompi_win_sync_f pompi_win_sync_f
#define ompi_win_test_f pompi_win_test_f
#define ompi_win_unlock_f pompi_win_unlock_f
#define ompi_win_unlock_all_f pompi_win_unlock_all_f
#define ompi_win_wait_f pompi_win_wait_f
#define ompi_wtick_f pompi_wtick_f
#define ompi_wtime_f pompi_wtime_f
#endif

