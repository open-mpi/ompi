! -*- f90 -*-
!
! Copyright (c) 2019-2020 Research Organization for Information Science
!                         and Technology (RIST).  All rights reserved.

#if OMPI_BUILD_MPI_PROFILING

#define MPI_Abort_f08 PMPI_Abort_f08
#define MPI_Abort PMPI_Abort
#define MPI_Accumulate_f08 PMPI_Accumulate_f08
#define MPI_Accumulate PMPI_Accumulate
#define MPI_Add_error_class_f08 PMPI_Add_error_class_f08
#define MPI_Add_error_class PMPI_Add_error_class
#define MPI_Add_error_code_f08 PMPI_Add_error_code_f08
#define MPI_Add_error_code PMPI_Add_error_code
#define MPI_Add_error_string_f08 PMPI_Add_error_string_f08
#define MPI_Add_error_string PMPI_Add_error_string
#define MPI_Aint_add_f08 PMPI_Aint_add_f08
#define MPI_Aint_add PMPI_Aint_add
#define MPI_Aint_diff_f08 PMPI_Aint_diff_f08
#define MPI_Aint_diff PMPI_Aint_diff
#define MPI_Allgather_f08 PMPI_Allgather_f08
#define MPI_Allgather_init_f08 PMPI_Allgather_init_f08
#define MPI_Allgather_init PMPI_Allgather_init
#define MPI_Allgather PMPI_Allgather
#define MPI_Allgatherv_f08 PMPI_Allgatherv_f08
#define MPI_Allgatherv_init_f08 PMPI_Allgatherv_init_f08
#define MPI_Allgatherv_init PMPI_Allgatherv_init
#define MPI_Allgatherv PMPI_Allgatherv
#define MPI_Alloc_mem_f08 PMPI_Alloc_mem_f08
#define MPI_Alloc_mem PMPI_Alloc_mem
#define MPI_Allreduce_f08 PMPI_Allreduce_f08
#define MPI_Allreduce_init_f08 PMPI_Allreduce_init_f08
#define MPI_Allreduce_init PMPI_Allreduce_init
#define MPI_Allreduce PMPI_Allreduce
#define MPI_Alltoall_f08 PMPI_Alltoall_f08
#define MPI_Alltoall_init_f08 PMPI_Alltoall_init_f08
#define MPI_Alltoall_init PMPI_Alltoall_init
#define MPI_Alltoall PMPI_Alltoall
#define MPI_Alltoallv_f08 PMPI_Alltoallv_f08
#define MPI_Alltoallv_init_f08 PMPI_Alltoallv_init_f08
#define MPI_Alltoallv_init PMPI_Alltoallv_init
#define MPI_Alltoallv PMPI_Alltoallv
#define MPI_Alltoallw_f08 PMPI_Alltoallw_f08
#define MPI_Alltoallw_init_f08 PMPI_Alltoallw_init_f08
#define MPI_Alltoallw_init PMPI_Alltoallw_init
#define MPI_Alltoallw PMPI_Alltoallw
#define MPI_Barrier_f08 PMPI_Barrier_f08
#define MPI_Barrier_init_f08 PMPI_Barrier_init_f08
#define MPI_Barrier_init PMPI_Barrier_init
#define MPI_Barrier PMPI_Barrier
#define MPI_Bcast_f08 PMPI_Bcast_f08
#define MPI_Bcast_init_f08 PMPI_Bcast_init_f08
#define MPI_Bcast_init PMPI_Bcast_init
#define MPI_Bcast PMPI_Bcast
#define MPI_Bsend_f08 PMPI_Bsend_f08
#define MPI_Bsend_init_f08 PMPI_Bsend_init_f08
#define MPI_Bsend_init PMPI_Bsend_init
#define MPI_Bsend PMPI_Bsend
#define MPI_Buffer_attach_f08 PMPI_Buffer_attach_f08
#define MPI_Buffer_attach PMPI_Buffer_attach
#define MPI_Buffer_detach_f08 PMPI_Buffer_detach_f08
#define MPI_Buffer_detach PMPI_Buffer_detach
#define MPI_Cancel_f08 PMPI_Cancel_f08
#define MPI_Cancel PMPI_Cancel
#define MPI_Cart_coords_f08 PMPI_Cart_coords_f08
#define MPI_Cart_coords PMPI_Cart_coords
#define MPI_Cart_create_f08 PMPI_Cart_create_f08
#define MPI_Cart_create PMPI_Cart_create
#define MPI_Cartdim_get_f08 PMPI_Cartdim_get_f08
#define MPI_Cartdim_get PMPI_Cartdim_get
#define MPI_Cart_get_f08 PMPI_Cart_get_f08
#define MPI_Cart_get PMPI_Cart_get
#define MPI_Cart_map_f08 PMPI_Cart_map_f08
#define MPI_Cart_map PMPI_Cart_map
#define MPI_Cart_rank_f08 PMPI_Cart_rank_f08
#define MPI_Cart_rank PMPI_Cart_rank
#define MPI_Cart_shift_f08 PMPI_Cart_shift_f08
#define MPI_Cart_shift PMPI_Cart_shift
#define MPI_Cart_sub_f08 PMPI_Cart_sub_f08
#define MPI_Cart_sub PMPI_Cart_sub
#define MPI_Close_port_f08 PMPI_Close_port_f08
#define MPI_Close_port PMPI_Close_port
#define MPI_Comm_accept_f08 PMPI_Comm_accept_f08
#define MPI_Comm_accept PMPI_Comm_accept
#define MPI_Comm_call_errhandler_f08 PMPI_Comm_call_errhandler_f08
#define MPI_Comm_call_errhandler PMPI_Comm_call_errhandler
#define MPI_Comm_compare_f08 PMPI_Comm_compare_f08
#define MPI_Comm_compare PMPI_Comm_compare
#define MPI_Comm_connect_f08 PMPI_Comm_connect_f08
#define MPI_Comm_connect PMPI_Comm_connect
#define MPI_Comm_create_errhandler_f08 PMPI_Comm_create_errhandler_f08
#define MPI_Comm_create_errhandler PMPI_Comm_create_errhandler
#define MPI_Comm_create_f08 PMPI_Comm_create_f08
#define MPI_Comm_create_from_group_f08 PMPI_Comm_create_from_group_f08
#define MPI_Comm_create_from_group PMPI_Comm_create_from_group
#define MPI_Comm_create_group_f08 PMPI_Comm_create_group_f08
#define MPI_Comm_create_group PMPI_Comm_create_group
#define MPI_Comm_create_keyval_f08 PMPI_Comm_create_keyval_f08
#define MPI_Comm_create_keyval PMPI_Comm_create_keyval
#define MPI_Comm_create PMPI_Comm_create
#define MPI_Comm_delete_attr_f08 PMPI_Comm_delete_attr_f08
#define MPI_Comm_delete_attr PMPI_Comm_delete_attr
#define MPI_Comm_disconnect_f08 PMPI_Comm_disconnect_f08
#define MPI_Comm_disconnect PMPI_Comm_disconnect
#define MPI_Comm_dup_f08 PMPI_Comm_dup_f08
#define MPI_Comm_dup PMPI_Comm_dup
#define MPI_Comm_dup_with_info_f08 PMPI_Comm_dup_with_info_f08
#define MPI_Comm_dup_with_info PMPI_Comm_dup_with_info
#define MPI_Comm_free_f08 PMPI_Comm_free_f08
#define MPI_Comm_free_keyval_f08 PMPI_Comm_free_keyval_f08
#define MPI_Comm_free_keyval PMPI_Comm_free_keyval
#define MPI_Comm_free PMPI_Comm_free
#define MPI_Comm_get_attr_f08 PMPI_Comm_get_attr_f08
#define MPI_Comm_get_attr PMPI_Comm_get_attr
#define MPI_Comm_get_errhandler_f08 PMPI_Comm_get_errhandler_f08
#define MPI_Comm_get_errhandler PMPI_Comm_get_errhandler
#define MPI_Comm_get_info_f08 PMPI_Comm_get_info_f08
#define MPI_Comm_get_info PMPI_Comm_get_info
#define MPI_Comm_get_name_f08 PMPI_Comm_get_name_f08
#define MPI_Comm_get_name PMPI_Comm_get_name
#define MPI_Comm_get_parent_f08 PMPI_Comm_get_parent_f08
#define MPI_Comm_get_parent PMPI_Comm_get_parent
#define MPI_Comm_group_f08 PMPI_Comm_group_f08
#define MPI_Comm_group PMPI_Comm_group
#define MPI_Comm_idup_f08 PMPI_Comm_idup_f08
#define MPI_Comm_idup PMPI_Comm_idup
#define MPI_Comm_idup_with_info_f08 PMPI_Comm_idup_with_info_f08
#define MPI_Comm_idup_with_info PMPI_Comm_idup_with_info
#define MPI_Comm_join_f08 PMPI_Comm_join_f08
#define MPI_Comm_join PMPI_Comm_join
#define MPI_Comm_rank_f08 PMPI_Comm_rank_f08
#define MPI_Comm_rank PMPI_Comm_rank
#define MPI_Comm_remote_group_f08 PMPI_Comm_remote_group_f08
#define MPI_Comm_remote_group PMPI_Comm_remote_group
#define MPI_Comm_remote_size_f08 PMPI_Comm_remote_size_f08
#define MPI_Comm_remote_size PMPI_Comm_remote_size
#define MPI_Comm_set_attr_f08 PMPI_Comm_set_attr_f08
#define MPI_Comm_set_attr PMPI_Comm_set_attr
#define MPI_Comm_set_errhandler_f08 PMPI_Comm_set_errhandler_f08
#define MPI_Comm_set_errhandler PMPI_Comm_set_errhandler
#define MPI_Comm_set_info_f08 PMPI_Comm_set_info_f08
#define MPI_Comm_set_info PMPI_Comm_set_info
#define MPI_Comm_set_name_f08 PMPI_Comm_set_name_f08
#define MPI_Comm_set_name PMPI_Comm_set_name
#define MPI_Comm_size_f08 PMPI_Comm_size_f08
#define MPI_Comm_size PMPI_Comm_size
#define MPI_Comm_spawn_f08 PMPI_Comm_spawn_f08
#define MPI_Comm_spawn_multiple_f08 PMPI_Comm_spawn_multiple_f08
#define MPI_Comm_spawn_multiple PMPI_Comm_spawn_multiple
#define MPI_Comm_spawn PMPI_Comm_spawn
#define MPI_Comm_split_f08 PMPI_Comm_split_f08
#define MPI_Comm_split PMPI_Comm_split
#define MPI_Comm_split_type_f08 PMPI_Comm_split_type_f08
#define MPI_Comm_split_type PMPI_Comm_split_type
#define MPI_Comm_test_inter_f08 PMPI_Comm_test_inter_f08
#define MPI_Comm_test_inter PMPI_Comm_test_inter
#define MPI_Compare_and_swap_f08 PMPI_Compare_and_swap_f08
#define MPI_Compare_and_swap PMPI_Compare_and_swap
#define MPI_Dims_create_f08 PMPI_Dims_create_f08
#define MPI_Dims_create PMPI_Dims_create
#define MPI_Dist_graph_create_adjacent_f08 PMPI_Dist_graph_create_adjacent_f08
#define MPI_Dist_graph_create_adjacent PMPI_Dist_graph_create_adjacent
#define MPI_Dist_graph_create_f08 PMPI_Dist_graph_create_f08
#define MPI_Dist_graph_create PMPI_Dist_graph_create
#define MPI_Dist_graph_neighbors_count_f08 PMPI_Dist_graph_neighbors_count_f08
#define MPI_Dist_graph_neighbors_count PMPI_Dist_graph_neighbors_count
#define MPI_Dist_graph_neighbors_f08 PMPI_Dist_graph_neighbors_f08
#define MPI_Dist_graph_neighbors PMPI_Dist_graph_neighbors
#define MPI_Errhandler_free_f08 PMPI_Errhandler_free_f08
#define MPI_Errhandler_free PMPI_Errhandler_free
#define MPI_Error_class_f08 PMPI_Error_class_f08
#define MPI_Error_class PMPI_Error_class
#define MPI_Error_string_f08 PMPI_Error_string_f08
#define MPI_Error_string PMPI_Error_string
#define MPI_Exscan_f08 PMPI_Exscan_f08
#define MPI_Exscan_init_f08 PMPI_Exscan_init_f08
#define MPI_Exscan_init PMPI_Exscan_init
#define MPI_Exscan PMPI_Exscan
#define MPI_Fetch_and_op_f08 PMPI_Fetch_and_op_f08
#define MPI_Fetch_and_op PMPI_Fetch_and_op
#define MPI_File_call_errhandler_f08 PMPI_File_call_errhandler_f08
#define MPI_File_call_errhandler PMPI_File_call_errhandler
#define MPI_File_close_f08 PMPI_File_close_f08
#define MPI_File_close PMPI_File_close
#define MPI_File_create_errhandler_f08 PMPI_File_create_errhandler_f08
#define MPI_File_create_errhandler PMPI_File_create_errhandler
#define MPI_File_delete_f08 PMPI_File_delete_f08
#define MPI_File_delete PMPI_File_delete
#define MPI_File_get_amode_f08 PMPI_File_get_amode_f08
#define MPI_File_get_amode PMPI_File_get_amode
#define MPI_File_get_atomicity_f08 PMPI_File_get_atomicity_f08
#define MPI_File_get_atomicity PMPI_File_get_atomicity
#define MPI_File_get_byte_offset_f08 PMPI_File_get_byte_offset_f08
#define MPI_File_get_byte_offset PMPI_File_get_byte_offset
#define MPI_File_get_errhandler_f08 PMPI_File_get_errhandler_f08
#define MPI_File_get_errhandler PMPI_File_get_errhandler
#define MPI_File_get_group_f08 PMPI_File_get_group_f08
#define MPI_File_get_group PMPI_File_get_group
#define MPI_File_get_info_f08 PMPI_File_get_info_f08
#define MPI_File_get_info PMPI_File_get_info
#define MPI_File_get_position_f08 PMPI_File_get_position_f08
#define MPI_File_get_position PMPI_File_get_position
#define MPI_File_get_position_shared_f08 PMPI_File_get_position_shared_f08
#define MPI_File_get_position_shared PMPI_File_get_position_shared
#define MPI_File_get_size_f08 PMPI_File_get_size_f08
#define MPI_File_get_size PMPI_File_get_size
#define MPI_File_get_type_extent_f08 PMPI_File_get_type_extent_f08
#define MPI_File_get_type_extent PMPI_File_get_type_extent
#define MPI_File_get_view_f08 PMPI_File_get_view_f08
#define MPI_File_get_view PMPI_File_get_view
#define MPI_File_iread_all_f08 PMPI_File_iread_all_f08
#define MPI_File_iread_all PMPI_File_iread_all
#define MPI_File_iread_at_all_f08 PMPI_File_iread_at_all_f08
#define MPI_File_iread_at_all PMPI_File_iread_at_all
#define MPI_File_iread_at_f08 PMPI_File_iread_at_f08
#define MPI_File_iread_at PMPI_File_iread_at
#define MPI_File_iread_f08 PMPI_File_iread_f08
#define MPI_File_iread PMPI_File_iread
#define MPI_File_iread_shared_f08 PMPI_File_iread_shared_f08
#define MPI_File_iread_shared PMPI_File_iread_shared
#define MPI_File_iwrite_all_f08 PMPI_File_iwrite_all_f08
#define MPI_File_iwrite_all PMPI_File_iwrite_all
#define MPI_File_iwrite_at_all_f08 PMPI_File_iwrite_at_all_f08
#define MPI_File_iwrite_at_all PMPI_File_iwrite_at_all
#define MPI_File_iwrite_at_f08 PMPI_File_iwrite_at_f08
#define MPI_File_iwrite_at PMPI_File_iwrite_at
#define MPI_File_iwrite_f08 PMPI_File_iwrite_f08
#define MPI_File_iwrite PMPI_File_iwrite
#define MPI_File_iwrite_shared_f08 PMPI_File_iwrite_shared_f08
#define MPI_File_iwrite_shared PMPI_File_iwrite_shared
#define MPI_File_open_f08 PMPI_File_open_f08
#define MPI_File_open PMPI_File_open
#define MPI_File_preallocate_f08 PMPI_File_preallocate_f08
#define MPI_File_preallocate PMPI_File_preallocate
#define MPI_File_read_all_begin_f08 PMPI_File_read_all_begin_f08
#define MPI_File_read_all_begin PMPI_File_read_all_begin
#define MPI_File_read_all_end_f08 PMPI_File_read_all_end_f08
#define MPI_File_read_all_end PMPI_File_read_all_end
#define MPI_File_read_all_f08 PMPI_File_read_all_f08
#define MPI_File_read_all PMPI_File_read_all
#define MPI_File_read_at_all_begin_f08 PMPI_File_read_at_all_begin_f08
#define MPI_File_read_at_all_begin PMPI_File_read_at_all_begin
#define MPI_File_read_at_all_end_f08 PMPI_File_read_at_all_end_f08
#define MPI_File_read_at_all_end PMPI_File_read_at_all_end
#define MPI_File_read_at_all_f08 PMPI_File_read_at_all_f08
#define MPI_File_read_at_all PMPI_File_read_at_all
#define MPI_File_read_at_f08 PMPI_File_read_at_f08
#define MPI_File_read_at PMPI_File_read_at
#define MPI_File_read_f08 PMPI_File_read_f08
#define MPI_File_read_ordered_begin_f08 PMPI_File_read_ordered_begin_f08
#define MPI_File_read_ordered_begin PMPI_File_read_ordered_begin
#define MPI_File_read_ordered_end_f08 PMPI_File_read_ordered_end_f08
#define MPI_File_read_ordered_end PMPI_File_read_ordered_end
#define MPI_File_read_ordered_f08 PMPI_File_read_ordered_f08
#define MPI_File_read_ordered PMPI_File_read_ordered
#define MPI_File_read PMPI_File_read
#define MPI_File_read_shared_f08 PMPI_File_read_shared_f08
#define MPI_File_read_shared PMPI_File_read_shared
#define MPI_File_seek_f08 PMPI_File_seek_f08
#define MPI_File_seek PMPI_File_seek
#define MPI_File_seek_shared_f08 PMPI_File_seek_shared_f08
#define MPI_File_seek_shared PMPI_File_seek_shared
#define MPI_File_set_atomicity_f08 PMPI_File_set_atomicity_f08
#define MPI_File_set_atomicity PMPI_File_set_atomicity
#define MPI_File_set_errhandler_f08 PMPI_File_set_errhandler_f08
#define MPI_File_set_errhandler PMPI_File_set_errhandler
#define MPI_File_set_info_f08 PMPI_File_set_info_f08
#define MPI_File_set_info PMPI_File_set_info
#define MPI_File_set_size_f08 PMPI_File_set_size_f08
#define MPI_File_set_size PMPI_File_set_size
#define MPI_File_set_view_f08 PMPI_File_set_view_f08
#define MPI_File_set_view PMPI_File_set_view
#define MPI_File_sync_f08 PMPI_File_sync_f08
#define MPI_File_sync PMPI_File_sync
#define MPI_File_write_all_begin_f08 PMPI_File_write_all_begin_f08
#define MPI_File_write_all_begin PMPI_File_write_all_begin
#define MPI_File_write_all_end_f08 PMPI_File_write_all_end_f08
#define MPI_File_write_all_end PMPI_File_write_all_end
#define MPI_File_write_all_f08 PMPI_File_write_all_f08
#define MPI_File_write_all PMPI_File_write_all
#define MPI_File_write_at_all_begin_f08 PMPI_File_write_at_all_begin_f08
#define MPI_File_write_at_all_begin PMPI_File_write_at_all_begin
#define MPI_File_write_at_all_end_f08 PMPI_File_write_at_all_end_f08
#define MPI_File_write_at_all_end PMPI_File_write_at_all_end
#define MPI_File_write_at_all_f08 PMPI_File_write_at_all_f08
#define MPI_File_write_at_all PMPI_File_write_at_all
#define MPI_File_write_at_f08 PMPI_File_write_at_f08
#define MPI_File_write_at PMPI_File_write_at
#define MPI_File_write_f08 PMPI_File_write_f08
#define MPI_File_write_ordered_begin_f08 PMPI_File_write_ordered_begin_f08
#define MPI_File_write_ordered_begin PMPI_File_write_ordered_begin
#define MPI_File_write_ordered_end_f08 PMPI_File_write_ordered_end_f08
#define MPI_File_write_ordered_end PMPI_File_write_ordered_end
#define MPI_File_write_ordered_f08 PMPI_File_write_ordered_f08
#define MPI_File_write_ordered PMPI_File_write_ordered
#define MPI_File_write PMPI_File_write
#define MPI_File_write_shared_f08 PMPI_File_write_shared_f08
#define MPI_File_write_shared PMPI_File_write_shared
#define MPI_Finalized_f08 PMPI_Finalized_f08
#define MPI_Finalized PMPI_Finalized
#define MPI_Finalize_f08 PMPI_Finalize_f08
#define MPI_Finalize PMPI_Finalize
#define MPI_Free_mem_f08 PMPI_Free_mem_f08
#define MPI_Free_mem PMPI_Free_mem
#define MPI_F_sync_reg_f08 PMPI_F_sync_reg_f08
#define MPI_F_sync_reg PMPI_F_sync_reg
#define MPI_Gather_f08 PMPI_Gather_f08
#define MPI_Gather_init_f08 PMPI_Gather_init_f08
#define MPI_Gather_init PMPI_Gather_init
#define MPI_Gather PMPI_Gather
#define MPI_Gatherv_f08 PMPI_Gatherv_f08
#define MPI_Gatherv_init_f08 PMPI_Gatherv_init_f08
#define MPI_Gatherv_init PMPI_Gatherv_init
#define MPI_Gatherv PMPI_Gatherv
#define MPI_Get_accumulate_f08 PMPI_Get_accumulate_f08
#define MPI_Get_accumulate PMPI_Get_accumulate
#define MPI_Get_address_f08 PMPI_Get_address_f08
#define MPI_Get_address PMPI_Get_address
#define MPI_Get_count_f08 PMPI_Get_count_f08
#define MPI_Get_count PMPI_Get_count
#define MPI_Get_elements_f08 PMPI_Get_elements_f08
#define MPI_Get_elements PMPI_Get_elements
#define MPI_Get_elements_x_f08 PMPI_Get_elements_x_f08
#define MPI_Get_elements_x PMPI_Get_elements_x
#define MPI_Get_f08 PMPI_Get_f08
#define MPI_Get_library_version_f08 PMPI_Get_library_version_f08
#define MPI_Get_library_version PMPI_Get_library_version
#define MPI_Get PMPI_Get
#define MPI_Get_processor_name_f08 PMPI_Get_processor_name_f08
#define MPI_Get_processor_name PMPI_Get_processor_name
#define MPI_Get_version_f08 PMPI_Get_version_f08
#define MPI_Get_version PMPI_Get_version
#define MPI_Graph_create_f08 PMPI_Graph_create_f08
#define MPI_Graph_create PMPI_Graph_create
#define MPI_Graphdims_get_f08 PMPI_Graphdims_get_f08
#define MPI_Graphdims_get PMPI_Graphdims_get
#define MPI_Graph_get_f08 PMPI_Graph_get_f08
#define MPI_Graph_get PMPI_Graph_get
#define MPI_Graph_map_f08 PMPI_Graph_map_f08
#define MPI_Graph_map PMPI_Graph_map
#define MPI_Graph_neighbors_count_f08 PMPI_Graph_neighbors_count_f08
#define MPI_Graph_neighbors_count PMPI_Graph_neighbors_count
#define MPI_Graph_neighbors_f08 PMPI_Graph_neighbors_f08
#define MPI_Graph_neighbors PMPI_Graph_neighbors
#define MPI_Grequest_complete_f08 PMPI_Grequest_complete_f08
#define MPI_Grequest_complete PMPI_Grequest_complete
#define MPI_Grequest_start_f08 PMPI_Grequest_start_f08
#define MPI_Grequest_start PMPI_Grequest_start
#define MPI_Group_compare_f08 PMPI_Group_compare_f08
#define MPI_Group_compare PMPI_Group_compare
#define MPI_Group_difference_f08 PMPI_Group_difference_f08
#define MPI_Group_difference PMPI_Group_difference
#define MPI_Group_excl_f08 PMPI_Group_excl_f08
#define MPI_Group_excl PMPI_Group_excl
#define MPI_Group_free_f08 PMPI_Group_free_f08
#define MPI_Group_free PMPI_Group_free
#define MPI_Group_from_session_pset_f08 PMPI_Group_from_session_pset_f08
#define MPI_Group_from_session_pset PMPI_Group_from_session_pset
#define MPI_Group_incl_f08 PMPI_Group_incl_f08
#define MPI_Group_incl PMPI_Group_incl
#define MPI_Group_intersection_f08 PMPI_Group_intersection_f08
#define MPI_Group_intersection PMPI_Group_intersection
#define MPI_Group_range_excl_f08 PMPI_Group_range_excl_f08
#define MPI_Group_range_excl PMPI_Group_range_excl
#define MPI_Group_range_incl_f08 PMPI_Group_range_incl_f08
#define MPI_Group_range_incl PMPI_Group_range_incl
#define MPI_Group_rank_f08 PMPI_Group_rank_f08
#define MPI_Group_rank PMPI_Group_rank
#define MPI_Group_size_f08 PMPI_Group_size_f08
#define MPI_Group_size PMPI_Group_size
#define MPI_Group_translate_ranks_f08 PMPI_Group_translate_ranks_f08
#define MPI_Group_translate_ranks PMPI_Group_translate_ranks
#define MPI_Group_union_f08 PMPI_Group_union_f08
#define MPI_Group_union PMPI_Group_union
#define MPI_Iallgather_f08 PMPI_Iallgather_f08
#define MPI_Iallgather PMPI_Iallgather
#define MPI_Iallgatherv_f08 PMPI_Iallgatherv_f08
#define MPI_Iallgatherv PMPI_Iallgatherv
#define MPI_Iallreduce_f08 PMPI_Iallreduce_f08
#define MPI_Iallreduce PMPI_Iallreduce
#define MPI_Ialltoall_f08 PMPI_Ialltoall_f08
#define MPI_Ialltoall PMPI_Ialltoall
#define MPI_Ialltoallv_f08 PMPI_Ialltoallv_f08
#define MPI_Ialltoallv PMPI_Ialltoallv
#define MPI_Ialltoallw_f08 PMPI_Ialltoallw_f08
#define MPI_Ialltoallw PMPI_Ialltoallw
#define MPI_Ibarrier_f08 PMPI_Ibarrier_f08
#define MPI_Ibarrier PMPI_Ibarrier
#define MPI_Ibcast_f08 PMPI_Ibcast_f08
#define MPI_Ibcast PMPI_Ibcast
#define MPI_Ibsend_f08 PMPI_Ibsend_f08
#define MPI_Ibsend PMPI_Ibsend
#define MPI_Iexscan_f08 PMPI_Iexscan_f08
#define MPI_Iexscan PMPI_Iexscan
#define MPI_Igather_f08 PMPI_Igather_f08
#define MPI_Igather PMPI_Igather
#define MPI_Igatherv_f08 PMPI_Igatherv_f08
#define MPI_Igatherv PMPI_Igatherv
#define MPI_Improbe_f08 PMPI_Improbe_f08
#define MPI_Improbe PMPI_Improbe
#define MPI_Imrecv_f08 PMPI_Imrecv_f08
#define MPI_Imrecv PMPI_Imrecv
#define MPI_Ineighbor_allgather_f08 PMPI_Ineighbor_allgather_f08
#define MPI_Ineighbor_allgather PMPI_Ineighbor_allgather
#define MPI_Ineighbor_allgatherv_f08 PMPI_Ineighbor_allgatherv_f08
#define MPI_Ineighbor_allgatherv PMPI_Ineighbor_allgatherv
#define MPI_Ineighbor_alltoall_f08 PMPI_Ineighbor_alltoall_f08
#define MPI_Ineighbor_alltoall PMPI_Ineighbor_alltoall
#define MPI_Ineighbor_alltoallv_f08 PMPI_Ineighbor_alltoallv_f08
#define MPI_Ineighbor_alltoallv PMPI_Ineighbor_alltoallv
#define MPI_Ineighbor_alltoallw_f08 PMPI_Ineighbor_alltoallw_f08
#define MPI_Ineighbor_alltoallw PMPI_Ineighbor_alltoallw
#define MPI_Info_create_env_f08 PMPI_Info_create_env_f08
#define MPI_Info_create_env PMPI_Info_create_env
#define MPI_Info_create_f08 PMPI_Info_create_f08
#define MPI_Info_create PMPI_Info_create
#define MPI_Info_delete_f08 PMPI_Info_delete_f08
#define MPI_Info_delete PMPI_Info_delete
#define MPI_Info_dup_f08 PMPI_Info_dup_f08
#define MPI_Info_dup PMPI_Info_dup
#define MPI_Info_free_f08 PMPI_Info_free_f08
#define MPI_Info_free PMPI_Info_free
#define MPI_Info_get_f08 PMPI_Info_get_f08
#define MPI_Info_get_nkeys_f08 PMPI_Info_get_nkeys_f08
#define MPI_Info_get_nkeys PMPI_Info_get_nkeys
#define MPI_Info_get_nthkey_f08 PMPI_Info_get_nthkey_f08
#define MPI_Info_get_nthkey PMPI_Info_get_nthkey
#define MPI_Info_get PMPI_Info_get
#define MPI_Info_get_string_f08 PMPI_Info_get_string_f08
#define MPI_Info_get_string PMPI_Info_get_string
#define MPI_Info_get_valuelen_f08 PMPI_Info_get_valuelen_f08
#define MPI_Info_get_valuelen PMPI_Info_get_valuelen
#define MPI_Info_set_f08 PMPI_Info_set_f08
#define MPI_Info_set PMPI_Info_set
#define MPI_Init_f08 PMPI_Init_f08
#define MPI_Initialized_f08 PMPI_Initialized_f08
#define MPI_Initialized PMPI_Initialized
#define MPI_Init PMPI_Init
#define MPI_Init_thread_f08 PMPI_Init_thread_f08
#define MPI_Init_thread PMPI_Init_thread
#define MPI_Intercomm_create_f08 PMPI_Intercomm_create_f08
#define MPI_Intercomm_create_from_groups_f08 PMPI_Intercomm_create_from_groups_f08
#define MPI_Intercomm_create_from_groups PMPI_Intercomm_create_from_groups
#define MPI_Intercomm_create PMPI_Intercomm_create
#define MPI_Intercomm_merge_f08 PMPI_Intercomm_merge_f08
#define MPI_Intercomm_merge PMPI_Intercomm_merge
#define MPI_Iprobe_f08 PMPI_Iprobe_f08
#define MPI_Iprobe PMPI_Iprobe
#define MPI_Irecv_f08 PMPI_Irecv_f08
#define MPI_Irecv PMPI_Irecv
#define MPI_Ireduce_f08 PMPI_Ireduce_f08
#define MPI_Ireduce PMPI_Ireduce
#define MPI_Ireduce_scatter_block_f08 PMPI_Ireduce_scatter_block_f08
#define MPI_Ireduce_scatter_block PMPI_Ireduce_scatter_block
#define MPI_Ireduce_scatter_f08 PMPI_Ireduce_scatter_f08
#define MPI_Ireduce_scatter PMPI_Ireduce_scatter
#define MPI_Irsend_f08 PMPI_Irsend_f08
#define MPI_Irsend PMPI_Irsend
#define MPI_Iscan_f08 PMPI_Iscan_f08
#define MPI_Iscan PMPI_Iscan
#define MPI_Iscatter_f08 PMPI_Iscatter_f08
#define MPI_Iscatter PMPI_Iscatter
#define MPI_Iscatterv_f08 PMPI_Iscatterv_f08
#define MPI_Iscatterv PMPI_Iscatterv
#define MPI_Isend_f08 PMPI_Isend_f08
#define MPI_Isend PMPI_Isend
#define MPI_Isendrecv_f08 PMPI_Isendrecv_f08
#define MPI_Isendrecv PMPI_Isendrecv
#define MPI_Isendrecv_replace_f08 PMPI_Isendrecv_replace_f08
#define MPI_Isendrecv_replace PMPI_Isendrecv_replace
#define MPI_Issend_f08 PMPI_Issend_f08
#define MPI_Issend PMPI_Issend
#define MPI_Is_thread_main_f08 PMPI_Is_thread_main_f08
#define MPI_Is_thread_main PMPI_Is_thread_main
#define MPI_Lookup_name_f08 PMPI_Lookup_name_f08
#define MPI_Lookup_name PMPI_Lookup_name
#define MPI_Mprobe_f08 PMPI_Mprobe_f08
#define MPI_Mprobe PMPI_Mprobe
#define MPI_Mrecv_f08 PMPI_Mrecv_f08
#define MPI_Mrecv PMPI_Mrecv
#define MPI_Neighbor_allgather_f08 PMPI_Neighbor_allgather_f08
#define MPI_Neighbor_allgather_init_f08 PMPI_Neighbor_allgather_init_f08
#define MPI_Neighbor_allgather_init PMPI_Neighbor_allgather_init
#define MPI_Neighbor_allgather PMPI_Neighbor_allgather
#define MPI_Neighbor_allgatherv_f08 PMPI_Neighbor_allgatherv_f08
#define MPI_Neighbor_allgatherv_init_f08 PMPI_Neighbor_allgatherv_init_f08
#define MPI_Neighbor_allgatherv_init PMPI_Neighbor_allgatherv_init
#define MPI_Neighbor_allgatherv PMPI_Neighbor_allgatherv
#define MPI_Neighbor_alltoall_f08 PMPI_Neighbor_alltoall_f08
#define MPI_Neighbor_alltoall_init_f08 PMPI_Neighbor_alltoall_init_f08
#define MPI_Neighbor_alltoall_init PMPI_Neighbor_alltoall_init
#define MPI_Neighbor_alltoall PMPI_Neighbor_alltoall
#define MPI_Neighbor_alltoallv_f08 PMPI_Neighbor_alltoallv_f08
#define MPI_Neighbor_alltoallv_init_f08 PMPI_Neighbor_alltoallv_init_f08
#define MPI_Neighbor_alltoallv_init PMPI_Neighbor_alltoallv_init
#define MPI_Neighbor_alltoallv PMPI_Neighbor_alltoallv
#define MPI_Neighbor_alltoallw_f08 PMPI_Neighbor_alltoallw_f08
#define MPI_Neighbor_alltoallw_init_f08 PMPI_Neighbor_alltoallw_init_f08
#define MPI_Neighbor_alltoallw_init PMPI_Neighbor_alltoallw_init
#define MPI_Neighbor_alltoallw PMPI_Neighbor_alltoallw
#define MPI_Op_commutative_f08 PMPI_Op_commutative_f08
#define MPI_Op_commutative PMPI_Op_commutative
#define MPI_Op_create_f08 PMPI_Op_create_f08
#define MPI_Op_create PMPI_Op_create
#define MPI_Open_port_f08 PMPI_Open_port_f08
#define MPI_Open_port PMPI_Open_port
#define MPI_Op_free_f08 PMPI_Op_free_f08
#define MPI_Op_free PMPI_Op_free
#define MPI_Pack_external_f08 PMPI_Pack_external_f08
#define MPI_Pack_external PMPI_Pack_external
#define MPI_Pack_external_size_f08 PMPI_Pack_external_size_f08
#define MPI_Pack_external_size PMPI_Pack_external_size
#define MPI_Pack_f08 PMPI_Pack_f08
#define MPI_Pack PMPI_Pack
#define MPI_Pack_size_f08 PMPI_Pack_size_f08
#define MPI_Pack_size PMPI_Pack_size
#define MPI_Parrived_f08 PMPI_Parrived_f08
#define MPI_Parrived PMPI_Parrived
#define MPI_Pcontrol_f08 PMPI_Pcontrol_f08
#define MPI_Pcontrol PMPI_Pcontrol
#define MPI_Pready_f08 PMPI_Pready_f08
#define MPI_Pready_list_f08 PMPI_Pready_list_f08
#define MPI_Pready_list PMPI_Pready_list
#define MPI_Pready PMPI_Pready
#define MPI_Pready_range_f08 PMPI_Pready_range_f08
#define MPI_Pready_range PMPI_Pready_range
#define MPI_Precv_init_f08 PMPI_Precv_init_f08
#define MPI_Precv_init PMPI_Precv_init
#define MPI_Probe_f08 PMPI_Probe_f08
#define MPI_Probe PMPI_Probe
#define MPI_Psend_init_f08 PMPI_Psend_init_f08
#define MPI_Psend_init PMPI_Psend_init
#define MPI_Publish_name_f08 PMPI_Publish_name_f08
#define MPI_Publish_name PMPI_Publish_name
#define MPI_Put_f08 PMPI_Put_f08
#define MPI_Put PMPI_Put
#define MPI_Query_thread_f08 PMPI_Query_thread_f08
#define MPI_Query_thread PMPI_Query_thread
#define MPI_Raccumulate_f08 PMPI_Raccumulate_f08
#define MPI_Raccumulate PMPI_Raccumulate
#define MPI_Recv_f08_c PMPI_Recv_f08_c
#define MPI_Recv_f08 PMPI_Recv_f08
#define MPI_Recv_init_f08 PMPI_Recv_init_f08
#define MPI_Recv_init PMPI_Recv_init
#define MPI_Recv PMPI_Recv
#define MPI_Reduce_f08 PMPI_Reduce_f08
#define MPI_Reduce_init_f08 PMPI_Reduce_init_f08
#define MPI_Reduce_init PMPI_Reduce_init
#define MPI_Reduce_local_f08 PMPI_Reduce_local_f08
#define MPI_Reduce_local PMPI_Reduce_local
#define MPI_Reduce PMPI_Reduce
#define MPI_Reduce_scatter_block_f08 PMPI_Reduce_scatter_block_f08
#define MPI_Reduce_scatter_block_init_f08 PMPI_Reduce_scatter_block_init_f08
#define MPI_Reduce_scatter_block_init PMPI_Reduce_scatter_block_init
#define MPI_Reduce_scatter_block PMPI_Reduce_scatter_block
#define MPI_Reduce_scatter_f08 PMPI_Reduce_scatter_f08
#define MPI_Reduce_scatter_init_f08 PMPI_Reduce_scatter_init_f08
#define MPI_Reduce_scatter_init PMPI_Reduce_scatter_init
#define MPI_Reduce_scatter PMPI_Reduce_scatter
#define MPI_Register_datarep_f08 PMPI_Register_datarep_f08
#define MPI_Register_datarep PMPI_Register_datarep
#define MPI_Request_free_f08 PMPI_Request_free_f08
#define MPI_Request_free PMPI_Request_free
#define MPI_Request_get_status_f08 PMPI_Request_get_status_f08
#define MPI_Request_get_status PMPI_Request_get_status
#define MPI_Rget_accumulate_f08 PMPI_Rget_accumulate_f08
#define MPI_Rget_accumulate PMPI_Rget_accumulate
#define MPI_Rget_f08 PMPI_Rget_f08
#define MPI_Rget PMPI_Rget
#define MPI_Rput_f08 PMPI_Rput_f08
#define MPI_Rput PMPI_Rput
#define MPI_Rsend_f08 PMPI_Rsend_f08
#define MPI_Rsend_init_f08 PMPI_Rsend_init_f08
#define MPI_Rsend_init PMPI_Rsend_init
#define MPI_Rsend PMPI_Rsend
#define MPI_Scan_f08 PMPI_Scan_f08
#define MPI_Scan_init_f08 PMPI_Scan_init_f08
#define MPI_Scan_init PMPI_Scan_init
#define MPI_Scan PMPI_Scan
#define MPI_Scatter_f08 PMPI_Scatter_f08
#define MPI_Scatter_init_f08 PMPI_Scatter_init_f08
#define MPI_Scatter_init PMPI_Scatter_init
#define MPI_Scatter PMPI_Scatter
#define MPI_Scatterv_f08 PMPI_Scatterv_f08
#define MPI_Scatterv_init_f08 PMPI_Scatterv_init_f08
#define MPI_Scatterv_init PMPI_Scatterv_init
#define MPI_Scatterv PMPI_Scatterv
#define MPI_Send_f08_c PMPI_Send_f08_c
#define MPI_Send_f08 PMPI_Send_f08
#define MPI_Send_init_f08 PMPI_Send_init_f08
#define MPI_Send_init PMPI_Send_init
#define MPI_Send PMPI_Send
#define MPI_Sendrecv_f08 PMPI_Sendrecv_f08
#define MPI_Sendrecv PMPI_Sendrecv
#define MPI_Sendrecv_replace_f08 PMPI_Sendrecv_replace_f08
#define MPI_Sendrecv_replace PMPI_Sendrecv_replace
#define MPI_Session_call_errhandler_f08 PMPI_Session_call_errhandler_f08
#define MPI_Session_call_errhandler PMPI_Session_call_errhandler
#define MPI_Session_create_errhandler_f08 PMPI_Session_create_errhandler_f08
#define MPI_Session_create_errhandler PMPI_Session_create_errhandler
#define MPI_Session_finalize_f08 PMPI_Session_finalize_f08
#define MPI_Session_finalize PMPI_Session_finalize
#define MPI_Session_get_errhandler_f08 PMPI_Session_get_errhandler_f08
#define MPI_Session_get_errhandler PMPI_Session_get_errhandler
#define MPI_Session_get_info_f08 PMPI_Session_get_info_f08
#define MPI_Session_get_info PMPI_Session_get_info
#define MPI_Session_get_info PMPI_Session_get_info
#define MPI_Session_get_nth_pset_f08 PMPI_Session_get_nth_pset_f08
#define MPI_Session_get_nth_psetlen_f08 PMPI_Session_get_nth_psetlen_f08
#define MPI_Session_get_nth_psetlen PMPI_Session_get_nth_psetlen
#define MPI_Session_get_nth_pset PMPI_Session_get_nth_pset
#define MPI_Session_get_num_psets_f08 PMPI_Session_get_num_psets_f08
#define MPI_Session_get_num_psets PMPI_Session_get_num_psets
#define MPI_Session_get_pset_info_f08 PMPI_Session_get_pset_info_f08
#define MPI_Session_get_pset_info PMPI_Session_get_pset_info
#define MPI_Session_init_f08 PMPI_Session_init_f08
#define MPI_Session_init PMPI_Session_init
#define MPI_Session_set_errhandler_f08 PMPI_Session_set_errhandler_f08
#define MPI_Session_set_errhandler PMPI_Session_set_errhandler
#define MPI_Ssend_f08 PMPI_Ssend_f08
#define MPI_Ssend_init_f08 PMPI_Ssend_init_f08
#define MPI_Ssend_init PMPI_Ssend_init
#define MPI_Ssend PMPI_Ssend
#define MPI_Startall_f08 PMPI_Startall_f08
#define MPI_Startall PMPI_Startall
#define MPI_Start_f08 PMPI_Start_f08
#define MPI_Start PMPI_Start
#define MPI_Status_f082f_f08 PMPI_Status_f082f_f08
#define MPI_Status_f082f PMPI_Status_f082f
#define MPI_Status_f2f08_f08 PMPI_Status_f2f08_f08
#define MPI_Status_f2f08 PMPI_Status_f2f08
#define MPI_Status_set_cancelled_f08 PMPI_Status_set_cancelled_f08
#define MPI_Status_set_cancelled PMPI_Status_set_cancelled
#define MPI_Status_set_elements_f08 PMPI_Status_set_elements_f08
#define MPI_Status_set_elements PMPI_Status_set_elements
#define MPI_Status_set_elements_x_f08 PMPI_Status_set_elements_x_f08
#define MPI_Status_set_elements_x PMPI_Status_set_elements_x
#define MPI_Testall_f08 PMPI_Testall_f08
#define MPI_Testall PMPI_Testall
#define MPI_Testany_f08 PMPI_Testany_f08
#define MPI_Testany PMPI_Testany
#define MPI_Test_cancelled_f08 PMPI_Test_cancelled_f08
#define MPI_Test_cancelled PMPI_Test_cancelled
#define MPI_Test_f08 PMPI_Test_f08
#define MPI_Test PMPI_Test
#define MPI_Testsome_f08 PMPI_Testsome_f08
#define MPI_Testsome PMPI_Testsome
#define MPI_Topo_test_f08 PMPI_Topo_test_f08
#define MPI_Topo_test PMPI_Topo_test
#define MPI_Type_commit_f08 PMPI_Type_commit_f08
#define MPI_Type_commit PMPI_Type_commit
#define MPI_Type_contiguous_f08 PMPI_Type_contiguous_f08
#define MPI_Type_contiguous PMPI_Type_contiguous
#define MPI_Type_create_darray_f08 PMPI_Type_create_darray_f08
#define MPI_Type_create_darray PMPI_Type_create_darray
#define MPI_Type_create_f90_complex_f08 PMPI_Type_create_f90_complex_f08
#define MPI_Type_create_f90_complex PMPI_Type_create_f90_complex
#define MPI_Type_create_f90_integer_f08 PMPI_Type_create_f90_integer_f08
#define MPI_Type_create_f90_integer PMPI_Type_create_f90_integer
#define MPI_Type_create_f90_real_f08 PMPI_Type_create_f90_real_f08
#define MPI_Type_create_f90_real PMPI_Type_create_f90_real
#define MPI_Type_create_hindexed_block_f08 PMPI_Type_create_hindexed_block_f08
#define MPI_Type_create_hindexed_block PMPI_Type_create_hindexed_block
#define MPI_Type_create_hindexed_f08 PMPI_Type_create_hindexed_f08
#define MPI_Type_create_hindexed PMPI_Type_create_hindexed
#define MPI_Type_create_hvector_f08 PMPI_Type_create_hvector_f08
#define MPI_Type_create_hvector PMPI_Type_create_hvector
#define MPI_Type_create_indexed_block_f08 PMPI_Type_create_indexed_block_f08
#define MPI_Type_create_indexed_block PMPI_Type_create_indexed_block
#define MPI_Type_create_keyval_f08 PMPI_Type_create_keyval_f08
#define MPI_Type_create_keyval PMPI_Type_create_keyval
#define MPI_Type_create_resized_f08 PMPI_Type_create_resized_f08
#define MPI_Type_create_resized PMPI_Type_create_resized
#define MPI_Type_create_struct_f08 PMPI_Type_create_struct_f08
#define MPI_Type_create_struct PMPI_Type_create_struct
#define MPI_Type_create_subarray_f08 PMPI_Type_create_subarray_f08
#define MPI_Type_create_subarray PMPI_Type_create_subarray
#define MPI_Type_delete_attr_f08 PMPI_Type_delete_attr_f08
#define MPI_Type_delete_attr PMPI_Type_delete_attr
#define MPI_Type_dup_f08 PMPI_Type_dup_f08
#define MPI_Type_dup PMPI_Type_dup
#define MPI_Type_free_f08 PMPI_Type_free_f08
#define MPI_Type_free_keyval_f08 PMPI_Type_free_keyval_f08
#define MPI_Type_free_keyval PMPI_Type_free_keyval
#define MPI_Type_free PMPI_Type_free
#define MPI_Type_get_attr_f08 PMPI_Type_get_attr_f08
#define MPI_Type_get_attr PMPI_Type_get_attr
#define MPI_Type_get_contents_f08_c PMPI_Type_get_contents_f08_c
#define MPI_Type_get_contents_f08 PMPI_Type_get_contents_f08
#define MPI_Type_get_contents PMPI_Type_get_contents
#define MPI_Type_get_envelope_f08_c PMPI_Type_get_envelope_f08_c
#define MPI_Type_get_envelope_f08 PMPI_Type_get_envelope_f08
#define MPI_Type_get_envelope PMPI_Type_get_envelope
#define MPI_Type_get_extent_f08 PMPI_Type_get_extent_f08
#define MPI_Type_get_extent PMPI_Type_get_extent
#define MPI_Type_get_extent_x_f08 PMPI_Type_get_extent_x_f08
#define MPI_Type_get_extent_x PMPI_Type_get_extent_x
#define MPI_Type_get_name_f08 PMPI_Type_get_name_f08
#define MPI_Type_get_name PMPI_Type_get_name
#define MPI_Type_get_true_extent_f08 PMPI_Type_get_true_extent_f08
#define MPI_Type_get_true_extent PMPI_Type_get_true_extent
#define MPI_Type_get_true_extent_x_f08 PMPI_Type_get_true_extent_x_f08
#define MPI_Type_get_true_extent_x PMPI_Type_get_true_extent_x
#define MPI_Type_indexed_f08 PMPI_Type_indexed_f08
#define MPI_Type_indexed PMPI_Type_indexed
#define MPI_Type_match_size_f08 PMPI_Type_match_size_f08
#define MPI_Type_match_size PMPI_Type_match_size
#define MPI_Type_set_attr_f08 PMPI_Type_set_attr_f08
#define MPI_Type_set_attr PMPI_Type_set_attr
#define MPI_Type_set_name_f08 PMPI_Type_set_name_f08
#define MPI_Type_set_name PMPI_Type_set_name
#define MPI_Type_size_f08 PMPI_Type_size_f08
#define MPI_Type_size PMPI_Type_size
#define MPI_Type_size_x_f08 PMPI_Type_size_x_f08
#define MPI_Type_size_x PMPI_Type_size_x
#define MPI_Type_vector_f08 PMPI_Type_vector_f08
#define MPI_Type_vector PMPI_Type_vector
#define MPI_Unpack_external_f08 PMPI_Unpack_external_f08
#define MPI_Unpack_external PMPI_Unpack_external
#define MPI_Unpack_f08 PMPI_Unpack_f08
#define MPI_Unpack PMPI_Unpack
#define MPI_Unpublish_name_f08 PMPI_Unpublish_name_f08
#define MPI_Unpublish_name PMPI_Unpublish_name
#define MPI_Waitall_f08 PMPI_Waitall_f08
#define MPI_Waitall PMPI_Waitall
#define MPI_Waitany_f08 PMPI_Waitany_f08
#define MPI_Waitany PMPI_Waitany
#define MPI_Wait_f08 PMPI_Wait_f08
#define MPI_Wait PMPI_Wait
#define MPI_Waitsome_f08 PMPI_Waitsome_f08
#define MPI_Waitsome PMPI_Waitsome
#define MPI_Win_allocate_f08 PMPI_Win_allocate_f08
#define MPI_Win_allocate PMPI_Win_allocate
#define MPI_Win_allocate_shared_f08 PMPI_Win_allocate_shared_f08
#define MPI_Win_allocate_shared PMPI_Win_allocate_shared
#define MPI_Win_attach_f08 PMPI_Win_attach_f08
#define MPI_Win_attach PMPI_Win_attach
#define MPI_Win_call_errhandler_f08 PMPI_Win_call_errhandler_f08
#define MPI_Win_call_errhandler PMPI_Win_call_errhandler
#define MPI_Win_complete_f08 PMPI_Win_complete_f08
#define MPI_Win_complete PMPI_Win_complete
#define MPI_Win_create_dynamic_f08 PMPI_Win_create_dynamic_f08
#define MPI_Win_create_dynamic PMPI_Win_create_dynamic
#define MPI_Win_create_errhandler_f08 PMPI_Win_create_errhandler_f08
#define MPI_Win_create_errhandler PMPI_Win_create_errhandler
#define MPI_Win_create_f08 PMPI_Win_create_f08
#define MPI_Win_create_keyval_f08 PMPI_Win_create_keyval_f08
#define MPI_Win_create_keyval PMPI_Win_create_keyval
#define MPI_Win_create PMPI_Win_create
#define MPI_Win_delete_attr_f08 PMPI_Win_delete_attr_f08
#define MPI_Win_delete_attr PMPI_Win_delete_attr
#define MPI_Win_detach_f08 PMPI_Win_detach_f08
#define MPI_Win_detach PMPI_Win_detach
#define MPI_Win_fence_f08 PMPI_Win_fence_f08
#define MPI_Win_fence PMPI_Win_fence
#define MPI_Win_flush_all_f08 PMPI_Win_flush_all_f08
#define MPI_Win_flush_all PMPI_Win_flush_all
#define MPI_Win_flush_f08 PMPI_Win_flush_f08
#define MPI_Win_flush_local_all_f08 PMPI_Win_flush_local_all_f08
#define MPI_Win_flush_local_all PMPI_Win_flush_local_all
#define MPI_Win_flush_local_f08 PMPI_Win_flush_local_f08
#define MPI_Win_flush_local PMPI_Win_flush_local
#define MPI_Win_flush PMPI_Win_flush
#define MPI_Win_free_f08 PMPI_Win_free_f08
#define MPI_Win_free_keyval_f08 PMPI_Win_free_keyval_f08
#define MPI_Win_free_keyval PMPI_Win_free_keyval
#define MPI_Win_free PMPI_Win_free
#define MPI_Win_get_attr_f08 PMPI_Win_get_attr_f08
#define MPI_Win_get_attr PMPI_Win_get_attr
#define MPI_Win_get_errhandler_f08 PMPI_Win_get_errhandler_f08
#define MPI_Win_get_errhandler PMPI_Win_get_errhandler
#define MPI_Win_get_group_f08 PMPI_Win_get_group_f08
#define MPI_Win_get_group PMPI_Win_get_group
#define MPI_Win_get_info_f08 PMPI_Win_get_info_f08
#define MPI_Win_get_info PMPI_Win_get_info
#define MPI_Win_get_name_f08 PMPI_Win_get_name_f08
#define MPI_Win_get_name PMPI_Win_get_name
#define MPI_Win_lock_all_f08 PMPI_Win_lock_all_f08
#define MPI_Win_lock_all PMPI_Win_lock_all
#define MPI_Win_lock_f08 PMPI_Win_lock_f08
#define MPI_Win_lock PMPI_Win_lock
#define MPI_Win_post_f08 PMPI_Win_post_f08
#define MPI_Win_post PMPI_Win_post
#define MPI_Win_set_attr_f08 PMPI_Win_set_attr_f08
#define MPI_Win_set_attr PMPI_Win_set_attr
#define MPI_Win_set_errhandler_f08 PMPI_Win_set_errhandler_f08
#define MPI_Win_set_errhandler PMPI_Win_set_errhandler
#define MPI_Win_set_info_f08 PMPI_Win_set_info_f08
#define MPI_Win_set_info PMPI_Win_set_info
#define MPI_Win_set_name_f08 PMPI_Win_set_name_f08
#define MPI_Win_set_name PMPI_Win_set_name
#define MPI_Win_shared_query_f08 PMPI_Win_shared_query_f08
#define MPI_Win_shared_query PMPI_Win_shared_query
#define MPI_Win_start_f08 PMPI_Win_start_f08
#define MPI_Win_start PMPI_Win_start
#define MPI_Win_sync_f08 PMPI_Win_sync_f08
#define MPI_Win_sync PMPI_Win_sync
#define MPI_Win_test_f08 PMPI_Win_test_f08
#define MPI_Win_test PMPI_Win_test
#define MPI_Win_unlock_all_f08 PMPI_Win_unlock_all_f08
#define MPI_Win_unlock_all PMPI_Win_unlock_all
#define MPI_Win_unlock_f08 PMPI_Win_unlock_f08
#define MPI_Win_unlock PMPI_Win_unlock
#define MPI_Win_wait_f08 PMPI_Win_wait_f08
#define MPI_Win_wait PMPI_Win_wait

#endif
