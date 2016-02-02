# -*- makefile -*-
#
# Copyright (c) 2006-2015 Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2012-2013 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2012-2013 Inria.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015-2016 Research Organization for Information Science
#                         and Technology (RIST). All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

# This Makefile is only relevant if we're building the "use mpi_f08"
# MPI bindings.
if OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS

AM_FCFLAGS = -I$(top_builddir)/ompi/include \
             -I$(top_srcdir)/ompi/include \
             $(OMPI_FC_MODULE_FLAG)$(top_builddir)/ompi/$(OMPI_FORTRAN_USEMPI_DIR) \
             $(OMPI_FC_MODULE_FLAG). \
             -I$(top_srcdir) $(FCFLAGS_f90)

MOSTLYCLEANFILES = *.mod

CLEANFILES += *.i90

lib_LTLIBRARIES = libmpi_usempif08.la

module_sentinel_file = \
        libforce_usempif08_internal_modules_to_be_built.la

noinst_LTLIBRARIES = $(module_sentinel_file)

mpi-f08.lo: $(module_sentinel_file)
mpi-f08.lo: mpi-f08.F90
mpi-f08.lo: mpi-f-interfaces-bind.h pmpi-f-interfaces-bind.h
mpi-f08.lo: sizeof_f08.h

#
# *sizeof_f08.* are generated based on some results from
# configure tests.
#

sizeof_pl=$(top_srcdir)/ompi/mpi/fortran/base/gen-mpi-sizeof.pl

sizeof_f08.h: $(top_builddir)/config.status
sizeof_f08.h: $(sizeof_pl)
sizeof_f08.h:
	$(OMPI_V_GEN) $(sizeof_pl) \
	    --header=$@ --ierror=optional \
	    --maxrank=$(OMPI_FORTRAN_MAX_ARRAY_RANK) \
	    --generate=$(OMPI_FORTRAN_BUILD_SIZEOF) \
	    --real16=$(OMPI_HAVE_FORTRAN_REAL16) \
	    --complex32=$(OMPI_HAVE_FORTRAN_COMPLEX32)

sizeof_f08.f90: $(top_builddir)/config.status
sizeof_f08.f90: $(sizeof_pl)
sizeof_f08.f90:
	$(OMPI_V_GEN) $(sizeof_pl) \
	    --impl=$@ --ierror=optional --mpi \
	    --maxrank=$(OMPI_FORTRAN_MAX_ARRAY_RANK) \
	    --generate=$(OMPI_FORTRAN_BUILD_SIZEOF) \
	    --real16=$(OMPI_HAVE_FORTRAN_REAL16) \
	    --complex32=$(OMPI_HAVE_FORTRAN_COMPLEX32)

profile/psizeof_f08.f90: $(top_builddir)/config.status
profile/psizeof_f08.f90: $(sizeof_pl)
profile/psizeof_f08.f90:
	$(OMPI_V_GEN) $(sizeof_pl) \
	    --impl=$@ --ierror=optional --pmpi \
	    --maxrank=$(OMPI_FORTRAN_MAX_ARRAY_RANK) \
	    --generate=$(OMPI_FORTRAN_BUILD_SIZEOF) \
	    --real16=$(OMPI_HAVE_FORTRAN_REAL16) \
	    --complex32=$(OMPI_HAVE_FORTRAN_COMPLEX32)

CLEANFILES += sizeof_f08.h sizeof_f08.f90 profile/psizeof_f08.f90

mpi_api_files = \
        abort_f08.F90 \
        accumulate_f08.F90 \
        add_error_class_f08.F90 \
        add_error_code_f08.F90 \
        add_error_string_f08.F90 \
        aint_add_f08.F90 \
        aint_diff_f08.F90 \
        allgather_f08.F90 \
        allgatherv_f08.F90 \
        alloc_mem_f08.F90 \
        allreduce_f08.F90 \
        alltoall_f08.F90 \
        alltoallv_f08.F90 \
        alltoallw_f08.F90 \
        barrier_f08.F90 \
        bcast_f08.F90 \
        bsend_f08.F90 \
        bsend_init_f08.F90 \
        buffer_attach_f08.F90 \
        buffer_detach_f08.F90 \
        cancel_f08.F90 \
        cart_coords_f08.F90 \
        cart_create_f08.F90 \
        cartdim_get_f08.F90 \
        cart_get_f08.F90 \
        cart_map_f08.F90 \
        cart_rank_f08.F90 \
        cart_shift_f08.F90 \
        cart_sub_f08.F90 \
        close_port_f08.F90 \
        comm_accept_f08.F90 \
        comm_call_errhandler_f08.F90 \
        comm_compare_f08.F90 \
        comm_connect_f08.F90 \
        comm_create_errhandler_f08.F90 \
        comm_create_f08.F90 \
        comm_create_group_f08.F90 \
        comm_create_keyval_f08.F90 \
        comm_delete_attr_f08.F90 \
        comm_disconnect_f08.F90 \
        comm_dup_f08.F90 \
        comm_dup_with_info_f08.F90 \
        comm_idup_f08.F90 \
        comm_free_f08.F90 \
        comm_free_keyval_f08.F90 \
        comm_get_attr_f08.F90 \
        comm_get_errhandler_f08.F90 \
        comm_get_info_f08.F90 \
        comm_get_name_f08.F90 \
        comm_get_parent_f08.F90 \
        comm_group_f08.F90 \
        comm_join_f08.F90 \
        comm_rank_f08.F90 \
        comm_remote_group_f08.F90 \
        comm_remote_size_f08.F90 \
        comm_set_attr_f08.F90 \
        comm_set_errhandler_f08.F90 \
        comm_set_info_f08.F90 \
        comm_set_name_f08.F90 \
        comm_size_f08.F90 \
        comm_spawn_f08.F90 \
        comm_spawn_multiple_f08.F90 \
        comm_split_f08.F90 \
        comm_split_type_f08.F90 \
        comm_test_inter_f08.F90 \
        compare_and_swap_f08.F90 \
        dist_graph_create_adjacent_f08.F90 \
        dist_graph_create_f08.F90 \
        dist_graph_neighbors_count_f08.F90 \
        dist_graph_neighbors_f08.F90 \
        dims_create_f08.F90 \
        errhandler_free_f08.F90 \
        error_class_f08.F90 \
        error_string_f08.F90 \
        exscan_f08.F90 \
        f_sync_reg_f08.F90 \
        fetch_and_op_f08.F90 \
        finalized_f08.F90 \
        finalize_f08.F90 \
        free_mem_f08.F90 \
        gather_f08.F90 \
        gatherv_f08.F90 \
        get_accumulate_f08.F90 \
        get_address_f08.F90 \
        get_count_f08.F90 \
        get_elements_f08.F90 \
        get_elements_x_f08.F90 \
        get_f08.F90 \
        get_library_version_f08.F90 \
        get_processor_name_f08.F90 \
        get_version_f08.F90 \
        graph_create_f08.F90 \
        graphdims_get_f08.F90 \
        graph_get_f08.F90 \
        graph_map_f08.F90 \
        graph_neighbors_count_f08.F90 \
        graph_neighbors_f08.F90 \
        grequest_complete_f08.F90 \
        grequest_start_f08.F90 \
        group_compare_f08.F90 \
        group_difference_f08.F90 \
        group_excl_f08.F90 \
        group_free_f08.F90 \
        group_incl_f08.F90 \
        group_intersection_f08.F90 \
        group_range_excl_f08.F90 \
        group_range_incl_f08.F90 \
        group_rank_f08.F90 \
        group_size_f08.F90 \
        group_translate_ranks_f08.F90 \
        group_union_f08.F90 \
        iallgather_f08.F90 \
        iallgatherv_f08.F90 \
        iallreduce_f08.F90 \
        ialltoall_f08.F90 \
        ialltoallv_f08.F90 \
        ialltoallw_f08.F90 \
        ibarrier_f08.F90 \
        ibcast_f08.F90 \
        ibsend_f08.F90 \
        iexscan_f08.F90 \
        igather_f08.F90 \
        igatherv_f08.F90 \
        improbe_f08.F90 \
        imrecv_f08.F90 \
        ineighbor_allgather_f08.F90 \
        ineighbor_allgatherv_f08.F90 \
        ineighbor_alltoall_f08.F90 \
        ineighbor_alltoallv_f08.F90 \
        ineighbor_alltoallw_f08.F90 \
        info_create_f08.F90 \
        info_delete_f08.F90 \
        info_dup_f08.F90 \
        info_free_f08.F90 \
        info_get_f08.F90 \
        info_get_nkeys_f08.F90 \
        info_get_nthkey_f08.F90 \
        info_get_valuelen_f08.F90 \
        info_set_f08.F90 \
        init_f08.F90 \
        initialized_f08.F90 \
        init_thread_f08.F90 \
        intercomm_create_f08.F90 \
        intercomm_merge_f08.F90 \
        iprobe_f08.F90 \
        irecv_f08.F90 \
        ireduce_f08.F90 \
        ireduce_scatter_f08.F90 \
        ireduce_scatter_block_f08.F90 \
        irsend_f08.F90 \
        iscan_f08.F90 \
        iscatter_f08.F90 \
        iscatterv_f08.F90 \
        isend_f08.F90 \
        issend_f08.F90 \
        is_thread_main_f08.F90 \
        lookup_name_f08.F90 \
        mprobe_f08.F90 \
        mrecv_f08.F90 \
        neighbor_allgather_f08.F90 \
        neighbor_allgatherv_f08.F90 \
        neighbor_alltoall_f08.F90 \
        neighbor_alltoallv_f08.F90 \
        neighbor_alltoallw_f08.F90 \
        op_commutative_f08.F90 \
        op_create_f08.F90 \
        open_port_f08.F90 \
        op_free_f08.F90 \
        pack_external_f08.F90 \
        pack_external_size_f08.F90 \
        pack_f08.F90 \
        pack_size_f08.F90 \
        pcontrol_f08.F90 \
        probe_f08.F90 \
        publish_name_f08.F90 \
        put_f08.F90 \
        query_thread_f08.F90 \
        raccumulate_f08.F90 \
        recv_f08.F90 \
        recv_init_f08.F90 \
        reduce_f08.F90 \
        reduce_local_f08.F90 \
        reduce_scatter_f08.F90 \
        reduce_scatter_block_f08.F90 \
        request_free_f08.F90 \
        request_get_status_f08.F90 \
        rget_f08.F90 \
        rget_accumulate_f08.F90 \
        rput_f08.F90 \
        rsend_f08.F90 \
        rsend_init_f08.F90 \
        scan_f08.F90 \
        scatter_f08.F90 \
        scatterv_f08.F90 \
        send_f08.F90 \
        send_init_f08.F90 \
        sendrecv_f08.F90 \
        sendrecv_replace_f08.F90 \
        ssend_f08.F90 \
        ssend_init_f08.F90 \
        startall_f08.F90 \
        start_f08.F90 \
        status_set_cancelled_f08.F90 \
        status_set_elements_f08.F90 \
        status_set_elements_x_f08.F90 \
        testall_f08.F90 \
        testany_f08.F90 \
        test_cancelled_f08.F90 \
        test_f08.F90 \
        testsome_f08.F90 \
        topo_test_f08.F90 \
        type_commit_f08.F90 \
        type_contiguous_f08.F90 \
        type_create_darray_f08.F90 \
        type_create_f90_complex_f08.F90 \
        type_create_f90_integer_f08.F90 \
        type_create_f90_real_f08.F90 \
        type_create_hindexed_f08.F90 \
        type_create_hvector_f08.F90 \
        type_create_indexed_block_f08.F90 \
        type_create_hindexed_block_f08.F90 \
        type_create_keyval_f08.F90 \
        type_create_resized_f08.F90 \
        type_create_struct_f08.F90 \
        type_create_subarray_f08.F90 \
        type_delete_attr_f08.F90 \
        type_dup_f08.F90 \
        type_free_f08.F90 \
        type_free_keyval_f08.F90 \
        type_get_attr_f08.F90 \
        type_get_contents_f08.F90 \
        type_get_envelope_f08.F90 \
        type_get_extent_f08.F90 \
        type_get_extent_x_f08.F90 \
        type_get_name_f08.F90 \
        type_get_true_extent_f08.F90 \
        type_get_true_extent_x_f08.F90 \
        type_indexed_f08.F90 \
        type_match_size_f08.F90 \
        type_set_attr_f08.F90 \
        type_set_name_f08.F90 \
        type_size_f08.F90 \
        type_size_x_f08.F90 \
        type_vector_f08.F90 \
        unpack_external_f08.F90 \
        unpack_f08.F90 \
        unpublish_name_f08.F90 \
        waitall_f08.F90 \
        waitany_f08.F90 \
        wait_f08.F90 \
        waitsome_f08.F90 \
        win_allocate_f08.F90 \
        win_allocate_shared_f08.F90 \
        win_attach_f08.F90 \
        win_call_errhandler_f08.F90 \
        win_complete_f08.F90 \
        win_create_dynamic_f08.F90 \
        win_create_errhandler_f08.F90 \
        win_create_f08.F90 \
        win_create_keyval_f08.F90 \
        win_delete_attr_f08.F90 \
        win_detach_f08.F90 \
        win_fence_f08.F90 \
        win_flush_f08.F90 \
        win_flush_all_f08.F90 \
        win_flush_local_f08.F90 \
        win_flush_local_all_f08.F90 \
        win_free_f08.F90 \
        win_free_keyval_f08.F90 \
        win_get_attr_f08.F90 \
        win_get_errhandler_f08.F90 \
        win_get_group_f08.F90 \
        win_get_info_f08.F90 \
        win_get_name_f08.F90 \
        win_lock_f08.F90 \
        win_lock_all_f08.F90 \
        win_post_f08.F90 \
        win_set_attr_f08.F90 \
        win_set_errhandler_f08.F90 \
        win_set_info_f08.F90 \
        win_set_name_f08.F90 \
        win_shared_query_f08.F90 \
        win_start_f08.F90 \
        win_sync_f08.F90 \
        win_test_f08.F90 \
        win_unlock_f08.F90 \
        win_unlock_all_f08.F90 \
        win_wait_f08.F90

if OMPI_PROVIDE_MPI_FILE_INTERFACE
mpi_api_files += \
        file_call_errhandler_f08.F90 \
        file_close_f08.F90 \
        file_create_errhandler_f08.F90 \
        file_delete_f08.F90 \
        file_get_amode_f08.F90 \
        file_get_atomicity_f08.F90 \
        file_get_byte_offset_f08.F90 \
        file_get_errhandler_f08.F90 \
        file_get_group_f08.F90 \
        file_get_info_f08.F90 \
        file_get_position_f08.F90 \
        file_get_position_shared_f08.F90 \
        file_get_size_f08.F90 \
        file_get_type_extent_f08.F90 \
        file_get_view_f08.F90 \
        file_iread_at_f08.F90 \
        file_iread_f08.F90 \
        file_iread_at_all_f08.F90 \
        file_iread_all_f08.F90 \
        file_iread_shared_f08.F90 \
        file_iwrite_at_f08.F90 \
        file_iwrite_f08.F90 \
        file_iwrite_at_all_f08.F90 \
        file_iwrite_all_f08.F90 \
        file_iwrite_shared_f08.F90 \
        file_open_f08.F90 \
        file_preallocate_f08.F90 \
        file_read_all_begin_f08.F90 \
        file_read_all_end_f08.F90 \
        file_read_all_f08.F90 \
        file_read_at_all_begin_f08.F90 \
        file_read_at_all_end_f08.F90 \
        file_read_at_all_f08.F90 \
        file_read_at_f08.F90 \
        file_read_f08.F90 \
        file_read_ordered_begin_f08.F90 \
        file_read_ordered_end_f08.F90 \
        file_read_ordered_f08.F90 \
        file_read_shared_f08.F90 \
        file_seek_f08.F90 \
        file_seek_shared_f08.F90 \
        file_set_atomicity_f08.F90 \
        file_set_errhandler_f08.F90 \
        file_set_info_f08.F90 \
        file_set_size_f08.F90 \
        file_set_view_f08.F90 \
        file_sync_f08.F90 \
        file_write_all_begin_f08.F90 \
        file_write_all_end_f08.F90 \
        file_write_all_f08.F90 \
        file_write_at_all_begin_f08.F90 \
        file_write_at_all_end_f08.F90 \
        file_write_at_all_f08.F90 \
        file_write_at_f08.F90 \
        file_write_f08.F90 \
        file_write_ordered_begin_f08.F90 \
        file_write_ordered_end_f08.F90 \
        file_write_ordered_f08.F90 \
        file_write_shared_f08.F90 \
        register_datarep_f08.F90
endif

# JMS Somehow this variable substitution isn't quite working, and I
# don't have time to figure it out.  So just wholesale copy the file
# list.  :-(
#pmpi_api_files = $(mpi_api_files:%=profile/p%)

pmpi_api_files = \
        profile/pabort_f08.F90 \
        profile/paccumulate_f08.F90 \
        profile/padd_error_class_f08.F90 \
        profile/padd_error_code_f08.F90 \
        profile/padd_error_string_f08.F90 \
        profile/paint_add_f08.F90 \
        profile/paint_diff_f08.F90 \
        profile/pallgather_f08.F90 \
        profile/pallgatherv_f08.F90 \
        profile/palloc_mem_f08.F90 \
        profile/pallreduce_f08.F90 \
        profile/palltoall_f08.F90 \
        profile/palltoallv_f08.F90 \
        profile/palltoallw_f08.F90 \
        profile/pbarrier_f08.F90 \
        profile/pbcast_f08.F90 \
        profile/pbsend_f08.F90 \
        profile/pbsend_init_f08.F90 \
        profile/pbuffer_attach_f08.F90 \
        profile/pbuffer_detach_f08.F90 \
        profile/pcancel_f08.F90 \
        profile/pcart_coords_f08.F90 \
        profile/pcart_create_f08.F90 \
        profile/pcartdim_get_f08.F90 \
        profile/pcart_get_f08.F90 \
        profile/pcart_map_f08.F90 \
        profile/pcart_rank_f08.F90 \
        profile/pcart_shift_f08.F90 \
        profile/pcart_sub_f08.F90 \
        profile/pclose_port_f08.F90 \
        profile/pcomm_accept_f08.F90 \
        profile/pcomm_call_errhandler_f08.F90 \
        profile/pcomm_compare_f08.F90 \
        profile/pcomm_connect_f08.F90 \
        profile/pcomm_create_errhandler_f08.F90 \
        profile/pcomm_create_f08.F90 \
        profile/pcomm_create_group_f08.F90 \
        profile/pcomm_create_keyval_f08.F90 \
        profile/pcomm_delete_attr_f08.F90 \
        profile/pcomm_disconnect_f08.F90 \
        profile/pcomm_dup_f08.F90 \
        profile/pcomm_dup_with_info_f08.F90 \
        profile/pcomm_idup_f08.F90 \
        profile/pcomm_free_f08.F90 \
        profile/pcomm_free_keyval_f08.F90 \
        profile/pcomm_get_attr_f08.F90 \
        profile/pcomm_get_errhandler_f08.F90 \
        profile/pcomm_get_info_f08.F90 \
        profile/pcomm_get_name_f08.F90 \
        profile/pcomm_get_parent_f08.F90 \
        profile/pcomm_group_f08.F90 \
        profile/pcomm_join_f08.F90 \
        profile/pcomm_rank_f08.F90 \
        profile/pcomm_remote_group_f08.F90 \
        profile/pcomm_remote_size_f08.F90 \
        profile/pcomm_set_attr_f08.F90 \
        profile/pcomm_set_errhandler_f08.F90 \
        profile/pcomm_set_info_f08.F90 \
        profile/pcomm_set_name_f08.F90 \
        profile/pcomm_size_f08.F90 \
        profile/pcomm_spawn_f08.F90 \
        profile/pcomm_spawn_multiple_f08.F90 \
        profile/pcomm_split_f08.F90 \
        profile/pcomm_split_type_f08.F90 \
        profile/pcomm_test_inter_f08.F90 \
        profile/pcompare_and_swap_f08.F90 \
        profile/pdims_create_f08.F90 \
        profile/pdist_graph_create_adjacent_f08.F90 \
        profile/pdist_graph_create_f08.F90 \
        profile/pdist_graph_neighbors_count_f08.F90 \
        profile/pdist_graph_neighbors_f08.F90 \
        profile/perrhandler_free_f08.F90 \
        profile/perror_class_f08.F90 \
        profile/perror_string_f08.F90 \
        profile/pexscan_f08.F90 \
        profile/pf_sync_reg_f08.F90 \
        profile/pfetch_and_op_f08.F90 \
        profile/pfinalized_f08.F90 \
        profile/pfinalize_f08.F90 \
        profile/pfree_mem_f08.F90 \
        profile/pgather_f08.F90 \
        profile/pgatherv_f08.F90 \
        profile/pget_accumulate_f08.F90 \
        profile/pget_address_f08.F90 \
        profile/pget_count_f08.F90 \
        profile/pget_elements_f08.F90 \
        profile/pget_elements_x_f08.F90 \
        profile/pget_f08.F90 \
        profile/pget_library_version_f08.F90 \
        profile/pget_processor_name_f08.F90 \
        profile/pget_version_f08.F90 \
        profile/pgraph_create_f08.F90 \
        profile/pgraphdims_get_f08.F90 \
        profile/pgraph_get_f08.F90 \
        profile/pgraph_map_f08.F90 \
        profile/pgraph_neighbors_count_f08.F90 \
        profile/pgraph_neighbors_f08.F90 \
        profile/pgrequest_complete_f08.F90 \
        profile/pgrequest_start_f08.F90 \
        profile/pgroup_compare_f08.F90 \
        profile/pgroup_difference_f08.F90 \
        profile/pgroup_excl_f08.F90 \
        profile/pgroup_free_f08.F90 \
        profile/pgroup_incl_f08.F90 \
        profile/pgroup_intersection_f08.F90 \
        profile/pgroup_range_excl_f08.F90 \
        profile/pgroup_range_incl_f08.F90 \
        profile/pgroup_rank_f08.F90 \
        profile/pgroup_size_f08.F90 \
        profile/pgroup_translate_ranks_f08.F90 \
        profile/pgroup_union_f08.F90 \
        profile/piallgather_f08.F90 \
        profile/piallgatherv_f08.F90 \
        profile/piallreduce_f08.F90 \
        profile/pialltoall_f08.F90 \
        profile/pialltoallv_f08.F90 \
        profile/pialltoallw_f08.F90 \
        profile/pibarrier_f08.F90 \
        profile/pibcast_f08.F90 \
        profile/pibsend_f08.F90 \
        profile/pigather_f08.F90 \
        profile/pigatherv_f08.F90 \
        profile/piexscan_f08.F90 \
        profile/pimprobe_f08.F90 \
        profile/pimrecv_f08.F90 \
        profile/pineighbor_allgather_f08.F90 \
        profile/pineighbor_allgatherv_f08.F90 \
        profile/pineighbor_alltoall_f08.F90 \
        profile/pineighbor_alltoallv_f08.F90 \
        profile/pineighbor_alltoallw_f08.F90 \
        profile/pinfo_create_f08.F90 \
        profile/pinfo_delete_f08.F90 \
        profile/pinfo_dup_f08.F90 \
        profile/pinfo_free_f08.F90 \
        profile/pinfo_get_f08.F90 \
        profile/pinfo_get_nkeys_f08.F90 \
        profile/pinfo_get_nthkey_f08.F90 \
        profile/pinfo_get_valuelen_f08.F90 \
        profile/pinfo_set_f08.F90 \
        profile/pinit_f08.F90 \
        profile/pinitialized_f08.F90 \
        profile/pinit_thread_f08.F90 \
        profile/pintercomm_create_f08.F90 \
        profile/pintercomm_merge_f08.F90 \
        profile/piprobe_f08.F90 \
        profile/pirecv_f08.F90 \
        profile/pireduce_f08.F90 \
        profile/pireduce_scatter_f08.F90 \
        profile/pireduce_scatter_block_f08.F90 \
        profile/pirsend_f08.F90 \
        profile/piscan_f08.F90 \
        profile/piscatter_f08.F90 \
        profile/piscatterv_f08.F90 \
        profile/pisend_f08.F90 \
        profile/pissend_f08.F90 \
        profile/pis_thread_main_f08.F90 \
        profile/plookup_name_f08.F90 \
        profile/pmprobe_f08.F90 \
        profile/pmrecv_f08.F90 \
        profile/pneighbor_allgather_f08.F90 \
        profile/pneighbor_allgatherv_f08.F90 \
        profile/pneighbor_alltoall_f08.F90 \
        profile/pneighbor_alltoallv_f08.F90 \
        profile/pneighbor_alltoallw_f08.F90 \
        profile/pop_commutative_f08.F90 \
        profile/pop_create_f08.F90 \
        profile/popen_port_f08.F90 \
        profile/pop_free_f08.F90 \
        profile/ppack_external_f08.F90 \
        profile/ppack_external_size_f08.F90 \
        profile/ppack_f08.F90 \
        profile/ppack_size_f08.F90 \
        profile/ppcontrol_f08.F90 \
        profile/pprobe_f08.F90 \
        profile/ppublish_name_f08.F90 \
        profile/pput_f08.F90 \
        profile/pquery_thread_f08.F90 \
        profile/praccumulate_f08.F90 \
        profile/precv_f08.F90 \
        profile/precv_init_f08.F90 \
        profile/preduce_f08.F90 \
        profile/preduce_local_f08.F90 \
        profile/preduce_scatter_f08.F90 \
        profile/preduce_scatter_block_f08.F90 \
        profile/prequest_free_f08.F90 \
        profile/prequest_get_status_f08.F90 \
        profile/prget_f08.F90 \
        profile/prget_accumulate_f08.F90 \
        profile/prput_f08.F90 \
        profile/prsend_f08.F90 \
        profile/prsend_init_f08.F90 \
        profile/pscan_f08.F90 \
        profile/pscatter_f08.F90 \
        profile/pscatterv_f08.F90 \
        profile/psend_f08.F90 \
        profile/psend_init_f08.F90 \
        profile/psendrecv_f08.F90 \
        profile/psendrecv_replace_f08.F90 \
        profile/pssend_f08.F90 \
        profile/pssend_init_f08.F90 \
        profile/pstartall_f08.F90 \
        profile/pstart_f08.F90 \
        profile/pstatus_set_cancelled_f08.F90 \
        profile/pstatus_set_elements_f08.F90 \
        profile/pstatus_set_elements_x_f08.F90 \
        profile/ptestall_f08.F90 \
        profile/ptestany_f08.F90 \
        profile/ptest_cancelled_f08.F90 \
        profile/ptest_f08.F90 \
        profile/ptestsome_f08.F90 \
        profile/ptopo_test_f08.F90 \
        profile/ptype_commit_f08.F90 \
        profile/ptype_contiguous_f08.F90 \
        profile/ptype_create_darray_f08.F90 \
        profile/ptype_create_f90_complex_f08.F90 \
        profile/ptype_create_f90_integer_f08.F90 \
        profile/ptype_create_f90_real_f08.F90 \
        profile/ptype_create_hindexed_f08.F90 \
        profile/ptype_create_hvector_f08.F90 \
        profile/ptype_create_indexed_block_f08.F90 \
        profile/ptype_create_hindexed_block_f08.F90 \
        profile/ptype_create_keyval_f08.F90 \
        profile/ptype_create_resized_f08.F90 \
        profile/ptype_create_struct_f08.F90 \
        profile/ptype_create_subarray_f08.F90 \
        profile/ptype_delete_attr_f08.F90 \
        profile/ptype_dup_f08.F90 \
        profile/ptype_free_f08.F90 \
        profile/ptype_free_keyval_f08.F90 \
        profile/ptype_get_attr_f08.F90 \
        profile/ptype_get_contents_f08.F90 \
        profile/ptype_get_envelope_f08.F90 \
        profile/ptype_get_extent_f08.F90 \
        profile/ptype_get_extent_x_f08.F90 \
        profile/ptype_get_name_f08.F90 \
        profile/ptype_get_true_extent_f08.F90 \
        profile/ptype_get_true_extent_x_f08.F90 \
        profile/ptype_indexed_f08.F90 \
        profile/ptype_match_size_f08.F90 \
        profile/ptype_set_attr_f08.F90 \
        profile/ptype_set_name_f08.F90 \
        profile/ptype_size_f08.F90 \
        profile/ptype_size_x_f08.F90 \
        profile/ptype_vector_f08.F90 \
        profile/punpack_external_f08.F90 \
        profile/punpack_f08.F90 \
        profile/punpublish_name_f08.F90 \
        profile/pwaitall_f08.F90 \
        profile/pwaitany_f08.F90 \
        profile/pwait_f08.F90 \
        profile/pwaitsome_f08.F90 \
        profile/pwin_allocate_f08.F90 \
        profile/pwin_allocate_shared_f08.F90 \
        profile/pwin_attach_f08.F90 \
        profile/pwin_call_errhandler_f08.F90 \
        profile/pwin_complete_f08.F90 \
        profile/pwin_create_dynamic_f08.F90 \
        profile/pwin_create_errhandler_f08.F90 \
        profile/pwin_create_f08.F90 \
        profile/pwin_create_keyval_f08.F90 \
        profile/pwin_delete_attr_f08.F90 \
        profile/pwin_detach_f08.F90 \
        profile/pwin_fence_f08.F90 \
        profile/pwin_flush_f08.F90 \
        profile/pwin_flush_all_f08.F90 \
        profile/pwin_flush_local_f08.F90 \
        profile/pwin_flush_local_all_f08.F90 \
        profile/pwin_free_f08.F90 \
        profile/pwin_free_keyval_f08.F90 \
        profile/pwin_get_attr_f08.F90 \
        profile/pwin_get_errhandler_f08.F90 \
        profile/pwin_get_group_f08.F90 \
        profile/pwin_get_info_f08.F90 \
        profile/pwin_get_name_f08.F90 \
        profile/pwin_lock_f08.F90 \
        profile/pwin_lock_all_f08.F90 \
        profile/pwin_post_f08.F90 \
        profile/pwin_set_attr_f08.F90 \
        profile/pwin_set_errhandler_f08.F90 \
        profile/pwin_set_info_f08.F90 \
        profile/pwin_set_name_f08.F90 \
        profile/pwin_shared_query_f08.F90 \
        profile/pwin_start_f08.F90 \
        profile/pwin_sync_f08.F90 \
        profile/pwin_test_f08.F90 \
        profile/pwin_unlock_f08.F90 \
        profile/pwin_unlock_all_f08.F90 \
        profile/pwin_wait_f08.F90

if OMPI_PROVIDE_MPI_FILE_INTERFACE
pmpi_api_files += \
        profile/pfile_call_errhandler_f08.F90 \
        profile/pfile_close_f08.F90 \
        profile/pfile_create_errhandler_f08.F90 \
        profile/pfile_delete_f08.F90 \
        profile/pfile_get_amode_f08.F90 \
        profile/pfile_get_atomicity_f08.F90 \
        profile/pfile_get_byte_offset_f08.F90 \
        profile/pfile_get_errhandler_f08.F90 \
        profile/pfile_get_group_f08.F90 \
        profile/pfile_get_info_f08.F90 \
        profile/pfile_get_position_f08.F90 \
        profile/pfile_get_position_shared_f08.F90 \
        profile/pfile_get_size_f08.F90 \
        profile/pfile_get_type_extent_f08.F90 \
        profile/pfile_get_view_f08.F90 \
        profile/pfile_iread_at_f08.F90 \
        profile/pfile_iread_f08.F90 \
        profile/pfile_iread_shared_f08.F90 \
        profile/pfile_iwrite_at_f08.F90 \
        profile/pfile_iwrite_f08.F90 \
        profile/pfile_iwrite_shared_f08.F90 \
        profile/pfile_open_f08.F90 \
        profile/pfile_preallocate_f08.F90 \
        profile/pfile_read_all_begin_f08.F90 \
        profile/pfile_read_all_end_f08.F90 \
        profile/pfile_read_all_f08.F90 \
        profile/pfile_read_at_all_begin_f08.F90 \
        profile/pfile_read_at_all_end_f08.F90 \
        profile/pfile_read_at_all_f08.F90 \
        profile/pfile_read_at_f08.F90 \
        profile/pfile_read_f08.F90 \
        profile/pfile_read_ordered_begin_f08.F90 \
        profile/pfile_read_ordered_end_f08.F90 \
        profile/pfile_read_ordered_f08.F90 \
        profile/pfile_read_shared_f08.F90 \
        profile/pfile_seek_f08.F90 \
        profile/pfile_seek_shared_f08.F90 \
        profile/pfile_set_atomicity_f08.F90 \
        profile/pfile_set_errhandler_f08.F90 \
        profile/pfile_set_info_f08.F90 \
        profile/pfile_set_size_f08.F90 \
        profile/pfile_set_view_f08.F90 \
        profile/pfile_sync_f08.F90 \
        profile/pfile_write_all_begin_f08.F90 \
        profile/pfile_write_all_end_f08.F90 \
        profile/pfile_write_all_f08.F90 \
        profile/pfile_write_at_all_begin_f08.F90 \
        profile/pfile_write_at_all_end_f08.F90 \
        profile/pfile_write_at_all_f08.F90 \
        profile/pfile_write_at_f08.F90 \
        profile/pfile_write_f08.F90 \
        profile/pfile_write_ordered_begin_f08.F90 \
        profile/pfile_write_ordered_end_f08.F90 \
        profile/pfile_write_ordered_f08.F90 \
        profile/pfile_write_shared_f08.F90 \
        profile/pregister_datarep_f08.F90
endif

libmpi_usempif08_la_SOURCES = \
        $(mpi_api_files) \
        $(pmpi_api_files) \
        mpi-f-interfaces-bind.h \
        pmpi-f-interfaces-bind.h \
        mpi-f08.F90 \
        buffer_detach.c \
        constants.h \
        constants.c

# These are generated; do not ship them
nodist_libmpi_usempif08_la_SOURCES =

if BUILD_FORTRAN_SIZEOF
SIZEOF_H = sizeof_f08.h
nodist_libmpi_usempif08_la_SOURCES += \
        sizeof_f08.h \
        sizeof_f08.f90 \
        profile/psizeof_f08.f90
endif

#
# Include the mpi_f08-based MPI extensions in libmpi_usempif08, too.
#

libmpi_usempif08_la_LIBADD = \
        $(module_sentinel_file) \
        $(OMPI_MPIEXT_USEMPIF08_LIBS) \
        $(top_builddir)/ompi/mpi/fortran/mpif-h/libmpi_mpifh.la \
        $(top_builddir)/ompi/libmpi.la
libmpi_usempif08_la_DEPENDENCIES = $(module_sentinel_file)
libmpi_usempif08_la_LDFLAGS = -version-info $(libmpi_usempif08_so_version)

#
# Automake doesn't do Fortran dependency analysis, so must list them
# manually here.  Bummer!
#

mpi_api_lo_files = $(mpi_api_files:.F90=.lo)
pmpi_api_lo_files = $(pmpi_api_files:.F90=.lo)

$(mpi_api_lo_files): mpi-f08.lo
$(pmpi_api_lo_files): mpi-f08.lo

mpi-f08.lo: $(module_sentinel_file) $(SIZEOF_H)
mpi-f08.lo: mpi-f-interfaces-bind.h pmpi-f-interfaces-bind.h

###########################################################################

# f08 support modules

libforce_usempif08_internal_modules_to_be_built_la_SOURCES = \
        mpi-f08-types.F90 \
        mpi-f08-interfaces.F90 \
        mpi-f08-interfaces-callbacks.F90 \
        mpi-f08-callbacks.F90 \
        pmpi-f08-interfaces.F90

config_h = \
    $(top_builddir)/ompi/mpi/fortran/configure-fortran-output.h \
    $(top_srcdir)/ompi/mpi/fortran/configure-fortran-output-bottom.h

#
# Automake doesn't do Fortran dependency analysis, so must list them
# manually here.  Bummer!
#

mpi-f08-types.lo: $(config_h)
mpi-f08-types.lo: mpi-f08-types.F90
mpi-f08-interfaces.lo: $(config_h)
mpi-f08-interfaces.lo: mpi-f08-interfaces.F90
mpi-f08-interfaces.lo: mpi-f08-interfaces-callbacks.lo
mpi-f08-interfaces-callbacks.lo: $(config_h)
mpi-f08-interfaces-callbacks.lo: mpi-f08-interfaces-callbacks.F90
mpi-f08-interfaces-callbacks.lo: mpi-f08-types.lo
mpi-f08-callbacks.lo: $(config_h)
mpi-f08-callbacks.lo: mpi-f08-callbacks.F90
mpi-f08-callbacks.lo: mpi-f08-types.lo
pmpi-f08-interfaces.lo: $(config_h)
pmpi-f08-interfaces.lo: pmpi-f08-interfaces.F90
pmpi-f08-interfaces.lo: mpi-f08-interfaces-callbacks.lo

###########################################################################

# Install the generated .mod files.  Unfortunately, each F90 compiler
# may generate different filenames, so we have to use a glob.  :-(

install-exec-hook:
	@ for file in `ls *.mod`; do \
	  echo $(INSTALL) $$file $(DESTDIR)$(libdir); \
	  $(INSTALL) $$file $(DESTDIR)$(libdir); \
	done

uninstall-local:
	@ for file in `ls *.mod`; do \
	  echo rm -f $(DESTDIR)$(libdir)/$$file; \
	  rm -f $(DESTDIR)$(libdir)/$$file; \
	done
else

# Need to have empty targets because AM can't handle having an
# AM_CONDITIONAL was targets in the "if" statement but not in the
# "else".  :-(

install-exec-hook:
uninstall-local:

endif
