
# -*- makefile.am	 -*-
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
# Copyright (c) 2012      Oak Ridge National Laboratory. All rights reserved.
# Copyright (c) 2012-2013 Inria.  All rights reserved.
# Copyright (c) 2013      Los Alamos National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2015-2021 Research Organization for Information Science
#                         and Technology (RIST).  All rights reserved.
# Copyright (c) 2022      Triad National Security, LLC. All rights
#                         reserved.
# Copyright (c) 2024      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

include $(top_srcdir)/Makefile.ompi-rules

# Note that Automake's Fortran-buidling rules uses CPPFLAGS and
# AM_CPPFLAGS.  This can cause weirdness (e.g.,
# https://github.com/open-mpi/ompi/issues/7253).  Let's just zero
# those out and rely on AM_FCFLAGS.
CPPFLAGS =
AM_CPPFLAGS =

# This Makefile is only relevant if we're building the "use mpi_f08"
# MPI bindings.
if OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS

AM_FCFLAGS = -I$(top_srcdir)/ompi/mpi/fortran/use-mpi-f08/mod \
             -I$(top_builddir)/ompi/include \
             -I$(top_srcdir)/ompi/include \
             $(OMPI_FC_MODULE_FLAG)$(top_builddir)/ompi/mpi/fortran/use-mpi \
             $(OMPI_FC_MODULE_FLAG)$(top_builddir)/ompi/$(OMPI_FORTRAN_USEMPI_DIR) \
             $(OMPI_FC_MODULE_FLAG)../mod \
             $(OMPI_FC_MODULE_FLAG)../bindings \
             -I$(top_srcdir) -I$(top_builddir) $(FCFLAGS_f90) \
             -DOMPI_BUILD_MPI_PROFILING=1

CLEANFILES += *.i90

noinst_LTLIBRARIES = libmpi_usempif08_pmpi.la

pmpi_api_files = \
        pabort_f08.F90 \
        paccumulate_f08.F90 \
        padd_error_class_f08.F90 \
        padd_error_code_f08.F90 \
        padd_error_string_f08.F90 \
        paint_add_f08.F90 \
        paint_diff_f08.F90 \
        pallgather_f08.F90 \
        pallgather_init_f08.F90 \
        pallgatherv_f08.F90 \
        pallgatherv_init_f08.F90 \
        palloc_mem_f08.F90 \
        pallreduce_f08.F90 \
        pallreduce_init_f08.F90 \
        palltoall_f08.F90 \
        palltoall_init_f08.F90 \
        palltoallv_f08.F90 \
        palltoallv_init_f08.F90 \
        palltoallw_f08.F90 \
        palltoallw_init_f08.F90 \
        pbarrier_f08.F90 \
        pbarrier_init_f08.F90 \
        pbcast_f08.F90 \
        pbcast_init_f08.F90 \
        pbsend_f08.F90 \
        pbsend_init_f08.F90 \
        pbuffer_attach_f08.F90 \
        pbuffer_detach_f08.F90 \
        pcancel_f08.F90 \
        pcart_coords_f08.F90 \
        pcart_create_f08.F90 \
        pcartdim_get_f08.F90 \
        pcart_get_f08.F90 \
        pcart_map_f08.F90 \
        pcart_rank_f08.F90 \
        pcart_shift_f08.F90 \
        pcart_sub_f08.F90 \
        pclose_port_f08.F90 \
        pcomm_accept_f08.F90 \
        pcomm_call_errhandler_f08.F90 \
        pcomm_compare_f08.F90 \
        pcomm_connect_f08.F90 \
        pcomm_create_from_group_f08.F90 \
        pcomm_create_errhandler_f08.F90 \
        pcomm_create_f08.F90 \
        pcomm_create_group_f08.F90 \
        pcomm_create_keyval_f08.F90 \
        pcomm_delete_attr_f08.F90 \
        pcomm_disconnect_f08.F90 \
        pcomm_dup_f08.F90 \
        pcomm_dup_with_info_f08.F90 \
        pcomm_idup_f08.F90 \
        pcomm_idup_with_info_f08.F90 \
        pcomm_free_f08.F90 \
        pcomm_free_keyval_f08.F90 \
        pcomm_get_attr_f08.F90 \
        pcomm_get_errhandler_f08.F90 \
        pcomm_get_info_f08.F90 \
        pcomm_get_name_f08.F90 \
        pcomm_get_parent_f08.F90 \
        pcomm_group_f08.F90 \
        pcomm_join_f08.F90 \
        pcomm_rank_f08.F90 \
        pcomm_remote_group_f08.F90 \
        pcomm_remote_size_f08.F90 \
        pcomm_set_attr_f08.F90 \
        pcomm_set_errhandler_f08.F90 \
        pcomm_set_info_f08.F90 \
        pcomm_set_name_f08.F90 \
        pcomm_size_f08.F90 \
        pcomm_spawn_f08.F90 \
        pcomm_spawn_multiple_f08.F90 \
        pcomm_split_f08.F90 \
        pcomm_split_type_f08.F90 \
        pcomm_test_inter_f08.F90 \
        pcompare_and_swap_f08.F90 \
        pdims_create_f08.F90 \
        pdist_graph_create_adjacent_f08.F90 \
        pdist_graph_create_f08.F90 \
        pdist_graph_neighbors_count_f08.F90 \
        pdist_graph_neighbors_f08.F90 \
        perrhandler_free_f08.F90 \
        perror_class_f08.F90 \
        perror_string_f08.F90 \
        pexscan_f08.F90 \
        pexscan_init_f08.F90 \
        pf_sync_reg_f08.F90 \
        pfetch_and_op_f08.F90 \
        pfile_call_errhandler_f08.F90 \
        pfile_close_f08.F90 \
        pfile_create_errhandler_f08.F90 \
        pfile_delete_f08.F90 \
        pfile_get_amode_f08.F90 \
        pfile_get_atomicity_f08.F90 \
        pfile_get_byte_offset_f08.F90 \
        pfile_get_errhandler_f08.F90 \
        pfile_get_group_f08.F90 \
        pfile_get_info_f08.F90 \
        pfile_get_position_f08.F90 \
        pfile_get_position_shared_f08.F90 \
        pfile_get_size_f08.F90 \
        pfile_get_type_extent_f08.F90 \
        pfile_get_view_f08.F90 \
        pfile_iread_at_f08.F90 \
        pfile_iread_f08.F90 \
        pfile_iread_at_all_f08.F90 \
        pfile_iread_all_f08.F90 \
        pfile_iread_shared_f08.F90 \
        pfile_iwrite_at_f08.F90 \
        pfile_iwrite_f08.F90 \
        pfile_iwrite_at_all_f08.F90 \
        pfile_iwrite_all_f08.F90 \
        pfile_iwrite_shared_f08.F90 \
        pfile_open_f08.F90 \
        pfile_preallocate_f08.F90 \
        pfile_read_all_begin_f08.F90 \
        pfile_read_all_end_f08.F90 \
        pfile_read_all_f08.F90 \
        pfile_read_at_all_begin_f08.F90 \
        pfile_read_at_all_end_f08.F90 \
        pfile_read_at_all_f08.F90 \
        pfile_read_at_f08.F90 \
        pfile_read_f08.F90 \
        pfile_read_ordered_begin_f08.F90 \
        pfile_read_ordered_end_f08.F90 \
        pfile_read_ordered_f08.F90 \
        pfile_read_shared_f08.F90 \
        pfile_seek_f08.F90 \
        pfile_seek_shared_f08.F90 \
        pfile_set_atomicity_f08.F90 \
        pfile_set_errhandler_f08.F90 \
        pfile_set_info_f08.F90 \
        pfile_set_size_f08.F90 \
        pfile_set_view_f08.F90 \
        pfile_sync_f08.F90 \
        pfile_write_all_begin_f08.F90 \
        pfile_write_all_end_f08.F90 \
        pfile_write_all_f08.F90 \
        pfile_write_at_all_begin_f08.F90 \
        pfile_write_at_all_end_f08.F90 \
        pfile_write_at_all_f08.F90 \
        pfile_write_at_f08.F90 \
        pfile_write_f08.F90 \
        pfile_write_ordered_begin_f08.F90 \
        pfile_write_ordered_end_f08.F90 \
        pfile_write_ordered_f08.F90 \
        pfile_write_shared_f08.F90 \
        pfinalized_f08.F90 \
        pfinalize_f08.F90 \
        pfree_mem_f08.F90 \
        pgather_f08.F90 \
        pgather_init_f08.F90 \
        pgatherv_f08.F90 \
        pgatherv_init_f08.F90 \
        pget_accumulate_f08.F90 \
        pget_address_f08.F90 \
        pget_count_f08.F90 \
        pget_elements_f08.F90 \
        pget_elements_x_f08.F90 \
        pget_f08.F90 \
        pget_library_version_f08.F90 \
        pget_processor_name_f08.F90 \
        pget_version_f08.F90 \
        pgraph_create_f08.F90 \
        pgraphdims_get_f08.F90 \
        pgraph_get_f08.F90 \
        pgraph_map_f08.F90 \
        pgraph_neighbors_count_f08.F90 \
        pgraph_neighbors_f08.F90 \
        pgrequest_complete_f08.F90 \
        pgrequest_start_f08.F90 \
        pgroup_compare_f08.F90 \
        pgroup_difference_f08.F90 \
        pgroup_excl_f08.F90 \
        pgroup_free_f08.F90 \
        pgroup_from_session_pset_f08.F90 \
        pgroup_incl_f08.F90 \
        pgroup_intersection_f08.F90 \
        pgroup_range_excl_f08.F90 \
        pgroup_range_incl_f08.F90 \
        pgroup_rank_f08.F90 \
        pgroup_size_f08.F90 \
        pgroup_translate_ranks_f08.F90 \
        pgroup_union_f08.F90 \
        piallgather_f08.F90 \
        piallgatherv_f08.F90 \
        piallreduce_f08.F90 \
        pialltoall_f08.F90 \
        pialltoallv_f08.F90 \
        pialltoallw_f08.F90 \
        pibarrier_f08.F90 \
        pibcast_f08.F90 \
        pibsend_f08.F90 \
        pigather_f08.F90 \
        pigatherv_f08.F90 \
        piexscan_f08.F90 \
        pimprobe_f08.F90 \
        pimrecv_f08.F90 \
        pineighbor_allgather_f08.F90 \
        pineighbor_allgatherv_f08.F90 \
        pineighbor_alltoall_f08.F90 \
        pineighbor_alltoallv_f08.F90 \
        pineighbor_alltoallw_f08.F90 \
        pinfo_create_f08.F90 \
        pinfo_create_env_f08.F90 \
        pinfo_delete_f08.F90 \
        pinfo_dup_f08.F90 \
        pinfo_free_f08.F90 \
        pinfo_get_f08.F90 \
        pinfo_get_nkeys_f08.F90 \
        pinfo_get_nthkey_f08.F90 \
        pinfo_get_string_f08.F90 \
        pinfo_get_valuelen_f08.F90 \
        pinfo_set_f08.F90 \
        pinit_f08.F90 \
        pinitialized_f08.F90 \
        pinit_thread_f08.F90 \
        pintercomm_create_f08.F90 \
        pintercomm_create_from_groups_f08.F90 \
        pintercomm_merge_f08.F90 \
        piprobe_f08.F90 \
        pirecv_f08.F90 \
        pireduce_f08.F90 \
        pireduce_scatter_f08.F90 \
        pireduce_scatter_block_f08.F90 \
        pirsend_f08.F90 \
        piscan_f08.F90 \
        piscatter_f08.F90 \
        piscatterv_f08.F90 \
        pisend_f08.F90 \
        pisendrecv_f08.F90 \
        pisendrecv_replace_f08.F90 \
        pissend_f08.F90 \
        pis_thread_main_f08.F90 \
        plookup_name_f08.F90 \
        pmprobe_f08.F90 \
        pmrecv_f08.F90 \
        pneighbor_allgather_f08.F90 \
        pneighbor_allgather_init_f08.F90 \
        pneighbor_allgatherv_f08.F90 \
        pneighbor_allgatherv_init_f08.F90 \
        pneighbor_alltoall_f08.F90 \
        pneighbor_alltoall_init_f08.F90 \
        pneighbor_alltoallv_f08.F90 \
        pneighbor_alltoallv_init_f08.F90 \
        pneighbor_alltoallw_f08.F90 \
        pneighbor_alltoallw_init_f08.F90 \
        pop_commutative_f08.F90 \
        pop_create_f08.F90 \
        popen_port_f08.F90 \
        pop_free_f08.F90 \
        ppack_external_f08.F90 \
        ppack_external_size_f08.F90 \
        ppack_f08.F90 \
        ppack_size_f08.F90 \
        pparrived_f08.F90 \
        ppcontrol_f08.F90 \
        ppready_f08.F90 \
        ppready_list_f08.F90 \
        ppready_range_f08.F90 \
        pprobe_f08.F90 \
        ppsend_init_f08.F90 \
        ppublish_name_f08.F90 \
        pput_f08.F90 \
        pquery_thread_f08.F90 \
        praccumulate_f08.F90 \
        precv_f08.F90 \
        precv_init_f08.F90 \
        preduce_f08.F90 \
        preduce_init_f08.F90 \
        preduce_local_f08.F90 \
        preduce_scatter_f08.F90 \
        preduce_scatter_init_f08.F90 \
        preduce_scatter_block_f08.F90 \
        preduce_scatter_block_init_f08.F90 \
        pregister_datarep_f08.F90 \
        prequest_free_f08.F90 \
        prequest_get_status_f08.F90 \
        prget_f08.F90 \
        prget_accumulate_f08.F90 \
        prput_f08.F90 \
        prsend_f08.F90 \
        prsend_init_f08.F90 \
        pscan_f08.F90 \
        pscan_init_f08.F90 \
        pscatter_f08.F90 \
        pscatter_init_f08.F90 \
        pscatterv_f08.F90 \
        pscatterv_init_f08.F90 \
        psend_f08.F90 \
        psend_init_f08.F90 \
        psendrecv_f08.F90 \
        psendrecv_replace_f08.F90 \
        psession_call_errhandler_f08.F90 \
        psession_create_errhandler_f08.F90\
        psession_get_errhandler_f08.F90\
        psession_get_info_f08.F90 \
        psession_get_nth_pset_f08.F90 \
        psession_get_num_psets_f08.F90 \
        psession_get_pset_info_f08.F90 \
        psession_init_f08.F90 \
        psession_finalize_f08.F90 \
        psession_set_errhandler_f08.F90\
        pssend_f08.F90 \
        pssend_init_f08.F90 \
        pstartall_f08.F90 \
        pstart_f08.F90 \
        pstatus_f082f_f08.F90 \
        pstatus_f2f08_f08.F90 \
        pstatus_set_cancelled_f08.F90 \
        pstatus_set_elements_f08.F90 \
        pstatus_set_elements_x_f08.F90 \
        ptestall_f08.F90 \
        ptestany_f08.F90 \
        ptest_cancelled_f08.F90 \
        ptest_f08.F90 \
        ptestsome_f08.F90 \
        ptopo_test_f08.F90 \
        ptype_commit_f08.F90 \
        ptype_contiguous_f08.F90 \
        ptype_create_darray_f08.F90 \
        ptype_create_f90_complex_f08.F90 \
        ptype_create_f90_integer_f08.F90 \
        ptype_create_f90_real_f08.F90 \
        ptype_create_hindexed_f08.F90 \
        ptype_create_hvector_f08.F90 \
        ptype_create_indexed_block_f08.F90 \
        ptype_create_hindexed_block_f08.F90 \
        ptype_create_keyval_f08.F90 \
        ptype_create_resized_f08.F90 \
        ptype_create_struct_f08.F90 \
        ptype_create_subarray_f08.F90 \
        ptype_delete_attr_f08.F90 \
        ptype_dup_f08.F90 \
        ptype_free_f08.F90 \
        ptype_free_keyval_f08.F90 \
        ptype_get_attr_f08.F90 \
        ptype_get_contents_f08.F90 \
        ptype_get_envelope_f08.F90 \
        ptype_get_extent_f08.F90 \
        ptype_get_extent_x_f08.F90 \
        ptype_get_name_f08.F90 \
        ptype_get_true_extent_f08.F90 \
        ptype_get_true_extent_x_f08.F90 \
        ptype_indexed_f08.F90 \
        ptype_match_size_f08.F90 \
        ptype_set_attr_f08.F90 \
        ptype_set_name_f08.F90 \
        ptype_size_f08.F90 \
        ptype_size_x_f08.F90 \
        ptype_vector_f08.F90 \
        punpack_external_f08.F90 \
        punpack_f08.F90 \
        punpublish_name_f08.F90 \
        pwaitall_f08.F90 \
        pwaitany_f08.F90 \
        pwait_f08.F90 \
        pwaitsome_f08.F90 \
        pwin_allocate_f08.F90 \
        pwin_allocate_shared_f08.F90 \
        pwin_attach_f08.F90 \
        pwin_call_errhandler_f08.F90 \
        pwin_complete_f08.F90 \
        pwin_create_dynamic_f08.F90 \
        pwin_create_errhandler_f08.F90 \
        pwin_create_f08.F90 \
        pwin_create_keyval_f08.F90 \
        pwin_delete_attr_f08.F90 \
        pwin_detach_f08.F90 \
        pwin_fence_f08.F90 \
        pwin_flush_f08.F90 \
        pwin_flush_all_f08.F90 \
        pwin_flush_local_f08.F90 \
        pwin_flush_local_all_f08.F90 \
        pwin_free_f08.F90 \
        pwin_free_keyval_f08.F90 \
        pwin_get_attr_f08.F90 \
        pwin_get_errhandler_f08.F90 \
        pwin_get_group_f08.F90 \
        pwin_get_info_f08.F90 \
        pwin_get_name_f08.F90 \
        pwin_lock_f08.F90 \
        pwin_lock_all_f08.F90 \
        pwin_post_f08.F90 \
        pwin_set_attr_f08.F90 \
        pwin_set_errhandler_f08.F90 \
        pwin_set_info_f08.F90 \
        pwin_set_name_f08.F90 \
        pwin_shared_query_f08.F90 \
        pwin_start_f08.F90 \
        pwin_sync_f08.F90 \
        pwin_test_f08.F90 \
        pwin_unlock_f08.F90 \
        pwin_unlock_all_f08.F90 \
        pwin_wait_f08.F90

#
# Automake doesn't do Fortran dependency analysis, so must list them
# manually here.  Bummer!
#

pmpi_api_lo_files = $(pmpi_api_files:.F90=.lo)

$(pmpi_api_lo_files): ../bindings/libforce_usempif08_internal_bindings_to_be_built.la

nodist_libmpi_usempif08_pmpi_la_SOURCES = \
        $(pmpi_api_files)

#
# Sym link in the sources from the real MPI directory
#
$(nodist_libmpi_usempif08_pmpi_la_SOURCES):
	$(OMPI_V_LN_S) if test ! -r $@ ; then \
		pname=`echo $@ | cut -b '2-'` ; \
		$(LN_S) $(top_srcdir)/ompi/mpi/fortran/use-mpi-f08/$$pname $@ ; \
	fi

# These files were created by targets above

MAINTAINERCLEANFILES = $(nodist_libmpi_usempif08_pmpi_la_SOURCES)

endif
