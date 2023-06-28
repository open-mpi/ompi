#
# Copyright (c) 2022 Cisco Systems, Inc.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# We need this Makefile to be executed serially.  Below, we list all
# the man pages as the targets of the rule that invokes Sphinx for
# dependency/generation reasons.  But a *single* execution of the make
# target will generate *all* of the man pages and HTML files.  Hence,
# when "make" determines that none of the man page files exist, it
# should execute the Sphinx-invocation rule once, and then it will
# realize that all the man pages files exist.  More specifically: if
# someone invokes "make -j N", we need make to not execute the
# Sphinx-invocation rule multiple times simultaneously.  Both GNU Make
# and BSD Make will honor the .NOTPARALLEL target to disable all
# parallel invocation in this Makefile[.am].
#
# Note that even though we explicitly disable make's parallelism,
# we'll use Sphinx's internal parallelism via "-j auto" -- see
# SPHINX_OPTS.
.NOTPARALLEL:

OUTDIR             = _build
SPHINX_CONFIG      = conf.py
SPHINX_OPTS       ?= -W --keep-going -j auto

# Note: it is significantly more convenient to list all the source
# files here using wildcards (vs. listing every single .rst file).
# However, it is necessary to list $(srcdir) when using wildcards.
TEXT_SOURCE_FILES  = \
        $(srcdir)/license/*.txt
IMAGE_SOURCE_FILES = \
        $(srcdir)/openmpi_logo.png \
        $(srcdir)/installing-open-mpi/required-support-libraries-dependency-graph.png
RST_SOURCE_FILES   = \
        $(srcdir)/*.rst \
        $(srcdir)/release-notes/*.rst \
        $(srcdir)/installing-open-mpi/*.rst \
        $(srcdir)/installing-open-mpi/*/*.rst \
        $(srcdir)/features/*.rst \
        $(srcdir)/building-apps/*.rst \
        $(srcdir)/launching-apps/*.rst \
        $(srcdir)/tuning-apps/*.rst \
        $(srcdir)/tuning-apps/*/*.rst \
        $(srcdir)/app-debug/*.rst \
        $(srcdir)/developers/*.rst \
        $(srcdir)/license/*.rst \
        $(srcdir)/news/*.rst \
        $(srcdir)/man-openmpi/*.rst \
        $(srcdir)/man-openmpi/man*/*.rst \
        $(srcdir)/man-openshmem/*.rst \
        $(srcdir)/man-openshmem/man*/*.rst

EXTRA_DIST         = \
        requirements.txt \
        $(SPHINX_CONFIG) \
        $(TEXT_SOURCE_FILES) \
        $(IMAGE_SOURCE_FILES) \
        $(RST_SOURCE_FILES)

###########################################################################

# Note: we list all the man pages explicitly (as opposed to using
# wildcards, like we do to list all the RST source files in
# EXTRA_DIST) because these files get installed by Automake.  There
# are less complications and portability issues (between GNU and BSD
# make(1), for example) if all the files to be installed are
# explicitly listed.

OMPI_MAN1 = \
        ompi-wrapper-compiler.1 \
        mpirun.1 \
        mpisync.1 \
        ompi_info.1 \
        opal_wrapper.1

OMPI_MAN3 = \
        MPI_Abort.3 \
        MPI_Accumulate.3 \
        MPI_Add_error_class.3 \
        MPI_Add_error_code.3 \
        MPI_Add_error_string.3 \
        MPI_Address.3 \
        MPI_Aint_add.3 \
        MPI_Aint_diff.3 \
        MPI_Allgather.3 \
        MPI_Allgather_init.3 \
        MPI_Allgatherv.3 \
        MPI_Allgatherv_init.3 \
        MPI_Alloc_mem.3 \
        MPI_Allreduce.3 \
        MPI_Allreduce_init.3 \
        MPI_Alltoall.3 \
        MPI_Alltoall_init.3 \
        MPI_Alltoallv.3 \
        MPI_Alltoallv_init.3 \
        MPI_Alltoallw.3 \
        MPI_Alltoallw_init.3 \
        MPI_Attr_delete.3 \
        MPI_Attr_get.3 \
        MPI_Attr_put.3 \
        MPI_Barrier.3 \
        MPI_Barrier_init.3 \
        MPI_Bcast.3 \
        MPI_Bcast_init.3 \
        MPI_Bsend.3 \
        MPI_Bsend_init.3 \
        MPI_Buffer_attach.3 \
        MPI_Buffer_detach.3 \
        MPI_Cancel.3 \
        MPI_Cart_coords.3 \
        MPI_Cart_create.3 \
        MPI_Cartdim_get.3 \
        MPI_Cart_get.3 \
        MPI_Cart_map.3 \
        MPI_Cart_rank.3 \
        MPI_Cart_shift.3 \
        MPI_Cart_sub.3 \
        MPI_Close_port.3 \
        MPI_Comm_accept.3 \
        MPI_Comm_c2f.3 \
        MPI_Comm_call_errhandler.3 \
        MPI_Comm_compare.3 \
        MPI_Comm_connect.3 \
        MPI_Comm_create.3 \
        MPI_Comm_create_errhandler.3 \
        MPI_Comm_create_from_group.3 \
        MPI_Comm_create_group.3 \
        MPI_Comm_create_keyval.3 \
        MPI_Comm_delete_attr.3 \
        MPI_Comm_disconnect.3 \
        MPI_Comm_dup.3 \
        MPI_Comm_dup_with_info.3 \
        MPI_Comm_f2c.3 \
        MPI_Comm_free.3 \
        MPI_Comm_free_keyval.3 \
        MPI_Comm_get_attr.3 \
        MPI_Comm_get_errhandler.3 \
        MPI_Comm_get_info.3 \
        MPI_Comm_get_name.3 \
        MPI_Comm_get_parent.3 \
        MPI_Comm_group.3 \
        MPI_Comm_idup.3 \
        MPI_Comm_idup_with_info.3 \
        MPI_Comm_join.3 \
        MPI_Comm_rank.3 \
        MPI_Comm_remote_group.3 \
        MPI_Comm_remote_size.3 \
        MPI_Comm_set_attr.3 \
        MPI_Comm_set_errhandler.3 \
        MPI_Comm_set_info.3 \
        MPI_Comm_set_name.3 \
        MPI_Comm_size.3 \
        MPI_Comm_spawn.3 \
        MPI_Comm_spawn_multiple.3 \
        MPI_Comm_split.3 \
        MPI_Comm_split_type.3 \
        MPI_Comm_test_inter.3 \
        MPI_Compare_and_swap.3 \
        MPI_Dims_create.3 \
        MPI_Dist_graph_create.3 \
        MPI_Dist_graph_create_adjacent.3 \
        MPI_Dist_graph_neighbors.3 \
        MPI_Dist_graph_neighbors_count.3 \
        MPI_Errhandler_create.3 \
        MPI_Errhandler_free.3 \
        MPI_Errhandler_get.3 \
        MPI_Errhandler_set.3 \
        MPI_Error_class.3 \
        MPI_Error_string.3 \
        MPI_Exscan.3 \
        MPI_Exscan_init.3 \
        MPI_Fetch_and_op.3 \
        MPI_File_c2f.3 \
        MPI_File_call_errhandler.3 \
        MPI_File_close.3 \
        MPI_File_create_errhandler.3 \
        MPI_File_delete.3 \
        MPI_File_f2c.3 \
        MPI_File_get_amode.3 \
        MPI_File_get_atomicity.3 \
        MPI_File_get_byte_offset.3 \
        MPI_File_get_errhandler.3 \
        MPI_File_get_group.3 \
        MPI_File_get_info.3 \
        MPI_File_get_position.3 \
        MPI_File_get_position_shared.3 \
        MPI_File_get_size.3 \
        MPI_File_get_type_extent.3 \
        MPI_File_get_view.3 \
        MPI_File_iread.3 \
        MPI_File_iread_all.3 \
        MPI_File_iread_at.3 \
        MPI_File_iread_at_all.3 \
        MPI_File_iread_shared.3 \
        MPI_File_iwrite.3 \
        MPI_File_iwrite_all.3 \
        MPI_File_iwrite_at.3 \
        MPI_File_iwrite_at_all.3 \
        MPI_File_iwrite_shared.3 \
        MPI_File_open.3 \
        MPI_File_preallocate.3 \
        MPI_File_read.3 \
        MPI_File_read_all.3 \
        MPI_File_read_all_begin.3 \
        MPI_File_read_all_end.3 \
        MPI_File_read_at.3 \
        MPI_File_read_at_all.3 \
        MPI_File_read_at_all_begin.3 \
        MPI_File_read_at_all_end.3 \
        MPI_File_read_ordered.3 \
        MPI_File_read_ordered_begin.3 \
        MPI_File_read_ordered_end.3 \
        MPI_File_read_shared.3 \
        MPI_File_seek.3 \
        MPI_File_seek_shared.3 \
        MPI_File_set_atomicity.3 \
        MPI_File_set_errhandler.3 \
        MPI_File_set_info.3 \
        MPI_File_set_size.3 \
        MPI_File_set_view.3 \
        MPI_File_sync.3 \
        MPI_File_write.3 \
        MPI_File_write_all.3 \
        MPI_File_write_all_begin.3 \
        MPI_File_write_all_end.3 \
        MPI_File_write_at.3 \
        MPI_File_write_at_all.3 \
        MPI_File_write_at_all_begin.3 \
        MPI_File_write_at_all_end.3 \
        MPI_File_write_ordered.3 \
        MPI_File_write_ordered_begin.3 \
        MPI_File_write_ordered_end.3 \
        MPI_File_write_shared.3 \
        MPI_Finalize.3 \
        MPI_Finalized.3 \
        MPI_Free_mem.3 \
        MPI_Gather.3 \
        MPI_Gather_init.3 \
        MPI_Gatherv.3 \
        MPI_Gatherv_init.3 \
        MPI_Get.3 \
        MPI_Get_accumulate.3 \
        MPI_Get_address.3 \
        MPI_Get_count.3 \
        MPI_Get_elements.3 \
        MPI_Get_elements_x.3 \
        MPI_Get_library_version.3 \
        MPI_Get_processor_name.3 \
        MPI_Get_version.3 \
        MPI_Graph_create.3 \
        MPI_Graphdims_get.3 \
        MPI_Graph_get.3 \
        MPI_Graph_map.3 \
        MPI_Graph_neighbors.3 \
        MPI_Graph_neighbors_count.3 \
        MPI_Grequest_complete.3 \
        MPI_Grequest_start.3 \
        MPI_Group_c2f.3 \
        MPI_Group_compare.3 \
        MPI_Group_difference.3 \
        MPI_Group_excl.3 \
        MPI_Group_f2c.3 \
        MPI_Group_free.3 \
        MPI_Group_from_session_pset.3 \
        MPI_Group_incl.3 \
        MPI_Group_intersection.3 \
        MPI_Group_range_excl.3 \
        MPI_Group_range_incl.3 \
        MPI_Group_rank.3 \
        MPI_Group_size.3 \
        MPI_Group_translate_ranks.3 \
        MPI_Group_union.3 \
        MPI_Iallgather.3 \
        MPI_Iallgatherv.3 \
        MPI_Iallreduce.3 \
        MPI_Ialltoall.3 \
        MPI_Ialltoallv.3 \
        MPI_Ialltoallw.3 \
        MPI_Ibarrier.3 \
        MPI_Ibcast.3 \
        MPI_Ibsend.3 \
        MPI_Iexscan.3 \
        MPI_Igather.3 \
        MPI_Igatherv.3 \
        MPI_Improbe.3 \
        MPI_Imrecv.3 \
        MPI_Ineighbor_allgather.3 \
        MPI_Ineighbor_allgatherv.3 \
        MPI_Ineighbor_alltoall.3 \
        MPI_Ineighbor_alltoallv.3 \
        MPI_Ineighbor_alltoallw.3 \
        MPI_Info_c2f.3 \
        MPI_Info_create.3 \
        MPI_Info_delete.3 \
        MPI_Info_dup.3 \
        MPI_Info_env.3 \
        MPI_Info_f2c.3 \
        MPI_Info_free.3 \
        MPI_Info_get.3 \
        MPI_Info_get_nkeys.3 \
        MPI_Info_get_nthkey.3 \
        MPI_Info_get_string.3 \
        MPI_Info_get_valuelen.3 \
        MPI_Info_set.3 \
        MPI_Init.3 \
        MPI_Initialized.3 \
        MPI_Init_thread.3 \
        MPI_Intercomm_create.3 \
        MPI_Intercomm_create_from_groups.3 \
        MPI_Intercomm_merge.3 \
        MPI_Iprobe.3 \
        MPI_Irecv.3 \
        MPI_Ireduce.3 \
        MPI_Ireduce_scatter.3 \
        MPI_Ireduce_scatter_block.3 \
        MPI_Irsend.3 \
        MPI_Iscan.3 \
        MPI_Iscatter.3 \
        MPI_Iscatterv.3 \
        MPI_Isend.3 \
        MPI_Isendrecv.3 \
        MPI_Isendrecv_replace.3 \
        MPI_Issend.3 \
        MPI_Is_thread_main.3 \
        MPI_Keyval_create.3 \
        MPI_Keyval_free.3 \
        MPI_Lookup_name.3 \
        MPI_Message_c2f.3 \
        MPI_Message_f2c.3 \
        MPI_Mprobe.3 \
        MPI_Mrecv.3 \
        MPI_Neighbor_allgather.3 \
        MPI_Neighbor_allgather_init.3 \
        MPI_Neighbor_allgatherv.3 \
        MPI_Neighbor_allgatherv_init.3 \
        MPI_Neighbor_alltoall.3 \
        MPI_Neighbor_alltoall_init.3 \
        MPI_Neighbor_alltoallv.3 \
        MPI_Neighbor_alltoallv_init.3 \
        MPI_Neighbor_alltoallw.3 \
        MPI_Neighbor_alltoallw_init.3 \
        MPI_Op_c2f.3 \
        MPI_Op_commutative.3 \
        MPI_Op_create.3 \
        MPI_Open_port.3 \
        MPI_Op_f2c.3 \
        MPI_Op_free.3 \
        MPI_Pack.3 \
        MPI_Pack_external.3 \
        MPI_Pack_external_size.3 \
        MPI_Pack_size.3 \
        MPI_Parrived.3 \
        MPI_Pcontrol.3 \
        MPI_Pready.3 \
        MPI_Pready_list.3 \
        MPI_Pready_range.3 \
        MPI_Precv_init.3 \
        MPI_Probe.3 \
        MPI_Psend_init.3 \
        MPI_Publish_name.3 \
        MPI_Put.3 \
        MPI_Query_thread.3 \
        MPI_Raccumulate.3 \
        MPI_Recv.3 \
        MPI_Recv_init.3 \
        MPI_Reduce.3 \
        MPI_Reduce_init.3 \
        MPI_Reduce_local.3 \
        MPI_Reduce_scatter.3 \
        MPI_Reduce_scatter_block.3 \
        MPI_Reduce_scatter_block_init.3 \
        MPI_Reduce_scatter_init.3 \
        MPI_Register_datarep.3 \
        MPI_Request_c2f.3 \
        MPI_Request_f2c.3 \
        MPI_Request_free.3 \
        MPI_Request_get_status.3 \
        MPI_Rget.3 \
        MPI_Rget_accumulate.3 \
        MPI_Rput.3 \
        MPI_Rsend.3 \
        MPI_Rsend_init.3 \
        MPI_Scan.3 \
        MPI_Scan_init.3 \
        MPI_Scatter.3 \
        MPI_Scatter_init.3 \
        MPI_Scatterv.3 \
        MPI_Scatterv_init.3 \
        MPI_Send.3 \
        MPI_Send_init.3 \
        MPI_Sendrecv.3 \
        MPI_Sendrecv_replace.3 \
        MPI_Session_create_errhandler.3 \
        MPI_Session_f2c.3 \
        MPI_Session_finalize.3 \
        MPI_Session_get_info.3 \
        MPI_Session_get_nth_pset.3 \
        MPI_Session_get_num_psets.3 \
        MPI_Session_get_pset_info.3 \
        MPI_Session_init.3 \
        MPI_Sizeof.3 \
        MPI_Ssend.3 \
        MPI_Ssend_init.3 \
        MPI_Start.3 \
        MPI_Startall.3 \
        MPI_Status_c2f08.3 \
        MPI_Status_c2f.3 \
        MPI_Status_f082c.3 \
        MPI_Status_f082f.3 \
        MPI_Status_f2c.3 \
        MPI_Status_f2f08.3 \
        MPI_Status_set_cancelled.3 \
        MPI_Status_set_elements.3 \
        MPI_Status_set_elements_x.3 \
        MPI_T.3 \
        MPI_T_category_changed.3 \
        MPI_T_category_get_categories.3 \
        MPI_T_category_get_cvars.3 \
        MPI_T_category_get_info.3 \
        MPI_T_category_get_num.3 \
        MPI_T_category_get_pvars.3 \
        MPI_T_cvar_get_info.3 \
        MPI_T_cvar_get_num.3 \
        MPI_T_cvar_handle_alloc.3 \
        MPI_T_cvar_handle_free.3 \
        MPI_T_cvar_read.3 \
        MPI_T_cvar_write.3 \
        MPI_T_enum_get_info.3 \
        MPI_T_enum_get_item.3 \
        MPI_Test.3 \
        MPI_Testall.3 \
        MPI_Testany.3 \
        MPI_Test_cancelled.3 \
        MPI_Testsome.3 \
        MPI_T_finalize.3 \
        MPI_T_init_thread.3 \
        MPI_Topo_test.3 \
        MPI_T_pvar_get_info.3 \
        MPI_T_pvar_get_num.3 \
        MPI_T_pvar_handle_alloc.3 \
        MPI_T_pvar_handle_free.3 \
        MPI_T_pvar_read.3 \
        MPI_T_pvar_readreset.3 \
        MPI_T_pvar_reset.3 \
        MPI_T_pvar_session_create.3 \
        MPI_T_pvar_session_free.3 \
        MPI_T_pvar_start.3 \
        MPI_T_pvar_stop.3 \
        MPI_T_pvar_write.3 \
        MPI_Type_c2f.3 \
        MPI_Type_commit.3 \
        MPI_Type_contiguous.3 \
        MPI_Type_create_darray.3 \
        MPI_Type_create_f90_complex.3 \
        MPI_Type_create_f90_integer.3 \
        MPI_Type_create_f90_real.3 \
        MPI_Type_create_hindexed.3 \
        MPI_Type_create_hindexed_block.3 \
        MPI_Type_create_hvector.3 \
        MPI_Type_create_indexed_block.3 \
        MPI_Type_create_keyval.3 \
        MPI_Type_create_resized.3 \
        MPI_Type_create_struct.3 \
        MPI_Type_create_subarray.3 \
        MPI_Type_delete_attr.3 \
        MPI_Type_dup.3 \
        MPI_Type_extent.3 \
        MPI_Type_f2c.3 \
        MPI_Type_free.3 \
        MPI_Type_free_keyval.3 \
        MPI_Type_get_attr.3 \
        MPI_Type_get_contents.3 \
        MPI_Type_get_envelope.3 \
        MPI_Type_get_extent.3 \
        MPI_Type_get_extent_x.3 \
        MPI_Type_get_name.3 \
        MPI_Type_get_true_extent.3 \
        MPI_Type_get_true_extent_x.3 \
        MPI_Type_hindexed.3 \
        MPI_Type_hvector.3 \
        MPI_Type_indexed.3 \
        MPI_Type_lb.3 \
        MPI_Type_match_size.3 \
        MPI_Type_set_attr.3 \
        MPI_Type_set_name.3 \
        MPI_Type_size.3 \
        MPI_Type_size_x.3 \
        MPI_Type_struct.3 \
        MPI_Type_ub.3 \
        MPI_Type_vector.3 \
        MPI_Unpack.3 \
        MPI_Unpack_external.3 \
        MPI_Unpublish_name.3 \
        MPI_Wait.3 \
        MPI_Waitall.3 \
        MPI_Waitany.3 \
        MPI_Waitsome.3 \
        MPI_Win_allocate.3 \
        MPI_Win_allocate_shared.3 \
        MPI_Win_attach.3 \
        MPI_Win_c2f.3 \
        MPI_Win_call_errhandler.3 \
        MPI_Win_complete.3 \
        MPI_Win_create.3 \
        MPI_Win_create_dynamic.3 \
        MPI_Win_create_errhandler.3 \
        MPI_Win_create_keyval.3 \
        MPI_Win_delete_attr.3 \
        MPI_Win_detach.3 \
        MPI_Win_f2c.3 \
        MPI_Win_fence.3 \
        MPI_Win_flush.3 \
        MPI_Win_flush_all.3 \
        MPI_Win_flush_local.3 \
        MPI_Win_flush_local_all.3 \
        MPI_Win_free.3 \
        MPI_Win_free_keyval.3 \
        MPI_Win_get_attr.3 \
        MPI_Win_get_errhandler.3 \
        MPI_Win_get_group.3 \
        MPI_Win_get_info.3 \
        MPI_Win_get_name.3 \
        MPI_Win_lock.3 \
        MPI_Win_lock_all.3 \
        MPI_Win_post.3 \
        MPI_Win_set_attr.3 \
        MPI_Win_set_errhandler.3 \
        MPI_Win_set_info.3 \
        MPI_Win_set_name.3 \
        MPI_Win_shared_query.3 \
        MPI_Win_start.3 \
        MPI_Win_sync.3 \
        MPI_Win_test.3 \
        MPI_Win_unlock.3 \
        MPI_Win_unlock_all.3 \
        MPI_Win_wait.3 \
        MPI_Wtick.3 \
        MPI_Wtime.3 \
        MPIX_Comm_ack_failed.3 \
        MPIX_Comm_agree.3 \
        MPIX_Comm_get_failed.3 \
        MPIX_Comm_iagree.3 \
        MPIX_Comm_ishrink.3 \
        MPIX_Comm_is_revoked.3 \
        MPIX_Comm_revoke.3 \
        MPIX_Comm_shrink.3 \
        MPIX_Query_cuda_support.3 \
        MPIX_Query_rocm_support.3 \
        OMPI_Affinity_str.3

OMPI_MAN7 = \
        Open-MPI.7

OSHMEM_MAN1 = \
        oshmem-wrapper-compiler.1 \
        oshmem_info.1

OSHMEM_MAN3 = \
        intro_shmem.3 \
        _my_pe.3 \
        _num_pes.3 \
        OpenSHMEM.3 \
        shfree.3 \
        shmalloc.3 \
        shmem_addr_accessible.3 \
        shmem_align.3 \
        shmemalign.3 \
        shmem_alltoall32.3 \
        shmem_alltoall64.3 \
        shmem_alltoalls32.3 \
        shmem_alltoalls64.3 \
        shmem_barrier.3 \
        shmem_barrier_all.3 \
        shmem_broadcast32.3 \
        shmem_broadcast64.3 \
        shmem_char_g.3 \
        shmem_char_get.3 \
        shmem_char_get_nbi.3 \
        shmem_char_p.3 \
        shmem_char_put.3 \
        shmem_char_put_nbi.3 \
        shmem_clear_cache_inv.3 \
        shmem_clear_cache_line_inv.3 \
        shmem_clear_lock.3 \
        shmem_collect32.3 \
        shmem_collect64.3 \
        shmem_complexd_prod_to_all.3 \
        shmem_complexd_sum_to_all.3 \
        shmem_complexf_prod_to_all.3 \
        shmem_complexf_sum_to_all.3 \
        shmem_double_fetch.3 \
        shmem_double_g.3 \
        shmem_double_get.3 \
        shmem_double_get_nbi.3 \
        shmem_double_iget.3 \
        shmem_double_iput.3 \
        shmem_double_max_to_all.3 \
        shmem_double_min_to_all.3 \
        shmem_double_p.3 \
        shmem_double_prod_to_all.3 \
        shmem_double_put.3 \
        shmem_double_put_nbi.3 \
        shmem_double_set.3 \
        shmem_double_sum_to_all.3 \
        shmem_double_swap.3 \
        shmem_fcollect32.3 \
        shmem_fcollect64.3 \
        shmem_fence.3 \
        shmem_finalize.3 \
        shmem_float_fetch.3 \
        shmem_float_g.3 \
        shmem_float_get.3 \
        shmem_float_get_nbi.3 \
        shmem_float_iget.3 \
        shmem_float_iput.3 \
        shmem_float_max_to_all.3 \
        shmem_float_min_to_all.3 \
        shmem_float_p.3 \
        shmem_float_prod_to_all.3 \
        shmem_float_put.3 \
        shmem_float_put_nbi.3 \
        shmem_float_set.3 \
        shmem_float_sum_to_all.3 \
        shmem_float_swap.3 \
        shmem_free.3 \
        shmem_get128.3 \
        shmem_get128_nbi.3 \
        shmem_get16_nbi.3 \
        shmem_get32.3 \
        shmem_get32_nbi.3 \
        shmem_get64.3 \
        shmem_get64_nbi.3 \
        shmem_get8_nbi.3 \
        shmem_getmem.3 \
        shmem_getmem_nbi.3 \
        shmem_global_exit.3 \
        shmem_iget128.3 \
        shmem_iget32.3 \
        shmem_iget64.3 \
        shmem_info_get_name.3 \
        shmem_info_get_version.3 \
        shmem_init.3 \
        shmem_int_add.3 \
        shmem_int_and_to_all.3 \
        shmem_int_cswap.3 \
        shmem_int_fadd.3 \
        shmem_int_fetch.3 \
        shmem_int_finc.3 \
        shmem_int_g.3 \
        shmem_int_get.3 \
        shmem_int_get_nbi.3 \
        shmem_int_iget.3 \
        shmem_int_inc.3 \
        shmem_int_iput.3 \
        shmem_int_max_to_all.3 \
        shmem_int_min_to_all.3 \
        shmem_int_or_to_all.3 \
        shmem_int_p.3 \
        shmem_int_prod_to_all.3 \
        shmem_int_put.3 \
        shmem_int_put_nbi.3 \
        shmem_int_set.3 \
        shmem_int_sum_to_all.3 \
        shmem_int_swap.3 \
        shmem_int_wait.3 \
        shmem_int_wait_until.3 \
        shmem_int_xor_to_all.3 \
        shmem_iput128.3 \
        shmem_iput32.3 \
        shmem_iput64.3 \
        shmem_long_add.3 \
        shmem_long_and_to_all.3 \
        shmem_long_cswap.3 \
        shmem_longdouble_g.3 \
        shmem_longdouble_get.3 \
        shmem_longdouble_get_nbi.3 \
        shmem_longdouble_iget.3 \
        shmem_longdouble_iput.3 \
        shmem_longdouble_max_to_all.3 \
        shmem_longdouble_min_to_all.3 \
        shmem_longdouble_p.3 \
        shmem_longdouble_prod_to_all.3 \
        shmem_longdouble_put.3 \
        shmem_longdouble_put_nbi.3 \
        shmem_longdouble_sum_to_all.3 \
        shmem_long_fadd.3 \
        shmem_long_fetch.3 \
        shmem_long_finc.3 \
        shmem_long_g.3 \
        shmem_long_get.3 \
        shmem_long_get_nbi.3 \
        shmem_long_iget.3 \
        shmem_long_inc.3 \
        shmem_long_iput.3 \
        shmem_longlong_add.3 \
        shmem_longlong_and_to_all.3 \
        shmem_longlong_cswap.3 \
        shmem_longlong_fadd.3 \
        shmem_longlong_fetch.3 \
        shmem_longlong_finc.3 \
        shmem_longlong_g.3 \
        shmem_longlong_get.3 \
        shmem_longlong_get_nbi.3 \
        shmem_longlong_iget.3 \
        shmem_longlong_inc.3 \
        shmem_longlong_iput.3 \
        shmem_longlong_max_to_all.3 \
        shmem_longlong_min_to_all.3 \
        shmem_longlong_or_to_all.3 \
        shmem_longlong_p.3 \
        shmem_longlong_prod_to_all.3 \
        shmem_longlong_put.3 \
        shmem_longlong_put_nbi.3 \
        shmem_longlong_set.3 \
        shmem_longlong_sum_to_all.3 \
        shmem_longlong_swap.3 \
        shmem_longlong_wait.3 \
        shmem_longlong_wait_until.3 \
        shmem_longlong_xor_to_all.3 \
        shmem_long_max_to_all.3 \
        shmem_long_min_to_all.3 \
        shmem_long_or_to_all.3 \
        shmem_long_p.3 \
        shmem_long_prod_to_all.3 \
        shmem_long_put.3 \
        shmem_long_put_nbi.3 \
        shmem_long_set.3 \
        shmem_long_sum_to_all.3 \
        shmem_long_swap.3 \
        shmem_long_wait.3 \
        shmem_long_wait_until.3 \
        shmem_long_xor_to_all.3 \
        shmem_malloc.3 \
        shmem_my_pe.3 \
        shmem_n_pes.3 \
        shmem_pe_accessible.3 \
        shmem_ptr.3 \
        shmem_put128.3 \
        shmem_put128_nbi.3 \
        shmem_put16_nbi.3 \
        shmem_put32.3 \
        shmem_put32_nbi.3 \
        shmem_put64.3 \
        shmem_put64_nbi.3 \
        shmem_put8_nbi.3 \
        shmem_putmem.3 \
        shmem_putmem_nbi.3 \
        shmem_quiet.3 \
        shmem_realloc.3 \
        shmem_set_cache_inv.3 \
        shmem_set_cache_line_inv.3 \
        shmem_set_lock.3 \
        shmem_short_and_to_all.3 \
        shmem_short_g.3 \
        shmem_short_get.3 \
        shmem_short_get_nbi.3 \
        shmem_short_iget.3 \
        shmem_short_iput.3 \
        shmem_short_max_to_all.3 \
        shmem_short_min_to_all.3 \
        shmem_short_or_to_all.3 \
        shmem_short_p.3 \
        shmem_short_prod_to_all.3 \
        shmem_short_put.3 \
        shmem_short_put_nbi.3 \
        shmem_short_sum_to_all.3 \
        shmem_short_wait.3 \
        shmem_short_wait_until.3 \
        shmem_short_xor_to_all.3 \
        shmem_swap.3 \
        shmem_test_lock.3 \
        shmem_udcflush.3 \
        shmem_udcflush_line.3 \
        shmem_wait.3 \
        shmem_wait_until.3 \
        shrealloc.3 \
        start_pes.3

MAN_OUTDIR = $(OUTDIR)/man

OMPI_MAN1_RST = $(OMPI_MAN1:%.1=man-openmpi/man1/%.1.rst)
OMPI_MAN1_BUILT = $(OMPI_MAN1:%.1=$(MAN_OUTDIR)/%.1)

OMPI_MAN3_RST = $(OMPI_MAN3:%.3=man-openmpi/man3/%.3.rst)
OMPI_MAN3_BUILT = $(OMPI_MAN3:%.3=$(MAN_OUTDIR)/%.3)

OMPI_MAN7_RST = $(OMPI_MAN7:%.7=man-openmpi/man7/%.7.rst)
OMPI_MAN7_BUILT = $(OMPI_MAN7:%.7=$(MAN_OUTDIR)/%.7)

OSHMEM_MAN1_RST = $(OSHMEM_MAN1:%.1=man-oshmem/man1/%.1.rst)
OSHMEM_MAN1_BUILT = $(OSHMEM_MAN1:%.1=$(MAN_OUTDIR)/%.1)

OSHMEM_MAN3_RST = $(OSHMEM_MAN3:%.3=man-oshmem/man3/%.3.rst)
OSHMEM_MAN3_BUILT = $(OSHMEM_MAN3:%.3=$(MAN_OUTDIR)/%.3)

EXTRA_DIST += \
        $(OMPI_MAN1_BUILT) \
        $(OMPI_MAN3_BUILT) \
        $(OMPI_MAN7_BUILT) \
        $(OSHMEM_MAN1_BUILT) \
        $(OSHMEM_MAN3_BUILT)

###########################################################################

# These files are not generated by Sphinx (they are redirects to
# Sphinx-generated man pages) -- they exist directly in Git.  We
# *always* want these files to be in EXTRA_DIST (so that they're in
# distribution tarballs).  We'll decide later whether we install each
# of these (e.g., if configure disabled the Java bindings, we won't
# install mpijavac.1).
OMPI_MAN1_C_REDIRECTS = man-openmpi/man1/mpicc.1
OMPI_MAN1_CXX_REDIRECTS = \
        man-openmpi/man1/mpicxx.1 \
        man-openmpi/man1/mpic++.1
OMPI_MAN1_FORTRAN_REDIRECTS = \
        man-openmpi/man1/mpifort.1 \
        man-openmpi/man1/mpif77.1 \
        man-openmpi/man1/mpif90.1
OMPI_MAN1_JAVA_REDIRECTS = man-openmpi/man1/mpijavac.1

OSHMEM_MAN1_C_REDIRECTS = \
        man-openshmem/man1/oshcc.1 \
        man-openshmem/man1/shmemcc.1
OSHMEM_MAN1_CXX_REDIRECTS = \
        man-openshmem/man1/oshcxx.1 \
        man-openshmem/man1/oshc++.1 \
        man-openshmem/man1/shmemcxx.1 \
        man-openshmem/man1/shmemc++.1
OSHMEM_MAN1_FORTRAN_REDIRECTS = \
        man-openshmem/man1/oshfort.1 \
        man-openshmem/man1/shmemfort.1

EXTRA_DIST += \
        $(OMPI_MAN1_C_REDIRECTS) \
        $(OMPI_MAN1_CXX_REDIRECTS) \
        $(OMPI_MAN1_FORTRAN_REDIRECTS) \
        $(OMPI_MAN1_JAVA_REDIRECTS) \
        $(OSHMEM_MAN1_C_REDIRECTS) \
        $(OSHMEM_MAN1_CXX_REDIRECTS) \
        $(OSHMEM_MAN1_FORTRAN_REDIRECTS)

###########################################################################
if OPAL_BUILD_DOCS

include $(top_srcdir)/Makefile.ompi-rules

# Have to not list these targets in EXTRA_DIST outside of the
# OPAL_BUILD_DOCS conditional because "make dist" will fail due to
# these missing targets (and therefore not run the "dist-hook" target
# in the top-level Makefile, which prints a pretty message about why
# "make dist" failed).
#
# We list the entire directory trees (html and man) to grab all
# generated files in them.
EXTRA_DIST += \
        $(OUTDIR)/html \
        $(OUTDIR)/man

ALL_MAN_BUILT = \
        $(OMPI_MAN1_BUILT) $(OMPI_MAN3_BUILT) $(OMPI_MAN7_BUILT) \
        $(OSHMEM_MAN1_BUILT) $(OSHMEM_MAN_3_BUILT)
$(ALL_MAN_BUILT): $(RST_SOURCE_FILES) $(IMAGE_SOURCE_FILES)
$(ALL_MAN_BUILT): $(TEXT_SOURCE_FILES) $(SPHINX_CONFIG)

# List both commands (HTML and man) in a single rule because they
# really need to be run in serial.  Specifically, if they were two
# different rules and someone ran "make -j", then both of them could
# be writing to $(OUTDIR)/doctrees simultaneously, which would be Bad.
# Use one of the man pages as a sentinel file to indicate whether all
# the HTML docs and man pages have been built.
$(ALL_MAN_BUILT):
	$(OMPI_V_SPHINX_HTML) $(SPHINX_BUILD) -M html "$(srcdir)" "$(OUTDIR)" $(SPHINX_OPTS)
	$(OMPI_V_SPHINX_MAN) $(SPHINX_BUILD) -M man "$(srcdir)" "$(OUTDIR)" $(SPHINX_OPTS)

# A useful rule to invoke manually to ensure that all of the external
# HTML links we have are valid.  Running this rule requires
# connectivity to the general internet.
linkcheck:
	$(SPHINX_BUILD) -M linkcheck "$(srcdir)" "$(OUTDIR)" $(SPHINX_OPTS)

.PHONY: linkcheck

maintainer-clean-local:
	$(SPHINX_BUILD) -M clean "$(srcdir)" "$(OUTDIR)" $(SPHINX_OPTS)

# List all the built man pages here in the Automake BUILT_SOURCES
# macro.  This hooks into the normal Automake build mechanisms, and
# will ultimately cause the invocation of the above rule that runs
# Sphinx to build the HTML and man pages.
BUILT_SOURCES = $(ALL_MAN_BUILT)

endif OPAL_BUILD_DOCS

###########################################################################
if OPAL_INSTALL_DOCS

man1_MANS = \
        $(OMPI_MAN1_BUILT) \
        $(OMPI_MAN1_C_REDIRECTS)
if OMPI_HAVE_CXX_COMPILER
man1_MANS += $(OMPI_MAN1_CXX_REDIRECTS)
endif
if OMPI_BUILD_ANY_FORTRAN_BINDINGS
man1_MANS += $(OMPI_MAN1_FORTRAN_REDIRECTS)
endif
if OMPI_WANT_JAVA_BINDINGS
man1_MANS += $(OMPI_MAN1_JAVA_REDIRECTS)
endif

man3_MANS = $(OMPI_MAN3_BUILT)
man7_MANS = $(OMPI_MAN7_BUILT)

if PROJECT_OSHMEM
man1_MANS += \
        $(OSHMEM_MAN1_BUILT) \
        $(OSHMEM_MAN1_C_REDIRECTS)
# There is no OSHMEM equivalent of this conditional; just use the OMPI
# conditional.
if OMPI_HAVE_CXX_COMPILER
man1_MANS += $(OSHMEM_MAN1_CXX_REDIRECTS)
endif
if OSHMEM_BUILD_FORTRAN_BINDINGS
man1_MANS += $(OSHMEM_MAN1_FORTRAN_REDIRECTS)
endif

man3_MANS += $(OSHMEM_MAN3_BUILT)
endif

# We do not know the names of all the generated HTML files: we only
# know that the generated tree will of files be in _build/html/.
# Unfortunately, Automake's installation process only handles
# individual files -- not entire trees of files.  Hence, we use a
# "find ..." to copy the entire generated tree to the target
# directory.
#
# Note: we do not use "cp -r ..." because if the source directory is
# write-protected (e.g., during "make distcheck"), then the cp -r will
# also write-protect the target directory.  Instead, use the
# Automake-provided install macros to set desirable permissions on the
# target directories and files.
#
# Since this might be a VPATH build, first check to see if _build/html
# exists in the source tree.  If not, do the find+install from the
# build tree.
install-data-hook:
	$(MKDIR_P) $(DESTDIR)$(docdir)
	if test -d $(srcdir)/_build/html; then \
	    topdir=$(srcdir)/_build; \
	else \
	    topdir=_build; \
	fi; \
	cd $$topdir; \
	find html -type d -exec $(mkinstalldirs) $(DESTDIR)$(docdir)/{} \; ; \
	find html -type f -exec $(INSTALL_DATA) {} $(DESTDIR)$(docdir)/{} \;

uninstall-hook:
	rm -rf $(DESTDIR)$(docdir)

endif OPAL_INSTALL_DOCS
