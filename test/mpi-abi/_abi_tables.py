#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""Static probe case tables and callback API-name sets (pure
data consumed by the probe and check layers)."""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _abi_common import (
    SKIP_DATAREP_UNSUPPORTED, SKIP_MPIT_EVENTS_UNAVAILABLE)


# Installed C probes are small source fragments inserted into
# templates/c_probe.c.in, compiled with mpicc_abi, and run with mpirun.
# Each case becomes its own executable because MPI process state after an
# error is not generally reusable.  Generated failures use fixed nonzero
# exit codes below 256 and print ABI_FAIL details to stderr; POSIX exit
# status truncation would otherwise turn return 256 into a false PASS.
# Keep the fragments self-contained.  Python fills only generated
# constant lists, exact-count expectations, and rank-count placeholders.
INSTALLED_C_ABI_PROBES = (
    {
        "name": "abi_version",
        "rank_count": 1,
        "body_file": "cases/c-abi/abi_version.cbody.in",
    },
    {
        "name": "init_finalize",
        "rank_count": 1,
        "body_file": "cases/c-abi/init_finalize.cbody.in",
    },
    {
        "name": "barrier_two_rank",
        "rank_count": 2,
        "body_file": "cases/c-abi/barrier_two_rank.cbody.in",
    },
    {
        "name": "sendrecv_two_rank",
        "rank_count": 2,
        "body_file": "cases/c-abi/sendrecv_two_rank.cbody.in",
    },
    {
        "name": "converter_predefined_handles",
        "rank_count": 1,
        "body_file": "cases/c-abi/converter_predefined_handles.cbody.in",
    },
    {
        "name": "converter_fortran_datatypes",
        "rank_count": 1,
        "requires_fortran": True,
        "body_file": "cases/c-abi/converter_fortran_datatypes.cbody.in",
    },
    {
        "name": "converter_dynamic_handles",
        "rank_count": 1,
        "body_file": "cases/c-abi/converter_dynamic_handles.cbody.in",
    },
    {
        "name": "converter_status_sentinels",
        "rank_count": 1,
        "body_file": "cases/c-abi/converter_status_sentinels.cbody.in",
    },
    {
        "name": "converter_error_keyvals",
        "rank_count": 1,
        "body_file": "cases/c-abi/converter_error_keyvals.cbody.in",
    },
)


# Phase 9 runtime API-family probes are intentionally separate from the
# converter probes above.  Each logical API-family check is still one
# generated source, one executable, and one mpirun launch: if an MPI API
# call fails, MPI process state and communicator/request lifetime are not
# portable enough to continue running unrelated checks in that process.
#
# The api_names field is a coverage contract against docs/ metadata.  It
# records the primary APIs this probe is meant to exercise; helper calls
# used for setup/teardown can be added to support_api_names when they are
# useful diagnostics but should not count as the main family coverage.
INSTALLED_C_RUNTIME_API_PROBES = (
    {
        "name": "runtime_init_state",
        "family": "init",
        "rank_count": 1,
        "api_names": (
            "MPI_Finalize",
            "MPI_Finalized",
            "MPI_Init",
            "MPI_Initialized",
        ),
        "body_file": "cases/c-runtime/runtime_init_state.cbody.in",
    },
    {
        "name": "runtime_init_thread",
        "family": "init",
        "rank_count": 1,
        "api_names": (
            "MPI_Init_thread",
            "MPI_Is_thread_main",
            "MPI_Query_thread",
        ),
        "support_api_names": (
            "MPI_Finalize",
        ),
        "body_file": "cases/c-runtime/runtime_init_thread.cbody.in",
    },
    {
        "name": "runtime_session_core",
        "family": "session",
        "rank_count": 1,
        "api_names": (
            "MPI_Session_call_errhandler",
            "MPI_Session_finalize",
            "MPI_Session_fromint",
            "MPI_Session_get_errhandler",
            "MPI_Session_get_info",
            "MPI_Session_get_nth_pset",
            "MPI_Session_get_num_psets",
            "MPI_Session_get_pset_info",
            "MPI_Session_init",
            "MPI_Session_set_errhandler",
            "MPI_Session_toint",
        ),
        "support_api_names": (
            "MPI_Errhandler_free",
            "MPI_Info_free",
        ),
        "body_file": "cases/c-runtime/runtime_session_core.cbody.in",
    },
    {
        "name": "runtime_session_buffer",
        "family": "session",
        "rank_count": 1,
        "api_names": (
            "MPI_Session_attach_buffer",
            "MPI_Session_detach_buffer",
            "MPI_Session_finalize",
            "MPI_Session_flush_buffer",
            "MPI_Session_iflush_buffer",
            "MPI_Session_init",
        ),
        "support_api_names": (
            "MPI_Wait",
        ),
        "body_file": "cases/c-runtime/runtime_session_buffer.cbody.in",
    },
    {
        "name": "runtime_session_group",
        "family": "session",
        "rank_count": 1,
        "api_names": (
            "MPI_Group_from_session_pset",
        ),
        "support_api_names": (
            "MPI_Group_free",
            "MPI_Group_size",
            "MPI_Session_finalize",
            "MPI_Session_get_nth_pset",
            "MPI_Session_get_num_psets",
            "MPI_Session_init",
        ),
        "body_file": "cases/c-runtime/runtime_session_group.cbody.in",
    },
    {
        "name": "runtime_comm_basic",
        "family": "comm",
        "rank_count": 1,
        "api_names": (
            "MPI_Comm_compare",
            "MPI_Comm_dup",
            "MPI_Comm_dup_with_info",
            "MPI_Comm_free",
            "MPI_Comm_get_info",
            "MPI_Comm_get_name",
            "MPI_Comm_idup",
            "MPI_Comm_idup_with_info",
            "MPI_Comm_rank",
            "MPI_Comm_set_info",
            "MPI_Comm_set_name",
            "MPI_Comm_size",
        ),
        "support_api_names": (
            "MPI_Finalize",
            "MPI_Info_create",
            "MPI_Info_free",
            "MPI_Info_set",
            "MPI_Init",
            "MPI_Wait",
        ),
        "body_file": "cases/c-runtime/runtime_comm_basic.cbody.in",
    },
    {
        "name": "runtime_comm_create_split",
        "family": "comm",
        "rank_count": 2,
        "api_names": (
            "MPI_Comm_create",
            "MPI_Comm_create_group",
            "MPI_Comm_free",
            "MPI_Comm_group",
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Comm_split",
            "MPI_Comm_split_type",
        ),
        "support_api_names": (
            "MPI_Finalize",
            "MPI_Group_free",
            "MPI_Group_incl",
            "MPI_Init",
        ),
        "body_file": "cases/c-runtime/runtime_comm_create_split.cbody.in",
    },
    {
        "name": "runtime_intercomm_group",
        "family": "comm",
        "rank_count": 2,
        "api_names": (
            "MPI_Comm_create_from_group",
            "MPI_Comm_remote_group",
            "MPI_Comm_remote_size",
            "MPI_Comm_test_inter",
            "MPI_Intercomm_create",
            "MPI_Intercomm_create_from_groups",
            "MPI_Intercomm_merge",
        ),
        "support_api_names": (
            "MPI_Comm_free",
            "MPI_Comm_group",
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Comm_split",
            "MPI_Finalize",
            "MPI_Group_free",
            "MPI_Group_incl",
            "MPI_Group_size",
            "MPI_Init",
        ),
        "body_file": "cases/c-runtime/runtime_intercomm_group.cbody.in",
    },
    {
        "name": "runtime_comm_errhandler_basic",
        "family": "comm",
        "rank_count": 1,
        "api_names": (
            "MPI_Comm_call_errhandler",
            "MPI_Comm_get_errhandler",
            "MPI_Comm_set_errhandler",
        ),
        "support_api_names": (
            "MPI_Errhandler_free",
            "MPI_Finalize",
            "MPI_Init",
        ),
        "body_file": "cases/c-runtime/runtime_comm_errhandler_basic.cbody.in",
    },
    {
        "name": "runtime_comm_attr_predefined",
        "family": "comm",
        "rank_count": 1,
        "api_names": (
            "MPI_Comm_get_attr",
        ),
        "support_api_names": (
            "MPI_Finalize",
            "MPI_Init",
        ),
        "body_file": "cases/c-runtime/runtime_comm_attr_predefined.cbody.in",
    },
    {
        "name": "runtime_comm_buffer",
        "family": "comm",
        "rank_count": 2,
        "api_names": (
            "MPI_Comm_attach_buffer",
            "MPI_Comm_detach_buffer",
            "MPI_Comm_flush_buffer",
            "MPI_Comm_iflush_buffer",
        ),
        "support_api_names": (
            "MPI_Bsend",
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Recv",
            "MPI_Wait",
        ),
        "body_file": "cases/c-runtime/runtime_comm_buffer.cbody.in",
    },
    {
        "name": "runtime_group_setops",
        "family": "group",
        "rank_count": 1,
        "api_names": (
            "MPI_Group_compare",
            "MPI_Group_difference",
            "MPI_Group_excl",
            "MPI_Group_free",
            "MPI_Group_incl",
            "MPI_Group_intersection",
            "MPI_Group_range_excl",
            "MPI_Group_range_incl",
            "MPI_Group_rank",
            "MPI_Group_size",
            "MPI_Group_translate_ranks",
            "MPI_Group_union",
        ),
        "support_api_names": (
            "MPI_Comm_group",
            "MPI_Finalize",
            "MPI_Init",
        ),
        "body_file": "cases/c-runtime/runtime_group_setops.cbody.in",
    },
    {
        "name": "runtime_point_to_point_basic",
        "family": "point_to_point",
        "rank_count": 2,
        "api_names": (
            "MPI_Irecv",
            "MPI_Isend",
            "MPI_Recv",
            "MPI_Send",
        ),
        "support_api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Get_count",
            "MPI_Init",
            "MPI_Wait",
        ),
        "body_file": "cases/c-runtime/runtime_point_to_point_basic.cbody.in",
    },
    {
        "name": "runtime_point_to_point_probe",
        "family": "point_to_point",
        "rank_count": 2,
        "api_names": (
            "MPI_Iprobe",
            "MPI_Probe",
        ),
        "support_api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Get_count",
            "MPI_Init",
            "MPI_Recv",
            "MPI_Send",
        ),
        "body_file": "cases/c-runtime/runtime_point_to_point_probe.cbody.in",
    },
    {
        "name": "runtime_point_to_point_persistent",
        "family": "point_to_point",
        "rank_count": 2,
        "api_names": (
            "MPI_Recv_init",
            "MPI_Send_init",
        ),
        "support_api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Request_free",
            "MPI_Start",
            "MPI_Startall",
            "MPI_Wait",
        ),
        "body_file": "cases/c-runtime/runtime_point_to_point_persistent.cbody.in",
    },
    {
        "name": "runtime_point_to_point_send_modes",
        "family": "point_to_point",
        "rank_count": 2,
        "api_names": (
            "MPI_Bsend",
            "MPI_Bsend_init",
            "MPI_Buffer_attach",
            "MPI_Buffer_detach",
            "MPI_Buffer_flush",
            "MPI_Buffer_iflush",
            "MPI_Ibsend",
            "MPI_Irsend",
            "MPI_Issend",
            "MPI_Rsend",
            "MPI_Rsend_init",
            "MPI_Ssend",
            "MPI_Ssend_init",
        ),
        "support_api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Irecv",
            "MPI_Recv",
            "MPI_Request_free",
            "MPI_Send",
            "MPI_Start",
            "MPI_Wait",
        ),
        "body_file": "cases/c-runtime/runtime_point_to_point_send_modes.cbody.in",
    },
    {
        "name": "runtime_point_to_point_matched_message",
        "family": "point_to_point",
        "rank_count": 2,
        "api_names": (
            "MPI_Improbe",
            "MPI_Imrecv",
            "MPI_Message_fromint",
            "MPI_Message_toint",
            "MPI_Mprobe",
            "MPI_Mrecv",
            "MPI_Sendrecv",
            "MPI_Sendrecv_replace",
        ),
        "support_api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Get_count",
            "MPI_Init",
            "MPI_Send",
            "MPI_Wait",
        ),
        "body_file": "cases/c-runtime/runtime_point_to_point_matched_message.cbody.in",
    },
    {
        "name": "runtime_point_to_point_isendrecv",
        "family": "point_to_point",
        "rank_count": 2,
        "api_names": (
            "MPI_Isendrecv",
            "MPI_Isendrecv_replace",
        ),
        "support_api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Wait",
        ),
        "body_file": "cases/c-runtime/runtime_point_to_point_isendrecv.cbody.in",
    },
    {
        "name": "runtime_point_to_point_partitioned",
        "family": "point_to_point",
        "rank_count": 2,
        "api_names": (
            "MPI_Parrived",
            "MPI_Pready",
            "MPI_Pready_list",
            "MPI_Pready_range",
            "MPI_Precv_init",
            "MPI_Psend_init",
        ),
        "support_api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Recv",
            "MPI_Request_free",
            "MPI_Send",
            "MPI_Start",
            "MPI_Wait",
        ),
        "body_file": "cases/c-runtime/runtime_point_to_point_partitioned.cbody.in",
    },
    {
        "name": "runtime_request_lifecycle",
        "family": "request",
        "rank_count": 2,
        "api_names": (
            "MPI_Cancel",
            "MPI_Request_free",
            "MPI_Request_fromint",
            "MPI_Request_get_status",
            "MPI_Request_get_status_all",
            "MPI_Request_get_status_any",
            "MPI_Request_get_status_some",
            "MPI_Request_toint",
            "MPI_Test",
            "MPI_Test_cancelled",
            "MPI_Testall",
            "MPI_Testany",
            "MPI_Testsome",
            "MPI_Wait",
            "MPI_Waitall",
            "MPI_Waitany",
            "MPI_Waitsome",
        ),
        "support_api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Irecv",
            "MPI_Isend",
            "MPI_Recv_init",
        ),
        "body_file": "cases/c-runtime/runtime_request_lifecycle.cbody.in",
    },
    {
        "name": "runtime_topology_cart_graph",
        "family": "topology",
        "rank_count": 2,
        "api_names": (
            "MPI_Cart_coords",
            "MPI_Cart_create",
            "MPI_Cart_get",
            "MPI_Cart_map",
            "MPI_Cart_rank",
            "MPI_Cart_shift",
            "MPI_Cart_sub",
            "MPI_Cartdim_get",
            "MPI_Dims_create",
            "MPI_Graph_create",
            "MPI_Graph_get",
            "MPI_Graph_map",
            "MPI_Graph_neighbors",
            "MPI_Graph_neighbors_count",
            "MPI_Graphdims_get",
            "MPI_Topo_test",
        ),
        "support_api_names": (
            "MPI_Comm_free",
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
        ),
        "body_file": "cases/c-runtime/runtime_topology_cart_graph.cbody.in",
    },
    {
        "name": "runtime_topology_dist_neighbor",
        "family": "topology",
        "rank_count": 2,
        "api_names": (
            "MPI_Dist_graph_create",
            "MPI_Dist_graph_create_adjacent",
            "MPI_Dist_graph_neighbors",
            "MPI_Dist_graph_neighbors_count",
            "MPI_Ineighbor_allgather",
            "MPI_Ineighbor_allgatherv",
            "MPI_Ineighbor_alltoall",
            "MPI_Ineighbor_alltoallv",
            "MPI_Ineighbor_alltoallw",
            "MPI_Neighbor_allgather",
            "MPI_Neighbor_allgather_init",
            "MPI_Neighbor_allgatherv",
            "MPI_Neighbor_allgatherv_init",
            "MPI_Neighbor_alltoall",
            "MPI_Neighbor_alltoall_init",
            "MPI_Neighbor_alltoallv",
            "MPI_Neighbor_alltoallv_init",
            "MPI_Neighbor_alltoallw",
            "MPI_Neighbor_alltoallw_init",
        ),
        "support_api_names": (
            "MPI_Comm_free",
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Request_free",
            "MPI_Start",
            "MPI_Wait",
        ),
        "body_file": "cases/c-runtime/runtime_topology_dist_neighbor.cbody.in",
    },
    {
        "name": "runtime_collective_basic",
        "family": "collective",
        "rank_count": 2,
        "api_names": (
            "MPI_Allreduce",
            "MPI_Barrier",
            "MPI_Bcast",
            "MPI_Reduce",
        ),
        "support_api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
        ),
        "body_file": "cases/c-runtime/runtime_collective_basic.cbody.in",
    },
    {
        "name": "runtime_collective_blocking",
        "family": "collective",
        "rank_count": 2,
        "api_names": (
            "MPI_Allgather",
            "MPI_Allgatherv",
            "MPI_Alltoall",
            "MPI_Alltoallv",
            "MPI_Alltoallw",
            "MPI_Exscan",
            "MPI_Gather",
            "MPI_Gatherv",
            "MPI_Reduce_local",
            "MPI_Reduce_scatter",
            "MPI_Reduce_scatter_block",
            "MPI_Scan",
            "MPI_Scatter",
            "MPI_Scatterv",
        ),
        "support_api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
        ),
        "body_file": "cases/c-runtime/runtime_collective_blocking.cbody.in",
    },
    {
        "name": "runtime_collective_nonblocking",
        "family": "collective",
        "rank_count": 2,
        "api_names": (
            "MPI_Iallgather",
            "MPI_Iallgatherv",
            "MPI_Iallreduce",
            "MPI_Ialltoall",
            "MPI_Ialltoallv",
            "MPI_Ialltoallw",
            "MPI_Ibarrier",
            "MPI_Ibcast",
            "MPI_Iexscan",
            "MPI_Igather",
            "MPI_Igatherv",
            "MPI_Ireduce",
            "MPI_Ireduce_scatter",
            "MPI_Ireduce_scatter_block",
            "MPI_Iscan",
            "MPI_Iscatter",
            "MPI_Iscatterv",
        ),
        "support_api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Wait",
        ),
        "body_file": "cases/c-runtime/runtime_collective_nonblocking.cbody.in",
    },
    {
        "name": "runtime_collective_persistent",
        "family": "collective",
        "rank_count": 2,
        "api_names": (
            "MPI_Allgather_init",
            "MPI_Allgatherv_init",
            "MPI_Allreduce_init",
            "MPI_Alltoall_init",
            "MPI_Alltoallv_init",
            "MPI_Alltoallw_init",
            "MPI_Barrier_init",
            "MPI_Bcast_init",
            "MPI_Exscan_init",
            "MPI_Gather_init",
            "MPI_Gatherv_init",
            "MPI_Reduce_init",
            "MPI_Reduce_scatter_block_init",
            "MPI_Reduce_scatter_init",
            "MPI_Scan_init",
            "MPI_Scatter_init",
            "MPI_Scatterv_init",
        ),
        "support_api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Request_free",
            "MPI_Start",
            "MPI_Wait",
        ),
        "body_file": "cases/c-runtime/runtime_collective_persistent.cbody.in",
    },
    {
        "name": "runtime_datatype_constructors",
        "family": "type",
        "rank_count": 1,
        "api_names": (
            "MPI_Type_commit",
            "MPI_Type_contiguous",
            "MPI_Type_create_darray",
            "MPI_Type_create_f90_complex",
            "MPI_Type_create_f90_integer",
            "MPI_Type_create_f90_real",
            "MPI_Type_create_hindexed",
            "MPI_Type_create_hindexed_block",
            "MPI_Type_create_hvector",
            "MPI_Type_create_indexed_block",
            "MPI_Type_create_resized",
            "MPI_Type_create_struct",
            "MPI_Type_create_subarray",
            "MPI_Type_dup",
            "MPI_Type_free",
            "MPI_Type_get_contents",
            "MPI_Type_get_envelope",
            "MPI_Type_get_extent",
            "MPI_Type_get_extent_x",
            "MPI_Type_get_name",
            "MPI_Type_get_true_extent",
            "MPI_Type_get_true_extent_x",
            "MPI_Type_get_value_index",
            "MPI_Type_indexed",
            "MPI_Type_match_size",
            "MPI_Type_set_name",
            "MPI_Type_size",
            "MPI_Type_size_x",
            "MPI_Type_vector",
        ),
        "support_api_names": (
            "MPI_Finalize",
            "MPI_Init",
        ),
        "body_file": "cases/c-runtime/runtime_datatype_constructors.cbody.in",
    },
    {
        "name": "runtime_datatype_contents",
        "family": "type",
        "rank_count": 1,
        "api_names": (
            "MPI_Type_get_contents",
        ),
        "support_api_names": (
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Type_create_struct",
            "MPI_Type_free",
            "MPI_Type_get_envelope",
        ),
        "body_file": "cases/c-runtime/runtime_datatype_contents.cbody.in",
    },
    {
        "name": "runtime_object_name_abi_length",
        "family": "misc",
        "rank_count": 1,
        "api_names": (
            "MPI_Comm_dup",
            "MPI_Comm_free",
            "MPI_Comm_get_name",
            "MPI_Comm_set_name",
            "MPI_Type_contiguous",
            "MPI_Type_free",
            "MPI_Type_get_name",
            "MPI_Type_set_name",
        ),
        "support_api_names": (
            "MPI_Finalize",
            "MPI_Init",
        ),
        "body_file": "cases/c-runtime/runtime_object_name_abi_length.cbody.in",
    },
    {
        "name": "runtime_win_object_name_abi_length",
        "family": "win",
        "requires_feature": "rma",
        "rank_count": 1,
        "api_names": (
            "MPI_Win_create_dynamic",
            "MPI_Win_free",
            "MPI_Win_get_name",
            "MPI_Win_set_name",
        ),
        "support_api_names": (
            "MPI_Finalize",
            "MPI_Init",
        ),
        "body_file": "cases/c-runtime/runtime_win_object_name_abi_length.cbody.in",
    },
    {
        "name": "runtime_pack_status",
        "family": "type",
        "rank_count": 1,
        "api_names": (
            "MPI_Pack",
            "MPI_Pack_external",
            "MPI_Pack_external_size",
            "MPI_Pack_size",
            "MPI_Status_get_error",
            "MPI_Status_get_source",
            "MPI_Status_get_tag",
            "MPI_Status_set_cancelled",
            "MPI_Status_set_elements",
            "MPI_Status_set_elements_x",
            "MPI_Status_set_error",
            "MPI_Status_set_source",
            "MPI_Status_set_tag",
            "MPI_Unpack",
            "MPI_Unpack_external",
        ),
        "support_api_names": (
            "MPI_Finalize",
            "MPI_Get_elements",
            "MPI_Get_elements_x",
            "MPI_Init",
            "MPI_Test_cancelled",
        ),
        "body_file": "cases/c-runtime/runtime_pack_status.cbody.in",
    },
    {
        "name": "runtime_op_predefined",
        "family": "op",
        "rank_count": 1,
        "api_names": (
            "MPI_Op_commutative",
            "MPI_Op_fromint",
            "MPI_Op_toint",
        ),
        "support_api_names": (
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Reduce_local",
        ),
        "body_file": "cases/c-runtime/runtime_op_predefined.cbody.in",
    },
    {
        "name": "runtime_win_lifecycle",
        "family": "win",
        "requires_feature": "rma",
        "rank_count": 1,
        "api_names": (
            "MPI_Win_allocate",
            "MPI_Win_allocate_shared",
            "MPI_Win_attach",
            "MPI_Win_call_errhandler",
            "MPI_Win_create",
            "MPI_Win_create_dynamic",
            "MPI_Win_detach",
            "MPI_Win_free",
            "MPI_Win_get_attr",
            "MPI_Win_get_errhandler",
            "MPI_Win_get_group",
            "MPI_Win_get_info",
            "MPI_Win_get_name",
            "MPI_Win_set_errhandler",
            "MPI_Win_set_info",
            "MPI_Win_set_name",
            "MPI_Win_shared_query",
        ),
        "support_api_names": (
            "MPI_Errhandler_free",
            "MPI_Finalize",
            "MPI_Group_free",
            "MPI_Info_create",
            "MPI_Info_free",
            "MPI_Info_set",
            "MPI_Init",
        ),
        "body_file": "cases/c-runtime/runtime_win_lifecycle.cbody.in",
    },
    {
        "name": "runtime_rma_operations",
        "family": "rma",
        "requires_feature": "rma",
        "rank_count": 2,
        "api_names": (
            "MPI_Accumulate",
            "MPI_Compare_and_swap",
            "MPI_Fetch_and_op",
            "MPI_Get",
            "MPI_Get_accumulate",
            "MPI_Put",
            "MPI_Raccumulate",
            "MPI_Rget",
            "MPI_Rget_accumulate",
            "MPI_Rput",
            "MPI_Win_complete",
            "MPI_Win_fence",
            "MPI_Win_flush",
            "MPI_Win_flush_all",
            "MPI_Win_flush_local",
            "MPI_Win_flush_local_all",
            "MPI_Win_lock",
            "MPI_Win_lock_all",
            "MPI_Win_post",
            "MPI_Win_start",
            "MPI_Win_sync",
            "MPI_Win_test",
            "MPI_Win_unlock",
            "MPI_Win_unlock_all",
            "MPI_Win_wait",
        ),
        "support_api_names": (
            "MPI_Barrier",
            "MPI_Comm_group",
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Group_free",
            "MPI_Group_incl",
            "MPI_Init",
            "MPI_Wait",
            "MPI_Win_create",
            "MPI_Win_free",
        ),
        "body_file": "cases/c-runtime/runtime_rma_operations.cbody.in",
    },
    {
        "name": "runtime_file_lifecycle",
        "family": "file",
        "requires_feature": "mpi_io",
        "rank_count": 1,
        "api_names": (
            "MPI_File_call_errhandler",
            "MPI_File_close",
            "MPI_File_delete",
            "MPI_File_get_amode",
            "MPI_File_get_atomicity",
            "MPI_File_get_byte_offset",
            "MPI_File_get_errhandler",
            "MPI_File_get_group",
            "MPI_File_get_info",
            "MPI_File_get_position",
            "MPI_File_get_size",
            "MPI_File_get_type_extent",
            "MPI_File_get_view",
            "MPI_File_open",
            "MPI_File_preallocate",
            "MPI_File_seek",
            "MPI_File_set_atomicity",
            "MPI_File_set_errhandler",
            "MPI_File_set_info",
            "MPI_File_set_size",
            "MPI_File_set_view",
            "MPI_File_sync",
        ),
        "support_api_names": (
            "MPI_Errhandler_free",
            "MPI_Finalize",
            "MPI_Group_free",
            "MPI_Info_create",
            "MPI_Info_free",
            "MPI_Info_set",
            "MPI_Init",
        ),
        "body_file": "cases/c-runtime/runtime_file_lifecycle.cbody.in",
    },
    {
        "name": "runtime_file_io",
        "family": "file",
        "requires_feature": "mpi_io",
        "rank_count": 1,
        "api_names": (
            "MPI_File_get_position_shared",
            "MPI_File_iread",
            "MPI_File_iread_all",
            "MPI_File_iread_at",
            "MPI_File_iread_at_all",
            "MPI_File_iread_shared",
            "MPI_File_iwrite",
            "MPI_File_iwrite_all",
            "MPI_File_iwrite_at",
            "MPI_File_iwrite_at_all",
            "MPI_File_iwrite_shared",
            "MPI_File_read",
            "MPI_File_read_all",
            "MPI_File_read_all_begin",
            "MPI_File_read_all_end",
            "MPI_File_read_at",
            "MPI_File_read_at_all",
            "MPI_File_read_at_all_begin",
            "MPI_File_read_at_all_end",
            "MPI_File_read_ordered",
            "MPI_File_read_ordered_begin",
            "MPI_File_read_ordered_end",
            "MPI_File_read_shared",
            "MPI_File_seek_shared",
            "MPI_File_write",
            "MPI_File_write_all",
            "MPI_File_write_all_begin",
            "MPI_File_write_all_end",
            "MPI_File_write_at",
            "MPI_File_write_at_all",
            "MPI_File_write_at_all_begin",
            "MPI_File_write_at_all_end",
            "MPI_File_write_ordered",
            "MPI_File_write_ordered_begin",
            "MPI_File_write_ordered_end",
            "MPI_File_write_shared",
        ),
        "support_api_names": (
            "MPI_File_close",
            "MPI_File_delete",
            "MPI_File_open",
            "MPI_File_seek",
            "MPI_File_set_size",
            "MPI_Finalize",
            "MPI_Get_count",
            "MPI_Init",
            "MPI_Wait",
        ),
        "body_file": "cases/c-runtime/runtime_file_io.cbody.in",
    },
    {
        "name": "runtime_dynamic_name_service",
        "family": "dynamic_process",
        "rank_count": 1,
        "api_names": (
            "MPI_Close_port",
            "MPI_Lookup_name",
            "MPI_Open_port",
            "MPI_Publish_name",
            "MPI_Unpublish_name",
        ),
        "support_api_names": (
            "MPI_Finalize",
            "MPI_Init",
        ),
        "requires_feature": "dynamic_process",
        "body_file": "cases/c-runtime/runtime_dynamic_name_service.cbody.in",
    },
    {
        "name": "runtime_dynamic_connect",
        "family": "dynamic_process",
        "rank_count": 2,
        "api_names": (
            "MPI_Comm_accept",
            "MPI_Comm_connect",
            "MPI_Comm_disconnect",
        ),
        "support_api_names": (
            "MPI_Bcast",
            "MPI_Close_port",
            "MPI_Comm_rank",
            "MPI_Comm_remote_size",
            "MPI_Comm_size",
            "MPI_Comm_test_inter",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Open_port",
        ),
        "requires_feature": "dynamic_process",
        "body_file": "cases/c-runtime/runtime_dynamic_connect.cbody.in",
    },
    {
        "name": "runtime_dynamic_spawn",
        "family": "dynamic_process",
        "rank_count": 1,
        "api_names": (
            "MPI_Comm_get_parent",
            "MPI_Comm_spawn",
            "MPI_Comm_spawn_multiple",
        ),
        "support_api_names": (
            "MPI_Comm_disconnect",
            "MPI_Comm_remote_size",
            "MPI_Finalize",
            "MPI_Init",
        ),
        "requires_feature": "dynamic_process",
        "body_file": "cases/c-runtime/runtime_dynamic_spawn.cbody.in",
    },
    {
        "name": "runtime_error_info_misc",
        "family": "misc",
        "rank_count": 1,
        "api_names": (
            "MPI_Add_error_class",
            "MPI_Add_error_code",
            "MPI_Add_error_string",
            "MPI_Aint_add",
            "MPI_Aint_diff",
            "MPI_Alloc_mem",
            "MPI_Error_class",
            "MPI_Error_string",
            "MPI_Free_mem",
            "MPI_Get_address",
            "MPI_Get_hw_resource_info",
            "MPI_Get_library_version",
            "MPI_Get_processor_name",
            "MPI_Get_version",
            "MPI_Info_create_env",
            "MPI_Info_delete",
            "MPI_Info_dup",
            "MPI_Info_get",
            "MPI_Info_get_nkeys",
            "MPI_Info_get_nthkey",
            "MPI_Info_get_string",
            "MPI_Info_get_valuelen",
            "MPI_Remove_error_class",
            "MPI_Remove_error_code",
            "MPI_Remove_error_string",
            "MPI_Wtick",
            "MPI_Wtime",
        ),
        "support_api_names": (
            "MPI_Finalize",
            "MPI_Info_create",
            "MPI_Info_free",
            "MPI_Info_set",
            "MPI_Init",
        ),
        "body_file": "cases/c-runtime/runtime_error_info_misc.cbody.in",
    },
)


INSTALLED_C_CALLBACK_PROBES = (
    {
        "name": "callback_comm_attr",
        "family": "callback_attribute",
        "rank_count": 1,
        "api_names": (
            "MPI_Comm_create_keyval",
            "MPI_Comm_delete_attr",
            "MPI_Comm_free_keyval",
            "MPI_Comm_get_attr",
            "MPI_Comm_set_attr",
        ),
        "support_api_names": (
            "MPI_Comm_dup",
            "MPI_Comm_free",
            "MPI_Finalize",
            "MPI_Init",
        ),
        "prologue_file": "cases/c-callback/callback_comm_attr.prologue.in",
        "body_file": "cases/c-callback/callback_comm_attr.cbody.in",
    },
    {
        "name": "callback_type_attr",
        "family": "callback_attribute",
        "rank_count": 1,
        "api_names": (
            "MPI_Type_create_keyval",
            "MPI_Type_delete_attr",
            "MPI_Type_free_keyval",
            "MPI_Type_get_attr",
            "MPI_Type_set_attr",
        ),
        "support_api_names": (
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Type_dup",
            "MPI_Type_free",
        ),
        "prologue_file": "cases/c-callback/callback_type_attr.prologue.in",
        "body_file": "cases/c-callback/callback_type_attr.cbody.in",
    },
    {
        "name": "callback_win_attr",
        "family": "callback_attribute",
        "rank_count": 1,
        "api_names": (
            "MPI_Win_create_keyval",
            "MPI_Win_delete_attr",
            "MPI_Win_free_keyval",
            "MPI_Win_get_attr",
            "MPI_Win_set_attr",
        ),
        "support_api_names": (
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Win_create",
            "MPI_Win_free",
        ),
        "requires_feature": "rma",
        "prologue_file": "cases/c-callback/callback_win_attr.prologue.in",
        "body_file": "cases/c-callback/callback_win_attr.cbody.in",
    },
    {
        "name": "callback_comm_errhandler",
        "family": "callback_errhandler",
        "rank_count": 1,
        "api_names": (
            "MPI_Comm_create_errhandler",
        ),
        "support_api_names": (
            "MPI_Comm_call_errhandler",
            "MPI_Comm_dup",
            "MPI_Comm_free",
            "MPI_Comm_get_errhandler",
            "MPI_Comm_set_errhandler",
            "MPI_Errhandler_free",
            "MPI_Finalize",
            "MPI_Init",
        ),
        "prologue_file": "cases/c-callback/callback_comm_errhandler.prologue.in",
        "body_file": "cases/c-callback/callback_comm_errhandler.cbody.in",
    },
    {
        "name": "callback_file_errhandler",
        "family": "callback_errhandler",
        "rank_count": 1,
        "api_names": (
            "MPI_File_create_errhandler",
        ),
        "support_api_names": (
            "MPI_Errhandler_free",
            "MPI_File_call_errhandler",
            "MPI_File_close",
            "MPI_File_delete",
            "MPI_File_get_errhandler",
            "MPI_File_open",
            "MPI_File_set_errhandler",
            "MPI_Finalize",
            "MPI_Init",
        ),
        "requires_feature": "mpi_io",
        "prologue_file": "cases/c-callback/callback_file_errhandler.prologue.in",
        "body_file": "cases/c-callback/callback_file_errhandler.cbody.in",
    },
    {
        "name": "callback_session_errhandler",
        "family": "callback_errhandler",
        "rank_count": 1,
        "api_names": (
            "MPI_Session_create_errhandler",
        ),
        "support_api_names": (
            "MPI_Errhandler_free",
            "MPI_Session_call_errhandler",
            "MPI_Session_finalize",
            "MPI_Session_get_errhandler",
            "MPI_Session_init",
            "MPI_Session_set_errhandler",
        ),
        "prologue_file": "cases/c-callback/callback_session_errhandler.prologue.in",
        "body_file": "cases/c-callback/callback_session_errhandler.cbody.in",
    },
    {
        "name": "callback_win_errhandler",
        "family": "callback_errhandler",
        "rank_count": 1,
        "api_names": (
            "MPI_Win_create_errhandler",
        ),
        "support_api_names": (
            "MPI_Errhandler_free",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Win_call_errhandler",
            "MPI_Win_create",
            "MPI_Win_free",
            "MPI_Win_get_errhandler",
            "MPI_Win_set_errhandler",
        ),
        "requires_feature": "rma",
        "prologue_file": "cases/c-callback/callback_win_errhandler.prologue.in",
        "body_file": "cases/c-callback/callback_win_errhandler.cbody.in",
    },
    {
        "name": "callback_user_op",
        "family": "callback_op",
        "rank_count": 2,
        "api_names": (
            "MPI_Op_create",
            "MPI_Op_free",
        ),
        "support_api_names": (
            "MPI_Allreduce",
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Op_commutative",
            "MPI_Reduce_local",
        ),
        "prologue_file": "cases/c-callback/callback_user_op.prologue.in",
        "body_file": "cases/c-callback/callback_user_op.cbody.in",
    },
    {
        "name": "callback_grequest",
        "family": "callback_grequest",
        "rank_count": 1,
        "api_names": (
            "MPI_Grequest_complete",
            "MPI_Grequest_start",
        ),
        "support_api_names": (
            "MPI_Cancel",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Status_get_source",
            "MPI_Status_get_tag",
            "MPI_Status_set_cancelled",
            "MPI_Status_set_error",
            "MPI_Status_set_source",
            "MPI_Status_set_tag",
            "MPI_Test_cancelled",
            "MPI_Wait",
        ),
        "prologue_file": "cases/c-callback/callback_grequest.prologue.in",
        "body_file": "cases/c-callback/callback_grequest.cbody.in",
    },
    {
        "name": "callback_datarep",
        "family": "callback_datarep",
        "rank_count": 1,
        "api_names": (
            "MPI_Register_datarep",
        ),
        "support_api_names": (
            "MPI_File_close",
            "MPI_File_delete",
            "MPI_File_open",
            "MPI_File_read_at",
            "MPI_File_set_view",
            "MPI_File_write_at",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Type_size",
        ),
        "requires_feature": "mpi_io",
        "skip_exit_codes": {
            77: SKIP_DATAREP_UNSUPPORTED,
        },
        "prologue_file": "cases/c-callback/callback_datarep.prologue.in",
        "body_file": "cases/c-callback/callback_datarep.cbody.in",
    },
    {
        "name": "callback_mpit_events",
        "family": "callback_mpit",
        "rank_count": 1,
        "api_names": (
            "MPI_T_event_callback_get_info",
            "MPI_T_event_callback_set_info",
            "MPI_T_event_handle_alloc",
            "MPI_T_event_handle_free",
            "MPI_T_event_register_callback",
            "MPI_T_event_set_dropped_handler",
        ),
        "support_api_names": (
            "MPI_Info_create",
            "MPI_Info_free",
            "MPI_T_event_get_info",
            "MPI_T_event_get_num",
            "MPI_T_finalize",
            "MPI_T_init_thread",
        ),
        "requires_feature": "mpit_events",
        "skip_exit_codes": {
            77: SKIP_MPIT_EVENTS_UNAVAILABLE,
        },
        "prologue_file": "cases/c-callback/callback_mpit_events.prologue.in",
        "body_file": "cases/c-callback/callback_mpit_events.cbody.in",
    },
    {
        "name": "lifetime_nonblocking_collective_arrays",
        "family": "callback_lifetime",
        "rank_count": 2,
        "api_names": (
            "MPI_Ialltoallw",
        ),
        "support_api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Wait",
        ),
        "body_file": (
            "cases/c-callback/"
            "lifetime_nonblocking_collective_arrays.cbody.in"
        ),
    },
    {
        "name": "lifetime_persistent_collective_arrays",
        "family": "callback_lifetime",
        "rank_count": 2,
        "api_names": (
            "MPI_Alltoallw_init",
        ),
        "support_api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Init",
            "MPI_Request_free",
            "MPI_Start",
            "MPI_Wait",
        ),
        "body_file": (
            "cases/c-callback/"
            "lifetime_persistent_collective_arrays.cbody.in"
        ),
    },
)


INSTALLED_FORTRAN_COMPILE_PROBES = (
    {
        "name": "fortran_mpifh_compile_conformance",
        "language": "mpif.h",
        "use_statement": "include 'mpif.h'",
        "api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
        ),
        "body": """
integer :: ierr
integer :: rank
integer :: size

call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
""",
    },
    {
        "name": "fortran_usempi_compile_conformance",
        "language": "use mpi",
        "use_statement": "use mpi",
        "api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
        ),
        "body": """
implicit none
integer :: ierr
integer :: rank
integer :: size

call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
""",
    },
    {
        "name": "fortran_usempif08_compile_conformance",
        "language": "use mpi_f08",
        "use_statement": "use mpi_f08",
        "api_names": (
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Get_count",
        ),
        "body": """
implicit none
integer :: ierr
integer :: rank
integer :: size
type(MPI_Comm) :: comm
type(MPI_Status) :: status

comm = MPI_COMM_WORLD
call MPI_Comm_rank(comm, rank, ierr)
call MPI_Comm_size(comm, size)
call MPI_Get_count(status, MPI_INTEGER, size, ierr)
""",
    },
)


INSTALLED_FORTRAN_RUNTIME_PROBES = (
    {
        "name": "fortran_mpifh_runtime_core",
        "language": "mpif.h",
        "rank_count": 2,
        "use_statement": "include 'mpif.h'",
        "api_names": (
            "MPI_Barrier",
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Get_count",
            "MPI_Init",
            "MPI_Sendrecv",
        ),
        "body": """
integer :: ierr
integer :: rank
integer :: size
integer :: peer
integer :: sendbuf
integer :: recvbuf
integer :: count
integer :: status(MPI_STATUS_SIZE)

call MPI_Init(ierr)
if (ierr .ne. MPI_SUCCESS) stop 1
call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
if (ierr .ne. MPI_SUCCESS) stop 2
call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
if (ierr .ne. MPI_SUCCESS .or. size .ne. 2) stop 3
peer = 1 - rank
sendbuf = rank + 10
recvbuf = -1
call MPI_Sendrecv(sendbuf, 1, MPI_INTEGER, peer, 1101, &
                  recvbuf, 1, MPI_INTEGER, peer, 1101, &
                  MPI_COMM_WORLD, status, ierr)
if (ierr .ne. MPI_SUCCESS) stop 4
call MPI_Get_count(status, MPI_INTEGER, count, ierr)
if (ierr .ne. MPI_SUCCESS .or. count .ne. 1) stop 5
if (status(MPI_SOURCE) .ne. peer) stop 6
if (status(MPI_TAG) .ne. 1101) stop 7
if (recvbuf .ne. peer + 10) stop 8
call MPI_Barrier(MPI_COMM_WORLD, ierr)
if (ierr .ne. MPI_SUCCESS) stop 9
call MPI_Finalize(ierr)
if (ierr .ne. MPI_SUCCESS) stop 10
""",
    },
    {
        "name": "fortran_usempi_runtime_core",
        "language": "use mpi",
        "rank_count": 2,
        "use_statement": "use mpi",
        "api_names": (
            "MPI_Barrier",
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Get_count",
            "MPI_Init",
            "MPI_Sendrecv",
        ),
        "body": """
implicit none
integer :: ierr
integer :: rank
integer :: size
integer :: peer
integer :: sendbuf
integer :: recvbuf
integer :: count
integer :: status(MPI_STATUS_SIZE)

call MPI_Init(ierr)
if (ierr .ne. MPI_SUCCESS) stop 1
call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
if (ierr .ne. MPI_SUCCESS) stop 2
call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
if (ierr .ne. MPI_SUCCESS .or. size .ne. 2) stop 3
peer = 1 - rank
sendbuf = rank + 20
recvbuf = -1
call MPI_Sendrecv(sendbuf, 1, MPI_INTEGER, peer, 1102, &
                  recvbuf, 1, MPI_INTEGER, peer, 1102, &
                  MPI_COMM_WORLD, status, ierr)
if (ierr .ne. MPI_SUCCESS) stop 4
call MPI_Get_count(status, MPI_INTEGER, count, ierr)
if (ierr .ne. MPI_SUCCESS .or. count .ne. 1) stop 5
if (status(MPI_SOURCE) .ne. peer) stop 6
if (status(MPI_TAG) .ne. 1102) stop 7
if (recvbuf .ne. peer + 20) stop 8
call MPI_Barrier(MPI_COMM_WORLD, ierr)
if (ierr .ne. MPI_SUCCESS) stop 9
call MPI_Finalize(ierr)
if (ierr .ne. MPI_SUCCESS) stop 10
""",
    },
    {
        "name": "fortran_usempif08_runtime_core",
        "language": "use mpi_f08",
        "rank_count": 2,
        "use_statement": "use mpi_f08",
        "api_names": (
            "MPI_Barrier",
            "MPI_Comm_dup",
            "MPI_Comm_free",
            "MPI_Comm_rank",
            "MPI_Comm_size",
            "MPI_Finalize",
            "MPI_Get_count",
            "MPI_Init",
            "MPI_Sendrecv",
            "MPI_Type_size",
        ),
        "body": """
implicit none
integer :: ierr
integer :: rank
integer :: size
integer :: peer
integer :: sendbuf
integer :: recvbuf
integer :: count
integer :: type_size
integer :: expected_type_size
type(MPI_Comm) :: comm
type(MPI_Status) :: status

call MPI_Init(ierr)
if (ierr /= MPI_SUCCESS) stop 1
call MPI_Comm_dup(MPI_COMM_WORLD, comm, ierr)
if (ierr /= MPI_SUCCESS) stop 2
call MPI_Comm_rank(comm, rank, ierr)
if (ierr /= MPI_SUCCESS) stop 3
call MPI_Comm_size(comm, size, ierr)
if (ierr /= MPI_SUCCESS .or. size /= 2) stop 4
call MPI_Type_size(MPI_INTEGER, type_size, ierr)
expected_type_size = storage_size(sendbuf) / 8
if (ierr /= MPI_SUCCESS .or. type_size /= expected_type_size) stop 5
peer = 1 - rank
sendbuf = rank + 30
recvbuf = -1
call MPI_Sendrecv(sendbuf, 1, MPI_INTEGER, peer, 1103, &
                  recvbuf, 1, MPI_INTEGER, peer, 1103, &
                  comm, status, ierr)
if (ierr /= MPI_SUCCESS) stop 6
call MPI_Get_count(status, MPI_INTEGER, count, ierr)
if (ierr /= MPI_SUCCESS .or. count /= 1) stop 7
if (status%MPI_SOURCE /= peer) stop 8
if (status%MPI_TAG /= 1103) stop 9
if (recvbuf /= peer + 30) stop 10
call MPI_Barrier(comm, ierr)
if (ierr /= MPI_SUCCESS) stop 11
call MPI_Comm_free(comm, ierr)
if (ierr /= MPI_SUCCESS) stop 12
call MPI_Finalize(ierr)
if (ierr /= MPI_SUCCESS) stop 13
""",
    },
    {
        "name": "fortran_mpifh_abi_helpers",
        "language": "mpif.h",
        "rank_count": 1,
        "use_statement": "include 'mpif.h'",
        "api_names": (
            "MPI_Abi_get_fortran_booleans",
            "MPI_Abi_get_fortran_info",
            "MPI_Abi_get_info",
            "MPI_Abi_get_version",
            "MPI_Abi_set_fortran_booleans",
            "MPI_Abi_set_fortran_info",
            "MPI_Comm_set_errhandler",
            "MPI_Info_free",
            "MPI_Init",
            "MPI_Finalize",
        ),
        "body": """
integer :: ierr
integer :: major
integer :: minor
integer :: info
integer :: finfo
integer :: logical_size
logical :: logical_true
logical :: logical_false
logical :: is_set
logical :: requested_true
logical :: requested_false
logical :: flag
character(len=32) :: value
integer :: numeric_value
integer :: read_status
real :: real_value
double precision :: double_value

call MPI_Init(ierr)
if (ierr .ne. MPI_SUCCESS) stop 1
call MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN, ierr)
if (ierr .ne. MPI_SUCCESS) stop 25
call MPI_Abi_get_version(major, minor, ierr)
if (ierr .ne. MPI_SUCCESS) stop 2
if (.not. ((major .eq. -1 .and. minor .eq. -1) .or. &
           (major .eq. 1 .and. minor .eq. 0))) stop 11
call MPI_Abi_get_info(info, ierr)
if (ierr .ne. MPI_SUCCESS .or. info .eq. MPI_INFO_NULL) stop 3
call MPI_Info_get(info, 'mpi_aint_size', len(value), value, flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 15
read(value, *, iostat=read_status) numeric_value
if (read_status .ne. 0 .or. numeric_value .le. 0) stop 16
call MPI_Info_get(info, 'mpi_count_size', len(value), value, flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 26
read(value, *, iostat=read_status) numeric_value
if (read_status .ne. 0 .or. numeric_value .le. 0) stop 27
call MPI_Info_get(info, 'mpi_offset_size', len(value), value, flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 28
read(value, *, iostat=read_status) numeric_value
if (read_status .ne. 0 .or. numeric_value .le. 0) stop 29
call MPI_Info_free(info, ierr)
if (ierr .ne. MPI_SUCCESS) stop 4
call MPI_Abi_get_fortran_info(finfo, ierr)
if (ierr .ne. MPI_SUCCESS .or. finfo .eq. MPI_INFO_NULL) stop 5
call MPI_Info_get(finfo, 'mpi_logical_size', len(value), value, flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 17
read(value, *, iostat=read_status) numeric_value
if (read_status .ne. 0 .or. &
    numeric_value .ne. storage_size(logical_true) / 8) stop 18
call MPI_Info_get(finfo, 'mpi_integer_size', len(value), value, flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 30
read(value, *, iostat=read_status) numeric_value
if (read_status .ne. 0 .or. &
    numeric_value .ne. storage_size(numeric_value) / 8) stop 31
call MPI_Info_get(finfo, 'mpi_real_size', len(value), value, flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 32
read(value, *, iostat=read_status) numeric_value
if (read_status .ne. 0 .or. &
    numeric_value .ne. storage_size(real_value) / 8) stop 33
call MPI_Info_get(finfo, 'mpi_double_precision_size', len(value), value, &
                  flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 34
read(value, *, iostat=read_status) numeric_value
if (read_status .ne. 0 .or. &
    numeric_value .ne. storage_size(double_value) / 8) stop 35
call MPI_Info_get(finfo, 'mpi_integer4_supported', len(value), value, &
                  flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 19
if (trim(value) .ne. 'true' .and. trim(value) .ne. 'false') stop 20
call MPI_Abi_set_fortran_info(finfo, ierr)
! Open MPI's configured Fortran runtime owns this state after MPI_Init,
! so the setter must reject attempts to replace it.
if (ierr .ne. MPI_ERR_ABI) stop 6
call MPI_Info_free(finfo, ierr)
if (ierr .ne. MPI_SUCCESS) stop 7
logical_size = storage_size(logical_true) / 8
call MPI_Abi_get_fortran_booleans(logical_size, logical_true, &
                                  logical_false, is_set, ierr)
if (ierr .ne. MPI_SUCCESS) stop 8
if (.not. is_set) stop 12
if (.not. logical_true) stop 13
if (logical_false) stop 14
requested_true = .false.
requested_false = .true.
call MPI_Abi_set_fortran_booleans(logical_size, requested_true, &
                                  requested_false, ierr)
! See the MPI_Abi_set_fortran_info comment above; after MPI_Init,
! accepting MPI_ERR_ABI avoids turning expected read-only runtime state
! into a false failure.
if (ierr .ne. MPI_SUCCESS .and. ierr .ne. MPI_ERR_ABI) stop 9
logical_true = .false.
logical_false = .true.
is_set = .false.
call MPI_Abi_get_fortran_booleans(logical_size, logical_true, &
                                  logical_false, is_set, ierr)
if (ierr .ne. MPI_SUCCESS) stop 21
if (.not. is_set) stop 22
if (.not. logical_true) stop 23
if (logical_false) stop 24
call MPI_Finalize(ierr)
if (ierr .ne. MPI_SUCCESS) stop 10
""",
    },
    {
        "name": "fortran_usempi_abi_helpers",
        "language": "use mpi",
        "rank_count": 1,
        "use_statement": "use mpi",
        "api_names": (
            "MPI_Abi_get_fortran_booleans",
            "MPI_Abi_get_fortran_info",
            "MPI_Abi_get_info",
            "MPI_Abi_get_version",
            "MPI_Abi_set_fortran_booleans",
            "MPI_Abi_set_fortran_info",
            "MPI_Comm_set_errhandler",
            "MPI_Info_free",
            "MPI_Init",
            "MPI_Finalize",
        ),
        "body": """
implicit none
integer :: ierr
integer :: major
integer :: minor
integer :: info
integer :: finfo
integer :: logical_size
logical :: logical_true
logical :: logical_false
logical :: is_set
logical :: requested_true
logical :: requested_false
logical :: flag
character(len=32) :: value
integer :: numeric_value
integer :: read_status
real :: real_value
double precision :: double_value

call MPI_Init(ierr)
if (ierr .ne. MPI_SUCCESS) stop 1
call MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN, ierr)
if (ierr .ne. MPI_SUCCESS) stop 25
call MPI_Abi_get_version(major, minor, ierr)
if (ierr .ne. MPI_SUCCESS) stop 2
if (.not. ((major .eq. -1 .and. minor .eq. -1) .or. &
           (major .eq. 1 .and. minor .eq. 0))) stop 11
call MPI_Abi_get_info(info, ierr)
if (ierr .ne. MPI_SUCCESS .or. info .eq. MPI_INFO_NULL) stop 3
call MPI_Info_get(info, 'mpi_aint_size', len(value), value, flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 15
read(value, *, iostat=read_status) numeric_value
if (read_status .ne. 0 .or. numeric_value .le. 0) stop 16
call MPI_Info_get(info, 'mpi_count_size', len(value), value, flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 26
read(value, *, iostat=read_status) numeric_value
if (read_status .ne. 0 .or. numeric_value .le. 0) stop 27
call MPI_Info_get(info, 'mpi_offset_size', len(value), value, flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 28
read(value, *, iostat=read_status) numeric_value
if (read_status .ne. 0 .or. numeric_value .le. 0) stop 29
call MPI_Info_free(info, ierr)
if (ierr .ne. MPI_SUCCESS) stop 4
call MPI_Abi_get_fortran_info(finfo, ierr)
if (ierr .ne. MPI_SUCCESS .or. finfo .eq. MPI_INFO_NULL) stop 5
call MPI_Info_get(finfo, 'mpi_logical_size', len(value), value, flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 17
read(value, *, iostat=read_status) numeric_value
if (read_status .ne. 0 .or. &
    numeric_value .ne. storage_size(logical_true) / 8) stop 18
call MPI_Info_get(finfo, 'mpi_integer_size', len(value), value, flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 30
read(value, *, iostat=read_status) numeric_value
if (read_status .ne. 0 .or. &
    numeric_value .ne. storage_size(numeric_value) / 8) stop 31
call MPI_Info_get(finfo, 'mpi_real_size', len(value), value, flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 32
read(value, *, iostat=read_status) numeric_value
if (read_status .ne. 0 .or. &
    numeric_value .ne. storage_size(real_value) / 8) stop 33
call MPI_Info_get(finfo, 'mpi_double_precision_size', len(value), value, &
                  flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 34
read(value, *, iostat=read_status) numeric_value
if (read_status .ne. 0 .or. &
    numeric_value .ne. storage_size(double_value) / 8) stop 35
call MPI_Info_get(finfo, 'mpi_integer4_supported', len(value), value, &
                  flag, ierr)
if (ierr .ne. MPI_SUCCESS .or. .not. flag) stop 19
if (trim(value) .ne. 'true' .and. trim(value) .ne. 'false') stop 20
call MPI_Abi_set_fortran_info(finfo, ierr)
! Open MPI's configured Fortran runtime owns this state after MPI_Init,
! so the setter must reject attempts to replace it.
if (ierr .ne. MPI_ERR_ABI) stop 6
call MPI_Info_free(finfo, ierr)
if (ierr .ne. MPI_SUCCESS) stop 7
logical_size = storage_size(logical_true) / 8
call MPI_Abi_get_fortran_booleans(logical_size, logical_true, &
                                  logical_false, is_set, ierr)
if (ierr .ne. MPI_SUCCESS) stop 8
if (.not. is_set) stop 12
if (.not. logical_true) stop 13
if (logical_false) stop 14
requested_true = .false.
requested_false = .true.
call MPI_Abi_set_fortran_booleans(logical_size, requested_true, &
                                  requested_false, ierr)
! See the MPI_Abi_set_fortran_info comment above; after MPI_Init,
! accepting MPI_ERR_ABI avoids turning expected read-only runtime state
! into a false failure.
if (ierr .ne. MPI_SUCCESS .and. ierr .ne. MPI_ERR_ABI) stop 9
logical_true = .false.
logical_false = .true.
is_set = .false.
call MPI_Abi_get_fortran_booleans(logical_size, logical_true, &
                                  logical_false, is_set, ierr)
if (ierr .ne. MPI_SUCCESS) stop 21
if (.not. is_set) stop 22
if (.not. logical_true) stop 23
if (logical_false) stop 24
call MPI_Finalize(ierr)
if (ierr .ne. MPI_SUCCESS) stop 10
""",
    },
    {
        "name": "fortran_usempif08_abi_helpers",
        "language": "use mpi_f08",
        "rank_count": 1,
        "use_statement": "use mpi_f08",
        "api_names": (
            "MPI_Abi_get_fortran_booleans",
            "MPI_Abi_get_fortran_info",
            "MPI_Abi_get_info",
            "MPI_Abi_get_version",
            "MPI_Abi_set_fortran_booleans",
            "MPI_Abi_set_fortran_info",
            "MPI_Comm_set_errhandler",
            "MPI_Info_free",
            "MPI_Init",
            "MPI_Finalize",
        ),
        "body": """
implicit none
integer :: ierr
integer :: major
integer :: minor
type(MPI_Info) :: info
type(MPI_Info) :: finfo
integer :: logical_size
logical :: logical_true
logical :: logical_false
logical :: is_set
logical :: requested_true
logical :: requested_false
logical :: flag
character(len=32) :: value
integer :: numeric_value
integer :: read_status
real :: real_value
double precision :: double_value

call MPI_Init(ierr)
if (ierr /= MPI_SUCCESS) stop 1
call MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN, ierr)
if (ierr /= MPI_SUCCESS) stop 25
call MPI_Abi_get_version(major, minor, ierr)
if (ierr /= MPI_SUCCESS) stop 2
if (.not. ((major == -1 .and. minor == -1) .or. &
           (major == 1 .and. minor == 0))) stop 11
call MPI_Abi_get_info(info, ierr)
if (ierr /= MPI_SUCCESS .or. info == MPI_INFO_NULL) stop 3
call MPI_Info_get(info, 'mpi_aint_size', len(value), value, flag, ierr)
if (ierr /= MPI_SUCCESS .or. .not. flag) stop 15
read(value, *, iostat=read_status) numeric_value
if (read_status /= 0 .or. numeric_value <= 0) stop 16
call MPI_Info_get(info, 'mpi_count_size', len(value), value, flag, ierr)
if (ierr /= MPI_SUCCESS .or. .not. flag) stop 26
read(value, *, iostat=read_status) numeric_value
if (read_status /= 0 .or. numeric_value <= 0) stop 27
call MPI_Info_get(info, 'mpi_offset_size', len(value), value, flag, ierr)
if (ierr /= MPI_SUCCESS .or. .not. flag) stop 28
read(value, *, iostat=read_status) numeric_value
if (read_status /= 0 .or. numeric_value <= 0) stop 29
call MPI_Info_free(info, ierr)
if (ierr /= MPI_SUCCESS) stop 4
call MPI_Abi_get_fortran_info(finfo, ierr)
if (ierr /= MPI_SUCCESS .or. finfo == MPI_INFO_NULL) stop 5
call MPI_Info_get(finfo, 'mpi_logical_size', len(value), value, flag, ierr)
if (ierr /= MPI_SUCCESS .or. .not. flag) stop 17
read(value, *, iostat=read_status) numeric_value
if (read_status /= 0 .or. &
    numeric_value /= storage_size(logical_true) / 8) stop 18
call MPI_Info_get(finfo, 'mpi_integer_size', len(value), value, flag, ierr)
if (ierr /= MPI_SUCCESS .or. .not. flag) stop 30
read(value, *, iostat=read_status) numeric_value
if (read_status /= 0 .or. &
    numeric_value /= storage_size(numeric_value) / 8) stop 31
call MPI_Info_get(finfo, 'mpi_real_size', len(value), value, flag, ierr)
if (ierr /= MPI_SUCCESS .or. .not. flag) stop 32
read(value, *, iostat=read_status) numeric_value
if (read_status /= 0 .or. &
    numeric_value /= storage_size(real_value) / 8) stop 33
call MPI_Info_get(finfo, 'mpi_double_precision_size', len(value), value, &
                  flag, ierr)
if (ierr /= MPI_SUCCESS .or. .not. flag) stop 34
read(value, *, iostat=read_status) numeric_value
if (read_status /= 0 .or. &
    numeric_value /= storage_size(double_value) / 8) stop 35
call MPI_Info_get(finfo, 'mpi_integer4_supported', len(value), value, &
                  flag, ierr)
if (ierr /= MPI_SUCCESS .or. .not. flag) stop 19
if (trim(value) /= 'true' .and. trim(value) /= 'false') stop 20
call MPI_Abi_set_fortran_info(finfo, ierr)
! Open MPI's configured Fortran runtime owns this state after MPI_Init,
! so the setter must reject attempts to replace it.
if (ierr /= MPI_ERR_ABI) stop 6
call MPI_Info_free(finfo, ierr)
if (ierr /= MPI_SUCCESS) stop 7
logical_size = storage_size(logical_true) / 8
call MPI_Abi_get_fortran_booleans(logical_size, logical_true, &
                                  logical_false, is_set, ierr)
if (ierr /= MPI_SUCCESS) stop 8
if (.not. is_set) stop 12
if (.not. logical_true) stop 13
if (logical_false) stop 14
requested_true = .false.
requested_false = .true.
call MPI_Abi_set_fortran_booleans(logical_size, requested_true, &
                                  requested_false, ierr)
! See the MPI_Abi_set_fortran_info comment above; after MPI_Init,
! accepting MPI_ERR_ABI avoids turning expected read-only runtime state
! into a false failure.
if (ierr /= MPI_SUCCESS .and. ierr /= MPI_ERR_ABI) stop 9
logical_true = .false.
logical_false = .true.
is_set = .false.
call MPI_Abi_get_fortran_booleans(logical_size, logical_true, &
                                  logical_false, is_set, ierr)
if (ierr /= MPI_SUCCESS) stop 21
if (.not. is_set) stop 22
if (.not. logical_true) stop 23
if (logical_false) stop 24
call MPI_Finalize(ierr)
if (ierr /= MPI_SUCCESS) stop 10
""",
    },
)


CALLBACK_ATTRIBUTE_API_NAMES = frozenset((
    "MPI_Comm_create_keyval",
    "MPI_Comm_delete_attr",
    "MPI_Comm_free_keyval",
    "MPI_Comm_get_attr",
    "MPI_Comm_set_attr",
    "MPI_Type_create_keyval",
    "MPI_Type_delete_attr",
    "MPI_Type_free_keyval",
    "MPI_Type_get_attr",
    "MPI_Type_set_attr",
    "MPI_Win_create_keyval",
    "MPI_Win_delete_attr",
    "MPI_Win_free_keyval",
    "MPI_Win_get_attr",
    "MPI_Win_set_attr",
))

LEGACY_ATTRIBUTE_API_NAMES = frozenset((
    "MPI_Attr_delete",
    "MPI_Attr_get",
    "MPI_Attr_put",
    "MPI_Keyval_create",
    "MPI_Keyval_free",
))

PHASE10B_CALLBACK_API_NAMES = frozenset((
    "MPI_Comm_create_errhandler",
    "MPI_File_create_errhandler",
    "MPI_Grequest_complete",
    "MPI_Grequest_start",
    "MPI_Op_create",
    "MPI_Op_free",
    "MPI_Register_datarep",
    "MPI_Session_create_errhandler",
    "MPI_T_event_callback_get_info",
    "MPI_T_event_callback_set_info",
    "MPI_T_event_handle_alloc",
    "MPI_T_event_handle_free",
    "MPI_T_event_register_callback",
    "MPI_T_event_set_dropped_handler",
    "MPI_Win_create_errhandler",
))
