#!/usr/bin/env python3
#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

"""MPI standard ABI test manifest and runner.

This implementation provides the phase 1-9 infrastructure: metadata
loading, manifest generation, fast metadata checks, installed ABI smoke
tests, installed C header/prototype/symbol checks, C converter probes,
seed C runtime API-family probes, tool discovery, skip handling, and
JSON/text reporting.  Later phases add exhaustive runtime tests on top
of this runner.
"""

import argparse
import ast
import json
import os
from pathlib import Path
import platform
import re
import shlex
import shutil
import subprocess
import sys
import tempfile


CLASS_IMPLEMENTED = "implemented"
CLASS_NOT_IMPLEMENTED = "not_implemented"
CLASS_NOT_IN_STANDARD_ABI = "not_in_standard_abi"
CLASS_UNSUPPORTED_BY_BUILD = "unsupported_by_build"
CLASS_UNSUPPORTED_BY_OPEN_MPI = "unsupported_by_open_mpi"

TEST_NOT_WRITTEN = "test_not_written_yet"
TEST_NOT_APPLICABLE = "not_applicable"
TEST_CALLBACK_DEFERRED = "callback_deferred"

SKIP_STANDARD_ABI_DISABLED = "standard_abi_disabled"
SKIP_OPEN_MPI_TOOLS_UNAVAILABLE = "open_mpi_tools_unavailable"
SKIP_MPICH_TOOLS_UNAVAILABLE = "mpich_tools_unavailable"
SKIP_HEADER_UNAVAILABLE = "generated_standard_abi_header_unavailable"
SKIP_LINKAGE_INSPECTION_UNAVAILABLE = "linkage_inspection_unavailable"
SKIP_SYMBOL_DIAGNOSTICS_UNAVAILABLE = "symbol_diagnostics_unavailable"
SKIP_FORTRAN_BINDINGS_DISABLED = "fortran_bindings_disabled"
SKIP_FORTRAN_BINDING_DISABLED = "fortran_binding_disabled"
SKIP_FORTRAN_HELPERS_SHARED = "fortran_abi_helpers_shared_with_mpifh"
SKIP_FORTRAN_OPTIONAL_DATATYPES_DEFERRED = (
    "fortran_optional_datatypes_deferred"
)
SKIP_FORTRAN_WRAPPER_UNAVAILABLE = "fortran_wrapper_unavailable"
SKIP_RMA_SUPPORT_DISABLED = "rma_support_disabled"
SKIP_MPI_IO_SUPPORT_DISABLED = "mpi_io_support_disabled"
SKIP_DYNAMIC_PROCESS_DISABLED = "dynamic_process_disabled"
SKIP_DATAREP_UNSUPPORTED = "datarep_unsupported"
SKIP_MPIT_EVENTS_UNAVAILABLE = "mpit_events_unavailable"
SKIP_MPIT_EVENTS_DISABLED = "mpit_events_disabled"
SKIP_MPI_ABORT_TERMINATES_JOB = "mpi_abort_terminates_job"
SKIP_COMM_JOIN_REQUIRES_CONNECTED_FD = "comm_join_requires_connected_fd"
SKIP_PHASE10_CALLBACK_REQUIRED = "phase10_callback_required"

EXPECTED_METADATA_VERSION = "5.0"
EXPECTED_API_COUNT = 567
EXPECTED_CONSTANT_COUNT = 373
DEFAULT_COMMAND_TIMEOUT = 120
MIN_EXPECTED_C_HEADER_PROTOTYPES = 1000

FORTRAN_BINDING_LANGUAGES = (
    "mpif.h",
    "use mpi",
    "use mpi_f08",
)

ANSI_RED = "\033[0;31m"
ANSI_GREEN = "\033[0;32m"
ANSI_BLUE = "\033[1;34m"
ANSI_MAGENTA = "\033[0;35m"
ANSI_RESET = "\033[m"

VALID_CLASSIFICATIONS = {
    CLASS_IMPLEMENTED,
    CLASS_NOT_IMPLEMENTED,
    CLASS_NOT_IN_STANDARD_ABI,
    CLASS_UNSUPPORTED_BY_BUILD,
    CLASS_UNSUPPORTED_BY_OPEN_MPI,
}

VALID_TEST_STATUSES = {
    TEST_NOT_WRITTEN,
    TEST_NOT_APPLICABLE,
    TEST_CALLBACK_DEFERRED,
}

# Static metadata expectations.  These are intentionally hard gates.  The
# runner is generated against the current docs/ metadata shape, and a
# metadata version/count change means the MPI Forum ABI/API authority has
# changed underneath us.  Failing here is preferable to silently accepting
# a new standard constant or API without reviewing the generated probes,
# task list, skip policy, and completion gate.
ABI_CONVERTER_HANDLES = (
    ("Comm", "comm"),
    ("Errhandler", "errhandler"),
    ("File", "file"),
    ("Group", "group"),
    ("Info", "info"),
    ("Message", "message"),
    ("Op", "op"),
    ("Request", "request"),
    ("Session", "session"),
    ("Type", "type"),
    ("Win", "win"),
)

ABI_CONVERTER_KIND_BY_C_TYPE = {
    "MPI_Comm": "Comm",
    "MPI_Datatype": "Type",
    "MPI_Errhandler": "Errhandler",
    "MPI_File": "File",
    "MPI_Group": "Group",
    "MPI_Info": "Info",
    "MPI_Message": "Message",
    "MPI_Op": "Op",
    "MPI_Request": "Request",
    "MPI_Session": "Session",
    "MPI_Win": "Win",
}

ABI_CONVERTER_REQUIRED_KINDS = tuple(
    kind for kind, _stem in ABI_CONVERTER_HANDLES)

FORTRAN_ABI_HELPERS = (
    "abi_get_fortran_booleans",
    "abi_get_fortran_info",
    "abi_get_info",
    "abi_get_version",
    "abi_set_fortran_booleans",
    "abi_set_fortran_info",
)

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


def _read_json(path):
    with path.open("r", encoding="utf-8") as stream:
        return json.load(stream)


def _read_text(path):
    return path.read_text(encoding="utf-8", errors="ignore")


def _write_json(path, payload):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as stream:
        json.dump(payload, stream, indent=2, sort_keys=True)
        stream.write("\n")


def _write_text(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as stream:
        stream.write(text)


def _probe_body_path(srcdir, case):
    """Return the checked-in C body snippet path for one probe case."""
    body_file = case.get("body_file")
    if body_file is None:
        return None
    return srcdir / "test" / "mpi-abi" / body_file


def _probe_body_text(srcdir, case):
    """Load the C body for one installed probe case.

    Long C snippets live in checked-in *.cbody.in files so the Python
    tables remain readable metadata contracts.  Keep a temporary inline
    fallback for older cases while preserving a one-body-source rule:
    exactly one of body or body_file may be present.
    """
    has_body = "body" in case
    has_body_file = "body_file" in case
    if has_body == has_body_file:
        raise RuntimeError(
            "probe {0} must define exactly one of body or body_file".
            format(case.get("name", "<unknown>")))
    if has_body:
        return case["body"]

    path = _probe_body_path(srcdir, case)
    if not path.exists():
        raise RuntimeError(
            "probe {0} body file is missing: {1}".format(
                case.get("name", "<unknown>"), path))
    return _read_text(path)


def _probe_prologue_text(srcdir, case):
    """Load optional top-level C helpers for one installed probe case.

    Most checked-in snippets are straight-line code inserted inside
    main().  Callback probes also need file-scope callback functions.
    Keeping those helpers in a separate prologue file preserves the
    body file as the readable description of the MPI runtime flow.
    """
    prologue_file = case.get("prologue_file")
    if prologue_file is None:
        return ""
    path = srcdir / "test" / "mpi-abi" / prologue_file
    if not path.exists():
        raise RuntimeError(
            "probe {0} prologue file is missing: {1}".format(
                case.get("name", "<unknown>"), path))
    return _read_text(path)


def _env_bool(name):
    value = os.environ.get(name)
    if value is None:
        return None
    return value.strip().lower() in ("1", "true", "yes", "on")


def _which(env_name, default_name):
    override = os.environ.get(env_name)
    if override:
        return override
    return shutil.which(default_name)


def _tool_available(path):
    if not path:
        return False
    if os.path.sep in path:
        return os.access(path, os.X_OK)
    return shutil.which(path) is not None


def _open_mpi_tools_available(tools):
    ompi_tools = tools["open_mpi"]
    return (
        _tool_available(ompi_tools["mpicc_abi"]) and
        _tool_available(ompi_tools["mpirun"])
    )


def _mpich_tools_available(tools):
    mpich_tools = tools["mpich"]
    return (
        _tool_available(mpich_tools["mpicc"]) and
        _tool_available(mpich_tools["mpirun"])
    )


def _api_stem(api_key, api_name):
    name = api_key
    if name.startswith("mpi_"):
        name = name[4:]
    elif api_name.startswith("MPI_"):
        name = api_name[4:].lower()
    return name


def _api_family(name):
    """Return a coarse MPI API family used for test planning metadata."""
    lower = name.lower()
    if lower.startswith("mpi_t_"):
        return "mpi_t"
    if lower.startswith("mpi_abi_"):
        return "abi"
    parts = lower.split("_")
    if len(parts) < 2:
        return "misc"
    if parts[1] in ("comm", "group", "type", "file", "win", "session"):
        return parts[1]
    if parts[1] in ("send", "recv", "isend", "irecv", "probe", "iprobe"):
        return "point_to_point"
    if parts[1] in ("allreduce", "allgather", "allgatherv", "alltoall"):
        return "collective"
    if parts[1] in ("barrier", "bcast", "gather", "gatherv"):
        return "collective"
    if parts[1] in ("reduce", "scan", "exscan"):
        return "collective"
    if parts[1] in ("put", "get", "accumulate", "rput", "rget"):
        return "rma"
    if parts[1] in ("init", "finalize", "initialized", "finalized"):
        return "init"
    if parts[1] in ("errhandler", "error"):
        return "error"
    return parts[1]


def _rank_requirement(family):
    """Return the default rank count needed for an API family."""
    if family in ("collective", "point_to_point"):
        return 2
    return 1


def _feature_requirement(family):
    """Return the optional Open MPI feature that gates an API family."""
    if family == "file":
        return "mpi_io"
    if family == "mpi_t":
        return "mpi_t"
    if family == "win" or family == "rma":
        return "rma"
    if family == "session":
        return "sessions"
    return None


def _find_standard_abi_setting(srcdir, builddir):
    """Detect whether this build configured Open MPI standard ABI support."""
    override = _env_bool("OMPI_ABI_TEST_STANDARD_ABI")
    if override is not None:
        return {
            "enabled": override,
            "source": "OMPI_ABI_TEST_STANDARD_ABI",
        }

    conditional = _conditional_enabled(builddir, "OMPI_STANDARD_ABI")
    if conditional["enabled"] is not None:
        return conditional

    return {
        "enabled": None,
        "source": "unknown",
    }


def _conditional_enabled(builddir, conditional):
    """Read an Automake conditional value from top-level config.status.

    The ABI runner is usually invoked from test/mpi-abi via recursive
    make, but the conditionals we need live in the top build directory's
    config.status.  Treat an unreadable conditional as unknown, not
    disabled, so a VPATH or partial tree does not accidentally claim a
    configured feature state it did not actually observe.
    """
    config_status = builddir / "config.status"
    if not config_status.exists():
        return {
            "enabled": None,
            "source": "unknown",
        }

    text = config_status.read_text(encoding="utf-8", errors="ignore")
    true_name = conditional + "_TRUE"
    false_name = conditional + "_FALSE"
    true_pattern = r'S\["' + re.escape(true_name) + r'"\]="([^"]*)"'
    false_pattern = r'S\["' + re.escape(false_name) + r'"\]="([^"]*)"'
    true_match = re.search(true_pattern, text)
    false_match = re.search(false_pattern, text)
    if true_match is not None:
        return {
            "enabled": true_match.group(1) == "",
            "source": str(config_status),
        }
    if false_match is not None:
        return {
            "enabled": false_match.group(1) != "",
            "source": str(config_status),
        }
    return {
        "enabled": None,
        "source": str(config_status),
    }


def _detect_fortran_support(builddir):
    """Discover which Open MPI Fortran binding layers were configured."""
    conditionals = {
        "mpif.h": "OMPI_BUILD_FORTRAN_MPIFH_BINDINGS",
        "use mpi": "OMPI_BUILD_FORTRAN_USEMPI_BINDINGS",
        "use mpi_f08": "OMPI_BUILD_FORTRAN_USEMPIF08_BINDINGS",
    }
    detected = {}
    for language, conditional in conditionals.items():
        detected[language] = _conditional_enabled(builddir, conditional)
    return detected


def _detect_optional_features(builddir):
    """Return configured optional MPI features that affect ABI coverage."""
    return {
        "dynamic_process": _detect_dynamic_process_support(),
        "mpi_io": _detect_mpi_io_support(builddir),
        "mpit_events": _detect_mpit_event_support(),
        "rma": _detect_rma_support(builddir),
    }


def _detect_dynamic_process_support():
    """Detect whether launch-sensitive dynamic process probes should run.

    MPI_Comm_spawn, MPI_Comm_connect/accept, and the name-service calls
    are standard MPI APIs, but many CI environments deliberately run with
    launchers or container restrictions that cannot service dynamic
    process management.  Open MPI does not expose a single configure
    conditional that captures that site policy, so the test runner treats
    dynamic process support as enabled unless the caller explicitly
    disables it with OMPI_ABI_TEST_DYNAMIC_PROCESS=0.  That gives
    restricted environments a stable SKIP instead of a hard FAIL while
    still running the probes by default on ordinary local installations.
    """
    override = _env_bool("OMPI_ABI_TEST_DYNAMIC_PROCESS")
    if override is not None:
        return {
            "enabled": override,
            "source": "OMPI_ABI_TEST_DYNAMIC_PROCESS",
        }
    return {
        "enabled": None,
        "source": "unknown",
    }


def _detect_mpi_io_support(builddir):
    """Detect whether the configured build includes MPI-IO support."""
    override = _env_bool("OMPI_ABI_TEST_MPI_IO")
    if override is not None:
        return {
            "enabled": override,
            "source": "OMPI_ABI_TEST_MPI_IO",
        }
    return _conditional_enabled(builddir, "OMPI_OMPIO_SUPPORT")


def _detect_mpit_event_support():
    """Detect whether MPI_T event callback probes should run.

    The MPI_T event APIs may be present in the ABI header even when the
    implementation has no registered event sources that can drive a
    callback.  The C probe performs the definitive runtime check and
    returns a stable skip code when no event registration can be
    allocated.  This override lets sites that know MPI_T events are not
    usable avoid the compile/run step entirely.
    """
    override = _env_bool("OMPI_ABI_TEST_MPIT_EVENTS")
    if override is not None:
        return {
            "enabled": override,
            "source": "OMPI_ABI_TEST_MPIT_EVENTS",
        }
    return {
        "enabled": None,
        "source": "unknown",
    }


def _detect_rma_support(builddir):
    """Detect whether the configured build includes an OSC component.

    RMA/window APIs require at least one OSC component at run time.  Open
    MPI's component Makefiles use DSO conditionals for install mode, so
    a false *_DSO conditional alone does not mean the component is
    disabled; a static build can still configure the component.  The
    generated config.status file is a better authority because it lists
    the component Makefiles that configure selected.
    """
    override = _env_bool("OMPI_ABI_TEST_RMA")
    if override is not None:
        return {
            "enabled": override,
            "source": "OMPI_ABI_TEST_RMA",
        }

    config_status = builddir / "config.status"
    if not config_status.exists():
        return {
            "enabled": None,
            "source": "unknown",
        }

    text = config_status.read_text(encoding="utf-8", errors="ignore")
    configured = sorted(set(
        match.group(1)
        for match in re.finditer(r"ompi/mca/osc/([^/]+)/Makefile", text)
        if match.group(1) != "base"
    ))
    if configured:
        return {
            "enabled": True,
            "source": str(config_status),
            "configured_components": configured,
        }
    if ("ompi/mca/osc/Makefile" not in text and
            "ompi/mca/osc/base/Makefile" not in text):
        return {
            "enabled": None,
            "source": str(config_status),
            "configured_components": configured,
        }
    return {
        "enabled": False,
        "source": str(config_status),
        "configured_components": configured,
    }


def _abi_metadata_version(abi):
    """Return the MPI ABI metadata version as MAJOR.MINOR."""
    mpi = abi.get("metadata", {}).get("mpi", {})
    version = mpi.get("version")
    subversion = mpi.get("subversion")
    if version is None or subversion is None:
        return None
    return "{0}.{1}".format(version, subversion)


def _source_exists(srcdir, stem):
    """Return whether an Open MPI C binding implementation exists."""
    c_dir = srcdir / "ompi" / "mpi" / "c"
    tool_dir = srcdir / "ompi" / "mpi" / "tool"
    candidates = (
        c_dir / (stem + "_abi_generated.c"),
        c_dir / (stem + "_abi.c"),
        c_dir / (stem + ".c.in"),
        tool_dir / (stem + "_abi_generated.c"),
        tool_dir / (stem + ".c.in"),
        tool_dir / (stem + ".c"),
    )
    if stem.startswith("t_"):
        tool_stem = stem[2:]
        candidates += (
            tool_dir / (tool_stem + "_abi_generated.c"),
            tool_dir / (tool_stem + ".c.in"),
            tool_dir / (tool_stem + ".c"),
        )
    return any(candidate.exists() for candidate in candidates)


def _classify_api(srcdir, api_key, api):
    """Classify one MPI API metadata entry for coverage accounting.

    This phase is a manifest-level inventory, not the final executable
    test suite.  Source existence is used as the conservative signal
    that Open MPI currently implements the C ABI binding.  Callback APIs
    are marked separately because many require caller-provided functions
    and cannot be honestly covered by a simple one-shot smoke probe.
    """
    attrs = api.get("attributes", {})
    name = api.get("name", api_key)
    stem = _api_stem(api_key, name)
    family = _api_family(name)
    c_expressible = bool(attrs.get("c_expressible"))
    f90_expressible = bool(attrs.get("f90_expressible"))
    f08_expressible = bool(attrs.get("f08_expressible"))
    mpif_expressible = bool(attrs.get("lis_expressible"))
    callback = bool(attrs.get("callback")) or any(
        param.get("func_type") for param in api.get("parameters", [])
    )

    if not any((c_expressible, f90_expressible, f08_expressible,
                mpif_expressible)):
        classification = CLASS_NOT_IN_STANDARD_ABI
        test_status = TEST_NOT_APPLICABLE
    elif _source_exists(srcdir, stem):
        classification = CLASS_IMPLEMENTED
        test_status = TEST_NOT_WRITTEN
    elif callback:
        classification = CLASS_NOT_IMPLEMENTED
        test_status = TEST_CALLBACK_DEFERRED
    else:
        classification = CLASS_NOT_IMPLEMENTED
        test_status = TEST_NOT_APPLICABLE

    skip_reason = None
    if classification != CLASS_IMPLEMENTED:
        skip_reason = classification

    return {
        "kind": "api",
        "key": api_key,
        "name": name,
        "stem": stem,
        "family": family,
        "classification": classification,
        "test_status": test_status,
        "skip_reason": skip_reason,
        "rank_count": _rank_requirement(family),
        "feature": _feature_requirement(family),
        "callback": callback,
        "requires_callback_test": callback,
        "languages": {
            "c": c_expressible,
            "mpif.h": (
                mpif_expressible and not bool(attrs.get("not_with_mpif"))
            ),
            "use mpi": f90_expressible,
            "use mpi_f08": f08_expressible,
        },
    }


def _classify_constant(key, constant):
    """Classify one standard ABI constant metadata entry."""
    c_handle = constant.get("handle_types", {}).get("c", {})
    return {
        "kind": "constant",
        "key": key,
        "name": constant.get("name", key),
        "category": constant.get("category"),
        "abi_value": constant.get("abi_value"),
        "c_type": c_handle.get("type"),
        "datatypes": constant.get("datatypes", {}),
        "classification": CLASS_IMPLEMENTED,
        "test_status": TEST_NOT_WRITTEN,
        "skip_reason": None,
    }


def load_metadata(srcdir):
    """Load and validate the docs/ MPI API and ABI metadata files.

    docs/ is the authority for this test suite.  Version and entry-count
    checks are deliberately strict so a regenerated or repackaged
    metadata file cannot silently change test coverage.
    """
    api_path = srcdir / "docs" / "mpi-standard-apis.json"
    abi_path = srcdir / "docs" / "mpi-standard-abi.json"
    api_resolved = api_path.resolve()
    abi_resolved = abi_path.resolve()
    apis = _read_json(api_path)
    abi = _read_json(abi_path)
    if not isinstance(apis, dict):
        raise RuntimeError("MPI API metadata must be a JSON object")
    if not isinstance(abi, dict):
        raise RuntimeError("MPI ABI metadata must be a JSON object")
    constants = abi.get("constants", {})
    if not isinstance(constants, dict):
        raise RuntimeError("MPI ABI constants metadata must be a JSON object")
    abi_version = _abi_metadata_version(abi)
    if abi_version != EXPECTED_METADATA_VERSION:
        raise RuntimeError(
            "unexpected MPI ABI metadata version: {0}".format(abi_version))
    if len(apis) != EXPECTED_API_COUNT:
        raise RuntimeError(
            "unexpected MPI API metadata entry count: {0} != {1}".format(
                len(apis), EXPECTED_API_COUNT))
    if len(constants) != EXPECTED_CONSTANT_COUNT:
        raise RuntimeError(
            "unexpected MPI ABI constant metadata entry count: {0} != {1}".
            format(len(constants), EXPECTED_CONSTANT_COUNT))
    return {
        "api_path": str(api_path),
        "abi_path": str(abi_path),
        "api_resolved_path": str(api_resolved),
        "abi_resolved_path": str(abi_resolved),
        "api_version": None,
        "api_version_source": "not present in API metadata",
        "abi_version": abi_version,
        "abi_version_source": "metadata.mpi",
        "expected_api_count": EXPECTED_API_COUNT,
        "expected_constant_count": EXPECTED_CONSTANT_COUNT,
        "apis": apis,
        "constants": constants,
        "abi_metadata": abi.get("metadata", {}),
    }


def build_manifest(srcdir, builddir):
    """Build the full test manifest from metadata and configure output."""
    metadata = load_metadata(srcdir)
    standard_abi = _find_standard_abi_setting(srcdir, builddir)
    fortran = _detect_fortran_support(builddir)
    optional_features = _detect_optional_features(builddir)

    api_entries = [
        _classify_api(srcdir, key, api)
        for key, api in sorted(metadata["apis"].items())
    ]
    constant_entries = [
        _classify_constant(key, constant)
        for key, constant in sorted(metadata["constants"].items())
    ]

    return {
        "metadata": {
            "api_path": metadata["api_path"],
            "abi_path": metadata["abi_path"],
            "api_resolved_path": metadata["api_resolved_path"],
            "abi_resolved_path": metadata["abi_resolved_path"],
            "api_version": metadata["api_version"],
            "api_version_source": metadata["api_version_source"],
            "abi_version": metadata["abi_version"],
            "abi_version_source": metadata["abi_version_source"],
            "versions_match": (
                metadata["api_version"] == metadata["abi_version"]
                if metadata["api_version"] is not None else None
            ),
            "expected_api_count": metadata["expected_api_count"],
            "expected_constant_count": metadata["expected_constant_count"],
            "abi_metadata": metadata["abi_metadata"],
        },
        "configuration": {
            "standard_abi": standard_abi,
            "fortran": fortran,
            "optional_features": optional_features,
        },
        "apis": api_entries,
        "constants": constant_entries,
    }


def _check_result(name, result, details=None, reason=None):
    """Create the normalized check result object used in reports."""
    return {
        "name": name,
        "result": result,
        "details": details or {},
        "skip_reason": reason,
    }


def _fail(name, message, **details):
    payload = {"message": message}
    payload.update(details)
    return _check_result(name, "FAIL", payload)


def _pass(name, **details):
    return _check_result(name, "PASS", details)


def _skip(name, reason, **details):
    return _check_result(name, "SKIP", details, reason)


def _color_tests_enabled(setting):
    """Resolve Automake-compatible color-test settings."""
    if setting in ("yes", "always"):
        return True
    if setting in ("no", "never"):
        return False

    env_setting = os.environ.get("OMPI_ABI_TEST_COLOR_TESTS")
    if env_setting is None:
        env_setting = os.environ.get("AM_COLOR_TESTS")
    if env_setting is not None:
        env_setting = env_setting.strip().lower()
        if env_setting in ("yes", "always"):
            return True
        if env_setting in ("no", "never"):
            return False

    return os.environ.get("TERM") != "dumb" and sys.stdout.isatty()


class _Colors:
    """Apply Automake-style colors to progress lines when enabled."""

    def __init__(self, enabled):
        self.enabled = enabled

    def result(self, status, text):
        if not self.enabled:
            return text
        color = {
            "PASS": ANSI_GREEN,
            "SKIP": ANSI_BLUE,
            "FAIL": ANSI_RED,
            "ERROR": ANSI_MAGENTA,
        }.get(status)
        if color is None:
            return text
        return color + text + ANSI_RESET


class _Progress:
    """Emit make-check-style TEST/PASS/SKIP/FAIL progress lines."""

    def __init__(self, enabled, colors):
        self.enabled = enabled
        self.colors = colors

    def start(self, name):
        if self.enabled:
            print("TEST: {0}".format(name), flush=True)

    def check(self, check):
        if not self.enabled:
            return
        line = "{0}: {1}".format(check["result"], check["name"])
        if check["skip_reason"]:
            line += " ({0})".format(check["skip_reason"])
        print(self.colors.result(check["result"], line), flush=True)


def _append_check(checks, check, progress):
    checks.append(check)
    if progress is not None:
        progress.check(check)


def _extend_checks(checks, new_checks, progress):
    checks.extend(new_checks)
    if progress is not None:
        for check in new_checks:
            progress.check(check)


def _manifest_sanity_checks(manifest):
    """Validate internal manifest classifications before deeper checks run."""
    checks = []
    apis = manifest["apis"]
    constants = manifest["constants"]
    for kind, entries in (("api", apis), ("constant", constants)):
        invalid_classifications = [
            entry["name"] for entry in entries
            if entry["classification"] not in VALID_CLASSIFICATIONS
        ]
        if invalid_classifications:
            checks.append(_fail(
                kind + "_classification_values",
                "invalid manifest classification values",
                entries=invalid_classifications[:20],
                count=len(invalid_classifications)))
        else:
            checks.append(_pass(
                kind + "_classification_values", count=len(entries)))

        invalid_statuses = [
            entry["name"] for entry in entries
            if entry["test_status"] not in VALID_TEST_STATUSES
        ]
        if invalid_statuses:
            checks.append(_fail(
                kind + "_test_status_values",
                "invalid manifest test status values",
                entries=invalid_statuses[:20],
                count=len(invalid_statuses)))
        else:
            checks.append(_pass(kind + "_test_status_values",
                                count=len(entries)))

    bad_skip_reasons = [
        entry["name"] for entry in apis + constants
        if ((entry["classification"] == CLASS_IMPLEMENTED
             and entry["skip_reason"] is not None)
            or (entry["classification"] != CLASS_IMPLEMENTED
                and entry["skip_reason"] is None))
    ]
    if bad_skip_reasons:
        checks.append(_fail(
            "manifest_skip_reasons",
            "manifest entries have inconsistent skip reasons",
            entries=bad_skip_reasons[:20],
            count=len(bad_skip_reasons)))
    else:
        checks.append(_pass("manifest_skip_reasons",
                            count=len(apis) + len(constants)))
    return checks


def _standard_abi_header_path(srcdir, builddir):
    """Return the generated source-tree/build-tree standard ABI mpi.h."""
    candidates = (
        builddir / "ompi" / "mpi" / "c" / "standard_abi" / "mpi.h",
        srcdir / "ompi" / "mpi" / "c" / "standard_abi" / "mpi.h",
    )
    for candidate in candidates:
        if candidate.exists():
            return candidate
    return None


def _strip_c_comments(value):
    value = re.sub(r"/\*.*?\*/", " ", value)
    return value.split("//", 1)[0]


def _strip_c_integer_suffixes(value):
    return re.sub(
        r"\b(0[xX][0-9a-fA-F]+|[0-9]+)(?:[uUlL]+)\b", r"\1", value)


def _strip_c_casts(value):
    type_name = r"(?:const\s+|volatile\s+)?[A-Za-z_]\w*"
    type_expr = type_name + r"(?:\s+" + type_name + r")*\s*\**"
    return re.sub(r"\(\s*" + type_expr + r"\s*\)", " ", value)


def _eval_integer_expression_node(node):
    """Evaluate a restricted Python AST mirroring C integer expressions.

    Header constants may be expressions such as shifts or bitwise ORs,
    not just literal integers.  Using Python's AST gives us structured
    parsing without eval(); only integer literals and arithmetic/bitwise
    operators that can appear in ABI constants are accepted.
    """
    if isinstance(node, ast.Expression):
        return _eval_integer_expression_node(node.body)
    if isinstance(node, ast.Constant) and type(node.value) is int:
        return node.value
    ast_num = getattr(ast, "Num", None)
    if (ast_num is not None and isinstance(node, ast_num) and
            type(node.n) is int):
        return node.n
    if isinstance(node, ast.UnaryOp):
        operand = _eval_integer_expression_node(node.operand)
        if operand is None:
            return None
        if isinstance(node.op, ast.UAdd):
            return operand
        if isinstance(node.op, ast.USub):
            return -operand
        if isinstance(node.op, ast.Invert):
            return ~operand
        return None
    if isinstance(node, ast.BinOp):
        left = _eval_integer_expression_node(node.left)
        right = _eval_integer_expression_node(node.right)
        if left is None or right is None:
            return None
        if isinstance(node.op, ast.Add):
            return left + right
        if isinstance(node.op, ast.Sub):
            return left - right
        if isinstance(node.op, ast.Mult):
            return left * right
        if isinstance(node.op, ast.Mod):
            return left % right
        if isinstance(node.op, ast.BitOr):
            return left | right
        if isinstance(node.op, ast.BitAnd):
            return left & right
        if isinstance(node.op, ast.BitXor):
            return left ^ right
        if isinstance(node.op, ast.LShift) and right >= 0:
            return left << right
        if isinstance(node.op, ast.RShift) and right >= 0:
            return left >> right
        return None
    return None


def _extract_integer_constant(value):
    """Parse a C integer constant expression into an int when safe.

    This intentionally rejects expressions with identifiers such as
    MPI_VERSION because evaluating those would require a preprocessor
    model.  Rejecting and reporting an unparsed constant is safer than a
    regex that happens to pull one integer token out of the wrong place.
    """
    value = _strip_c_integer_suffixes(_strip_c_casts(_strip_c_comments(value)))
    if re.search(r"\bMPI_VERSION\b", value):
        return None
    if re.search(r"[^0-9a-fA-FxX\s()+\-*%<>&|^~]", value):
        return None
    try:
        tree = ast.parse(value, mode="eval")
    except SyntaxError:
        return None
    return _eval_integer_expression_node(tree)


def _metadata_integer_value(value):
    """Normalize an ABI metadata integer value into an int."""
    if isinstance(value, int):
        return value
    if isinstance(value, str):
        try:
            return int(value, 0)
        except ValueError:
            return None
    return None


def _parse_header_constants(path):
    """Parse numeric MPI constants from a standard ABI mpi.h.

    This parser is for semantic value comparison, so it only records
    constants whose expressions can be reduced to integers.  Constants
    that are declared but not numerically parseable are tracked as
    unparsed so the header check can fail with a concrete reason.
    """
    constants = {}
    unparsed = {}
    define_re = re.compile(r"^\s*#define\s+(MPI\w+)\s+(.+?)\s*$")
    enum_re = re.compile(r"^\s*(MPI\w+)\s*=\s*([^,]+),")
    for line in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        match = define_re.match(line)
        if match is None:
            match = enum_re.match(line)
        if match is None:
            continue
        name, value = match.groups()
        integer = _extract_integer_constant(value)
        if integer is not None:
            constants[name] = integer
        else:
            unparsed[name] = value
    return constants, unparsed


def _parse_header_constant_names(path):
    """Parse declared MPI constant names, including implicit enum members.

    Runtime probes need to know whether mpi.h declares a constant, not
    whether its value can be evaluated.  Keep this parser broader than
    _parse_header_constants() so an implicit enum member is not silently
    dropped from the installed-header declaration preflight.
    """
    names = set()
    define_re = re.compile(r"^\s*#define\s+(MPI\w+)\b")
    enum_re = re.compile(r"^\s*(MPI\w+)\b\s*(?:=|,)")
    for line in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        match = define_re.match(line)
        if match is None:
            match = enum_re.match(line)
        if match is not None:
            names.add(match.group(1))
    return names


def _constant_sort_key(entry):
    """Sort constants by ABI value first, then by name."""
    abi_value = _metadata_integer_value(entry["abi_value"])
    if abi_value is None:
        abi_value = sys.maxsize
    return (abi_value, entry["name"])


def _metadata_constant_entries(manifest):
    """Return implemented standard ABI constants from the manifest."""
    return [
        entry for entry in manifest["constants"]
        if entry["classification"] == CLASS_IMPLEMENTED
    ]


def _is_fortran_datatype_constant(entry):
    """Return whether an MPI_Datatype metadata entry is Fortran-only.

    C and Fortran predefined datatypes share the MPI_Datatype C handle
    type, but they are not governed by the same availability rules.
    Separating them lets C datatype probes stay exhaustive while the
    Fortran datatype probe remains gated on configured Fortran support.
    """
    if entry["c_type"] != "MPI_Datatype":
        return False
    if entry["category"] in (
            "OPT_DATATYPES_FORTRAN",
            "REDUCTION_FUNC_DATATYPES_FORTRAN"):
        return True
    datatypes = entry.get("datatypes", {})
    return "c" not in datatypes and (
        "f90" in datatypes or "f08" in datatypes
    )


def _is_c_predefined_handle_constant(entry):
    """Return whether a constant can use a C handle converter.

    The toint/fromint converters apply to MPI object handles, not every
    ABI constant.  For MPI_Datatype, exclude Fortran-only datatypes here
    so they are checked by the Fortran-specific probe and skip policy.
    """
    if entry["c_type"] not in ABI_CONVERTER_KIND_BY_C_TYPE:
        return False
    if entry["c_type"] == "MPI_Datatype":
        return not _is_fortran_datatype_constant(entry)
    return True


def _generated_check_lines(entries, template):
    """Render metadata-selected constants into C probe statements.

    Empty output is allowed here because the caller may be assembling a
    probe that is skipped in the current configuration.  Nonempty family
    requirements are enforced by _runtime_probe_generation_check() before
    installed probes are compiled, where the failure can name the family.
    """
    lines = []
    for entry in sorted(entries, key=_constant_sort_key):
        lines.append(template.format(
            name=entry["name"],
            kind=ABI_CONVERTER_KIND_BY_C_TYPE.get(entry["c_type"])))
    if not lines:
        return "    /* No metadata/header-declared constants to check. */"
    return "\n".join(lines)


def _predefined_handle_entries(manifest):
    """Return metadata constants tested by predefined handle probes."""
    return [
        entry for entry in _metadata_constant_entries(manifest)
        if _is_c_predefined_handle_constant(entry)
    ]


def _fortran_datatype_entries(manifest):
    """Return metadata constants tested by Fortran datatype probes."""
    return [
        entry for entry in _metadata_constant_entries(manifest)
        if _is_fortran_datatype_constant(entry)
    ]


def _declared_fortran_datatype_entries(manifest, declared_names):
    """Return Fortran datatype constants declared by installed mpi.h.

    Optional Fortran datatype availability is compiler/configuration
    dependent.  Unlike C handle constants, the current Phase 8 probe
    covers the subset the installed standard ABI header actually
    declares; unavailable optional datatype behavior is tracked by a
    later task.
    """
    return [
        entry for entry in _fortran_datatype_entries(manifest)
        if entry["name"] in declared_names
    ]


def _error_class_entries(manifest):
    """Return MPI_ERR_* classes that should round-trip via MPI_Error_class."""
    return [
        entry for entry in _metadata_constant_entries(manifest)
        if entry["category"] == "ERROR_CLASSES"
        and entry["name"].startswith("MPI_ERR_")
        and entry["name"] != "MPI_ERR_LASTCODE"
    ]


def _predefined_handle_counts_by_kind(entries):
    """Count generated predefined-handle checks per converter kind."""
    counts = {}
    for entry in entries:
        kind = ABI_CONVERTER_KIND_BY_C_TYPE[entry["c_type"]]
        counts[kind] = counts.get(kind, 0) + 1
    return counts


def _predefined_handle_decls(manifest):
    """Generate C counters for each predefined-handle converter kind."""
    counts = _predefined_handle_counts_by_kind(
        _predefined_handle_entries(manifest))
    lines = []
    for kind in sorted(counts):
        lines.append("    int predefined_{0}_checks = 0;".format(kind))
    return "\n".join(lines)


def _predefined_handle_guards(manifest):
    """Generate C guards that enforce exact per-kind check counts.

    A single aggregate counter is not enough: if a parser or metadata
    change dropped every MPI_Win constant, other handle kinds could keep
    the aggregate nonzero.  Per-kind guards make that erosion visible.
    """
    counts = _predefined_handle_counts_by_kind(
        _predefined_handle_entries(manifest))
    lines = []
    for kind in sorted(counts):
        lines.extend([
            "    if ({0} != predefined_{1}_checks) {{".format(
                counts[kind], kind),
            "        fprintf(stderr, "
            "\"ABI_FAIL:predefined:{0}:count:%d:%d\\n\",".format(kind),
            "                {0}, predefined_{1}_checks);".format(
                counts[kind], kind),
            "        ret = MPI_Finalize();",
            "        if (MPI_SUCCESS != ret) {",
            "            return 21;",
            "        }",
            "        return 22;",
            "    }",
        ])
    return "\n".join(lines)


def _predefined_handle_checks(manifest):
    """Generate predefined-handle converter round-trip statements."""
    entries = [
        entry for entry in _predefined_handle_entries(manifest)
    ]
    return _generated_check_lines(
        entries,
        "    ABI_CHECK_PREDEFINED_HANDLE({kind}, {name});")


def _fortran_datatype_checks(manifest, declared_names):
    """Generate Fortran datatype converter round-trip statements."""
    entries = _declared_fortran_datatype_entries(manifest, declared_names)
    return _generated_check_lines(
        entries,
        "    ABI_CHECK_FORTRAN_TYPE({name});")


def _error_class_checks(manifest):
    """Generate MPI_Error_class statements for predefined error classes."""
    entries = _error_class_entries(manifest)
    return _generated_check_lines(
        entries,
        "    ABI_CHECK_ERROR_CLASS({name});")


def _runtime_probe_constant_names(manifest, include_fortran, declared_names):
    """Return constants expected in installed mpi.h for runtime probes."""
    entries = (
        _predefined_handle_entries(manifest) +
        _error_class_entries(manifest)
    )
    if include_fortran:
        entries += _declared_fortran_datatype_entries(
            manifest, declared_names)
    return set(entry["name"] for entry in entries)


def _runtime_probe_generation_check(manifest, include_fortran,
                                    declared_names):
    """Fail if metadata-derived runtime probe families would be empty.

    The runtime probes are metadata-driven so future constants are picked
    up automatically.  That same indirection can hide mistakes if a
    classification rule starts returning an empty family; this preflight
    fails before C generation so an empty generated block is never
    accepted as an intentional zero-check probe.
    """
    predefined_entries = _predefined_handle_entries(manifest)
    predefined_counts = _predefined_handle_counts_by_kind(predefined_entries)
    missing_kinds = [
        kind for kind in ABI_CONVERTER_REQUIRED_KINDS
        if predefined_counts.get(kind, 0) == 0
    ]
    error_class_count = len(_error_class_entries(manifest))
    fortran_type_count = (
        len(_declared_fortran_datatype_entries(manifest, declared_names))
        if include_fortran else None
    )

    if missing_kinds or error_class_count == 0 or (
            include_fortran and fortran_type_count == 0):
        return _fail(
            "installed_c_probe_generation",
            "metadata-derived converter probe family is empty",
            missing_predefined_handle_kinds=missing_kinds,
            predefined_handle_counts=predefined_counts,
            error_class_count=error_class_count,
            fortran_type_count=fortran_type_count)

    return _pass(
        "installed_c_probe_generation",
        predefined_handle_counts=predefined_counts,
        error_class_count=error_class_count,
        fortran_type_count=fortran_type_count)


def _runtime_api_probe_api_names(cases, include_support=False):
    """Return API names referenced by installed runtime API probes.

    api_names is the primary coverage set.  support_api_names records
    setup/teardown calls such as MPI_Init or MPI_Comm_rank that should be
    validated against metadata and the header but not counted as the
    probe's advertised family coverage.
    """
    names = set()
    for case in cases:
        names.update(case.get("api_names", ()))
        if include_support:
            names.update(case.get("support_api_names", ()))
    return names


def _runtime_api_probe_family_counts(cases):
    """Count primary runtime API probe coverage by declared family."""
    counts = {}
    for case in cases:
        family = case["family"]
        counts[family] = counts.get(family, 0) + len(case["api_names"])
    return counts


def _runtime_audit_work_package(entry):
    """Return the Phase 9/10 work package that owns an uncovered API.

    This audit is deliberately advisory until the completion gate is
    enabled.  It gives future implementation chunks a metadata-derived
    missing list without pretending that every implemented C API needs a
    runtime probe: ABI helpers and integer handle converters are covered
    by earlier phases, callback APIs belong to Phase 10, and some APIs
    need explicit skip/defer policy rather than local CI execution.
    """
    name = entry["name"]
    family = entry["family"]
    if entry["callback"]:
        return "phase10_callback"
    if name.startswith("MPI_Abi_"):
        return "phase8_abi_helper"
    if name.endswith("_toint") or name.endswith("_fromint"):
        return "phase8_converter"
    if name in (
        "MPI_Comm_delete_attr",
        "MPI_Comm_free_keyval",
        "MPI_Comm_set_attr",
        "MPI_Grequest_complete",
        "MPI_Op_free",
        "MPI_Type_delete_attr",
        "MPI_Type_free_keyval",
        "MPI_Type_get_attr",
        "MPI_Type_set_attr",
        "MPI_Win_delete_attr",
        "MPI_Win_free_keyval",
        "MPI_Win_set_attr",
    ):
        return "phase10_callback"
    if name in (
        "MPI_Comm_accept",
        "MPI_Comm_connect",
        "MPI_Comm_disconnect",
        "MPI_Comm_get_parent",
        "MPI_Comm_join",
        "MPI_Comm_spawn",
        "MPI_Comm_spawn_multiple",
    ):
        return "chunk9h_dynamic_process"
    if name == "MPI_Abort":
        return "chunk9i_intentional_abort"
    if family in (
        "point_to_point", "bsend", "ibsend", "ssend", "issend",
        "rsend", "irsend", "sendrecv", "isendrecv", "mprobe",
        "improbe", "mrecv", "imrecv", "message", "psend", "precv",
        "pready", "parrived", "request", "wait", "waitall",
        "waitany", "waitsome", "test", "testall", "testany",
        "testsome", "start", "startall", "cancel", "buffer",
    ):
        return "chunk9b_point_to_point_request"
    if family in (
        "cart", "cartdim", "graph", "graphdims", "dist", "dims",
        "topo", "neighbor", "ineighbor",
    ):
        return "chunk9c_topology_neighbor"
    if family in (
        "collective", "alltoallv", "alltoallw", "iallgather",
        "iallgatherv", "iallreduce", "ialltoall", "ialltoallv",
        "ialltoallw", "ibarrier", "ibcast", "iexscan", "igather",
        "igatherv", "ireduce", "iscan", "iscatter", "iscatterv",
        "scatter", "scatterv",
    ):
        return "chunk9d_collective"
    if family in ("type", "status", "pack", "unpack", "op"):
        return "chunk9e_datatype_status_pack_op"
    if family in ("comm", "group", "intercomm"):
        return "chunk9_comm_group_remaining"
    if family in ("win", "rma", "compare", "fetch", "raccumulate"):
        return "chunk9f_rma_window"
    if family == "file":
        return "chunk9g_mpi_io"
    if family in (
        "open", "close", "publish", "lookup", "unpublish",
        "spawn", "connect", "accept",
    ):
        return "chunk9h_dynamic_process"
    if family == "mpi_t":
        return "chunk9i_mpi_t"
    return "chunk9i_misc"


def _runtime_explicit_skip_reason(name, package):
    """Return the stable reason for APIs intentionally not run in Phase 9."""
    if package == "phase10_callback":
        return SKIP_PHASE10_CALLBACK_REQUIRED
    if name == "MPI_Abort":
        return SKIP_MPI_ABORT_TERMINATES_JOB
    if name == "MPI_Comm_join":
        return SKIP_COMM_JOIN_REQUIRES_CONNECTED_FD
    return None


def _runtime_api_coverage_audit(manifest, header, cases):
    """Report implemented C APIs not yet covered by runtime probes.

    The check result is PASS during phased development.  The completion
    gate is the place that eventually turns any remaining
    test_not_written_yet coverage into a hard failure.  Keeping this as
    an installed check means it audits against the same standard ABI
    header that runtime probes compile against.
    """
    prototypes = _parse_c_header_prototypes(header)
    declared_names = set(prototypes)
    runtime_names = _runtime_api_probe_api_names(
        cases, include_support=True)
    missing_by_package = {}
    explicit_skips_by_package = {}
    covered = 0
    explicitly_skipped = 0
    skipped_not_declared = 0

    for entry in manifest["apis"]:
        if entry["classification"] != CLASS_IMPLEMENTED:
            continue
        if not entry["languages"]["c"]:
            continue
        name = entry["name"]
        if name not in declared_names:
            skipped_not_declared += 1
            continue
        package = _runtime_audit_work_package(entry)
        if package in ("phase8_abi_helper", "phase8_converter"):
            covered += 1
            continue
        if name in runtime_names:
            covered += 1
            continue
        skip_reason = _runtime_explicit_skip_reason(name, package)
        if skip_reason is not None:
            explicitly_skipped += 1
            explicit_skips_by_package.setdefault(package, []).append({
                "name": name,
                "skip_reason": skip_reason,
            })
            continue
        missing_by_package.setdefault(package, []).append(name)

    summarized = {
        package: names[:20]
        for package, names in sorted(missing_by_package.items())
    }
    counts = {
        package: len(names)
        for package, names in sorted(missing_by_package.items())
    }
    skip_counts = {
        package: len(items)
        for package, items in sorted(explicit_skips_by_package.items())
    }
    summarized_skips = {
        package: items[:20]
        for package, items in sorted(explicit_skips_by_package.items())
    }
    return _pass(
        "installed_c_runtime_api_coverage_audit",
        covered=covered,
        explicitly_skipped=explicitly_skipped,
        explicit_skips_by_package=summarized_skips,
        explicit_skips_by_package_counts=skip_counts,
        missing_count=sum(counts.values()),
        missing_by_package=summarized,
        missing_by_package_counts=counts,
        skipped_not_declared=skipped_not_declared)


def _runtime_api_probe_body_calls(srcdir, case):
    """Return MPI API calls made by a runtime probe source.

    The runtime probe table is declarative: api_names/support_api_names
    tell reports what a C body and its optional file-scope prologue are
    supposed to exercise.  Keep that declaration tied to the generated
    source by checking for direct MPI_* function calls in both snippets.
    If later probes need function pointers or macro indirection, they
    should extend this guard instead of weakening the coverage contract.
    """
    call_re = re.compile(r"(?<![A-Za-z0-9_])(MPI_[A-Za-z0-9_]+)\s*\(")
    source = _probe_prologue_text(srcdir, case)
    source += "\n"
    source += _probe_body_text(srcdir, case)
    return set(call_re.findall(source))


def _runtime_api_probe_generation_check(
        srcdir, manifest, header, cases,
        check_name="installed_c_runtime_api_probe_generation"):
    """Validate the runtime API probe table before compiling C sources.

    Phase 9 starts with seed API-family probes, not exhaustive family
    coverage.  Even so, each seed probe names its intended MPI API
    coverage explicitly.  This preflight keeps that contract honest by
    checking that every named API exists in docs/ metadata, is classified
    as implemented in this tree, and is declared by the installed
    standard ABI header that mpicc_abi will use for the generated source.
    It also checks the declared coverage against the source body so a
    stale api_names entry cannot claim coverage for a call that was
    removed.
    """
    entries_by_name = {
        entry["name"]: entry for entry in manifest["apis"]
    }
    primary_names = _runtime_api_probe_api_names(cases)
    all_names = _runtime_api_probe_api_names(cases, include_support=True)
    prototypes = _parse_c_header_prototypes(header)
    declared_names = set(prototypes)

    missing_metadata = sorted(
        name for name in all_names if name not in entries_by_name)
    not_implemented = sorted(
        name for name in all_names
        if name in entries_by_name and
        entries_by_name[name]["classification"] != CLASS_IMPLEMENTED)
    missing_header = sorted(name for name in all_names
                            if name not in declared_names)

    empty_cases = sorted(case["name"] for case in cases
                         if not case.get("api_names"))

    missing_body_calls = {}
    undeclared_body_calls = {}
    body_file_errors = {}
    prologue_file_errors = {}
    for case in cases:
        advertised = (
            set(case.get("api_names", ())) |
            set(case.get("support_api_names", ()))
        )
        try:
            _probe_prologue_text(srcdir, case)
        except RuntimeError as exc:
            prologue_file_errors[case["name"]] = str(exc)
        try:
            called = _runtime_api_probe_body_calls(srcdir, case)
        except RuntimeError as exc:
            body_file_errors[case["name"]] = str(exc)
            continue
        missing = sorted(advertised - called)
        undeclared = sorted(called - advertised)
        if missing:
            missing_body_calls[case["name"]] = missing
        if undeclared:
            undeclared_body_calls[case["name"]] = undeclared

    if (missing_metadata or not_implemented or missing_header or
            empty_cases or missing_body_calls or undeclared_body_calls or
            body_file_errors or prologue_file_errors):
        return _fail(
            check_name,
            "runtime API probe table does not match metadata/header",
            missing_metadata=missing_metadata,
            not_implemented=not_implemented,
            missing_header=missing_header[:20],
            missing_header_count=len(missing_header),
            empty_cases=empty_cases,
            missing_body_calls=missing_body_calls,
            undeclared_body_calls=undeclared_body_calls,
            body_file_errors=body_file_errors,
            prologue_file_errors=prologue_file_errors,
            probe_count=len(cases))

    return _pass(
        check_name,
        probe_count=len(cases),
        primary_api_count=len(primary_names),
        support_api_count=len(all_names - primary_names),
        family_counts=_runtime_api_probe_family_counts(cases))


def _callback_api_work_package(name):
    """Return which callback implementation chunk owns an API name."""
    if name in CALLBACK_ATTRIBUTE_API_NAMES:
        return "chunk10a_attribute_callbacks"
    if name in LEGACY_ATTRIBUTE_API_NAMES:
        return "chunk10a_legacy_attribute_callbacks"
    if name in PHASE10B_CALLBACK_API_NAMES:
        return "chunk10b_callback_lifetime"
    return None


def _callback_api_coverage_audit(manifest, header, cases):
    """Report callback-owned C ABI APIs not covered by callback probes.

    The ordinary runtime audit treats callback APIs as deferred because
    callback failures can leave MPI objects or requests in undefined
    states.  This audit is the Phase 10 counterpart: every implemented
    callback-owned C API declared by the installed standard ABI header
    must be covered by a callback probe.  Unsupported optional callback
    families are still represented by probes, but those probes skip at
    run time with stable reasons when the installed implementation lacks
    the underlying feature.
    """
    prototypes = _parse_c_header_prototypes(header)
    declared_names = set(prototypes)
    covered_names = _runtime_api_probe_api_names(cases)
    entries_by_name = {
        entry["name"]: entry for entry in manifest["apis"]
    }
    missing_by_package = {}
    legacy_not_declared = []
    unclassified_callback_apis = []
    covered = 0

    for entry in manifest["apis"]:
        if not entry["languages"]["c"]:
            continue
        name = entry["name"]
        package = _callback_api_work_package(name)
        if package is None:
            # Hard-coded ownership lists are intentional for reviewable
            # chunks, but they must not silently hide new callback APIs
            # that appear in docs/ metadata or the installed ABI header.
            # Treat any implemented, declared C API that metadata marks
            # as callback-owned as an audit failure until the API is
            # assigned to this chunk, a later callback chunk, or an
            # explicit skip category.
            if (entry["classification"] == CLASS_IMPLEMENTED and
                    name in declared_names and
                    (entry["callback"] or
                     _runtime_audit_work_package(entry) ==
                     "phase10_callback")):
                unclassified_callback_apis.append(name)
            continue
        if entry["classification"] != CLASS_IMPLEMENTED:
            if name in LEGACY_ATTRIBUTE_API_NAMES and name not in declared_names:
                legacy_not_declared.append(name)
            continue
        if name not in declared_names:
            if name in LEGACY_ATTRIBUTE_API_NAMES:
                legacy_not_declared.append(name)
            continue
        if name in covered_names:
            covered += 1
            continue
        missing_by_package.setdefault(package, []).append(name)

    legacy_declared = sorted(
        name for name in LEGACY_ATTRIBUTE_API_NAMES
        if name in declared_names and
        entries_by_name.get(name, {}).get("classification") ==
        CLASS_IMPLEMENTED
    )
    if legacy_declared:
        missing_by_package.setdefault(
            "chunk10a_legacy_attribute_callbacks", []).extend(
                legacy_declared)

    counts = {
        package: len(names)
        for package, names in sorted(missing_by_package.items())
    }
    details = dict(
        covered=covered,
        legacy_not_declared=sorted(set(legacy_not_declared)),
        missing_by_package={
            package: names[:20]
            for package, names in sorted(missing_by_package.items())
        },
        missing_by_package_counts=counts,
        missing_count=sum(counts.values()),
        unclassified_callback_apis=sorted(unclassified_callback_apis),
        unclassified_callback_api_count=len(unclassified_callback_apis),
    )
    if missing_by_package or unclassified_callback_apis:
        return _fail(
            "installed_c_callback_api_coverage_audit",
            "callback API coverage has undeferred gaps",
            **details)
    return _pass("installed_c_callback_api_coverage_audit", **details)


def _prepare_installed_c_probe_body(srcdir, case, manifest, declared_names):
    """Expand generated C snippets inside one installed probe body.

    Long checked-in body snippets stay readable by using placeholders
    for generated constant lists and expected counts.  Substitution
    happens immediately before writing the temporary source, after the
    manifest has been built, configure-dependent Fortran gating is
    known, and the installed ABI header's declared Fortran datatype set
    has been parsed.
    """
    body = _probe_body_text(srcdir, case)
    replacements = {
        "@PREDEFINED_HANDLE_DECLS@": _predefined_handle_decls(manifest),
        "@PREDEFINED_HANDLE_CHECKS@": _predefined_handle_checks(manifest),
        "@PREDEFINED_HANDLE_GUARDS@": _predefined_handle_guards(manifest),
        "@FORTRAN_TYPE_CHECKS@": _fortran_datatype_checks(
            manifest, declared_names),
        "@ERROR_CLASS_CHECKS@": _error_class_checks(manifest),
        "@FORTRAN_TYPE_EXPECTED@": str(len(
            _declared_fortran_datatype_entries(manifest, declared_names))),
        "@ERROR_CLASS_EXPECTED@": str(len(_error_class_entries(manifest))),
    }
    for placeholder, generated in replacements.items():
        body = body.replace(placeholder, generated)
    return body


def _header_constant_checks(manifest, srcdir, builddir):
    """Compare generated standard ABI header constants to ABI metadata.

    This is the fast, in-tree constant check.  It validates the generated
    header against docs/ metadata without requiring an installed Open MPI
    or mpirun, so it can run before the installed runtime probes.
    """
    path = _standard_abi_header_path(srcdir, builddir)
    if path is None:
        return [_skip("standard_abi_header_constants",
                      SKIP_HEADER_UNAVAILABLE)]

    header_constants, unparsed_header_constants = _parse_header_constants(path)
    missing = []
    mismatches = []
    unparsed = []
    checked = 0
    skipped = []
    for entry in manifest["constants"]:
        name = entry["name"]
        expected = _metadata_integer_value(entry["abi_value"])
        if expected is None:
            skipped.append(name)
            continue
        if entry["c_type"] is None:
            skipped.append(name)
            continue
        if entry["category"] == "DEPRECATED_FUNCS":
            skipped.append(name)
            continue
        checked += 1
        if name not in header_constants:
            if name in unparsed_header_constants:
                unparsed.append(name)
            else:
                missing.append(name)
        elif header_constants[name] != expected:
            mismatches.append({
                "name": name,
                "expected": expected,
                "actual": header_constants[name],
            })

    if checked == 0:
        return [_fail(
            "standard_abi_header_constants",
            "no standard ABI header constants were validated",
            header=str(path),
            checked=checked,
            skipped_count=len(skipped),
            parsed_header_count=len(header_constants),
            unparsed_header_count=len(unparsed_header_constants))]

    if missing or mismatches or unparsed:
        return [_fail(
            "standard_abi_header_constants",
            "standard ABI header constants differ from metadata",
            header=str(path),
            checked=checked,
            skipped_count=len(skipped),
            missing=missing[:20],
            missing_count=len(missing),
            unparsed=unparsed[:20],
            unparsed_count=len(unparsed),
            mismatches=mismatches[:20],
            mismatch_count=len(mismatches))]

    return [_pass("standard_abi_header_constants",
                  header=str(path),
                  checked=checked,
                  skipped_count=len(skipped))]


def _require_substrings(text, requirements):
    missing = []
    for label, substring in requirements:
        if substring not in text:
            missing.append(label)
    return missing


def _token_pattern(token):
    return r"(?<![A-Za-z0-9_])" + re.escape(token) + \
           r"(?![A-Za-z0-9_])"


def _contains_token(text, token):
    return re.search(_token_pattern(token), text) is not None


def _abi_converter_checks(srcdir):
    """Check source contracts for ABI converter implementation files.

    These are source-contract checks, not behavioral unit tests.  They
    make sure each converter source is present, wired into the ABI
    makefile, and contains the expected wrapper/conversion structure.
    Runtime converter behavior is exercised later by installed C probes.
    """
    c_dir = srcdir / "ompi" / "mpi" / "c"
    makefile = c_dir / "Makefile_abi.include"
    converter_header = c_dir / "abi_converters.h"
    converter_source = c_dir / "abi_converters.c"
    missing_files = []
    missing_patterns = []

    for path in (makefile, converter_header, converter_source):
        if not path.exists():
            missing_files.append(str(path))

    if missing_files:
        return [_fail(
            "abi_converter_sources",
            "required ABI converter files are missing",
            missing_files=missing_files)]

    makefile_text = _read_text(makefile)
    header_text = _read_text(converter_header)
    source_text = _read_text(converter_source)

    header_requirements = [
        ("OMPI_ABI_HANDLE_BASE_OFFSET", "#define OMPI_ABI_HANDLE_BASE_OFFSET"),
        ("ABI base offset value", "16385"),
        ("error conversion to OMPI", "ompi_convert_abi_error_intern_error"),
        ("error conversion to ABI", "ompi_convert_intern_error_abi_error"),
        ("status conversion to OMPI", "ompi_convert_abi_status_intern_status"),
        ("status conversion to ABI", "ompi_convert_intern_status_abi_status"),
    ]
    for label in _require_substrings(header_text, header_requirements):
        missing_patterns.append({
            "file": str(converter_header),
            "pattern": label,
        })

    source_requirements = [
        ("errhandler argument converter",
         "ompi_convert_errhandler_args_intern_to_abi"),
        ("communicator callback conversion",
         "ompi_convert_comm_ompi_to_standard"),
        ("window callback conversion", "ompi_convert_win_ompi_to_standard"),
        ("file callback conversion", "ompi_convert_file_ompi_to_standard"),
        ("session callback conversion",
         "ompi_convert_session_ompi_to_standard"),
    ]
    for label in _require_substrings(source_text, source_requirements):
        missing_patterns.append({
            "file": str(converter_source),
            "pattern": label,
        })

    checked_sources = 0
    for symbol, stem in ABI_CONVERTER_HANDLES:
        to_file = c_dir / (stem + "_toint_abi.c")
        from_file = c_dir / (stem + "_fromint_abi.c")
        to_source_name = to_file.name
        from_source_name = from_file.name
        to_symbol = "MPI_" + symbol + "_toint"
        from_symbol = "MPI_" + symbol + "_fromint"
        pto_symbol = "PMPI_" + symbol + "_toint"
        pfrom_symbol = "PMPI_" + symbol + "_fromint"

        for path in (to_file, from_file):
            if not path.exists():
                missing_files.append(str(path))

        for source_name in (to_source_name, from_source_name):
            if source_name not in makefile_text:
                missing_patterns.append({
                    "file": str(makefile),
                    "pattern": source_name,
                })

        if to_file.exists():
            to_text = _read_text(to_file)
            to_requirements = [
                ("abi.h include", '#include "ompi/mpi/c/abi.h"'),
                ("abi_converters.h include",
                 '#include "ompi/mpi/c/abi_converters.h"'),
                ("profiling weak symbol",
                 "#pragma weak {0} = {1}".format(to_symbol, pto_symbol)),
                ("function name", 'FUNC_NAME[] = "{0}"'.format(to_symbol)),
                ("public function", to_symbol + "("),
                ("preserve ABI integer handles",
                 "OMPI_ABI_HANDLE_BASE_OFFSET >"),
                ("apply ABI base offset",
                 "+= OMPI_ABI_HANDLE_BASE_OFFSET"),
            ]
            for label in _require_substrings(to_text, to_requirements):
                missing_patterns.append({
                    "file": str(to_file),
                    "pattern": label,
                })
            checked_sources += 1

        if from_file.exists():
            from_text = _read_text(from_file)
            from_requirements = [
                ("abi.h include", '#include "ompi/mpi/c/abi.h"'),
                ("abi_converters.h include",
                 '#include "ompi/mpi/c/abi_converters.h"'),
                ("profiling weak symbol",
                 "#pragma weak {0} = {1}".format(from_symbol, pfrom_symbol)),
                ("function name", 'FUNC_NAME[] = "{0}"'.format(from_symbol)),
                ("public function", from_symbol + "("),
                ("preserve ABI integer handles",
                 "OMPI_ABI_HANDLE_BASE_OFFSET >"),
                ("remove ABI base offset",
                 "- OMPI_ABI_HANDLE_BASE_OFFSET"),
                ("recover OMPI object", "opal_pointer_array_get_item"),
            ]
            for label in _require_substrings(from_text, from_requirements):
                missing_patterns.append({
                    "file": str(from_file),
                    "pattern": label,
                })
            checked_sources += 1

    if missing_files or missing_patterns:
        return [_fail(
            "abi_converter_sources",
            "ABI converter source checks failed",
            checked_sources=checked_sources,
            missing_files=missing_files[:20],
            missing_file_count=len(missing_files),
            missing_patterns=missing_patterns[:20],
            missing_pattern_count=len(missing_patterns))]

    return [_pass("abi_converter_sources",
                  checked_sources=checked_sources,
                  handle_count=len(ABI_CONVERTER_HANDLES))]


def _fortran_enabled(manifest, language):
    """Return whether one Fortran binding layer was configured."""
    return manifest["configuration"]["fortran"][language]["enabled"]


def _fortran_mpifh_helper_checks(srcdir, manifest):
    """Check mpif.h ABI helper source contracts.

    The helper entry points support legacy Fortran binding layers even
    though the standard ABI is centered on mpi_f08.  This check verifies
    the generated symbol names and PMPI forwarding contracts without
    requiring a Fortran compiler or installed runtime.
    """
    enabled = _fortran_enabled(manifest, "mpif.h")
    if enabled is False:
        return [_skip("fortran_mpifh_abi_helpers",
                      SKIP_FORTRAN_BINDINGS_DISABLED,
                      language="mpif.h")]

    base = srcdir / "ompi" / "mpi" / "fortran" / "mpif-h"
    makefile = base / "Makefile.am"
    prototypes = base / "prototypes_mpi.h"
    missing_files = []
    missing_patterns = []

    for path in (makefile, prototypes):
        if not path.exists():
            missing_files.append(str(path))

    makefile_text = _read_text(makefile) if makefile.exists() else ""
    prototypes_text = _read_text(prototypes) if prototypes.exists() else ""
    checked_sources = 0

    for helper in FORTRAN_ABI_HELPERS:
        c_name = helper + "_f.c"
        c_path = base / c_name
        helper_suffix = helper[4:]
        mixed_name = "MPI_Abi_" + helper_suffix
        lower_name = "mpi_abi_" + helper_suffix
        upper_name = lower_name.upper()
        internal_name = "ompi_" + helper + "_f"
        pmpi_name = "PMPI_Abi_" + helper_suffix

        if not c_path.exists():
            missing_files.append(str(c_path))
            continue

        checked_sources += 1
        text = _read_text(c_path)
        requirements = [
            ("Makefile source", c_name),
            ("prototype declaration", mixed_name),
            ("uppercase weak symbol", upper_name),
            ("lowercase weak symbol", lower_name),
            ("mixed-case weak symbol", mixed_name + "_f"),
            ("f08 weak symbol", mixed_name + "_f08"),
            ("internal wrapper", internal_name),
            ("calls PMPI C helper", pmpi_name),
        ]
        for label, pattern in requirements:
            haystack = makefile_text if label == "Makefile source" else text
            if label == "prototype declaration":
                haystack = prototypes_text
            found = pattern in haystack
            if label != "Makefile source":
                found = _contains_token(haystack, pattern)
            if not found:
                missing_patterns.append({
                    "file": str(c_path if label != "prototype declaration"
                                else prototypes),
                    "pattern": label,
                    "helper": helper,
                })

    if missing_files or missing_patterns:
        return [_fail(
            "fortran_mpifh_abi_helpers",
            "mpif.h ABI helper source checks failed",
            configured=enabled,
            checked_sources=checked_sources,
            missing_files=missing_files[:20],
            missing_file_count=len(missing_files),
            missing_patterns=missing_patterns[:20],
            missing_pattern_count=len(missing_patterns))]

    return [_pass("fortran_mpifh_abi_helpers",
                  configured=enabled,
                  checked_sources=checked_sources)]


def _fortran_usempi_helper_checks(srcdir, manifest):
    """Check the use mpi layer's shared-helper status.

    use mpi shares the mpif.h helper entry points in this code base, so
    there is no independent per-helper implementation to validate here.
    Reporting a stable SKIP is more honest than a PASS with no specific
    layer contract.
    """
    enabled = _fortran_enabled(manifest, "use mpi")
    if enabled is False:
        return [_skip("fortran_usempi_abi_helpers",
                      SKIP_FORTRAN_BINDINGS_DISABLED,
                      language="use mpi")]

    makefile = (srcdir / "ompi" / "mpi" / "fortran" / "use-mpi" /
                "Makefile.am")
    if not makefile.exists():
        return [_fail(
            "fortran_usempi_abi_helpers",
            "use mpi Makefile is missing",
            configured=enabled,
            missing_files=[str(makefile)])]

    return [_skip(
        "fortran_usempi_abi_helpers",
        SKIP_FORTRAN_HELPERS_SHARED,
        configured=enabled,
        source=str(makefile),
        note="use mpi ABI helpers share the mpif.h helper entry points")]


def _fortran_f08_helper_checks(srcdir, manifest):
    """Check use mpi_f08 ABI helper templates and generated interfaces.

    This layer is the ABI-relevant Fortran binding for MPI-5.x.  The
    checks look for template inputs and generated interface renames so a
    missing f08 helper is caught during fast source checks.
    """
    enabled = _fortran_enabled(manifest, "use mpi_f08")
    if enabled is False:
        return [_skip("fortran_f08_abi_helpers",
                      SKIP_FORTRAN_BINDINGS_DISABLED,
                      language="use mpi_f08")]

    base = srcdir / "ompi" / "mpi" / "fortran" / "use-mpi-f08"
    prototype_file = base / "Makefile.prototype_files"
    interface_file = base / "mod" / "mpi-f08-interfaces-generated.h"
    missing_files = []
    missing_patterns = []

    for path in (prototype_file, interface_file):
        if not path.exists():
            missing_files.append(str(path))

    prototype_text = (
        _read_text(prototype_file) if prototype_file.exists() else ""
    )
    interface_text = (
        _read_text(interface_file) if interface_file.exists() else ""
    )
    checked_templates = 0

    for helper in FORTRAN_ABI_HELPERS:
        template_name = helper + ".c.in"
        template_path = base / template_name
        mpi_name = "MPI_Abi_" + helper[4:]
        pmpi_name = "PMPI_Abi_" + helper[4:]

        if not template_path.exists():
            missing_files.append(str(template_path))
            continue

        checked_templates += 1
        template_text = _read_text(template_path)
        requirements = [
            ("prototype file entry", template_name, prototype_text,
             str(prototype_file), False),
            ("PROTOTYPE declaration", "PROTOTYPE", template_text,
             str(template_path), False),
            ("inner C call", "@INNER_CALL@", template_text,
             str(template_path), False),
            ("generated f08 rename", mpi_name + "_f08", interface_text,
             str(interface_file), True),
            ("generated generic rename", mpi_name, interface_text,
             str(interface_file), True),
            ("generated PMPI target", pmpi_name, interface_text,
             str(interface_file), True),
        ]
        for label, pattern, haystack, source, token_match in requirements:
            found = _contains_token(haystack, pattern) if token_match \
                    else pattern in haystack
            if not found:
                missing_patterns.append({
                    "file": source,
                    "pattern": label,
                    "helper": helper,
                })

    if missing_files or missing_patterns:
        return [_fail(
            "fortran_f08_abi_helpers",
            "use mpi_f08 ABI helper source checks failed",
            configured=enabled,
            checked_templates=checked_templates,
            missing_files=missing_files[:20],
            missing_file_count=len(missing_files),
            missing_patterns=missing_patterns[:20],
            missing_pattern_count=len(missing_patterns))]

    return [_pass("fortran_f08_abi_helpers",
                  configured=enabled,
                  checked_templates=checked_templates)]


def _fortran_helper_checks(srcdir, manifest):
    """Run all configured Fortran ABI helper source checks."""
    checks = []
    checks.extend(_fortran_mpifh_helper_checks(srcdir, manifest))
    checks.extend(_fortran_usempi_helper_checks(srcdir, manifest))
    checks.extend(_fortran_f08_helper_checks(srcdir, manifest))
    return checks


def _fortran_bindings_enabled(manifest):
    """Return whether any Fortran binding layer is enabled."""
    return any(
        item.get("enabled") is True
        for item in manifest["configuration"]["fortran"].values()
    )


def _optional_feature_info(manifest, feature):
    """Return configured optional feature state from the manifest."""
    return manifest["configuration"].get("optional_features", {}).get(
        feature, {
            "enabled": None,
            "source": "unknown",
        })


def _optional_feature_skip_reason(feature):
    """Return the stable skip reason for a disabled optional feature."""
    reasons = {
        "dynamic_process": SKIP_DYNAMIC_PROCESS_DISABLED,
        "mpi_io": SKIP_MPI_IO_SUPPORT_DISABLED,
        "mpit_events": SKIP_MPIT_EVENTS_DISABLED,
        "rma": SKIP_RMA_SUPPORT_DISABLED,
    }
    return reasons.get(feature, "optional_feature_disabled")


def run_fast_checks(manifest, srcdir, builddir, progress=None):
    """Run in-tree checks that do not require an installed Open MPI."""
    checks = []
    if progress is not None:
        progress.start("fast manifest sanity checks")
    _extend_checks(checks, _manifest_sanity_checks(manifest), progress)

    if progress is not None:
        progress.start("fast standard ABI header constants")
    _extend_checks(
        checks, _header_constant_checks(manifest, srcdir, builddir), progress)

    if progress is not None:
        progress.start("fast ABI converter source checks")
    _extend_checks(checks, _abi_converter_checks(srcdir), progress)

    if progress is not None:
        progress.start("fast Fortran ABI helper source checks")
    _extend_checks(checks, _fortran_helper_checks(srcdir, manifest), progress)
    return checks


def _count_by(entries, key):
    counts = {}
    for entry in entries:
        value = entry.get(key)
        counts[value] = counts.get(value, 0) + 1
    return counts


def _language_counts(entries):
    counts = {
        "c": 0,
        "mpif.h": 0,
        "use mpi": 0,
        "use mpi_f08": 0,
    }
    for entry in entries:
        for language, enabled in entry.get("languages", {}).items():
            if enabled:
                counts[language] += 1
    return counts


def _check_counts(checks):
    return _count_by(checks, "result")


def _command_timeout():
    """Return the timeout used for compile, launcher, and inspection jobs.

    MPI launch failures often manifest as hangs rather than immediate
    errors, especially around two-rank barriers or mismatched launcher
    environments.  A timeout turns those hangs into ordinary FAIL records
    with command logs instead of wedging make check-abi indefinitely.
    """
    return int(os.environ.get("OMPI_ABI_TEST_TIMEOUT",
                              str(DEFAULT_COMMAND_TIMEOUT)))


def _command_result(name, command, cwd, env, log_path):
    """Run one subprocess, capture output, and write a JSON command log."""
    log_path.parent.mkdir(parents=True, exist_ok=True)
    timeout = _command_timeout()
    try:
        completed = subprocess.run(
            command,
            cwd=str(cwd),
            env=env,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=False,
            timeout=timeout)
        return_code = completed.returncode
        stdout = completed.stdout
        stderr = completed.stderr
        timed_out = False
    except subprocess.TimeoutExpired as exc:
        return_code = 124
        stdout = exc.stdout or ""
        stderr = exc.stderr or ""
        if isinstance(stdout, bytes):
            stdout = stdout.decode("utf-8", errors="replace")
        if isinstance(stderr, bytes):
            stderr = stderr.decode("utf-8", errors="replace")
        stderr += "\ncommand timed out after {0} seconds".format(timeout)
        timed_out = True
    except OSError as exc:
        return_code = 127
        stdout = ""
        stderr = str(exc)
        timed_out = False

    log = {
        "name": name,
        "command": command,
        "cwd": str(cwd),
        "returncode": return_code,
        "timeout": timeout,
        "timed_out": timed_out,
        "stdout": stdout,
        "stderr": stderr,
    }
    _write_json(log_path, log)
    return {
        "command": command,
        "returncode": return_code,
        "timeout": timeout,
        "timed_out": timed_out,
        "stdout": stdout,
        "stderr": stderr,
        "log": str(log_path),
    }


def _installed_test_env(tools):
    """Build the environment for installed wrapper/compiler/runtime checks."""
    env = os.environ.copy()
    library_path = tools["paths"]["library"]
    loader_var = tools["paths"]["runtime_loader"]
    if library_path and loader_var:
        existing = env.get(loader_var)
        env[loader_var] = (
            library_path if not existing
            else library_path + os.pathsep + existing
        )
    return env


def _installed_test_dirs(outdir):
    """Create and return the installed-test scratch directory layout."""
    base = outdir / "installed"
    dirs = {
        "base": base,
        "src": base / "src",
        "bin": base / "bin",
        "logs": base / "logs",
    }
    for path in dirs.values():
        path.mkdir(parents=True, exist_ok=True)
    return dirs


def _launcher_args(tools):
    """Return optional extra mpirun arguments from the tool configuration."""
    args = tools["paths"]["launcher_args"]
    return shlex.split(args) if args else []


def _compile_overrides(tools):
    """Return optional include/library overrides for installed C probes."""
    args = []
    include_path = tools["paths"]["include"]
    library_path = tools["paths"]["library"]
    if include_path:
        args.append("-I" + include_path)
    if library_path:
        args.append("-L" + library_path)
    return args


def _linkage_command(executable):
    """Return the platform-specific command for linkage inspection.

    Linkage inspection is a diagnostic check, not a portability
    requirement.  Platforms without readelf/otool return None and are
    reported as SKIP by the caller instead of making the ABI suite fail
    just because the inspection tool is unavailable.
    """
    system = platform.system()
    if system == "Darwin":
        tool = shutil.which("otool")
        if tool:
            return [tool, "-L", str(executable)]
    if system == "Linux":
        tool = shutil.which("readelf")
        if tool:
            return [tool, "-d", str(executable)]
    return None


def _verify_executable_libmpi_abi(executable, dirs, env, name):
    """Verify an executable is actually linked against libmpi_abi.

    The wrapper flag checks prove mpicc_abi advertises ABI linkage, but
    the executable inspection catches cases where overrides, stale
    paths, or wrapper bugs still produce a binary not linked to
    libmpi_abi.  Missing inspection tools are SKIP; a runnable tool that
    shows the wrong linkage is FAIL.
    """
    command = _linkage_command(executable)
    if command is None:
        return {
            "result": "SKIP",
            "skip_reason": SKIP_LINKAGE_INSPECTION_UNAVAILABLE,
            "command": None,
            "returncode": 127,
            "log": None,
        }

    result = _command_result(
        "linkage_" + name,
        command,
        dirs["base"],
        env,
        dirs["logs"] / ("linkage_" + name + ".json"))
    output = result["stdout"] + result["stderr"]
    if result["returncode"] != 0:
        result["result"] = "FAIL"
        result["message"] = "executable linkage inspection failed"
    elif re.search(r"(?<![A-Za-z0-9_])libmpi_abi(?:[.][A-Za-z0-9_.-]+)?",
                   output) is None:
        result["result"] = "FAIL"
        result["message"] = "executable is not linked against libmpi_abi"
    else:
        result["result"] = "PASS"
        result["message"] = None
    return result


def _showme_words(mpicc_abi, option, dirs, env, name):
    """Run an mpicc_abi --showme option and shell-split its output."""
    result = _command_result(
        name,
        [mpicc_abi, option],
        dirs["base"],
        env,
        dirs["logs"] / (name + ".json"))
    if result["returncode"] != 0:
        return result, []
    return result, shlex.split(result["stdout"])


def _installed_standard_abi_header(tools, dirs, env):
    """Find the installed standard ABI mpi.h used by mpicc_abi.

    The MPI_H_ABI marker is required so an include path that happens to
    contain another mpi.h cannot satisfy the check.  This matters because
    installed probes intentionally compile outside the build tree.
    """
    include_override = tools["paths"]["include"]
    candidates = []
    if include_override:
        candidates.append(Path(include_override) / "mpi.h")

    mpicc_abi = tools["open_mpi"]["mpicc_abi"]
    _, incdirs = _showme_words(
        mpicc_abi, "--showme:incdirs", dirs, env, "mpicc_abi_showme_incdirs")
    for incdir in incdirs:
        candidates.append(Path(incdir) / "mpi.h")

    for candidate in candidates:
        if not candidate.exists():
            continue
        text = _read_text(candidate)
        if re.search(r"^\s*#define\s+MPI_H_ABI\b", text, re.MULTILINE):
            return candidate
    return None


def _remove_c_comments(text):
    return re.sub(r"/\*.*?\*/", " ", text, flags=re.DOTALL)


def _parse_c_header_prototypes(header):
    """Parse MPI and PMPI C prototypes from an installed mpi.h.

    This parser is intentionally simple because it targets the generated
    ABI header format, not arbitrary C.  The caller enforces a high
    minimum prototype count so a future formatting change cannot reduce
    this to an empty or tiny parse that still passes downstream checks.
    """
    text = _remove_c_comments(_read_text(header))
    prototypes = {}
    pattern = re.compile(
        r"(?m)^\s*(?!typedef\b)"
        r"(?:(?:extern|OMPI_DECLSPEC)\s+)*"
        r"(?P<return_type>[A-Za-z_][A-Za-z0-9_\s]*\s*\**)\s+"
        r"(?P<name>(?:P)?MPI_\w+)\s*"
        r"\((?P<args>[^;{}]*)\)\s*;")
    for match in pattern.finditer(text):
        return_type = _normalize_c_signature_text(
            " ".join(match.group("return_type").split()))
        args = _normalize_c_signature_text(
            " ".join(match.group("args").split()))
        name = match.group("name")
        signature = "{0} {1}({2})".format(return_type, name, args)
        prototypes[name] = {
            "name": name,
            "return_type": return_type,
            "args": args,
            "signature": _normalize_c_signature_text(signature),
            "prototype": match.group(0).strip(),
        }
    return prototypes


def _parse_c_header_deprecated_functions(srcdir):
    """Read the generator's authoritative deprecated C function list."""
    c_header = srcdir / "ompi" / "mpi" / "bindings" / "c_header.py"
    tree = ast.parse(_read_text(c_header), filename=str(c_header))
    for node in tree.body:
        if not isinstance(node, ast.Assign):
            continue
        if not any(isinstance(target, ast.Name)
                   and target.id == "DEPRECATED_FUNCTIONS"
                   for target in node.targets):
            continue
        value = ast.literal_eval(node.value)
        return set(value)
    raise RuntimeError("DEPRECATED_FUNCTIONS not found in {0}".format(
        c_header))


def _normalize_c_signature_text(text):
    """Normalize C signature spelling for stable comparisons."""
    text = text.strip().rstrip(";")
    text = text.replace("char argv[]", "char *argv[]")
    text = re.sub(r"\s+", " ", text)
    text = re.sub(r"\s*\*\s*", "* ", text)
    text = re.sub(r"\s+([,\)])", r"\1", text)
    text = re.sub(r"\(\s+", "(", text)
    return text.strip()


def _parse_c_signature(signature):
    """Parse one normalized MPI C signature into comparable fields."""
    signature = _normalize_c_signature_text(signature)
    match = re.match(
        r"^(?P<return_type>.+?)\s+(?P<name>(?:P)?MPI_\w+)\("
        r"(?P<args>.*)\)$",
        signature)
    if match is None:
        return None
    return {
        "name": match.group("name"),
        "return_type": _normalize_c_signature_text(
            match.group("return_type")),
        "args": _normalize_c_signature_text(match.group("args")),
        "signature": signature,
    }


def _standard_abi_expected_signatures(srcdir):
    """Build expected C/PMPI signatures from pympistandard metadata.

    Comparing against pympistandard avoids a circular test where the
    installed header is parsed and then used as its own authority.
    Deprecated C APIs and Fortran-only entry points are excluded by the
    same generator-side rules used to build the standard ABI header.
    """
    sys.path.insert(
        0, str((srcdir / "3rd-party" / "pympistandard" / "src").resolve()))
    import pympistandard as std

    std.use_api_version()
    deprecated = _parse_c_header_deprecated_functions(srcdir)
    expected = {}
    excluded = set()

    def add_signature(signature):
        signature = str(signature).replace(r"\ldots", "...")
        signature = signature.replace("@MPI_COUNT@", "MPI_Count")
        signature = signature.replace("@MPI_AINT@", "MPI_Aint")
        signature = signature.replace("@MPI_OFFSET@", "MPI_Offset")
        signature = signature.replace("char argv[]", "char *argv[]")
        parsed = _parse_c_signature(signature)
        if parsed is None:
            raise RuntimeError("could not parse expected C signature: {0}".
                               format(signature))
        name = parsed["name"]
        if any(function in signature for function in deprecated):
            excluded.add(name)
            return
        if "MPI_Fint" in signature or "MPI_F08_status" in signature:
            excluded.add(name)
            return
        expected[name] = parsed

    for proc in std.all_iso_c_procedures():
        add_signature(proc.express.iso_c)
        if proc.has_embiggenment():
            add_signature(proc.express.embiggen.iso_c)

    for proc in std.all_iso_c_procedures():
        add_signature(proc.express.profile.iso_c)
        if proc.has_embiggenment():
            binding = str(proc.express.embiggen.iso_c).split()
            add_signature("{0} P{1};".format(binding[0],
                                             " ".join(binding[1:])))

    return expected, excluded


def _probe_variable_name(name):
    return "probe_" + re.sub(r"[^A-Za-z0-9_]", "_", name)


def _header_probe_source(prototypes):
    """Generate a C source that forces references to parsed prototypes.

    Parsing a prototype is not enough; this source assigns every parsed
    MPI/PMPI symbol to a function pointer so the compiler and linker must
    accept the declarations and resolve the ABI entry points.
    """
    lines = [
        "#include \"mpi.h\"",
        "",
        "#ifndef MPI_H_ABI",
        "#error \"standard ABI mpi.h was not used\"",
        "#endif",
        "",
    ]
    for name in sorted(prototypes):
        proto = prototypes[name]
        variable = _probe_variable_name(name)
        lines.append("static {0} (*{1})({2}) = {3};".format(
            proto["return_type"], variable, proto["args"], name))

    lines.extend([
        "",
        "int main(void)",
        "{",
    ])
    for name in sorted(prototypes):
        variable = _probe_variable_name(name)
        lines.extend([
            "    if (0 == {0}) {{".format(variable),
            "        return 1;",
            "    }",
        ])
    lines.extend([
        "    return 0;",
        "}",
        "",
    ])
    return "\n".join(lines)


def _metadata_c_api_header_check(manifest, mpi_prototypes, excluded_names):
    """Ensure all implemented C ABI APIs are present in installed mpi.h."""
    expected = []
    missing = []
    non_abi_absent = []
    for entry in manifest["apis"]:
        if entry["classification"] != CLASS_IMPLEMENTED:
            continue
        if not entry["languages"]["c"]:
            continue
        name = entry["name"]
        if name in excluded_names:
            non_abi_absent.append(name)
            continue
        expected.append(name)
        if name not in mpi_prototypes:
            missing.append(name)

    if missing:
        return _fail(
            "installed_c_header_metadata_apis",
            "implemented C ABI metadata entries are missing from mpi.h",
            checked=len(expected),
            missing=missing[:20],
            missing_count=len(missing),
            non_abi_absent_count=len(non_abi_absent))

    return _pass(
        "installed_c_header_metadata_apis",
        checked=len(expected),
        non_abi_absent_count=len(non_abi_absent))


def _signature_comparison_check(prototypes, expected_signatures):
    """Compare installed header prototypes against generated signatures."""
    missing = []
    mismatches = []
    for name, expected in sorted(expected_signatures.items()):
        actual = prototypes.get(name)
        if actual is None:
            missing.append(name)
            continue
        if (actual["return_type"] != expected["return_type"]
                or actual["args"] != expected["args"]):
            mismatches.append({
                "name": name,
                "expected": expected["signature"],
                "actual": actual["signature"],
            })

    extra = sorted(
        name for name in prototypes
        if name not in expected_signatures and
        (name.startswith("MPI_") or name.startswith("PMPI_"))
    )

    if missing or mismatches or extra:
        return _fail(
            "installed_c_header_signature_semantics",
            "standard ABI header signatures differ from MPI C signatures",
            checked=len(expected_signatures),
            missing=missing[:20],
            missing_count=len(missing),
            mismatches=mismatches[:20],
            mismatch_count=len(mismatches),
            extra=extra[:20],
            extra_count=len(extra))

    return _pass("installed_c_header_signature_semantics",
                 checked=len(expected_signatures))


def _non_abi_absence_check(prototypes, excluded_names):
    """Ensure APIs excluded from the standard ABI are not exposed."""
    exposed = [
        name for name in sorted(prototypes)
        if (name.startswith("MPI_") or name.startswith("PMPI_"))
        and name in excluded_names
    ]
    if exposed:
        return _fail(
            "installed_c_header_non_abi_absence",
            "non-ABI C APIs are exposed by the standard ABI header",
            exposed=exposed[:20],
            exposed_count=len(exposed))
    return _pass("installed_c_header_non_abi_absence",
                 checked=len(excluded_names))


def _prototype_pair_check(prototypes, excluded_names):
    """Ensure each MPI prototype has its matching PMPI prototype."""
    mpi_names = sorted(
        name for name in prototypes
        if name.startswith("MPI_") and name not in excluded_names
    )
    pmpi_names = sorted(
        name for name in prototypes if name.startswith("PMPI_"))
    missing_pmpi = []
    for name in mpi_names:
        pmpi_name = "P" + name
        if pmpi_name not in prototypes:
            missing_pmpi.append(pmpi_name)

    if missing_pmpi:
        return _fail(
            "installed_c_header_api_prototypes",
            "standard ABI header is missing PMPI prototypes",
            mpi_count=len(mpi_names),
            pmpi_count=len(pmpi_names),
            missing_pmpi=missing_pmpi[:20],
            missing_pmpi_count=len(missing_pmpi))

    return _pass("installed_c_header_api_prototypes",
                 mpi_count=len(mpi_names),
                 pmpi_count=len(pmpi_names))


def _libmpi_abi_search_names():
    """Return likely libmpi_abi filenames for the current platform."""
    system = platform.system()
    if system == "Darwin":
        return ("libmpi_abi.dylib", "libmpi_abi.0.dylib")
    if system == "Linux":
        return ("libmpi_abi.so", "libmpi_abi.so.0")
    return ("libmpi_abi.so", "libmpi_abi.dylib", "libmpi_abi.a")


def _installed_libmpi_abi_path(tools, dirs, env):
    """Locate installed libmpi_abi using overrides and wrapper metadata."""
    library_override = tools["paths"]["library"]
    libdirs = []
    if library_override:
        libdirs.append(library_override)

    mpicc_abi = tools["open_mpi"]["mpicc_abi"]
    _, showme_libdirs = _showme_words(
        mpicc_abi, "--showme:libdirs", dirs, env, "mpicc_abi_showme_libdirs")
    libdirs.extend(showme_libdirs)

    for libdir in libdirs:
        for name in _libmpi_abi_search_names():
            candidate = Path(libdir) / name
            if candidate.exists():
                return candidate
    return None


def _defined_nm_symbols(output):
    """Extract defined symbol names from nm output.

    Undefined symbols, including weak undefined symbols, are deliberately
    excluded.  Counting a referenced-but-not-defined MPI symbol as an
    export would make the libmpi_abi symbol check pass for a missing ABI
    entry point.
    """
    symbols = set()
    for line in output.splitlines():
        parts = line.split()
        if len(parts) < 2:
            continue
        symbol = parts[-1]
        symbol_type = parts[-2] if len(parts) >= 3 else parts[0]
        if symbol_type in ("U", "w", "v"):
            continue
        if symbol.startswith("_"):
            symbol = symbol[1:]
        symbols.add(symbol)
    return symbols


def _symbol_table_check(prototypes, excluded_names, tools, dirs, env):
    """Optionally verify libmpi_abi exports all expected ABI symbols."""
    nm = shutil.which("nm")
    library = _installed_libmpi_abi_path(tools, dirs, env)
    if nm is None or library is None:
        return _skip(
            "installed_libmpi_abi_symbols",
            SKIP_SYMBOL_DIAGNOSTICS_UNAVAILABLE,
            nm=nm,
            library=str(library) if library else None)

    result = _command_result(
        "nm_libmpi_abi",
        [nm, "-g", str(library)],
        dirs["base"],
        env,
        dirs["logs"] / "nm_libmpi_abi.json")
    if result["returncode"] != 0:
        return _fail(
            "installed_libmpi_abi_symbols",
            "nm failed while inspecting libmpi_abi",
            command=result["command"],
            returncode=result["returncode"],
            log=result["log"],
            library=str(library))

    output = result["stdout"] + result["stderr"]
    defined_symbols = _defined_nm_symbols(output)
    expected = [
        name for name in sorted(prototypes)
        if (name.startswith("MPI_") or name.startswith("PMPI_"))
        and name not in excluded_names
    ]
    missing = [
        name for name in expected
        if name not in defined_symbols
    ]
    if missing:
        return _fail(
            "installed_libmpi_abi_symbols",
            "libmpi_abi is missing defined standard ABI symbols",
            library=str(library),
            checked=len(expected),
            missing=missing[:20],
            missing_count=len(missing),
            log=result["log"])

    return _pass(
        "installed_libmpi_abi_symbols",
        library=str(library),
        checked=len(expected),
        log=result["log"])


def _installed_c_header_symbol_checks(manifest, tools, dirs, progress=None):
    """Run installed header, signature, compile/link, and symbol checks.

    This combines several independent views of the installed C ABI:
    metadata coverage, PMPI pairing, semantic signatures, absence of
    non-ABI APIs, compile/link reachability, and optional library symbol
    diagnostics.  Keeping them separate makes failures actionable.
    """
    checks = []
    env = _installed_test_env(tools)
    if progress is not None:
        progress.start("installed_c_header_api_prototypes")
    header = _installed_standard_abi_header(tools, dirs, env)
    if header is None:
        check = _skip("installed_c_header_api_prototypes",
                      SKIP_HEADER_UNAVAILABLE)
        if progress is not None:
            progress.check(check)
        return [check]

    prototypes = _parse_c_header_prototypes(header)
    if len(prototypes) < MIN_EXPECTED_C_HEADER_PROTOTYPES:
        check = _fail(
            "installed_c_header_api_prototypes",
            "standard ABI header prototype parser found too few entries",
            header=str(header),
            prototype_count=len(prototypes),
            minimum_expected=MIN_EXPECTED_C_HEADER_PROTOTYPES)
        if progress is not None:
            progress.check(check)
        return [check]

    expected_signatures, excluded_names = _standard_abi_expected_signatures(
        Path(manifest["metadata"]["api_path"]).parents[1])
    mpi_prototypes = {
        name: proto for name, proto in prototypes.items()
        if name.startswith("MPI_")
    }
    _append_check(
        checks, _prototype_pair_check(prototypes, excluded_names), progress)

    if progress is not None:
        progress.start("installed_c_header_metadata_apis")
    _append_check(checks, _metadata_c_api_header_check(
        manifest, mpi_prototypes, excluded_names), progress)

    if progress is not None:
        progress.start("installed_c_header_signature_semantics")
    _append_check(checks, _signature_comparison_check(
        prototypes, expected_signatures), progress)

    if progress is not None:
        progress.start("installed_c_header_non_abi_absence")
    _append_check(
        checks, _non_abi_absence_check(prototypes, excluded_names), progress)

    source = dirs["src"] / "c_header_api_prototypes.c"
    executable = dirs["bin"] / "c_header_api_prototypes"
    _write_text(source, _header_probe_source(prototypes))

    compile_command = (
        [tools["open_mpi"]["mpicc_abi"]] + _compile_overrides(tools) +
        [str(source), "-o", str(executable)]
    )
    if progress is not None:
        progress.start("installed_c_header_compile_link")
    compile_result = _command_result(
        "compile_c_header_api_prototypes",
        compile_command,
        dirs["base"],
        env,
        dirs["logs"] / "compile_c_header_api_prototypes.json")
    if compile_result["returncode"] != 0:
        _append_check(checks, _fail(
            "installed_c_header_compile_link",
            "standard ABI header prototype compile/link probe failed",
            source=str(source),
            executable=str(executable),
            command=compile_result["command"],
            returncode=compile_result["returncode"],
            log=compile_result["log"],
            prototype_count=len(prototypes)), progress)
        return checks

    linkage_result = _verify_executable_libmpi_abi(
        executable, dirs, env, "c_header_api_prototypes")
    if linkage_result["result"] == "SKIP":
        _append_check(checks, _skip(
            "installed_c_header_compile_link",
            linkage_result["skip_reason"],
            source=str(source),
            executable=str(executable),
            compile_log=compile_result["log"],
            linkage_log=linkage_result["log"],
            prototype_count=len(prototypes)), progress)
    elif linkage_result["result"] != "PASS":
        _append_check(checks, _fail(
            "installed_c_header_compile_link",
            linkage_result["message"],
            source=str(source),
            executable=str(executable),
            command=linkage_result["command"],
            returncode=linkage_result["returncode"],
            compile_log=compile_result["log"],
            linkage_log=linkage_result["log"],
            prototype_count=len(prototypes)), progress)
    else:
        _append_check(checks, _pass(
            "installed_c_header_compile_link",
            source=str(source),
            executable=str(executable),
            header=str(header),
            prototype_count=len(prototypes),
            compile_log=compile_result["log"],
            linkage_log=linkage_result["log"]), progress)

    if progress is not None:
        progress.start("installed_libmpi_abi_symbols")
    _append_check(checks, _symbol_table_check(prototypes, excluded_names,
                                              tools, dirs, env), progress)
    return checks


def _installed_wrapper_checks(tools, dirs, progress=None):
    """Check mpicc_abi wrapper flags for ABI include and library linkage.

    These are coarse wrapper sanity checks.  They are intentionally not
    the only proof of correctness; later installed header and executable
    linkage checks verify that the advertised paths actually work.
    """
    checks = []
    mpicc_abi = tools["open_mpi"]["mpicc_abi"]
    env = _installed_test_env(tools)
    compile_log = dirs["logs"] / "mpicc_abi_showme_compile.json"
    link_log = dirs["logs"] / "mpicc_abi_showme_link.json"
    if progress is not None:
        progress.start("installed_standard_abi_wrapper_flags")
    compile_result = _command_result(
        "mpicc_abi_showme_compile",
        [mpicc_abi, "--showme:compile"],
        dirs["base"],
        env,
        compile_log)

    if compile_result["returncode"] != 0:
        _append_check(checks, _fail(
            "installed_standard_abi_wrapper_flags",
            "mpicc_abi --showme:compile failed",
            command=compile_result["command"],
            returncode=compile_result["returncode"],
            log=compile_result["log"]), progress)
    elif "standard_abi" not in compile_result["stdout"]:
        _append_check(checks, _fail(
            "installed_standard_abi_wrapper_flags",
            "mpicc_abi does not advertise standard ABI include path",
            command=compile_result["command"],
            stdout=compile_result["stdout"],
            log=compile_result["log"]), progress)
    else:
        _append_check(checks, _pass(
            "installed_standard_abi_wrapper_flags",
            command=compile_result["command"],
            stdout=compile_result["stdout"].strip(),
            log=compile_result["log"]), progress)

    if progress is not None:
        progress.start("installed_libmpi_abi_wrapper_flags")
    link_result = _command_result(
        "mpicc_abi_showme_link",
        [mpicc_abi, "--showme:link"],
        dirs["base"],
        env,
        link_log)
    if link_result["returncode"] != 0:
        _append_check(checks, _fail(
            "installed_libmpi_abi_wrapper_flags",
            "mpicc_abi --showme:link failed",
            command=link_result["command"],
            returncode=link_result["returncode"],
            log=link_result["log"]), progress)
    elif re.search(r"(?<![A-Za-z0-9_])-lmpi_abi(?![A-Za-z0-9_])",
                   link_result["stdout"]) is None:
        _append_check(checks, _fail(
            "installed_libmpi_abi_wrapper_flags",
            "mpicc_abi does not advertise libmpi_abi linkage",
            command=link_result["command"],
            stdout=link_result["stdout"],
            log=link_result["log"]), progress)
    else:
        _append_check(checks, _pass(
            "installed_libmpi_abi_wrapper_flags",
            command=link_result["command"],
            stdout=link_result["stdout"].strip(),
            log=link_result["log"]), progress)

    return checks


def _c_probe_source(srcdir, case, body, rank_count):
    """Render one installed C probe source from the shared template."""
    template = _read_text(srcdir / "test" / "mpi-abi" /
                          "templates" / "c_probe.c.in")
    body = body.replace("@EXPECTED_RANKS@", str(rank_count))
    prologue = _probe_prologue_text(srcdir, case).rstrip()
    # Keep checked-in *.cbody.in snippets at natural column-zero C
    # indentation.  The snippets are always inserted inside main(), so
    # the generated source owns the function-body indentation instead of
    # baking an extraction artifact into every snippet file.
    body = "\n".join(
        "    " + line if line else line
        for line in body.rstrip().splitlines())
    return (
        template
        .replace("@PROLOGUE@", prologue)
        .replace("@BODY@", body.rstrip())
    )


def _fortran_probe_source(srcdir, case):
    """Render one compile-only Fortran probe from the shared template."""
    template = _read_text(srcdir / "test" / "mpi-abi" /
                          "templates" / "fortran_probe.f90.in")
    body = case["body"].strip()
    return (
        template
        .replace("@USE_STATEMENT@", case["use_statement"])
        .replace("@BODY@", body)
    )


def _fortran_binding_skip(manifest, tools, language):
    """Return a stable skip reason for unavailable Fortran checks."""
    state = manifest["configuration"]["fortran"][language]
    if state["enabled"] is False:
        return SKIP_FORTRAN_BINDING_DISABLED
    if not _tool_available(tools["open_mpi"].get("mpifort")):
        return SKIP_FORTRAN_WRAPPER_UNAVAILABLE
    return None


def _fortran_check_api_names(checks):
    """Return API names covered by successful Fortran probe results.

    Static case metadata is useful for generating probes, but runtime
    coverage must come from checks that actually PASS.  This keeps a
    probe that compiles but exits through a stable SKIP from being
    reported as runtime coverage.
    """
    covered = {language: set() for language in FORTRAN_BINDING_LANGUAGES}
    for check in checks:
        language = check.get("language")
        if check.get("result") == "PASS" and language in covered:
            covered[language].update(check.get("api_names", ()))
    return covered


def _fortran_coverage_audit(manifest, tools, compile_checks, runtime_checks):
    """Report configured Fortran coverage populations for Phase 11.

    Phase 11 grows in layers.  The audit is intentionally grouped by
    binding because mpif.h/use mpi are regression coverage, while
    use mpi_f08 is the standard Fortran ABI-relevant layer.  Until the
    exhaustive generated Fortran probes exist, the audit reports pending
    APIs instead of treating them as hidden PASSes.
    """
    compile_covered = _fortran_check_api_names(compile_checks)
    runtime_covered = _fortran_check_api_names(runtime_checks)
    by_language = {}
    for language in FORTRAN_BINDING_LANGUAGES:
        state = manifest["configuration"]["fortran"][language]
        expressible = [
            entry for entry in manifest["apis"]
            if entry["languages"].get(language)
        ]
        implemented = [
            entry for entry in expressible
            if entry["classification"] == CLASS_IMPLEMENTED
        ]
        covered_names = compile_covered[language] | runtime_covered[language]
        covered_implemented = sorted(
            entry["name"] for entry in implemented
            if entry["name"] in covered_names
        )
        pending = sorted(
            entry["name"] for entry in implemented
            if entry["name"] not in covered_names
        )
        by_language[language] = {
            "configured": state,
            "skip_reason": _fortran_binding_skip(manifest, tools, language),
            "expressible_count": len(expressible),
            "implemented_count": len(implemented),
            "compile_probe_api_names": sorted(compile_covered[language]),
            "runtime_probe_api_names": sorted(runtime_covered[language]),
            "covered_implemented_count": len(covered_implemented),
            "pending_phase11b_count": len(pending),
            "pending_phase11b": pending[:20],
            "coverage_kind": (
                "standard_abi" if language == "use mpi_f08"
                else "regression"
            ),
        }

    return _pass(
        "installed_fortran_coverage_audit",
        enforcement="advisory_until_exhaustive_fortran_generation",
        languages=by_language,
        mpifort=tools["open_mpi"].get("mpifort"))


def _run_installed_fortran_compile_probes(srcdir, manifest, tools, dirs,
                                          progress=None):
    """Compile one installed Fortran probe per configured binding layer."""
    checks = []
    mpifort = tools["open_mpi"].get("mpifort")
    env = _installed_test_env(tools)
    compile_overrides = _compile_overrides(tools)

    for case in INSTALLED_FORTRAN_COMPILE_PROBES:
        name = case["name"]
        language = case["language"]
        check_name = "installed_" + name
        skip_reason = _fortran_binding_skip(manifest, tools, language)
        if skip_reason is not None:
            if progress is not None:
                progress.start(check_name)
            _append_check(checks, _skip(
                check_name,
                skip_reason,
                phase="configure",
                language=language,
                configured=manifest["configuration"]["fortran"][language],
                mpifort=mpifort), progress)
            continue

        source = dirs["src"] / (name + ".f90")
        executable = dirs["bin"] / name
        _write_text(source, _fortran_probe_source(srcdir, case))
        compile_command = (
            [mpifort] + compile_overrides +
            [str(source), "-o", str(executable)]
        )
        if progress is not None:
            progress.start(check_name)
        compile_result = _command_result(
            "compile_" + name,
            compile_command,
            dirs["base"],
            env,
            dirs["logs"] / ("compile_" + name + ".json"))
        if compile_result["returncode"] != 0:
            _append_check(checks, _fail(
                check_name,
                "installed Fortran binding compile probe failed",
                phase="compile",
                language=language,
                configured=manifest["configuration"]["fortran"][language],
                source=str(source),
                executable=str(executable),
                command=compile_result["command"],
                returncode=compile_result["returncode"],
                log=compile_result["log"]), progress)
            continue

        _append_check(checks, _pass(
            check_name,
            language=language,
            configured=manifest["configuration"]["fortran"][language],
            source=str(source),
            executable=str(executable),
            api_names=list(case.get("api_names", ())),
            compile_command=compile_result["command"],
            compile_log=compile_result["log"]), progress)

    return checks


def _run_installed_fortran_runtime_probes(srcdir, manifest, tools, dirs,
                                          progress=None):
    """Compile and launch one installed Fortran runtime probe per case."""
    checks = []
    mpifort = tools["open_mpi"].get("mpifort")
    mpirun = tools["open_mpi"]["mpirun"]
    env = _installed_test_env(tools)
    launcher_args = _launcher_args(tools)
    compile_overrides = _compile_overrides(tools)

    for case in INSTALLED_FORTRAN_RUNTIME_PROBES:
        name = case["name"]
        language = case["language"]
        check_name = "installed_" + name
        skip_reason = _fortran_binding_skip(manifest, tools, language)
        if skip_reason is not None:
            if progress is not None:
                progress.start(check_name)
            _append_check(checks, _skip(
                check_name,
                skip_reason,
                phase="configure",
                language=language,
                configured=manifest["configuration"]["fortran"][language],
                mpifort=mpifort), progress)
            continue

        rank_count = tools["rank_counts"]["np{0}".format(
            case["rank_count"])]
        source = dirs["src"] / (name + ".f90")
        executable = dirs["bin"] / name
        _write_text(source, _fortran_probe_source(srcdir, case))
        compile_command = (
            [mpifort] + compile_overrides +
            [str(source), "-o", str(executable)]
        )
        if progress is not None:
            progress.start(check_name)
        compile_result = _command_result(
            "compile_" + name,
            compile_command,
            dirs["base"],
            env,
            dirs["logs"] / ("compile_" + name + ".json"))
        if compile_result["returncode"] != 0:
            _append_check(checks, _fail(
                check_name,
                "installed Fortran runtime probe compile failed",
                phase="compile",
                language=language,
                configured=manifest["configuration"]["fortran"][language],
                source=str(source),
                executable=str(executable),
                command=compile_result["command"],
                returncode=compile_result["returncode"],
                log=compile_result["log"]), progress)
            continue

        run_command = (
            [mpirun] + launcher_args +
            ["--np", str(rank_count), str(executable)]
        )
        run_result = _command_result(
            "run_" + name,
            run_command,
            dirs["base"],
            env,
            dirs["logs"] / ("run_" + name + ".json"))
        if run_result["returncode"] != 0:
            skip_reason = case.get("skip_exit_codes", {}).get(
                run_result["returncode"])
            if skip_reason is not None:
                _append_check(checks, _skip(
                    check_name,
                    skip_reason,
                    phase="run",
                    language=language,
                    configured=manifest["configuration"]["fortran"][
                        language],
                    source=str(source),
                    executable=str(executable),
                    rank_count=rank_count,
                    command=run_result["command"],
                    returncode=run_result["returncode"],
                    compile_log=compile_result["log"],
                    run_log=run_result["log"]), progress)
                continue

            _append_check(checks, _fail(
                check_name,
                "installed Fortran runtime probe failed",
                phase="run",
                language=language,
                configured=manifest["configuration"]["fortran"][language],
                source=str(source),
                executable=str(executable),
                rank_count=rank_count,
                command=run_result["command"],
                returncode=run_result["returncode"],
                compile_log=compile_result["log"],
                run_log=run_result["log"]), progress)
            continue

        _append_check(checks, _pass(
            check_name,
            language=language,
            configured=manifest["configuration"]["fortran"][language],
            source=str(source),
            executable=str(executable),
            rank_count=rank_count,
            api_names=list(case.get("api_names", ())),
            compile_command=compile_result["command"],
            run_command=run_result["command"],
            compile_log=compile_result["log"],
            run_log=run_result["log"]), progress)

    return checks


def _fortran_optional_datatype_skip(manifest, tools, progress=None):
    """Record the still-deferred optional Fortran datatype work item.

    The C ABI converter tests already guard optional Fortran datatype
    declarations against the installed standard ABI header.  Exhaustive
    Fortran binding probes need generated source driven by the installed
    module declarations, which is intentionally left as a distinct open
    task so unavailable optional types become precise SKIPs instead of
    compile failures.
    """
    check_name = "installed_fortran_optional_datatype_generation"
    if progress is not None:
        progress.start(check_name)
    return _skip(
        check_name,
        SKIP_FORTRAN_OPTIONAL_DATATYPES_DEFERRED,
        phase="generation",
        mpifort=tools["open_mpi"].get("mpifort"),
        fortran=manifest["configuration"]["fortran"])


def _installed_fortran_checks(srcdir, manifest, tools, dirs, progress=None):
    """Run installed Fortran detection, compile, and runtime checks."""
    checks = []
    compile_checks = _run_installed_fortran_compile_probes(
        srcdir, manifest, tools, dirs, progress)
    checks.extend(compile_checks)
    runtime_checks = _run_installed_fortran_runtime_probes(
        srcdir, manifest, tools, dirs, progress)
    checks.extend(runtime_checks)
    if progress is not None:
        progress.start("installed_fortran_coverage_audit")
    _append_check(checks, _fortran_coverage_audit(
        manifest, tools, compile_checks, runtime_checks), progress)
    _append_check(checks, _fortran_optional_datatype_skip(
        manifest, tools, progress), progress)
    return checks


def _run_installed_c_probe_cases(srcdir, manifest, tools, dirs, header_names,
                                 cases, progress=None):
    """Compile, link-inspect, and launch installed C probe cases.

    Keep this loop shared by the Phase 1-8 ABI/converter probes and the
    Phase 9 runtime API probes.  The caller decides which generation
    preflights must pass before a case group runs, which prevents a
    Phase 9 table issue from suppressing older ABI coverage.
    """
    checks = []
    mpicc_abi = tools["open_mpi"]["mpicc_abi"]
    mpirun = tools["open_mpi"]["mpirun"]
    env = _installed_test_env(tools)
    launcher_args = _launcher_args(tools)
    compile_overrides = _compile_overrides(tools)

    for case in cases:
        name = case["name"]
        check_name = "installed_c_probe_" + name
        if (case.get("requires_fortran")
                and not _fortran_bindings_enabled(manifest)):
            if progress is not None:
                progress.start(check_name)
            _append_check(checks, _skip(
                check_name,
                SKIP_FORTRAN_BINDINGS_DISABLED,
                phase="configure"), progress)
            continue
        feature = case.get("requires_feature")
        if feature is not None:
            feature_info = _optional_feature_info(manifest, feature)
            if feature_info["enabled"] is False:
                if progress is not None:
                    progress.start(check_name)
                _append_check(checks, _skip(
                    check_name,
                    _optional_feature_skip_reason(feature),
                    phase="configure",
                    feature=feature,
                    feature_state=feature_info), progress)
                continue
        rank_count = tools["rank_counts"]["np{0}".format(
            case["rank_count"])]
        source = dirs["src"] / (name + ".c")
        executable = dirs["bin"] / name
        try:
            body = _prepare_installed_c_probe_body(
                srcdir, case, manifest, header_names)
        except RuntimeError as exc:
            if progress is not None:
                progress.start(check_name)
            _append_check(checks, _fail(
                check_name,
                "installed C ABI probe body is unavailable",
                phase="source",
                error=str(exc)), progress)
            continue
        try:
            probe_source = _c_probe_source(srcdir, case, body, rank_count)
        except RuntimeError as exc:
            if progress is not None:
                progress.start(check_name)
            _append_check(checks, _fail(
                check_name,
                "installed C ABI probe prologue is unavailable",
                phase="source",
                error=str(exc)), progress)
            continue
        _write_text(source, probe_source)

        compile_command = (
            [mpicc_abi] + compile_overrides +
            [str(source), "-o", str(executable)]
        )
        if progress is not None:
            progress.start(check_name)
        compile_result = _command_result(
            "compile_" + name,
            compile_command,
            dirs["base"],
            env,
            dirs["logs"] / ("compile_" + name + ".json"))
        if compile_result["returncode"] != 0:
            _append_check(checks, _fail(
                check_name,
                "installed C ABI probe compile failed",
                phase="compile",
                source=str(source),
                executable=str(executable),
                command=compile_result["command"],
                returncode=compile_result["returncode"],
                log=compile_result["log"]), progress)
            continue

        linkage_result = _verify_executable_libmpi_abi(
            executable, dirs, env, name)
        if linkage_result["result"] == "SKIP":
            _append_check(checks, _skip(
                check_name,
                linkage_result["skip_reason"],
                phase="linkage",
                source=str(source),
                executable=str(executable),
                command=linkage_result["command"],
                returncode=linkage_result["returncode"],
                compile_log=compile_result["log"],
                linkage_log=linkage_result["log"]), progress)
            continue

        if linkage_result["result"] != "PASS":
            _append_check(checks, _fail(
                check_name,
                linkage_result["message"],
                phase="linkage",
                source=str(source),
                executable=str(executable),
                command=linkage_result["command"],
                returncode=linkage_result["returncode"],
                compile_log=compile_result["log"],
                linkage_log=linkage_result["log"]), progress)
            continue

        run_command = (
            [mpirun] + launcher_args +
            ["--np", str(rank_count), str(executable)]
        )
        run_result = _command_result(
            "run_" + name,
            run_command,
            dirs["base"],
            env,
            dirs["logs"] / ("run_" + name + ".json"))
        skip_reason = case.get("skip_exit_codes", {}).get(
            run_result["returncode"])
        if skip_reason is not None:
            _append_check(checks, _skip(
                check_name,
                skip_reason,
                phase="run",
                source=str(source),
                executable=str(executable),
                rank_count=rank_count,
                command=run_result["command"],
                returncode=run_result["returncode"],
                compile_log=compile_result["log"],
                run_log=run_result["log"]), progress)
            continue
        if run_result["returncode"] != 0:
            _append_check(checks, _fail(
                check_name,
                "installed C ABI probe runtime failed",
                phase="run",
                source=str(source),
                executable=str(executable),
                rank_count=rank_count,
                command=run_result["command"],
                returncode=run_result["returncode"],
                compile_log=compile_result["log"],
                run_log=run_result["log"]), progress)
            continue

        _append_check(checks, _pass(
            check_name,
            source=str(source),
            executable=str(executable),
            rank_count=rank_count,
            family=case.get("family"),
            api_names=list(case.get("api_names", ())),
            support_api_names=list(case.get("support_api_names", ())),
            compile_command=compile_result["command"],
            run_command=run_result["command"],
            compile_log=compile_result["log"],
            linkage_command=linkage_result["command"],
            linkage_log=linkage_result["log"],
            run_log=run_result["log"]), progress)

    return checks


def _installed_c_probe_checks(srcdir, manifest, tools, dirs, progress=None):
    """Compile, link-inspect, and run installed C ABI runtime probes.

    Before compiling generated probes, this function verifies that probe
    families are nonempty and that the installed ABI header declares the
    metadata constants the probes will reference.  That makes metadata
    drift a clear preflight failure instead of a smaller generated C
    source that silently preserves PASS.
    """
    checks = []
    env = _installed_test_env(tools)
    header = _installed_standard_abi_header(tools, dirs, env)
    include_fortran = _fortran_bindings_enabled(manifest)
    if header is None:
        if progress is not None:
            progress.start("installed_c_probe_metadata_constants")
        _append_check(checks, _skip(
            "installed_c_probe_metadata_constants",
            SKIP_HEADER_UNAVAILABLE), progress)
        return checks

    declared_names = (
        _parse_header_constant_names(header)
    )
    if progress is not None:
        progress.start("installed_c_probe_generation")
    generation_check = _runtime_probe_generation_check(
        manifest, include_fortran, declared_names)
    _append_check(checks, generation_check, progress)
    if generation_check["result"] != "PASS":
        return checks

    expected_names = _runtime_probe_constant_names(
        manifest, include_fortran, declared_names)
    missing_names = sorted(expected_names - declared_names)
    if progress is not None:
        progress.start("installed_c_probe_metadata_constants")
    if missing_names:
        _append_check(checks, _fail(
            "installed_c_probe_metadata_constants",
            "installed standard ABI header is missing probe constants",
            header=str(header),
            missing=missing_names[:20],
            missing_count=len(missing_names),
            expected_count=len(expected_names),
            declared_count=len(declared_names)), progress)
        return checks
    else:
        _append_check(checks, _pass(
            "installed_c_probe_metadata_constants",
            header=str(header),
            checked=len(expected_names),
            declared_count=len(declared_names)), progress)

    checks.extend(_run_installed_c_probe_cases(
        srcdir, manifest, tools, dirs, declared_names,
        INSTALLED_C_ABI_PROBES, progress))

    if progress is not None:
        progress.start("installed_c_runtime_api_probe_generation")
    runtime_api_generation_check = _runtime_api_probe_generation_check(
        srcdir, manifest, header, INSTALLED_C_RUNTIME_API_PROBES)
    _append_check(checks, runtime_api_generation_check, progress)
    if runtime_api_generation_check["result"] != "PASS":
        return checks

    if progress is not None:
        progress.start("installed_c_runtime_api_coverage_audit")
    _append_check(checks, _runtime_api_coverage_audit(
        manifest, header, INSTALLED_C_RUNTIME_API_PROBES), progress)

    checks.extend(_run_installed_c_probe_cases(
        srcdir, manifest, tools, dirs, declared_names,
        INSTALLED_C_RUNTIME_API_PROBES, progress))

    if progress is not None:
        progress.start("installed_c_callback_api_probe_generation")
    callback_api_generation_check = _runtime_api_probe_generation_check(
        srcdir, manifest, header, INSTALLED_C_CALLBACK_PROBES,
        "installed_c_callback_api_probe_generation")
    _append_check(checks, callback_api_generation_check, progress)
    if callback_api_generation_check["result"] != "PASS":
        return checks

    if progress is not None:
        progress.start("installed_c_callback_api_coverage_audit")
    _append_check(checks, _callback_api_coverage_audit(
        manifest, header, INSTALLED_C_CALLBACK_PROBES), progress)

    checks.extend(_run_installed_c_probe_cases(
        srcdir, manifest, tools, dirs, declared_names,
        INSTALLED_C_CALLBACK_PROBES, progress))

    return checks


def run_installed_checks(manifest, mode, srcdir, outdir, tools,
                         progress=None):
    """Run checks that require an installed Open MPI standard ABI build.

    Installed checks intentionally live outside make check's normal
    check_PROGRAMS flow: they need an installed mpicc_abi/mpirun pair and
    create one short-lived executable per probe so MPI runtime failures
    do not contaminate later probes in the same process.
    """
    if mode != "check-abi":
        return []
    dirs = _installed_test_dirs(outdir)
    checks = []
    checks.extend(_installed_wrapper_checks(tools, dirs, progress))
    checks.extend(_installed_c_header_symbol_checks(
        manifest, tools, dirs, progress))
    checks.extend(_installed_c_probe_checks(
        srcdir, manifest, tools, dirs, progress))
    checks.extend(_installed_fortran_checks(
        srcdir, manifest, tools, dirs, progress))
    return checks


def _tool_info(mode):
    """Resolve tool paths, rank counts, and optional override paths."""
    tools = {
        "open_mpi": {
            "mpicc_abi": _which("OMPI_ABI_TEST_MPICC_ABI", "mpicc_abi"),
            "mpifort": _which("OMPI_ABI_TEST_MPIFORT", "mpifort"),
            "mpirun": _which("OMPI_ABI_TEST_MPIRUN", "mpirun"),
        },
        "mpich": {
            "mpicc": _which("MPICH_ABI_TEST_MPICC", "mpicc"),
            "mpirun": _which("MPICH_ABI_TEST_MPIRUN", "mpirun"),
        },
        "rank_counts": {
            "np1": int(os.environ.get("OMPI_ABI_TEST_NP1", "1")),
            "np2": int(os.environ.get("OMPI_ABI_TEST_NP2", "2")),
        },
        "paths": {
            "include": os.environ.get("OMPI_ABI_TEST_INCLUDE_PATH"),
            "library": os.environ.get("OMPI_ABI_TEST_LIBRARY_PATH"),
            "launcher_args": os.environ.get("OMPI_ABI_TEST_LAUNCHER_ARGS"),
            "runtime_loader": _runtime_loader_var(),
        },
    }
    if mode != "check-abi-cross":
        tools.pop("mpich")
    return tools


def _symbol_diagnostics():
    """Report optional symbol/linkage diagnostic tools for the host."""
    system = platform.system()
    tools = {
        "nm": shutil.which("nm"),
    }
    if system == "Darwin":
        tools["otool"] = shutil.which("otool")
    elif system == "Linux":
        tools["readelf"] = shutil.which("readelf")
    return {
        "platform": system,
        "tools": tools,
    }


def _runtime_loader_var():
    """Return the runtime library-path environment variable for the host."""
    system = platform.system()
    if system == "Darwin":
        return "DYLD_LIBRARY_PATH"
    if system == "Linux":
        return "LD_LIBRARY_PATH"
    return None


def build_report(manifest, mode, srcdir, builddir, outdir, progress=None):
    """Run the requested mode and assemble the machine-readable report.

    Mode-level SKIPs are decided before running subordinate checks so a
    build without standard ABI support remains a spec-sanctioned skip
    rather than failing completion gates or installed-tool discovery.
    """
    api_entries = manifest["apis"]
    constant_entries = manifest["constants"]
    standard_abi = manifest["configuration"]["standard_abi"]
    classifications = _count_by(api_entries, "classification")
    test_status = _count_by(api_entries, "test_status")
    constant_classifications = _count_by(constant_entries, "classification")
    constant_test_status = _count_by(constant_entries, "test_status")

    skip_reason = None
    result = "PASS"
    tools = _tool_info(mode)
    fast_checks = []
    installed_checks = []

    if mode in ("coverage", "check-fast", "check-abi", "check-abi-cross"):
        if standard_abi["enabled"] is False:
            result = "SKIP"
            skip_reason = SKIP_STANDARD_ABI_DISABLED

    if result != "SKIP" and mode in ("coverage", "check-fast"):
        fast_checks = run_fast_checks(manifest, srcdir, builddir, progress)
        if any(check["result"] == "FAIL" for check in fast_checks):
            result = "FAIL"

    if result != "SKIP" and mode == "check-abi":
        if not _open_mpi_tools_available(tools):
            result = "SKIP"
            skip_reason = SKIP_OPEN_MPI_TOOLS_UNAVAILABLE
        else:
            installed_checks = run_installed_checks(
                manifest, mode, srcdir, outdir, tools, progress)
            if any(check["result"] == "FAIL"
                   for check in installed_checks):
                result = "FAIL"

    if result != "SKIP" and mode == "check-abi-cross":
        if not _open_mpi_tools_available(tools):
            result = "SKIP"
            skip_reason = SKIP_OPEN_MPI_TOOLS_UNAVAILABLE
        elif not _mpich_tools_available(tools):
            result = "SKIP"
            skip_reason = SKIP_MPICH_TOOLS_UNAVAILABLE

    return {
        "mode": mode,
        "result": result,
        "skip_reason": skip_reason,
        "srcdir": str(srcdir),
        "builddir": str(builddir),
        "tmpdir": os.environ.get("OMPI_ABI_TEST_TMPDIR")
                  or tempfile.gettempdir(),
        "tools": tools,
        "symbol_diagnostics": _symbol_diagnostics(),
        "fast_checks": fast_checks,
        "installed_checks": installed_checks,
        "summary": {
            "apis_total": len(api_entries),
            "api_classifications": classifications,
            "api_test_status": test_status,
            "constants_total": len(constant_entries),
            "constant_classifications": constant_classifications,
            "constant_test_status": constant_test_status,
            "fast_check_status": _check_counts(fast_checks),
            "installed_check_status": _check_counts(installed_checks),
            "language_coverage": _language_counts(api_entries),
        },
    }


def _summary_text(report, colors=None):
    """Render a human-readable summary from a report object."""
    if colors is None:
        colors = _Colors(False)
    lines = []
    lines.append("MPI ABI test summary")
    lines.append("====================")
    lines.append("mode: {0}".format(report["mode"]))
    result_text = "result: {0}".format(report["result"])
    lines.append(colors.result(report["result"], result_text))
    if report["skip_reason"]:
        lines.append("skip_reason: {0}".format(report["skip_reason"]))
    lines.append("srcdir: {0}".format(report["srcdir"]))
    lines.append("builddir: {0}".format(report["builddir"]))
    lines.append("tmpdir: {0}".format(report["tmpdir"]))
    lines.append("")
    lines.append("API entries: {0}".format(report["summary"]["apis_total"]))
    for key, value in sorted(report["summary"]["api_classifications"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("API test status:")
    for key, value in sorted(report["summary"]["api_test_status"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("ABI constants: {0}".format(
        report["summary"]["constants_total"]))
    for key, value in sorted(
            report["summary"]["constant_classifications"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("ABI constant test status:")
    for key, value in sorted(
            report["summary"]["constant_test_status"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("Fast checks:")
    if report["fast_checks"]:
        for check in report["fast_checks"]:
            line = "  {0}: {1}".format(check["name"], check["result"])
            if check["skip_reason"]:
                line += " ({0})".format(check["skip_reason"])
            lines.append(colors.result(check["result"], line))
    else:
        lines.append("  none")
    lines.append("")
    lines.append("Installed checks:")
    if report["installed_checks"]:
        for check in report["installed_checks"]:
            line = "  {0}: {1}".format(check["name"], check["result"])
            if check["skip_reason"]:
                line += " ({0})".format(check["skip_reason"])
            lines.append(colors.result(check["result"], line))
    else:
        lines.append("  none")
    lines.append("")
    lines.append("Language coverage:")
    for key, value in sorted(report["summary"]["language_coverage"].items()):
        lines.append("  {0}: {1}".format(key, value))
    lines.append("")
    lines.append("Tools:")
    lines.append(json.dumps(report["tools"], indent=2, sort_keys=True))
    lines.append("")
    lines.append("Symbol diagnostics:")
    lines.append(json.dumps(
        report["symbol_diagnostics"], indent=2, sort_keys=True))
    lines.append("")
    return "\n".join(lines)


def write_outputs(manifest, report, outdir):
    """Write manifest, JSON report, and plain-text summary files."""
    outdir.mkdir(parents=True, exist_ok=True)
    manifest_path = outdir / "abi-manifest.json"
    report_path = outdir / "abi-report.json"
    summary_path = outdir / "abi-summary.txt"
    _write_json(manifest_path, manifest)
    _write_json(report_path, report)
    _write_text(summary_path, _summary_text(report))
    return manifest_path, report_path, summary_path


def command(args):
    """Execute the parsed CLI command and return a shell exit status.

    Reports are written for every mode, including failures, because the
    JSON command logs are the primary diagnostic artifact for CI and for
    reviewer investigations.  The completion gate is applied after the
    report is built and deliberately ignored for skipped runs.
    """
    srcdir = Path(args.srcdir).resolve()
    builddir = Path(args.builddir).resolve()
    outdir = (
        Path(args.outdir).resolve() if args.outdir else builddir / ".mpi-abi"
    )
    manifest = build_manifest(srcdir, builddir)
    mode = args.mode

    if mode == "manifest":
        outdir.mkdir(parents=True, exist_ok=True)
        path = outdir / "abi-manifest.json"
        _write_json(path, manifest)
        print("Wrote {0}".format(path))
        return 0

    progress_enabled = args.progress
    env_progress = _env_bool("OMPI_ABI_TEST_PROGRESS")
    if env_progress is not None:
        progress_enabled = env_progress
    colors = _Colors(_color_tests_enabled(args.color_tests))
    progress = _Progress(progress_enabled, colors)

    report = build_report(manifest, mode, srcdir, builddir, outdir, progress)
    _, report_path, summary_path = write_outputs(manifest, report, outdir)
    if progress.enabled:
        print("")
    print(_summary_text(report, colors), end="")
    print("Wrote {0}".format(report_path))
    print("Wrote {0}".format(summary_path))

    if args.complete_gate and report["result"] != "SKIP":
        api_not_written = report["summary"]["api_test_status"].get(
            TEST_NOT_WRITTEN, 0)
        constant_not_written = report["summary"][
            "constant_test_status"].get(TEST_NOT_WRITTEN, 0)
        not_written = api_not_written + constant_not_written
        if not_written:
            print(
                "ERROR: completion gate found {0} unwritten tests "
                "({1} APIs, {2} constants)".format(
                    not_written, api_not_written, constant_not_written),
                file=sys.stderr)
            return 1

    if report["result"] == "FAIL":
        return 1

    return 0


def _default_srcdir():
    """Return the top source tree for direct runner invocations.

    Automake passes --srcdir explicitly, but argparse still evaluates
    defaults before parsing the command line.  Python 3.7 keeps
    Path(__file__) relative when the script is invoked as
    ./mpi_abi_tests.py, so resolve it before indexing parents.
    """
    return Path(__file__).resolve().parents[2]


def main(argv):
    """Parse command-line arguments and run the MPI ABI test runner."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--srcdir", default=_default_srcdir(),
                        help="top source directory")
    parser.add_argument("--builddir", default=os.getcwd(),
                        help="top build directory")
    parser.add_argument("--outdir",
                        help="output directory for generated reports")
    parser.add_argument("--complete-gate", action="store_true",
                        help="fail when implemented ABI entries lack tests")
    parser.add_argument("--no-progress", dest="progress",
                        action="store_false",
                        help="do not print per-check progress lines")
    parser.set_defaults(progress=True)
    parser.add_argument("--color-tests", default="auto",
                        choices=("auto", "yes", "no", "always", "never"),
                        help="colorize console test results")
    parser.add_argument("mode", choices=(
        "manifest",
        "coverage",
        "check-fast",
        "check-abi",
        "check-abi-cross",
    ))
    args = parser.parse_args(argv)
    try:
        return command(args)
    except Exception as exc:
        print("ERROR: {0}".format(exc), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
