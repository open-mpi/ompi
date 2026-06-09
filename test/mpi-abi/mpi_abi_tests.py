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
SKIP_FORTRAN_HELPERS_SHARED = "fortran_abi_helpers_shared_with_mpifh"

EXPECTED_METADATA_VERSION = "5.0"
EXPECTED_API_COUNT = 567
EXPECTED_CONSTANT_COUNT = 373
DEFAULT_COMMAND_TIMEOUT = 120
MIN_EXPECTED_C_HEADER_PROTOTYPES = 1000

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
        "body": """
    int major = -1;
    int minor = -1;
    int ret = MPI_Abi_get_version(&major, &minor);
    if (MPI_SUCCESS != ret) {
        return 1;
    }
    if (MPI_ABI_VERSION != major) {
        return 2;
    }
    if (MPI_ABI_SUBVERSION != minor) {
        return 3;
    }
""",
    },
    {
        "name": "init_finalize",
        "rank_count": 1,
        "body": """
    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }
    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 2;
    }
""",
    },
    {
        "name": "barrier_two_rank",
        "rank_count": 2,
        "body": """
    int size = 0;
    int rank = -1;
    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }
    ret = MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (MPI_SUCCESS != ret) {
        return 2;
    }
    ret = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if (MPI_SUCCESS != ret) {
        return 3;
    }
    if (size != @EXPECTED_RANKS@ || rank < 0 || rank >= size) {
        return 4;
    }
    ret = MPI_Barrier(MPI_COMM_WORLD);
    if (MPI_SUCCESS != ret) {
        return 5;
    }
    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 6;
    }
""",
    },
    {
        "name": "sendrecv_two_rank",
        "rank_count": 2,
        "body": """
    int size = 0;
    int rank = -1;
    int value = -1;
    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }
    ret = MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (MPI_SUCCESS != ret) {
        return 2;
    }
    ret = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if (MPI_SUCCESS != ret) {
        return 3;
    }
    if (size != @EXPECTED_RANKS@ || rank < 0 || rank >= size) {
        return 4;
    }
    if (0 == rank) {
        value = 1234;
        ret = MPI_Send(&value, 1, MPI_INT, 1, 99, MPI_COMM_WORLD);
        if (MPI_SUCCESS != ret) {
            return 5;
        }
    } else if (1 == rank) {
        ret = MPI_Recv(&value, 1, MPI_INT, 0, 99,
                       MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        if (MPI_SUCCESS != ret) {
            return 6;
        }
        if (1234 != value) {
            return 7;
        }
    }
    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 8;
    }
""",
    },
    {
        "name": "converter_predefined_handles",
        "rank_count": 1,
        "body": """
#define ABI_CHECK_PREDEFINED_HANDLE(kind, value) do {                       \\
        ++predefined_ ## kind ## _checks;                                   \\
        int abi_value = MPI_ ## kind ## _toint(value);                      \\
        if (abi_value != (int) (intptr_t) (value)) {                        \\
            fprintf(stderr, "ABI_FAIL:predefined:%s:toint\\n", #value);     \\
            return 20;                                                      \\
        }                                                                   \\
        if (value != MPI_ ## kind ## _fromint(abi_value)) {                 \\
            fprintf(stderr, "ABI_FAIL:predefined:%s:fromint\\n", #value);   \\
            return 20;                                                      \\
        }                                                                   \\
        if (abi_value != PMPI_ ## kind ## _toint(value)) {                  \\
            fprintf(stderr, "ABI_FAIL:predefined:%s:ptoint\\n", #value);    \\
            return 20;                                                      \\
        }                                                                   \\
        if (value != PMPI_ ## kind ## _fromint(abi_value)) {                \\
            fprintf(stderr, "ABI_FAIL:predefined:%s:pfromint\\n", #value);  \\
            return 20;                                                      \\
        }                                                                   \\
    } while (0)

    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }

@PREDEFINED_HANDLE_DECLS@

@PREDEFINED_HANDLE_CHECKS@

@PREDEFINED_HANDLE_GUARDS@

    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 12;
    }
""",
    },
    {
        "name": "converter_fortran_datatypes",
        "rank_count": 1,
        "requires_fortran": True,
        "body": """
#define ABI_EXPECTED_FORTRAN_TYPE_CHECKS @FORTRAN_TYPE_EXPECTED@
#define ABI_CHECK_FORTRAN_TYPE(value) do {                                  \\
        ++fortran_type_checks;                                              \\
        int abi_value = MPI_Type_toint(value);                              \\
        if (abi_value != (int) (intptr_t) (value)) {                        \\
            fprintf(stderr, "ABI_FAIL:fortran_type:%s:toint\\n", #value);   \\
            return 20;                                                      \\
        }                                                                   \\
        if (value != MPI_Type_fromint(abi_value)) {                         \\
            fprintf(stderr, "ABI_FAIL:fortran_type:%s:fromint\\n", #value); \\
            return 20;                                                      \\
        }                                                                   \\
        if (abi_value != PMPI_Type_toint(value)) {                          \\
            fprintf(stderr, "ABI_FAIL:fortran_type:%s:ptoint\\n", #value);  \\
            return 20;                                                      \\
        }                                                                   \\
        if (value != PMPI_Type_fromint(abi_value)) {                        \\
            fprintf(stderr, "ABI_FAIL:fortran_type:%s:pfromint\\n", #value);\\
            return 20;                                                      \\
        }                                                                   \\
    } while (0)

    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }

    int fortran_type_checks = 0;

@FORTRAN_TYPE_CHECKS@

    if (ABI_EXPECTED_FORTRAN_TYPE_CHECKS != fortran_type_checks) {
        fprintf(stderr, "ABI_FAIL:fortran_type:count:%d:%d\\n",
                ABI_EXPECTED_FORTRAN_TYPE_CHECKS, fortran_type_checks);
        ret = MPI_Finalize();
        if (MPI_SUCCESS != ret) {
            return 21;
        }
        return 22;
    }

    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 23;
    }
""",
    },
    {
        "name": "converter_dynamic_handles",
        "rank_count": 1,
        "body": """
#define ABI_CHECK_DYNAMIC_HANDLE(kind, type, value, code) do {              \\
        int abi_value = MPI_ ## kind ## _toint(value);                      \\
        type roundtrip = MPI_ ## kind ## _fromint(abi_value);               \\
        if (roundtrip != (value)) {                                         \\
            return (code);                                                  \\
        }                                                                   \\
        if (abi_value != PMPI_ ## kind ## _toint(value)) {                  \\
            return (code) + 1;                                              \\
        }                                                                   \\
        if (value != PMPI_ ## kind ## _fromint(abi_value)) {                \\
            return (code) + 2;                                              \\
        }                                                                   \\
    } while (0)

    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }

    MPI_Comm comm = MPI_COMM_NULL;
    ret = MPI_Comm_dup(MPI_COMM_WORLD, &comm);
    if (MPI_SUCCESS != ret) {
        return 10;
    }
    ABI_CHECK_DYNAMIC_HANDLE(Comm, MPI_Comm, comm, 20);
    ret = MPI_Comm_free(&comm);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL != comm) {
        return 30;
    }

    MPI_Group group = MPI_GROUP_NULL;
    ret = MPI_Comm_group(MPI_COMM_WORLD, &group);
    if (MPI_SUCCESS != ret) {
        return 40;
    }
    ABI_CHECK_DYNAMIC_HANDLE(Group, MPI_Group, group, 50);
    ret = MPI_Group_free(&group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL != group) {
        return 60;
    }

    MPI_Info info = MPI_INFO_NULL;
    ret = MPI_Info_create(&info);
    if (MPI_SUCCESS != ret) {
        return 70;
    }
    ABI_CHECK_DYNAMIC_HANDLE(Info, MPI_Info, info, 80);
    ret = MPI_Info_free(&info);
    if (MPI_SUCCESS != ret || MPI_INFO_NULL != info) {
        return 90;
    }

    MPI_Datatype datatype = MPI_DATATYPE_NULL;
    ret = MPI_Type_contiguous(2, MPI_INT, &datatype);
    if (MPI_SUCCESS != ret) {
        return 100;
    }
    ret = MPI_Type_commit(&datatype);
    if (MPI_SUCCESS != ret) {
        return 110;
    }
    ABI_CHECK_DYNAMIC_HANDLE(Type, MPI_Datatype, datatype, 120);
    ret = MPI_Type_free(&datatype);
    if (MPI_SUCCESS != ret || MPI_DATATYPE_NULL != datatype) {
        return 130;
    }

    int win_storage = 0;
    MPI_Win win = MPI_WIN_NULL;
    ret = MPI_Win_create(&win_storage, sizeof(win_storage), sizeof(int),
                         MPI_INFO_NULL, MPI_COMM_WORLD, &win);
    if (MPI_SUCCESS != ret) {
        return 140;
    }
    ABI_CHECK_DYNAMIC_HANDLE(Win, MPI_Win, win, 150);
    ret = MPI_Win_free(&win);
    if (MPI_SUCCESS != ret || MPI_WIN_NULL != win) {
        return 160;
    }

    MPI_File file = MPI_FILE_NULL;
    ret = MPI_File_open(MPI_COMM_WORLD, "ompi_abi_converter_file.tmp",
                        MPI_MODE_CREATE | MPI_MODE_RDWR |
                        MPI_MODE_DELETE_ON_CLOSE,
                        MPI_INFO_NULL, &file);
    if (MPI_SUCCESS != ret) {
        return 170;
    }
    ABI_CHECK_DYNAMIC_HANDLE(File, MPI_File, file, 180);
    ret = MPI_File_close(&file);
    if (MPI_SUCCESS != ret || MPI_FILE_NULL != file) {
        return 190;
    }

    MPI_Request request = MPI_REQUEST_NULL;
    ret = MPI_Ibarrier(MPI_COMM_WORLD, &request);
    if (MPI_SUCCESS != ret) {
        return 200;
    }
    ABI_CHECK_DYNAMIC_HANDLE(Request, MPI_Request, request, 210);
    ret = MPI_Wait(&request, MPI_STATUS_IGNORE);
    if (MPI_SUCCESS != ret || MPI_REQUEST_NULL != request) {
        return 220;
    }

    int send_value = 17;
    int recv_value = -1;
    MPI_Status status;
    MPI_Message message = MPI_MESSAGE_NULL;
    MPI_Request send_request = MPI_REQUEST_NULL;
    ret = MPI_Isend(&send_value, 1, MPI_INT, 0, 77,
                    MPI_COMM_WORLD, &send_request);
    if (MPI_SUCCESS != ret) {
        return 230;
    }
    ret = MPI_Mprobe(0, 77, MPI_COMM_WORLD, &message, &status);
    if (MPI_SUCCESS != ret) {
        return 240;
    }
    ABI_CHECK_DYNAMIC_HANDLE(Message, MPI_Message, message, 250);
    ret = MPI_Mrecv(&recv_value, 1, MPI_INT, &message, &status);
    if (MPI_SUCCESS != ret || 17 != recv_value ||
        MPI_MESSAGE_NULL != message) {
        return 241;
    }
    ret = MPI_Wait(&send_request, MPI_STATUS_IGNORE);
    if (MPI_SUCCESS != ret || MPI_REQUEST_NULL != send_request) {
        return 242;
    }

    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 243;
    }
""",
    },
    {
        "name": "converter_status_sentinels",
        "rank_count": 1,
        "body": """
    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }

    int send_value = 29;
    int recv_value = -1;
    MPI_Status status;
    ret = MPI_Sendrecv(&send_value, 1, MPI_INT, 0, 81,
                       &recv_value, 1, MPI_INT, 0, 81,
                       MPI_COMM_WORLD, &status);
    if (MPI_SUCCESS != ret) {
        return 10;
    }
    if (29 != recv_value || 0 != status.MPI_SOURCE ||
        81 != status.MPI_TAG) {
        return 20;
    }

    int count = -1;
    ret = MPI_Get_count(&status, MPI_INT, &count);
    if (MPI_SUCCESS != ret || 1 != count) {
        return 30;
    }

    recv_value = -1;
    ret = MPI_Sendrecv(&send_value, 1, MPI_INT, 0, 82,
                       &recv_value, 1, MPI_INT, 0, 82,
                       MPI_COMM_WORLD, MPI_STATUS_IGNORE);
    if (MPI_SUCCESS != ret || 29 != recv_value) {
        return 40;
    }

    MPI_Request request = MPI_REQUEST_NULL;
    ret = MPI_Ibarrier(MPI_COMM_WORLD, &request);
    if (MPI_SUCCESS != ret) {
        return 50;
    }
    ret = MPI_Waitall(1, &request, MPI_STATUSES_IGNORE);
    if (MPI_SUCCESS != ret || MPI_REQUEST_NULL != request) {
        return 60;
    }

    int inplace_value = 5;
    ret = MPI_Allreduce(MPI_IN_PLACE, &inplace_value, 1, MPI_INT,
                        MPI_SUM, MPI_COMM_WORLD);
    if (MPI_SUCCESS != ret || 5 != inplace_value) {
        return 70;
    }

    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 80;
    }
""",
    },
    {
        "name": "converter_error_keyvals",
        "rank_count": 1,
        "body": """
#define ABI_EXPECTED_ERROR_CLASS_CHECKS @ERROR_CLASS_EXPECTED@
#define ABI_CHECK_ERROR_CLASS(value) do {                                   \\
        ++error_class_checks;                                               \\
        ret = MPI_Error_class(value, &error_class);                         \\
        if (MPI_SUCCESS != ret || value != error_class) {                   \\
            fprintf(stderr, "ABI_FAIL:error_class:%s\\n", #value);          \\
            return 20;                                                      \\
        }                                                                   \\
    } while (0)

    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }

    int error_class = MPI_ERR_UNKNOWN;
    int error_class_checks = 0;

@ERROR_CLASS_CHECKS@

    if (ABI_EXPECTED_ERROR_CLASS_CHECKS != error_class_checks) {
        fprintf(stderr, "ABI_FAIL:error_class:count:%d:%d\\n",
                ABI_EXPECTED_ERROR_CLASS_CHECKS, error_class_checks);
        ret = MPI_Finalize();
        if (MPI_SUCCESS != ret) {
            return 21;
        }
        return 22;
    }

    int dynamic_class = MPI_ERR_UNKNOWN;
    ret = MPI_Add_error_class(&dynamic_class);
    if (MPI_SUCCESS != ret) {
        return 30;
    }
    int dynamic_code = MPI_ERR_UNKNOWN;
    ret = MPI_Add_error_code(dynamic_class, &dynamic_code);
    if (MPI_SUCCESS != ret) {
        return 31;
    }
    ret = MPI_Error_class(dynamic_code, &error_class);
    if (MPI_SUCCESS != ret || dynamic_class != error_class) {
        return 32;
    }

    if (MPI_COMM_NULL_COPY_FN != (MPI_Comm_copy_attr_function *) 0 ||
        MPI_COMM_NULL_DELETE_FN !=
        (MPI_Comm_delete_attr_function *) 0 ||
        MPI_COMM_DUP_FN != (MPI_Comm_copy_attr_function *) 1) {
        return 40;
    }
    if (MPI_TYPE_NULL_COPY_FN != (MPI_Type_copy_attr_function *) 0 ||
        MPI_TYPE_NULL_DELETE_FN !=
        (MPI_Type_delete_attr_function *) 0 ||
        MPI_TYPE_DUP_FN != (MPI_Type_copy_attr_function *) 1) {
        return 41;
    }
    if (MPI_WIN_NULL_COPY_FN != (MPI_Win_copy_attr_function *) 0 ||
        MPI_WIN_NULL_DELETE_FN != (MPI_Win_delete_attr_function *) 0 ||
        MPI_WIN_DUP_FN != (MPI_Win_copy_attr_function *) 1) {
        return 42;
    }

    int attr_value = 12345;
    void *attr_out = NULL;
    int flag = 0;
    int comm_keyval = MPI_KEYVAL_INVALID;
    ret = MPI_Comm_create_keyval(MPI_COMM_NULL_COPY_FN,
                                 MPI_COMM_NULL_DELETE_FN,
                                 &comm_keyval, NULL);
    if (MPI_SUCCESS != ret || MPI_KEYVAL_INVALID == comm_keyval) {
        return 50;
    }
    MPI_Comm comm = MPI_COMM_NULL;
    MPI_Comm comm_dup = MPI_COMM_NULL;
    ret = MPI_Comm_dup(MPI_COMM_WORLD, &comm);
    if (MPI_SUCCESS != ret) {
        return 51;
    }
    ret = MPI_Comm_set_attr(comm, comm_keyval, &attr_value);
    if (MPI_SUCCESS != ret) {
        return 52;
    }
    ret = MPI_Comm_dup(comm, &comm_dup);
    if (MPI_SUCCESS != ret) {
        return 53;
    }
    ret = MPI_Comm_get_attr(comm_dup, comm_keyval, &attr_out, &flag);
    if (MPI_SUCCESS != ret || flag) {
        return 54;
    }
    ret = MPI_Comm_free(&comm_dup);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL != comm_dup) {
        return 55;
    }
    ret = MPI_Comm_delete_attr(comm, comm_keyval);
    if (MPI_SUCCESS != ret) {
        return 56;
    }
    ret = MPI_Comm_free(&comm);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL != comm) {
        return 57;
    }
    ret = MPI_Comm_free_keyval(&comm_keyval);
    if (MPI_SUCCESS != ret || MPI_KEYVAL_INVALID != comm_keyval) {
        return 58;
    }

    ret = MPI_Comm_create_keyval(MPI_COMM_DUP_FN,
                                 MPI_COMM_NULL_DELETE_FN,
                                 &comm_keyval, NULL);
    if (MPI_SUCCESS != ret || MPI_KEYVAL_INVALID == comm_keyval) {
        return 60;
    }
    ret = MPI_Comm_dup(MPI_COMM_WORLD, &comm);
    if (MPI_SUCCESS != ret) {
        return 61;
    }
    ret = MPI_Comm_set_attr(comm, comm_keyval, &attr_value);
    if (MPI_SUCCESS != ret) {
        return 62;
    }
    ret = MPI_Comm_dup(comm, &comm_dup);
    if (MPI_SUCCESS != ret) {
        return 63;
    }
    attr_out = NULL;
    flag = 0;
    ret = MPI_Comm_get_attr(comm_dup, comm_keyval, &attr_out, &flag);
    if (MPI_SUCCESS != ret || !flag || attr_out != &attr_value) {
        return 64;
    }
    ret = MPI_Comm_free(&comm_dup);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL != comm_dup) {
        return 65;
    }
    ret = MPI_Comm_delete_attr(comm, comm_keyval);
    if (MPI_SUCCESS != ret) {
        return 66;
    }
    ret = MPI_Comm_free(&comm);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL != comm) {
        return 67;
    }
    ret = MPI_Comm_free_keyval(&comm_keyval);
    if (MPI_SUCCESS != ret || MPI_KEYVAL_INVALID != comm_keyval) {
        return 68;
    }

    int type_keyval = MPI_KEYVAL_INVALID;
    ret = MPI_Type_create_keyval(MPI_TYPE_NULL_COPY_FN,
                                 MPI_TYPE_NULL_DELETE_FN,
                                 &type_keyval, NULL);
    if (MPI_SUCCESS != ret || MPI_KEYVAL_INVALID == type_keyval) {
        return 80;
    }
    MPI_Datatype datatype = MPI_DATATYPE_NULL;
    MPI_Datatype datatype_dup = MPI_DATATYPE_NULL;
    ret = MPI_Type_contiguous(2, MPI_INT, &datatype);
    if (MPI_SUCCESS != ret) {
        return 81;
    }
    ret = MPI_Type_commit(&datatype);
    if (MPI_SUCCESS != ret) {
        return 82;
    }
    ret = MPI_Type_set_attr(datatype, type_keyval, &attr_value);
    if (MPI_SUCCESS != ret) {
        return 83;
    }
    ret = MPI_Type_dup(datatype, &datatype_dup);
    if (MPI_SUCCESS != ret) {
        return 84;
    }
    attr_out = NULL;
    flag = 0;
    ret = MPI_Type_get_attr(datatype_dup, type_keyval, &attr_out, &flag);
    if (MPI_SUCCESS != ret || flag) {
        return 85;
    }
    ret = MPI_Type_free(&datatype_dup);
    if (MPI_SUCCESS != ret || MPI_DATATYPE_NULL != datatype_dup) {
        return 86;
    }
    ret = MPI_Type_delete_attr(datatype, type_keyval);
    if (MPI_SUCCESS != ret) {
        return 87;
    }
    ret = MPI_Type_free(&datatype);
    if (MPI_SUCCESS != ret || MPI_DATATYPE_NULL != datatype) {
        return 88;
    }
    ret = MPI_Type_free_keyval(&type_keyval);
    if (MPI_SUCCESS != ret || MPI_KEYVAL_INVALID != type_keyval) {
        return 89;
    }

    ret = MPI_Type_create_keyval(MPI_TYPE_DUP_FN,
                                 MPI_TYPE_NULL_DELETE_FN,
                                 &type_keyval, NULL);
    if (MPI_SUCCESS != ret || MPI_KEYVAL_INVALID == type_keyval) {
        return 100;
    }
    ret = MPI_Type_contiguous(2, MPI_INT, &datatype);
    if (MPI_SUCCESS != ret) {
        return 101;
    }
    ret = MPI_Type_commit(&datatype);
    if (MPI_SUCCESS != ret) {
        return 102;
    }
    ret = MPI_Type_set_attr(datatype, type_keyval, &attr_value);
    if (MPI_SUCCESS != ret) {
        return 103;
    }
    ret = MPI_Type_dup(datatype, &datatype_dup);
    if (MPI_SUCCESS != ret) {
        return 104;
    }
    attr_out = NULL;
    flag = 0;
    ret = MPI_Type_get_attr(datatype_dup, type_keyval, &attr_out, &flag);
    if (MPI_SUCCESS != ret || !flag || attr_out != &attr_value) {
        return 105;
    }
    ret = MPI_Type_free(&datatype_dup);
    if (MPI_SUCCESS != ret || MPI_DATATYPE_NULL != datatype_dup) {
        return 106;
    }
    ret = MPI_Type_delete_attr(datatype, type_keyval);
    if (MPI_SUCCESS != ret) {
        return 107;
    }
    ret = MPI_Type_free(&datatype);
    if (MPI_SUCCESS != ret || MPI_DATATYPE_NULL != datatype) {
        return 108;
    }
    ret = MPI_Type_free_keyval(&type_keyval);
    if (MPI_SUCCESS != ret || MPI_KEYVAL_INVALID != type_keyval) {
        return 109;
    }

    int win_storage = 0;
    MPI_Win win = MPI_WIN_NULL;
    ret = MPI_Win_create(&win_storage, sizeof(win_storage), sizeof(int),
                         MPI_INFO_NULL, MPI_COMM_WORLD, &win);
    if (MPI_SUCCESS != ret) {
        return 130;
    }

    int win_keyval = MPI_KEYVAL_INVALID;
    ret = MPI_Win_create_keyval(MPI_WIN_NULL_COPY_FN,
                                MPI_WIN_NULL_DELETE_FN,
                                &win_keyval, NULL);
    if (MPI_SUCCESS != ret || MPI_KEYVAL_INVALID == win_keyval) {
        return 131;
    }
    ret = MPI_Win_set_attr(win, win_keyval, &attr_value);
    if (MPI_SUCCESS != ret) {
        return 132;
    }
    ret = MPI_Win_delete_attr(win, win_keyval);
    if (MPI_SUCCESS != ret) {
        return 133;
    }
    ret = MPI_Win_free_keyval(&win_keyval);
    if (MPI_SUCCESS != ret || MPI_KEYVAL_INVALID != win_keyval) {
        return 134;
    }

    ret = MPI_Win_free(&win);
    if (MPI_SUCCESS != ret || MPI_WIN_NULL != win) {
        return 140;
    }

    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 141;
    }
""",
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
        "body": """
    int initialized = -1;
    int finalized = -1;
    int ret = MPI_Initialized(&initialized);
    if (MPI_SUCCESS != ret || initialized) {
        return 1;
    }
    ret = MPI_Finalized(&finalized);
    if (MPI_SUCCESS != ret || finalized) {
        return 2;
    }
    ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 3;
    }
    ret = MPI_Initialized(&initialized);
    if (MPI_SUCCESS != ret || !initialized) {
        return 4;
    }
    ret = MPI_Finalized(&finalized);
    if (MPI_SUCCESS != ret || finalized) {
        return 5;
    }
    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 6;
    }
    ret = MPI_Finalized(&finalized);
    if (MPI_SUCCESS != ret || !finalized) {
        return 7;
    }
""",
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
        "body": """
    int provided = -1;
    int queried = -1;
    int main_thread = 0;
    int ret = MPI_Init_thread(&argc, &argv, MPI_THREAD_SINGLE, &provided);
    if (MPI_SUCCESS != ret || provided < MPI_THREAD_SINGLE) {
        return 1;
    }
    ret = MPI_Query_thread(&queried);
    if (MPI_SUCCESS != ret || queried != provided) {
        return 2;
    }
    ret = MPI_Is_thread_main(&main_thread);
    if (MPI_SUCCESS != ret || !main_thread) {
        return 3;
    }
    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 4;
    }
""",
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
        "body": """
    MPI_Session session = MPI_SESSION_NULL;
    int ret = MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN,
                               &session);
    if (MPI_SUCCESS != ret || MPI_SESSION_NULL == session) {
        return 1;
    }

    int abi_value = MPI_Session_toint(session);
    if (MPI_Session_fromint(abi_value) != session) {
        return 2;
    }

    MPI_Info info = MPI_INFO_NULL;
    ret = MPI_Session_get_info(session, &info);
    if (MPI_SUCCESS != ret || MPI_INFO_NULL == info) {
        return 3;
    }
    ret = MPI_Info_free(&info);
    if (MPI_SUCCESS != ret || MPI_INFO_NULL != info) {
        return 4;
    }

    int npsets = -1;
    ret = MPI_Session_get_num_psets(session, MPI_INFO_NULL, &npsets);
    if (MPI_SUCCESS != ret || npsets < 1) {
        return 5;
    }

    int pset_len = 0;
    ret = MPI_Session_get_nth_pset(session, MPI_INFO_NULL, 0,
                                   &pset_len, NULL);
    if (MPI_SUCCESS != ret || pset_len < 2) {
        return 6;
    }
    char *pset_name = malloc((size_t) pset_len);
    if (NULL == pset_name) {
        return 7;
    }
    ret = MPI_Session_get_nth_pset(session, MPI_INFO_NULL, 0,
                                   &pset_len, pset_name);
    if (MPI_SUCCESS != ret || '\\0' == pset_name[0]) {
        free(pset_name);
        return 8;
    }

    ret = MPI_Session_get_pset_info(session, pset_name, &info);
    free(pset_name);
    if (MPI_SUCCESS != ret || MPI_INFO_NULL == info) {
        return 9;
    }
    ret = MPI_Info_free(&info);
    if (MPI_SUCCESS != ret || MPI_INFO_NULL != info) {
        return 10;
    }

    ret = MPI_Session_set_errhandler(session, MPI_ERRORS_RETURN);
    if (MPI_SUCCESS != ret) {
        return 11;
    }
    MPI_Errhandler errhandler = MPI_ERRHANDLER_NULL;
    ret = MPI_Session_get_errhandler(session, &errhandler);
    if (MPI_SUCCESS != ret || MPI_ERRORS_RETURN != errhandler) {
        return 12;
    }
    ret = MPI_Errhandler_free(&errhandler);
    if (MPI_SUCCESS != ret || MPI_ERRHANDLER_NULL != errhandler) {
        return 13;
    }
    ret = MPI_Session_call_errhandler(session, MPI_ERR_OTHER);
    if (MPI_SUCCESS != ret) {
        return 14;
    }

    ret = MPI_Session_finalize(&session);
    if (MPI_SUCCESS != ret || MPI_SESSION_NULL != session) {
        return 15;
    }
""",
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
        "body": """
    MPI_Session session = MPI_SESSION_NULL;
    int ret = MPI_Session_init(MPI_INFO_NULL, MPI_ERRORS_RETURN,
                               &session);
    if (MPI_SUCCESS != ret || MPI_SESSION_NULL == session) {
        return 1;
    }

    char buffer[8192];
    ret = MPI_Session_attach_buffer(session, buffer, sizeof(buffer));
    if (MPI_SUCCESS != ret) {
        return 2;
    }
    ret = MPI_Session_flush_buffer(session);
    if (MPI_SUCCESS != ret) {
        return 3;
    }

    MPI_Request request = MPI_REQUEST_NULL;
    ret = MPI_Session_iflush_buffer(session, &request);
    if (MPI_SUCCESS != ret) {
        return 4;
    }
    if (MPI_REQUEST_NULL != request) {
        ret = MPI_Wait(&request, MPI_STATUS_IGNORE);
        if (MPI_SUCCESS != ret || MPI_REQUEST_NULL != request) {
            return 5;
        }
    }

    void *detached = NULL;
    int size = 0;
    ret = MPI_Session_detach_buffer(session, &detached, &size);
    if (MPI_SUCCESS != ret || detached != buffer ||
        size != (int) sizeof(buffer)) {
        return 6;
    }

    ret = MPI_Session_finalize(&session);
    if (MPI_SUCCESS != ret || MPI_SESSION_NULL != session) {
        return 7;
    }
""",
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
        "body": """
    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }

    int size = 0;
    int rank = -1;
    ret = MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (MPI_SUCCESS != ret || size < 1) {
        return 2;
    }
    ret = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if (MPI_SUCCESS != ret || rank < 0 || rank >= size) {
        return 3;
    }

    MPI_Info info = MPI_INFO_NULL;
    ret = MPI_Info_create(&info);
    if (MPI_SUCCESS != ret || MPI_INFO_NULL == info) {
        return 4;
    }
    ret = MPI_Info_set(info, "mpi_assert_no_any_tag", "true");
    if (MPI_SUCCESS != ret) {
        return 5;
    }

    MPI_Comm comm = MPI_COMM_NULL;
    ret = MPI_Comm_dup_with_info(MPI_COMM_WORLD, info, &comm);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL == comm) {
        return 6;
    }
    ret = MPI_Comm_set_info(comm, info);
    if (MPI_SUCCESS != ret) {
        return 7;
    }
    ret = MPI_Info_free(&info);
    if (MPI_SUCCESS != ret || MPI_INFO_NULL != info) {
        return 8;
    }

    ret = MPI_Comm_set_name(comm, "abi-runtime-comm");
    if (MPI_SUCCESS != ret) {
        return 9;
    }
    char name[MPI_MAX_OBJECT_NAME];
    int name_len = -1;
    ret = MPI_Comm_get_name(comm, name, &name_len);
    if (MPI_SUCCESS != ret || 16 != name_len ||
        0 != strcmp(name, "abi-runtime-comm")) {
        return 10;
    }

    int compare = MPI_UNEQUAL;
    ret = MPI_Comm_compare(MPI_COMM_WORLD, comm, &compare);
    if (MPI_SUCCESS != ret || MPI_CONGRUENT != compare) {
        return 11;
    }

    MPI_Info used = MPI_INFO_NULL;
    ret = MPI_Comm_get_info(comm, &used);
    if (MPI_SUCCESS != ret || MPI_INFO_NULL == used) {
        return 12;
    }
    ret = MPI_Info_free(&used);
    if (MPI_SUCCESS != ret || MPI_INFO_NULL != used) {
        return 13;
    }

    MPI_Comm dup = MPI_COMM_NULL;
    MPI_Request request = MPI_REQUEST_NULL;
    ret = MPI_Comm_idup(comm, &dup, &request);
    if (MPI_SUCCESS != ret || MPI_REQUEST_NULL == request) {
        return 14;
    }
    ret = MPI_Wait(&request, MPI_STATUS_IGNORE);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL == dup ||
        MPI_REQUEST_NULL != request) {
        return 15;
    }
    ret = MPI_Comm_free(&dup);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL != dup) {
        return 16;
    }

    ret = MPI_Info_create(&info);
    if (MPI_SUCCESS != ret || MPI_INFO_NULL == info) {
        return 17;
    }
    ret = MPI_Comm_idup_with_info(comm, info, &dup, &request);
    if (MPI_SUCCESS != ret || MPI_REQUEST_NULL == request) {
        return 18;
    }
    ret = MPI_Wait(&request, MPI_STATUS_IGNORE);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL == dup ||
        MPI_REQUEST_NULL != request) {
        return 19;
    }
    ret = MPI_Info_free(&info);
    if (MPI_SUCCESS != ret || MPI_INFO_NULL != info) {
        return 20;
    }
    ret = MPI_Comm_free(&dup);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL != dup) {
        return 21;
    }

    ret = MPI_Comm_dup(comm, &dup);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL == dup) {
        return 22;
    }
    ret = MPI_Comm_free(&dup);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL != dup) {
        return 23;
    }
    ret = MPI_Comm_free(&comm);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL != comm) {
        return 24;
    }
    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 25;
    }
""",
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
        "body": """
    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }
    int size = 0;
    int rank = -1;
    ret = MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (MPI_SUCCESS != ret) {
        return 2;
    }
    ret = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if (MPI_SUCCESS != ret) {
        return 3;
    }
    if (size != @EXPECTED_RANKS@ || rank < 0 || rank >= size) {
        return 4;
    }

    MPI_Comm split = MPI_COMM_NULL;
    ret = MPI_Comm_split(MPI_COMM_WORLD, 0, rank, &split);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL == split) {
        return 5;
    }
    ret = MPI_Comm_free(&split);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL != split) {
        return 6;
    }

    MPI_Comm shared = MPI_COMM_NULL;
    ret = MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, rank,
                              MPI_INFO_NULL, &shared);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL == shared) {
        return 7;
    }
    ret = MPI_Comm_free(&shared);
    if (MPI_SUCCESS != ret || MPI_COMM_NULL != shared) {
        return 8;
    }

    MPI_Group world_group = MPI_GROUP_NULL;
    MPI_Group first_group = MPI_GROUP_NULL;
    int first_rank[1] = {0};
    ret = MPI_Comm_group(MPI_COMM_WORLD, &world_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL == world_group) {
        return 9;
    }
    ret = MPI_Group_incl(world_group, 1, first_rank, &first_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL == first_group) {
        return 10;
    }

    MPI_Comm created = MPI_COMM_NULL;
    ret = MPI_Comm_create(MPI_COMM_WORLD, first_group, &created);
    if (MPI_SUCCESS != ret) {
        return 11;
    }
    if (0 == rank) {
        if (MPI_COMM_NULL == created) {
            return 12;
        }
        ret = MPI_Comm_free(&created);
        if (MPI_SUCCESS != ret || MPI_COMM_NULL != created) {
            return 13;
        }
    } else if (MPI_COMM_NULL != created) {
        return 14;
    }

    if (0 == rank) {
        ret = MPI_Comm_create_group(MPI_COMM_WORLD, first_group, 83,
                                    &created);
        if (MPI_SUCCESS != ret || MPI_COMM_NULL == created) {
            return 15;
        }
        ret = MPI_Comm_free(&created);
        if (MPI_SUCCESS != ret || MPI_COMM_NULL != created) {
            return 16;
        }
    }

    ret = MPI_Group_free(&first_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL != first_group) {
        return 17;
    }
    ret = MPI_Group_free(&world_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL != world_group) {
        return 18;
    }
    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 19;
    }
""",
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
        "body": """
    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }

    ret = MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
    if (MPI_SUCCESS != ret) {
        return 2;
    }
    MPI_Errhandler errhandler = MPI_ERRHANDLER_NULL;
    ret = MPI_Comm_get_errhandler(MPI_COMM_WORLD, &errhandler);
    if (MPI_SUCCESS != ret || MPI_ERRORS_RETURN != errhandler) {
        return 3;
    }
    ret = MPI_Errhandler_free(&errhandler);
    if (MPI_SUCCESS != ret || MPI_ERRHANDLER_NULL != errhandler) {
        return 4;
    }
    ret = MPI_Comm_call_errhandler(MPI_COMM_WORLD, MPI_ERR_OTHER);
    if (MPI_SUCCESS != ret) {
        return 5;
    }

    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 6;
    }
""",
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
        "body": """
    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }

    MPI_Group world_group = MPI_GROUP_NULL;
    ret = MPI_Comm_group(MPI_COMM_WORLD, &world_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL == world_group) {
        return 2;
    }
    int size = 0;
    int rank = -1;
    ret = MPI_Group_size(world_group, &size);
    if (MPI_SUCCESS != ret || 1 != size) {
        return 3;
    }
    ret = MPI_Group_rank(world_group, &rank);
    if (MPI_SUCCESS != ret || 0 != rank) {
        return 4;
    }

    int ranks[1] = {0};
    MPI_Group incl_group = MPI_GROUP_NULL;
    ret = MPI_Group_incl(world_group, 1, ranks, &incl_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL == incl_group) {
        return 5;
    }

    MPI_Group empty_group = MPI_GROUP_NULL;
    ret = MPI_Group_excl(world_group, 1, ranks, &empty_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_EMPTY != empty_group) {
        return 6;
    }

    int ranges[1][3] = {{0, 0, 1}};
    MPI_Group range_group = MPI_GROUP_NULL;
    ret = MPI_Group_range_incl(world_group, 1, ranges, &range_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL == range_group) {
        return 7;
    }

    MPI_Group range_empty = MPI_GROUP_NULL;
    ret = MPI_Group_range_excl(world_group, 1, ranges, &range_empty);
    if (MPI_SUCCESS != ret || MPI_GROUP_EMPTY != range_empty) {
        return 8;
    }

    MPI_Group union_group = MPI_GROUP_NULL;
    ret = MPI_Group_union(empty_group, world_group, &union_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL == union_group) {
        return 9;
    }

    MPI_Group intersection_group = MPI_GROUP_NULL;
    ret = MPI_Group_intersection(world_group, incl_group,
                                 &intersection_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL == intersection_group) {
        return 10;
    }

    MPI_Group difference_group = MPI_GROUP_NULL;
    ret = MPI_Group_difference(world_group, empty_group,
                               &difference_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL == difference_group) {
        return 11;
    }

    int translated[1] = {-1};
    ret = MPI_Group_translate_ranks(incl_group, 1, ranks,
                                    world_group, translated);
    if (MPI_SUCCESS != ret || 0 != translated[0]) {
        return 12;
    }

    int compare = MPI_UNEQUAL;
    ret = MPI_Group_compare(world_group, union_group, &compare);
    if (MPI_SUCCESS != ret || MPI_IDENT != compare) {
        return 13;
    }

    ret = MPI_Group_free(&difference_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL != difference_group) {
        return 14;
    }
    ret = MPI_Group_free(&intersection_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL != intersection_group) {
        return 15;
    }
    ret = MPI_Group_free(&union_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL != union_group) {
        return 16;
    }
    ret = MPI_Group_free(&range_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL != range_group) {
        return 17;
    }
    ret = MPI_Group_free(&incl_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL != incl_group) {
        return 18;
    }
    ret = MPI_Group_free(&world_group);
    if (MPI_SUCCESS != ret || MPI_GROUP_NULL != world_group) {
        return 19;
    }
    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 20;
    }
""",
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
        "body": """
    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }
    int size = 0;
    int rank = -1;
    ret = MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (MPI_SUCCESS != ret) {
        return 2;
    }
    ret = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if (MPI_SUCCESS != ret) {
        return 3;
    }
    if (size != @EXPECTED_RANKS@ || rank < 0 || rank >= size) {
        return 4;
    }

    MPI_Status status;
    MPI_Request request = MPI_REQUEST_NULL;
    if (0 == rank) {
        int send_value = 19;
        ret = MPI_Send(&send_value, 1, MPI_INT, 1, 910,
                       MPI_COMM_WORLD);
        if (MPI_SUCCESS != ret) {
            return 5;
        }
        send_value = 23;
        ret = MPI_Isend(&send_value, 1, MPI_INT, 1, 911,
                        MPI_COMM_WORLD, &request);
        if (MPI_SUCCESS != ret) {
            return 6;
        }
        ret = MPI_Wait(&request, MPI_STATUS_IGNORE);
        if (MPI_SUCCESS != ret || MPI_REQUEST_NULL != request) {
            return 7;
        }
    } else if (1 == rank) {
        int recv_value = -1;
        ret = MPI_Recv(&recv_value, 1, MPI_INT, 0, 910,
                       MPI_COMM_WORLD, &status);
        if (MPI_SUCCESS != ret || 19 != recv_value) {
            return 8;
        }
        int count = -1;
        ret = MPI_Get_count(&status, MPI_INT, &count);
        if (MPI_SUCCESS != ret || 1 != count) {
            return 9;
        }
        ret = MPI_Irecv(&recv_value, 1, MPI_INT, 0, 911,
                        MPI_COMM_WORLD, &request);
        if (MPI_SUCCESS != ret) {
            return 10;
        }
        ret = MPI_Wait(&request, &status);
        if (MPI_SUCCESS != ret || MPI_REQUEST_NULL != request ||
            23 != recv_value) {
            return 11;
        }
    }

    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 12;
    }
""",
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
        "body": """
    int ret = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != ret) {
        return 1;
    }
    int size = 0;
    int rank = -1;
    ret = MPI_Comm_size(MPI_COMM_WORLD, &size);
    if (MPI_SUCCESS != ret) {
        return 2;
    }
    ret = MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if (MPI_SUCCESS != ret) {
        return 3;
    }
    if (size != @EXPECTED_RANKS@ || rank < 0 || rank >= size) {
        return 4;
    }

    int value = 0;
    if (0 == rank) {
        value = 37;
    }
    ret = MPI_Bcast(&value, 1, MPI_INT, 0, MPI_COMM_WORLD);
    if (MPI_SUCCESS != ret || 37 != value) {
        return 5;
    }

    int reduced = -1;
    ret = MPI_Allreduce(&rank, &reduced, 1, MPI_INT, MPI_SUM,
                        MPI_COMM_WORLD);
    if (MPI_SUCCESS != ret || 1 != reduced) {
        return 6;
    }
    reduced = -1;
    ret = MPI_Reduce(&rank, &reduced, 1, MPI_INT, MPI_SUM, 0,
                     MPI_COMM_WORLD);
    if (MPI_SUCCESS != ret) {
        return 7;
    }
    if (0 == rank && 1 != reduced) {
        return 8;
    }

    ret = MPI_Barrier(MPI_COMM_WORLD);
    if (MPI_SUCCESS != ret) {
        return 9;
    }
    ret = MPI_Finalize();
    if (MPI_SUCCESS != ret) {
        return 10;
    }
""",
    },
)


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
    # No configured optional MPI API-family feature currently maps
    # cleanly to a standard ABI unsupported_by_build classification.
    return {}


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


def _runtime_api_probe_body_calls(case):
    """Return MPI API calls made by a runtime probe body.

    The runtime probe table is declarative: api_names/support_api_names
    tell reports what a C body is supposed to exercise.  Keep that
    declaration tied to the generated source by checking for direct
    MPI_* function calls in the body.  These Phase 9 seed probes use
    direct calls only; if later probes need function pointers or macro
    indirection, they should extend this guard instead of weakening the
    coverage contract.
    """
    call_re = re.compile(r"(?<![A-Za-z0-9_])(MPI_[A-Za-z0-9_]+)\s*\(")
    return set(call_re.findall(case["body"]))


def _runtime_api_probe_generation_check(manifest, header, cases):
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
    for case in cases:
        advertised = (
            set(case.get("api_names", ())) |
            set(case.get("support_api_names", ()))
        )
        called = _runtime_api_probe_body_calls(case)
        missing = sorted(advertised - called)
        undeclared = sorted(called - advertised)
        if missing:
            missing_body_calls[case["name"]] = missing
        if undeclared:
            undeclared_body_calls[case["name"]] = undeclared

    if (missing_metadata or not_implemented or missing_header or
            empty_cases or missing_body_calls or undeclared_body_calls):
        return _fail(
            "installed_c_runtime_api_probe_generation",
            "runtime API probe table does not match metadata/header",
            missing_metadata=missing_metadata,
            not_implemented=not_implemented,
            missing_header=missing_header[:20],
            missing_header_count=len(missing_header),
            empty_cases=empty_cases,
            missing_body_calls=missing_body_calls,
            undeclared_body_calls=undeclared_body_calls,
            probe_count=len(cases))

    return _pass(
        "installed_c_runtime_api_probe_generation",
        probe_count=len(cases),
        primary_api_count=len(primary_names),
        support_api_count=len(all_names - primary_names),
        family_counts=_runtime_api_probe_family_counts(cases))


def _prepare_installed_c_probe_body(case, manifest, declared_names):
    """Expand generated C snippets inside one installed probe body.

    The probe template strings stay readable by using placeholders for
    generated constant lists and expected counts.  Substitution happens
    immediately before writing the temporary source, after the manifest
    has been built, configure-dependent Fortran gating is known, and the
    installed ABI header's declared Fortran datatype set has been parsed.
    """
    body = case["body"]
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


def _c_probe_source(srcdir, body, rank_count):
    """Render one installed C probe source from the shared template."""
    template = _read_text(srcdir / "test" / "mpi-abi" /
                          "templates" / "c_probe.c.in")
    body = body.replace("@EXPECTED_RANKS@", str(rank_count))
    return template.replace("@BODY@", body.rstrip())


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
        rank_count = tools["rank_counts"]["np{0}".format(
            case["rank_count"])]
        source = dirs["src"] / (name + ".c")
        executable = dirs["bin"] / name
        body = _prepare_installed_c_probe_body(
            case, manifest, header_names)
        _write_text(source, _c_probe_source(srcdir, body, rank_count))

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
        manifest, header, INSTALLED_C_RUNTIME_API_PROBES)
    _append_check(checks, runtime_api_generation_check, progress)
    if runtime_api_generation_check["result"] != "PASS":
        return checks

    checks.extend(_run_installed_c_probe_cases(
        srcdir, manifest, tools, dirs, declared_names,
        INSTALLED_C_RUNTIME_API_PROBES, progress))

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
    return checks


def _tool_info(mode):
    """Resolve tool paths, rank counts, and optional override paths."""
    tools = {
        "open_mpi": {
            "mpicc_abi": _which("OMPI_ABI_TEST_MPICC_ABI", "mpicc_abi"),
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


def main(argv):
    """Parse command-line arguments and run the MPI ABI test runner."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--srcdir", default=Path(__file__).parents[2],
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
