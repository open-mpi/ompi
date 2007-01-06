/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>
#include "mpi.h"

#include "ompi/errhandler/errcode.h"
#include "ompi/constants.h"

/* Table holding all error codes */
ompi_pointer_array_t ompi_mpi_errcodes;
int ompi_mpi_errcode_lastused=0;
int ompi_mpi_errcode_lastpredefined=0;

ompi_mpi_errcode_t ompi_success;
ompi_mpi_errcode_t ompi_err_buffer;
ompi_mpi_errcode_t ompi_err_count;
ompi_mpi_errcode_t ompi_err_type;
ompi_mpi_errcode_t ompi_err_tag;
ompi_mpi_errcode_t ompi_err_comm;
ompi_mpi_errcode_t ompi_err_rank;
ompi_mpi_errcode_t ompi_err_request;
ompi_mpi_errcode_t ompi_err_root;
ompi_mpi_errcode_t ompi_err_group;
ompi_mpi_errcode_t ompi_err_op;
ompi_mpi_errcode_t ompi_err_topology;
ompi_mpi_errcode_t ompi_err_dims;
ompi_mpi_errcode_t ompi_err_arg;
ompi_mpi_errcode_t ompi_err_unknown;
ompi_mpi_errcode_t ompi_err_truncate;
ompi_mpi_errcode_t ompi_err_other;
ompi_mpi_errcode_t ompi_err_intern;
ompi_mpi_errcode_t ompi_err_in_status;
ompi_mpi_errcode_t ompi_err_pending;

ompi_mpi_errcode_t ompi_err_access;
ompi_mpi_errcode_t ompi_err_amode;
ompi_mpi_errcode_t ompi_err_assert;
ompi_mpi_errcode_t ompi_err_bad_file;
ompi_mpi_errcode_t ompi_err_base;
ompi_mpi_errcode_t ompi_err_conversion;
ompi_mpi_errcode_t ompi_err_disp;
ompi_mpi_errcode_t ompi_err_dup_datarep;
ompi_mpi_errcode_t ompi_err_file_exists;
ompi_mpi_errcode_t ompi_err_file_in_use;
ompi_mpi_errcode_t ompi_err_file;
ompi_mpi_errcode_t ompi_err_info_key;
ompi_mpi_errcode_t ompi_err_info_nokey;
ompi_mpi_errcode_t ompi_err_info_value;
ompi_mpi_errcode_t ompi_err_info;
ompi_mpi_errcode_t ompi_err_io;
ompi_mpi_errcode_t ompi_err_keyval;
ompi_mpi_errcode_t ompi_err_locktype;
ompi_mpi_errcode_t ompi_err_name;
ompi_mpi_errcode_t ompi_err_no_mem;
ompi_mpi_errcode_t ompi_err_not_same;
ompi_mpi_errcode_t ompi_err_no_space;
ompi_mpi_errcode_t ompi_err_no_such_file;
ompi_mpi_errcode_t ompi_err_port;
ompi_mpi_errcode_t ompi_err_quota;
ompi_mpi_errcode_t ompi_err_read_only;
ompi_mpi_errcode_t ompi_err_rma_conflict;
ompi_mpi_errcode_t ompi_err_rma_sync;
ompi_mpi_errcode_t ompi_err_service;
ompi_mpi_errcode_t ompi_err_size;
ompi_mpi_errcode_t ompi_err_spawn;
ompi_mpi_errcode_t ompi_err_unsupported_datarep;
ompi_mpi_errcode_t ompi_err_unsupported_operation;
ompi_mpi_errcode_t ompi_err_win;

static void ompi_mpi_errcode_construct(ompi_mpi_errcode_t* errcode);
static void ompi_mpi_errcode_destruct(ompi_mpi_errcode_t* errcode);

OBJ_CLASS_INSTANCE(ompi_mpi_errcode_t,opal_object_t,ompi_mpi_errcode_construct, ompi_mpi_errcode_destruct);

int ompi_mpi_errcode_init (void)
{
    /* Initialize the pointer array, which will hold the references to
       the error objects */
    OBJ_CONSTRUCT(&ompi_mpi_errcodes, ompi_pointer_array_t);

    /* Initialize now each predefined error code and register
       it in the pointer-array. */
    OBJ_CONSTRUCT(&ompi_success, ompi_mpi_errcode_t);
    ompi_success.code = MPI_SUCCESS;
    ompi_success.cls = MPI_SUCCESS;
    strcpy(ompi_success.errstring, "MPI_SUCCESS: no errors");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_SUCCESS, &ompi_success);

    OBJ_CONSTRUCT(&ompi_err_buffer, ompi_mpi_errcode_t);
    ompi_err_buffer.code = MPI_ERR_BUFFER;
    ompi_err_buffer.cls = MPI_ERR_BUFFER;
    strcpy(ompi_err_buffer.errstring, "MPI_ERR_BUFFER: invalid buffer pointer");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_BUFFER, &ompi_err_buffer);

    OBJ_CONSTRUCT(&ompi_err_count, ompi_mpi_errcode_t);
    ompi_err_count.code = MPI_ERR_COUNT;
    ompi_err_count.cls = MPI_ERR_COUNT;
    strcpy(ompi_err_count.errstring, "MPI_ERR_COUNT: invalid count argument");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_COUNT, &ompi_err_count);

    OBJ_CONSTRUCT(&ompi_err_type, ompi_mpi_errcode_t);
    ompi_err_type.code = MPI_ERR_TYPE;
    ompi_err_type.cls = MPI_ERR_TYPE;
    strcpy(ompi_err_type.errstring, "MPI_ERR_TYPE: invalid datatype");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_TYPE, &ompi_err_type);

    OBJ_CONSTRUCT(&ompi_err_tag, ompi_mpi_errcode_t);
    ompi_err_tag.code = MPI_ERR_TAG;
    ompi_err_tag.cls = MPI_ERR_TAG;
    strcpy(ompi_err_tag.errstring, "MPI_ERR_TAG: invalid tag");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_TAG, &ompi_err_tag);

    OBJ_CONSTRUCT(&ompi_err_comm, ompi_mpi_errcode_t);
    ompi_err_comm.code = MPI_ERR_COMM;
    ompi_err_comm.cls = MPI_ERR_COMM;
    strcpy(ompi_err_comm.errstring, "MPI_ERR_COMM: invalid communicator");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_COMM, &ompi_err_comm);

    OBJ_CONSTRUCT(&ompi_err_rank, ompi_mpi_errcode_t);
    ompi_err_rank.code = MPI_ERR_RANK;
    ompi_err_rank.cls = MPI_ERR_RANK;
    strcpy(ompi_err_rank.errstring, "MPI_ERR_RANK: invalid rank");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_RANK, &ompi_err_rank);

    OBJ_CONSTRUCT(&ompi_err_request, ompi_mpi_errcode_t);
    ompi_err_request.code = MPI_ERR_REQUEST;
    ompi_err_request.cls = MPI_ERR_REQUEST;
    strcpy(ompi_err_request.errstring, "MPI_ERR_REQUEST: invalid request");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_REQUEST, &ompi_err_request);

    OBJ_CONSTRUCT(&ompi_err_root, ompi_mpi_errcode_t);
    ompi_err_root.code = MPI_ERR_ROOT;
    ompi_err_root.cls = MPI_ERR_ROOT;
    strcpy(ompi_err_root.errstring, "MPI_ERR_ROOT: invalid root");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_ROOT, &ompi_err_root);

    OBJ_CONSTRUCT(&ompi_err_group, ompi_mpi_errcode_t);
    ompi_err_group.code = MPI_ERR_GROUP;
    ompi_err_group.cls = MPI_ERR_GROUP;
    strcpy(ompi_err_group.errstring, "MPI_ERR_GROUP: invalid group");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_GROUP, &ompi_err_group);

    OBJ_CONSTRUCT(&ompi_err_op, ompi_mpi_errcode_t);
    ompi_err_op.code = MPI_ERR_OP;
    ompi_err_op.cls = MPI_ERR_OP;
    strcpy(ompi_err_op.errstring, "MPI_ERR_OP: invalid reduce operation");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_OP, &ompi_err_op);

    OBJ_CONSTRUCT(&ompi_err_topology, ompi_mpi_errcode_t);
    ompi_err_topology.code = MPI_ERR_TOPOLOGY;
    ompi_err_topology.cls = MPI_ERR_TOPOLOGY;
    strcpy(ompi_err_topology.errstring, "MPI_ERR_TOPOLOGY: invalid communicator topology");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_TOPOLOGY, &ompi_err_topology);

    OBJ_CONSTRUCT(&ompi_err_dims, ompi_mpi_errcode_t);
    ompi_err_dims.code = MPI_ERR_DIMS;
    ompi_err_dims.cls = MPI_ERR_DIMS;
    strcpy(ompi_err_dims.errstring, "MPI_ERR_DIMS: invalid topology dimension");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_DIMS, &ompi_err_dims);

    OBJ_CONSTRUCT(&ompi_err_arg, ompi_mpi_errcode_t);
    ompi_err_arg.code = MPI_ERR_ARG;
    ompi_err_arg.cls = MPI_ERR_ARG;
    strcpy(ompi_err_arg.errstring, "MPI_ERR_ARG: invalid argument of some other kind");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_ARG, &ompi_err_arg);

    OBJ_CONSTRUCT(&ompi_err_unknown, ompi_mpi_errcode_t);
    ompi_err_unknown.code = MPI_ERR_UNKNOWN;
    ompi_err_unknown.cls = MPI_ERR_UNKNOWN;
    strcpy(ompi_err_unknown.errstring, "MPI_ERR_UNKNOWN: unknown error");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_UNKNOWN, &ompi_err_unknown);

    OBJ_CONSTRUCT(&ompi_err_truncate, ompi_mpi_errcode_t);
    ompi_err_truncate.code = MPI_ERR_TRUNCATE;
    ompi_err_truncate.cls = MPI_ERR_TRUNCATE;
    strcpy(ompi_err_truncate.errstring, "MPI_ERR_TRUNCATE: message truncated");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_TRUNCATE, &ompi_err_truncate);

    OBJ_CONSTRUCT(&ompi_err_other, ompi_mpi_errcode_t);
    ompi_err_other.code = MPI_ERR_OTHER;
    ompi_err_other.cls = MPI_ERR_OTHER;
    strcpy(ompi_err_other.errstring, "MPI_ERR_OTHER: known error not in list");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_OTHER, &ompi_err_other);

    OBJ_CONSTRUCT(&ompi_err_intern, ompi_mpi_errcode_t);
    ompi_err_intern.code = MPI_ERR_INTERN;
    ompi_err_intern.cls = MPI_ERR_INTERN;
    strcpy(ompi_err_intern.errstring, "MPI_ERR_INTERN: internal error");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_INTERN, &ompi_err_intern);

    OBJ_CONSTRUCT(&ompi_err_in_status, ompi_mpi_errcode_t);
    ompi_err_in_status.code = MPI_ERR_IN_STATUS;
    ompi_err_in_status.cls = MPI_ERR_IN_STATUS;
    strcpy(ompi_err_in_status.errstring, "MPI_ERR_IN_STATUS: error code in status");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_IN_STATUS, &ompi_err_in_status);

    OBJ_CONSTRUCT(&ompi_err_pending, ompi_mpi_errcode_t);
    ompi_err_pending.code = MPI_ERR_PENDING;
    ompi_err_pending.cls = MPI_ERR_PENDING;
    strcpy(ompi_err_pending.errstring, "MPI_ERR_PENDING: pending request");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_PENDING, &ompi_err_pending);

    OBJ_CONSTRUCT(&ompi_err_access, ompi_mpi_errcode_t);
    ompi_err_access.code = MPI_ERR_ACCESS;
    ompi_err_access.cls = MPI_ERR_ACCESS;
    strcpy(ompi_err_access.errstring, "MPI_ERR_ACCESS: invalid access mode");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_ACCESS, &ompi_err_access);

    OBJ_CONSTRUCT(&ompi_err_amode, ompi_mpi_errcode_t);
    ompi_err_amode.code = MPI_ERR_AMODE;
    ompi_err_amode.cls = MPI_ERR_AMODE;
    strcpy(ompi_err_amode.errstring, "MPI_ERR_AMODE: invalid amode argument");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_AMODE, &ompi_err_amode);

    OBJ_CONSTRUCT(&ompi_err_assert, ompi_mpi_errcode_t);
    ompi_err_assert.code = MPI_ERR_ASSERT;
    ompi_err_assert.cls = MPI_ERR_ASSERT;
    strcpy(ompi_err_assert.errstring, "MPI_ERR_ASSERT: invalid assert argument");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_ASSERT, &ompi_err_assert);

    OBJ_CONSTRUCT(&ompi_err_bad_file, ompi_mpi_errcode_t);
    ompi_err_bad_file.code = MPI_ERR_BAD_FILE;
    ompi_err_bad_file.cls = MPI_ERR_BAD_FILE;
    strcpy(ompi_err_bad_file.errstring, "MPI_ERR_BAD_FILE: bad file");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_BAD_FILE, &ompi_err_bad_file);

    OBJ_CONSTRUCT(&ompi_err_base, ompi_mpi_errcode_t);
    ompi_err_base.code = MPI_ERR_BASE;
    ompi_err_base.cls = MPI_ERR_BASE;
    strcpy(ompi_err_base.errstring, "MPI_ERR_BASE: invalid base");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_BASE, &ompi_err_base);

    OBJ_CONSTRUCT(&ompi_err_conversion, ompi_mpi_errcode_t);
    ompi_err_conversion.code = MPI_ERR_CONVERSION;
    ompi_err_conversion.cls = MPI_ERR_CONVERSION;
    strcpy(ompi_err_conversion.errstring, "MPI_ERR_CONVERSION: error in data conversion");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_CONVERSION, &ompi_err_conversion);

    OBJ_CONSTRUCT(&ompi_err_disp, ompi_mpi_errcode_t);
    ompi_err_disp.code = MPI_ERR_DISP;
    ompi_err_disp.cls = MPI_ERR_DISP;
    strcpy(ompi_err_disp.errstring, "MPI_ERR_DISP: invalid displacement");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_DISP, &ompi_err_disp);

    OBJ_CONSTRUCT(&ompi_err_dup_datarep, ompi_mpi_errcode_t);
    ompi_err_dup_datarep.code = MPI_ERR_DUP_DATAREP;
    ompi_err_dup_datarep.cls = MPI_ERR_DUP_DATAREP;
    strcpy(ompi_err_dup_datarep.errstring, 
           "MPI_ERR_DUP_DATAREP: error while duplicating data representation");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_DUP_DATAREP, &ompi_err_dup_datarep);

    OBJ_CONSTRUCT(&ompi_err_file_exists, ompi_mpi_errcode_t);
    ompi_err_file_exists.code = MPI_ERR_FILE_EXISTS;
    ompi_err_file_exists.cls = MPI_ERR_FILE_EXISTS;
    strcpy(ompi_err_file_exists.errstring, "MPI_ERR_FILE_EXISTS: file exists alreay");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_FILE_EXISTS, &ompi_err_file_exists);

    OBJ_CONSTRUCT(&ompi_err_file_in_use, ompi_mpi_errcode_t);
    ompi_err_file_in_use.code = MPI_ERR_FILE_IN_USE;
    ompi_err_file_in_use.cls = MPI_ERR_FILE_IN_USE;
    strcpy(ompi_err_file_in_use.errstring, "MPI_ERR_FILE_IN_USE: file already in use");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_FILE_IN_USE, &ompi_err_file_in_use);

    OBJ_CONSTRUCT(&ompi_err_file, ompi_mpi_errcode_t);
    ompi_err_file.code = MPI_ERR_FILE;
    ompi_err_file.cls = MPI_ERR_FILE;
    strcpy(ompi_err_file.errstring, "MPI_ERR_FILE: invalid file");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_FILE, &ompi_err_file);

    OBJ_CONSTRUCT(&ompi_err_info_key, ompi_mpi_errcode_t);
    ompi_err_info_key.code = MPI_ERR_INFO_KEY;
    ompi_err_info_key.cls = MPI_ERR_INFO_KEY;
    strcpy(ompi_err_info_key.errstring, "MPI_ERR_INFO_KEY: invalid key argument for info object");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_INFO_KEY, &ompi_err_info_key);

    OBJ_CONSTRUCT(&ompi_err_info_nokey, ompi_mpi_errcode_t);
    ompi_err_info_nokey.code = MPI_ERR_INFO_NOKEY;
    ompi_err_info_nokey.cls = MPI_ERR_INFO_NOKEY;
    strcpy(ompi_err_info_nokey.errstring, "MPI_ERR_INFO_NOKEY: unknown key for given info object");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_INFO_NOKEY, &ompi_err_info_nokey);

    OBJ_CONSTRUCT(&ompi_err_info_value, ompi_mpi_errcode_t);
    ompi_err_info_value.code = MPI_ERR_INFO_VALUE;
    ompi_err_info_value.cls = MPI_ERR_INFO_VALUE;
    strcpy(ompi_err_info_value.errstring, 
           "MPI_ERR_INFO_VALUE: invalid value argument for info object");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_INFO_VALUE, &ompi_err_info_value);

    OBJ_CONSTRUCT(&ompi_err_info, ompi_mpi_errcode_t);
    ompi_err_info.code = MPI_ERR_INFO;
    ompi_err_info.cls = MPI_ERR_INFO;
    strcpy(ompi_err_info.errstring, "MPI_ERR_INFO: invalid info object");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_INFO, &ompi_err_info);

    OBJ_CONSTRUCT(&ompi_err_io, ompi_mpi_errcode_t);
    ompi_err_io.code = MPI_ERR_IO;
    ompi_err_io.cls = MPI_ERR_IO;
    strcpy(ompi_err_io.errstring, "MPI_ERR_IO: input/output error");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_IO, &ompi_err_io);

    OBJ_CONSTRUCT(&ompi_err_keyval, ompi_mpi_errcode_t);
    ompi_err_keyval.code = MPI_ERR_KEYVAL;
    ompi_err_keyval.cls = MPI_ERR_KEYVAL;
    strcpy(ompi_err_keyval.errstring, "MPI_ERR_KEYVAL: invalid key value");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_KEYVAL, &ompi_err_keyval);

    OBJ_CONSTRUCT(&ompi_err_locktype, ompi_mpi_errcode_t);
    ompi_err_locktype.code = MPI_ERR_LOCKTYPE;
    ompi_err_locktype.cls = MPI_ERR_LOCKTYPE;
    strcpy(ompi_err_locktype.errstring, "MPI_ERR_LOCKTYPE: invalid lock");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_LOCKTYPE, &ompi_err_locktype);

    OBJ_CONSTRUCT(&ompi_err_name, ompi_mpi_errcode_t);
    ompi_err_name.code = MPI_ERR_NAME;
    ompi_err_name.cls = MPI_ERR_NAME;
    strcpy(ompi_err_name.errstring, "MPI_ERR_NAME: invalid name argument");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_NAME, &ompi_err_name);

    OBJ_CONSTRUCT(&ompi_err_no_mem, ompi_mpi_errcode_t);
    ompi_err_no_mem.code = MPI_ERR_NO_MEM;
    ompi_err_no_mem.cls = MPI_ERR_NO_MEM;
    strcpy(ompi_err_no_mem.errstring, "MPI_ERR_NO_MEM: out of memory");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_NO_MEM, &ompi_err_no_mem);

    OBJ_CONSTRUCT(&ompi_err_not_same, ompi_mpi_errcode_t);
    ompi_err_not_same.code = MPI_ERR_NOT_SAME;
    ompi_err_not_same.cls = MPI_ERR_NOT_SAME;
    strcpy(ompi_err_not_same.errstring, "MPI_ERR_NOT_SAME: objects are not identical");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_NOT_SAME, &ompi_err_not_same);

    OBJ_CONSTRUCT(&ompi_err_no_space, ompi_mpi_errcode_t);
    ompi_err_no_space.code = MPI_ERR_NO_SPACE;
    ompi_err_no_space.cls = MPI_ERR_NO_SPACE;
    strcpy(ompi_err_no_space.errstring, "MPI_ERR_NO_SPACE: no space left on device");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_NO_SPACE, &ompi_err_no_space);

    OBJ_CONSTRUCT(&ompi_err_no_such_file, ompi_mpi_errcode_t);
    ompi_err_no_such_file.code = MPI_ERR_NO_SUCH_FILE;
    ompi_err_no_such_file.cls = MPI_ERR_NO_SUCH_FILE;
    strcpy(ompi_err_no_such_file.errstring, "MPI_ERR_NO_SUCH_FILE: no such file or directory");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_NO_SUCH_FILE, &ompi_err_no_such_file);

    OBJ_CONSTRUCT(&ompi_err_port, ompi_mpi_errcode_t);
    ompi_err_port.code = MPI_ERR_PORT;
    ompi_err_port.cls = MPI_ERR_PORT;
    strcpy(ompi_err_port.errstring, "MPI_ERR_PORT: MPI_ERR_PORT: invalid port");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_PORT, &ompi_err_port);

    OBJ_CONSTRUCT(&ompi_err_quota, ompi_mpi_errcode_t);
    ompi_err_quota.code = MPI_ERR_QUOTA;
    ompi_err_quota.cls = MPI_ERR_QUOTA;
    strcpy(ompi_err_quota.errstring, "MPI_ERR_QUOTA: out of quota");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_QUOTA, &ompi_err_quota);

    OBJ_CONSTRUCT(&ompi_err_read_only, ompi_mpi_errcode_t);
    ompi_err_read_only.code = MPI_ERR_READ_ONLY;
    ompi_err_read_only.cls = MPI_ERR_READ_ONLY;
    strcpy(ompi_err_read_only.errstring, "MPI_ERR_READ_ONLY: file is read only");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_READ_ONLY, &ompi_err_read_only);

    OBJ_CONSTRUCT(&ompi_err_rma_conflict, ompi_mpi_errcode_t);
    ompi_err_rma_conflict.code = MPI_ERR_RMA_CONFLICT;
    ompi_err_rma_conflict.cls = MPI_ERR_RMA_CONFLICT;
    strcpy(ompi_err_rma_conflict.errstring, "MPI_ERR_RMA_CONFLICT: rma conflict during operation");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_RMA_CONFLICT, &ompi_err_rma_conflict);

    OBJ_CONSTRUCT(&ompi_err_rma_sync, ompi_mpi_errcode_t);
    ompi_err_rma_sync.code = MPI_ERR_RMA_SYNC;
    ompi_err_rma_sync.cls = MPI_ERR_RMA_SYNC;
    strcpy(ompi_err_rma_sync.errstring, "MPI_ERR_RMA_SYNC: error while executing rma sync");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_RMA_SYNC, &ompi_err_rma_sync);

    OBJ_CONSTRUCT(&ompi_err_service, ompi_mpi_errcode_t);
    ompi_err_service.code = MPI_ERR_SERVICE;
    ompi_err_service.cls = MPI_ERR_SERVICE;
    strcpy(ompi_err_service.errstring, "MPI_ERR_SERVICE: unknown service name");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_SERVICE, &ompi_err_service);

    OBJ_CONSTRUCT(&ompi_err_size, ompi_mpi_errcode_t);
    ompi_err_size.code = MPI_ERR_SIZE;
    ompi_err_size.cls = MPI_ERR_SIZE;
    strcpy(ompi_err_size.errstring, "MPI_ERR_SIZE: invalid size");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_SIZE, &ompi_err_size);

    OBJ_CONSTRUCT(&ompi_err_spawn, ompi_mpi_errcode_t);
    ompi_err_spawn.code = MPI_ERR_SPAWN;
    ompi_err_spawn.cls = MPI_ERR_SPAWN;
    strcpy(ompi_err_spawn.errstring, "MPI_ERR_SPAWN: could not spawn processes");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_SPAWN, &ompi_err_spawn);

    OBJ_CONSTRUCT(&ompi_err_unsupported_datarep, ompi_mpi_errcode_t);
    ompi_err_unsupported_datarep.code = MPI_ERR_UNSUPPORTED_DATAREP;
    ompi_err_unsupported_datarep.cls = MPI_ERR_UNSUPPORTED_DATAREP;
    strcpy(ompi_err_unsupported_datarep.errstring, 
           "MPI_ERR_UNSUPPORTED_DATAREP: requested data representation not supported");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_UNSUPPORTED_DATAREP, 
                              &ompi_err_unsupported_datarep);

    OBJ_CONSTRUCT(&ompi_err_unsupported_operation, ompi_mpi_errcode_t);
    ompi_err_unsupported_operation.code = MPI_ERR_UNSUPPORTED_OPERATION;
    ompi_err_unsupported_operation.cls = MPI_ERR_UNSUPPORTED_OPERATION;
    strcpy(ompi_err_unsupported_operation.errstring, 
           "MPI_ERR_UNSUPPORTED_OPERATION: requested operation not suppported");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_UNSUPPORTED_OPERATION, 
                              &ompi_err_unsupported_operation);

    OBJ_CONSTRUCT(&ompi_err_win, ompi_mpi_errcode_t);
    ompi_err_win.code = MPI_ERR_WIN;
    ompi_err_win.cls = MPI_ERR_WIN;
    strcpy(ompi_err_win.errstring, "MPI_ERR_WIN:invalid window");
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, MPI_ERR_WIN, &ompi_err_win);

    ompi_mpi_errcode_lastused=MPI_ERR_WIN;
    ompi_mpi_errcode_lastpredefined=MPI_ERR_WIN;
    return OMPI_SUCCESS;
}

int ompi_mpi_errcode_finalize(void)
{
    int i;
    ompi_mpi_errcode_t *errc;
    
    for (i=ompi_mpi_errcode_lastpredefined+1; i<=ompi_mpi_errcode_lastused; i++) {
        /* 
         * there are some user defined error-codes, which
         * we have to free.
         */
        errc = (ompi_mpi_errcode_t *)ompi_pointer_array_get_item(&ompi_mpi_errcodes, i);
        OBJ_RELEASE (errc);
    }

    OBJ_DESTRUCT(&ompi_success);
    OBJ_DESTRUCT(&ompi_err_buffer);
    OBJ_DESTRUCT(&ompi_err_count);
    OBJ_DESTRUCT(&ompi_err_type);
    OBJ_DESTRUCT(&ompi_err_tag);
    OBJ_DESTRUCT(&ompi_err_comm);
    OBJ_DESTRUCT(&ompi_err_rank);
    OBJ_DESTRUCT(&ompi_err_request);
    OBJ_DESTRUCT(&ompi_err_root);
    OBJ_DESTRUCT(&ompi_err_group);
    OBJ_DESTRUCT(&ompi_err_op);
    OBJ_DESTRUCT(&ompi_err_topology);
    OBJ_DESTRUCT(&ompi_err_dims);
    OBJ_DESTRUCT(&ompi_err_arg);
    OBJ_DESTRUCT(&ompi_err_unknown);
    OBJ_DESTRUCT(&ompi_err_truncate);
    OBJ_DESTRUCT(&ompi_err_other);
    OBJ_DESTRUCT(&ompi_err_intern);
    OBJ_DESTRUCT(&ompi_err_in_status);
    OBJ_DESTRUCT(&ompi_err_pending);
    OBJ_DESTRUCT(&ompi_err_access);
    OBJ_DESTRUCT(&ompi_err_amode);
    OBJ_DESTRUCT(&ompi_err_assert);
    OBJ_DESTRUCT(&ompi_err_bad_file);
    OBJ_DESTRUCT(&ompi_err_base);
    OBJ_DESTRUCT(&ompi_err_conversion);
    OBJ_DESTRUCT(&ompi_err_disp);
    OBJ_DESTRUCT(&ompi_err_dup_datarep);
    OBJ_DESTRUCT(&ompi_err_file_exists);
    OBJ_DESTRUCT(&ompi_err_file_in_use);
    OBJ_DESTRUCT(&ompi_err_file);
    OBJ_DESTRUCT(&ompi_err_info_key);
    OBJ_DESTRUCT(&ompi_err_info_nokey);
    OBJ_DESTRUCT(&ompi_err_info_value);
    OBJ_DESTRUCT(&ompi_err_info);
    OBJ_DESTRUCT(&ompi_err_io);
    OBJ_DESTRUCT(&ompi_err_keyval);
    OBJ_DESTRUCT(&ompi_err_locktype);
    OBJ_DESTRUCT(&ompi_err_name);
    OBJ_DESTRUCT(&ompi_err_no_mem);
    OBJ_DESTRUCT(&ompi_err_not_same);
    OBJ_DESTRUCT(&ompi_err_no_space);
    OBJ_DESTRUCT(&ompi_err_no_such_file);
    OBJ_DESTRUCT(&ompi_err_port);
    OBJ_DESTRUCT(&ompi_err_quota);
    OBJ_DESTRUCT(&ompi_err_read_only);
    OBJ_DESTRUCT(&ompi_err_rma_conflict);
    OBJ_DESTRUCT(&ompi_err_rma_sync);
    OBJ_DESTRUCT(&ompi_err_service);
    OBJ_DESTRUCT(&ompi_err_size);
    OBJ_DESTRUCT(&ompi_err_spawn);
    OBJ_DESTRUCT(&ompi_err_unsupported_datarep);
    OBJ_DESTRUCT(&ompi_err_unsupported_operation);
    OBJ_DESTRUCT(&ompi_err_win);

    OBJ_DESTRUCT(&ompi_mpi_errcodes);
    return OMPI_SUCCESS;
}

int ompi_mpi_errcode_add(int errclass )
{
    ompi_mpi_errcode_t *newerrcode;

    newerrcode = OBJ_NEW(ompi_mpi_errcode_t);
    newerrcode->code = (ompi_mpi_errcode_lastused+1);
    newerrcode->cls = errclass;
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, newerrcode->code, newerrcode);
    
    ompi_mpi_errcode_lastused++;
    return newerrcode->code;
}

int ompi_mpi_errclass_add(void)
{
    ompi_mpi_errcode_t *newerrcode;

    newerrcode = OBJ_NEW(ompi_mpi_errcode_t);
    newerrcode->cls = ( ompi_mpi_errcode_lastused+1);
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, newerrcode->cls, newerrcode);
    
    ompi_mpi_errcode_lastused++;
    return newerrcode->cls;
}

int ompi_mpi_errnum_add_string(int errnum, char *errstring, int len)
{
    ompi_mpi_errcode_t *errcodep;

    errcodep = (ompi_mpi_errcode_t *)ompi_pointer_array_get_item(&ompi_mpi_errcodes, errnum);
    if ( NULL == errcodep ) { 
        return OMPI_ERROR;
    }

    if ( MPI_MAX_ERROR_STRING > len ) {
        len = MPI_MAX_ERROR_STRING;
    }
    
    strncpy ( errcodep->errstring, errstring, len );
    return OMPI_SUCCESS;
}

static void ompi_mpi_errcode_construct(ompi_mpi_errcode_t *errcode)
{
    errcode->code = MPI_UNDEFINED;
    errcode->cls = MPI_UNDEFINED;
    memset ( errcode->errstring, 0, MPI_MAX_ERROR_STRING);
    return;
}

static void ompi_mpi_errcode_destruct(ompi_mpi_errcode_t *errcode)
{
    ompi_pointer_array_set_item(&ompi_mpi_errcodes, errcode->code, NULL);
    return;
}
