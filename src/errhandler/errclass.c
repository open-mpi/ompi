/*
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include "mpi.h"

#include "errhandler/errclass.h"
#include "include/constants.h"

/* Table holding all error codes */
ompi_pointer_array_t ompi_errclasses;
int ompi_errclass_lastused=0;
int ompi_errclass_lastpredefined=0;

ompi_errclass_t ompi_errclass_success;
ompi_errclass_t ompi_errclass_buffer;
ompi_errclass_t ompi_errclass_count;
ompi_errclass_t ompi_errclass_type;
ompi_errclass_t ompi_errclass_tag;
ompi_errclass_t ompi_errclass_comm;
ompi_errclass_t ompi_errclass_rank;
ompi_errclass_t ompi_errclass_request;
ompi_errclass_t ompi_errclass_root;
ompi_errclass_t ompi_errclass_group;
ompi_errclass_t ompi_errclass_op;
ompi_errclass_t ompi_errclass_topology;
ompi_errclass_t ompi_errclass_dims;
ompi_errclass_t ompi_errclass_arg;
ompi_errclass_t ompi_errclass_unknown;
ompi_errclass_t ompi_errclass_truncate;
ompi_errclass_t ompi_errclass_other;
ompi_errclass_t ompi_errclass_intern;
ompi_errclass_t ompi_errclass_in_status;
ompi_errclass_t ompi_errclass_pending;

ompi_errclass_t ompi_errclass_access;
ompi_errclass_t ompi_errclass_amode;
ompi_errclass_t ompi_errclass_assert;
ompi_errclass_t ompi_errclass_bad_file;
ompi_errclass_t ompi_errclass_base;
ompi_errclass_t ompi_errclass_conversion;
ompi_errclass_t ompi_errclass_disp;
ompi_errclass_t ompi_errclass_dup_datarep;
ompi_errclass_t ompi_errclass_file_exists;
ompi_errclass_t ompi_errclass_file_in_use;
ompi_errclass_t ompi_errclass_file;
ompi_errclass_t ompi_errclass_info_key;
ompi_errclass_t ompi_errclass_info_nokey;
ompi_errclass_t ompi_errclass_info_value;
ompi_errclass_t ompi_errclass_info;
ompi_errclass_t ompi_errclass_io;
ompi_errclass_t ompi_errclass_keyval;
ompi_errclass_t ompi_errclass_locktype;
ompi_errclass_t ompi_errclass_name;
ompi_errclass_t ompi_errclass_no_mem;
ompi_errclass_t ompi_errclass_not_same;
ompi_errclass_t ompi_errclass_no_space;
ompi_errclass_t ompi_errclass_no_such_file;
ompi_errclass_t ompi_errclass_port;
ompi_errclass_t ompi_errclass_quota;
ompi_errclass_t ompi_errclass_read_only;
ompi_errclass_t ompi_errclass_rma_conflict;
ompi_errclass_t ompi_errclass_rma_sync;
ompi_errclass_t ompi_errclass_service;
ompi_errclass_t ompi_errclass_size;
ompi_errclass_t ompi_errclass_spawn;
ompi_errclass_t ompi_errclass_unsupported_datarep;
ompi_errclass_t ompi_errclass_unsupported_operation;
ompi_errclass_t ompi_errclass_win;

static void ompi_errclass_construct(ompi_errclass_t* errcode);
static void ompi_errclass_destruct(ompi_errclass_t* errcode);

OBJ_CLASS_INSTANCE(ompi_errclass_t,ompi_object_t,ompi_errclass_construct, ompi_errclass_destruct);

int ompi_errclass_init (void)
{
    /* 
     * Initialize the pointer array, which will hold the references to
     * the error objects 
     */
    OBJ_CONSTRUCT(&ompi_errclasses, ompi_pointer_array_t);
    
    /* 
     * Initialize now each predefined error class and register
     * it in the pointer-array. 
     */
    OBJ_CONSTRUCT(&ompi_errclass_success, ompi_errclass_t);
    ompi_errclass_success.class = MPI_SUCCESS;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_SUCCESS, &ompi_errclass_success);
    
    OBJ_CONSTRUCT(&ompi_errclass_buffer, ompi_errclass_t);
    ompi_errclass_buffer.class = MPI_ERR_BUFFER;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_BUFFER, &ompi_errclass_buffer);
    
    OBJ_CONSTRUCT(&ompi_errclass_count, ompi_errclass_t);
    ompi_errclass_count.class = MPI_ERR_COUNT;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_COUNT, &ompi_errclass_count);
    
    OBJ_CONSTRUCT(&ompi_errclass_type, ompi_errclass_t);
    ompi_errclass_type.class = MPI_ERR_TYPE;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_TYPE, &ompi_errclass_type);
    
    OBJ_CONSTRUCT(&ompi_errclass_tag, ompi_errclass_t);
    ompi_errclass_tag.class = MPI_ERR_TAG;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_TAG, &ompi_errclass_tag);
    
    OBJ_CONSTRUCT(&ompi_errclass_comm, ompi_errclass_t);
    ompi_errclass_comm.class = MPI_ERR_COMM;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_COMM, &ompi_errclass_comm);
    
    OBJ_CONSTRUCT(&ompi_errclass_rank, ompi_errclass_t);
    ompi_errclass_rank.class = MPI_ERR_RANK;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_RANK, &ompi_errclass_rank);
    
    OBJ_CONSTRUCT(&ompi_errclass_request, ompi_errclass_t);
    ompi_errclass_request.class = MPI_ERR_REQUEST;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_REQUEST, &ompi_errclass_request);
    
    OBJ_CONSTRUCT(&ompi_errclass_root, ompi_errclass_t);
    ompi_errclass_root.class = MPI_ERR_ROOT;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_ROOT, &ompi_errclass_root);
    
    OBJ_CONSTRUCT(&ompi_errclass_group, ompi_errclass_t);
    ompi_errclass_group.class = MPI_ERR_GROUP;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_GROUP, &ompi_errclass_group);
    
    OBJ_CONSTRUCT(&ompi_errclass_op, ompi_errclass_t);
    ompi_errclass_op.class = MPI_ERR_OP;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_OP, &ompi_errclass_op);
    
    OBJ_CONSTRUCT(&ompi_errclass_topology, ompi_errclass_t);
    ompi_errclass_topology.class = MPI_ERR_TOPOLOGY;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_TOPOLOGY, &ompi_errclass_topology);
    
    OBJ_CONSTRUCT(&ompi_errclass_dims, ompi_errclass_t);
    ompi_errclass_dims.class = MPI_ERR_DIMS;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_DIMS, &ompi_errclass_dims);
    
    OBJ_CONSTRUCT(&ompi_errclass_arg, ompi_errclass_t);
    ompi_errclass_arg.class = MPI_ERR_ARG;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_ARG, &ompi_errclass_arg);
    
    OBJ_CONSTRUCT(&ompi_errclass_unknown, ompi_errclass_t);
    ompi_errclass_unknown.class = MPI_ERR_UNKNOWN;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_UNKNOWN, &ompi_errclass_unknown);
    
    OBJ_CONSTRUCT(&ompi_errclass_truncate, ompi_errclass_t);
    ompi_errclass_truncate.class = MPI_ERR_TRUNCATE;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_TRUNCATE, &ompi_errclass_truncate);
    
    OBJ_CONSTRUCT(&ompi_errclass_other, ompi_errclass_t);
    ompi_errclass_other.class = MPI_ERR_OTHER;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_OTHER, &ompi_errclass_other);
    
    OBJ_CONSTRUCT(&ompi_errclass_intern, ompi_errclass_t);
    ompi_errclass_intern.class = MPI_ERR_INTERN;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_INTERN, &ompi_errclass_intern);
    
    OBJ_CONSTRUCT(&ompi_errclass_in_status, ompi_errclass_t);
    ompi_errclass_in_status.class = MPI_ERR_IN_STATUS;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_IN_STATUS, &ompi_errclass_in_status);
    
    OBJ_CONSTRUCT(&ompi_errclass_pending, ompi_errclass_t);
    ompi_errclass_pending.class = MPI_ERR_PENDING;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_PENDING, &ompi_errclass_pending);
    
    OBJ_CONSTRUCT(&ompi_errclass_access, ompi_errclass_t);
    ompi_errclass_access.class = MPI_ERR_ACCESS;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_ACCESS, &ompi_errclass_access);
    
    OBJ_CONSTRUCT(&ompi_errclass_amode, ompi_errclass_t);
    ompi_errclass_amode.class = MPI_ERR_AMODE;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_AMODE, &ompi_errclass_amode);
    
    OBJ_CONSTRUCT(&ompi_errclass_assert, ompi_errclass_t);
    ompi_errclass_assert.class = MPI_ERR_ASSERT;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_ASSERT, &ompi_errclass_assert);
    
    OBJ_CONSTRUCT(&ompi_errclass_bad_file, ompi_errclass_t);
    ompi_errclass_bad_file.class = MPI_ERR_BAD_FILE;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_BAD_FILE, &ompi_errclass_bad_file);
    
    OBJ_CONSTRUCT(&ompi_errclass_base, ompi_errclass_t);
    ompi_errclass_base.class = MPI_ERR_BASE;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_BASE, &ompi_errclass_base);

    OBJ_CONSTRUCT(&ompi_errclass_conversion, ompi_errclass_t);
    ompi_errclass_conversion.class = MPI_ERR_CONVERSION;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_CONVERSION, &ompi_errclass_conversion);

    OBJ_CONSTRUCT(&ompi_errclass_disp, ompi_errclass_t);
    ompi_errclass_disp.class = MPI_ERR_DISP;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_DISP, &ompi_errclass_disp);

    OBJ_CONSTRUCT(&ompi_errclass_dup_datarep, ompi_errclass_t);
    ompi_errclass_dup_datarep.class = MPI_ERR_DUP_DATAREP;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_DUP_DATAREP, &ompi_errclass_dup_datarep);
    
    OBJ_CONSTRUCT(&ompi_errclass_file_exists, ompi_errclass_t);
    ompi_errclass_file_exists.class = MPI_ERR_FILE_EXISTS;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_FILE_EXISTS, &ompi_errclass_file_exists);

    OBJ_CONSTRUCT(&ompi_errclass_file_in_use, ompi_errclass_t);
    ompi_errclass_file_in_use.class = MPI_ERR_FILE_IN_USE;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_FILE_IN_USE, &ompi_errclass_file_in_use);

    OBJ_CONSTRUCT(&ompi_errclass_file, ompi_errclass_t);
    ompi_errclass_file.class = MPI_ERR_FILE;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_FILE, &ompi_errclass_file);

    OBJ_CONSTRUCT(&ompi_errclass_info_key, ompi_errclass_t);
    ompi_errclass_info_key.class = MPI_ERR_INFO_KEY;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_INFO_KEY, &ompi_errclass_info_key);

    OBJ_CONSTRUCT(&ompi_errclass_info_nokey, ompi_errclass_t);
    ompi_errclass_info_nokey.class = MPI_ERR_INFO_NOKEY;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_INFO_NOKEY, &ompi_errclass_info_nokey);

    OBJ_CONSTRUCT(&ompi_errclass_info_value, ompi_errclass_t);
    ompi_errclass_info_value.class = MPI_ERR_INFO_VALUE;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_INFO_VALUE, &ompi_errclass_info_value);

    OBJ_CONSTRUCT(&ompi_errclass_info, ompi_errclass_t);
    ompi_errclass_info.class = MPI_ERR_INFO;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_INFO, &ompi_errclass_info);

    OBJ_CONSTRUCT(&ompi_errclass_io, ompi_errclass_t);
    ompi_errclass_io.class = MPI_ERR_IO;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_IO, &ompi_errclass_io);

    OBJ_CONSTRUCT(&ompi_errclass_keyval, ompi_errclass_t);
    ompi_errclass_keyval.class = MPI_ERR_KEYVAL;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_KEYVAL, &ompi_errclass_keyval);

    OBJ_CONSTRUCT(&ompi_errclass_locktype, ompi_errclass_t);
    ompi_errclass_locktype.class = MPI_ERR_LOCKTYPE;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_LOCKTYPE, &ompi_errclass_locktype);

    OBJ_CONSTRUCT(&ompi_errclass_name, ompi_errclass_t);
    ompi_errclass_name.class = MPI_ERR_NAME;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_NAME, &ompi_errclass_name);

    OBJ_CONSTRUCT(&ompi_errclass_no_mem, ompi_errclass_t);
    ompi_errclass_no_mem.class = MPI_ERR_NO_MEM;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_NO_MEM, &ompi_errclass_no_mem);

    OBJ_CONSTRUCT(&ompi_errclass_not_same, ompi_errclass_t);
    ompi_errclass_not_same.class = MPI_ERR_NOT_SAME;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_NOT_SAME, &ompi_errclass_not_same);

    OBJ_CONSTRUCT(&ompi_errclass_no_space, ompi_errclass_t);
    ompi_errclass_no_space.class = MPI_ERR_NO_SPACE;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_NO_SPACE, &ompi_errclass_no_space);

    OBJ_CONSTRUCT(&ompi_errclass_no_such_file, ompi_errclass_t);
    ompi_errclass_no_such_file.class = MPI_ERR_NO_SUCH_FILE;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_NO_SUCH_FILE, &ompi_errclass_no_such_file);

    OBJ_CONSTRUCT(&ompi_errclass_port, ompi_errclass_t);
    ompi_errclass_port.class = MPI_ERR_PORT;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_PORT, &ompi_errclass_port);

    OBJ_CONSTRUCT(&ompi_errclass_quota, ompi_errclass_t);
    ompi_errclass_quota.class = MPI_ERR_QUOTA;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_QUOTA, &ompi_errclass_quota);

    OBJ_CONSTRUCT(&ompi_errclass_read_only, ompi_errclass_t);
    ompi_errclass_read_only.class = MPI_ERR_READ_ONLY;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_READ_ONLY, &ompi_errclass_read_only);

    OBJ_CONSTRUCT(&ompi_errclass_rma_conflict, ompi_errclass_t);
    ompi_errclass_rma_conflict.class = MPI_ERR_RMA_CONFLICT;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_RMA_CONFLICT, &ompi_errclass_rma_conflict);

    OBJ_CONSTRUCT(&ompi_errclass_rma_sync, ompi_errclass_t);
    ompi_errclass_rma_sync.class = MPI_ERR_RMA_SYNC;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_RMA_SYNC, &ompi_errclass_rma_sync);

    OBJ_CONSTRUCT(&ompi_errclass_service, ompi_errclass_t);
    ompi_errclass_service.class = MPI_ERR_SERVICE;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_SERVICE, &ompi_errclass_service);

    OBJ_CONSTRUCT(&ompi_errclass_size, ompi_errclass_t);
    ompi_errclass_size.class = MPI_ERR_SIZE;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_SIZE, &ompi_errclass_size);

    OBJ_CONSTRUCT(&ompi_errclass_spawn, ompi_errclass_t);
    ompi_errclass_spawn.class = MPI_ERR_SPAWN;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_SPAWN, &ompi_errclass_spawn);

    OBJ_CONSTRUCT(&ompi_errclass_unsupported_datarep, ompi_errclass_t);
    ompi_errclass_unsupported_datarep.class = MPI_ERR_UNSUPPORTED_DATAREP;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_UNSUPPORTED_DATAREP, 
                                &ompi_errclass_unsupported_datarep);

    OBJ_CONSTRUCT(&ompi_errclass_unsupported_operation, ompi_errclass_t);
    ompi_errclass_unsupported_operation.class = MPI_ERR_UNSUPPORTED_OPERATION;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_UNSUPPORTED_OPERATION, 
                                &ompi_errclass_unsupported_operation);
    
    OBJ_CONSTRUCT(&ompi_errclass_win, ompi_errclass_t);
    ompi_errclass_win.class = MPI_ERR_WIN;
    ompi_pointer_array_set_item(&ompi_errclasses, MPI_ERR_WIN, &ompi_errclass_win);
    
    ompi_errclass_lastused=MPI_ERR_WIN+1;
    ompi_errclass_lastpredefined=MPI_ERR_WIN+1;
    return OMPI_SUCCESS;
}

int ompi_errclass_finalize(void)
{
    int i;
    ompi_errclass_t *errc;

    for (i=ompi_errclass_lastpredefined; i < ompi_errclass_lastused; i++) {
        /* 
         * Release user defined error classes
         */
        errc = ompi_pointer_array_get_item(&ompi_errclasses, i);
        OBJ_RELEASE (errc);
    }
    
    OBJ_DESTRUCT(&ompi_errclass_success);
    OBJ_DESTRUCT(&ompi_errclass_buffer);
    OBJ_DESTRUCT(&ompi_errclass_count);
    OBJ_DESTRUCT(&ompi_errclass_type);
    OBJ_DESTRUCT(&ompi_errclass_tag);
    OBJ_DESTRUCT(&ompi_errclass_comm);
    OBJ_DESTRUCT(&ompi_errclass_rank);
    OBJ_DESTRUCT(&ompi_errclass_request);
    OBJ_DESTRUCT(&ompi_errclass_root);
    OBJ_DESTRUCT(&ompi_errclass_group);
    OBJ_DESTRUCT(&ompi_errclass_op);
    OBJ_DESTRUCT(&ompi_errclass_topology);
    OBJ_DESTRUCT(&ompi_errclass_dims);
    OBJ_DESTRUCT(&ompi_errclass_arg);
    OBJ_DESTRUCT(&ompi_errclass_unknown);
    OBJ_DESTRUCT(&ompi_errclass_truncate);
    OBJ_DESTRUCT(&ompi_errclass_other);
    OBJ_DESTRUCT(&ompi_errclass_intern);
    OBJ_DESTRUCT(&ompi_errclass_in_status);
    OBJ_DESTRUCT(&ompi_errclass_pending);
    OBJ_DESTRUCT(&ompi_errclass_access);
    OBJ_DESTRUCT(&ompi_errclass_amode);
    OBJ_DESTRUCT(&ompi_errclass_assert);
    OBJ_DESTRUCT(&ompi_errclass_bad_file);
    OBJ_DESTRUCT(&ompi_errclass_base);
    OBJ_DESTRUCT(&ompi_errclass_conversion);
    OBJ_DESTRUCT(&ompi_errclass_disp);
    OBJ_DESTRUCT(&ompi_errclass_dup_datarep);
    OBJ_DESTRUCT(&ompi_errclass_file_exists);
    OBJ_DESTRUCT(&ompi_errclass_file_in_use);
    OBJ_DESTRUCT(&ompi_errclass_file);
    OBJ_DESTRUCT(&ompi_errclass_info_key);
    OBJ_DESTRUCT(&ompi_errclass_info_nokey);
    OBJ_DESTRUCT(&ompi_errclass_info_value);
    OBJ_DESTRUCT(&ompi_errclass_info);
    OBJ_DESTRUCT(&ompi_errclass_io);
    OBJ_DESTRUCT(&ompi_errclass_keyval);
    OBJ_DESTRUCT(&ompi_errclass_locktype);
    OBJ_DESTRUCT(&ompi_errclass_name);
    OBJ_DESTRUCT(&ompi_errclass_no_mem);
    OBJ_DESTRUCT(&ompi_errclass_not_same);
    OBJ_DESTRUCT(&ompi_errclass_no_space);
    OBJ_DESTRUCT(&ompi_errclass_no_such_file);
    OBJ_DESTRUCT(&ompi_errclass_port);
    OBJ_DESTRUCT(&ompi_errclass_quota);
    OBJ_DESTRUCT(&ompi_errclass_read_only);
    OBJ_DESTRUCT(&ompi_errclass_rma_conflict);
    OBJ_DESTRUCT(&ompi_errclass_rma_sync);
    OBJ_DESTRUCT(&ompi_errclass_service);
    OBJ_DESTRUCT(&ompi_errclass_size);
    OBJ_DESTRUCT(&ompi_errclass_spawn);
    OBJ_DESTRUCT(&ompi_errclass_unsupported_datarep);
    OBJ_DESTRUCT(&ompi_errclass_unsupported_operation);
    OBJ_DESTRUCT(&ompi_errclass_win);

    OBJ_DESTRUCT(&ompi_errclasses);
    return OMPI_SUCCESS;
}

int ompi_errclass_add(void)
{
    ompi_errclass_t *newerrclass;

    newerrclass = OBJ_NEW(ompi_errclass_t);
    newerrclass->class = ompi_errclass_lastused;
    ompi_errclass_lastused++;
    
    ompi_pointer_array_set_item(&ompi_errclasses, newerrclass->class, newerrclass);
    return OMPI_SUCCESS;
}

static void ompi_errclass_construct(ompi_errclass_t *errclass)
{
    errclass->class = MPI_UNDEFINED;
    return;
}

static void ompi_errclass_destruct(ompi_errclass_t *errclass)
{
    ompi_pointer_array_set_item(&ompi_errclasses, errclass->class, NULL);
    return;
}
