/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil ; -*- 
 *     vim: ts=8 sts=4 sw=4 noexpandtab 
 * 
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "adio.h"
#include "adio_extern.h"
#include "ad_pvfs2.h"
#include <string.h>

#include "ad_pvfs2_common.h"
#include "mpiu_greq.h"
#include "../../mpi-io/mpioimpl.h"

#define READ 0
#define WRITE 1

static int ADIOI_PVFS2_greq_class = 0;
int ADIOI_PVFS2_aio_free_fn(void *extra_state);
int ADIOI_PVFS2_aio_poll_fn(void *extra_state, MPI_Status *status);
int ADIOI_PVFS2_aio_wait_fn(int count, void ** array_of_states, 
		double timeout, MPI_Status *status);

void ADIOI_PVFS2_IReadContig(ADIO_File fd, void *buf, int count, 
			    MPI_Datatype datatype, int file_ptr_type,
			    ADIO_Offset offset, MPI_Request *request,
			    int *error_code)
{
    ADIOI_PVFS2_AIO_contig(fd, buf, count, datatype, file_ptr_type,
	    offset, request, READ, error_code);
}

void ADIOI_PVFS2_IWriteContig(ADIO_File fd, const void *buf, int count,
			    MPI_Datatype datatype, int file_ptr_type,
			    ADIO_Offset offset, MPI_Request *request,
			    int *error_code)
{
    ADIOI_PVFS2_AIO_contig(fd, (void *)buf, count, datatype, file_ptr_type,
	    offset, request, WRITE, error_code);
}

void ADIOI_PVFS2_AIO_contig(ADIO_File fd, void *buf, int count, 
			    MPI_Datatype datatype, int file_ptr_type,
			    ADIO_Offset offset, MPI_Request *request,
			    int flag, int *error_code)
{

    int ret;
    MPI_Count datatype_size, len;
    ADIOI_PVFS2_fs *pvfs_fs;
    ADIOI_AIO_Request *aio_req;
    static char myname[] = "ADIOI_PVFS2_AIO_contig";

    pvfs_fs = (ADIOI_PVFS2_fs*)fd->fs_ptr;

    aio_req = (ADIOI_AIO_Request*)ADIOI_Calloc(sizeof(ADIOI_AIO_Request), 1);

    MPI_Type_size_x(datatype, &datatype_size);
    len = datatype_size * count;

    ret = PVFS_Request_contiguous(len, PVFS_BYTE, &(aio_req->mem_req));
    /* --BEGIN ERROR HANDLING-- */
    if (ret != 0) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   ADIOI_PVFS2_error_convert(ret),
					   "Error in pvfs_request_contig (memory)", 0);
	return;
    }
    /* --END ERROR HANDLING-- */

    ret = PVFS_Request_contiguous(len, PVFS_BYTE, &(aio_req->file_req));
    /* --BEGIN ERROR HANDLING-- */
    if (ret != 0) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   ADIOI_PVFS2_error_convert(ret),
					   "Error in pvfs_request_contig (file)", 0);
	return;
    }
    /* --END ERROR HANDLING-- */

    if (file_ptr_type == ADIO_INDIVIDUAL) {
	/* copy individual file pointer into offset variable, continue */
	offset = fd->fp_ind;
    } 
    if (flag == READ) {
#ifdef ADIOI_MPE_LOGGING
	MPE_Log_event( ADIOI_MPE_iread_a, 0, NULL );
#endif
	ret = PVFS_isys_read(pvfs_fs->object_ref, aio_req->file_req, offset, 
		buf, aio_req->mem_req, &(pvfs_fs->credentials), 
		&(aio_req->resp_io), &(aio_req->op_id), NULL);
#ifdef ADIOI_MPE_LOGGING
	MPE_Log_event( ADIOI_MPE_iread_b, 0, NULL );
#endif
    } else if (flag == WRITE) {
#ifdef ADIOI_MPE_LOGGING
	MPE_Log_event( ADIOI_MPE_iwrite_a, 0, NULL );
#endif
	ret = PVFS_isys_write(pvfs_fs->object_ref, aio_req->file_req, offset, 
		buf, aio_req->mem_req, &(pvfs_fs->credentials), 
		&(aio_req->resp_io), &(aio_req->op_id), NULL);
#ifdef ADIOI_MPE_LOGGING
	MPE_Log_event( ADIOI_MPE_iwrite_b, 0, NULL );
#endif 
    } 

    /* --BEGIN ERROR HANDLING-- */
    if (ret < 0 ) {
	*error_code = MPIO_Err_create_code(MPI_SUCCESS,
					   MPIR_ERR_RECOVERABLE,
					   myname, __LINE__,
					   ADIOI_PVFS2_error_convert(ret),
					   "Error in PVFS_isys_io", 0);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */

    /* posted. defered completion */
    if (ret == 0) { 
	if (ADIOI_PVFS2_greq_class == 0) {
	    MPIX_Grequest_class_create(ADIOI_GEN_aio_query_fn, 
		    ADIOI_PVFS2_aio_free_fn, MPIU_Greq_cancel_fn,
		    ADIOI_PVFS2_aio_poll_fn, ADIOI_PVFS2_aio_wait_fn,
		    &ADIOI_PVFS2_greq_class);
	}
	MPIX_Grequest_class_allocate(ADIOI_PVFS2_greq_class, aio_req, request);
	memcpy(&(aio_req->req), request, sizeof(*request));
    }

    /* immediate completion */
    if (ret == 1) {
	MPIO_Completed_request_create(&fd, len, error_code, request);
    }

    if (file_ptr_type == ADIO_INDIVIDUAL) {
	fd->fp_ind += len;
    }
    fd->fp_sys_posn = offset + len;

    *error_code = MPI_SUCCESS;
fn_exit:
    return;
}

int ADIOI_PVFS2_aio_free_fn(void *extra_state)
{
    ADIOI_AIO_Request *aio_req;
    aio_req = (ADIOI_AIO_Request*)extra_state;

    PVFS_Request_free(&(aio_req->mem_req));
    PVFS_Request_free(&(aio_req->file_req));
    ADIOI_Free(aio_req);

    return MPI_SUCCESS;
}

int ADIOI_PVFS2_aio_poll_fn(void *extra_state, MPI_Status *status)
{
    ADIOI_AIO_Request *aio_req;
    int ret, error;

    aio_req = (ADIOI_AIO_Request *)extra_state;

    /* BUG: cannot PVFS_sys_testsome: does not work for a specific request */
    ret = PVFS_sys_wait(aio_req->op_id, "ADIOI_PVFS2_aio_poll_fn", &error);
    if (ret == 0) {
	aio_req->nbytes = aio_req->resp_io.total_completed;
	MPI_Grequest_complete(aio_req->req);
	return MPI_SUCCESS;
    } else
	return MPI_UNDEFINED; /* TODO: what's this error? */
}

/* wait for multiple requests to complete */
int ADIOI_PVFS2_aio_wait_fn(int count, void ** array_of_states, 
		double timeout, MPI_Status *status)
{

    ADIOI_AIO_Request **aio_reqlist;
    PVFS_sys_op_id *op_id_array;
    int i,j, greq_count, completed_count=0;
    int *error_array;

    aio_reqlist = (ADIOI_AIO_Request **)array_of_states;

    op_id_array = (PVFS_sys_op_id*)ADIOI_Calloc(count, sizeof(PVFS_sys_op_id));
    error_array = (int *)ADIOI_Calloc(count, sizeof(int));
    greq_count = count;


    /* PVFS-2.6: testsome actually tests all requests and fills in op_id_array
     * with the ones that have completed.  count is an in/out parameter.
     * returns with the number of completed operations.  what a mess! */
    while (completed_count < greq_count ) {
	count = greq_count;
	PVFS_sys_testsome(op_id_array, &count, NULL, error_array, INT_MAX);
	completed_count += count;
	for (i=0; i< count; i++) {
	    for (j=0; j<greq_count; j++) {
		if (op_id_array[i] == aio_reqlist[j]->op_id) {
		    aio_reqlist[j]->nbytes = 
			aio_reqlist[j]->resp_io.total_completed;
		    MPI_Grequest_complete(aio_reqlist[j]->req);
		}
	    }
	}
    }
    return MPI_SUCCESS; /* TODO: no idea how to deal with errors */
}


/*
 * vim: ts=8 sts=4 sw=4 noexpandtab 
 */
