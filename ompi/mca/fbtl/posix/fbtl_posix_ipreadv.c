/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2021 University of Houston. All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2022      IBM Corporation. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include "fbtl_posix.h"

#include <unistd.h>
#include <sys/uio.h>
#if HAVE_AIO_H
#include <aio.h>
#endif

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fbtl/fbtl.h"

#define MAX_ATTEMPTS 10

ssize_t mca_fbtl_posix_ipreadv (ompio_file_t *fh,
                               ompi_request_t *request)
{
#if defined (FBTL_POSIX_HAVE_AIO)
    mca_fbtl_posix_request_data_t *data;
    mca_ompio_request_t *req = (mca_ompio_request_t *) request;
    int i=0, ret;
    off_t start_offset, end_offset, total_length;

    data = (mca_fbtl_posix_request_data_t *) malloc ( sizeof (mca_fbtl_posix_request_data_t));
    if ( NULL == data ) {
        opal_output (1,"mca_fbtl_posix_ipreadv: could not allocate memory\n");
        return 0;
    }

    data->prd_req_count = fh->f_num_of_io_entries;
    data->prd_open_reqs = fh->f_num_of_io_entries;
    data->prd_req_type  = FBTL_POSIX_AIO_READ;
    data->prd_req_chunks = ompi_fbtl_posix_max_prd_active_reqs;
    data->prd_total_len = 0;
    data->prd_aio.aio_reqs = (struct aiocb *) malloc (sizeof(struct aiocb) *
                                              fh->f_num_of_io_entries);
    if (NULL == data->prd_aio.aio_reqs) {
        opal_output(1, "mca_fbtl_posix_ipreadv: could not allocate memory\n");
        free(data);
        return 0;
    }

    data->prd_aio.aio_req_status = (int *) malloc (sizeof(int) * fh->f_num_of_io_entries);
    if (NULL == data->prd_aio.aio_req_status) {
        opal_output(1, "mca_fbtl_posix_ipreadv: could not allocate memory\n");
        free(data->prd_aio.aio_reqs);
        free(data);
        return 0;
    }
    data->prd_lock_counter = 0;
    data->prd_fh = fh;

    if ( fh->f_atomicity ) {
        OMPIO_SET_ATOMICITY_LOCK(fh, data->prd_lock, data->prd_lock_counter, F_RDLCK);
    }
    
    for ( i=0; i<fh->f_num_of_io_entries; i++ ) {
        data->prd_aio.aio_reqs[i].aio_offset  = (OMPI_MPI_OFFSET_TYPE)(intptr_t)
            fh->f_io_array[i].offset;
        data->prd_aio.aio_reqs[i].aio_buf     = fh->f_io_array[i].memory_address;
        data->prd_aio.aio_reqs[i].aio_nbytes  = fh->f_io_array[i].length;
        data->prd_aio.aio_reqs[i].aio_fildes  = fh->fd;
        data->prd_aio.aio_reqs[i].aio_reqprio = 0;
        data->prd_aio.aio_reqs[i].aio_sigevent.sigev_notify = SIGEV_NONE;
        data->prd_aio.aio_req_status[i]        = EINPROGRESS;
    }

    data->prd_first_active_req = 0;
    if ( data->prd_req_count > data->prd_req_chunks ) {
        data->prd_last_active_req = data->prd_req_chunks;
    }
    else {
        data->prd_last_active_req = data->prd_req_count;
    }

    start_offset = data->prd_aio.aio_reqs[data->prd_first_active_req].aio_offset;
    end_offset   = data->prd_aio.aio_reqs[data->prd_last_active_req-1].aio_offset +
                   data->prd_aio.aio_reqs[data->prd_last_active_req-1].aio_nbytes;
    total_length = (end_offset - start_offset);
    ret = mca_fbtl_posix_lock( &data->prd_lock, data->prd_fh, F_RDLCK, start_offset, total_length,
                               OMPIO_LOCK_ENTIRE_REGION, &data->prd_lock_counter );
    if ( 0 < ret ) {
        opal_output(1, "mca_fbtl_posix_ipreadv: error in mca_fbtl_posix_lock() error ret=%d  %s", ret, strerror(errno));
        mca_fbtl_posix_unlock ( &data->prd_lock, data->prd_fh, &data->prd_lock_counter);
        free(data->prd_aio.aio_reqs);
        free(data->prd_aio.aio_req_status);
        free(data);
        return OMPI_ERROR;
    }

    for (i=0; i < data->prd_last_active_req; i++) {
        int counter=0;
        while ( MAX_ATTEMPTS > counter ) { 
            if  ( -1 != aio_read(&data->prd_aio.aio_reqs[i]) ) {
                break;
            }
            counter++;
            mca_common_ompio_progress();
        }
        if ( MAX_ATTEMPTS == counter ) {
           opal_output(1, "mca_fbtl_posix_ipreadv: error in aio_read(): errno %d %s", errno, strerror(errno));
           mca_fbtl_posix_unlock ( &data->prd_lock, data->prd_fh, &data->prd_lock_counter);
           free(data->prd_aio.aio_reqs);
           free(data->prd_aio.aio_req_status);
           free(data);
           return OMPI_ERROR;
        }
    }

    req->req_data = data;
    req->req_progress_fn = mca_fbtl_posix_progress;
    req->req_free_fn     = mca_fbtl_posix_request_free;
#endif
    return OMPI_SUCCESS;
}
