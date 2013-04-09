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
 * Copyright (c) 2008-2011 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "fbtl_posix.h"

#include "mpi.h"
#include <unistd.h>
#include <sys/uio.h>
#include <aio.h>
#include "ompi/constants.h"
#include "ompi/mca/fbtl/fbtl.h"

size_t 
mca_fbtl_posix_ipwritev (mca_io_ompio_file_t *fh,
                         int *sorted,
                         ompi_request_t **request)
{
    int i;
    int num_req = 0;
    int merge = 0;
    size_t k;
    char *merge_buf = NULL;
    size_t merge_length = 0;
    OMPI_MPI_OFFSET_TYPE merge_offset = 0;
    struct aiocb *aiocbp;

    aiocbp = (struct aiocb *) malloc (sizeof(struct aiocb) * 
                                      fh->f_num_of_io_entries);
    if (NULL == aiocbp) {
        opal_output(1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (NULL != sorted) {
        for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
            if (fh->f_num_of_io_entries != i+1) {
	      if (((OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[sorted[i]].offset + 
                     (OPAL_PTRDIFF_TYPE)fh->f_io_array[sorted[i]].length) == 
		  (OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[sorted[i+1]].offset) {
                    if (!merge) {
		      merge_offset = (OMPI_MPI_OFFSET_TYPE)(intptr_t)
                            fh->f_io_array[sorted[i]].offset;
                        merge_length = fh->f_io_array[sorted[i]].length;
                    }
                    merge_length += fh->f_io_array[sorted[i+1]].length;
                    merge++;
                    continue;
                }
            }

            if (merge) {
                merge_buf = malloc (merge_length);
                if (NULL == merge_buf) {
                    opal_output(1, "OUT OF MEMORY\n");
                    return OMPI_ERR_OUT_OF_RESOURCE;
                }
                k = 0;
                while (merge >= 0) {
                    memcpy (merge_buf + k, 
                            fh->f_io_array[sorted[i-merge]].memory_address,
                            fh->f_io_array[sorted[i-merge]].length);
                    k += fh->f_io_array[sorted[i-merge]].length;
                    merge --;
                }

                aiocbp[num_req].aio_offset  = merge_offset;
                aiocbp[num_req].aio_buf     = merge_buf;
                aiocbp[num_req].aio_nbytes  = merge_length;
                aiocbp[num_req].aio_fildes  = fh->fd;
                aiocbp[num_req].aio_reqprio = 0;
                aiocbp[num_req].aio_sigevent.sigev_notify = SIGEV_NONE;

                if (-1 == aio_write(&aiocbp[num_req])) {
                    perror("aio_write() error");
                    return OMPI_ERROR;
                }
                merge = 0;
                merge_offset = 0;
                merge_length = 0;
                if (NULL != merge_buf) {
                    free (merge_buf);
                    merge_buf = NULL;
                }
            }
            else {
	      aiocbp[num_req].aio_offset  = (OMPI_MPI_OFFSET_TYPE)(intptr_t)
                    fh->f_io_array[sorted[i]].offset;
                aiocbp[num_req].aio_buf     = 
                    fh->f_io_array[sorted[i]].memory_address;
                aiocbp[num_req].aio_nbytes  = fh->f_io_array[sorted[i]].length;
                aiocbp[num_req].aio_fildes  = fh->fd;
                aiocbp[num_req].aio_reqprio = 0;
                aiocbp[num_req].aio_sigevent.sigev_notify = SIGEV_NONE;

                if (-1 == aio_write(&aiocbp[num_req])) {
                    perror("aio_write() error");
                    return OMPI_ERROR;
                }
            }
            num_req ++;
        }
    }

    else {
        for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
            if (fh->f_num_of_io_entries != i+1) {
	      if (((OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i].offset + 
                     (OPAL_PTRDIFF_TYPE)fh->f_io_array[i].length) == 
		  (OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i+1].offset) {
                    if (!merge) {
		      merge_offset = (OMPI_MPI_OFFSET_TYPE)(intptr_t)
                            fh->f_io_array[i].offset;
                        merge_length = fh->f_io_array[i].length;
                    }
                    merge_length += fh->f_io_array[i+1].length;
                    merge++;
                    continue;
                }
            }

            if (merge) {
                merge_buf = malloc (merge_length);
                if (NULL == merge_buf) {
                    opal_output(1, "OUT OF MEMORY\n");
                    return OMPI_ERR_OUT_OF_RESOURCE;
                }
                k = 0;
                while (merge >= 0) {
                    memcpy (merge_buf + k, 
                            fh->f_io_array[i-merge].memory_address,
                            fh->f_io_array[i-merge].length);
                    k += fh->f_io_array[i-merge].length;
                    merge --;
                }
                aiocbp[num_req].aio_offset  = merge_offset;
                aiocbp[num_req].aio_buf     = merge_buf;
                aiocbp[num_req].aio_nbytes  = merge_length;
                aiocbp[num_req].aio_fildes  = fh->fd;
                aiocbp[num_req].aio_reqprio = 0;
                aiocbp[num_req].aio_sigevent.sigev_notify = SIGEV_NONE;

                if (-1 == aio_write(&aiocbp[num_req])) {
                    perror("aio_write() error");
                    return OMPI_ERROR;
                }
                merge = 0;
                merge_offset = 0;
                merge_length = 0;
                if (NULL != merge_buf) {
                    free (merge_buf);
                    merge_buf = NULL;
                }
            }
            else {
	      aiocbp[num_req].aio_offset  = (OMPI_MPI_OFFSET_TYPE)(intptr_t)
                    fh->f_io_array[i].offset;
                aiocbp[num_req].aio_buf     = fh->f_io_array[i].memory_address;
                aiocbp[num_req].aio_nbytes  = fh->f_io_array[i].length;
                aiocbp[num_req].aio_fildes  = fh->fd;
                aiocbp[num_req].aio_reqprio = 0;
                aiocbp[num_req].aio_sigevent.sigev_notify = SIGEV_NONE;

                if (-1 == aio_write(&aiocbp[num_req])) {
                    perror("aio_write() error");
                    return OMPI_ERROR;
                }
            }
            num_req ++;
        }
    }
    /*
    ompi_grequest_start (mca_fbtl_aio_query_fn,
                         mca_fbtl_aio_free_fn, 
                         mca_fbtl_aio_cancel_fn,
                         mca_fbtl_aio_poll_fn, 
                         mca_fbtl_aio_wait_fn,
                         request);
    */
    return OMPI_SUCCESS;
}
