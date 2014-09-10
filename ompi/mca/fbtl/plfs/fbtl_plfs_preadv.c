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
 * Copyright (c) 2008-2014 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "fbtl_plfs.h"

#include "mpi.h"
#include <unistd.h>
#include "ompi/constants.h"
#include "ompi/mca/fbtl/fbtl.h"

ssize_t  mca_fbtl_plfs_preadv (mca_io_ompio_file_t *fh )
{

    Plfs_fd *pfd = NULL;
    plfs_error_t plfs_ret;
    pfd = fh->f_fs_ptr;
    ssize_t total_bytes_read=0;

    int i, block=1;
    struct iovec *iov = NULL;
    int iov_count = 0;
    OMPI_MPI_OFFSET_TYPE iov_offset = 0;

    if (NULL == fh->f_io_array) {
        return OMPI_ERROR;
    }

    iov = (struct iovec *) malloc 
        (OMPIO_IOVEC_INITIAL_SIZE * sizeof (struct iovec));
    if (NULL == iov) {
        opal_output(1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
	if (0 == iov_count) {
	    iov[iov_count].iov_base = fh->f_io_array[i].memory_address;
	    iov[iov_count].iov_len = fh->f_io_array[i].length;
	    iov_offset = (OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i].offset;
	    iov_count ++;
	}
	
	if (OMPIO_IOVEC_INITIAL_SIZE*block <= iov_count) {
	    block ++;
	    iov = (struct iovec *)realloc
		(iov, OMPIO_IOVEC_INITIAL_SIZE * block *
		 sizeof(struct iovec));
	    if (NULL == iov) {
		opal_output(1, "OUT OF MEMORY\n");
		return OMPI_ERR_OUT_OF_RESOURCE;
	    }
	}
	
	if (fh->f_num_of_io_entries != i+1) {
	    if (((OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i].offset + 
		 (OPAL_PTRDIFF_TYPE)fh->f_io_array[i].length) == 
		(OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i+1].offset) {                    
		iov[iov_count].iov_base = 
		    fh->f_io_array[i+1].memory_address;
		iov[iov_count].iov_len = fh->f_io_array[i+1].length;
		iov_count ++;
		continue;
	    }
	}
	
	// Find the total number of bytes to be read.
	size_t bytes = 0;
	for (int i = 0; i < iov_count; ++i) {
	    bytes += iov[i].iov_len;
	}
	
	// Allocate a temporary buffer to hold the data
	char *buffer;
	buffer = (char *) malloc (bytes);
	if (buffer == NULL) {
	    return OMPI_ERROR;
	}
	
	// Read the data
	ssize_t bytes_read;
	plfs_ret = plfs_read( pfd, buffer, bytes, iov_offset, &bytes_read );
	if (PLFS_SUCCESS != plfs_ret) {
	    opal_output(0, "fbtl_plfs_preadv: Error in plfs_read:\n%s\n", strplfserr(plfs_ret));
	    return OMPI_ERROR;
	}
	
	if (bytes_read < 0)
	    return OMPI_ERROR;
	total_bytes_read += bytes_read;
	// Copy the data from BUFFER into the memory specified by IOV
	bytes = bytes_read;
	for (int i = 0; i < iov_count; ++i) {
	    size_t copy = MIN (iov[i].iov_len, bytes);
	    (void) memcpy ((void *) iov[i].iov_base, (void *) buffer, copy);
	    buffer += copy;
	    bytes -= copy;
	    if (bytes == 0) {
		break;
	    }
	}
	iov_count = 0;
	if ( NULL != buffer ) {
	    free (buffer);
	    buffer=NULL;
	}
    }

    if (NULL != iov) {
	free (iov);
	iov = NULL;
    }

    return total_bytes_read;
}
