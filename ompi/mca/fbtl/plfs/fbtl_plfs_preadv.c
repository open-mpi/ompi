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

    Plfs_fd *pfd = fh->f_fs_ptr;
    plfs_error_t plfs_ret;
    ssize_t total_bytes_read=0;
    int i;
    ssize_t bytes_read;

    if (NULL == fh->f_io_array) {
        return OMPI_ERROR;
    }

    for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
	plfs_ret = plfs_read( pfd, fh->f_io_array[i].memory_address, fh->f_io_array[i].length,
                              (off_t )fh->f_io_array[i].offset, &bytes_read );
	if (PLFS_SUCCESS != plfs_ret) {
	    opal_output(0, "fbtl_plfs_preadv: Error in plfs_read:\n%s\n", strplfserr(plfs_ret));
	    return OMPI_ERROR;
	}

	if (bytes_read < 0)
	    return OMPI_ERROR;
	total_bytes_read += bytes_read;
    }

    return total_bytes_read;
}
