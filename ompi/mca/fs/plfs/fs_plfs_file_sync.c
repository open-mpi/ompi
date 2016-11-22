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
#include "fs_plfs.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/fs.h"

/*
 *	file_sync_plfs
 *
 *	Function:	- closes a new file
 *	Accepts:	- file handle
 *	Returns:	- Success if file closed
 */
int
mca_fs_plfs_file_sync (mca_io_ompio_file_t *fh)
{
    plfs_error_t plfs_ret;
    plfs_ret = plfs_sync( fh->f_fs_ptr );
    if (PLFS_SUCCESS != plfs_ret) {
        opal_output(0, "fs_plfs_file_sync: Error in plfs_sync:\n%s\n", strplfserr(plfs_ret));
	return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}
