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

/* This code is based on the PVFS2 ADIO module in ROMIO
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ompi_config.h"
#include "fs_pvfs2.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/fs.h"

/*
 *	file_sync_pvfs2
 *
 *	Function:	- closes a new file
 *	Accepts:	- file handle
 *	Returns:	- Success if file closed
 */
int
mca_fs_pvfs2_file_sync (mca_io_ompio_file_t *fh)
{
    int ret;
    mca_fs_pvfs2 *pvfs2_fs;

    ret = OMPI_SUCCESS;

    pvfs2_fs = (mca_fs_pvfs2 *)fh->f_fs_ptr;

    if (OMPIO_ROOT == fh->f_rank) {
        ret = PVFS_sys_flush(pvfs2_fs->object_ref, &(pvfs2_fs->credentials));
    }

    fh->f_comm->c_coll.coll_bcast (&ret,
                                   1,
                                   MPI_INT,
                                   OMPIO_ROOT,
                                   fh->f_comm,
                                   fh->f_comm->c_coll.coll_bcast_module);

    if (0 != ret) {
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}
