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
 *	file_get_size_pvfs2
 *
 *	Function:	- get_size of a file
 *	Accepts:	- same arguments as MPI_File_get_size()
 *	Returns:	- Success if size is get
 */
int
mca_fs_pvfs2_file_get_size (mca_io_ompio_file_t *fh,
                            OMPI_MPI_OFFSET_TYPE *size)
{
    int ret;
    mca_fs_pvfs2 *pvfs2_fs;
    PVFS_sysresp_getattr resp_getattr;

    pvfs2_fs = (mca_fs_pvfs2 *)fh->f_fs_ptr;

    ret = PVFS_sys_getattr (pvfs2_fs->object_ref, PVFS_ATTR_SYS_SIZE,
                            &(pvfs2_fs->credentials), &resp_getattr);
    if (ret != 0 ) {
        return OMPI_ERROR;
    }

    *size = resp_getattr.attr.size;

    return OMPI_SUCCESS;
}
