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

#include <fcntl.h>
#include <unistd.h>
#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/fs.h"


/*
 *	file_close_plfs
 *
 *	Function:	- closes a new file
 *	Accepts:	- file handle
 *	Returns:	- Success if file closed
 */
int
mca_fs_plfs_file_close (mca_io_ompio_file_t *fh)
{
    int flags;
    plfs_error_t plfs_ret = PLFS_SUCCESS;
    int amode;
    char wpath[1024];

    fh->f_comm->c_coll.coll_barrier (fh->f_comm,
                                     fh->f_comm->c_coll.coll_barrier_module);

    getcwd( wpath, sizeof(wpath) );
    sprintf( wpath,"%s/%s",wpath,fh->f_filename );

    plfs_ret = plfs_access(wpath, F_OK); 
    if ( PLFS_SUCCESS != plfs_ret ) {
        opal_output(0, "fs_plfs_file_close: Error in plfs_access:\n%s\n", strplfserr(plfs_ret));
        return OMPI_ERROR; // file doesn't exist
    }

    amode = 0;
    if (fh->f_amode & MPI_MODE_CREATE)
        amode = amode | O_CREAT;
    if (fh->f_amode & MPI_MODE_RDONLY)
        amode = amode | O_RDONLY;
    if (fh->f_amode & MPI_MODE_WRONLY)
        amode = amode | O_WRONLY;
    if (fh->f_amode & MPI_MODE_RDWR)
        amode = amode | O_RDWR;
    if (fh->f_amode & MPI_MODE_EXCL) {
        return OMPI_ERROR;
    }

    plfs_ret = plfs_sync(fh->f_fs_ptr);
    if (PLFS_SUCCESS != plfs_ret) {
        opal_output(0, "fs_plfs_file_close: Error in plfs_sync:\n%s\n", strplfserr(plfs_ret));
        return OMPI_ERROR;
    }

    
    plfs_ret = plfs_close(fh->f_fs_ptr, fh->f_rank, 0, amode ,NULL, &flags);
    if (PLFS_SUCCESS != plfs_ret) {
        opal_output(0, "fs_plfs_file_close: Error in plfs_close:\n%s\n", strplfserr(plfs_ret));
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}
