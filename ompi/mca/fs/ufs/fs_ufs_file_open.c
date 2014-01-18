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
#include "fs_ufs.h"

#include <fcntl.h>
#include <sys/stat.h>
#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"

/*
 *	file_open_ufs
 *
 *	Function:	- opens a new file
 *	Accepts:	- same arguments as MPI_File_open()
 *	Returns:	- Success if new file handle
 */
int
mca_fs_ufs_file_open (struct ompi_communicator_t *comm, 
		      char* filename,
		      int access_mode,
		      struct ompi_info_t *info,
		      mca_io_ompio_file_t *fh)
{
    int amode;
    int old_mask, perm;
    int rank, ret;

    rank = ompi_comm_rank ( comm );

    if (fh->f_perm == OMPIO_PERM_NULL)  {
        old_mask = umask(022);
        umask(old_mask);
        perm = old_mask ^ 0666;
    }
    else {
        perm = fh->f_perm;
    }

    amode = 0;

    if (access_mode & MPI_MODE_RDONLY)
        amode = amode | O_RDONLY;
    if (access_mode & MPI_MODE_WRONLY)
        amode = amode | O_WRONLY;
    if (access_mode & MPI_MODE_RDWR)
        amode = amode | O_RDWR;
    
    if ( 0 == rank ) {
	/* MODE_CREATE and MODE_EXCL can only be set by one process */
	if ( access_mode & MPI_MODE_CREATE )
	    amode = amode | O_CREAT;
	if (access_mode & MPI_MODE_EXCL)
	    amode = amode | O_EXCL;
	fh->fd = open (filename, amode, perm);
	ret = fh->fd;
    }

    comm->c_coll.coll_bcast ( &ret, 1, MPI_INT, 0, comm, comm->c_coll.coll_bcast_module);
    if ( -1 == ret ) {
	fh->fd = ret;
	return OMPI_ERROR;
    }
    if ( 0 != rank ) {
	fh->fd = open (filename, amode, perm);
	if (-1 == fh->fd) {
	    return OMPI_ERROR;
	}
    }

    return OMPI_SUCCESS;
}
