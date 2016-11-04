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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"

#include <sys/ioctl.h>

/*
 *	file_open_plfs
 *
 *	Function:	- opens a new file
 *	Accepts:	- same arguments as MPI_File_open()
 *	Returns:	- Success if new file handle
 */
int
mca_fs_plfs_file_open (struct ompi_communicator_t *comm,
                     const char* filename,
                     int access_mode,
                     struct ompi_info_t *info,
                     mca_io_ompio_file_t *fh)
{
    int rank;
    int amode;
    int old_mask, perm;
    plfs_error_t plfs_ret;
    Plfs_fd *pfd = NULL;
    char wpath[1024];

    rank = ompi_comm_rank ( comm );

    getcwd( wpath, sizeof(wpath) );
    sprintf( wpath,"%s/%s",wpath,filename );

    if (OMPIO_PERM_NULL == fh->f_perm) {
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
    if (access_mode & MPI_MODE_EXCL) {
        if( is_plfs_path(wpath) == 1 ) { //the file already exists
	    return OMPI_ERROR;
	}
    }

    if (0 == rank) {
        /* MODE_CREATE and MODE_EXCL can only be set by one process */
        if (access_mode & MPI_MODE_CREATE)
	    amode = amode | O_CREAT;

	plfs_ret = plfs_open( &pfd, wpath, amode, fh->f_rank, perm, NULL );
	fh->f_fs_ptr = pfd;
    }

    comm->c_coll.coll_bcast ( &plfs_ret, 1, MPI_INT, 0, comm, comm->c_coll.coll_bcast_module);
    if ( PLFS_SUCCESS != plfs_ret ) {
        return OMPI_ERROR;
    }

    if (0 != rank) {
        plfs_ret = plfs_open( &pfd, wpath, amode, fh->f_rank, perm, NULL );
	if (PLFS_SUCCESS != plfs_ret) {
	    opal_output(0, "fs_plfs_file_open: Error in plfs_open:\n%s\n", strplfserr(plfs_ret));
	    return OMPI_ERROR;
	}
	else {
	    fh->f_fs_ptr = pfd;
	}
    }

    return OMPI_SUCCESS;
}
