/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2014 University of Houston. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
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
#include "ompi/mca/fs/base/base.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"
#include "opal/util/path.h"

/*
 *	file_open_ufs
 *
 *	Function:	- opens a new file
 *	Accepts:	- same arguments as MPI_File_open()
 *	Returns:	- Success if new file handle
 */
int
mca_fs_ufs_file_open (struct ompi_communicator_t *comm,
		      const char* filename,
		      int access_mode,
		      struct opal_info_t *info,
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
        if ( !(fh->f_flags & OMPIO_SHAREDFP_IS_SET)) {
            if ( access_mode & MPI_MODE_CREATE )
                amode = amode | O_CREAT;
            if (access_mode & MPI_MODE_EXCL)
                amode = amode | O_EXCL;
        }
	fh->fd = open (filename, amode, perm);
	ret = fh->fd;
    }

    comm->c_coll->coll_bcast ( &ret, 1, MPI_INT, 0, comm, comm->c_coll->coll_bcast_module);
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

    fh->f_stripe_size=0;
    fh->f_stripe_count=1;
    /* Need to find a way to determine the file system block size at run time.
       4096 is the most common value, but it might not always be accurate.
    */
    fh->f_fs_block_size = 4096;

    /* Need to check for NFS here. If the file system is not NFS but a regular UFS file system,
       we do not need to enforce locking. A regular XFS or EXT4 file system can only be used 
       within a single node, local environment, and in this case the OS will already ensure correct
       handling of file system blocks;
    */
       
    char *fstype=NULL;
    bool bret = opal_path_nfs ( (char *)filename, &fstype );

    if ( false == bret ) {
        char *dir;
        mca_fs_base_get_parent_dir ( (char *)filename, &dir );
        bret = opal_path_nfs (dir, &fstype);
        free(dir);
    }
    
    if ( true == bret ) {
        if ( 0 == strncasecmp(fstype, "nfs", sizeof("nfs")) ) {
            /* Nothing really to be done in this case. Locking can stay */
        }
        else {
            fh->f_flags |= OMPIO_LOCK_NEVER;
        }
    }
    else {
            fh->f_flags |= OMPIO_LOCK_NEVER;
    }
    free (fstype);

    return OMPI_SUCCESS;
}
