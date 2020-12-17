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
 * Copyright (c) 2008-2018 University of Houston. All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
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
		      ompio_file_t *fh)
{
    int amode, perm;
    int ret=OMPI_SUCCESS;

    perm = mca_fs_base_get_file_perm(fh);
    amode = mca_fs_base_get_file_amode(fh->f_rank, access_mode);

    /* Reset errno */
    errno = 0;
    if (OMPIO_ROOT == fh->f_rank) {
	   fh->fd = open (filename, amode, perm);
        if ( 0 > fh->fd ) {
            ret = mca_fs_base_get_mpi_err(errno);
        }
    }

    comm->c_coll->coll_bcast ( &ret, 1, MPI_INT, 0, comm, comm->c_coll->coll_bcast_module);
    if ( OMPI_SUCCESS != ret ) {
        fh->fd = -1;
        return ret;
    }

    if (OMPIO_ROOT != fh->f_rank) {
        fh->fd = open (filename, amode, perm);
        if ( 0 > fh->fd) {
            return mca_fs_base_get_mpi_err(errno);
        }
    }

    fh->f_stripe_size=0;
    fh->f_stripe_count=1;

    /* Need to check for NFS here. If the file system is not NFS but a regular UFS file system,
       we do not need to enforce locking. A regular XFS or EXT4 file system can only be used 
       within a single node, local environment, and in this case the OS will already ensure correct
       handling of file system blocks;
    */

    if ( FS_UFS_LOCK_AUTO == mca_fs_ufs_lock_algorithm ) {       
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
                /* Based on my tests, only locking the entire file for all operations
                   guarantueed for the entire teststuite to pass correctly. I would not
                   be surprised, if depending on the NFS configuration that might not always
                   be necessary, and the user can change that with an MCA parameter of this 
                   component. */
                fh->f_flags |= OMPIO_LOCK_ENTIRE_FILE;
            }
            else {
                fh->f_flags |= OMPIO_LOCK_NEVER;
            }
        }
        else {
            fh->f_flags |= OMPIO_LOCK_NEVER;
        }
        free (fstype);
    }
    else if ( FS_UFS_LOCK_NEVER == mca_fs_ufs_lock_algorithm ) {
        fh->f_flags |= OMPIO_LOCK_NEVER;
    }
    else if ( FS_UFS_LOCK_ENTIRE_FILE == mca_fs_ufs_lock_algorithm ) {
        fh->f_flags |= OMPIO_LOCK_ENTIRE_FILE;
    }
    else if ( FS_UFS_LOCK_RANGES == mca_fs_ufs_lock_algorithm ) {
        /* Nothing to be done. This is what the posix fbtl component would do
           anyway without additional information . */
    }
    else {
        opal_output ( 1, "Invalid value for mca_fs_ufs_lock_algorithm %d", mca_fs_ufs_lock_algorithm );
    }

    return OMPI_SUCCESS;
}
