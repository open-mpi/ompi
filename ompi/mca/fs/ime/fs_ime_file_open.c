/*
 * Copyright (c) 2018      DataDirect Networks. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ime_native.h"

#include "ompi_config.h"
#include "fs_ime.h"

#include <sys/stat.h>
#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"
#include "opal/util/path.h"

/*
 *	file_open_ime
 *
 *	Function:	- opens a new file
 *	Accepts:	- same arguments as MPI_File_open()
 *	Returns:	- Success if new file handle
 */
int mca_fs_ime_file_open (struct ompi_communicator_t *comm,
                          const char* filename,
                          int access_mode,
                          struct opal_info_t *info,
                          ompio_file_t *fh)
{
    int amode = 0;
    int old_mask, perm;
    int rank, ret = OMPI_SUCCESS;

    rank = ompi_comm_rank ( comm );

    if (fh->f_perm == OMPIO_PERM_NULL)  {
        old_mask = umask(022);
        umask(old_mask);
        perm = old_mask ^ 0666;
    }
    else {
        perm = fh->f_perm;
    }

    if (access_mode & MPI_MODE_RDONLY)
        amode = amode | O_RDONLY;
    if (access_mode & MPI_MODE_WRONLY)
        amode = amode | O_WRONLY;
    if (access_mode & MPI_MODE_RDWR)
        amode = amode | O_RDWR;

    /* Reset errno */
    errno = 0;

    if ( rank == OMPIO_ROOT ) {
        /* MODE_CREATE and MODE_EXCL should only be set by one process */
        if ( access_mode & MPI_MODE_CREATE )
            amode = amode | O_CREAT;
        if (access_mode & MPI_MODE_EXCL)
            amode = amode | O_EXCL;

        fh->fd = ime_native_open(filename, amode, perm);
        if ( fh->fd < 0 ) {
            ret = mca_fs_ime_get_mpi_err(errno);
        }
    }

    comm->c_coll->coll_bcast (&ret, 1, MPI_INT, OMPIO_ROOT, comm,
                              comm->c_coll->coll_bcast_module);
    if ( ret != OMPI_SUCCESS ) {
        fh->fd = -1;
        return ret;
    }

    if ( rank != OMPIO_ROOT ) {
        errno = 0;
        fh->fd = ime_native_open(filename, amode, perm);
        if ( fh->fd < 0 ) {
            return mca_fs_ime_get_mpi_err(errno);
        }
    }

    return OMPI_SUCCESS;
}
