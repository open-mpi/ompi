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
    int perm, amode;
    int ret = OMPI_SUCCESS;

    perm = mca_fs_base_get_file_perm(fh);
    amode = mca_fs_base_get_file_amode(fh->f_rank, access_mode);


    /* Reset errno */
    errno = 0;

    if (OMPIO_ROOT == fh->f_rank) {
        fh->fd = ime_native_open(filename, amode, perm);
        if ( fh->fd < 0 ) {
            ret = mca_fs_base_get_mpi_err(errno);
        }
    }

    comm->c_coll->coll_bcast (&ret, 1, MPI_INT, OMPIO_ROOT, comm,
                              comm->c_coll->coll_bcast_module);
    if ( ret != OMPI_SUCCESS ) {
        fh->fd = -1;
        return ret;
    }

    if (OMPIO_ROOT != fh->f_rank) {
        errno = 0;
        fh->fd = ime_native_open(filename, amode, perm);
        if ( fh->fd < 0 ) {
            return mca_fs_base_get_mpi_err(errno);
        }
    }

    return OMPI_SUCCESS;
}
