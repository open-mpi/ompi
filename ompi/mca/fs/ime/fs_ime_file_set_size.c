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

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/mca/fs/fs.h"

/*
 *	file_set_size_ime
 *
 *	Function:	- set_size of a file
 *	Accepts:	- same arguments as MPI_File_set_size()
 *	Returns:	- Success if size is set
 */
int mca_fs_ime_file_set_size (ompio_file_t *fh,
                              OMPI_MPI_OFFSET_TYPE size)
{
    int ret = 0;

    /* reset errno */
    errno = 0;

    if (OMPIO_ROOT == fh->f_rank) {
        ret = ime_native_ftruncate(fh->fd, size);
    }

    fh->f_comm->c_coll->coll_bcast(&ret,
                                   1,
                                   MPI_INT,
                                   OMPIO_ROOT,
                                   fh->f_comm,
                                   fh->f_comm->c_coll->coll_bcast_module);

    if (ret < 0) {
        return mca_fs_base_get_mpi_err(errno);
    }
    return OMPI_SUCCESS;
}
