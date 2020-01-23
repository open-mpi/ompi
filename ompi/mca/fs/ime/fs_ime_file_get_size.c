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
 *	file_get_size_ime
 *
 *	Function:	- get_size of a file
 *	Accepts:	- same arguments as MPI_File_get_size()
 *	Returns:	- Success if size is get
 */
int mca_fs_ime_file_get_size (ompio_file_t *fh,
                              OMPI_MPI_OFFSET_TYPE *size)
{
    /* reset errno */
    errno = 0;

    *size = ime_native_lseek(fh->fd, 0, SEEK_END);
    if (*size < 0) {
        return mca_fs_base_get_mpi_err(errno);
    }

    errno = 0;
    if ((ime_native_lseek(fh->fd, fh->f_offset, SEEK_SET)) < 0) {
        return mca_fs_base_get_mpi_err(errno);
    }

    return OMPI_SUCCESS;
}
