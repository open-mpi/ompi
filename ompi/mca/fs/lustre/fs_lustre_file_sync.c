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
 * Copyright (c) 2008-2009 University of Houston. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include "fs_lustre.h"
#include <unistd.h>

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/fs.h"

/*
 *	file_sync_lustre
 *
 *	Function:	- closes a new file
 *	Accepts:	- file handle
 *	Returns:	- Success if file closed
 */
int
mca_fs_lustre_file_sync (mca_io_ompio_file_t *fh)
{
    int err;

    err = fsync(fh->fd);

    if (-1 == err) {
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}
