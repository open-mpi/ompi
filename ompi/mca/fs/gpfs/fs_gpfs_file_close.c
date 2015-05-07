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
 * Copyright (c) 2008-2011 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "fs_gpfs.h"
#include "gpfs.h"

#include <fcntl.h>
#include <unistd.h>
#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/fs.h"

/*
 *	file_close_gpfs
 *
 *	Function:	- closes a new file
 *	Accepts:	- file handle
 *	Returns:	- Success if file closed
 */
int
mca_fs_gpfs_file_close (mca_io_ompio_file_t *fh)
{
	printf("Using mca_fs_gpfs_file_close to close a file.\n");

    fh->f_comm->c_coll.coll_barrier (fh->f_comm,
                                     fh->f_comm->c_coll.coll_barrier_module);
    /*    close (*(int *)fh->fd);*/
    close (fh->fd);

    return OMPI_SUCCESS;
}
