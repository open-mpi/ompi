/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2015 High Performance Computing Center Stuttgart,
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
#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/fs.h"

#include <unistd.h>
#include <string.h>

#include <gpfs.h>
#include <fcntl.h>
#include <errno.h>
#include <gpfs_fcntl.h>

/*
 *  file_get_info_gpfs
 *
 *  Function:   - get_info of a file
 *  Accepts:    - same arguments as MPI_File_get_info()
 *  Returns:    - new info object
 */

int mca_fs_gpfs_file_get_info(ompio_file_t *fh,
                        ompi_info_t **info_used)
{
    int ret = OMPI_SUCCESS;

    *info_used = OBJ_NEW(ompi_info_t);
    if (NULL == *info_used) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ret = ompi_info_dup ((ompi_info_t *)fh->f_info, info_used);

    return ret;
}
