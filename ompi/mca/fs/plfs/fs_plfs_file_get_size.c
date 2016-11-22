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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include "fs_plfs.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/fs.h"

/*
 *	file_get_size_plfs
 *
 *	Function:	- get_size of a file
 *	Accepts:	- same arguments as MPI_File_get_size()
 *	Returns:	- Success if size is get
 */
int
mca_fs_plfs_file_get_size (mca_io_ompio_file_t *fh,
                         OMPI_MPI_OFFSET_TYPE *size)
{
    Plfs_fd *pfd = NULL;
    plfs_error_t plfs_ret;
    struct stat st;
    char wpath[1024];
    int size_only = 1;

    getcwd(wpath, sizeof(wpath));
    sprintf(wpath,"%s/%s",wpath,fh->f_filename);

    plfs_ret = plfs_getattr(pfd, wpath, &st, size_only);
    if (PLFS_SUCCESS != plfs_ret) {
        opal_output(0, "fs_plfs_file_get_size: Error in plfs_getattr:\n%s\n", strplfserr(plfs_ret));
	return OMPI_ERROR;
    }

    *size = st.st_size;
    return OMPI_SUCCESS;
}
