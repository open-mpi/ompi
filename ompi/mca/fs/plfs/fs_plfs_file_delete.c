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
#include <unistd.h>

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/fs.h"

/*
 *	file_delete_plfs
 *
 *	Function:	- deletes a file
 *	Accepts:	- file name & info
 *	Returns:	- Success if file closed
 */
int
mca_fs_plfs_file_delete (char* file_name,
                       struct ompi_info_t *info)
{
    plfs_error_t plfs_ret;
    char wpath[1024];
    getcwd( wpath, sizeof(wpath) );
    sprintf( wpath,"%s/%s",wpath,file_name );
    plfs_ret = plfs_unlink( wpath );
    if (PLFS_SUCCESS != plfs_ret) {
        opal_output(0, "fs_plfs_file_delete: Error in plfs_unlink:\n%s\n", strplfserr(plfs_ret));
	return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}
