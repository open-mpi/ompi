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

/* This code is based on the PVFS2 ADIO module in ROMIO
 *   Copyright (C) 1997 University of Chicago. 
 *   See COPYRIGHT notice in top-level directory.
 */

#include "ompi_config.h"
#include "fs_pvfs2.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/fs.h"

/*
 *	file_delete_pvfs2
 *
 *	Function:	- deletes a file
 *	Accepts:	- file name & info
 *	Returns:	- Success if file closed
 */
int
mca_fs_pvfs2_file_delete (char* file_name,
                          struct ompi_info_t *info)
{
    PVFS_credentials credentials;
    PVFS_sysresp_getparent resp_getparent;
    int ret;
    PVFS_fs_id pvfs2_id;
    char pvfs2_path[OMPIO_MAX_NAME] = {0};
    char * ncache_timeout;

    if (!mca_fs_pvfs2_IS_INITIALIZED) {
        /* disable the pvfs2 ncache */
        ncache_timeout = getenv("PVFS2_NCACHE_TIMEOUT");
        if (ncache_timeout == NULL )
            setenv("PVFS2_NCACHE_TIMEOUT", "0", 1);

        ret = PVFS_util_init_defaults();
        if (ret < 0) {
            return OMPI_ERROR;
        }
        mca_fs_pvfs2_IS_INITIALIZED = 1;
    }

    memset (&credentials, 0, sizeof(PVFS_credentials));
    PVFS_util_gen_credentials (&credentials);

    ret = PVFS_util_resolve(file_name, &pvfs2_id, pvfs2_path, OMPIO_MAX_NAME);
    if (ret != 0) {
        return OMPI_ERROR;
    }

    ret = PVFS_sys_getparent(pvfs2_id, pvfs2_path, &credentials, &resp_getparent);

    ret = PVFS_sys_remove(resp_getparent.basename,
                          resp_getparent.parent_ref, &credentials);
    if (ret != 0) {
        return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}
