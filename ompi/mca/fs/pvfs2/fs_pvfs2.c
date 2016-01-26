/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2016 University of Houston. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics. Since linkers generally pull in symbols by object fules,
 * keeping these symbols as the only symbols in this file prevents
 * utility programs such as "ompi_info" from having to import entire
 * modules just to query their version and parameters
 */


#include "ompi_config.h"
#include "mpi.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/mca/fs/pvfs2/fs_pvfs2.h"

#ifdef HAVE_SYS_STATFS_H
#include <sys/statfs.h> /* or <sys/vfs.h> */
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

/*
 * *******************************************************************
 * ************************ actions structure ************************
 * *******************************************************************
 */
static mca_fs_base_module_1_0_0_t pvfs2 =  {
    mca_fs_pvfs2_module_init, /* initalise after being selected */
    mca_fs_pvfs2_module_finalize, /* close a module on a communicator */
    mca_fs_pvfs2_file_open,
    mca_fs_pvfs2_file_close,
    mca_fs_pvfs2_file_delete,
    mca_fs_pvfs2_file_set_size,
    mca_fs_pvfs2_file_get_size,
    mca_fs_pvfs2_file_sync
};
/*
 * *******************************************************************
 * ************************* structure ends **************************
 * *******************************************************************
 */

int mca_fs_pvfs2_component_init_query(bool enable_progress_threads,
                                      bool enable_mpi_threads)
{
    /* Nothing to do */

   return OMPI_SUCCESS;
}

struct mca_fs_base_module_1_0_0_t *
mca_fs_pvfs2_component_file_query (mca_io_ompio_file_t *fh, int *priority)
{

    /* The code in this function is based on the ADIO FS selection in ROMIO
     *   Copyright (C) 1997 University of Chicago.
     *   See COPYRIGHT notice in top-level directory.
     */

    int err;
    char *dir;
    struct statfs fsbuf;
    char *tmp;

    /* The code in this function is based on the ADIO FS selection in ROMIO
     *   Copyright (C) 1997 University of Chicago.
     *   See COPYRIGHT notice in top-level directory.
     */

   *priority = mca_fs_pvfs2_priority;

    tmp = strchr (fh->f_filename, ':');
    if (!tmp) {
        if (OMPIO_ROOT == fh->f_rank) {
            fh->f_fstype = mca_fs_base_get_fstype ( fh->f_filename );
	}
	fh->f_comm->c_coll.coll_bcast (&(fh->f_fstype),
				       1,
				       MPI_INT,
				       OMPIO_ROOT,
				       fh->f_comm,
				       fh->f_comm->c_coll.coll_bcast_module);
    }
    else {
        if (!strncmp(fh->f_filename, "pvfs2:", 6) ||
            !strncmp(fh->f_filename, "PVFS2:", 6)) {
            fh->f_fstype = PVFS2;
        }
    }

   if (PVFS2 == fh->f_fstype) {
       if (*priority < 50) {
           *priority = 50;
	   return &pvfs2;
       }
   }

   return NULL;
}

int mca_fs_pvfs2_component_file_unquery (mca_io_ompio_file_t *file)
{
   /* This function might be needed for some purposes later. for now it
    * does not have anything to do since there are no steps which need
    * to be undone if this module is not selected */

   return OMPI_SUCCESS;
}

int mca_fs_pvfs2_module_init (mca_io_ompio_file_t *file)
{
    /* Make sure the file type is not overwritten by the last queried
	 * component */
    file->f_fstype = PVFS2;
    return OMPI_SUCCESS;
}


int mca_fs_pvfs2_module_finalize (mca_io_ompio_file_t *file)
{
    return OMPI_SUCCESS;
}
