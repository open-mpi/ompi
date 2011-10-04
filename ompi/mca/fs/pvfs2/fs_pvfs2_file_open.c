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
#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype.h"
#include "ompi/datatype/ompi_datatype.h"

struct open_status_s {
    int error;
    PVFS_object_ref object_ref;
};
typedef struct open_status_s open_status;

static void fake_an_open(PVFS_fs_id id, 
                         char *pvfs2_name, 
                         int access_mode,
	                 int stripe_width, 
                         PVFS_size stripe_size,
                         mca_fs_pvfs2 *pvfs2_fs, 
			 open_status *o_status);
/*
 *	file_open_pvfs2: This is the same strategy as ROMIO's pvfs2 open
 *
 *	Function:	- opens a new file
 *	Accepts:	- same arguments as MPI_File_open()
 *	Returns:	- Success if new file handle
 */
int
mca_fs_pvfs2_file_open (struct ompi_communicator_t *comm, 
                        char* filename,
                        int access_mode,
                        struct ompi_info_t *info,
                        mca_io_ompio_file_t *fh)
{
    /*    int amode;
    int old_mask, perm;
    */
    int ret;
    mca_fs_pvfs2 *pvfs2_fs;
    PVFS_fs_id pvfs2_id;
    char pvfs2_path[OMPIO_MAX_NAME] = {0};
    char * ncache_timeout;
    open_status o_status = {0, {0, 0}};
    struct ompi_datatype_t *open_status_type;
    struct ompi_datatype_t *types[2] = {&ompi_mpi_int.dt, &ompi_mpi_byte.dt};
    int lens[2] = {1, sizeof(PVFS_object_ref)};
    OPAL_PTRDIFF_TYPE offsets[2];

    /* We are going to do what ROMIO does with one process resolving
     * the name and broadcasting to others */

    pvfs2_fs = (mca_fs_pvfs2 *) malloc(sizeof(mca_fs_pvfs2));
    if (NULL == pvfs2_fs) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (!mca_fs_pvfs2_IS_INITIALIZED) {
        /* disable the pvfs2 ncache */
        ncache_timeout = getenv("PVFS2_NCACHE_TIMEOUT");
        if (ncache_timeout == NULL ) {
            setenv("PVFS2_NCACHE_TIMEOUT", "0", 1);
        }
        ret = PVFS_util_init_defaults();
        if (ret < 0) {
            PVFS_perror("PVFS_util_init_defaults", ret);
            return OMPI_ERROR;
        }
        mca_fs_pvfs2_IS_INITIALIZED = 1;
    }

    memset(&(pvfs2_fs->credentials), 0, sizeof(PVFS_credentials));
    PVFS_util_gen_credentials(&(pvfs2_fs->credentials));

    if (OMPIO_ROOT == fh->f_rank) {
        ret = PVFS_util_resolve(filename, &pvfs2_id, pvfs2_path, OMPIO_MAX_NAME);
        if (ret < 0 ) {
            PVFS_perror("PVFS_util_resolve", ret);
	    o_status.error = -1;
        }
        else {
            fake_an_open (pvfs2_id, 
                          pvfs2_path,
                          access_mode, 
                          mca_fs_pvfs2_stripe_width,
                          (PVFS_size)mca_fs_pvfs2_stripe_size,
                          pvfs2_fs, 
                          &o_status);
        }
        pvfs2_fs->object_ref = o_status.object_ref;
	fh->f_fs_ptr = pvfs2_fs;
    }

    /* broadcast status and (possibly valid) object reference */
    MPI_Address(&o_status.error, &offsets[0]);
    MPI_Address(&o_status.object_ref, &offsets[1]);

    ompi_datatype_create_struct (2, lens, offsets, types, &open_status_type);
    ompi_datatype_commit (&open_status_type);

    fh->f_comm->c_coll.coll_bcast (MPI_BOTTOM,
                                   1,
                                   open_status_type,
                                   OMPIO_ROOT,
                                   fh->f_comm,
                                   fh->f_comm->c_coll.coll_bcast_module);

    ompi_datatype_destroy (&open_status_type);

    if (o_status.error != 0)
    {
        if (NULL != pvfs2_fs) {
            free(pvfs2_fs);
            pvfs2_fs = NULL;
        }
	return OMPI_ERROR;
    }

    pvfs2_fs->object_ref = o_status.object_ref;
    fh->f_fs_ptr = pvfs2_fs;
    if (mca_fs_pvfs2_stripe_size > 0) {
        fh->f_stripe_size = mca_fs_pvfs2_stripe_size;
    }
    else {
        fh->f_stripe_size = 65536;
    }
    return OMPI_SUCCESS;

    /*
    if (fh->f_perm == OMPIO_PERM_NULL) {
        old_mask = umask(022);
        umask(old_mask);
        perm = old_mask ^ 0666;
    }
    else {
        perm = fh->f_perm;
    }

    amode = 0;
    if (fh->f_amode & MPI_MODE_CREATE)
        amode = amode | O_CREAT;
    if (fh->f_amode & MPI_MODE_RDONLY)
        amode = amode | O_RDONLY;
    if (fh->f_amode & MPI_MODE_WRONLY)
        amode = amode | O_WRONLY;
    if (fh->f_amode & MPI_MODE_RDWR)
        amode = amode | O_RDWR;
    if (fh->f_amode & MPI_MODE_EXCL)
        amode = amode | O_EXCL;

    fh->fd = open (filename, amode, perm);
    if (fh->fd < 0) {
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
    */
}

static void fake_an_open(PVFS_fs_id id, 
                         char *pvfs2_name, 
                         int access_mode,
	                 int stripe_width, 
                         PVFS_size stripe_size,
                         mca_fs_pvfs2 *pvfs2_fs, 
			 open_status *o_status)
{
    int ret;
    PVFS_sysresp_lookup resp_lookup;
    PVFS_sysresp_getparent resp_getparent;
    PVFS_sysresp_create resp_create;
    PVFS_sys_attr attribs;
    PVFS_sys_dist *dist;

    memset(&attribs, 0, sizeof(PVFS_sys_attr));
    
    attribs.owner = geteuid();
    attribs.group = getegid();
    attribs.perms = 0644;
    attribs.mask =  PVFS_ATTR_SYS_ALL_SETABLE;
    attribs.atime = time(NULL);
    attribs.mtime = attribs.atime;
    attribs.ctime = attribs.atime;

    if (stripe_width > 0 ) {
	attribs.dfile_count = stripe_width;
	attribs.mask |= PVFS_ATTR_SYS_DFILE_COUNT;
    }

    dist = NULL;

    memset(&resp_lookup, 0, sizeof(resp_lookup));
    memset(&resp_getparent, 0, sizeof(resp_getparent));
    memset(&resp_create, 0, sizeof(resp_create));

    ret = PVFS_sys_lookup(id, 
                          pvfs2_name,
                          &(pvfs2_fs->credentials), 
                          &resp_lookup, 
                          PVFS2_LOOKUP_LINK_FOLLOW);

    if (ret == (-PVFS_ENOENT)) {
	if (access_mode & MPI_MODE_CREATE)  {
	    ret = PVFS_sys_getparent(id, 
                                     pvfs2_name,
                                     &(pvfs2_fs->credentials), 
                                     &resp_getparent); 
	    if (ret < 0) {
                opal_output (1, "pvfs_sys_getparent returns with %d\n", ret);
		o_status->error = ret;
		return;
	    }
            
            /* Set the distribution strip size if specified */
            if (0 < stripe_size) {
                /* Note that the distribution is hardcoded here */
                dist = PVFS_sys_dist_lookup ("simple_stripe");
                ret = PVFS_sys_dist_setparam (dist,
                                              "strip_size",
                                              &stripe_size);
                if (ret < 0)
                {
                    opal_output (1, 
                            "pvfs_sys_dist_setparam returns with %d\n", ret);
                    o_status->error = ret;
                }
            }

            /* Perform file creation */
            ret = PVFS_sys_create(resp_getparent.basename, 
                                  resp_getparent.parent_ref, 
                                  attribs,
                                  &(pvfs2_fs->credentials), 
                                  dist, 
                                  &resp_create); 
            /*
#ifdef HAVE_PVFS2_CREATE_WITHOUT_LAYOUT
            ret = PVFS_sys_create(resp_getparent.basename, 
                                  resp_getparent.parent_ref, 
                                  attribs, 
                                  &(pvfs2_fs->credentials), 
                                  dist, 
                                  &resp_create); 
#else             
            ret = PVFS_sys_create(resp_getparent.basename, 
                                  resp_getparent.parent_ref, 
                                  attribs, 
                                  &(pvfs2_fs->credentials), 
                                  dist, 
                                  NULL, 
                                  &resp_create);
            #endif
            */

	    /* if many creates are happening in this directory, the earlier
	     * sys_lookup may have returned ENOENT, but the sys_create could
	     * return EEXISTS.  That means the file has been created anyway, so
	     * less work for us and we can just open it up and return the
	     * handle */
	    if (ret == (-PVFS_EEXIST)) {
		ret = PVFS_sys_lookup(id, 
                                      pvfs2_name,
                                      &(pvfs2_fs->credentials), 
                                      &resp_lookup, 
                                      PVFS2_LOOKUP_LINK_FOLLOW);
		if ( ret < 0 ) {
		    o_status->error = ret;
		    return;
		}
		o_status->error = ret;
		o_status->object_ref = resp_lookup.ref;
		return;
	    }
	    o_status->object_ref = resp_create.ref;
	}
        else {
	    opal_output (1, "cannot create file without MPI_MODE_CREATE\n");
	    o_status->error = ret;
	    return;
	}
    } 
    else if (access_mode & MPI_MODE_EXCL) {
	/* lookup should not succeed if opened with EXCL */
	o_status->error = -PVFS_EEXIST;
	return;
    } 
    else {
	o_status->object_ref = resp_lookup.ref;
    }
    o_status->error = ret;
    return;
}
