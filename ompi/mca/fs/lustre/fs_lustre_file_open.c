/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2018 University of Houston. All rights reserved.
 * Copyright (c) 2015-2020 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * Copyright (c) 2024      Advanced Micro Devices, Inc. All rights reserverd.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include "fs_lustre.h"

#include <fcntl.h>
#include <unistd.h>
#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"

#include <sys/ioctl.h>

static void *alloc_lum(void);

static void *alloc_lum(void)
{
  int v1, v3;

  v1 = sizeof(struct lov_user_md_v1) +
    LOV_MAX_STRIPE_COUNT * sizeof(struct lov_user_ost_data_v1);
  v3 = sizeof(struct lov_user_md_v3) +
    LOV_MAX_STRIPE_COUNT * sizeof(struct lov_user_ost_data_v1);

  return malloc(MAX(v1, v3));
}

/*
 *	file_open_lustre
 *
 *	Function:	- opens a new file
 *	Accepts:	- same arguments as MPI_File_open()
 *	Returns:	- Success if new file handle
 */

int
mca_fs_lustre_file_open (struct ompi_communicator_t *comm,
                     const char* filename,
                     int access_mode,
                     struct opal_info_t *info,
                     ompio_file_t *fh)
{
    int amode, perm;
    int rc, ret=OMPI_SUCCESS;
    int flag;
    int fs_lustre_stripe_size = -1;
    int fs_lustre_stripe_width = -1;
    opal_cstring_t *stripe_str;
    char *rfilename = (char *)filename;
    struct lov_user_md *lump=NULL;

    perm = mca_fs_base_get_file_perm(fh);
    amode = mca_fs_base_get_file_amode(fh->f_rank, access_mode);

    opal_info_get (info, "striping_unit", &stripe_str, &flag);
    if ( flag ) {
        sscanf ( stripe_str->string, "%d", &fs_lustre_stripe_size );
        OBJ_RELEASE(stripe_str);
    }
    else {
        //internal info object name used earlier. Kept for backwards compatibility.
        opal_info_get (info, "stripe_size", &stripe_str, &flag);
        if ( flag ) {
            sscanf ( stripe_str->string, "%d", &fs_lustre_stripe_size );
            OBJ_RELEASE(stripe_str);
        }
    }
    
    opal_info_get (info, "striping_factor", &stripe_str, &flag);
    if ( flag ) {
        sscanf ( stripe_str->string, "%d", &fs_lustre_stripe_width );
        OBJ_RELEASE(stripe_str);
    }
    else {
        //internal info object name used earlier. Kept for backwards compatibility.        
        opal_info_get (info, "stripe_width", &stripe_str, &flag);
        if ( flag ) {
            sscanf ( stripe_str->string, "%d", &fs_lustre_stripe_width );
            OBJ_RELEASE(stripe_str);
        }
    }
    
    if (fs_lustre_stripe_size < 0) {
        fs_lustre_stripe_size = mca_fs_lustre_stripe_size;
    }   

    if (fs_lustre_stripe_width < 0) {
        fs_lustre_stripe_width = mca_fs_lustre_stripe_width;
    }

    /* Check for soft links and replace filename by the actual
       file used in case it is a soft link */
    if (mca_fs_base_is_link(filename)) {
        mca_fs_base_get_real_filename(filename, &rfilename);
        /* make sure the real file is also on a Lustre file system */
        if (LUSTRE != mca_fs_base_get_fstype(rfilename)) {
            opal_output(1, "cannot use a soft-link between a LUSTRE and non-LUSTRE file system\n");
            return OPAL_ERROR;
        }
    }
    
    /* Reset errno */
    errno = 0;
    if (OMPIO_ROOT == fh->f_rank) {
        if ( (fs_lustre_stripe_size>0 || fs_lustre_stripe_width>0) &&
             ( amode&O_CREAT)                                      && 
             ( (amode&O_RDWR)|| amode&O_WRONLY) ) {
            /* this cannot be a soft-link since we are creating the file.
               Not using rfilename here */
            llapi_file_create(filename,
                              fs_lustre_stripe_size,
                              -1, /* MSC need to change that */
                              fs_lustre_stripe_width,
                              0); /* MSC need to change that */
            
            fh->fd = open(filename, amode | O_LOV_DELAY_CREATE, perm);
        }
        else {
            fh->fd = open (filename, amode, perm);
        }

        if ( 0 > fh->fd ) {
            ret = mca_fs_base_get_mpi_err(errno);
        }
    }

    comm->c_coll->coll_bcast ( &ret, 1, MPI_INT, 0, comm, comm->c_coll->coll_bcast_module);
    if ( OMPI_SUCCESS != ret ) {
        fh->fd = -1;
        return ret;
    }

    if (OMPIO_ROOT != fh->f_rank) {
        fh->fd = open (filename, amode, perm);
        if ( 0 > fh->fd) {
            return mca_fs_base_get_mpi_err(errno);
        }
    }

    lump = alloc_lum();
    if (NULL == lump) {
        fprintf(stderr,"Cannot allocate memory for extracting stripe size\n");
        return OMPI_ERROR;
    }
    rc = llapi_file_get_stripe(rfilename, lump);
    if (rc != 0) {
        opal_output(1, "get_stripe failed: %d (%s)\n", errno, strerror(errno));
        free(lump);
        return OMPI_ERROR;
    }
    fh->f_stripe_size   = lump->lmm_stripe_size;
    fh->f_stripe_count  = lump->lmm_stripe_count;
    fh->f_fs_block_size = lump->lmm_stripe_size;
    free(lump);

    if (FS_LUSTRE_LOCK_AUTO == mca_fs_lustre_lock_algorithm ||
        FS_LUSTRE_LOCK_NEVER == mca_fs_lustre_lock_algorithm ) {
        fh->f_flags |= OMPIO_LOCK_NEVER;
    }
    else if (FS_LUSTRE_LOCK_ENTIRE_FILE == mca_fs_lustre_lock_algorithm) {
        fh->f_flags |= OMPIO_LOCK_ENTIRE_FILE;
    }
    else if (FS_LUSTRE_LOCK_RANGES == mca_fs_lustre_lock_algorithm) {
        /* Nothing to be done. This is what the posix fbtl component would do
           anyway without additional information . */
    }
    else {
        opal_output ( 1, "Invalid value for mca_fs_lustre_lock_algorithm %d", mca_fs_lustre_lock_algorithm );
    }

    return OMPI_SUCCESS;
}
