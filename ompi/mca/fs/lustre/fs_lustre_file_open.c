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
 * Copyright (c) 2008-2015 University of Houston. All rights reserved.
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
#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"

#include <sys/ioctl.h>
#include <lustre/liblustreapi.h>
#include <lustre/lustre_user.h>

/*
 *	file_open_lustre
 *
 *	Function:	- opens a new file
 *	Accepts:	- same arguments as MPI_File_open()
 *	Returns:	- Success if new file handle
 */
int
mca_fs_lustre_file_open (struct ompi_communicator_t *comm, 
                     char* filename,
                     int access_mode,
                     struct ompi_info_t *info,
                     mca_io_ompio_file_t *fh)
{
    int amode;
    int old_mask, perm;
    int rc;
    int flag;
    int fs_lustre_stripe_size = -1;
    int fs_lustre_stripe_width = -1;
    char char_stripe[MPI_MAX_INFO_KEY];

    struct lov_user_md *lump=NULL;

    if (fh->f_perm == OMPIO_PERM_NULL) {
        old_mask = umask(022);
        umask(old_mask);
        perm = old_mask ^ 0666;
    }
    else {
        perm = fh->f_perm;
    }

    amode = 0;
    if (access_mode & MPI_MODE_CREATE)
        amode = amode | O_CREAT;
    if (access_mode & MPI_MODE_RDONLY)
        amode = amode | O_RDONLY;
    if (access_mode & MPI_MODE_WRONLY)
        amode = amode | O_WRONLY;
    if (access_mode & MPI_MODE_RDWR)
        amode = amode | O_RDWR;
    if (access_mode & MPI_MODE_EXCL)
        amode = amode | O_EXCL;


    ompi_info_get (info, "stripe_size", MPI_MAX_INFO_VAL, char_stripe, &flag);
    if ( flag ) {
        sscanf ( char_stripe, "%d", &fs_lustre_stripe_size );
    }

    ompi_info_get (info, "stripe_width", MPI_MAX_INFO_VAL, char_stripe, &flag);
    if ( flag ) {
        sscanf ( char_stripe, "%d", &fs_lustre_stripe_width );
    }

    if (fs_lustre_stripe_size < 0) {
        fs_lustre_stripe_size = mca_fs_lustre_stripe_size;
    }

    if (fs_lustre_stripe_width < 0) {
        fs_lustre_stripe_width = mca_fs_lustre_stripe_width;
    }


    if ( (fs_lustre_stripe_size>0 || fs_lustre_stripe_width>0) &&
        (amode&O_CREAT) && (amode&O_RDWR)) {
        if (0 == fh->f_rank) {
            llapi_file_create(filename,
                              fs_lustre_stripe_size,
                              -1, /* MSC need to change that */
                              fs_lustre_stripe_width,
                              0); /* MSC need to change that */

            fh->fd = open(filename, O_CREAT | O_RDWR | O_LOV_DELAY_CREATE, perm);
            if (fh->fd < 0) {
                fprintf(stderr, "Can't open %s file: %d (%s)\n", 
                        filename, errno, strerror(errno));
                return OMPI_ERROR;
            }
            close (fh->fd);
        }
        fh->f_comm->c_coll.coll_barrier (fh->f_comm, 
                                         fh->f_comm->c_coll.coll_barrier_module);
    }

    fh->fd = open (filename, amode, perm);
    if (fh->fd < 0) {
        return OMPI_ERROR;
    }

    if (mca_fs_lustre_stripe_size > 0) {
        fh->f_stripe_size = mca_fs_lustre_stripe_size;
    }
    else {
      lump = (struct lov_user_md  *) malloc (sizeof(struct lov_user_md));
      if (NULL == lump ){
	fprintf(stderr,"Cannot allocate memory for extracting stripe size\n");
	return OMPI_ERROR;
      }
      rc = llapi_file_get_stripe(filename, lump);
      if (rc != 0) {
	  fprintf(stderr, "get_stripe failed: %d (%s)\n",errno, strerror(errno));
	  return OMPI_ERROR;
      }
      fh->f_stripe_size = lump->lmm_stripe_size;
    }
    return OMPI_SUCCESS;
}
