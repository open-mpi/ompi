/*
 *  Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                          University Research and Technology
 *                          Corporation.  All rights reserved.
 *  Copyright (c) 2004-2005 The University of Tennessee and The University
 *                          of Tennessee Research Foundation.  All rights
 *                          reserved.
 *  Copyright (c) 2004-2015 High Performance Computing Center Stuttgart,
 *                          University of Stuttgart.  All rights reserved.
 *  Copyright (c) 2004-2005 The Regents of the University of California.
 *                          All rights reserved.
 *  Copyright (c) 2008-2012 University of Houston. All rights reserved.
 *  $COPYRIGHT$
 *  
 *  Additional copyrights may follow
 *  
 *  $HEADER$
 */

#include "ompi_config.h"

#include "ompi/communicator/communicator.h"
#include "ompi/info/info.h"
#include "ompi/file/file.h"
#include "ompi/mca/fs/fs.h"
#include "ompi/mca/fs/base/base.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/fcoll/base/base.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/fbtl/base/base.h"
#include "fs_gpfs.h"

#include <sys/types.h>
#include <sys/stat.h>

#include <unistd.h>
#include <gpfs.h>
#include <fcntl.h>
#include <errno.h>
#include <gpfs_fcntl.h>

int
mca_fs_gpfs_file_open (struct ompi_communicator_t *comm,
                        const char* filename,
                        int access_mode,
                        struct opal_info_t *info,
                        ompio_file_t *fh)
{
    int amode;
    int old_mask, perm;

    if (fh->f_perm == OMPIO_PERM_NULL)  {
        old_mask = umask(022);
        umask(old_mask);
        perm = old_mask ^ 0666;
    }
    else {
        perm = fh->f_perm;
    }

    amode = 0;

    if (access_mode & MPI_MODE_CREATE) {
        amode = amode | O_CREAT;
    }
    if (access_mode & MPI_MODE_RDONLY) {
        amode = amode | O_RDONLY;
    }
    if (access_mode & MPI_MODE_WRONLY) {
        amode = amode | O_WRONLY;
    }
    if (access_mode & MPI_MODE_RDWR) {
        amode = amode | O_RDWR;
    }
    if (access_mode & MPI_MODE_EXCL) {
        amode = amode | O_EXCL;
    }

    //DEBUG: fprintf(stderr, "Opening a file using Linux open() within fs_gpfs_file_open\n");

    fh->fd = open (filename, amode, perm);
    if (-1 == fh->fd) {
        return OMPI_ERROR;
    }

    fh->f_amode=access_mode;
    mca_fs_gpfs_file_set_info(fh, (struct ompi_info_t *) info);

    return OMPI_SUCCESS;
}

int
mca_fs_gpfs_file_get_amode (ompi_file_t *fh,
                             int *amode)
{
    mca_common_ompio_data_t *data;

    data = (mca_common_ompio_data_t *) fh->f_io_selected_data;

    *amode = data->ompio_fh.f_amode;

    return OMPI_SUCCESS;
}

//CN: Is mca_fs_gpfs_file_seek needed at all. Not used anywhere!
int
mca_fs_gpfs_file_seek(ompi_file_t *fh, OMPI_MPI_OFFSET_TYPE off, int whence) {
    //DEBUG: fprintf(stderr, "GPFS FILE SEEK!");
    int ret = OMPI_SUCCESS;
    mca_common_ompio_data_t *data;
    OMPI_MPI_OFFSET_TYPE offset, temp_offset;

    data = (mca_common_ompio_data_t *) fh->f_io_selected_data;

    offset = off * data->ompio_fh.f_etype_size;

    switch (whence) {
        case MPI_SEEK_SET:
            if (offset < 0) {
                return OMPI_ERROR;
            }
            break;
        case MPI_SEEK_CUR:
            offset += data->ompio_fh.f_position_in_file_view;
            offset += data->ompio_fh.f_disp;
            if (offset < 0) {
                return OMPI_ERROR;
            }
            break;
        case MPI_SEEK_END:
            ret = data->ompio_fh.f_fs->fs_file_get_size(&data->ompio_fh,
                    &temp_offset);
            offset += temp_offset;
            if (offset < 0 || OMPI_SUCCESS != ret) {
                return OMPI_ERROR;
            }
            break;
        default:
            return OMPI_ERROR;
    }

    ret = mca_common_ompio_set_explicit_offset(&data->ompio_fh, offset
            / data->ompio_fh.f_etype_size);
    return ret;
}

int mca_fs_gpfs_file_get_position(ompi_file_t *fh, OMPI_MPI_OFFSET_TYPE *offset) {
    mca_common_ompio_data_t *data;

    data = (mca_common_ompio_data_t *) fh->f_io_selected_data;

    *offset = data->ompio_fh.f_position_in_file_view
            / data->ompio_fh.f_etype_size;

    return OMPI_SUCCESS;
}
