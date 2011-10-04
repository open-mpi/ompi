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
#include "fbtl_pvfs2.h"

#include "mpi.h"
#include <unistd.h>
#include "ompi/constants.h"
#include "ompi/mca/fbtl/fbtl.h"

size_t 
mca_fbtl_pvfs2_preadv (mca_io_ompio_file_t *fh,
                       int *sorted)
{
    int i;
    int ret;
    size_t k;
    int merge = 0;
    char *merge_buf = NULL;
    size_t merge_length = 0;
    OMPI_MPI_OFFSET_TYPE merge_offset = 0;
    PVFS_sysresp_io resp_io;
    PVFS_Request file_req;
    PVFS_Request mem_req;
    mca_fs_pvfs2 *pvfs2_fs;

    pvfs2_fs = (mca_fs_pvfs2 *)fh->f_fs_ptr;

    if (NULL == fh->f_io_array) {
        return OMPI_ERROR;
    }

    if (NULL != sorted) {
        for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
            if (fh->f_num_of_io_entries != i+1) {
                if (((OMPI_MPI_OFFSET_TYPE)fh->f_io_array[sorted[i]].offset + 
                     (OPAL_PTRDIFF_TYPE)fh->f_io_array[sorted[i]].length) == 
                    (OMPI_MPI_OFFSET_TYPE)fh->f_io_array[sorted[i+1]].offset) {
                    if (!merge) {
                        merge_offset = (OMPI_MPI_OFFSET_TYPE)
                            fh->f_io_array[sorted[i]].offset;
                        merge_length = fh->f_io_array[sorted[i]].length;
                    }
                    merge_length += fh->f_io_array[sorted[i+1]].length;
                    merge++;
                    continue;
                }
            }
            if (merge) {
                merge_buf = malloc (merge_length);

                ret = PVFS_Request_contiguous (merge_length, 
                                               PVFS_BYTE, 
                                               &mem_req);
                if (ret != 0) {
                    perror("PVFS_Request_contiguous() error");
                    return OMPI_ERROR;
                }
                ret = PVFS_Request_contiguous (merge_length, 
                                               PVFS_BYTE, 
                                               &file_req);
                if (ret != 0) {
                    perror("PVFS_Request_contiguous() error");
                    return OMPI_ERROR;
                }
                ret = PVFS_sys_read(pvfs2_fs->object_ref, 
                                    file_req,
                                    merge_offset,
                                    merge_buf, 
                                    mem_req,
                                    &(pvfs2_fs->credentials), 
                                    &resp_io);
                if (ret != 0) {
                    perror("PVFS_sys_write() error");
                    return OMPI_ERROR;
                }

                k = 0;
                while (merge >= 0) {
                    memcpy (fh->f_io_array[sorted[i-merge]].memory_address,
                            merge_buf + k, 
                            fh->f_io_array[sorted[i-merge]].length);
                    k += fh->f_io_array[sorted[i-merge]].length;
                    merge --;
                }
                merge = 0;
                merge_offset = 0;
                merge_length = 0;
                if (NULL != merge_buf) {
                    free (merge_buf);
                    merge_buf = NULL;
                }
            }
            else {
                ret = PVFS_Request_contiguous (fh->f_io_array[sorted[i]].length, 
                                               PVFS_BYTE, 
                                               &mem_req);
                if (ret != 0) {
                    perror("PVFS_Request_contiguous() error");
                    return OMPI_ERROR;
                }
                ret = PVFS_Request_contiguous (fh->f_io_array[sorted[i]].length, 
                                               PVFS_BYTE, 
                                               &file_req);
                if (ret != 0) {
                    perror("PVFS_Request_contiguous() error");
                    return OMPI_ERROR;
                }
                ret = PVFS_sys_read(pvfs2_fs->object_ref, 
                                    file_req,
                                    (OMPI_MPI_OFFSET_TYPE)
                                    fh->f_io_array[sorted[i]].offset,
                                    fh->f_io_array[sorted[i]].memory_address, 
                                    mem_req,
                                    &(pvfs2_fs->credentials), 
                                    &resp_io);
                if (ret != 0) {
                    perror("PVFS_sys_write() error");
                    return OMPI_ERROR;
                }
            }
        }
    }

    else {
        for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
            if (fh->f_num_of_io_entries != i+1) {
                if (((OMPI_MPI_OFFSET_TYPE)fh->f_io_array[i].offset + 
                     (OPAL_PTRDIFF_TYPE)fh->f_io_array[i].length) == 
                    (OMPI_MPI_OFFSET_TYPE)fh->f_io_array[i+1].offset) {
                    if (!merge) {
                        merge_offset = (OMPI_MPI_OFFSET_TYPE)
                            fh->f_io_array[i].offset;
                        merge_length = fh->f_io_array[i].length;
                    }
                    merge_length += fh->f_io_array[i+1].length;
                    merge++;
                    continue;
                }
            }
            if (merge) {
                merge_buf = malloc (merge_length);

                ret = PVFS_Request_contiguous (merge_length, 
                                               PVFS_BYTE, 
                                               &mem_req);
                if (ret != 0) {
                    perror("PVFS_Request_contiguous() error");
                    return OMPI_ERROR;
                }
                ret = PVFS_Request_contiguous (merge_length, 
                                               PVFS_BYTE, 
                                               &file_req);
                if (ret != 0) {
                    perror("PVFS_Request_contiguous() error");
                    return OMPI_ERROR;
                }
                ret = PVFS_sys_read (pvfs2_fs->object_ref, 
                                     file_req,
                                     merge_offset,
                                     merge_buf, 
                                     mem_req,
                                     &(pvfs2_fs->credentials), 
                                     &resp_io);
                if (ret != 0) {
                    perror("PVFS_sys_write() error");
                    return OMPI_ERROR;
                }

                k = 0;
                while (merge >= 0) {
                    memcpy (fh->f_io_array[i-merge].memory_address,
                            merge_buf + k, 
                            fh->f_io_array[i-merge].length);
                    k += fh->f_io_array[i-merge].length;
                    merge --;
                }
                merge = 0;
                merge_offset = 0;
                merge_length = 0;
                if (NULL != merge_buf) {
                    free (merge_buf);
                    merge_buf = NULL;
                }
            }
            else {
                ret = PVFS_Request_contiguous (fh->f_io_array[i].length, 
                                               PVFS_BYTE, 
                                               &mem_req);
                if (ret != 0) {
                    perror("PVFS_Request_contiguous() error");
                    return OMPI_ERROR;
                }
                ret = PVFS_Request_contiguous (fh->f_io_array[i].length, 
                                               PVFS_BYTE, 
                                               &file_req);
                if (ret != 0) {
                    perror("PVFS_Request_contiguous() error");
                    return OMPI_ERROR;
                }
                ret = PVFS_sys_read (pvfs2_fs->object_ref, 
                                     file_req,
                                     (OMPI_MPI_OFFSET_TYPE)
                                     fh ->f_io_array[i].offset,
                                     fh->f_io_array[i].memory_address, 
                                     mem_req,
                                     &(pvfs2_fs->credentials), 
                                     &resp_io);
                if (ret != 0) {
                    perror("PVFS_sys_write() error");
                    return OMPI_ERROR;
                }
            }
        }
    }
    return OMPI_SUCCESS;
}
