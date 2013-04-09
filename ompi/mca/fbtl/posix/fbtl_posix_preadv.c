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

#include "ompi_config.h"
#include "fbtl_posix.h"

#include "mpi.h"
#include <unistd.h>
#include "ompi/constants.h"
#include "ompi/mca/fbtl/fbtl.h"

size_t 
mca_fbtl_posix_preadv (mca_io_ompio_file_t *fh,
                       int *sorted)
{
    /*int *fp = NULL;*/
    int i, block=1;
    struct iovec *iov = NULL;
    int iov_count = 0;
    OMPI_MPI_OFFSET_TYPE iov_offset = 0;
#if 0
    int k;
    int merge = 0;
    char *merge_buf = NULL;
    size_t merge_length = 0;
    OMPI_MPI_OFFSET_TYPE merge_offset = 0;
#endif
    if (NULL == fh->f_io_array) {
        return OMPI_ERROR;
    }
    /*
    fp = (int *)fh->fd;
    if (0 == *fp)
    {
        return OMPI_ERROR;
    }
    */

#if 1
    iov = (struct iovec *) malloc 
        (OMPIO_IOVEC_INITIAL_SIZE * sizeof (struct iovec));
    if (NULL == iov) {
        opal_output(1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (NULL != sorted) {
        for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
            if (0 == iov_count) {
                iov[iov_count].iov_base = fh->f_io_array[sorted[i]].memory_address;
                iov[iov_count].iov_len = fh->f_io_array[sorted[i]].length;
                iov_offset = (OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[sorted[i]].offset;
                iov_count ++;
            }

            if (OMPIO_IOVEC_INITIAL_SIZE*block <= iov_count) {
                block ++;
                iov = (struct iovec *)realloc
                    (iov, OMPIO_IOVEC_INITIAL_SIZE * block *
                     sizeof(struct iovec));
                if (NULL == iov) {
                    opal_output(1, "OUT OF MEMORY\n");
                    return OMPI_ERR_OUT_OF_RESOURCE;
                }
            }

            if (fh->f_num_of_io_entries != i+1) {
	      if (((OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[sorted[i]].offset + 
                     (OPAL_PTRDIFF_TYPE)fh->f_io_array[sorted[i]].length) == 
		  (OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[sorted[i+1]].offset) {                    
                    iov[iov_count].iov_base = 
                        fh->f_io_array[sorted[i+1]].memory_address;
                    iov[iov_count].iov_len = fh->f_io_array[sorted[i+1]].length;
                    iov_count ++;
                    continue;
                }
            }

            if (-1 == lseek (fh->fd, iov_offset, SEEK_SET)) {
                perror ("fseek");
                return OMPI_ERROR;
            }
            if (-1 == readv (fh->fd, iov, iov_count)) {
                perror ("writev");
                return OMPI_ERROR;
            }
            else {
                iov_count = 0;
            }
        }
    }

    else {
        for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
            if (0 == iov_count) {
                iov[iov_count].iov_base = fh->f_io_array[i].memory_address;
                iov[iov_count].iov_len = fh->f_io_array[i].length;
                iov_offset = (OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i].offset;
                iov_count ++;
            }

            if (OMPIO_IOVEC_INITIAL_SIZE*block <= iov_count) {
                block ++;
                iov = (struct iovec *)realloc
                    (iov, OMPIO_IOVEC_INITIAL_SIZE * block *
                     sizeof(struct iovec));
                if (NULL == iov) {
                    opal_output(1, "OUT OF MEMORY\n");
                    return OMPI_ERR_OUT_OF_RESOURCE;
                }
            }

            if (fh->f_num_of_io_entries != i+1) {
	      if (((OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i].offset + 
                     (OPAL_PTRDIFF_TYPE)fh->f_io_array[i].length) == 
		  (OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i+1].offset) {                    
                    iov[iov_count].iov_base = 
                        fh->f_io_array[i+1].memory_address;
                    iov[iov_count].iov_len = fh->f_io_array[i+1].length;
                    iov_count ++;
                    continue;
                }
            }

            if (-1 == lseek (fh->fd, iov_offset, SEEK_SET)) {
                perror ("fseek");
                return OMPI_ERROR;
            }
            if (-1 == readv (fh->fd, iov, iov_count)) {
                perror ("writev");
                return OMPI_ERROR;
            }
            else {
                iov_count = 0;
            }
        }
    }
    if (NULL != iov) {
        free (iov);
        iov = NULL;
    }
#endif
#if 0
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
                if (-1 == pread(fh->fd,
                                merge_buf,
                                merge_length,
                                merge_offset)) {
                    perror("pread() error");
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
                if (-1 == pread(fh->fd,
                                fh->f_io_array[sorted[i]].memory_address,
                                fh->f_io_array[sorted[i]].length,
                                (OMPI_MPI_OFFSET_TYPE)fh->f_io_array[sorted[i]].offset)) {
                    perror("pread() error");
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
                if (-1 == pread(fh->fd,
                                merge_buf,
                                merge_length,
                                merge_offset)) {
                    perror("pread() error");
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
                if (-1 == pread(fh->fd,
                                fh->f_io_array[i].memory_address,
                                fh->f_io_array[i].length,
                                (OMPI_MPI_OFFSET_TYPE)fh->f_io_array[i].offset)) {
                    perror("pread() error");
                    return OMPI_ERROR;
                }
            }
        }
    }
#endif
    return OMPI_SUCCESS;
}
