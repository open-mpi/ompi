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
 * Copyright (c) 2008-2020 University of Houston. All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include <limits.h>
#include "ompi/constants.h"
#include "ompi/mca/fbtl/fbtl.h"


static int mca_fbtl_posix_preadv_datasieving (ompio_file_t *fh);
static int mca_fbtl_posix_preadv_generic (ompio_file_t *fh);

ssize_t mca_fbtl_posix_preadv (ompio_file_t *fh )
{
    ssize_t bytes_read=0, ret_code=0;
    struct flock lock;
    int ret;

    if (NULL == fh->f_io_array) {
        return OMPI_ERROR;
    }
    
    if ( fh->f_num_of_io_entries > 1 ) {
        bool do_data_sieving = false;
        
        if ( do_data_sieving) {
            return mca_fbtl_posix_preadv_datasieving (fh);
        }
        else {
            return mca_fbtl_posix_preadv_generic (fh);
        }
    }
    else {
        // Case num_of_io_entries == 1
        ret = mca_fbtl_posix_lock ( &lock, fh, F_RDLCK, (off_t)fh->f_io_array[0].offset,
                                    (off_t)fh->f_io_array[0].length, OMPIO_LOCK_ENTIRE_REGION ); 
        if ( 0 < ret ) {
            opal_output(1, "mca_fbtl_posix_preadv: error in mca_fbtl_posix_lock() ret=%d: %s", ret, strerror(errno));
            /* Just in case some part of the lock worked */
            mca_fbtl_posix_unlock ( &lock, fh);
            return OMPI_ERROR;
        }
        
        ret_code = pread(fh->fd, fh->f_io_array[0].memory_address, fh->f_io_array[0].length, (off_t)fh->f_io_array[0].offset );
        mca_fbtl_posix_unlock ( &lock, fh );
        if ( ret_code == -1 ) {
            opal_output(1, "mca_fbtl_posix_preadv: error in (p)read(v):%s", strerror(errno));
	    return OMPI_ERROR;
	}
        
        bytes_read += ret_code;
    }
        
    return bytes_read;
}

int mca_fbtl_posix_preadv_datasieving (ompio_file_t *fh)
{
    size_t start, end, len;
    int ret, i;
    ssize_t bytes_read=0, ret_code=0;
    struct flock lock;
    
    start = (off_t)fh->f_io_array[0].offset;
    end   = (off_t)fh->f_io_array[fh->f_num_of_io_entries-1].offset + fh->f_io_array[fh->f_num_of_io_entries-1].length;
    len   = end - start;
    
    char *temp_buf = (char *) malloc ( len );
    if ( NULL == temp_buf ) {
        opal_output(1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    // Read the entire block.
    ret = mca_fbtl_posix_lock ( &lock, fh, F_RDLCK, start, len, OMPIO_LOCK_ENTIRE_REGION ); 
    if ( 0 < ret ) {
        opal_output(1, "mca_fbtl_posix_preadv_datasieving: error in mca_fbtl_posix_lock() ret=%d: %s", ret, strerror(errno));
        /* Just in case some part of the lock worked */
        mca_fbtl_posix_unlock ( &lock, fh);
        return OMPI_ERROR;
    }
    
    ret_code = pread (fh->fd, temp_buf, len, start);
    mca_fbtl_posix_unlock ( &lock, fh);
    if ( ret_code == -1 ) {
        opal_output(1, "mca_fbtl_posix_preadv_datasieving: error in (p)read(v):%s", strerror(errno));
        return OMPI_ERROR;
    }
    
    // Copy out the elements that were requested.
    size_t pos = 0;
    size_t num_bytes;
    size_t start_offset = (size_t) fh->f_io_array[0].offset;
    for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
        pos = (size_t) fh->f_io_array[i].offset - start_offset;
        if ( (ssize_t) pos > ret_code ) {
            break;
        }
        num_bytes = fh->f_io_array[i].length;
        if ( ((ssize_t) pos + (ssize_t)num_bytes) > ret_code ) {
            num_bytes = ret_code - (ssize_t)pos;
        }
        
        memcpy (fh->f_io_array[i].memory_address, temp_buf + pos, num_bytes);
        bytes_read += num_bytes;            
    }
    
    free ( temp_buf);
    return bytes_read;
}

int mca_fbtl_posix_preadv_generic (ompio_file_t *fh )
{
    ssize_t bytes_read=0, ret_code=0;
    struct iovec *iov = NULL;
    struct flock lock;
    int ret, i;

    int block=1;
    int iov_count = 0;
    OMPI_MPI_OFFSET_TYPE iov_offset = 0;
    off_t total_length, end_offset=0;
    
    iov = (struct iovec *) malloc (OMPIO_IOVEC_INITIAL_SIZE * sizeof (struct iovec));
    if (NULL == iov) {
        opal_output(1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
        if (0 == iov_count) {
            iov[iov_count].iov_base = fh->f_io_array[i].memory_address;
            iov[iov_count].iov_len = fh->f_io_array[i].length;
            iov_offset = (OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i].offset;
            end_offset = (off_t)fh->f_io_array[i].offset + (off_t)fh->f_io_array[i].length;
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
            if (((((OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i].offset +
                   (ptrdiff_t)fh->f_io_array[i].length) ==
                  (OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i+1].offset)) &&
                (iov_count < IOV_MAX ) ){
                iov[iov_count].iov_base =
                    fh->f_io_array[i+1].memory_address;
                iov[iov_count].iov_len = fh->f_io_array[i+1].length;
                end_offset = (off_t)fh->f_io_array[i].offset + (off_t)fh->f_io_array[i].length;
                iov_count ++;
                continue;
            }
        }
        
        total_length = (end_offset - (off_t)iov_offset );
        
        ret = mca_fbtl_posix_lock ( &lock, fh, F_RDLCK, iov_offset, total_length, OMPIO_LOCK_SELECTIVE ); 
        if ( 0 < ret ) {
            opal_output(1, "mca_fbtl_posix_preadv_generic: error in mca_fbtl_posix_lock() ret=%d: %s", ret, strerror(errno));
            free (iov);
            /* Just in case some part of the lock worked */
            mca_fbtl_posix_unlock ( &lock, fh);
            return OMPI_ERROR;
        }
#if defined(HAVE_PREADV)
        ret_code = preadv (fh->fd, iov, iov_count, iov_offset);
#else
        if (-1 == lseek (fh->fd, iov_offset, SEEK_SET)) {
            opal_output(1, "mca_fbtl_posix_preadv_generic: error in lseek:%s", strerror(errno));
            free(iov);
            mca_fbtl_posix_unlock ( &lock, fh );
            return OMPI_ERROR;
        }
        ret_code = readv (fh->fd, iov, iov_count);
#endif
        mca_fbtl_posix_unlock ( &lock, fh );
        if ( 0 < ret_code ) {
            bytes_read+=ret_code;
        }
        else if ( ret_code == -1 ) {
            opal_output(1, "mca_fbtl_posix_preadv_generic: error in (p)readv:%s", strerror(errno));
            free(iov);
            return OMPI_ERROR;
        }
        else if ( 0 == ret_code ){
            /* end of file reached, no point in continue reading; */
            break;
        }
        iov_count = 0;
    }   

    free (iov);
    return bytes_read;
}
