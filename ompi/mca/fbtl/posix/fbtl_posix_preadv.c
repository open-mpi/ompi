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
 * Copyright (c) 2008-2021 University of Houston. All rights reserved.
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


static ssize_t mca_fbtl_posix_preadv_datasieving (ompio_file_t *fh, struct flock *lock, int *lock_counter);
static ssize_t mca_fbtl_posix_preadv_generic (ompio_file_t *fh, struct flock *lock, int *lock_counter);
static ssize_t mca_fbtl_posix_preadv_single (ompio_file_t *fh, struct flock *lock, int *lock_counter);


ssize_t mca_fbtl_posix_preadv (ompio_file_t *fh )
{
    ssize_t bytes_read=0;
    struct flock lock;
    int lock_counter=0;    

    if (NULL == fh->f_io_array) {
        return OMPI_ERROR;
    }
    
    if ( fh->f_atomicity ) {
        OMPIO_SET_ATOMICITY_LOCK(fh, lock, lock_counter, F_RDLCK);
    }

    if ( fh->f_num_of_io_entries > 1 ) {
        bool do_data_sieving = true;

        size_t avg_gap_size=0;
        size_t avg_block_size = 0;
        off_t prev_offset = (off_t)fh->f_io_array[0].offset;
        int i;
        for ( i=0; i< fh->f_num_of_io_entries; i++ ) {
            avg_block_size += fh->f_io_array[i].length;
            avg_gap_size   += (size_t)((off_t)fh->f_io_array[i].offset - prev_offset);
            prev_offset     = (off_t)fh->f_io_array[i].offset;
        }
        avg_block_size = avg_block_size / fh->f_num_of_io_entries;
        avg_gap_size = avg_gap_size / fh->f_num_of_io_entries;

        if ( false == mca_fbtl_posix_read_datasieving       ||
             0     == avg_gap_size                          ||
             avg_block_size > mca_fbtl_posix_max_block_size ||
             avg_gap_size   > mca_fbtl_posix_max_gap_size     ) {
            do_data_sieving = false;
        }

        if ( do_data_sieving) {
            bytes_read = mca_fbtl_posix_preadv_datasieving (fh, &lock, &lock_counter);
        }
        else {
            bytes_read =  mca_fbtl_posix_preadv_generic (fh, &lock, &lock_counter);
        }
    }
    else {
        // i.e. fh->f_num_of_io_entries == 1
        bytes_read = mca_fbtl_posix_preadv_single (fh, &lock, &lock_counter );
    }

    if ( fh->f_atomicity ) {
        mca_fbtl_posix_unlock ( &lock, fh, &lock_counter );
    }
    
        
    return bytes_read;
}

ssize_t mca_fbtl_posix_preadv_single (ompio_file_t *fh, struct flock *lock, int *lock_counter)
{
    ssize_t bytes_read=0, ret_code;
    size_t total_bytes = 0;
    int ret;
    
    ret = mca_fbtl_posix_lock ( lock, fh, F_RDLCK, (off_t)fh->f_io_array[0].offset,
                                (off_t)fh->f_io_array[0].length, OMPIO_LOCK_ENTIRE_REGION,
                                lock_counter ); 
    if ( 0 < ret ) {
        opal_output(1, "mca_fbtl_posix_preadv_single: error in mca_fbtl_posix_lock() ret=%d: %s",
                    ret, strerror(errno));
        /* Just in case some part of the lock worked */
        mca_fbtl_posix_unlock ( lock, fh, lock_counter);
        return OMPI_ERROR;
    }
    
    size_t len = fh->f_io_array[0].length;
    while ( total_bytes < len ) {
        ret_code = pread(fh->fd, (char*)fh->f_io_array[0].memory_address+total_bytes,
                         fh->f_io_array[0].length-total_bytes,
                         (off_t)fh->f_io_array[0].offset+total_bytes );
        if ( ret_code == -1 ) {
            opal_output(1, "mca_fbtl_posix_preadv_single: error in (p)read(v):%s", strerror(errno));
            mca_fbtl_posix_unlock ( lock, fh, lock_counter );
            return OMPI_ERROR;
        }
        if ( ret_code == 0 ) {
            // end of file
	    break;
        }
        total_bytes += ret_code;
    }   
    bytes_read = total_bytes;
    mca_fbtl_posix_unlock ( lock, fh, lock_counter );

    return bytes_read;
}

ssize_t mca_fbtl_posix_preadv_datasieving (ompio_file_t *fh, struct flock *lock, int *lock_counter)
{
    size_t start, end, len;
    size_t bufsize = 0;
    int ret, i, j;
    ssize_t bytes_read=0, ret_code=0;
    char *temp_buf = NULL;
    
    int startindex = 0;
    int endindex   = 0;
    bool done = false;
    
    while (!done) {
        // Break the io_array into chunks such that the size of the temporary
        // buffer does not exceed mca_fbtl_posix_max_tmpbuf_size bytes.
        // Each iteration will thus work in the range (startindex, endindex[
        startindex = endindex;
        if ( startindex >= fh->f_num_of_io_entries  ) {
            done = true;
            break;
        }
        
        size_t sstart = (size_t)fh->f_io_array[startindex].offset;
        size_t slen=0, maxlen=0;
        int maxindex = startindex;

        for ( j = startindex; j < fh->f_num_of_io_entries; j++ ) {
            endindex = j;
            slen = ((size_t)fh->f_io_array[j].offset + fh->f_io_array[j].length) - sstart;            
            if (slen > mca_fbtl_posix_max_tmpbuf_size ) {
                endindex = j-1;
                break;
            }
            if (slen > maxlen) {
                maxlen   = slen;
                maxindex = endindex;
            }
        }
        // Need to increment the value of endindex
        // by one for the loop syntax to work correctly.
        endindex++;
        
        start = (size_t)fh->f_io_array[startindex].offset;
        end   = (size_t)fh->f_io_array[maxindex].offset + fh->f_io_array[maxindex].length;
        len   = end - start;

        if ( len > bufsize ) {
            if ( NULL != temp_buf ) {
                free ( temp_buf);
            }
            temp_buf = (char *) malloc ( len );
            if ( NULL == temp_buf ) {
                opal_output(1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            bufsize = len;
        }
        
        // Read the entire block.
        ret = mca_fbtl_posix_lock ( lock, fh, F_RDLCK, start, len, OMPIO_LOCK_ENTIRE_REGION, lock_counter ); 
        if ( 0 < ret ) {
            opal_output(1, "mca_fbtl_posix_preadv_datasieving: error in mca_fbtl_posix_lock() ret=%d: %s",
                        ret, strerror(errno));
            /* Just in case some part of the lock worked */
            mca_fbtl_posix_unlock ( lock, fh, lock_counter);
            free ( temp_buf);
            return OMPI_ERROR;
        }
        size_t total_bytes = 0;
        
        while ( total_bytes < len ) {
            ret_code = pread (fh->fd, temp_buf+total_bytes, len-total_bytes, start+total_bytes);
            if ( ret_code == -1 ) {
                opal_output(1, "mca_fbtl_posix_preadv_datasieving: error in (p)read(v):%s", strerror(errno));
                mca_fbtl_posix_unlock ( lock, fh, lock_counter );
                free ( temp_buf);
                return OMPI_ERROR;
            }
            if ( ret_code == 0 ) {
                // end of file
		break;
            }
            total_bytes += ret_code;
        }
        mca_fbtl_posix_unlock ( lock, fh, lock_counter);
        
        // Copy out the elements that were requested.
        size_t pos = 0;
        size_t num_bytes;
        size_t start_offset = (size_t) fh->f_io_array[startindex].offset;
        for ( i = startindex ; i < endindex ; i++) {
            pos = (size_t) fh->f_io_array[i].offset - start_offset;
            if ( (ssize_t) pos > total_bytes ) {
                break;
            }
            num_bytes = fh->f_io_array[i].length;
            if ( ((ssize_t) pos + (ssize_t)num_bytes) > total_bytes ) {
                num_bytes = total_bytes - (ssize_t)pos;
            }
            
            memcpy (fh->f_io_array[i].memory_address, temp_buf + pos, num_bytes);
            bytes_read += num_bytes;            
        }
    }
    
    free ( temp_buf);
    return bytes_read;
}

ssize_t mca_fbtl_posix_preadv_generic (ompio_file_t *fh, struct flock *lock, int *lock_counter )
{
    ssize_t bytes_read=0, ret_code=0;
    struct iovec *iov = NULL;
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
        
        ret = mca_fbtl_posix_lock ( lock, fh, F_RDLCK, iov_offset, total_length, OMPIO_LOCK_SELECTIVE, lock_counter ); 
        if ( 0 < ret ) {
            opal_output(1, "mca_fbtl_posix_preadv_generic: error in mca_fbtl_posix_lock() ret=%d: %s", ret, strerror(errno));
            free (iov);
            /* Just in case some part of the lock worked */
            mca_fbtl_posix_unlock ( lock, fh, lock_counter);
            return OMPI_ERROR;
        }
#if defined(HAVE_PREADV)
        ret_code = preadv (fh->fd, iov, iov_count, iov_offset);
#else
        if (-1 == lseek (fh->fd, iov_offset, SEEK_SET)) {
            opal_output(1, "mca_fbtl_posix_preadv_generic: error in lseek:%s", strerror(errno));
            free(iov);
            mca_fbtl_posix_unlock ( lock, fh, lock_counter );
            return OMPI_ERROR;
        }
        ret_code = readv (fh->fd, iov, iov_count);
#endif
        mca_fbtl_posix_unlock ( lock, fh, lock_counter );
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
