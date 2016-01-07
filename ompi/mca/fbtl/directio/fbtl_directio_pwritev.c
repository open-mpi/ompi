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
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"
#include "fbtl_directio.h"

#include "mpi.h"
#include <unistd.h>
#include <sys/uio.h>
#include <limits.h>
#include <stdlib.h>
#include "ompi/constants.h"
#include "ompi/mca/fbtl/fbtl.h"

ssize_t  mca_fbtl_directio_pwritev(mca_io_ompio_file_t *fh )
{
    int i;
    ssize_t bytes_written=0, total_bytes_written=0;
    size_t nbytes, rem, diff;
    void *newbuf=NULL;
    int fs_ptr;

    if (NULL == fh->f_io_array) {
        return OMPI_ERROR;
    }

    memcpy ( &fs_ptr, &fh->f_fs_ptr, sizeof(int));
    
    for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
        /* 
        ** To use direct I/O :
        ** 1. make sure fh->f_io_array[i].offset starts at a block boundary
        ** 2. make sure fh->f_io_array[i].length is a multiple of the block size
        ** 3. make sure fh->f_io_array[i].memory_address is aligned to a page size
        */
        
        /* 
        ** This is step 1. Make sure offset is aligned 
        */
        if ( (OMPI_MPI_OFFSET_TYPE ) fh->f_io_array[i].offset % FBTL_DIRECTIO_BLOCK_SIZE) {
            diff = FBTL_DIRECTIO_BLOCK_SIZE - ( (OMPI_MPI_OFFSET_TYPE) fh->f_io_array[i].offset % FBTL_DIRECTIO_BLOCK_SIZE);
            if ( fh->f_io_array[i].length < diff ) {
                diff = fh->f_io_array[i].length;
            }
            /* 
            ** Use regular, bufferd I/O to write a partial block. This is represented in the
            ** fh->f_fd handle.
            */
            total_bytes_written = pwrite(fh->fd, (void *)fh->f_io_array[i].memory_address, 
                                      diff, 
                                      (off_t) fh->f_io_array[i].offset);
            if ( 0 > total_bytes_written ) {
                opal_output (1, "fbtl_directio_pwritev: could not write\n" );
                return OMPI_ERROR;
            }
            fh->f_io_array[i].memory_address = ((char *) fh->f_io_array[i].memory_address) + diff;
            fh->f_io_array[i].offset = (IOVBASE_TYPE *)(intptr_t)((OMPI_MPI_OFFSET_TYPE)fh->f_io_array[i].offset + diff);
            fh->f_io_array[i].length -= diff;
        }
        
        /* 
        ** This is step 2. Make sure length is a multiple of block size 
        */
        rem = fh->f_io_array[i].length % FBTL_DIRECTIO_BLOCK_SIZE;
        nbytes = fh->f_io_array[i].length -rem;
        
        if ( nbytes > 0 ) {
            if ( (((long)fh->f_io_array[i].memory_address) % FBTL_DIRECTIO_MEMALIGN_SIZE ) ) {
                /* 
                ** This is step 3. Make sure the buffer that we use starts at a page boundary 
                */
                posix_memalign(&newbuf, FBTL_DIRECTIO_MEMALIGN_SIZE, nbytes);
                if (NULL == newbuf) {
                    opal_output(1, "fbtl_directio_pwritev: memalign failed ");
                }
                else {
                    memcpy(newbuf, fh->f_io_array[i].memory_address,nbytes);
                }
            }
            else {
                newbuf = fh->f_io_array[i].memory_address;
            }
            
            /* 
            ** Write a multiple of block sizes using direct I/O 
            ** This is achieved using the fh->f_fdirect handle 
            */
            bytes_written = pwrite( fs_ptr, 
                                 newbuf, 
                                 nbytes,
                                 (off_t )fh->f_io_array[i].offset );
            
            if ( (((long)fh->f_io_array[i].memory_address) % FBTL_DIRECTIO_MEMALIGN_SIZE ) ) {
                free ( newbuf );
            }
            if (bytes_written < 0) {
                opal_output (1, "fbtl_directio_pwritev: could not write on direct I/O descriptor \n" );
                return OMPI_ERROR;
            }
        }
        /* 
        ** Write the remaining portion using buffered I/O 
        */
        if ( rem > 0 ) {            
            bytes_written += pwrite (fh->fd, 
                                  ((char *)fh->f_io_array[i].memory_address) + nbytes, 
                                  rem,
                                  (off_t)fh->f_io_array[i].offset + nbytes );
        }
        
	if (bytes_written < 0) {
            opal_output (1, "fbtl_directio_pwritev: could not write\n" );
	    return OMPI_ERROR;
        }
	total_bytes_written += bytes_written;
    }

    return total_bytes_written;
}
