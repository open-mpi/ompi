/*
 * Copyright (c) 2018      DataDirect Networks. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "fbtl_ime.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fbtl/fbtl.h"

static ssize_t mca_fbtl_ime_blocking_op(ompio_file_t *fh, int io_op);

ssize_t mca_fbtl_ime_preadv(ompio_file_t *fh)
{
    return mca_fbtl_ime_blocking_op(fh, FBTL_IME_READ);
}

ssize_t  mca_fbtl_ime_pwritev(ompio_file_t *fh)
{
    return mca_fbtl_ime_blocking_op(fh, FBTL_IME_WRITE);
}

static ssize_t mca_fbtl_ime_blocking_op(ompio_file_t *fh, int io_op)
{
    int i, block = 1, ret;
    struct iovec *iov = NULL;
    int iov_count = 0;
    OMPI_MPI_OFFSET_TYPE iov_offset = 0;
    ssize_t bytes_processed = 0, ret_code = 0;

    if (NULL == fh->f_io_array) {
        return OMPI_ERROR;
    }

    iov = (struct iovec *) malloc
        (OMPIO_IOVEC_INITIAL_SIZE * sizeof (struct iovec));
    if (NULL == iov) {
        opal_output(1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Go through all IO entries and try to aggregate them. */
    for (i = 0 ; i < fh->f_num_of_io_entries; i++) {
        iov[iov_count].iov_base = fh->f_io_array[i].memory_address;
        iov[iov_count].iov_len = fh->f_io_array[i].length;
        iov_count++;

        /* Save the file offset if the current iovec is
           the first one in the iovec array. */
        if (iov_count == 1) {
            iov_offset = (OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i].offset;
        }

        /* Allocate more memory for the iovecs if necessary */
        if (iov_count == OMPIO_IOVEC_INITIAL_SIZE * block) {
            block++;
            struct iovec *new_iov = (struct iovec *) realloc(iov, 
                    OMPIO_IOVEC_INITIAL_SIZE * block * sizeof(struct iovec));
            if (new_iov == NULL) {
                free(iov);
                opal_output(1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }

        /* If:
           - There is no next iovec
           - OR the next iovec is not "contiguous"
           - OR we exceeded the advised number of iovecs for IME
           Then: pwritev/preadv shall be called,
                 and the iovec array reset */
        if (i+1 == fh->f_num_of_io_entries ||
            ((OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i].offset +
             (ptrdiff_t)fh->f_io_array[i].length) !=
              (OMPI_MPI_OFFSET_TYPE)(intptr_t)fh->f_io_array[i+1].offset ||
            iov_count >= mca_fbtl_ime_iov_max ) {

            switch (io_op) {
            case FBTL_IME_READ:
                ret_code = ime_native_preadv(fh->fd, iov, iov_count, iov_offset);
                if (ret_code < 0) {
                    opal_output(1, "mca_fbtl_ime_blocking_op: error in "
                                   "ime_native_preadv error ret=%zd  %s",
                                   ret_code, strerror(errno));
                    goto error_exit;
                }
                break;

            case FBTL_IME_WRITE:
                ret_code = ime_native_pwritev(fh->fd, iov, iov_count, iov_offset);
                if (ret_code < 0) {
                    opal_output(1, "mca_fbtl_ime_blocking_op: error in "
                                   "ime_native_pwritev error ret=%zd  %s",
                                   ret_code, strerror(errno));
                    goto error_exit;
                }
                break;

            default:
                opal_output(1, "mca_fbtl_ime_blocking_op: an unsupported "
                               "IO operation was requested. io_op=%d", io_op);
                goto error_exit;
            }

            bytes_processed += ret_code;
            iov_count = 0;
        }
    }

    free (iov);
    return bytes_processed;

error_exit:
    free(iov);
    return OMPI_ERROR;
}
