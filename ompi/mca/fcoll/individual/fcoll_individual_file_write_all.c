/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
#include "fcoll_individual.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/io/ompio/io_ompio.h"
#include "ompi/mca/io/io.h"
#include "math.h"
#include <unistd.h>

#define TIME_BREAKDOWN 0

int mca_fcoll_individual_file_write_all (mca_io_ompio_file_t *fh, 
                                         void *buf, 
                                         int count, 
                                         struct ompi_datatype_t *datatype, 
                                         ompi_status_public_t *status)
{
    size_t total_bytes_written = 0;     /* total bytes that have been written*/
    size_t bytes_to_write_in_cycle = 0; /* left to be written in a cycle*/
    size_t bytes_per_cycle = 0;         /* total written in each cycle by each process*/
    int index = 0;
    int cycles = 0;

    uint32_t iov_count = 0;
    struct iovec *decoded_iov = NULL;

    size_t max_data = 0; 
    int i = 0; /* index into the decoded iovec of the buffer */
    int j = 0; /* index into the file vie iovec */
    int k = 0; /* index into the io_array */
    size_t sum_previous_counts = 0;
    size_t sum_previous_length = 0;
#if TIME_BREAKDOWN
    double start = 0, end=0, start_all=0, end_all=0, total_io=0;
#endif

#if TIME_BREAKDOWN
    start_all = MPI_Wtime();
#endif

    ompi_io_ompio_decode_datatype (fh, 
                                   datatype, 
                                   count, 
                                   buf, 
                                   &max_data, 
                                   &decoded_iov, 
                                   &iov_count);

    if (mca_fcoll_individual_constant_cbs) {
        bytes_per_cycle = mca_fcoll_individual_cycle_buffer_size/fh->f_size;
    }
    else {
        bytes_per_cycle = mca_fcoll_individual_cycle_buffer_size;
    }

    cycles = ceil((float)max_data/bytes_per_cycle);

#if 0
    printf ("MAX DATA: %d\n", max_data);
    printf ("Bytes per Cycle: %d   Cycles: %d\n",bytes_per_cycle, cycles);
#endif

    sum_previous_length = fh->f_position_in_file_view;
    j = fh->f_index_in_file_view;

    for (index = 0; index < cycles; index++) {
        OPAL_PTRDIFF_TYPE disp;
        int block = 1;

        k = 0;
        if ((index == cycles-1) && (max_data % bytes_per_cycle)) {
            bytes_to_write_in_cycle = max_data % bytes_per_cycle;
        }
        else {
            bytes_to_write_in_cycle = bytes_per_cycle;
        }

        fh->f_io_array = (mca_io_ompio_io_array_t *)malloc 
            (OMPIO_IOVEC_INITIAL_SIZE * sizeof (mca_io_ompio_io_array_t));
        if (NULL == fh->f_io_array) {
            opal_output(1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        while (bytes_to_write_in_cycle) {
            /* reallocate if needed  */
            if (OMPIO_IOVEC_INITIAL_SIZE*block <= k) {
                block ++;
                fh->f_io_array = (mca_io_ompio_io_array_t *)realloc 
                    (fh->f_io_array, OMPIO_IOVEC_INITIAL_SIZE * 
                     block * sizeof (mca_io_ompio_io_array_t));
                if (NULL == fh->f_io_array) {
                    opal_output(1, "OUT OF MEMORY\n");
                    return OMPI_ERR_OUT_OF_RESOURCE;
                }
            }

            if (decoded_iov[i].iov_len - 
                (total_bytes_written - sum_previous_counts) <= 0) {
                sum_previous_counts += decoded_iov[i].iov_len;
                i = i + 1;
            }

            disp = (OPAL_PTRDIFF_TYPE)decoded_iov[i].iov_base + 
                (total_bytes_written - sum_previous_counts);
            fh->f_io_array[k].memory_address = (IOVBASE_TYPE *)disp;
            
            if (decoded_iov[i].iov_len - 
                (total_bytes_written - sum_previous_counts) >= 
                bytes_to_write_in_cycle) {
                fh->f_io_array[k].length = bytes_to_write_in_cycle;
            }
            else {
                fh->f_io_array[k].length =  decoded_iov[i].iov_len - 
                    (total_bytes_written - sum_previous_counts);
            }

            if (fh->f_decoded_iov[j].iov_len - 
                (fh->f_total_bytes - sum_previous_length) <= 0) {
                sum_previous_length += fh->f_decoded_iov[j].iov_len;
                j = j + 1;
                if (j == (int)fh->f_iov_count) {
                    j = 0;
                    sum_previous_length = 0;
                    fh->f_offset += fh->f_view_extent;
                    fh->f_position_in_file_view = sum_previous_length;
                    fh->f_index_in_file_view = j;
                    fh->f_total_bytes = 0;
                }
            }

            disp = (OPAL_PTRDIFF_TYPE)fh->f_decoded_iov[j].iov_base + 
                (fh->f_total_bytes - sum_previous_length);
            fh->f_io_array[k].offset = (IOVBASE_TYPE *)(disp + fh->f_offset);

            if (fh->f_decoded_iov[j].iov_len - 
                (fh->f_total_bytes - sum_previous_length) 
                < fh->f_io_array[k].length) {
                fh->f_io_array[k].length = fh->f_decoded_iov[j].iov_len - 
                    (fh->f_total_bytes - sum_previous_length);
            }
            total_bytes_written += fh->f_io_array[k].length;
            fh->f_total_bytes += fh->f_io_array[k].length;
            bytes_to_write_in_cycle -= fh->f_io_array[k].length;
            k = k + 1;
        }
        fh->f_position_in_file_view = sum_previous_length;
        fh->f_index_in_file_view = j;
        fh->f_num_of_io_entries = k;

#if 0
        if (fh->f_rank == 0) {
            int d;
            printf("*************************** %d\n", fh->f_num_of_io_entries);

            for (d=0 ; d<fh->f_num_of_io_entries ; d++) {
                printf(" ADDRESS: %p  OFFSET: %p   LENGTH: %d\n",
                       fh->f_io_array[d].memory_address,
                       fh->f_io_array[d].offset,
                       fh->f_io_array[d].length);
            }
        }
#endif

        if (fh->f_num_of_io_entries) {
            fh->f_fbtl->fbtl_pwritev (fh, NULL);
        }

        fh->f_num_of_io_entries = 0;
        if (NULL != fh->f_io_array) {
            free (fh->f_io_array);
            fh->f_io_array = NULL;
        }
    }

    if (NULL != decoded_iov) {
        free (decoded_iov);
        decoded_iov = NULL;
    }

    return OMPI_SUCCESS;
}
