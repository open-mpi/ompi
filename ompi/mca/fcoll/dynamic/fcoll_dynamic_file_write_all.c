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
#include "fcoll_dynamic.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/io/ompio/io_ompio.h"
#include "ompi/mca/io/io.h"
#include "math.h"
#include "ompi/mca/pml/pml.h"
#include <unistd.h>

#define TIME_BREAKDOWN 0

int
mca_fcoll_dynamic_file_write_all (mca_io_ompio_file_t *fh,
                                  void *buf,
                                  int count,
                                  struct ompi_datatype_t *datatype,
                                  ompi_status_public_t *status)
{
    MPI_Aint total_bytes_written = 0;  /* total bytes that have been written*/
    MPI_Aint total_bytes = 0;          /* total bytes to be written */
    MPI_Aint bytes_to_write_in_cycle = 0; /* left to be written in a cycle*/
    MPI_Aint bytes_per_cycle = 0;      /* total written in each cycle by each process*/
    int index = 0;
    int cycles = 0;
    int i=0, j=0, x=0;
    int n=0; /* current position in total_bytes_per_process array */
    MPI_Aint bytes_remaining = 0; /* how many bytes have been written from the current
                                value from total_bytes_per_process */
    int bytes_sent = 0;
    int blocks = 0;
    /* iovec structure and count of the buffer passed in */
    uint32_t iov_count = 0;
    struct iovec *decoded_iov = NULL;
    int iov_index = 0;
    char *send_buf = NULL;
    size_t current_position = 0;

    /* global iovec at the writers that contain the iovecs created from
       file_set_view */
    uint32_t total_fview_count = 0;
    struct iovec *global_fview = NULL;
    int local_count = 0;
    struct iovec *iov = NULL;
    int *fview_count = NULL;
    int current_index;

    char *global_buf = NULL;
    MPI_Aint global_count = 0;

    /* array that contains the sorted indices of the global_iov */
    int *sorted = NULL;
    int *displs = NULL;
    size_t max_data = 0; 
    int *bytes_per_process = NULL;
    MPI_Aint bytes_left = 0;
    MPI_Aint *total_bytes_per_process = NULL;
#if TIME_BREAKDOWN
    double start_time=0, end_time=0, start_time2=0, end_time2=0;
    double total=0 , total_io=0;
#endif

    if (opal_datatype_is_contiguous_memory_layout(&datatype->super,1)) {
        fh->f_flags |= OMPIO_CONTIGUOUS_MEMORY;
    }
    /**************************************************************************
     ** In case the data is not contigous in memory, decode it into an iovec **
     **************************************************************************/
    if (! (fh->f_flags & OMPIO_CONTIGUOUS_MEMORY)) {
        ompi_io_ompio_decode_datatype (fh,
                                       datatype,
                                       count,
                                       buf,
                                       &max_data,
                                       &decoded_iov,
                                       &iov_count);
    }
    else {
        max_data = count * datatype->super.size;
    }

    if (! (fh->f_flags & OMPIO_AGGREGATOR_IS_SET)) {
        ompi_io_ompio_set_aggregator_props (fh, 
                                            mca_fcoll_dynamic_num_io_procs,
                                            max_data);
    }

#if TIME_BREAKDOWN
    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
        start_time = MPI_Wtime();
    }
#endif

    total_bytes_per_process = (MPI_Aint*)malloc
        (fh->f_procs_per_group*sizeof(MPI_Aint));
    if (NULL == total_bytes_per_process) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ompi_io_ompio_allgather_array (&max_data,
                                   1,
                                   MPI_LONG,
                                   total_bytes_per_process,
                                   1,
                                   MPI_LONG,
                                   fh->f_aggregator_index,
                                   fh->f_procs_in_group,
                                   fh->f_procs_per_group,
                                   fh->f_comm);

    for (i=0 ; i<fh->f_procs_per_group ; i++) {
        total_bytes += total_bytes_per_process[i];
    }

    if (NULL != total_bytes_per_process) {
        free (total_bytes_per_process);
        total_bytes_per_process = NULL;
    }

    /*********************************************************************
     *** Generate the File offsets/lengths corresponding to this write ***
     ********************************************************************/
    ompi_io_ompio_generate_current_file_view (fh, max_data, &iov, &local_count);
    /*
    for (i=0 ; i<local_count ; i++) {
        printf("%d: OFFSET: %p   LENGTH: %lld\n",
               fh->f_rank,
               iov[i].iov_base,
               iov[i].iov_len);
    }
    */
    /*************************************************************
     *** ALLGather the File View information at all processes ***
     *************************************************************/

    fview_count = (int *) malloc (fh->f_procs_per_group * sizeof (int));
    if (NULL == fview_count) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ompi_io_ompio_allgather_array (&local_count,
                                   1,
                                   MPI_INT,
                                   fview_count,
                                   1,
                                   MPI_INT,
                                   fh->f_aggregator_index,
                                   fh->f_procs_in_group,
                                   fh->f_procs_per_group,
                                   fh->f_comm);
    displs = (int*) malloc (fh->f_procs_per_group * sizeof (int));
    if (NULL == displs) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    displs[0] = 0;
    total_fview_count = fview_count[0];
    for (i=1 ; i<fh->f_procs_per_group ; i++) {
        total_fview_count += fview_count[i];
        displs[i] = displs[i-1] + fview_count[i-1];
    }
    /*
    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
        for (i=0 ; i<fh->f_procs_per_group ; i++) {
            printf ("%d: PROCESS: %d  ELEMENTS: %d  DISPLS: %d\n",
                    fh->f_rank,
                    i,
                    fview_count[i],
                    displs[i]);
        }
    }
    */
    /* allocate the global iovec  */
    if (0 != total_fview_count) {
        global_fview = (struct iovec*)malloc (total_fview_count * 
                                              sizeof(struct iovec));
        if (NULL == global_fview) {
            opal_output (1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }
    if (fh->f_flags & OMPIO_UNIFORM_FVIEW) {
        ompi_io_ompio_allgather_array (iov,
                                       local_count,
                                       fh->f_iov_type,
                                       global_fview,
                                       local_count,
                                       fh->f_iov_type,
                                       fh->f_aggregator_index,
                                       fh->f_procs_in_group,
                                       fh->f_procs_per_group,
                                       fh->f_comm);
    }
    else { 
        ompi_io_ompio_allgatherv_array (iov,
                                        local_count,
                                        fh->f_iov_type,
                                        global_fview,
                                        fview_count,
                                        displs,
                                        fh->f_iov_type,
                                        fh->f_aggregator_index,
                                        fh->f_procs_in_group,
                                        fh->f_procs_per_group,
                                        fh->f_comm);
    }
    /* sort it */
    if (0 != total_fview_count) {
        sorted = (int *)malloc (total_fview_count * sizeof(int));
        if (NULL == sorted) {
            opal_output (1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        ompi_io_ompio_sort_iovec (global_fview, total_fview_count, sorted);
    }

    if (NULL != iov) {
        free (iov);
        iov = NULL;
    }
    /*
    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
        for (i=0 ; i<total_fview_count ; i++) {
            printf("%d: OFFSET: %p   LENGTH: %lld\n",
                   total_fview_count,
                   global_fview[i].iov_base,
                   global_fview[i].iov_len);
        }
    }
    */
    /*
    fh->f_flags |= OMPIO_UNIFORM_FVIEW;
    if (ompi_ddt_is_contiguous_memory_layout (datatype, count))
        fh->f_flags |= OMPIO_CONTIGUOUS_MEMORY;
    */
    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
        bytes_per_process = (int *)malloc (fh->f_procs_per_group * sizeof (int));
        if (NULL == bytes_per_process) {
            opal_output (1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    /* MSC THIS does not work at the moment
     * Calculate how many bytes are written in each cycle
    if (mca_fcoll_dynamic_constant_cbs) {
        bytes_per_cycle = 
            mca_fcoll_dynamic_cycle_buffer_size/mca_fcoll_dynamic_num_io_procs;
    }
    else {
    */
    bytes_per_cycle = mca_fcoll_dynamic_cycle_buffer_size;

    cycles = ceil((double)total_bytes/bytes_per_cycle);

    n = 0; 
    bytes_remaining = 0;
    current_index = 0;

#if TIME_BREAKDOWN
    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
        end_time = MPI_Wtime();
        total = end_time-start_time;
        printf ("%d: Preprocessing --- %f\n", fh->f_rank, total);
        total = 0;
    }
#endif

    for (index = 0; index < cycles; index++) {
        int k;
        
#if TIME_BREAKDOWN
        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
            start_time = MPI_Wtime();
        }
#endif

        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
            memset(displs, 0x0, fh->f_procs_per_group*sizeof(int));
            memset(bytes_per_process, 0x0, fh->f_procs_per_group*sizeof(int));
        }

        if (cycles-1 == index) {
            bytes_to_write_in_cycle = total_bytes - bytes_per_cycle*index;
        }
        else {
            bytes_to_write_in_cycle = bytes_per_cycle;
        }
        /*
        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
            printf ("****%d: CYCLE %d   Bytes %lld**********\n",
                    fh->f_rank,
                    index, 
                    bytes_to_write_in_cycle);
        }
        */
        /**********************************************************
         **Gather the Data from all the processes at the writers **
         *********************************************************/

        /* Calculate how much data will be contributed in this cycle 
           by each process*/
        bytes_sent = 0;

        while (bytes_to_write_in_cycle) {
            blocks = fview_count[0];
            for (j=0 ; j<fh->f_procs_per_group ; j++) {
                if (sorted[current_index] < blocks) {
                    n = j;
                    break;
                }
                else {
                    blocks += fview_count[j+1];
                }
            }
            if (bytes_remaining) {
                if (bytes_remaining <= bytes_to_write_in_cycle) {
                    if (fh->f_procs_in_group[fh->f_aggregator_index] == 
                        fh->f_rank) {
                        bytes_per_process[n] += bytes_remaining;
                    }
                    if (fh->f_procs_in_group[n] == fh->f_rank) {
                        bytes_sent += bytes_remaining;
                    }
                    current_index ++;
                    bytes_to_write_in_cycle -= bytes_remaining;
                    bytes_remaining = 0;
                    continue;
                }
                else {
                    if (fh->f_procs_in_group[fh->f_aggregator_index] == 
                        fh->f_rank) {
                        bytes_per_process[n] += bytes_to_write_in_cycle;
                    }
                    if (fh->f_procs_in_group[n] == fh->f_rank) {
                        bytes_sent += bytes_to_write_in_cycle;
                    }
                    bytes_remaining -= bytes_to_write_in_cycle;
                    bytes_to_write_in_cycle = 0;
                    break;
                }
            }
            else {
                if (bytes_to_write_in_cycle < 
                    global_fview[sorted[current_index]].iov_len) {
                    if (fh->f_procs_in_group[fh->f_aggregator_index] == 
                        fh->f_rank) {
                        bytes_per_process[n] += bytes_to_write_in_cycle;
                    }
                    if (fh->f_procs_in_group[n] == fh->f_rank) {
                        bytes_sent += bytes_to_write_in_cycle;
                    }
                    bytes_remaining = global_fview[sorted[current_index]].iov_len - 
                        bytes_to_write_in_cycle;
                    bytes_to_write_in_cycle = 0;
                    break;
                }
                else {
                    if (fh->f_procs_in_group[fh->f_aggregator_index] == 
                        fh->f_rank) {
                        bytes_per_process[n] += 
                            global_fview[sorted[current_index]].iov_len;
                    }
                    if (fh->f_procs_in_group[n] == fh->f_rank) {
                        bytes_sent += global_fview[sorted[current_index]].iov_len;
                    }
                    bytes_to_write_in_cycle -= 
                        global_fview[sorted[current_index]].iov_len;
                    current_index ++;
                    continue;
                }
            }
        }
        /* Calculate the displacement on where to put the data and allocate
           the recieve buffer (global_buf) */
        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
            displs[0] = 0;
            global_count = bytes_per_process[0];
            for (i=1 ; i<fh->f_procs_per_group ; i++) {
                global_count += bytes_per_process[i];
                displs[i] = displs[i-1] + bytes_per_process[i-1];
            }
            /*
            for (i=0 ; i<fh->f_procs_per_group ; i++) {
                printf ("Proc %d sending %lld at %lld\n",
                        i,
                        bytes_per_process[i],
                        displs[i]);
            }
            */
            global_buf = malloc (global_count);
            if (NULL == global_buf) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }

        if (fh->f_flags & OMPIO_CONTIGUOUS_MEMORY) {
            send_buf = &((char*)buf)[total_bytes_written];
        }
        else if (bytes_sent) {
            /* allocate a send buffer and copy the data that needs
               to be sent into it in case the data is non-contigous
               in memory */
            OPAL_PTRDIFF_TYPE mem_address;
            size_t remaining = 0;
            size_t temp_position = 0;

            send_buf = malloc (bytes_sent);
            if (NULL == send_buf) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            remaining = bytes_sent;

            while (remaining) {
                mem_address = (OPAL_PTRDIFF_TYPE)
                    (decoded_iov[iov_index].iov_base) + current_position;

                if (remaining >= 
                    (decoded_iov[iov_index].iov_len - current_position)) {
                    memcpy (send_buf+temp_position,
                            (IOVBASE_TYPE *)mem_address,
                            decoded_iov[iov_index].iov_len - current_position);
                    remaining = remaining - 
                        (decoded_iov[iov_index].iov_len - current_position);
                    temp_position = temp_position +
                        (decoded_iov[iov_index].iov_len - current_position);
                    iov_index = iov_index + 1;
                    current_position = 0;
                }
                else {
                    memcpy (send_buf+temp_position,
                            (IOVBASE_TYPE *) mem_address,
                            remaining);
                    current_position = current_position + remaining;
                    remaining = 0;
                }
            }
        }
        total_bytes_written += bytes_sent;

        /* Get the data from all processes to the writer*/
        ompi_io_ompio_gatherv_array (send_buf,
                                     bytes_sent,
                                     MPI_BYTE,
                                     global_buf,
                                     bytes_per_process,
                                     displs,
                                     MPI_BYTE,
                                     fh->f_aggregator_index,
                                     fh->f_procs_in_group,
                                     fh->f_procs_per_group,
                                     fh->f_comm);

        if (! (fh->f_flags & OMPIO_CONTIGUOUS_MEMORY)) {
            if (NULL != send_buf) {
                free (send_buf);
                send_buf = NULL;
            }
        }
        /*                        
        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank)
            for (i=0 ; i<global_count/4 ; i++)
                printf ("RECV %d \n", 
                        ((int *)global_buf)[i]);
        */
        /**********************************************************
         **************** DONE GATHERING OF DATA ******************
         *********************************************************/

        /**********************************************************
         ******* Create the io array, and pass it to fbtl *********
         *********************************************************/
        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
            MPI_Aint bytes_to_write = global_count;
            MPI_Aint *temp = NULL;
            int block = 1;
            k = 0;

            temp = (MPI_Aint *)malloc (sizeof(MPI_Aint) * fh->f_procs_per_group);
            if (NULL == temp) {
                opal_output(1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            memset(temp, 0x0, fh->f_procs_per_group*sizeof(MPI_Aint));
            fh->f_io_array = (mca_io_ompio_io_array_t *) malloc 
                (OMPIO_IOVEC_INITIAL_SIZE * sizeof (mca_io_ompio_io_array_t));
            if (NULL == fh->f_io_array) {
                opal_output(1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            
            while (bytes_to_write) {
                int start = 0;

                if (OMPIO_IOVEC_INITIAL_SIZE*block <= k) {
                    block ++;
                    fh->f_io_array = (mca_io_ompio_io_array_t *)realloc
                        (fh->f_io_array, OMPIO_IOVEC_INITIAL_SIZE * block *
                         sizeof(mca_io_ompio_io_array_t));
                    if (NULL == fh->f_io_array) {
                        opal_output(1, "OUT OF MEMORY\n");
                        return OMPI_ERR_OUT_OF_RESOURCE;
                    }
                }
                
                blocks = fview_count[0];
                for (j=0 ; j<fh->f_procs_per_group ; j++) {
                    if (sorted[x] < blocks) {
                        n = j;
                        break;
                    }
                    else {
                        blocks += fview_count[j+1];
                    }
                }
                for (j=0 ; j<n ; j++) {
                    start += bytes_per_process[j];
                }
 
                if (bytes_left) {
                    if (bytes_left <= bytes_to_write) {
                        fh->f_io_array[k].offset = (IOVBASE_TYPE *)
                            ((OPAL_PTRDIFF_TYPE)global_fview[sorted[x]].iov_base + 
                             (global_fview[sorted[x]].iov_len - bytes_left));
                        fh->f_io_array[k].length = bytes_left;
                        fh->f_io_array[k].memory_address = 
                            &global_buf[start+temp[n]];
                        temp[n] += fh->f_io_array[k].length;
                        bytes_to_write -= bytes_left;
                        bytes_left = 0;
                        k ++;
                        x ++;
                        continue;
                    }
                    else {
                        fh->f_io_array[k].offset = (IOVBASE_TYPE *) 
                            ((OPAL_PTRDIFF_TYPE)global_fview[sorted[x]].iov_base + 
                             (global_fview[sorted[x]].iov_len - bytes_left));
                        fh->f_io_array[k].length = bytes_to_write;
                        fh->f_io_array[k].memory_address = 
                            &global_buf[start+temp[n]];
                        temp[n] += fh->f_io_array[k].length;
                        bytes_left -= bytes_to_write;
                        bytes_to_write = 0;;
                        k ++;
                        break;
                    }
                }
                else {
                    if (bytes_to_write < global_fview[sorted[x]].iov_len) {
                        fh->f_io_array[k].offset = global_fview[sorted[x]].iov_base;
                        fh->f_io_array[k].length = bytes_to_write;
                        fh->f_io_array[k].memory_address = 
                            &global_buf[start+temp[n]];
                        bytes_left = 
                            global_fview[sorted[x]].iov_len - bytes_to_write;
                        bytes_to_write = 0;
                        k ++;
                        break;
                    }
                    else {
                        fh->f_io_array[k].offset = global_fview[sorted[x]].iov_base;
                        fh->f_io_array[k].length = global_fview[sorted[x]].iov_len;
                        fh->f_io_array[k].memory_address = 
                            &global_buf[start+temp[n]];
                        temp[n] += fh->f_io_array[k].length;
                        bytes_to_write -= global_fview[sorted[x]].iov_len;
                        k ++;
                        x ++;
                        continue;
                    }
                }
            }

            fh->f_num_of_io_entries = k;
            /*
            printf("*************************** %d\n", fh->f_num_of_io_entries);
            for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
                printf(" ADDRESS: %p  OFFSET: %p   LENGTH: %d\n",
                       fh->f_io_array[i].memory_address,
                       fh->f_io_array[i].offset,
                       fh->f_io_array[i].length);
            }
            */
#if TIME_BREAKDOWN
            if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
                start_time2 = MPI_Wtime();
            }
#endif
            if (fh->f_num_of_io_entries) {
                if (OMPI_SUCCESS != fh->f_fbtl->fbtl_pwritev (fh, NULL)) {
                    opal_output (1, "WRITE FAILED\n");
                    return OMPI_ERROR;
                }
            }
#if TIME_BREAKDOWN
            if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
                end_time2 = MPI_Wtime();
                total_io += end_time2-start_time2;
            }
#endif
            if (NULL != temp) {
                free (temp);
                temp = NULL;
            }
        }
        /**********************************************************
         ******************** DONE WRITING ************************
         *********************************************************/

        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
            fh->f_num_of_io_entries = 0;
            if (NULL != fh->f_io_array) {
                free (fh->f_io_array);
                fh->f_io_array = NULL;
            }
            if (NULL != global_buf) {
                free (global_buf);
                global_buf = NULL;
            }
        }
#if TIME_BREAKDOWN
        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
            end_time = MPI_Wtime();
            total += end_time-start_time;
        }
#endif
    }
#if TIME_BREAKDOWN
    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
        printf ("%d: Total --- %f     I/O ---- %f\n", fh->f_rank, total, total_io);
    }
#endif

    if (NULL != sorted) {
        free (sorted);
        sorted = NULL;
    }
    if (NULL != global_fview) {
        free (global_fview);
        global_fview = NULL;
    }
    if (NULL != fview_count) {
        free (fview_count);
        fview_count = NULL;
    }
    if (NULL != decoded_iov) {
        free (decoded_iov);
        decoded_iov = NULL;
    }
    if (NULL != bytes_per_process) {
        free (bytes_per_process);
        bytes_per_process = NULL;
    }
    if (NULL != displs) {
        free (displs);
        displs = NULL;
    }

    return OMPI_SUCCESS;
}
