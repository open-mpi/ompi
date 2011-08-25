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
#include "fcoll_static.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/io/ompio/io_ompio.h"
#include "ompi/mca/io/io.h"
#include "math.h"
#include "ompi/mca/pml/pml.h"
#include <unistd.h>

int
mca_fcoll_static_file_write_all (mca_io_ompio_file_t *fh, 
                                 void *buf, 
                                 int count, 
                                 struct ompi_datatype_t *datatype, 
                                 ompi_status_public_t *status)
{
    MPI_Aint total_bytes_written = 0;  /* total bytes that have been written*/
    MPI_Aint bytes_to_write_in_cycle = 0; /* left to be written in a cycle*/
    size_t bytes_per_cycle = 0;      /* total written in each cycle by each process*/

    int index = 0;
    int cycles = 0, local_cycles = 0;
    int i=0;
    int ret;

    /* iovec structure and count of the buffer passed in */
    uint32_t iov_count = 0;
    struct iovec *decoded_iov = NULL;
    uint32_t iov_index = 0;
    size_t current_position = 0;
    char *send_buf = NULL;

    /* global iovec at the writers that contain the iovecs created from
       file_set_view */
    uint32_t global_iov_count = 0;
    struct iovec *global_iov = NULL;
    int *sorted;

    char *global_buf = NULL;
    MPI_Aint global_count = 0;

    int *displs = NULL;

    size_t max_data = 0;
    int *iovec_count_per_process = NULL;
    int *bytes_per_process = NULL;

    if (opal_datatype_is_contiguous_memory_layout(&datatype->super,1)) {
        fh->f_flags |= OMPIO_CONTIGUOUS_MEMORY;
    }

    /* In case the data is not contigous in memory, decode it into an iovec */
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
                                            mca_fcoll_static_num_io_procs,
                                            max_data);
    }

    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
        bytes_per_process = (int *)malloc (sizeof(int)*fh->f_procs_per_group);
        if (NULL == bytes_per_process) {
            opal_output (1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        iovec_count_per_process = (int *)malloc (sizeof(int)*fh->f_procs_per_group);
        if (NULL == iovec_count_per_process) {
            opal_output (1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        displs = (int*)malloc (fh->f_procs_per_group*sizeof(int));
        if (NULL == displs) {
            opal_output (1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    /*
     * Calculate how many bytes are written in each cycle
     */
    if (mca_fcoll_static_constant_cbs) {
        bytes_per_cycle = 
            mca_fcoll_static_cycle_buffer_size/fh->f_procs_per_group;
    }
    else {
        bytes_per_cycle = mca_fcoll_static_cycle_buffer_size;
    }

    /* TODO : number of cycles has to be the same for all processes in a group,
       so need to Allreduce the cycle within a group.
       This works now if all processes are writing the same amount of data */

    cycles = ceil((double)max_data/bytes_per_cycle);
    local_cycles = cycles;
    ret = fh->f_comm->c_coll.coll_allreduce (&local_cycles, 
                                             &cycles,
                                             1,
                                             MPI_INT,
                                             MPI_MAX,
                                             fh->f_comm,
                                             fh->f_comm->c_coll.coll_allreduce_module);
#if 0
    printf ("Max Data: %d  Bytes per Process: %d   Cycles: %d  Procs_per_group %d\n",
            max_data, bytes_per_cycle, cycles, fh->f_procs_per_group);
#endif
    for (index = 0; index < cycles; index++) {
        struct iovec *iov = NULL;
        int iov_size = 0;

        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
            /*printf ("********** CYCLE %d **************\n",index);*/
            memset(displs, 0x0, fh->f_procs_per_group*sizeof(int));
            memset(bytes_per_process, 0x0, fh->f_procs_per_group*sizeof(int));
            memset(iovec_count_per_process, 0x0, fh->f_procs_per_group*sizeof(int));
        }

        if (local_cycles > index) {
            if ((index == local_cycles-1) && (max_data % bytes_per_cycle)) {
                bytes_to_write_in_cycle = max_data % bytes_per_cycle;
            }
            else if (max_data <= bytes_per_cycle) {
                bytes_to_write_in_cycle = max_data;
            }
            else {
                bytes_to_write_in_cycle = bytes_per_cycle;
            }
        }
        else {
            bytes_to_write_in_cycle = 0;
        }
        /*
        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
            printf ("****%d: CYCLE %d   Bytes %d**********\n",
                    fh->f_rank,
                    index, 
                    bytes_to_write_in_cycle);
        }
        */

        /**********************************************************
         **Gather the Data from all the processes at the writers **
         *********************************************************/

        /* gather from each process how many bytes each will be sending */
        ompi_io_ompio_gather_array (&bytes_to_write_in_cycle,
                                    1,
                                    MPI_INT,
                                    bytes_per_process,
                                    1,
                                    MPI_INT,
                                    fh->f_aggregator_index,
                                    fh->f_procs_in_group,
                                    fh->f_procs_per_group,
                                    fh->f_comm);

        /* Calculate the displacement on where to put the data and allocate
           the recieve buffer (global_buf) */
        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
            displs[0] = 0;
            global_count = bytes_per_process[0];
            for (i=1 ; i<fh->f_procs_per_group ; i++) {
                global_count += bytes_per_process[i];
                displs[i] = displs[i-1] + bytes_per_process[i-1];
            }

            global_buf = malloc (global_count);
            if (NULL == global_buf) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }

        if (fh->f_flags & OMPIO_CONTIGUOUS_MEMORY) {
            send_buf = &((char*)buf)[total_bytes_written];
        }
        else if (bytes_to_write_in_cycle) {
            /* allocate a send buffer and copy the data that needs
               to be sent into it in case the data is non-contigous
               in memory */
            OPAL_PTRDIFF_TYPE mem_address;
            size_t remaining = 0;
            size_t temp_position = 0;

            send_buf = malloc (bytes_to_write_in_cycle);
            if (NULL == send_buf) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            remaining = bytes_to_write_in_cycle;

            while (remaining && (iov_count > iov_index)) {
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
                            (IOVBASE_TYPE *)mem_address,
                            remaining);
                    current_position = current_position + remaining;
                    remaining = 0;
                }
            }
        }
        total_bytes_written += bytes_to_write_in_cycle;

        /* Get the data from all processes to the writer*/
        ompi_io_ompio_gatherv_array (send_buf,
                                     bytes_to_write_in_cycle,
                                     MPI_BYTE,
                                     global_buf,
                                     bytes_per_process,
                                     displs,
                                     MPI_BYTE,
                                     fh->f_aggregator_index,
                                     fh->f_procs_in_group,
                                     fh->f_procs_per_group,
                                     fh->f_comm);

        if (!(fh->f_flags & OMPIO_CONTIGUOUS_MEMORY)) {
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
         Gather from each process iovecs to where to write the data
        *********************************************************/
        iov_size = 0;
        ompi_io_ompio_generate_current_file_view (fh, 
                                                  bytes_to_write_in_cycle, 
                                                  &iov, 
                                                  &iov_size);
        /*
        for (i=0 ; i<iov_size ; i++) {
            printf("%d: OFFSET: %p   LENGTH: %d\n",
                   fh->f_rank,
                   iov[i].iov_base,
                   iov[i].iov_len);
        }
        */
        ompi_io_ompio_gather_array (&iov_size,
                                    1,
                                    MPI_INT,
                                    iovec_count_per_process,
                                    1,
                                    MPI_INT,
                                    fh->f_aggregator_index,
                                    fh->f_procs_in_group,
                                    fh->f_procs_per_group,
                                    fh->f_comm);
        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
            displs[0] = 0;
            global_iov_count = iovec_count_per_process[0];
            for (i=1 ; i<fh->f_procs_per_group ; i++) {
                global_iov_count += iovec_count_per_process[i];
                displs[i] = displs[i-1] + iovec_count_per_process[i-1];
            }
        }

        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
            global_iov = (struct iovec*)malloc (global_iov_count * 
                                                sizeof(struct iovec));
            if (NULL == global_iov) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }

        if (fh->f_flags & OMPIO_UNIFORM_FVIEW) {
            ompi_io_ompio_gather_array (iov,
                                        iov_size,
                                        fh->f_iov_type,
                                        global_iov,
                                        iov_size,
                                        fh->f_iov_type,
                                        fh->f_aggregator_index,
                                        fh->f_procs_in_group,
                                        fh->f_procs_per_group,
                                        fh->f_comm);
        }
        else {
            ompi_io_ompio_gatherv_array (iov,
                                         iov_size,
                                         fh->f_iov_type,
                                         global_iov,
                                         iovec_count_per_process,
                                         displs,
                                         fh->f_iov_type,
                                         fh->f_aggregator_index,
                                         fh->f_procs_in_group,
                                         fh->f_procs_per_group,
                                         fh->f_comm);
        }

        /* sort it */
        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
            sorted = (int *)malloc (global_iov_count * sizeof(int));
            if (NULL == sorted) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            ompi_io_ompio_sort_iovec (global_iov, global_iov_count, sorted);
        }

        /**********************************************************
         **************** DONE GATHERING OF IOVECS ****************
         *********************************************************/

        /**********************************************************
         ** Create the io array, sort it,  and pass it to fbtl ****
         *********************************************************/
        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
            MPI_Aint temp = 0;
            int x = 0, k = 0;

            fh->f_io_array = (mca_io_ompio_io_array_t *) malloc 
                (global_iov_count * sizeof (mca_io_ompio_io_array_t));
            if (NULL == fh->f_io_array) {
                opal_output(1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            for (i=0 ; i<fh->f_procs_per_group ; i++) {
                for (x=0 ; x<iovec_count_per_process[i] ; x++) {
                    fh->f_io_array[k].offset = global_iov[sorted[k]].iov_base;
                    fh->f_io_array[k].length = global_iov[sorted[k]].iov_len;
                    fh->f_io_array[k].memory_address = &global_buf[temp];
                    temp += fh->f_io_array[k].length;
                    k ++;
                }
            }
            fh->f_num_of_io_entries = k;

            /*
            printf("*************************** %d\n", fh->f_num_of_io_entries);
            for (i=0 ; i<fh->f_num_of_io_entries ; i++)
            {
                printf(" ADDRESS: %p  OFFSET: %p   LENGTH: %d\n",
                       fh->f_io_array[i].memory_address,
                       fh->f_io_array[i].offset,
                       fh->f_io_array[i].length);
            }
            
            printf("******* SORTED ************ %d\n", fh->f_num_of_io_entries);
            for (i=0 ; i<fh->f_num_of_io_entries ; i++)
            {
                printf(" ADDRESS: %p  OFFSET: %p   LENGTH: %d\n",
                       fh->f_io_array[sorted[i]].memory_address,
                       fh->f_io_array[sorted[i]].offset,
                       fh->f_io_array[sorted[i]].length);
            }
            */
            if (fh->f_num_of_io_entries) {
                fh->f_fbtl->fbtl_pwritev (fh, NULL);
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
            if (NULL != global_iov) {
                free (global_iov);
                global_iov = NULL;
            }
            if (NULL != global_buf) {
                free (global_buf);
                global_buf = NULL;
            }
            if (NULL != sorted) {
                free (sorted);
                sorted = NULL;
            }
        }
        if (NULL != iov) {
            free (iov);
            iov = NULL;
        }
    }
    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
        if (NULL != iovec_count_per_process) {
            free (iovec_count_per_process);
            iovec_count_per_process = NULL;
        }
        if (NULL != bytes_per_process) {
            free (bytes_per_process);
            bytes_per_process = NULL;
        }
        if (NULL != displs) {
            free (displs);
            displs = NULL;
        }
    }

    return OMPI_SUCCESS;
}
