/*
 *  Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                          University Research and Technology
 *                          Corporation.  All rights reserved.
 *  Copyright (c) 2004-2005 The University of Tennessee and The University
 *                          of Tennessee Research Foundation.  All rights
 *                          reserved.
 *  Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                          University of Stuttgart.  All rights reserved.
 *  Copyright (c) 2004-2005 The Regents of the University of California.
 *                          All rights reserved.
 *  Copyright (c) 2008-2011 University of Houston. All rights reserved.
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
#include "ompi/mca/fcoll/dynamic/fcoll_dynamic.h"
#include "ompi/mca/fcoll/static/fcoll_static.h"
#include "ompi/mca/fcoll/individual/fcoll_individual.h"
#include "ompi/mca/fcoll/two_phase/fcoll_two_phase.h"
#include "ompi/mca/fcoll/ylib/fcoll_ylib.h"
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/fbtl/base/base.h"

#include "io_ompio.h"
#include "math.h"

int
mca_io_ompio_file_read (ompi_file_t *fp,
                        void *buf,
                        int count,
                        struct ompi_datatype_t *datatype,
                        ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;

    size_t total_bytes_read = 0;       /* total bytes that have been read*/
    size_t bytes_to_read_in_cycle = 0; /* left to be read in a cycle*/
    size_t bytes_per_cycle = 0;        /* total read in each cycle by each process*/
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

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    ompi_io_ompio_decode_datatype (fh, 
                                   datatype, 
                                   count, 
                                   buf, 
                                   &max_data, 
                                   &decoded_iov, 
                                   &iov_count);

    bytes_per_cycle = mca_io_ompio_cycle_buffer_size;
    cycles = ceil((float)max_data/bytes_per_cycle);

#if 0
    printf ("Bytes per Cycle: %d   Cycles: %d\n",bytes_per_cycle, cycles);
#endif

    sum_previous_length = fh->f_position_in_file_view;
    j = fh->f_index_in_file_view;

    for (index = 0; index < cycles; index++) {
        OPAL_PTRDIFF_TYPE disp;
        int block = 1;
        k = 0;
        if ((index == cycles-1) && (max_data % bytes_per_cycle)) {
            bytes_to_read_in_cycle = max_data % bytes_per_cycle;
        }
        else {
            bytes_to_read_in_cycle = bytes_per_cycle;
        }

        /*
        ompi_io_ompio_create_list (fh->f_decoded_iov, fh->f_iov_count,
                                   decoded_iov, iov_count,
                                   &total_bytes_read, &bytes_to_read_in_cycle,
                                   &sum_previous_counts, &sum_previous_length,
                                   &decoded_iov_index, &fview_iov_index,
                                   &fh->f_io_array, &fh->f_num_of_io_entries);
        */

        fh->f_io_array = (mca_io_ompio_io_array_t *)malloc 
            (OMPIO_IOVEC_INITIAL_SIZE * sizeof (mca_io_ompio_io_array_t));
        if (NULL == fh->f_io_array) {
            opal_output(1, "OUT OF MEMORY\n");
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        while (bytes_to_read_in_cycle) {
            /* reallocate if needed */
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
                (total_bytes_read - sum_previous_counts) <= 0) {
                sum_previous_counts += decoded_iov[i].iov_len;
                i = i + 1;
            }

            disp = (OPAL_PTRDIFF_TYPE)decoded_iov[i].iov_base + 
                (total_bytes_read - sum_previous_counts);
            fh->f_io_array[k].memory_address = (IOVBASE_TYPE *)disp;
            
            if (decoded_iov[i].iov_len - 
                (total_bytes_read - sum_previous_counts) >= 
                bytes_to_read_in_cycle) {
                fh->f_io_array[k].length = bytes_to_read_in_cycle;
            }
            else {
                fh->f_io_array[k].length =  decoded_iov[i].iov_len - 
                    (total_bytes_read - sum_previous_counts);
            }
            if (! (fh->f_flags & OMPIO_CONTIGUOUS_FVIEW)) {
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
            }

            disp = (OPAL_PTRDIFF_TYPE)fh->f_decoded_iov[j].iov_base + 
                (fh->f_total_bytes - sum_previous_length);
            fh->f_io_array[k].offset = (IOVBASE_TYPE *)(disp + fh->f_offset);
            
            if (! (fh->f_flags & OMPIO_CONTIGUOUS_FVIEW)) {
                if (fh->f_decoded_iov[j].iov_len - 
                    (fh->f_total_bytes - sum_previous_length) 
                    < fh->f_io_array[k].length) {
                    fh->f_io_array[k].length = fh->f_decoded_iov[j].iov_len - 
                        (fh->f_total_bytes - sum_previous_length);
                }
            }

            total_bytes_read += fh->f_io_array[k].length;
            fh->f_total_bytes += fh->f_io_array[k].length;
            bytes_to_read_in_cycle -= fh->f_io_array[k].length;
            k = k + 1;
        }
        fh->f_position_in_file_view = sum_previous_length;
        fh->f_index_in_file_view = j;
        fh->f_num_of_io_entries = k;

#if 0
        if (fh->f_rank == 0) {
            int i;
            printf("*************************** %d\n", fh->f_num_of_io_entries);

            for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
                printf(" ADDRESS: %p  OFFSET: %p   LENGTH: %d\n",
                       fh->f_io_array[i].memory_address,
                       fh->f_io_array[i].offset,
                       fh->f_io_array[i].length);
            }
        }
#endif

        if (fh->f_num_of_io_entries) {
            fh->f_fbtl->fbtl_preadv (fh, NULL);
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

    return ret;
}

int
mca_io_ompio_file_read_at (ompi_file_t *fh,
                           OMPI_MPI_OFFSET_TYPE offset,
                           void *buf,
                           int count,
                           struct ompi_datatype_t *datatype,
                           ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ompi_io_ompio_set_explicit_offset (&data->ompio_fh, offset);

    mca_io_ompio_file_read (fh,
                            buf,
                            count,
                            datatype,
                            status);
    return ret;
}

int
mca_io_ompio_file_read_all (ompi_file_t *fh,
                            void *buf,
                            int count,
                            struct ompi_datatype_t *datatype,
                            ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = data->ompio_fh.
        f_fcoll->fcoll_file_read_all (&data->ompio_fh, 
                                     buf, 
                                     count, 
                                     datatype,
                                     status);

    return ret;
}

int
mca_io_ompio_file_read_all_begin (ompi_file_t *fh,
                                  void *buf,
                                  int count,
                                  struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = data->ompio_fh.
        f_fcoll->fcoll_file_read_all_begin (&data->ompio_fh, 
                                           buf, 
                                           count,
                                           datatype);

    return ret;
}

int
mca_io_ompio_file_read_all_end (ompi_file_t *fh,
                                void *buf,
                                ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = data->ompio_fh.
        f_fcoll->fcoll_file_read_all_end (&data->ompio_fh, 
                                         buf, 
                                         status);

    return ret;
}

int
mca_io_ompio_file_read_at_all (ompi_file_t *fh,
                               OMPI_MPI_OFFSET_TYPE offset,
                               void *buf,
                               int count,
                               struct ompi_datatype_t *datatype,
                               ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ompi_io_ompio_set_explicit_offset (&data->ompio_fh, offset);

    ret = data->ompio_fh.
        f_fcoll->fcoll_file_read_all (&data->ompio_fh, 
                                     buf, 
                                     count,
                                     datatype, 
                                     status);
    return ret;
}

int
mca_io_ompio_file_read_at_all_begin (ompi_file_t *fh,
                                     OMPI_MPI_OFFSET_TYPE offset,
                                     void *buf,
                                     int count,
                                     struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ompi_io_ompio_set_explicit_offset (&data->ompio_fh, offset);

    ret = data->ompio_fh.
        f_fcoll->fcoll_file_read_all_begin (&data->ompio_fh, 
                                           buf,
                                           count, 
                                           datatype);

    return ret;
}

int
mca_io_ompio_file_read_at_all_end (ompi_file_t *fh,
                                   void *buf,
                                   ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = data->ompio_fh.
        f_fcoll->fcoll_file_read_all_end (&data->ompio_fh, 
                                         buf, 
                                         status);

    return ret;
}

int
mca_io_ompio_file_iread (ompi_file_t *fh,
                         void *buf,
                         int count,
                         struct ompi_datatype_t *datatype,
                         ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    return ret;
}

int
mca_io_ompio_file_iread_at (ompi_file_t *fh,
                            OMPI_MPI_OFFSET_TYPE offset,
                            void *buf,
                            int count,
                            struct ompi_datatype_t *datatype,
                            ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    return ret;
}

int
mca_io_ompio_file_read_shared (ompi_file_t *fh,
                               void *buf,
                               int count,
                               struct ompi_datatype_t *datatype,
                               ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    return ret;
}

int
mca_io_ompio_file_iread_shared (ompi_file_t *fh,
                                void *buf,
                                int count,
                                struct ompi_datatype_t *datatype,
                                ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    return ret;
}

int
mca_io_ompio_file_read_ordered (ompi_file_t *fh,
                                void *buf,
                                int count,
                                struct ompi_datatype_t *datatype,
                                ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    return ret;
}

int
mca_io_ompio_file_read_ordered_begin (ompi_file_t *fh,
                                      void *buf,
                                      int count,
                                      struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    return ret;
}

int
mca_io_ompio_file_read_ordered_end (ompi_file_t *fh,
                                    void *buf,
                                    ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    return ret;
}


    /*
    if (! (fh->f_flags & OMPIO_AGGREGATOR_IS_SET)) {
        if (!strcmp (data->ompio_fh.f_fcoll_component->mca_component_name,
                     "dynamic")) {
            printf ("Dynamic: %d\n",mca_fcoll_dynamic_num_io_procs);
            ompi_io_ompio_set_aggregator_props (&data->ompio_fh, 
                                                mca_fcoll_dynamic_num_io_procs);
        }
        else if (!strcmp (data->ompio_fh.f_fcoll_component->mca_component_name,
                     "static")) {
            printf ("Static: %d\n",mca_fcoll_static_num_io_procs);
            ompi_io_ompio_set_aggregator_props (&data->ompio_fh, 
                                                mca_fcoll_static_num_io_procs);
        }
        else if (!strcmp (data->ompio_fh.f_fcoll_component->mca_component_name,
                     "two_phase")) {
            printf ("Two Phase: %d\n",mca_fcoll_two_phase_num_io_procs);
            ompi_io_ompio_set_aggregator_props (&data->ompio_fh, 
                                                mca_fcoll_two_phase_num_io_procs);
            mca_fcoll_two_phase_num_io_procs = 
                ceil((float)data->ompio_fh.f_size/data->ompio_fh.f_procs_per_group);
            data->ompio_fh.f_aggregator_index = 
                ceil((float)data->ompio_fh.f_size/mca_fcoll_two_phase_num_io_procs);
        }
        else if (!strcmp (data->ompio_fh.f_fcoll_component->mca_component_name,
                     "ylib")) {
            ompi_io_ompio_set_aggregator_props (&data->ompio_fh, 
                                                mca_fcoll_ylib_num_io_procs);
            mca_fcoll_ylib_num_io_procs = 
                ceil((float)data->ompio_fh.f_size/data->ompio_fh.f_procs_per_group);
            data->ompio_fh.f_aggregator_index = 
                ceil((float)data->ompio_fh.f_size/mca_fcoll_ylib_num_io_procs);
        }
    }
    */
