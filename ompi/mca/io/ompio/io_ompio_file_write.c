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
 *  Copyright (c) 2008-2013 University of Houston. All rights reserved.
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
#include "ompi/mca/fbtl/fbtl.h"
#include "ompi/mca/fbtl/base/base.h"
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/sharedfp/base/base.h"

#include "io_ompio.h"
#include "math.h"
#include <unistd.h>

int
mca_io_ompio_file_write (ompi_file_t *fp,
                         void *buf,
                         int count,
                         struct ompi_datatype_t *datatype,
                         ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    ret = ompio_io_ompio_file_write(fh,buf,count,datatype,status);
    return ret;
}

int
ompio_io_ompio_file_write (mca_io_ompio_file_t *fh,
                           void *buf,
                           int count,
                           struct ompi_datatype_t *datatype,
                           ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;

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

    if ( 0 == count ) {
	if ( MPI_STATUS_IGNORE != status ) {
	    status->_ucount = 0;
	}
	return ret;
    }

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
            fh->f_io_array[k].offset = (IOVBASE_TYPE *)(intptr_t)(disp + fh->f_offset);

            if (! (fh->f_flags & OMPIO_CONTIGUOUS_FVIEW)) { 
                if (fh->f_decoded_iov[j].iov_len - 
                    (fh->f_total_bytes - sum_previous_length) 
                    < fh->f_io_array[k].length) {
                    fh->f_io_array[k].length = fh->f_decoded_iov[j].iov_len - 
                        (fh->f_total_bytes - sum_previous_length);
                }
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

    if ( MPI_STATUS_IGNORE != status ) {
	status->_ucount = max_data;
    }

    return ret;
}

int
mca_io_ompio_file_write_at (ompi_file_t *fh,
                            OMPI_MPI_OFFSET_TYPE offset,
                            void *buf,
                            int count,
                            struct ompi_datatype_t *datatype,
                            ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = ompio_io_ompio_file_write_at (&data->ompio_fh, offset,buf,count,datatype,status);
    return ret;
}

int
ompio_io_ompio_file_write_at (mca_io_ompio_file_t *fh,
                              OMPI_MPI_OFFSET_TYPE offset,
                              void *buf,
                              int count,
                              struct ompi_datatype_t *datatype,
                              ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    ompi_io_ompio_set_explicit_offset (fh, offset);

    ret = ompio_io_ompio_file_write (fh,
                                     buf,
                                     count,
                                     datatype,
                                     status);

    return ret;
}

int
mca_io_ompio_file_write_all (ompi_file_t *fh,
                             void *buf,
                             int count,
                             struct ompi_datatype_t *datatype,
                             ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = data->ompio_fh.
        f_fcoll->fcoll_file_write_all (&data->ompio_fh, 
                                       buf, 
                                       count, 
                                       datatype,
                                       status);
    
    if ( MPI_STATUS_IGNORE != status ) {
	size_t size;

	opal_datatype_type_size (&datatype->super, &size);
	status->_ucount = count * size;
    }

    return ret;
}

int
mca_io_ompio_file_write_all_begin (ompi_file_t *fh,
                                   void *buf,
                                   int count,
                                   struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = data->ompio_fh.
        f_fcoll->fcoll_file_write_all_begin (&data->ompio_fh, 
                                            buf, 
                                            count,
                                            datatype);

    return ret;
}

int
mca_io_ompio_file_write_all_end (ompi_file_t *fh,
                                 void *buf,
                                 ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;

    ret = data->ompio_fh.
        f_fcoll->fcoll_file_write_all_end (&data->ompio_fh, 
                                          buf, 
                                          status);
    
    return ret;
}

int
mca_io_ompio_file_write_at_all (ompi_file_t *fh,
                                OMPI_MPI_OFFSET_TYPE offset,
                                void *buf,
                                int count,
                                struct ompi_datatype_t *datatype,
                                ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_write_at_all(&data->ompio_fh,offset,buf,count,datatype,status);

    return ret;
}

int
ompio_io_ompio_file_write_at_all (mca_io_ompio_file_t *fh,
                                  OMPI_MPI_OFFSET_TYPE offset,
                                  void *buf,
                                  int count,
                                  struct ompi_datatype_t *datatype,
                                  ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;

    ompi_io_ompio_set_explicit_offset (fh, offset);

    ret = fh->f_fcoll->fcoll_file_write_all (fh,
                                             buf,
                                             count,
                                             datatype,
                                             status);
    return ret;
}

int
mca_io_ompio_file_write_at_all_begin (ompi_file_t *fh,
                                      OMPI_MPI_OFFSET_TYPE offset,
                                      void *buf,
                                      int count,
                                      struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_write_at_all_begin(&data->ompio_fh,offset,buf,count,datatype);

    return ret;
}

int
ompio_io_ompio_file_write_at_all_begin (mca_io_ompio_file_t *fh,
                                        OMPI_MPI_OFFSET_TYPE offset,
                                        void *buf,
                                        int count,
                                        struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;

    ompi_io_ompio_set_explicit_offset (fh, offset);

    ret = fh->f_fcoll->fcoll_file_write_all_begin (fh,
                                                   buf,
                                                   count,
                                                   datatype);

    return ret;
}

int
mca_io_ompio_file_write_at_all_end (ompi_file_t *fh,
                                    void *buf,
                                    ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_write_at_all_end(&data->ompio_fh,buf,status);

    return ret;
}

int
ompio_io_ompio_file_write_at_all_end (mca_io_ompio_file_t *fh,
                                      void *buf,
                                      ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;

    ret = fh->f_fcoll->fcoll_file_write_all_end (fh,
                                                 buf,
                                                 status);

    return ret;
}

int
mca_io_ompio_file_iwrite (ompi_file_t *fp,
                          void *buf,
                          int count,
                          struct ompi_datatype_t *datatype,
                          ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    ret = ompio_io_ompio_file_iwrite(&data->ompio_fh,buf,count,datatype,request);

    return ret;
}

int
ompio_io_ompio_file_iwrite (mca_io_ompio_file_t *fh,
                            void *buf,
                            int count,
                            struct ompi_datatype_t *datatype,
                            ompi_request_t **request)
{

    int ret = MPI_ERR_OTHER;
    return ret;
}

int
mca_io_ompio_file_iwrite_at (ompi_file_t *fh,
                             OMPI_MPI_OFFSET_TYPE offset,
                             void *buf,
                             int count,
                             struct ompi_datatype_t *datatype,
                             ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
        
    data = (mca_io_ompio_data_t *) fh->f_io_selected_data;
    ret = ompio_io_ompio_file_iwrite_at(&data->ompio_fh,offset,buf,count,datatype,request);

    return ret;
}

int
ompio_io_ompio_file_iwrite_at (mca_io_ompio_file_t *fh,
                             OMPI_MPI_OFFSET_TYPE offset,
                             void *buf,
                             int count,
                             struct ompi_datatype_t *datatype,
                             ompi_request_t **request)
{

    int ret = OMPI_SUCCESS;

    ompi_io_ompio_set_explicit_offset (fh, offset);
    ret = ompio_io_ompio_file_iwrite (fh,
                                    buf,
                                    count,
                                    datatype,
                                    request);
    return ret;
}

int
mca_io_ompio_file_write_shared (ompi_file_t *fp,
                                void *buf,
                                int count,
                                struct ompi_datatype_t *datatype,
                                ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    ret = shared_fp_base_module->sharedfp_write(fh,buf,count,datatype,status);

    return ret;
}

int
mca_io_ompio_file_iwrite_shared (ompi_file_t *fp,
                                 void *buf,
                                 int count,
                                 struct ompi_datatype_t *datatype,
                                 ompi_request_t **request)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    ret = shared_fp_base_module->sharedfp_iwrite(fh,buf,count,datatype,request);

    return ret;
}

int
mca_io_ompio_file_write_ordered (ompi_file_t *fp,
                                 void *buf,
                                 int count,
                                 struct ompi_datatype_t *datatype,
                                 ompi_status_public_t * status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    ret = shared_fp_base_module->sharedfp_write_ordered(fh,buf,count,datatype,status);

    return ret;
}

int
mca_io_ompio_file_write_ordered_begin (ompi_file_t *fp,
                                       void *buf,
                                       int count,
                                       struct ompi_datatype_t *datatype)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    ret = shared_fp_base_module->sharedfp_write_ordered_begin(fh,buf,count,datatype);

    return ret;
}

int
mca_io_ompio_file_write_ordered_end (ompi_file_t *fp,
                                     void *buf,
                                     ompi_status_public_t *status)
{
    int ret = OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    mca_sharedfp_base_module_t * shared_fp_base_module;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    /*get the shared fp module associated with this file*/
    shared_fp_base_module = fh->f_sharedfp;
    ret = shared_fp_base_module->sharedfp_write_ordered_end(fh,buf,status);

    return ret;
}
