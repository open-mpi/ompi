/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2021 University of Houston. All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2024      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2024      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "fcoll_vulcan.h"
#include "fcoll_vulcan_internal.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/fcoll/base/fcoll_base_coll_array.h"
#include "ompi/mca/common/ompio/common_ompio.h"
#include "ompi/mca/common/ompio/common_ompio_buffer.h"
#include "ompi/mca/io/io.h"
#include "ompi/mca/common/ompio/common_ompio_request.h"
#include "math.h"
#include "ompi/mca/pml/pml.h"
#include "opal/mca/accelerator/accelerator.h"
#include <unistd.h>

#define DEBUG_ON 0
#define NOT_AGGR_INDEX -1

static int shuffle_init (int index, int cycles, int aggregator, int rank,
                         mca_io_ompio_aggregator_data *data, ompi_request_t **reqs);

static int read_init (ompio_file_t *fh, int index, int cycles, int aggregator, int rank,
		      mca_io_ompio_aggregator_data *aggr_data,
                      int read_syncType, ompi_request_t **request,
                      bool is_accelerator_buffer);

int mca_fcoll_vulcan_file_read_all (struct ompio_file_t *fh,
                                    void *buf,
                                    size_t count,
                                    struct ompi_datatype_t *datatype,
                                    ompi_status_public_t *status)
{
    int index = 0;
    int cycles = 0;
    int ret =0, l, i, j, bytes_per_cycle;
    uint32_t iov_count = 0;
    struct iovec *decoded_iov = NULL;
    struct iovec *local_iov_array=NULL;
    uint32_t total_fview_count = 0;
    int local_count = 0;
    ompi_request_t **reqs = NULL;
    ompi_request_t *req_iread = MPI_REQUEST_NULL;
    ompi_request_t *req_tmp = MPI_REQUEST_NULL;
    mca_io_ompio_aggregator_data **aggr_data=NULL;

    ptrdiff_t *displs = NULL;
    int vulcan_num_io_procs;
    size_t max_data = 0;

    struct iovec **broken_iov_arrays=NULL;
    struct iovec **broken_decoded_iovs=NULL;
    int *broken_counts=NULL;
    int *broken_iov_counts=NULL;
    MPI_Aint *broken_total_lengths=NULL;

    int aggr_index = NOT_AGGR_INDEX;
    int read_sync_type = 2;
    int *result_counts=NULL;

    ompi_count_array_t fview_count_desc;
    ompi_disp_array_t displs_desc;
    int is_gpu, is_managed;
    bool use_accelerator_buffer = false;

#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
    double read_time = 0.0, start_read_time = 0.0, end_read_time = 0.0;
    double comm_time = 0.0, start_comm_time = 0.0, end_comm_time = 0.0;
    double exch_read = 0.0, start_exch = 0.0, end_exch = 0.0;
    mca_common_ompio_print_entry nentry;
#endif

    vulcan_num_io_procs = fh->f_get_mca_parameter_value ( "num_aggregators", strlen ("num_aggregators"));
    if (OMPI_ERR_MAX == vulcan_num_io_procs) {
        ret = OMPI_ERROR;
        goto exit;
    }
    bytes_per_cycle = fh->f_bytes_per_agg;

    if ((1 == mca_fcoll_vulcan_async_io) && (NULL == fh->f_fbtl->fbtl_ipreadv)) {
        opal_output (1, "vulcan_read_all: fbtl Does NOT support ipreadv() (asynchronous read) \n");
        ret = MPI_ERR_UNSUPPORTED_OPERATION;
        goto exit;
    }

    mca_common_ompio_check_gpu_buf (fh, buf, &is_gpu, &is_managed);
    if (is_gpu && !is_managed &&
        fh->f_get_mca_parameter_value ("use_accelerator_buffers", strlen("use_accelerator_buffers"))) {
        use_accelerator_buffer = true;
    }
    /* since we want to overlap 2 iterations, define the bytes_per_cycle to be half of what
       the user requested */
    bytes_per_cycle = bytes_per_cycle/2;

    /**************************************************************************
     ** 1. Decode user buffer into an iovec
     **************************************************************************/
    ret = mca_common_ompio_decode_datatype ((struct ompio_file_t *) fh,
                                            datatype, count, buf, &max_data,
                                            fh->f_mem_convertor, &decoded_iov,
                                            &iov_count);
    if (OMPI_SUCCESS != ret){
        goto exit;
    }

    if (MPI_STATUS_IGNORE != status) {
        status->_ucount = max_data;
    }

    ret = mca_fcoll_vulcan_get_configuration (fh, vulcan_num_io_procs, max_data);
    if (OMPI_SUCCESS != ret){
        goto exit;
    }
    opal_output_verbose(10, ompi_fcoll_base_framework.framework_output,
                        "Using %d aggregators for the read_all operation \n", fh->f_num_aggrs);

    aggr_data = (mca_io_ompio_aggregator_data **) malloc (fh->f_num_aggrs *
                                            sizeof(mca_io_ompio_aggregator_data*));
    if (NULL == aggr_data) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    for (i = 0; i < fh->f_num_aggrs; i++) {
        // At this point we know the number of aggregators. If there is a correlation between
        // number of aggregators and number of IO nodes, we know how many aggr_data arrays we need
        // to allocate.
        aggr_data[i] = (mca_io_ompio_aggregator_data *) calloc (1, sizeof(mca_io_ompio_aggregator_data));
        aggr_data[i]->procs_per_group = fh->f_procs_per_group;
        aggr_data[i]->procs_in_group  = fh->f_procs_in_group;
        aggr_data[i]->comm = fh->f_comm;
        // Identify if the process is an aggregator.
        // If so, aggr_index would be its index in "aggr_data" and "aggregators" arrays.
        if (fh->f_aggr_list[i] == fh->f_rank) {
            aggr_index = i;
        }
    }

    /*********************************************************************
     *** 2. Generate the local offsets/lengths array corresponding to
     ***    this read operation
     ********************************************************************/
    ret = fh->f_generate_current_file_view ((struct ompio_file_t *) fh,
                                            max_data, &local_iov_array,
                                            &local_count);
    if (ret != OMPI_SUCCESS) {
        goto exit;
    }

    /*************************************************************************
     ** 2b. Separate the local_iov_array entries based on the number of aggregators
     *************************************************************************/
    // Modifications for the even distribution:
    long domain_size;
    ret = mca_fcoll_vulcan_minmax (fh, local_iov_array, local_count, fh->f_num_aggrs, &domain_size);

    // broken_iov_arrays[0] contains broken_counts[0] entries to aggregator 0,
    // broken_iov_arrays[1] contains broken_counts[1] entries to aggregator 1, etc.
    ret = mca_fcoll_vulcan_break_file_view (decoded_iov, iov_count,
                                            local_iov_array, local_count,
                                            &broken_decoded_iovs, &broken_iov_counts,
                                            &broken_iov_arrays, &broken_counts,
                                            &broken_total_lengths,
                                            fh->f_num_aggrs, domain_size);

    /**************************************************************************
     ** 3. Determine the total amount of data to be read and no. of cycles
     **************************************************************************/
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
    start_comm_time = MPI_Wtime();
#endif
    ret = fh->f_comm->c_coll->coll_allreduce (MPI_IN_PLACE, broken_total_lengths,
                                              fh->f_num_aggrs, MPI_LONG, MPI_SUM,
                                              fh->f_comm,
                                              fh->f_comm->c_coll->coll_allreduce_module);
    if (OMPI_SUCCESS != ret) {
        goto exit;
    }

#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
    end_comm_time = MPI_Wtime();
    comm_time += (end_comm_time - start_comm_time);
#endif

    cycles=0;
    for (i = 0; i < fh->f_num_aggrs; i++) {
#if DEBUG_ON
        printf("%d: Overall broken_total_lengths[%d] = %ld\n", fh->f_rank, i, broken_total_lengths[i]);
#endif
        if (ceil((double)broken_total_lengths[i]/bytes_per_cycle) > cycles) {
            cycles = ceil((double)broken_total_lengths[i]/bytes_per_cycle);
        }
    }

    result_counts = (int *) malloc (fh->f_num_aggrs * fh->f_procs_per_group * sizeof(int));
    if (NULL == result_counts) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
    start_comm_time = MPI_Wtime();
#endif
    ret = fh->f_comm->c_coll->coll_allgather (broken_counts, fh->f_num_aggrs, MPI_INT,
                                              result_counts, fh->f_num_aggrs, MPI_INT,
                                              fh->f_comm,
					      fh->f_comm->c_coll->coll_allgather_module);
    if (OMPI_SUCCESS != ret) {
        goto exit;
    }
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
    end_comm_time = MPI_Wtime();
    comm_time += (end_comm_time - start_comm_time);
#endif

    /*************************************************************
     *** 4. Allgather the offset/lengths array from all processes
     *************************************************************/
    for (i = 0; i < fh->f_num_aggrs; i++) {
        aggr_data[i]->total_bytes = broken_total_lengths[i];
        aggr_data[i]->decoded_iov = broken_decoded_iovs[i];
        aggr_data[i]->fview_count = (size_t *)malloc (fh->f_procs_per_group * sizeof (size_t));
        if (NULL == aggr_data[i]->fview_count) {
            opal_output (1, "OUT OF MEMORY\n");
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }

        for (j = 0; j < fh->f_procs_per_group; j++) {
            aggr_data[i]->fview_count[j] = result_counts[fh->f_num_aggrs*j+i];
        }

        displs = (ptrdiff_t *)malloc (fh->f_procs_per_group * sizeof (ptrdiff_t));
        if (NULL == displs) {
            opal_output (1, "OUT OF MEMORY\n");
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }

        displs[0] = 0;
        total_fview_count = (uint32_t) aggr_data[i]->fview_count[0];
        for (j = 1 ; j < fh->f_procs_per_group ; j++) {
            total_fview_count += aggr_data[i]->fview_count[j];
            displs[j] = displs[j-1] + aggr_data[i]->fview_count[j-1];
        }

#if DEBUG_ON
        printf("total_fview_count : %d\n", total_fview_count);
        if (fh->f_aggr_list[i] == fh->f_rank) {
            for (j=0 ; j<fh->f_procs_per_group ; i++) {
                printf ("%d: PROCESS: %d  ELEMENTS: %ld  DISPLS: %ld\n",
                        fh->f_rank, j,
                        aggr_data[i]->fview_count[j],
                        displs[j]);
            }
        }
#endif

        /* allocate the global iovec  */
        if (0 != total_fview_count) {
            aggr_data[i]->global_iov_array = (struct iovec*) malloc (total_fview_count *
                                                                     sizeof(struct iovec));
            if (NULL == aggr_data[i]->global_iov_array) {
                opal_output(1, "OUT OF MEMORY\n");
                ret = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }
        }

#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
        start_comm_time = MPI_Wtime();
#endif
        OMPI_COUNT_ARRAY_INIT(&fview_count_desc, aggr_data[i]->fview_count);
        OMPI_DISP_ARRAY_INIT(&displs_desc, displs);
        ret = fh->f_comm->c_coll->coll_allgatherv (broken_iov_arrays[i],
                                                   broken_counts[i],
                                                   fh->f_iov_type,
                                                   aggr_data[i]->global_iov_array,
                                                   fview_count_desc,
                                                   displs_desc,
                                                   fh->f_iov_type,
                                                   fh->f_comm,
                                                   fh->f_comm->c_coll->coll_allgatherv_module );
        if (OMPI_SUCCESS != ret) {
            goto exit;
        }

#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
        end_comm_time = MPI_Wtime();
        comm_time += (end_comm_time - start_comm_time);
#endif

        /****************************************************************************************
         *** 5. Sort the global offset/lengths list based on the offsets.
         *** The result of the sort operation is the 'sorted', an integer array,
         *** which contains the indexes of the global_iov_array based on the offset.
         *** For example, if global_iov_array[x].offset is followed by global_iov_array[y].offset
         *** in the file, and that one is followed by global_iov_array[z].offset, than
         *** sorted[0] = x, sorted[1]=y and sorted[2]=z;
         ******************************************************************************************/
        if (0 != total_fview_count) {
            aggr_data[i]->sorted = (int *)malloc (total_fview_count * sizeof(int));
            if (NULL == aggr_data[i]->sorted) {
                opal_output (1, "OUT OF MEMORY\n");
                ret = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }
            ompi_fcoll_base_sort_iovec (aggr_data[i]->global_iov_array, total_fview_count,
					aggr_data[i]->sorted);
        }

        if (NULL != local_iov_array) {
            free(local_iov_array);
            local_iov_array = NULL;
        }

        if (NULL != displs) {
            free(displs);
            displs=NULL;
        }

#if DEBUG_ON
        if (fh->f_aggr_list[i] == fh->f_rank) {
            uint32_t tv=0;
            for (tv = 0 ; tv < total_fview_count ; tv++) {
                printf("%d: OFFSET: %lu   LENGTH: %ld\n",
                       fh->f_rank,
                       (uint64_t)aggr_data[i]->global_iov_array[aggr_data[i]->sorted[tv]].iov_base,
                       aggr_data[i]->global_iov_array[aggr_data[i]->sorted[tv]].iov_len);
            }
        }
#endif
        /*************************************************************
         *** 6. Determine the number of cycles required to execute this
         ***    operation
         *************************************************************/
        aggr_data[i]->bytes_per_cycle = bytes_per_cycle;

        if (fh->f_aggr_list[i] == fh->f_rank) {
            aggr_data[i]->disp_index = (int *)malloc (fh->f_procs_per_group * sizeof (int));
            if (NULL == aggr_data[i]->disp_index) {
                opal_output (1, "OUT OF MEMORY\n");
                ret = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }

            aggr_data[i]->max_disp_index = (int *)calloc (fh->f_procs_per_group,  sizeof (int));
            if (NULL == aggr_data[i]->max_disp_index) {
                opal_output (1, "OUT OF MEMORY\n");
                ret = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }

            aggr_data[i]->blocklen_per_process = (int **)calloc (fh->f_procs_per_group, sizeof (int*));
            if (NULL == aggr_data[i]->blocklen_per_process) {
                opal_output (1, "OUT OF MEMORY\n");
                ret = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }

            aggr_data[i]->displs_per_process = (MPI_Aint **)calloc (fh->f_procs_per_group, sizeof (MPI_Aint*));
            if (NULL == aggr_data[i]->displs_per_process) {
                opal_output (1, "OUT OF MEMORY\n");
                ret = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }

            if (use_accelerator_buffer) {
                opal_output_verbose(10, ompi_fcoll_base_framework.framework_output,
                                    "Allocating GPU device buffer for aggregation\n");
                ret = opal_accelerator.mem_alloc(MCA_ACCELERATOR_NO_DEVICE_ID, (void**)&aggr_data[i]->global_buf,
                                                 bytes_per_cycle);
                if (OPAL_SUCCESS != ret) {
                    opal_output(1, "Could not allocate accelerator memory");
                    ret = OMPI_ERR_OUT_OF_RESOURCE;
                    goto exit;
                }
                ret = opal_accelerator.mem_alloc(MCA_ACCELERATOR_NO_DEVICE_ID, (void**)&aggr_data[i]->prev_global_buf,
                                                 bytes_per_cycle);
                if (OPAL_SUCCESS != ret) {
                    opal_output(1, "Could not allocate accelerator memory");
                    ret = OMPI_ERR_OUT_OF_RESOURCE;
                    goto exit;
                }
            } else {
                aggr_data[i]->global_buf       = (char *) malloc (bytes_per_cycle);
                aggr_data[i]->prev_global_buf  = (char *) malloc (bytes_per_cycle);
                if (NULL == aggr_data[i]->global_buf || NULL == aggr_data[i]->prev_global_buf){
                    opal_output(1, "OUT OF MEMORY");
                    ret = OMPI_ERR_OUT_OF_RESOURCE;
                    goto exit;
                }
            }

            aggr_data[i]->recvtype = (ompi_datatype_t **) malloc (fh->f_procs_per_group  *
                                                                  sizeof(ompi_datatype_t *));
            aggr_data[i]->prev_recvtype = (ompi_datatype_t **) malloc (fh->f_procs_per_group  *
                                                                       sizeof(ompi_datatype_t *));
            if (NULL == aggr_data[i]->recvtype || NULL == aggr_data[i]->prev_recvtype) {
                opal_output (1, "OUT OF MEMORY\n");
                ret = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }
            for(l=0;l<fh->f_procs_per_group;l++){
                aggr_data[i]->recvtype[l]      = MPI_DATATYPE_NULL;
                aggr_data[i]->prev_recvtype[l] = MPI_DATATYPE_NULL;
            }
        }

#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
        start_exch = MPI_Wtime();
#endif
    }

    reqs = (ompi_request_t **)malloc ((fh->f_procs_per_group + 1 )*fh->f_num_aggrs *sizeof(ompi_request_t *));
    if (NULL == reqs) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    for (l = 0, i = 0; i < fh->f_num_aggrs; i++) {
        for (j=0; j< (fh->f_procs_per_group+1); j++) {
            reqs[l] = MPI_REQUEST_NULL;
            l++;
        }
    }

    if( (1 == mca_fcoll_vulcan_async_io) ||
        ( (0 == mca_fcoll_vulcan_async_io) && (NULL != fh->f_fbtl->fbtl_ipreadv) && (2 < cycles))) {
        read_sync_type = 1;
    }

    if (cycles > 0) {
        if (NOT_AGGR_INDEX != aggr_index) {
	    // Register progress function that should be used by ompi_request_wait
	    mca_common_ompio_register_progress ();
	}
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
	start_read_time = MPI_Wtime();
#endif
        for (i = 0; i < fh->f_num_aggrs; i++) {
            ret = read_init (fh, 0, cycles, fh->f_aggr_list[i], fh->f_rank,
                             aggr_data[i], read_sync_type, &req_tmp,
                             use_accelerator_buffer);
            if (OMPI_SUCCESS != ret) {
                goto exit;
            }
            if (fh->f_aggr_list[i] == fh->f_rank) {
                req_iread = req_tmp;
            }
	}

        if (NOT_AGGR_INDEX != aggr_index) {
            ret = ompi_request_wait(&req_iread, MPI_STATUS_IGNORE);
            if (OMPI_SUCCESS != ret){
                goto exit;
            }
        }
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
	end_read_time = MPI_Wtime();
	read_time += end_read_time - start_read_time;
#endif
    }

    for (index = 1; index < cycles; index++) {
        for (i = 0; i < fh->f_num_aggrs; i++) {
            ret = shuffle_init (index-1, cycles, fh->f_aggr_list[i], fh->f_rank, aggr_data[i],
                                &reqs[i*(fh->f_procs_per_group + 1)] );
            if (OMPI_SUCCESS != ret) {
                goto exit;
            }
        }

        SWAP_AGGR_POINTERS(aggr_data, fh->f_num_aggrs);
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
        start_read_time = MPI_Wtime();
#endif
        for (i = 0; i < fh->f_num_aggrs; i++) {
            ret = read_init (fh, index, cycles, fh->f_aggr_list[i], fh->f_rank,
			     aggr_data[i], read_sync_type,
			     &req_tmp, use_accelerator_buffer);
            if (OMPI_SUCCESS != ret){
                goto exit;
            }
            if (fh->f_aggr_list[i] == fh->f_rank) {
                req_iread = req_tmp;
            }
        }
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
	end_read_time = MPI_Wtime();
	read_time += end_read_time - start_read_time;
#endif
	ret = ompi_request_wait_all ((fh->f_procs_per_group + 1 )*fh->f_num_aggrs,
                                     reqs, MPI_STATUS_IGNORE);
        if (OMPI_SUCCESS != ret){
            goto exit;
        }

        if (NOT_AGGR_INDEX != aggr_index) {
            ret = ompi_request_wait (&req_iread, MPI_STATUS_IGNORE);
            if (OMPI_SUCCESS != ret){
                goto exit;
            }
        }
    } /* end  for (index = 1; index < cycles; index++) */

    if (cycles > 0) {
        for (i = 0; i < fh->f_num_aggrs; i++) {
            ret = shuffle_init (index-1, cycles, fh->f_aggr_list[i], fh->f_rank, aggr_data[i],
                                &reqs[i*(fh->f_procs_per_group + 1)] );
            if (OMPI_SUCCESS != ret) {
                goto exit;
            }
        }
	ret = ompi_request_wait_all ((fh->f_procs_per_group + 1 )*fh->f_num_aggrs,
                                     reqs, MPI_STATUS_IGNORE);
        if (OMPI_SUCCESS != ret){
            goto exit;
        }
    }

#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
    end_exch = MPI_Wtime();
    exch_read += end_exch - start_exch;
    nentry.time[0] = read_time;
    nentry.time[1] = comm_time;
    nentry.time[2] = exch_read;
    nentry.aggregator = 0;
    for ( i=0; i<fh->f_num_aggrs; i++ ) {
        if (fh->f_aggr_list[i] == fh->f_rank)
        nentry.aggregator = 1;
    }
    nentry.nprocs_for_coll = fh->f_num_aggrs;
    if (!mca_common_ompio_full_print_queue(fh->f_coll_read_time)){
        mca_common_ompio_register_print_entry(fh->f_coll_read_time,
                                               nentry);
    }
#endif

exit :
    if (NULL != aggr_data) {

        for (i = 0; i < fh->f_num_aggrs; i++) {
            if (fh->f_aggr_list[i] == fh->f_rank) {
                if (NULL != aggr_data[i]->recvtype){
                    for (j = 0; j < aggr_data[i]->procs_per_group; j++) {
                        if (MPI_DATATYPE_NULL != aggr_data[i]->recvtype[j]) {
                            ompi_datatype_destroy(&aggr_data[i]->recvtype[j]);
                        }
                        if (MPI_DATATYPE_NULL != aggr_data[i]->prev_recvtype[j]) {
                            ompi_datatype_destroy(&aggr_data[i]->prev_recvtype[j]);
                        }
                    }
                    free(aggr_data[i]->recvtype);
                    free(aggr_data[i]->prev_recvtype);
                }

                free (aggr_data[i]->disp_index);
                free (aggr_data[i]->max_disp_index);
                if (use_accelerator_buffer) {
                    opal_accelerator.mem_release(MCA_ACCELERATOR_NO_DEVICE_ID, aggr_data[i]->global_buf);
                    opal_accelerator.mem_release(MCA_ACCELERATOR_NO_DEVICE_ID, aggr_data[i]->prev_global_buf);
                } else {
                    free (aggr_data[i]->global_buf);
                    free (aggr_data[i]->prev_global_buf);
                }
                for (l = 0;l < aggr_data[i]->procs_per_group; l++) {
                    free (aggr_data[i]->blocklen_per_process[l]);
                    free (aggr_data[i]->displs_per_process[l]);
                }

                free (aggr_data[i]->blocklen_per_process);
                free (aggr_data[i]->displs_per_process);
            }
            free (aggr_data[i]->sorted);
            free (aggr_data[i]->global_iov_array);
            free (aggr_data[i]->fview_count);
            free (aggr_data[i]->decoded_iov);

            free (aggr_data[i]);
        }
        free (aggr_data);
    }
    free(displs);
    free(decoded_iov);
    free(broken_counts);
    free(broken_total_lengths);
    free(broken_iov_counts);
    free(broken_decoded_iovs); // decoded_iov arrays[i] were freed as aggr_data[i]->decoded_iov;
    if (NULL != broken_iov_arrays) {
        for (i = 0; i < fh->f_num_aggrs; i++) {
            free(broken_iov_arrays[i]);
        }
    }
    free(broken_iov_arrays);
    free(fh->f_procs_in_group);
    free(fh->f_aggr_list);
    fh->f_procs_in_group=NULL;
    fh->f_procs_per_group=0;
    fh->f_aggr_list=NULL;
    free(result_counts);
    free(reqs);

    return ret;
}

static int read_init (ompio_file_t *fh, int index, int cycles, int aggregator, int rank,
		      mca_io_ompio_aggregator_data *data,
                      int read_syncType, ompi_request_t **request,
                      bool is_accelerator_buffer)
{
    int ret = OMPI_SUCCESS;
    ssize_t ret_temp = 0;
    mca_ompio_request_t *ompio_req = NULL;
    int i, j, l;
    int entries_per_aggregator=0;
    mca_io_ompio_local_io_array *file_offsets_for_agg=NULL;
    MPI_Aint *memory_displacements=NULL;
    int* blocklength_proc=NULL;
    ptrdiff_t* displs_proc=NULL;
    int *sorted_file_offsets=NULL;

    /**********************************************************************
     ***  7a. Getting ready for next cycle: initializing and freeing buffers
     **********************************************************************/
    data->bytes_sent = 0;

    if (aggregator == rank) {
	if (NULL != data->recvtype){
	    for (i = 0; i < data->procs_per_group; i++) {
		if (MPI_DATATYPE_NULL != data->recvtype[i]) {
		    ompi_datatype_destroy(&data->recvtype[i]);
		    data->recvtype[i] = MPI_DATATYPE_NULL;
		}
	    }
	}

	for (l = 0; l < data->procs_per_group; l++) {
	    data->disp_index[l] = 0;

	    if (data->max_disp_index[l] == 0) {
		data->blocklen_per_process[l] = (int *) calloc (INIT_LEN, sizeof(int));
		data->displs_per_process[l] = (MPI_Aint *) calloc (INIT_LEN, sizeof(MPI_Aint));
		if (NULL == data->displs_per_process[l] || NULL == data->blocklen_per_process[l]){
		    opal_output (1, "OUT OF MEMORY for displs\n");
		    ret = OMPI_ERR_OUT_OF_RESOURCE;
		    goto exit;
		}
		data->max_disp_index[l] = INIT_LEN;
	    } else {
		memset (data->blocklen_per_process[l], 0, data->max_disp_index[l]*sizeof(int));
		memset (data->displs_per_process[l], 0, data->max_disp_index[l]*sizeof(MPI_Aint));
	    }
	}
    } /* rank == aggregator */

    /**************************************************************************
     ***  7b. Determine the number of bytes to be actually read in this cycle
     **************************************************************************/
    int local_cycles= ceil((double)data->total_bytes / data->bytes_per_cycle);
    if (index  < (local_cycles -1)) {
        data->bytes_to_write_in_cycle = data->bytes_per_cycle;
    } else if ( index == (local_cycles -1)) {
        data->bytes_to_write_in_cycle = data->total_bytes - data->bytes_per_cycle*index;
    } else {
        data->bytes_to_write_in_cycle = 0;
    }
    data->bytes_to_write = data->bytes_to_write_in_cycle;

#if DEBUG_ON
    if (aggregator == rank) {
        printf ("****%d: CYCLE %d   Bytes %d**********\n",
                rank, index, data->bytes_to_write_in_cycle);
    }
#endif

    /*****************************************************************
     *** 7c. Calculate how much data will be sent to each process in
     *** this cycle
     *****************************************************************/
    mca_fcoll_vulcan_calc_blocklen_disps(data, aggregator, rank, &data->bytes_sent);

    /*************************************************************************
     *** 7d. Calculate the displacement
     *************************************************************************/
    if (rank == aggregator) {
        for (i = 0; i < data->procs_per_group; i++){
            for (j = 0; j < data->disp_index[i]; j++){
                if (data->blocklen_per_process[i][j] > 0)
                    entries_per_aggregator++ ;
            }
        }
    }
#if DEBUG_ON
    if (aggregator == rank) {
        printf("%d : Entries per aggregator : %d\n", rank, entries_per_aggregator);
    }
#endif

    if (entries_per_aggregator > 0) {
	file_offsets_for_agg = (mca_io_ompio_local_io_array *) malloc (entries_per_aggregator
							* sizeof(mca_io_ompio_local_io_array));
	memory_displacements = (MPI_Aint *) malloc (entries_per_aggregator * sizeof(MPI_Aint));
	sorted_file_offsets = (int *) malloc (entries_per_aggregator*sizeof(int));
	if (NULL == file_offsets_for_agg || NULL == memory_displacements ||
	    NULL == sorted_file_offsets) {
	    opal_output (1, "OUT OF MEMORY\n");
	    ret = OMPI_ERR_OUT_OF_RESOURCE;
	    goto exit;
	}

	ret = mca_fcoll_vulcan_calc_file_offsets(data, file_offsets_for_agg, sorted_file_offsets,
						 memory_displacements, entries_per_aggregator,
						 rank, index);
	if (OMPI_SUCCESS != ret) {
	  goto exit;
	}

	/**********************************************************
	 *** 7f. Create the io array
	 *********************************************************/
        fh->f_io_array = (mca_common_ompio_io_array_t *) malloc (entries_per_aggregator
						* sizeof (mca_common_ompio_io_array_t));
        if (NULL == fh->f_io_array) {
            opal_output(1, "OUT OF MEMORY\n");
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }

        mca_fcoll_vulcan_calc_io_array(fh->f_io_array, &fh->f_num_of_io_entries, entries_per_aggregator,
                                       (char*)data->global_buf, file_offsets_for_agg, sorted_file_offsets,
                                       memory_displacements, rank);
    }

    if (rank == aggregator && fh->f_num_of_io_entries) {
        mca_common_ompio_request_alloc (&ompio_req, MCA_OMPIO_REQUEST_READ);

        if (1 == read_syncType) {
            if (is_accelerator_buffer) {
                ret = mca_common_ompio_file_iread_pregen(fh, (ompi_request_t *) ompio_req);
                if (0 > ret) {
                    opal_output (1, "vulcan_read_all: mca_common_ompio_iread_pregen failed\n");
                    ompio_req->req_ompi.req_status.MPI_ERROR = ret;
                    ompio_req->req_ompi.req_status._ucount = 0;
                }
            } else {
                ret = fh->f_fbtl->fbtl_ipreadv(fh, (ompi_request_t *) ompio_req);
                if (0 > ret) {
                    opal_output (1, "vulcan_read_all: fbtl_ipreadv failed\n");
                    ompio_req->req_ompi.req_status.MPI_ERROR = ret;
                    ompio_req->req_ompi.req_status._ucount = 0;
                }
            }
        }
        else {
            ret_temp = fh->f_fbtl->fbtl_preadv(fh);
            if (0 > ret_temp) {
                opal_output (1, "vulcan_read_all: fbtl_preadv failed\n");
                ret = ret_temp;
                ret_temp = 0;
            }

            ompio_req->req_ompi.req_status.MPI_ERROR = ret;
            ompio_req->req_ompi.req_status._ucount = ret_temp;
            ompi_request_complete (&ompio_req->req_ompi, false);
        }

        free(fh->f_io_array);
    }

#if DEBUG_ON
    printf("************Cycle: %d,  Aggregator: %d ***************\n",
	   index, rank);
    for (i = 0; i < data->procs_per_group; i++) {
	for (j = 0; j < data->disp_index[i]; j++) {
	    if (data->blocklen_per_process[i][j] > 0) {
		printf("%d sends blocklen[%d]: %d, disp[%d]: %ld to %d\n",
		       data->procs_in_group[i],j,
		       data->blocklen_per_process[i][j],j,
		       data->displs_per_process[i][j], rank);
	    }
	}
    }
#endif

exit:
    free(sorted_file_offsets);
    free(file_offsets_for_agg);
    free(memory_displacements);
    free(blocklength_proc);
    free(displs_proc);

    fh->f_io_array = NULL;
    fh->f_num_of_io_entries = 0;

    *request = (ompi_request_t *) ompio_req;
    return ret;
}

static int shuffle_init (int index, int cycles, int aggregator, int rank, mca_io_ompio_aggregator_data *data,
                         ompi_request_t **reqs)
{
    int i, ret = OMPI_SUCCESS;
    int* blocklength_proc=NULL;
    ptrdiff_t* displs_proc=NULL;

    /*************************************************************************
     *** 7e. Perform the actual communication
     *************************************************************************/
    if (aggregator == rank ) {
	for (i = 0; i < data->procs_per_group; i++) {
	    size_t datatype_size;
            reqs[i] = MPI_REQUEST_NULL;
	    if (0 < data->disp_index[i]) {
		ompi_datatype_create_hindexed (data->disp_index[i],
					       OMPI_COUNT_ARRAY_CREATE(data->blocklen_per_process[i]),
					       OMPI_DISP_ARRAY_CREATE(data->displs_per_process[i]),
					       MPI_BYTE,
					       &data->recvtype[i]);
		ompi_datatype_commit (&data->recvtype[i]);
		opal_datatype_type_size (&data->recvtype[i]->super, &datatype_size);

		if (datatype_size){
		    ret = MCA_PML_CALL(isend(data->global_buf,
					     1, data->recvtype[i],
					     data->procs_in_group[i],
					     FCOLL_VULCAN_SHUFFLE_TAG+index,
					     MCA_PML_BASE_SEND_STANDARD,
					     data->comm, &reqs[i]));
		    if (OMPI_SUCCESS != ret){
			goto exit;
		    }
		}
	    }
	}
	// }  /* end if (entries_per_aggr > 0 ) */
    }/* end if (aggregator == rank ) */

    reqs[data->procs_per_group] = MPI_REQUEST_NULL;
    if (data->bytes_sent) {
        size_t remaining      = data->bytes_sent;
        int block_index       = -1;
        int blocklength_size  = INIT_LEN;

        ptrdiff_t recv_mem_address  = 0;
        ompi_datatype_t *newType    = MPI_DATATYPE_NULL;
        blocklength_proc            = (int *)       calloc (blocklength_size, sizeof (int));
        displs_proc                 = (ptrdiff_t *) calloc (blocklength_size, sizeof (ptrdiff_t));

        if (NULL == blocklength_proc || NULL == displs_proc ) {
            opal_output (1, "OUT OF MEMORY\n");
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }

        while (remaining) {
            block_index++;

            if(0 == block_index) {
                recv_mem_address = (ptrdiff_t) (data->decoded_iov[data->iov_index].iov_base) +
                                                data->current_position;
            }
            else {
                // Reallocate more memory if blocklength_size is not enough
                if(0 == block_index % INIT_LEN) {
                    blocklength_size += INIT_LEN;
                    blocklength_proc = (int *)       realloc(blocklength_proc, blocklength_size * sizeof(int));
                    displs_proc      = (ptrdiff_t *) realloc(displs_proc, blocklength_size * sizeof(ptrdiff_t));
                }
                displs_proc[block_index] = (ptrdiff_t) (data->decoded_iov[data->iov_index].iov_base) +
                                                        data->current_position - recv_mem_address;
            }

            if (remaining >=
                (data->decoded_iov[data->iov_index].iov_len - data->current_position)) {

                blocklength_proc[block_index] = data->decoded_iov[data->iov_index].iov_len -
                                                data->current_position;
                remaining = remaining - (data->decoded_iov[data->iov_index].iov_len -
					 data->current_position);
                data->iov_index = data->iov_index + 1;
                data->current_position = 0;
            } else {
                blocklength_proc[block_index] = remaining;
                data->current_position += remaining;
                remaining = 0;
            }
        }

        data->total_bytes_written += data->bytes_sent;

        if (0 <= block_index) {
            ompi_datatype_create_hindexed (block_index+1,
                                           OMPI_COUNT_ARRAY_CREATE(blocklength_proc),
                                           OMPI_DISP_ARRAY_CREATE(displs_proc),
                                           MPI_BYTE,
                                           &newType);
            ompi_datatype_commit (&newType);

            ret = MCA_PML_CALL(irecv((char *)recv_mem_address,
                                     1,
                                     newType,
                                     aggregator,
                                     FCOLL_VULCAN_SHUFFLE_TAG+index,
                                     data->comm,
                                     &reqs[data->procs_per_group]));
            if (MPI_DATATYPE_NULL != newType) {
                ompi_datatype_destroy(&newType);
            }
            if (OMPI_SUCCESS != ret){
                goto exit;
            }
        }
    }
exit:
    return ret;
}
