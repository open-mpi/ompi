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
 * Copyright (c) 2008-2016 University of Houston. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "fcoll_dynamic_gen2.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/io/ompio/io_ompio.h"
#include "ompi/mca/io/io.h"
#include "math.h"
#include "ompi/mca/pml/pml.h"
#include <unistd.h>


#define DEBUG_ON 0

/*Used for loading file-offsets per aggregator*/
typedef struct mca_io_ompio_local_io_array{
    OMPI_MPI_OFFSET_TYPE offset;
    MPI_Aint             length;
    int                  process_id;
}mca_io_ompio_local_io_array;

typedef struct mca_io_ompio_aggregator_data {
    int *disp_index, *sorted, *fview_count, n;
    int **blocklen_per_process;
    MPI_Aint **displs_per_process, total_bytes, bytes_per_cycle, total_bytes_written;
    MPI_Request *recv_req;
    MPI_Comm comm;
    char *global_buf, *buf;
    ompi_datatype_t **recvtype;
    struct iovec *global_iov_array;
    int current_index, current_position;
    int bytes_to_write_in_cycle, bytes_remaining, procs_per_group;    
    int *procs_in_group, iov_index;
    bool sendbuf_is_contiguous;
    struct iovec *decoded_iov;
} mca_io_ompio_aggregator_data;

static int subroutine ( int index, int cycles, int aggregator, int rank, mca_io_ompio_aggregator_data *data, 
                        mca_io_ompio_io_array_t **ret_io_array, int *ret_num_io_entries );

int mca_fcoll_dynamic_gen2_break_file_view ( struct iovec *decoded_iov, int iov_count, 
                                        struct iovec *local_iov_array, int local_count, 
                                        struct iovec ***broken_decoded_iovs, int **broken_iov_counts,
                                        struct iovec ***broken_iov_arrays, int **broken_counts, 
                                        MPI_Aint **broken_total_lengths,
                                        int stripe_count, int stripe_size); 


int mca_fcoll_dynamic_gen2_get_configuration (mca_io_ompio_file_t *fh, int *dynamic_gen2_num_io_procs, int **ret_aggregators);


static int local_heap_sort (mca_io_ompio_local_io_array *io_array,
			    int num_entries,
			    int *sorted);


int mca_fcoll_dynamic_gen2_file_write_all (mca_io_ompio_file_t *fh,
                                      const void *buf,
                                      int count,
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
    
    mca_io_ompio_aggregator_data **aggr_data=NULL;
    
    int *displs = NULL;
    int dynamic_gen2_num_io_procs;
    size_t max_data = 0;
    
    MPI_Aint *total_bytes_per_process = NULL;
    mca_io_ompio_io_array_t *io_array;
    int num_io_entries;
    
    struct iovec **broken_iov_arrays=NULL;
    struct iovec **broken_decoded_iovs=NULL;
    int *broken_counts=NULL;
    int *broken_iov_counts=NULL;
    MPI_Aint *broken_total_lengths=NULL;

    int *aggregators=NULL;

    //Edgar: just for quick testing:
    int stripe_size=1048576;
    
    
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
    double write_time = 0.0, start_write_time = 0.0, end_write_time = 0.0;
    double comm_time = 0.0, start_comm_time = 0.0, end_comm_time = 0.0;
    double exch_write = 0.0, start_exch = 0.0, end_exch = 0.0;
    mca_io_ompio_print_entry nentry;
#endif
    
    
    /**************************************************************************
     ** 1.  In case the data is not contigous in memory, decode it into an iovec
     **************************************************************************/
    fh->f_get_num_aggregators ( &dynamic_gen2_num_io_procs );
    fh->f_get_bytes_per_agg ( (int *)&bytes_per_cycle );
        
    ret =   fh->f_decode_datatype ((struct mca_io_ompio_file_t *) fh,
                                   datatype,
                                   count,
                                   buf,
                                   &max_data,
                                   &decoded_iov,
                                   &iov_count);
    if (OMPI_SUCCESS != ret ){
        goto exit;
    }

    if ( MPI_STATUS_IGNORE != status ) {
	status->_ucount = max_data;
    }
    
    /* difference to the first generation of this function:
    ** dynamic_gen2_num_io_procs should be the number of io_procs per group
    ** consequently.Initially, we will have only 1 group.
    */

    // EDGAR: just a quick heck for testing 
    ret = mca_fcoll_dynamic_gen2_get_configuration (fh, &dynamic_gen2_num_io_procs, &aggregators);
    if (OMPI_SUCCESS != ret){
	goto exit;
    }

    if ( fh->f_stripe_size > 0 ) {
        stripe_size = fh->f_stripe_size;
    }

    
    aggr_data = (mca_io_ompio_aggregator_data **) malloc ( dynamic_gen2_num_io_procs * 
                                                           sizeof(mca_io_ompio_aggregator_data*));

    for ( i=0; i< dynamic_gen2_num_io_procs; i++ ) {
        // At this point we know the number of aggregators. If there is a correlation between
        // number of aggregators and number of IO nodes, we know how many aggr_data arrays we need
        // to allocate.
        aggr_data[i] = (mca_io_ompio_aggregator_data *) calloc ( 1, sizeof(mca_io_ompio_aggregator_data));
        aggr_data[i]->procs_per_group = fh->f_procs_per_group;
        aggr_data[i]->procs_in_group  = fh->f_procs_in_group;
        aggr_data[i]->comm = fh->f_comm;
        aggr_data[i]->buf  = (char *)buf;             // should not be used in the new version.
        aggr_data[i]->sendbuf_is_contiguous = false;  //safe assumption for right now
    }

    /*********************************************************************
     *** 2. Generate the local offsets/lengths array corresponding to
     ***    this write operation
     ********************************************************************/
    ret = fh->f_generate_current_file_view( (struct mca_io_ompio_file_t *) fh,
					    max_data,
					    &local_iov_array,
					    &local_count);
    if (ret != OMPI_SUCCESS){
	goto exit;
    }

    /*************************************************************************
     ** 2b. Separate the local_iov_array entries based on the number of aggregators
     *************************************************************************/
    // broken_iov_arrays[0] contains broken_counts[0] entries to aggregator 0,
    // broken_iov_arrays[1] contains broken_counts[1] entries to aggregator 1, etc.
    ret = mca_fcoll_dynamic_gen2_break_file_view ( decoded_iov, iov_count, 
                                              local_iov_array, local_count, 
                                              &broken_decoded_iovs, &broken_iov_counts,
                                              &broken_iov_arrays, &broken_counts, 
                                              &broken_total_lengths,
                                              dynamic_gen2_num_io_procs,  stripe_size); 


    /**************************************************************************
     ** 3. Determine the total amount of data to be written and no. of cycles
     **************************************************************************/
    total_bytes_per_process = (MPI_Aint*)malloc
        (dynamic_gen2_num_io_procs * fh->f_procs_per_group*sizeof(MPI_Aint));
    if (NULL == total_bytes_per_process) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
	goto exit;
    }
    
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
    start_comm_time = MPI_Wtime();
#endif
    ret = fh->f_allgather_array (broken_total_lengths,
				 dynamic_gen2_num_io_procs,
				 MPI_LONG,
				 total_bytes_per_process,
				 dynamic_gen2_num_io_procs,
				 MPI_LONG,
				 0,
				 fh->f_procs_in_group,
				 fh->f_procs_per_group,
				 fh->f_comm);
    
    if( OMPI_SUCCESS != ret){
	goto exit;
    }
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
    end_comm_time = MPI_Wtime();
    comm_time += (end_comm_time - start_comm_time);
#endif

    cycles=0;
    for ( i=0; i<dynamic_gen2_num_io_procs; i++ ) {
        broken_total_lengths[i] = 0;
        for (j=0 ; j<fh->f_procs_per_group ; j++) {
            broken_total_lengths[i] += total_bytes_per_process[j*dynamic_gen2_num_io_procs + i];
        }
#if DEBUG_ON
        printf("%d: Overall broken_total_lengths[%d] = %ld\n", fh->f_rank, i, broken_total_lengths[i]);
#endif
        if ( ceil((double)broken_total_lengths[i]/bytes_per_cycle) > cycles ) {
            cycles = ceil((double)broken_total_lengths[i]/bytes_per_cycle);
        }
    }
    
    if (NULL != total_bytes_per_process) {
        free (total_bytes_per_process);
        total_bytes_per_process = NULL;
    }
    
    
    /*************************************************************
     *** 4. Allgather the offset/lengths array from all processes
     *************************************************************/
    for ( i=0; i< dynamic_gen2_num_io_procs; i++ ) {
        aggr_data[i]->total_bytes = broken_total_lengths[i];
        aggr_data[i]->decoded_iov = broken_decoded_iovs[i];
        aggr_data[i]->fview_count = (int *) malloc (fh->f_procs_per_group * sizeof (int));
        if (NULL == aggr_data[i]->fview_count) {
            opal_output (1, "OUT OF MEMORY\n");
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
        start_comm_time = MPI_Wtime();
#endif
        ret = fh->f_allgather_array (&broken_counts[i],
                                     1,
                                     MPI_INT,
                                     aggr_data[i]->fview_count,
                                     1,
                                     MPI_INT,
                                     i,
                                     fh->f_procs_in_group,
                                     fh->f_procs_per_group,
                                     fh->f_comm);
    
        if( OMPI_SUCCESS != ret){
            goto exit;
        }
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
        end_comm_time = MPI_Wtime();
        comm_time += (end_comm_time - start_comm_time);
#endif
    
        displs = (int*) malloc (fh->f_procs_per_group * sizeof (int));
        if (NULL == displs) {
            opal_output (1, "OUT OF MEMORY\n");
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
        
        displs[0] = 0;
        total_fview_count = aggr_data[i]->fview_count[0];
        for (j=1 ; j<fh->f_procs_per_group ; j++) {
            total_fview_count += aggr_data[i]->fview_count[j];
            displs[j] = displs[j-1] + aggr_data[i]->fview_count[j-1];
        }
        
#if DEBUG_ON
        printf("total_fview_count : %d\n", total_fview_count);
        if (aggregators[i] == fh->f_rank) {
            for (j=0 ; j<fh->f_procs_per_group ; i++) {
                printf ("%d: PROCESS: %d  ELEMENTS: %d  DISPLS: %d\n",
                        fh->f_rank,
                        j,
                        aggr_data[i]->fview_count[j],
                        displs[j]);
            }
        }
#endif
    
        /* allocate the global iovec  */
        if (0 != total_fview_count) {
            aggr_data[i]->global_iov_array = (struct iovec*) malloc (total_fview_count *
                                                                     sizeof(struct iovec));
            if (NULL == aggr_data[i]->global_iov_array){
                opal_output(1, "OUT OF MEMORY\n");
                ret = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }            
        }
    
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
        start_comm_time = MPI_Wtime();
#endif
        ret = fh->f_allgatherv_array (broken_iov_arrays[i],
                                      broken_counts[i],
                                      fh->f_iov_type,
                                      aggr_data[i]->global_iov_array,
                                      aggr_data[i]->fview_count,
                                      displs,
                                      fh->f_iov_type,
                                      i,
                                      fh->f_procs_in_group,
                                      fh->f_procs_per_group,
                                      fh->f_comm);
        if (OMPI_SUCCESS != ret){
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
            fh->f_sort_iovec (aggr_data[i]->global_iov_array, total_fview_count, aggr_data[i]->sorted);
        }
        
        if (NULL != local_iov_array){
            free(local_iov_array);
            local_iov_array = NULL;
        }
        
        if (NULL != displs){
            free(displs);
            displs=NULL;
        }
    
    
#if DEBUG_ON
        if (my_aggregator == fh->f_rank) {
            uint32_t tv=0;
            for (tv=0 ; tv<total_fview_count ; tv++) {
                printf("%d: OFFSET: %lld   LENGTH: %ld\n",
                       fh->f_rank,
                       aggr_data[i]->global_iov_array[aggr_data[i]->sorted[tv]].iov_base,
                       aggr_data[i]->global_iov_array[aggr_data[i]->sorted[tv]].iov_len);
            }
        }
#endif
        /*************************************************************
         *** 6. Determine the number of cycles required to execute this
         ***    operation
         *************************************************************/
        
        aggr_data[i]->bytes_per_cycle = bytes_per_cycle;
    
        if (aggregators[i] == fh->f_rank) {
            aggr_data[i]->disp_index = (int *)malloc (fh->f_procs_per_group * sizeof (int));
            if (NULL == aggr_data[i]->disp_index) {
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
        
            aggr_data[i]->recv_req = (MPI_Request *)malloc ((fh->f_procs_per_group)*sizeof(MPI_Request));
            if ( NULL == aggr_data[i]->recv_req ) {
                opal_output (1, "OUT OF MEMORY\n");
                ret = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }
            
            aggr_data[i]->global_buf  = (char *) malloc (bytes_per_cycle);
            if (NULL == aggr_data[i]->global_buf){
                opal_output(1, "OUT OF MEMORY");
                ret = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }
        
            aggr_data[i]->recvtype = (ompi_datatype_t **) malloc (fh->f_procs_per_group  * 
                                                                  sizeof(ompi_datatype_t *));
            if (NULL == aggr_data[i]->recvtype) {
                opal_output (1, "OUT OF MEMORY\n");
                ret = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }
            for(l=0;l<fh->f_procs_per_group;l++){
                aggr_data[i]->recvtype[l] = MPI_DATATYPE_NULL;
            }
        }
    
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
        start_exch = MPI_Wtime();
#endif
    }    


    for (index = 0; index < cycles; index++) {
        for ( i=0; i<dynamic_gen2_num_io_procs; i++ ) {
            
            ret = subroutine ( index, cycles, aggregators[i], fh->f_rank, aggr_data[i], 
                               &io_array, &num_io_entries );
            if ( OMPI_SUCCESS != ret ) {
                goto exit;
            }
            if ( aggregators[i] == fh->f_rank ) {
                fh->f_num_of_io_entries = num_io_entries;
                fh->f_io_array = io_array;
                if (fh->f_num_of_io_entries) {
                    if ( 0 >  fh->f_fbtl->fbtl_pwritev (fh)) {
                        opal_output (1, "WRITE FAILED\n");
                        ret = OMPI_ERROR;
                        goto exit;
                    }
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
                    end_write_time = MPI_Wtime();
                    write_time += end_write_time - start_write_time;
#endif
                    free ( fh->f_io_array );
                }
                fh->f_io_array=NULL;
                fh->f_num_of_io_entries=0;
            } /* end if (my_aggregator == fh->f_rank) */
        } 
    } /* end  for (index = 0; index < cycles; index++) */
    
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
    end_exch = MPI_Wtime();
    exch_write += end_exch - start_exch;
    nentry.time[0] = write_time;
    nentry.time[1] = comm_time;
    nentry.time[2] = exch_write;
    if (my_aggregator == fh->f_rank)
	nentry.aggregator = 1;
    else
	nentry.aggregator = 0;
    nentry.nprocs_for_coll = dynamic_gen2_num_io_procs;
    if (!fh->f_full_print_queue(WRITE_PRINT_QUEUE)){
        fh->f_register_print_entry(WRITE_PRINT_QUEUE,
                                   nentry);
    }
#endif
    
    
exit :
    
    if ( NULL != aggr_data ) {
        
        for ( i=0; i< dynamic_gen2_num_io_procs; i++ ) {            
            if (aggregators[i] == fh->f_rank) {
                if (NULL != aggr_data[i]->recvtype){
                    for (j =0; j< aggr_data[i]->procs_per_group; j++) {
                        if ( MPI_DATATYPE_NULL != aggr_data[i]->recvtype[j] ) {
                            ompi_datatype_destroy(&aggr_data[i]->recvtype[j]);
                        }
                    }
                    free(aggr_data[i]->recvtype);
                }
                
                free (aggr_data[i]->disp_index);
                free (aggr_data[i]->recv_req);
                free (aggr_data[i]->global_buf);
                for(l=0;l<aggr_data[i]->procs_per_group;l++){
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
    free(broken_iov_counts);
    free(broken_decoded_iovs); // decoded_iov arrays[i] were freed as aggr_data[i]->decoded_iov;
    for (i=0; i<dynamic_gen2_num_io_procs; i++ ) {
      free(broken_iov_arrays[i]);
    }
    free(broken_iov_arrays);
     
    return OMPI_SUCCESS;
}


static int subroutine ( int index, int cycles, int aggregator, int rank, mca_io_ompio_aggregator_data *data, 
                        mca_io_ompio_io_array_t **ret_io_array, int *ret_num_io_entries )
{
    int bytes_sent = 0;
    int blocks=0, temp_pindex;
    char *send_buf = NULL;
    int i, j, l, ret;
    MPI_Request send_req;
    int  entries_per_aggregator=0;
    mca_io_ompio_local_io_array *file_offsets_for_agg=NULL;
    int *sorted_file_offsets=NULL;
    int temp_index=0;
    MPI_Aint *memory_displacements=NULL;
    mca_io_ompio_io_array_t *io_array;
    int num_of_io_entries;
    int *temp_disp_index=NULL;
    MPI_Aint global_count = 0;

    *ret_num_io_entries = 0;
    /**********************************************************************
     ***  7a. Getting ready for next cycle: initializing and freeing buffers
     **********************************************************************/
    if (aggregator == rank) {
        num_of_io_entries = 0;
        
        if (NULL != data->recvtype){
            for (i =0; i< data->procs_per_group; i++) {
                if ( MPI_DATATYPE_NULL != data->recvtype[i] ) {
                    ompi_datatype_destroy(&data->recvtype[i]);
                    data->recvtype[i] = MPI_DATATYPE_NULL;
                }
            }
        }
        
        for(l=0;l<data->procs_per_group;l++){
            data->disp_index[l] =  1;
            
            free(data->blocklen_per_process[l]);
            free(data->displs_per_process[l]);
            
            data->blocklen_per_process[l] = (int *) calloc (1, sizeof(int));
            data->displs_per_process[l] = (MPI_Aint *) calloc (1, sizeof(MPI_Aint));
            if (NULL == data->displs_per_process[l] || NULL == data->blocklen_per_process[l]){
                opal_output (1, "OUT OF MEMORY for displs\n");
                ret = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }
        }
    } /* (aggregator == rank */
    
    /**************************************************************************
     ***  7b. Determine the number of bytes to be actually written in this cycle
     **************************************************************************/
    if (cycles-1 == index) {
        data->bytes_to_write_in_cycle = data->total_bytes - data->bytes_per_cycle*index;
    }
    else {
        data->bytes_to_write_in_cycle = data->bytes_per_cycle;
    }
    
#if DEBUG_ON
    if (aggregator == rank) {
        printf ("****%d: CYCLE %d   Bytes %lld**********\n",
                rank,
                index,
                data->bytes_to_write_in_cycle);
    }
#endif
    /**********************************************************
     **Gather the Data from all the processes at the writers **
     *********************************************************/
    
#if DEBUG_ON
    printf("bytes_to_write_in_cycle: %ld, cycle : %d\n", data->bytes_to_write_in_cycle,
           index);
#endif
    
    /*****************************************************************
     *** 7c. Calculate how much data will be contributed in this cycle
     ***     by each process
     *****************************************************************/
    
    /* The blocklen and displs calculation only done at aggregators!*/
    while (data->bytes_to_write_in_cycle) {
        
        /* This next block identifies which process is the holder
        ** of the sorted[current_index] element;
        */
        blocks = data->fview_count[0];
        for (j=0 ; j<data->procs_per_group ; j++) {
            if (data->sorted[data->current_index] < blocks) {
                data->n = j;
                break;
            }
            else {
                blocks += data->fview_count[j+1];
            }
        }
        
        if (data->bytes_remaining) {
            /* Finish up a partially used buffer from the previous  cycle */
            
            if (data->bytes_remaining <= data->bytes_to_write_in_cycle) {
                /* The data fits completely into the block */
                if (aggregator == rank) {
                    data->blocklen_per_process[data->n][data->disp_index[data->n] - 1] = data->bytes_remaining;
                    data->displs_per_process[data->n][data->disp_index[data->n] - 1] =
                        (OPAL_PTRDIFF_TYPE)data->global_iov_array[data->sorted[data->current_index]].iov_base +
                        (data->global_iov_array[data->sorted[data->current_index]].iov_len
                         - data->bytes_remaining);
                    
                    /* In this cases the length is consumed so allocating for
                       next displacement and blocklength*/
                    data->blocklen_per_process[data->n] = (int *) realloc
                        ((void *)data->blocklen_per_process[data->n], (data->disp_index[data->n]+1)*sizeof(int));
                    data->displs_per_process[data->n] = (MPI_Aint *) realloc
                        ((void *)data->displs_per_process[data->n], (data->disp_index[data->n]+1)*sizeof(MPI_Aint));
                    data->blocklen_per_process[data->n][data->disp_index[data->n]] = 0;
                    data->displs_per_process[data->n][data->disp_index[data->n]] = 0;
                    data->disp_index[data->n] += 1;
                }
                if (data->procs_in_group[data->n] == rank) {
                    bytes_sent += data->bytes_remaining;
                }
                data->current_index ++;
                data->bytes_to_write_in_cycle -= data->bytes_remaining;
                data->bytes_remaining = 0;
//                continue;
//                break;
            }
            else {
                /* the remaining data from the previous cycle is larger than the
                   data->bytes_to_write_in_cycle, so we have to segment again */
                if (aggregator == rank) {
                    data->blocklen_per_process[data->n][data->disp_index[data->n] - 1] = data->bytes_to_write_in_cycle;
                    data->displs_per_process[data->n][data->disp_index[data->n] - 1] =
                        (OPAL_PTRDIFF_TYPE)data->global_iov_array[data->sorted[data->current_index]].iov_base +
                        (data->global_iov_array[data->sorted[data->current_index]].iov_len
                         - data->bytes_remaining);
                }
                
                if (data->procs_in_group[data->n] == rank) {
                    bytes_sent += data->bytes_to_write_in_cycle;
                }
                data->bytes_remaining -= data->bytes_to_write_in_cycle;
                data->bytes_to_write_in_cycle = 0;
                break;
            }
        }
        else {
            /* No partially used entry available, have to start a new one */
            if (data->bytes_to_write_in_cycle <
                (MPI_Aint) data->global_iov_array[data->sorted[data->current_index]].iov_len) {
                /* This entry has more data than we can sendin one cycle */
                if (aggregator == rank) {
                    data->blocklen_per_process[data->n][data->disp_index[data->n] - 1] = data->bytes_to_write_in_cycle;
                    data->displs_per_process[data->n][data->disp_index[data->n] - 1] =
                        (OPAL_PTRDIFF_TYPE)data->global_iov_array[data->sorted[data->current_index]].iov_base ;
                }
                if (data->procs_in_group[data->n] == rank) {
                    bytes_sent += data->bytes_to_write_in_cycle;
                    
                }
                data->bytes_remaining = data->global_iov_array[data->sorted[data->current_index]].iov_len -
                    data->bytes_to_write_in_cycle;
                data->bytes_to_write_in_cycle = 0;
                break;
            }
            else {
                /* Next data entry is less than data->bytes_to_write_in_cycle */
                if (aggregator == rank) {
                    data->blocklen_per_process[data->n][data->disp_index[data->n] - 1] =
                        data->global_iov_array[data->sorted[data->current_index]].iov_len;
                    data->displs_per_process[data->n][data->disp_index[data->n] - 1] = (OPAL_PTRDIFF_TYPE)
                        data->global_iov_array[data->sorted[data->current_index]].iov_base;
                    
                    /*realloc for next blocklength
                      and assign this displacement and check for next displs as
                      the total length of this entry has been consumed!*/
                    data->blocklen_per_process[data->n] =
                        (int *) realloc ((void *)data->blocklen_per_process[data->n], (data->disp_index[data->n]+1)*sizeof(int));
                    data->displs_per_process[data->n] = (MPI_Aint *)realloc
                        ((void *)data->displs_per_process[data->n], (data->disp_index[data->n]+1)*sizeof(MPI_Aint));
                    data->blocklen_per_process[data->n][data->disp_index[data->n]] = 0;
                    data->displs_per_process[data->n][data->disp_index[data->n]] = 0;
                    data->disp_index[data->n] += 1;
                }
                if (data->procs_in_group[data->n] == rank) {
                    bytes_sent += data->global_iov_array[data->sorted[data->current_index]].iov_len;
                }
                data->bytes_to_write_in_cycle -=
                    data->global_iov_array[data->sorted[data->current_index]].iov_len;
                data->current_index ++;
//                continue;
            }
        }
    }
    
    
    /*************************************************************************
     *** 7d. Calculate the displacement on where to put the data and allocate
     ***     the recieve buffer (global_buf)
     *************************************************************************/
    if (aggregator == rank) {
        entries_per_aggregator=0;
        for (i=0;i<data->procs_per_group; i++){
            for (j=0;j<data->disp_index[i];j++){
                if (data->blocklen_per_process[i][j] > 0)
                    entries_per_aggregator++ ;
            }
        }
        
#if DEBUG_ON
        printf("%d: cycle: %d, bytes_sent: %d\n ",rank,index,
               bytes_sent);
        printf("%d : Entries per aggregator : %d\n",rank,entries_per_aggregator);
#endif
        
        if (entries_per_aggregator > 0){
            file_offsets_for_agg = (mca_io_ompio_local_io_array *)
                malloc(entries_per_aggregator*sizeof(mca_io_ompio_local_io_array));
            if (NULL == file_offsets_for_agg) {
                opal_output (1, "OUT OF MEMORY\n");
                ret = OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }
            
            sorted_file_offsets = (int *)
                malloc (entries_per_aggregator*sizeof(int));
            if (NULL == sorted_file_offsets){
                opal_output (1, "OUT OF MEMORY\n");
                ret =  OMPI_ERR_OUT_OF_RESOURCE;
                goto exit;
            }
            
            /*Moving file offsets to an IO array!*/
            temp_index = 0;
            
            for (i=0;i<data->procs_per_group; i++){
                for(j=0;j<data->disp_index[i];j++){
                    if (data->blocklen_per_process[i][j] > 0){
                        file_offsets_for_agg[temp_index].length =
                            data->blocklen_per_process[i][j];
                        file_offsets_for_agg[temp_index].process_id = i;
                        file_offsets_for_agg[temp_index].offset =
                            data->displs_per_process[i][j];
                        temp_index++;
                        
#if DEBUG_ON
                        printf("************Cycle: %d,  Aggregator: %d ***************\n",
                               index+1,rank);
                        
                        printf("%d sends blocklen[%d]: %d, disp[%d]: %ld to %d\n",
                               data->procs_in_group[i],j,
                               data->blocklen_per_process[i][j],j,
                               data->displs_per_process[i][j],
                               rank);
#endif
                    }
                }
            }
        }
      else{
//            continue;
          return OMPI_SUCCESS;
      }
        /* Sort the displacements for each aggregator*/
        local_heap_sort (file_offsets_for_agg,
                         entries_per_aggregator,
                         sorted_file_offsets);
        
        /*create contiguous memory displacements
          based on blocklens on the same displs array
          and map it to this aggregator's actual
          file-displacements (this is in the io-array created above)*/
        memory_displacements = (MPI_Aint *) malloc
            (entries_per_aggregator * sizeof(MPI_Aint));
        
        memory_displacements[sorted_file_offsets[0]] = 0;
        for (i=1; i<entries_per_aggregator; i++){
            memory_displacements[sorted_file_offsets[i]] =
                memory_displacements[sorted_file_offsets[i-1]] +
                file_offsets_for_agg[sorted_file_offsets[i-1]].length;
        }
        
        temp_disp_index = (int *)calloc (1, data->procs_per_group * sizeof (int));
        if (NULL == temp_disp_index) {
            opal_output (1, "OUT OF MEMORY\n");
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
        
        /*Now update the displacements array  with memory offsets*/
        global_count = 0;
        for (i=0;i<entries_per_aggregator;i++){
            temp_pindex =
                file_offsets_for_agg[sorted_file_offsets[i]].process_id;
            data->displs_per_process[temp_pindex][temp_disp_index[temp_pindex]] =
                memory_displacements[sorted_file_offsets[i]];
            if (temp_disp_index[temp_pindex] < data->disp_index[temp_pindex])
                temp_disp_index[temp_pindex] += 1;
            else{
                printf("temp_disp_index[%d]: %d is greater than disp_index[%d]: %d\n",
                       temp_pindex, temp_disp_index[temp_pindex],
                       temp_pindex, data->disp_index[temp_pindex]);
            }
            global_count +=
                file_offsets_for_agg[sorted_file_offsets[i]].length;
        }
        
        if (NULL != temp_disp_index){
            free(temp_disp_index);
            temp_disp_index = NULL;
        }
        
#if DEBUG_ON
        
        printf("************Cycle: %d,  Aggregator: %d ***************\n",
               index+1,rank);
        for (i=0;i<data->procs_per_group; i++){
            for(j=0;j<data->disp_index[i];j++){
                if (data->blocklen_per_process[i][j] > 0){
                    printf("%d sends blocklen[%d]: %d, disp[%d]: %ld to %d\n",
                           data->procs_in_group[i],j,
                           data->blocklen_per_process[i][j],j,
                           data->displs_per_process[i][j],
                           rank);
                    
                }
            }
        }
        printf("************Cycle: %d,  Aggregator: %d ***************\n",
               index+1,rank);
        for (i=0; i<entries_per_aggregator;i++){
            printf("%d: OFFSET: %lld   LENGTH: %ld, Mem-offset: %ld\n",
                   file_offsets_for_agg[sorted_file_offsets[i]].process_id,
                   file_offsets_for_agg[sorted_file_offsets[i]].offset,
                   file_offsets_for_agg[sorted_file_offsets[i]].length,
                   memory_displacements[sorted_file_offsets[i]]);
        }
        printf("%d : global_count : %ld, bytes_sent : %d\n",
               rank,global_count, bytes_sent);
#endif
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
        start_comm_time = MPI_Wtime();
#endif
        /*************************************************************************
         *** 7e. Perform the actual communication
         *************************************************************************/
        for (i=0;i<data->procs_per_group; i++) {
            size_t datatype_size;
            data->recv_req[i] = MPI_REQUEST_NULL;
            if ( 0 < data->disp_index[i] ) {
                ompi_datatype_create_hindexed(data->disp_index[i],
                                              data->blocklen_per_process[i],
                                              data->displs_per_process[i],
                                              MPI_BYTE,
                                              &data->recvtype[i]);
                ompi_datatype_commit(&data->recvtype[i]);
                opal_datatype_type_size(&data->recvtype[i]->super, &datatype_size);
                
                if (datatype_size){
                    ret = MCA_PML_CALL(irecv(data->global_buf,
                                             1,
                                             data->recvtype[i],
                                             data->procs_in_group[i],
                                             123,
                                             data->comm,
                                             &data->recv_req[i]));
                    if (OMPI_SUCCESS != ret){
                        goto exit;
                    }
                }
            }
        }
    } /* end if (aggregator == rank ) */
    
    
    if ( data->sendbuf_is_contiguous ) {
        send_buf = &((char*)data->buf)[data->total_bytes_written];
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
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
        
        remaining = bytes_sent;
        
        while (remaining) {
            mem_address = (OPAL_PTRDIFF_TYPE)
                (data->decoded_iov[data->iov_index].iov_base) + data->current_position;
            
            if (remaining >=
                (data->decoded_iov[data->iov_index].iov_len - data->current_position)) {
                memcpy (send_buf+temp_position,
                        (IOVBASE_TYPE *)mem_address,
                        data->decoded_iov[data->iov_index].iov_len - data->current_position);
                remaining = remaining -
                    (data->decoded_iov[data->iov_index].iov_len - data->current_position);
                temp_position = temp_position +
                    (data->decoded_iov[data->iov_index].iov_len - data->current_position);
                data->iov_index = data->iov_index + 1;
                data->current_position = 0;
            }
            else {
                memcpy (send_buf+temp_position,
                        (IOVBASE_TYPE *) mem_address,
                        remaining);
                data->current_position += remaining;
                remaining = 0;
            }
        }
    }
    data->total_bytes_written += bytes_sent;
    
    /* Gather the sendbuf from each process in appropritate locations in
       aggregators*/
    
    if (bytes_sent){
        ret = MCA_PML_CALL(isend(send_buf,
                                 bytes_sent,
                                 MPI_BYTE,
                                 aggregator,
                                 123,
                                 MCA_PML_BASE_SEND_STANDARD,
                                 data->comm,
                                 &send_req));
        
        
        if ( OMPI_SUCCESS != ret ){
            goto exit;
        }
        
        ret = ompi_request_wait(&send_req, MPI_STATUS_IGNORE);
        if (OMPI_SUCCESS != ret){
            goto exit;
        }
    }
    
    if (aggregator == rank) {
        ret = ompi_request_wait_all (data->procs_per_group,
                                     data->recv_req,
                                     MPI_STATUS_IGNORE);
        
        if (OMPI_SUCCESS != ret){
            goto exit;
        }
    }
    
#if DEBUG_ON
    if (aggregator == rank){
        printf("************Cycle: %d,  Aggregator: %d ***************\n",
               index+1,rank);
        for (i=0 ; i<global_count/4 ; i++)
            printf (" RECV %d \n",((int *)data->global_buf)[i]);
    }
#endif
    
    if (! data->sendbuf_is_contiguous) {
        if (NULL != send_buf) {
            free (send_buf);
            send_buf = NULL;
        }
    }
    
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
    end_comm_time = MPI_Wtime();
    comm_time += (end_comm_time - start_comm_time);
#endif
    /**********************************************************
     *** 7f. Create the io array, and pass it to fbtl
     *********************************************************/
    
    if (aggregator == rank) {
        
#if OMPIO_FCOLL_WANT_TIME_BREAKDOWN
        start_write_time = MPI_Wtime();
#endif
        
        io_array = (mca_io_ompio_io_array_t *) malloc
            (entries_per_aggregator * sizeof (mca_io_ompio_io_array_t));
        if (NULL == io_array) {
            opal_output(1, "OUT OF MEMORY\n");
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
        
        num_of_io_entries = 0;
        /*First entry for every aggregator*/
        io_array[0].offset =
            (IOVBASE_TYPE *)(intptr_t)file_offsets_for_agg[sorted_file_offsets[0]].offset;
        io_array[0].length =
            file_offsets_for_agg[sorted_file_offsets[0]].length;
        io_array[0].memory_address =
            data->global_buf+memory_displacements[sorted_file_offsets[0]];
        num_of_io_entries++;
        
        for (i=1;i<entries_per_aggregator;i++){
            /* If the enrties are contiguous merge them,
               else make a new entry */
            if (file_offsets_for_agg[sorted_file_offsets[i-1]].offset +
                file_offsets_for_agg[sorted_file_offsets[i-1]].length ==
                file_offsets_for_agg[sorted_file_offsets[i]].offset){
                io_array[num_of_io_entries - 1].length +=
                    file_offsets_for_agg[sorted_file_offsets[i]].length;
            }
            else {
                io_array[num_of_io_entries].offset =
                    (IOVBASE_TYPE *)(intptr_t)file_offsets_for_agg[sorted_file_offsets[i]].offset;
                io_array[num_of_io_entries].length =
                    file_offsets_for_agg[sorted_file_offsets[i]].length;
                io_array[num_of_io_entries].memory_address =
                    data->global_buf+memory_displacements[sorted_file_offsets[i]];
                num_of_io_entries++;
            }
            
        }
        
#if DEBUG_ON
        printf("*************************** %d\n", num_of_io_entries);
        for (i=0 ; i<num_of_io_entries ; i++) {
            printf(" ADDRESS: %p  OFFSET: %ld   LENGTH: %ld\n",
                   io_array[i].memory_address,
                   (OPAL_PTRDIFF_TYPE)io_array[i].offset,
                   io_array[i].length);
        }
        
#endif
    }
        
exit:
    free(sorted_file_offsets);
    free(file_offsets_for_agg);
    free(memory_displacements);
    
    
    *ret_num_io_entries = num_of_io_entries;
    *ret_io_array = io_array;

    return OMPI_SUCCESS;
}
    
    

int mca_fcoll_dynamic_gen2_break_file_view ( struct iovec *mem_iov, int mem_count, 
                                        struct iovec *file_iov, int file_count, 
                                        struct iovec ***ret_broken_mem_iovs, int **ret_broken_mem_counts,
                                        struct iovec ***ret_broken_file_iovs, int **ret_broken_file_counts, 
                                        MPI_Aint **ret_broken_total_lengths,
                                        int stripe_count, int stripe_size)
{
    int i, j, ret=OMPI_SUCCESS;
    struct iovec **broken_mem_iovs=NULL; 
    int *broken_mem_counts=NULL;
    struct iovec **broken_file_iovs=NULL; 
    int *broken_file_counts=NULL;
    MPI_Aint *broken_total_lengths=NULL;
    int **block=NULL, **max_lengths=NULL;
    
    broken_mem_iovs  = (struct iovec **) malloc ( stripe_count * sizeof(struct iovec *)); 
    broken_file_iovs = (struct iovec **) malloc ( stripe_count * sizeof(struct iovec *)); 
    if ( NULL == broken_mem_iovs || NULL == broken_file_iovs ) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }
    for ( i=0; i<stripe_count; i++ ) {
        broken_mem_iovs[i]  = (struct iovec*) malloc (sizeof(struct iovec ));
        broken_file_iovs[i] = (struct iovec*) malloc (sizeof(struct iovec ));
    }
    
    broken_mem_counts    = (int *) calloc ( stripe_count, sizeof(int));
    broken_file_counts   = (int *) calloc ( stripe_count, sizeof(int));
    broken_total_lengths = (MPI_Aint *) calloc ( stripe_count, sizeof(MPI_Aint));
    if ( NULL == broken_mem_counts || NULL == broken_file_counts ||
         NULL == broken_total_lengths ) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    block       = (int **) malloc ( stripe_count * sizeof(int *));
    max_lengths = (int **) malloc ( stripe_count * sizeof(int *));
    if ( NULL == block || NULL == max_lengths ) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }
    
    for ( i=0; i<stripe_count; i++ ){
        block[i]       = (int *) malloc ( 5 * sizeof(int));
        max_lengths[i] = (int *) malloc ( 2 * sizeof(int));
        if ( NULL == block[i] || NULL == max_lengths[i]) {
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto exit;
        }
        max_lengths[i][0] = 1;
        max_lengths[i][1] = 1;
        
        for ( j=0; j<5; j++ ) {
            block[i][j]=2;
        }
    }
    
    /* Step 1: separate the local_iov_array per aggregator */
    int owner, rest, len, temp_len, blocklen, memlen;
    off_t offset, temp_offset, start_offset, memoffset;

    i=j=0;

    if ( 0 < mem_count ) {
        memoffset = (off_t) mem_iov[j].iov_base;
        memlen    = mem_iov[j].iov_len;
    }
    while ( i < file_count) {
        offset = (off_t) file_iov[i].iov_base;
        len    = file_iov[i].iov_len;


#if DEBUG_ON
        printf("%d:file_iov[%d].base=%ld .len=%d\n", rank, i, 
               file_iov[i].iov_base, file_iov[i].iov_len);
#endif
        do {
            owner        = (offset / stripe_size ) % stripe_count;
            start_offset = (offset / stripe_size );
            rest         = (start_offset + 1) * stripe_size - offset;

            if ( len >= rest ) {
                blocklen    = rest;
                temp_offset = offset+rest;
                temp_len    = len - rest;
            }
            else {
                blocklen    = len;
                temp_offset = 0;
                temp_len    = 0;
            }
            
            broken_file_iovs[owner][broken_file_counts[owner]].iov_base = (void *)offset;
            broken_file_iovs[owner][broken_file_counts[owner]].iov_len  = blocklen;
#if DEBUG_ON
            printf("%d: owner=%d b_file_iovs[%d].base=%ld .len=%d \n", rank, owner, 
                   broken_file_counts[owner], 
                   broken_file_iovs[owner][broken_file_counts[owner]].iov_base, 
                   broken_file_iovs[owner][broken_file_counts[owner]].iov_len );
#endif
            do {
                if ( memlen >=  blocklen ) {
                    broken_mem_iovs[owner][broken_mem_counts[owner]].iov_base = (void *) memoffset;
                    broken_mem_iovs[owner][broken_mem_counts[owner]].iov_len  = blocklen;
                    memoffset += blocklen;
                    memlen    -= blocklen;
                    blocklen   = 0;

                    if ( 0 == memlen ) {
                        j++;
                        memoffset = (off_t ) mem_iov[j].iov_base;
                        memlen    = mem_iov[j].iov_len;
                    }
                }                
                else {
                    broken_mem_iovs[owner][broken_mem_counts[owner]].iov_base = (void *) memoffset;
                    broken_mem_iovs[owner][broken_mem_counts[owner]].iov_len  = memlen;
                    blocklen -= memlen;
                    
                    j++;
                    memoffset = (off_t ) mem_iov[j].iov_base;
                    memlen    = mem_iov[j].iov_len;
                }
#if DEBUG_ON
                printf("%d: owner=%d b_mem_iovs[%d].base=%ld .len=%d\n", rank, owner,
                       broken_mem_counts[owner],
                       broken_mem_iovs[owner][broken_mem_counts[owner]].iov_base,
                       broken_mem_iovs[owner][broken_mem_counts[owner]].iov_len);
#endif

                broken_mem_counts[owner]++;
                if ( broken_mem_counts[owner] >= max_lengths[owner][0] ) {
                    broken_mem_iovs[owner] = (struct iovec*) realloc ( broken_mem_iovs[owner],
                                                                       mem_count * block[owner][0] * 
                                                                       sizeof(struct iovec ));
                    max_lengths[owner][0] = mem_count * block[owner][0];
                    block[owner][0]++;
                }

            } while ( blocklen > 0 );

            broken_file_counts[owner]++;
            if ( broken_file_counts[owner] >= max_lengths[owner][1] ) {
                broken_file_iovs[owner] = (struct iovec*) realloc ( broken_file_iovs[owner],
                                                                    file_count * block[owner][1] * 
                                                                    sizeof(struct iovec ));
                max_lengths[owner][1] = file_count * block[owner][1];
                block[owner][1]++;
            }

            offset = temp_offset;
            len    = temp_len;
        } while( temp_len > 0 );

        i++;
    } 

    
    /* Step 2: recalculating the total lengths per aggregator */
    for ( i=0; i< stripe_count; i++ ) {
        for ( j=0; j<broken_file_counts[i]; j++ ) {
            broken_total_lengths[i] += broken_file_iovs[i][j].iov_len;
        }
#if DEBUG_ON
        printf("%d: broken_total_lengths[%d] = %d\n", rank, i, broken_total_lengths[i]);
#endif
    }

    *ret_broken_mem_iovs      = broken_mem_iovs;
    *ret_broken_mem_counts    = broken_mem_counts;
    *ret_broken_file_iovs     = broken_file_iovs;
    *ret_broken_file_counts   = broken_file_counts;
    *ret_broken_total_lengths = broken_total_lengths;    

    if ( NULL != block) {
        for ( i=0; i<stripe_count; i++ ){
            free (block[i] );
        }
        free ( block);
    }
    if ( NULL != max_lengths) {
        for ( i=0; i<stripe_count; i++ ){
            free (max_lengths[i] );
        }
        free ( max_lengths);
    }

    return ret;

exit:
    free ( broken_mem_iovs);    
    free ( broken_mem_counts);
    free ( broken_file_iovs );
    free ( broken_file_counts);
    free ( broken_total_lengths);

    if ( NULL != block) {
        for ( i=0; i<stripe_count; i++ ){
            free (block[i] );
        }
        free ( block);
    }
    if ( NULL != max_lengths) {
        for ( i=0; i<stripe_count; i++ ){
            free (max_lengths[i] );
        }
        free ( max_lengths);
    }

    *ret_broken_mem_iovs      = NULL;
    *ret_broken_mem_counts    = NULL;
    *ret_broken_file_iovs     = NULL;
    *ret_broken_file_counts   = NULL;
    *ret_broken_total_lengths = NULL;

    return ret;
}


int mca_fcoll_dynamic_gen2_get_configuration (mca_io_ompio_file_t *fh, int *dynamic_gen2_num_io_procs, int **ret_aggregators)
{
    int *aggregators=NULL;
    int num_io_procs = *dynamic_gen2_num_io_procs;
    int i;

    if ( num_io_procs < 1 ) {
        num_io_procs = fh->f_stripe_count;
        if ( num_io_procs < 1 ) {
            num_io_procs = 1;
        }
        if ( num_io_procs > fh->f_size ) {
            num_io_procs = fh->f_size;
        }
    }

    fh->f_procs_per_group = fh->f_size;
    fh->f_procs_in_group = (int *) malloc ( sizeof(int) * fh->f_size );
    if ( NULL == fh->f_procs_in_group) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for (i=0; i<fh->f_size; i++ ) {
        fh->f_procs_in_group[i]=i;
    }


    aggregators = (int *) malloc ( num_io_procs * sizeof(int));
    if ( NULL == aggregators ) {
        // fh->procs_in_group will be freed with the fh structure. No need to do it here.
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for ( i=0; i<num_io_procs; i++ ) {
        aggregators[i] = i * fh->f_size / num_io_procs;
    }

    *dynamic_gen2_num_io_procs = num_io_procs;
    *ret_aggregators = aggregators;

    return OMPI_SUCCESS;
}    
    
    
static int local_heap_sort (mca_io_ompio_local_io_array *io_array,
			    int num_entries,
			    int *sorted)
{
    int i = 0;
    int j = 0;
    int left = 0;
    int right = 0;
    int largest = 0;
    int heap_size = num_entries - 1;
    int temp = 0;
    unsigned char done = 0;
    int* temp_arr = NULL;

    temp_arr = (int*)malloc(num_entries*sizeof(int));
    if (NULL == temp_arr) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    temp_arr[0] = 0;
    for (i = 1; i < num_entries; ++i) {
        temp_arr[i] = i;
    }
    /* num_entries can be a large no. so NO RECURSION */
    for (i = num_entries/2-1 ; i>=0 ; i--) {
        done = 0;
        j = i;
        largest = j;

        while (!done) {
            left = j*2+1;
            right = j*2+2;
            if ((left <= heap_size) &&
                (io_array[temp_arr[left]].offset > io_array[temp_arr[j]].offset)) {
                largest = left;
            }
            else {
                largest = j;
            }
            if ((right <= heap_size) &&
                (io_array[temp_arr[right]].offset >
                 io_array[temp_arr[largest]].offset)) {
                largest = right;
            }
            if (largest != j) {
                temp = temp_arr[largest];
                temp_arr[largest] = temp_arr[j];
                temp_arr[j] = temp;
                j = largest;
            }
            else {
                done = 1;
            }
        }
    }

    for (i = num_entries-1; i >=1; --i) {
        temp = temp_arr[0];
        temp_arr[0] = temp_arr[i];
        temp_arr[i] = temp;
        heap_size--;
        done = 0;
        j = 0;
        largest = j;

        while (!done) {
            left =  j*2+1;
            right = j*2+2;

            if ((left <= heap_size) &&
                (io_array[temp_arr[left]].offset >
                 io_array[temp_arr[j]].offset)) {
                largest = left;
            }
            else {
                largest = j;
            }
            if ((right <= heap_size) &&
                (io_array[temp_arr[right]].offset >
                 io_array[temp_arr[largest]].offset)) {
                largest = right;
            }
            if (largest != j) {
                temp = temp_arr[largest];
                temp_arr[largest] = temp_arr[j];
                temp_arr[j] = temp;
                j = largest;
            }
            else {
                done = 1;
            }
        }
        sorted[i] = temp_arr[i];
    }
    sorted[0] = temp_arr[0];

    if (NULL != temp_arr) {
        free(temp_arr);
        temp_arr = NULL;
    }
    return OMPI_SUCCESS;
}

