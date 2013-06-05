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


#define DEBUG_ON 0
#define TIME_BREAKDOWN 0

/*Used for loading file-offsets per aggregator*/
typedef struct local_io_array{
  OMPI_MPI_OFFSET_TYPE offset;
  MPI_Aint             length;  
  int                  process_id;
}local_io_array;



static int local_heap_sort (local_io_array *io_array,
			    int num_entries,
			    int *sorted);


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
    int i=0, j=0, l=0;
    int n=0; /* current position in total_bytes_per_process array */
    MPI_Aint bytes_remaining = 0; /* how many bytes have been written from the current
                                value from total_bytes_per_process */
    int bytes_sent = 0, ret =0;
    int blocks=0, entries_per_aggregator=0;

    /* iovec structure and count of the buffer passed in */
    uint32_t iov_count = 0;
    struct iovec *decoded_iov = NULL;
    int iov_index = 0;
    char *send_buf = NULL;
    size_t current_position = 0;
    struct iovec *local_iov_array=NULL, *global_iov_array=NULL;
    local_io_array *file_offsets_for_agg=NULL;
    /* global iovec at the writers that contain the iovecs created from
       file_set_view */
    uint32_t total_fview_count = 0;
    int local_count = 0, temp_pindex;
    int *fview_count = NULL, *disp_index=NULL, *temp_disp_index=NULL;
    int current_index = 0, temp_index=0;

    char *global_buf = NULL;
    MPI_Aint global_count = 0;
    
    
    /* array that contains the sorted indices of the global_iov */
    int *sorted = NULL, *sorted_file_offsets=NULL;
    int *displs = NULL;
    size_t max_data = 0, datatype_size = 0; 
    int **blocklen_per_process=NULL;
    MPI_Aint **displs_per_process=NULL, *memory_displacements=NULL;
    ompi_datatype_t **recvtype = NULL;
    MPI_Aint *total_bytes_per_process = NULL;
    MPI_Request *send_req=NULL, *recv_req=NULL;
    int recv_req_count=0;
    

#if TIME_BREAKDOWN
    double write_time = 0.0, start_write_time = 0.0, end_write_time = 0.0;
    double comm_time = 0.0, start_comm_time = 0.0, end_comm_time = 0.0;
    double exch_write = 0.0, start_exch = 0.0, end_exch = 0.0;
    print_entry nentry;
#endif


    if (opal_datatype_is_contiguous_memory_layout(&datatype->super,1)) {
        fh->f_flags |= OMPIO_CONTIGUOUS_MEMORY;
    }

    /**************************************************************************
     ** In case the data is not contigous in memory, decode it into an iovec **
     **************************************************************************/
    if (! (fh->f_flags & OMPIO_CONTIGUOUS_MEMORY)) {
      ret =   ompi_io_ompio_decode_datatype (fh,
					     datatype,
					     count,
					     buf,
					     &max_data,
					     &decoded_iov,
					     &iov_count);
      if (OMPI_SUCCESS != ret ){
	goto exit;
      }
    }
    else {
        max_data = count * datatype->super.size;
    }

    if ( MPI_STATUS_IGNORE != status ) {
	status->_ucount = max_data;
    }
	

    if (! (fh->f_flags & OMPIO_AGGREGATOR_IS_SET)) {
	ret = ompi_io_ompio_set_aggregator_props (fh, 
						  mca_fcoll_dynamic_num_io_procs,
						  max_data);
	
	if (OMPI_SUCCESS != ret){
	    goto exit;
	}
    }

    if (-1 == mca_fcoll_dynamic_num_io_procs) {
        mca_fcoll_dynamic_num_io_procs = 1;
    }




    total_bytes_per_process = (MPI_Aint*)malloc
        (fh->f_procs_per_group*sizeof(MPI_Aint));
    if (NULL == total_bytes_per_process) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
	goto exit;
    }

    ret = ompi_io_ompio_allgather_array (&max_data,
					 1,
					 MPI_LONG,
					 total_bytes_per_process,
					 1,
					 MPI_LONG,
					 fh->f_aggregator_index,
					 fh->f_procs_in_group,
					 fh->f_procs_per_group,
					 fh->f_comm);
    
    if( OMPI_SUCCESS != ret){
      goto exit;
    }
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
    ret = ompi_io_ompio_generate_current_file_view(fh,
						   max_data,
						   &local_iov_array,
						   &local_count);
    if (ret != OMPI_SUCCESS){
      goto exit;
    }

#if DEBUG_ON    
    for (i=0 ; i<local_count ; i++) {
   
      printf("%d: OFFSET: %d   LENGTH: %ld\n",
	     fh->f_rank,
	     local_iov_array[i].iov_base,
	     local_iov_array[i].iov_len);

    }
#endif   

    /*************************************************************
     *** ALLGather the File View information at all processes ***
     *************************************************************/

    fview_count = (int *) malloc (fh->f_procs_per_group * sizeof (int));
    if (NULL == fview_count) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
	goto exit;
    }

    ret = ompi_io_ompio_allgather_array (&local_count,
					 1,
					 MPI_INT,
					 fview_count,
					 1,
					 MPI_INT,
					 fh->f_aggregator_index,
					 fh->f_procs_in_group,
					 fh->f_procs_per_group,
					 fh->f_comm);
    
    if( OMPI_SUCCESS != ret){
      goto exit;
    }

    displs = (int*) malloc (fh->f_procs_per_group * sizeof (int));
    if (NULL == displs) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
	goto exit;
    }

    displs[0] = 0;
    total_fview_count = fview_count[0];
    for (i=1 ; i<fh->f_procs_per_group ; i++) {
        total_fview_count += fview_count[i];
        displs[i] = displs[i-1] + fview_count[i-1];
    }
    
#if DEBUG_ON
    printf("total_fview_count : %d\n", total_fview_count);
    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
        for (i=0 ; i<fh->f_procs_per_group ; i++) {
            printf ("%d: PROCESS: %d  ELEMENTS: %d  DISPLS: %d\n",
                    fh->f_rank,
                    i,
                    fview_count[i],
                    displs[i]);
        }
    }
#endif
    
    /* allocate the global iovec  */

    if (0 != total_fview_count) {
      global_iov_array = (struct iovec*) malloc (total_fview_count *
						    sizeof(struct iovec));
      if (NULL == global_iov_array){
	opal_output(1, "OUT OF MEMORY\n");
	ret = OMPI_ERR_OUT_OF_RESOURCE;
	goto exit;
      }

    }
    
    ret = ompi_io_ompio_allgatherv_array (local_iov_array,
					  local_count,
					  fh->f_iov_type,
					  global_iov_array,
					  fview_count,
					  displs,
					  fh->f_iov_type,
					  fh->f_aggregator_index,
					  fh->f_procs_in_group,
					  fh->f_procs_per_group,
					  fh->f_comm);
    if (OMPI_SUCCESS != ret){
      goto exit;
    }

    /* sort it */
    if (0 != total_fview_count) {
        sorted = (int *)malloc (total_fview_count * sizeof(int));
        if (NULL == sorted) {
            opal_output (1, "OUT OF MEMORY\n");
            ret = OMPI_ERR_OUT_OF_RESOURCE;
	    goto exit;
        }
	ompi_io_ompio_sort_iovec (global_iov_array, total_fview_count, sorted);
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
    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
      uint32_t tv=0;
      for (tv=0 ; tv<total_fview_count ; tv++) {
	printf("%d: OFFSET: %lld   LENGTH: %ld\n",
	       fh->f_rank,
	       global_iov_array[sorted[tv]].iov_base,
	       global_iov_array[sorted[tv]].iov_len);
      }
    }
#endif
    
    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
        disp_index = (int *)malloc (fh->f_procs_per_group * sizeof (int));
        if (NULL == disp_index) {
            opal_output (1, "OUT OF MEMORY\n");
            ret = OMPI_ERR_OUT_OF_RESOURCE;
	    goto exit;
        }

	blocklen_per_process = (int **)malloc (fh->f_procs_per_group * sizeof (int*));
        if (NULL == blocklen_per_process) {
            opal_output (1, "OUT OF MEMORY\n");
            ret = OMPI_ERR_OUT_OF_RESOURCE;
	    goto exit;
        }

	displs_per_process = (MPI_Aint **)malloc (fh->f_procs_per_group * sizeof (MPI_Aint*));
	if (NULL == displs_per_process) {
            opal_output (1, "OUT OF MEMORY\n");
            ret = OMPI_ERR_OUT_OF_RESOURCE;
	    goto exit;
        }

	for(i=0;i<fh->f_procs_per_group;i++){
	  blocklen_per_process[i] = NULL;
	  displs_per_process[i] = NULL;
	}
    }
    

    bytes_per_cycle = mca_fcoll_dynamic_cycle_buffer_size;
    cycles = ceil((double)total_bytes/bytes_per_cycle);


    n = 0; 
    bytes_remaining = 0;
    current_index = 0;




#if TIME_BREAKDOWN
      start_exch = MPI_Wtime();
#endif
    
    for (index = 0; index < cycles; index++) {

      /* Getting ready for next cycle 
	 Initializing and freeing buffers*/
      if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
	
	if (NULL == recvtype){
	  recvtype = (ompi_datatype_t **) 
	    malloc (fh->f_procs_per_group  * sizeof(ompi_datatype_t *));
	    if (NULL == recvtype) {
	      opal_output (1, "OUT OF MEMORY\n");
	      ret = OMPI_ERR_OUT_OF_RESOURCE;
	      goto exit;
	    }
	}
	
	for(l=0;l<fh->f_procs_per_group;l++){
	  disp_index[l] =  1;
	 
	  if (NULL != blocklen_per_process[l]){
	    free(blocklen_per_process[l]);
	    blocklen_per_process[l] = NULL;
	  }
	  if (NULL != displs_per_process[l]){
	    free(displs_per_process[l]);
	    displs_per_process[l] = NULL;
	  }
	  blocklen_per_process[l] = (int *) calloc (1, sizeof(int));
	  if (NULL == blocklen_per_process[l]) {
	    opal_output (1, "OUT OF MEMORY for blocklen\n");
	    ret = OMPI_ERR_OUT_OF_RESOURCE;
	    goto exit;
	  }
	  displs_per_process[l] = (MPI_Aint *) calloc (1, sizeof(MPI_Aint));
	  if (NULL == displs_per_process[l]){      
	      opal_output (1, "OUT OF MEMORY for displs\n");
	      ret = OMPI_ERR_OUT_OF_RESOURCE;
	      goto exit;
	  }
	}
	
	if (NULL != sorted_file_offsets){
	  free(sorted_file_offsets);
	  sorted_file_offsets = NULL;
	}
	
	if(NULL != file_offsets_for_agg){
	  free(file_offsets_for_agg);
	  file_offsets_for_agg = NULL;
	}
	
	if (NULL != memory_displacements){
	  free(memory_displacements);
	  memory_displacements = NULL;
	}
	
      }
      
      if (cycles-1 == index) {
	bytes_to_write_in_cycle = total_bytes - bytes_per_cycle*index;
      }
      else {
	bytes_to_write_in_cycle = bytes_per_cycle;
      }
      
#if DEBUG_ON
      if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
	printf ("****%d: CYCLE %d   Bytes %lld**********\n",
		fh->f_rank,
		index, 
		bytes_to_write_in_cycle);
      }
#endif
      /**********************************************************
       **Gather the Data from all the processes at the writers **
       *********************************************************/
      
      /* Calculate how much data will be contributed in this cycle 
	 by each process*/
      bytes_sent = 0;
#if DEBUG_ON
      printf("bytes_to_write_in_cycle: %ld, cycle : %d\n", bytes_to_write_in_cycle,
	       index);
#endif
      /* The blocklen and displs calculation only done at aggregators!*/


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
	      blocklen_per_process[n][disp_index[n] - 1] = bytes_remaining;
	      displs_per_process[n][disp_index[n] - 1] = 
		(OPAL_PTRDIFF_TYPE)global_iov_array[sorted[current_index]].iov_base + 
		(global_iov_array[sorted[current_index]].iov_len
		 - bytes_remaining);
	      
	    }
	    if (fh->f_procs_in_group[n] == fh->f_rank) {
	      bytes_sent += bytes_remaining;
	    }
	    current_index ++;
	    bytes_to_write_in_cycle -= bytes_remaining;
	    bytes_remaining = 0;
	    if (fh->f_procs_in_group[fh->f_aggregator_index] == 
		fh->f_rank) {		
	    /* In this cases the length is consumed so allocating for
	       next displacement and blocklength*/
     
	      blocklen_per_process[n] = (int *) realloc
		((void *)blocklen_per_process[n], (disp_index[n]+1)*sizeof(int));
	      displs_per_process[n] = (MPI_Aint *) realloc
		((void *)displs_per_process[n], (disp_index[n]+1)*sizeof(MPI_Aint));
	      blocklen_per_process[n][disp_index[n]] = 0;
	      displs_per_process[n][disp_index[n]] = 0;
	      disp_index[n] += 1;
	    }
	    continue;
	  }
	  else {
	    if (fh->f_procs_in_group[fh->f_aggregator_index] == 
		fh->f_rank) {
	      blocklen_per_process[n][disp_index[n] - 1] = bytes_to_write_in_cycle;
	      displs_per_process[n][disp_index[n] - 1] = 
		(OPAL_PTRDIFF_TYPE)global_iov_array[sorted[current_index]].iov_base +
                (global_iov_array[sorted[current_index]].iov_len
                 - bytes_remaining);
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
	      (MPI_Aint) global_iov_array[sorted[current_index]].iov_len) {
	    if (fh->f_procs_in_group[fh->f_aggregator_index] == 
		fh->f_rank) {

	      blocklen_per_process[n][disp_index[n] - 1] = bytes_to_write_in_cycle;
	      displs_per_process[n][disp_index[n] - 1] = 
		(OPAL_PTRDIFF_TYPE)global_iov_array[sorted[current_index]].iov_base ;


	    }
	    if (fh->f_procs_in_group[n] == fh->f_rank) {
	      bytes_sent += bytes_to_write_in_cycle;
	      
	    }
	    bytes_remaining = global_iov_array[sorted[current_index]].iov_len - 
	      bytes_to_write_in_cycle;
	    bytes_to_write_in_cycle = 0;
	    break;
	  }
	  else {
	    if (fh->f_procs_in_group[fh->f_aggregator_index] == 
		fh->f_rank) {
	      blocklen_per_process[n][disp_index[n] - 1] =
		global_iov_array[sorted[current_index]].iov_len;
	      displs_per_process[n][disp_index[n] - 1] = (OPAL_PTRDIFF_TYPE)
		global_iov_array[sorted[current_index]].iov_base;
	      
	      blocklen_per_process[n] = 
		(int *) realloc ((void *)blocklen_per_process[n], (disp_index[n]+1)*sizeof(int));
	      displs_per_process[n] = (MPI_Aint *)realloc
		((void *)displs_per_process[n], (disp_index[n]+1)*sizeof(MPI_Aint));
	      blocklen_per_process[n][disp_index[n]] = 0;
	      displs_per_process[n][disp_index[n]] = 0;
	      disp_index[n] += 1;
	      /*realloc for next blocklength 
		and assign this displacement and check for next displs as
	      the total length of this entry has been consumed!*/
	    }
	    if (fh->f_procs_in_group[n] == fh->f_rank) {
	      bytes_sent += global_iov_array[sorted[current_index]].iov_len;
	    }
	    bytes_to_write_in_cycle -= 
	      global_iov_array[sorted[current_index]].iov_len;
	    current_index ++;
	    continue;
	  }
	}
      }
      

      /* Calculate the displacement on where to put the data and allocate
	 the recieve buffer (global_buf) */
      if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
	entries_per_aggregator=0;
	for (i=0;i<fh->f_procs_per_group; i++){
	  for (j=0;j<disp_index[i];j++){
	    if (blocklen_per_process[i][j] > 0) 
	      entries_per_aggregator++ ;
	  }
	}

#if DEBUG_ON
	printf("%d: cycle: %d, bytes_sent: %d\n ",fh->f_rank,index,
		 bytes_sent);
	printf("%d : Entries per aggregator : %d\n",fh->f_rank,entries_per_aggregator);
#endif
	
	if (entries_per_aggregator > 0){
	  file_offsets_for_agg = (local_io_array *)
	    malloc(entries_per_aggregator*sizeof(local_io_array));
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
	 	  
	  for (i=0;i<fh->f_procs_per_group; i++){
	    for(j=0;j<disp_index[i];j++){
	      if (blocklen_per_process[i][j] > 0){
		file_offsets_for_agg[temp_index].length =
		  blocklen_per_process[i][j];
		file_offsets_for_agg[temp_index].process_id = i;
		file_offsets_for_agg[temp_index].offset = 
		  displs_per_process[i][j];
		temp_index++;
 
#if DEBUG_ON
		printf("************Cycle: %d,  Aggregator: %d ***************\n", 
		       index+1,fh->f_rank);
		
		printf("%d sends blocklen[%d]: %d, disp[%d]: %ld to %d\n",
		       fh->f_procs_in_group[i],j,
		     blocklen_per_process[i][j],j,
		     displs_per_process[i][j],
		     fh->f_rank);
#endif
	      }
	    }
	  }
	}
	else{
	  continue;
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
	
	temp_disp_index = (int *)calloc (1, fh->f_procs_per_group * sizeof (int));
	if (NULL == temp_disp_index) {
	  opal_output (1, "OUT OF MEMORY\n");
	  return OMPI_ERR_OUT_OF_RESOURCE;
	}
	
	/*Now update the displacements array  with memory offsets*/
	global_count = 0;
	for (i=0;i<entries_per_aggregator;i++){
	  temp_pindex =
	    file_offsets_for_agg[sorted_file_offsets[i]].process_id;
	  displs_per_process[temp_pindex][temp_disp_index[temp_pindex]] =
	    memory_displacements[sorted_file_offsets[i]];
	  if (temp_disp_index[temp_pindex] < disp_index[temp_pindex])
	      temp_disp_index[temp_pindex] += 1;
	  else{
	    printf("temp_disp_index[%d]: %d is greater than disp_index[%d]: %d\n",
		   temp_pindex, temp_disp_index[temp_pindex],
		   temp_pindex, disp_index[temp_pindex]);
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
	       index+1,fh->f_rank);
	for (i=0;i<fh->f_procs_per_group; i++){
	  for(j=0;j<disp_index[i];j++){
	    if (blocklen_per_process[i][j] > 0){
	      printf("%d sends blocklen[%d]: %d, disp[%d]: %ld to %d\n",
		     fh->f_procs_in_group[i],j,
		     blocklen_per_process[i][j],j,
		     displs_per_process[i][j],
		     fh->f_rank);
	      
	    }
	  }
	}
	printf("************Cycle: %d,  Aggregator: %d ***************\n", 
	       index+1,fh->f_rank);
	for (i=0; i<entries_per_aggregator;i++){
	  printf("%d: OFFSET: %lld   LENGTH: %ld, Mem-offset: %ld\n",
		   file_offsets_for_agg[sorted_file_offsets[i]].process_id,
		 file_offsets_for_agg[sorted_file_offsets[i]].offset,
		 file_offsets_for_agg[sorted_file_offsets[i]].length,
		 memory_displacements[sorted_file_offsets[i]]);
	}
	printf("%d : global_count : %ld, bytes_sent : %d\n",
	       fh->f_rank,global_count, bytes_sent);
#endif
#if TIME_BREAKDOWN
	  start_comm_time = MPI_Wtime();
#endif

	
	global_buf  = (char *) malloc (global_count);
	if (NULL == global_buf){
	    opal_output(1, "OUT OF MEMORY");
	    ret = OMPI_ERR_OUT_OF_RESOURCE;
	  goto exit;
	}

	recv_req_count = 0;
	for (i=0;i<fh->f_procs_per_group; i++){
	    
	  ompi_datatype_create_hindexed(disp_index[i],
					blocklen_per_process[i],
					displs_per_process[i],
					MPI_BYTE,
					&recvtype[i]);
	  ompi_datatype_commit(&recvtype[i]); 
	  
	  opal_datatype_type_size(&recvtype[i]->super, 
				  &datatype_size);
	  
	  if (datatype_size){
	    
	    recv_req = (MPI_Request *)realloc 
			((void *)recv_req, (recv_req_count + 1)*sizeof(MPI_Request));
	    
	    ret = MCA_PML_CALL(irecv(global_buf,
				     1,
				     recvtype[i],
				     fh->f_procs_in_group[i],
				     123,
				     fh->f_comm,
				     &recv_req[recv_req_count]));
	    recv_req_count++;
	    
	    if (OMPI_SUCCESS != ret){
	      goto exit;
	    }
	  }
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

	/* Gather the sendbuf from each process in appropritate locations in 
	 aggregators*/
	
	send_req = (MPI_Request *) malloc (sizeof(MPI_Request));
	if (NULL == send_req){
	  opal_output (1, "OUT OF MEMORY\n");
	  ret = OMPI_ERR_OUT_OF_RESOURCE;
	  goto exit;
	}

	
	if (bytes_sent){

	  ret = MCA_PML_CALL(isend(send_buf,
				   bytes_sent,
				   MPI_BYTE,
				   fh->f_procs_in_group[fh->f_aggregator_index],
				   123,
				   MCA_PML_BASE_SEND_STANDARD, 
				   fh->f_comm,
				   send_req));	

	    
	    if ( OMPI_SUCCESS != ret ){
		goto exit;
	    }

	    ret = ompi_request_wait(send_req, MPI_STATUS_IGNORE);
	    if (OMPI_SUCCESS != ret){
		goto exit;
	    }
	} 
	if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {       
	  ret = ompi_request_wait_all (recv_req_count,
				       recv_req,
				       MPI_STATUS_IGNORE);
	  
	  if (OMPI_SUCCESS != ret){
	    goto exit;
	  }
	}
#if DEBUG_ON
	if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank){
	  printf("************Cycle: %d,  Aggregator: %d ***************\n", 
		 index+1,fh->f_rank);
	  for (i=0 ; i<global_count/4 ; i++)
	    printf (" RECV %d \n",((int *)global_buf)[i]);
	}
#endif




    
    if (! (fh->f_flags & OMPIO_CONTIGUOUS_MEMORY)) {
	if (NULL != send_buf) {
	    free (send_buf);
	    send_buf = NULL;
	}
    }

#if TIME_BREAKDOWN
      end_comm_time = MPI_Wtime();
      comm_time += (end_comm_time - start_comm_time);
#endif


    /**********************************************************
     **************** DONE GATHERING OF DATA ******************
     *********************************************************/
    
        /**********************************************************
         ******* Create the io array, and pass it to fbtl *********
         *********************************************************/

	if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {

#if TIME_BREAKDOWN
	    start_write_time = MPI_Wtime();
#endif
	  
	  fh->f_io_array = (mca_io_ompio_io_array_t *) malloc 
	    (entries_per_aggregator * sizeof (mca_io_ompio_io_array_t));
	  if (NULL == fh->f_io_array) {
	    opal_output(1, "OUT OF MEMORY\n");
	    return OMPI_ERR_OUT_OF_RESOURCE;
	  }
	  
	  fh->f_num_of_io_entries = 0;
	  /*First entry for every aggregator*/
	  fh->f_io_array[fh->f_num_of_io_entries].offset = 
	    (IOVBASE_TYPE *)(intptr_t)file_offsets_for_agg[sorted_file_offsets[0]].offset;
	  fh->f_io_array[fh->f_num_of_io_entries].length = 
	    file_offsets_for_agg[sorted_file_offsets[0]].length;
	  fh->f_io_array[fh->f_num_of_io_entries].memory_address = 
	    global_buf+memory_displacements[sorted_file_offsets[0]];
	  fh->f_num_of_io_entries++;

	  for (i=1;i<entries_per_aggregator;i++){
	    /* If the enrties are contiguous merge them,
	       else make a new entry */
	    if (file_offsets_for_agg[sorted_file_offsets[i-1]].offset + 
		file_offsets_for_agg[sorted_file_offsets[i-1]].length ==
		file_offsets_for_agg[sorted_file_offsets[i]].offset){
	      fh->f_io_array[fh->f_num_of_io_entries - 1].length +=
		file_offsets_for_agg[sorted_file_offsets[i]].length;	
	    }
	    else {
	      fh->f_io_array[fh->f_num_of_io_entries].offset = 
		(IOVBASE_TYPE *)(intptr_t)file_offsets_for_agg[sorted_file_offsets[i]].offset;
	      fh->f_io_array[fh->f_num_of_io_entries].length = 
		file_offsets_for_agg[sorted_file_offsets[i]].length;
	      fh->f_io_array[fh->f_num_of_io_entries].memory_address = 
		global_buf+memory_displacements[sorted_file_offsets[i]];
	      fh->f_num_of_io_entries++;
	    }
	    
	  }
	 
#if DEBUG_ON
	  printf("*************************** %d\n", fh->f_num_of_io_entries);
	  for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
	    printf(" ADDRESS: %p  OFFSET: %ld   LENGTH: %ld\n",
		   fh->f_io_array[i].memory_address,
		   (OPAL_PTRDIFF_TYPE)fh->f_io_array[i].offset,
		   fh->f_io_array[i].length);
	  }
	  
#endif


	  if (fh->f_num_of_io_entries) {
	    if (OMPI_SUCCESS != fh->f_fbtl->fbtl_pwritev (fh, NULL)) {
	      opal_output (1, "WRITE FAILED\n");
	      ret = OMPI_ERROR;
	      goto exit;
	    }
	  }
#if TIME_BREAKDOWN
	  end_write_time = MPI_Wtime();
	  write_time += end_write_time - start_write_time;
#endif	  


	}
	
	
	if (NULL != send_req){
	  free(send_req);
	  send_req = NULL;
	}
	
        if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
	  fh->f_num_of_io_entries = 0;
	  if (NULL != fh->f_io_array) {
	    free (fh->f_io_array);
	    fh->f_io_array = NULL;
	  }
	  for (i =0; i< fh->f_procs_per_group; i++) 
	    ompi_datatype_destroy(recvtype+i);
	  if (NULL != recvtype){
	      free(recvtype);
	      recvtype=NULL;
	  }
	  if (NULL != recv_req){
	    free(recv_req);
	    recv_req = NULL;
	    
	  }
	  if (NULL != global_buf) {
	    free (global_buf);
	    global_buf = NULL;
	  }
	  
	}
	
    }

#if TIME_BREAKDOWN
      end_exch = MPI_Wtime();
      exch_write += end_exch - start_exch;
      nentry.time[0] = write_time;
      nentry.time[1] = comm_time;
      nentry.time[2] = exch_write;
      if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank)
	nentry.aggregator = 1;
      else
	nentry.aggregator = 0;
      nentry.nprocs_for_coll = mca_fcoll_dynamic_num_io_procs;
      if (!ompi_io_ompio_full_print_queue(WRITE_PRINT_QUEUE)){
	ompi_io_ompio_register_print_entry(WRITE_PRINT_QUEUE,
					   nentry);
      } 
#endif


 exit :
    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
      if (NULL != fh->f_io_array) {
	free (fh->f_io_array);
	fh->f_io_array = NULL;
      }
      if (NULL != disp_index){
	free(disp_index);
	disp_index = NULL;
      }
      if (NULL != recvtype){
	free(recvtype);
	recvtype=NULL;
      }
      if (NULL != recv_req){
	free(recv_req);
	recv_req = NULL;
      }
      if (NULL != global_buf) {
	free (global_buf);
	global_buf = NULL;
      }
      for(l=0;l<fh->f_procs_per_group;l++){
	if (NULL != blocklen_per_process[l]){
	  free(blocklen_per_process[l]);
	  blocklen_per_process[l] = NULL;
	}
	if (NULL != displs_per_process[l]){
	  free(displs_per_process[l]);
	  displs_per_process[l] = NULL;
	}
      }
      if (NULL != blocklen_per_process){
	free(blocklen_per_process);
	blocklen_per_process = NULL;
      }
      if (NULL != displs_per_process){
	free(displs_per_process);
	displs_per_process = NULL;
      }

    }
    
    if (NULL != sorted) {
        free (sorted);
        sorted = NULL;
    }
    if (NULL != global_iov_array) {
        free (global_iov_array);
	global_iov_array = NULL;
    }
    if (NULL != fview_count) {
        free (fview_count);
        fview_count = NULL;
    }
    if (NULL != decoded_iov) {
        free (decoded_iov);
        decoded_iov = NULL;
    }
    
    if (NULL != send_req){
      free(send_req);
      send_req = NULL;
    }

    return OMPI_SUCCESS;
}


static int local_heap_sort (local_io_array *io_array,
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


