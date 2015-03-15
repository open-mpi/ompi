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
 * Copyright (c) 2008-2014 University of Houston. All rights reserved.
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

#define DEBUG_ON 0
#define TIME_BREAKDOWN 0


typedef struct local_io_array {
  OMPI_MPI_OFFSET_TYPE offset;
  MPI_Aint             length;
  int                  process_id;
}local_io_array;



int read_local_heap_sort (local_io_array *io_array,
			  int num_entries,
			  int *sorted);

int read_find_next_index( int proc_index,
			  int c_index,
			  mca_io_ompio_file_t *fh, 
			  local_io_array *global_iov_array,
			  int global_iov_count,
			  int *sorted);

int read_get_process_id (int rank,
			 mca_io_ompio_file_t *fh);



int
mca_fcoll_static_file_read_all (mca_io_ompio_file_t *fh, 
				void *buf, 
				int count, 
				struct ompi_datatype_t *datatype, 
				ompi_status_public_t *status)
{

  int ret = OMPI_SUCCESS, iov_size=0, *bytes_remaining=NULL;
  int i, j, l,cycles=0, local_cycles=0, *current_index=NULL;
  int index, *disp_index=NULL, *bytes_per_process=NULL, current_position=0;
  int **blocklen_per_process=NULL, *iovec_count_per_process=NULL;
  int *displs=NULL, *sorted=NULL ,entries_per_aggregator=0;
  int *sorted_file_offsets=NULL, temp_index=0, position=0, *temp_disp_index=NULL;

  
  MPI_Aint **displs_per_process=NULL, global_iov_count=0, global_count=0;
  MPI_Aint *memory_displacements=NULL;
  int bytes_to_read_in_cycle=0;
  size_t max_data=0, bytes_per_cycle=0;
  uint32_t iov_count=0, iov_index=0;
  struct iovec *decoded_iov=NULL, *iov=NULL;
  local_io_array *local_iov_array=NULL, *global_iov_array=NULL;
  local_io_array *file_offsets_for_agg=NULL;

  char *global_buf=NULL, *receive_buf=NULL;
  
  int blocklen[3] = {1, 1, 1};
  int static_num_io_procs=1;
  OPAL_PTRDIFF_TYPE d[3], base;
  ompi_datatype_t *types[3];
  ompi_datatype_t *io_array_type=MPI_DATATYPE_NULL;
  ompi_datatype_t **sendtype = NULL;
  MPI_Request *send_req=NULL, *recv_req=NULL;
  /*  MPI_Request *grecv_req=NULL, *gsend_req=NULL; */

#if TIME_BREAKDOWN
  double read_time = 0.0, start_read_time = 0.0, end_read_time = 0.0;
  double rcomm_time = 0.0, start_rcomm_time = 0.0, end_rcomm_time = 0.0;
  double read_exch = 0.0, start_rexch = 0.0, end_rexch = 0.0;
  print_entry nentry;
#endif
#if DEBUG_ON 
    MPI_Aint gc_in;
#endif

//  if (opal_datatype_is_contiguous_memory_layout(&datatype->super,1)) {
//   fh->f_flags |= OMPIO_CONTIGUOUS_MEMORY;
//  }

  
  /* In case the data is not contigous in memory, decode it into an iovec */
  if (! (fh->f_flags & OMPIO_CONTIGUOUS_MEMORY)) {
      fh->f_decode_datatype ( (struct mca_io_ompio_file_t *)fh,
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

  if ( MPI_STATUS_IGNORE != status ) {
      status->_ucount = max_data;
  }
  

  fh->f_get_num_aggregators ( &static_num_io_procs );
  fh->f_set_aggregator_props ((struct mca_io_ompio_file_t *) fh, 
			      static_num_io_procs,
			      max_data);
  
  /*  printf("max_data %ld\n", max_data);  */
  ret = fh->f_generate_current_file_view((struct mca_io_ompio_file_t *)fh,
					 max_data,
					 &iov,
					 &iov_size);
  if (ret != OMPI_SUCCESS){
      goto exit;
  }

  if ( iov_size > 0 ) {
      local_iov_array = (local_io_array *)malloc (iov_size * sizeof(local_io_array));
      if ( NULL == local_iov_array){
	  ret = OMPI_ERR_OUT_OF_RESOURCE;
	  goto exit;
      }
  

      for (j=0; j < iov_size; j++){
	  local_iov_array[j].offset = (OMPI_MPI_OFFSET_TYPE)(intptr_t)
	      iov[j].iov_base;
	  local_iov_array[j].length = (size_t)iov[j].iov_len;
	  local_iov_array[j].process_id = fh->f_rank;
    
      }
  }
  else {
      /* Allocate at least one element to correctly create the derived
	 data type */
      local_iov_array = (local_io_array *)malloc (sizeof(local_io_array));
      if ( NULL == local_iov_array){
	  ret = OMPI_ERR_OUT_OF_RESOURCE;
	  goto exit;
      }
  

      local_iov_array[0].offset = (OMPI_MPI_OFFSET_TYPE)(intptr_t) 0;
      local_iov_array[0].length = (size_t) 0;
      local_iov_array[0].process_id = fh->f_rank;
  }

  d[0] = (OPAL_PTRDIFF_TYPE)&local_iov_array[0];
  d[1] = (OPAL_PTRDIFF_TYPE)&local_iov_array[0].length;
  d[2] = (OPAL_PTRDIFF_TYPE)&local_iov_array[0].process_id;
  base = d[0];
  for (i=0 ; i<3 ; i++) {
      d[i] -= base;
  }

  /* io_array datatype  for using in communication*/
  types[0] = &ompi_mpi_long.dt;
  types[1] = &ompi_mpi_long.dt;
  types[2] = &ompi_mpi_int.dt;
  
  ompi_datatype_create_struct (3,
			       blocklen,
			       d,
			       types,
			       &io_array_type);
  ompi_datatype_commit (&io_array_type);
  /* #########################################################*/
  
  
  fh->f_get_bytes_per_agg ( (int*) &bytes_per_cycle);  
  local_cycles = ceil((double)max_data/bytes_per_cycle);
  ret = fh->f_comm->c_coll.coll_allreduce (&local_cycles, 
					   &cycles,
					   1,
					   MPI_INT,
					   MPI_MAX,
					   fh->f_comm,
					   fh->f_comm->c_coll.coll_allreduce_module);

  if (OMPI_SUCCESS != ret){
    goto exit;
  }

  if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
    disp_index = (int *) malloc (fh->f_procs_per_group * sizeof(int));
    if (NULL == disp_index) {
      opal_output (1, "OUT OF MEMORY\n");
      ret = OMPI_ERR_OUT_OF_RESOURCE;
      goto exit;
    }

    bytes_per_process = (int *) malloc (fh->f_procs_per_group * sizeof(int ));
    if (NULL == bytes_per_process){
      opal_output (1, "OUT OF MEMORY\n");
      ret = OMPI_ERR_OUT_OF_RESOURCE;
      goto exit;
    }

    bytes_remaining = (int *) malloc (fh->f_procs_per_group * sizeof(int));
    if (NULL == bytes_remaining){
      opal_output (1, "OUT OF MEMORY\n");
      ret = OMPI_ERR_OUT_OF_RESOURCE;
      goto exit;
    }

    current_index = (int *) malloc (fh->f_procs_per_group * sizeof(int));
    if (NULL == current_index){
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
      current_index[i] = 0;
      bytes_remaining[i] = 0;
      blocklen_per_process[i] = NULL;
      displs_per_process[i] = NULL;
    }
  }


  iovec_count_per_process = (int *) malloc (fh->f_procs_per_group * sizeof(int));
  if (NULL == iovec_count_per_process){
    opal_output (1, "OUT OF MEMORY\n");
    ret = OMPI_ERR_OUT_OF_RESOURCE;
    goto exit;
  }

  displs = (int *) malloc (fh->f_procs_per_group * sizeof(int));
  if (NULL == displs){
    opal_output (1, "OUT OF MEMORY\n");
    ret = OMPI_ERR_OUT_OF_RESOURCE;
    goto exit;
  }


  ret = fh->f_allgather_array (&iov_size,
			       1,
			       MPI_INT,
			       iovec_count_per_process,
			       1,
			       MPI_INT,
			       fh->f_aggregator_index,
			       fh->f_procs_in_group,
			       fh->f_procs_per_group,
			       fh->f_comm);
  
  if( OMPI_SUCCESS != ret){
    goto exit;
  }
  
  if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
    displs[0] = 0;
    global_iov_count = iovec_count_per_process[0];
    for (i=1 ; i<fh->f_procs_per_group ; i++) {
      global_iov_count += iovec_count_per_process[i];
      displs[i] = displs[i-1] + iovec_count_per_process[i-1];
    }
  }

  
  if ( (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) && 
       (global_iov_count >  0 )) {
    global_iov_array = (local_io_array *) malloc (global_iov_count *
						  sizeof(local_io_array));
    if (NULL == global_iov_array){
      opal_output (1, "OUT OF MEMORY\n");
      ret = OMPI_ERR_OUT_OF_RESOURCE;
      goto exit;
    }
  }
  
  ret = fh->f_gatherv_array (local_iov_array,
			     iov_size,
			     io_array_type,
			     global_iov_array,
			     iovec_count_per_process,
			     displs,
			     io_array_type,
			     fh->f_aggregator_index,
			     fh->f_procs_in_group,
			     fh->f_procs_per_group,
			     fh->f_comm);
  
  if (OMPI_SUCCESS != ret){
      fprintf(stderr,"global_iov_array gather error!\n");
      goto exit;
  }

  
  if (NULL != local_iov_array){
    free(local_iov_array);
    local_iov_array = NULL;
  }
  
  if ( ( fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) && 
       ( global_iov_count > 0 )) {
    sorted = (int *)malloc (global_iov_count * sizeof(int));
    if (NULL == sorted) {
      opal_output (1, "OUT OF MEMORY\n");
      ret = OMPI_ERR_OUT_OF_RESOURCE;
      goto exit;
    }
    read_local_heap_sort (global_iov_array, global_iov_count, sorted);
  }
  
#if DEBUG_ON

  if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
    for (gc_in=0; gc_in<global_iov_count; gc_in++){
      printf("%d: Offset[%ld]: %lld, Length[%ld]: %ld\n",
	     global_iov_array[sorted[gc_in]].process_id,
	     gc_in, global_iov_array[sorted[gc_in]].offset,
	     gc_in, global_iov_array[sorted[gc_in]].length);
    }
  }
#endif
  
#if TIME_BREAKDOWN
    start_rexch = MPI_Wtime();
#endif

  for (index = 0; index < cycles; index++){


    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {

      if (NULL == sendtype){
	sendtype = (ompi_datatype_t **) 
	  malloc (fh->f_procs_per_group  * sizeof(ompi_datatype_t *));
	if (NULL == sendtype) {
	  opal_output (1, "OUT OF MEMORY\n");
	  ret = OMPI_ERR_OUT_OF_RESOURCE;
	  goto exit;
	}
      }
	 
      if (NULL == bytes_per_process){
	bytes_per_process = (int *) malloc (fh->f_procs_per_group * sizeof(int));
	if (NULL == bytes_per_process){
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
    }
    if (local_cycles > index) {
      if ((index == local_cycles-1) && (max_data % bytes_per_cycle)) {
	bytes_to_read_in_cycle = max_data % bytes_per_cycle;
      }
      else if (max_data <= bytes_per_cycle) {
	bytes_to_read_in_cycle = max_data;
      }
      else {
	bytes_to_read_in_cycle = bytes_per_cycle;
      }
    }
    else {
      bytes_to_read_in_cycle = 0;
    }
    fh->f_gather_array (&bytes_to_read_in_cycle,
			1,
			MPI_INT,
			bytes_per_process,
			1,
			MPI_INT,
			fh->f_aggregator_index,
			fh->f_procs_in_group,
			fh->f_procs_per_group,
			fh->f_comm);
    
    if (fh->f_flags & OMPIO_CONTIGUOUS_MEMORY) {
      receive_buf = &((char*)buf)[position];
    }
    else if (bytes_to_read_in_cycle) {
      receive_buf = (char *) malloc (bytes_to_read_in_cycle * sizeof(char));
      if ( NULL == receive_buf){
	opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
      }
    }    
    
    recv_req = (MPI_Request *) malloc (sizeof (MPI_Request));
    if (NULL == recv_req){
      opal_output (1, "OUT OF MEMORY\n");
      ret = OMPI_ERR_OUT_OF_RESOURCE;
      goto exit;
    }

#if TIME_BREAKDOWN
      start_rcomm_time = MPI_Wtime();
#endif
    
    ret = MCA_PML_CALL(irecv(receive_buf,
			     bytes_to_read_in_cycle,
			     MPI_BYTE,
			     fh->f_procs_in_group[fh->f_aggregator_index],
			     123,
			     fh->f_comm,
			     recv_req));
    if (OMPI_SUCCESS != ret){
      goto exit;
    }

#if TIME_BREAKDOWN
      end_rcomm_time = MPI_Wtime();
      rcomm_time  += end_rcomm_time - start_rcomm_time;
#endif
    
    
    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
      for (i=0;i<fh->f_procs_per_group; i++){
	while (bytes_per_process[i] > 0){
	  /*printf("%d: bytes_per_process[%d]: %d, bytes_remaining[%d]: %d\n",
	    index, i, bytes_per_process[i], i, bytes_remaining[i]);*/
	  if (read_get_process_id(global_iov_array[sorted[current_index[i]]].process_id,
				  fh) == i){ /* current id owns this entry!*/
	    if (bytes_remaining[i]){ /*Remaining bytes in the current entry of 
				       the global offset array*/
	      if (bytes_remaining[i] <= bytes_per_process[i]){

		blocklen_per_process[i][disp_index[i] - 1] = bytes_remaining[i];
		displs_per_process[i][disp_index[i] - 1] = 
		  global_iov_array[sorted[current_index[i]]].offset + 
		  (global_iov_array[sorted[current_index[i]]].length
		   - bytes_remaining[i]);
		blocklen_per_process[i] = (int *) realloc
		  ((void *)blocklen_per_process[i], (disp_index[i]+1)*sizeof(int));
		displs_per_process[i] = (MPI_Aint *)realloc
		  ((void *)displs_per_process[i], (disp_index[i]+1)*sizeof(MPI_Aint));
		bytes_per_process[i] -= bytes_remaining[i];
		blocklen_per_process[i][disp_index[i]] = 0;
		displs_per_process[i][disp_index[i]] = 0;
		disp_index[i] += 1;
		bytes_remaining[i] = 0;
		/* This entry has been used up, we need to move to the 
		 next entry of this process and make current_index point there*/
		current_index[i]  = read_find_next_index(i,
						    current_index[i],
						    fh,
						    global_iov_array,
						    global_iov_count,
						    sorted);
		if (current_index[i] == -1){
		  break;
		}
		continue;
	      }
	      else{
		blocklen_per_process[i][disp_index[i] - 1] = bytes_per_process[i];
		displs_per_process[i][disp_index[i] - 1] = 
		  global_iov_array[sorted[current_index[i]]].offset +
		  (global_iov_array[sorted[current_index[i]]].length
		   - bytes_remaining[i]);
		bytes_remaining[i] -= bytes_per_process[i];
		bytes_per_process[i] = 0;
		break;
	      }
	    }
	    else{
	      if (bytes_per_process[i] < 
		  global_iov_array[sorted[current_index[i]]].length){
		blocklen_per_process[i][disp_index[i] - 1] =
		  bytes_per_process[i];
		displs_per_process[i][disp_index[i] - 1] =
		  global_iov_array[sorted[current_index[i]]].offset;
		bytes_remaining[i] = 
		  global_iov_array[sorted[current_index[i]]].length - 
		  bytes_per_process[i];
		bytes_per_process[i] = 0;
		break;
	      }
	      else {
		blocklen_per_process[i][disp_index[i] - 1] =
		  global_iov_array[sorted[current_index[i]]].length;
		displs_per_process[i][disp_index[i] - 1] =
		  global_iov_array[sorted[current_index[i]]].offset;
		blocklen_per_process[i] = 
		  (int *) realloc ((void *)blocklen_per_process[i], (disp_index[i]+1)*sizeof(int));
		displs_per_process[i] = (MPI_Aint *)realloc
		  ((void *)displs_per_process[i], (disp_index[i]+1)*sizeof(MPI_Aint));
		blocklen_per_process[i][disp_index[i]] = 0;
		displs_per_process[i][disp_index[i]] = 0;
		disp_index[i] += 1;
		bytes_per_process[i] -=  
		  global_iov_array[sorted[current_index[i]]].length;
		current_index[i] = read_find_next_index(i,
							current_index[i],
							fh,
							global_iov_array,
							global_iov_count,
							sorted);
		if (current_index[i] == -1){
		  break;
		}
	      }
	    }
	  }
	  else{   
	    current_index[i] = read_find_next_index(i,
						    current_index[i],
						    fh,
						    global_iov_array,
						    global_iov_count,
						    sorted);
	    if (current_index[i] == -1){
	      bytes_per_process = 0; /* no more entries left 
					to service this request*/
	      continue;
	    }
	  }
	}
      }
      
      entries_per_aggregator=0;
      for (i=0;i<fh->f_procs_per_group;i++){
	for (j=0;j<disp_index[i];j++){
	    if (blocklen_per_process[i][j] > 0){
	      entries_per_aggregator++;
#if DEBUG_ON
	      printf("%d sends blocklen[%d]: %d, disp[%d]: %ld to %d\n",
		     fh->f_procs_in_group[i],j,
		     blocklen_per_process[i][j],j,
		     displs_per_process[i][j],
		     fh->f_rank);
	      
#endif
	    }
	}
      }      
      
      if (entries_per_aggregator > 0){
	file_offsets_for_agg = (local_io_array *)
	  malloc(entries_per_aggregator*sizeof(local_io_array));
	if (NULL == file_offsets_for_agg) {
	  opal_output (1, "OUT OF MEMORY\n");
	  ret = OMPI_ERR_OUT_OF_RESOURCE;
	  goto exit;
	}
	sorted_file_offsets = (int *) 
	  malloc (entries_per_aggregator * sizeof(int));
	if (NULL == sorted_file_offsets){
	  opal_output (1, "OUT OF MEMORY\n");
	  ret =  OMPI_ERR_OUT_OF_RESOURCE;
	  goto exit;
	}
	temp_index=0;
	global_count = 0;
	for (i=0;i<fh->f_procs_per_group; i++){
	  for(j=0;j<disp_index[i]; j++){
	    if (blocklen_per_process[i][j] > 0){
	      file_offsets_for_agg[temp_index].length =
		blocklen_per_process[i][j];
	      global_count += blocklen_per_process[i][j];
	      file_offsets_for_agg[temp_index].process_id = i;
	      file_offsets_for_agg[temp_index].offset = 
		displs_per_process[i][j];
	      temp_index++;
	    }
	  }
	}
      }
      else{
	continue;
      }
      read_local_heap_sort (file_offsets_for_agg,
			    entries_per_aggregator,
			    sorted_file_offsets); 
      memory_displacements = (MPI_Aint *) malloc 
	(entries_per_aggregator * sizeof(MPI_Aint));
      memory_displacements[sorted_file_offsets[0]] = 0;
      for (i=1; i<entries_per_aggregator; i++){
	memory_displacements[sorted_file_offsets[i]] = 
	  memory_displacements[sorted_file_offsets[i-1]] + 
	  file_offsets_for_agg[sorted_file_offsets[i-1]].length;
      }

      global_buf = (char *) malloc (global_count * sizeof(char));
      if (NULL == global_buf){
	opal_output(1, "OUT OF MEMORY\n");
	ret = OMPI_ERR_OUT_OF_RESOURCE;
	goto exit;
      }
#if DEBUG_ON
      printf("************Cycle: %d,  Aggregator: %d ***************\n", 
	     index+1,fh->f_rank);
      for (i=0; i<entries_per_aggregator;i++){
	printf("%d: OFFSET: %lld   LENGTH: %ld, Mem-offset: %ld, disp_index :%d\n",
	       file_offsets_for_agg[sorted_file_offsets[i]].process_id,
	       file_offsets_for_agg[sorted_file_offsets[i]].offset,
	       file_offsets_for_agg[sorted_file_offsets[i]].length,
	       memory_displacements[sorted_file_offsets[i]],
	       disp_index[i]);
      }
#endif
      
      fh->f_io_array = (mca_io_ompio_io_array_t *) malloc 
	(entries_per_aggregator * sizeof (mca_io_ompio_io_array_t));
      if (NULL == fh->f_io_array) {
	opal_output(1, "OUT OF MEMORY\n");
	ret =  OMPI_ERR_OUT_OF_RESOURCE;
	goto exit;
      }
      


      fh->f_num_of_io_entries = 0;
      fh->f_io_array[fh->f_num_of_io_entries].offset = 
	(IOVBASE_TYPE *)(intptr_t)file_offsets_for_agg[sorted_file_offsets[0]].offset;
      fh->f_io_array[fh->f_num_of_io_entries].length = 
	file_offsets_for_agg[sorted_file_offsets[0]].length;
      fh->f_io_array[fh->f_num_of_io_entries].memory_address = 
	global_buf+memory_displacements[sorted_file_offsets[0]];
      fh->f_num_of_io_entries++;
      for (i=1;i<entries_per_aggregator;i++){
	if (file_offsets_for_agg[sorted_file_offsets[i-1]].offset + 
	    file_offsets_for_agg[sorted_file_offsets[i-1]].length ==
	    file_offsets_for_agg[sorted_file_offsets[i]].offset){
	  fh->f_io_array[fh->f_num_of_io_entries - 1].length +=
	    file_offsets_for_agg[sorted_file_offsets[i]].length;	
	}
	else{
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
#if TIME_BREAKDOWN      
	start_read_time = MPI_Wtime();
#endif

      if (fh->f_num_of_io_entries) {
	if ( 0 > fh->f_fbtl->fbtl_preadv (fh)) {
	  opal_output (1, "READ FAILED\n");
	  ret = OMPI_ERROR;
	  goto exit;
	}
      }

#if TIME_BREAKDOWN
	end_read_time = MPI_Wtime();
	read_time += end_read_time - start_read_time;
#endif


#if DEBUG_ON
      printf("************Cycle: %d,  Aggregator: %d ***************\n", 
	     index+1,fh->f_rank);
      if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank){
	for (i=0 ; i<global_count/4 ; i++)
	  printf (" READ %d \n",((int *)global_buf)[i]);
      }
#endif
      
      temp_disp_index = (int *)calloc (1, fh->f_procs_per_group * sizeof (int));
      if (NULL == temp_disp_index) {
	opal_output (1, "OUT OF MEMORY\n");
	ret = OMPI_ERR_OUT_OF_RESOURCE;
	goto exit;
      }

      for (i=0; i<entries_per_aggregator; i++){
	temp_index = 
	   file_offsets_for_agg[sorted_file_offsets[i]].process_id;
	displs_per_process[temp_index][temp_disp_index[temp_index]] =
	  memory_displacements[sorted_file_offsets[i]];
	if (temp_disp_index[temp_index] < disp_index[temp_index]){
	  temp_disp_index[temp_index] += 1;
	}
	else{
	  printf("temp_disp_index[%d]: %d is greater than disp_index[%d]: %d\n",
		 temp_index, temp_disp_index[temp_index],
		 temp_index, disp_index[temp_index]);
	}
      }
      if (NULL != temp_disp_index){
	free(temp_disp_index);
	temp_disp_index = NULL;
      }
      
      send_req = (MPI_Request *)
	malloc (fh->f_procs_per_group * sizeof(MPI_Request));
      if (NULL == send_req){
	opal_output ( 1, "OUT OF MEMORY\n");
	ret = OMPI_ERR_OUT_OF_RESOURCE;
	goto exit;
      }
#if TIME_BREAKDOWN
	start_rcomm_time = MPI_Wtime();
#endif

      for (i=0;i<fh->f_procs_per_group; i++){
	ompi_datatype_create_hindexed(disp_index[i],
				      blocklen_per_process[i],
				      displs_per_process[i],
				      MPI_BYTE,
				      &sendtype[i]);
	ompi_datatype_commit(&sendtype[i]); 
	ret = MCA_PML_CALL (isend(global_buf,
				  1,
				  sendtype[i],
				  fh->f_procs_in_group[i],
				  123,
				  MCA_PML_BASE_SEND_STANDARD, 
				  fh->f_comm,
				  &send_req[i]));
	if(OMPI_SUCCESS != ret){
	  goto exit;
	}
      }
      
      ret = ompi_request_wait_all (fh->f_procs_per_group,
				   send_req,
				   MPI_STATUS_IGNORE);
      if (OMPI_SUCCESS != ret){
	goto exit;
      }
    }
    
    ret = ompi_request_wait (recv_req, MPI_STATUS_IGNORE);
    if (OMPI_SUCCESS != ret){
      goto exit;
    }

#if TIME_BREAKDOWN
      end_rcomm_time = MPI_Wtime();
      rcomm_time += end_rcomm_time - start_rcomm_time;
#endif

    position += bytes_to_read_in_cycle;
    
    if (!(fh->f_flags & OMPIO_CONTIGUOUS_MEMORY)) {
      OPAL_PTRDIFF_TYPE mem_address;
      size_t remaining = 0;
      size_t temp_position = 0;
      
      remaining = bytes_to_read_in_cycle;
      
      while (remaining && (iov_count > iov_index)){
	mem_address = (OPAL_PTRDIFF_TYPE)
	  (decoded_iov[iov_index].iov_base) + current_position;
	
	if (remaining >= 
	    (decoded_iov[iov_index].iov_len - current_position)) {
	  memcpy ((IOVBASE_TYPE *) mem_address,
		  receive_buf+temp_position,
		  decoded_iov[iov_index].iov_len - current_position);
	  remaining = remaining - 
	    (decoded_iov[iov_index].iov_len - current_position);
	  temp_position = temp_position +
	    (decoded_iov[iov_index].iov_len - current_position);
	  iov_index = iov_index + 1;
	  current_position = 0;
	}
	else{
	  memcpy ((IOVBASE_TYPE *) mem_address,
		  receive_buf+temp_position,
		  remaining);
	  current_position = current_position + remaining;
	  remaining = 0;
	}
      }
      if (NULL != receive_buf) {
	free (receive_buf);
	receive_buf = NULL;
      }
    }

    if (NULL != recv_req){
      free(recv_req);
      recv_req = NULL;
    }

    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {
      fh->f_num_of_io_entries = 0;
      if (NULL != fh->f_io_array) {
	free (fh->f_io_array);
	fh->f_io_array = NULL;
      }
      for (i = 0; i < fh->f_procs_per_group; i++)
	ompi_datatype_destroy(sendtype+i);
      if (NULL != sendtype){
	free(sendtype);
	sendtype=NULL;
      }
      if (NULL != send_req){
	free(send_req);
	send_req = NULL;
      }
      if (NULL != global_buf) {
	free (global_buf);
	global_buf = NULL;
      }
     

      if (NULL != sorted_file_offsets){
	free(sorted_file_offsets);
	sorted_file_offsets = NULL;
      }
      if (NULL != file_offsets_for_agg){
	free(file_offsets_for_agg);
	file_offsets_for_agg = NULL;
      }
      if (NULL != bytes_per_process){
	free(bytes_per_process);
	bytes_per_process =NULL;
      }
      if (NULL != memory_displacements){
	free(memory_displacements);
	memory_displacements= NULL;
      }
    }
  }
#if TIME_BREAKDOWN
    end_rexch = MPI_Wtime();
    read_exch += end_rexch - start_rexch;
    nentry.time[0] = read_time;
    nentry.time[1] = rcomm_time;
    nentry.time[2] = read_exch;
    if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank)
      nentry.aggregator = 1;
    else
      nentry.aggregator = 0;
    nentry.nprocs_for_coll = static_num_io_procs;
    if (!fh->f_full_print_queue(READ_PRINT_QUEUE)){
      fh->f_register_print_entry(READ_PRINT_QUEUE,
				 nentry);
    } 
#endif
      
 exit:
  if (NULL != decoded_iov){
    free(decoded_iov);
    decoded_iov = NULL;
  }
  
  if (NULL != displs){
    free(displs);
    displs = NULL;
  }

  if (NULL != iovec_count_per_process){
    free(iovec_count_per_process);
    iovec_count_per_process=NULL;
  }

  if (NULL != local_iov_array){
    free(local_iov_array);
    local_iov_array=NULL;
  }

  if (NULL != global_iov_array){
    free(global_iov_array);
    global_iov_array=NULL;
  }

  if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank) {  
    
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
  }

  if (NULL != bytes_per_process){
    free(bytes_per_process);
    bytes_per_process =NULL;
  }

  if (NULL != disp_index){
    free(disp_index);
    disp_index =NULL;
  }

  if (NULL != displs_per_process){
    free(displs_per_process);
    displs_per_process = NULL;
  }

  if(NULL != bytes_remaining){
    free(bytes_remaining);
    bytes_remaining = NULL;
  }

  if(NULL != current_index){
    free(current_index);
    current_index = NULL;
  }

  if (NULL != blocklen_per_process){
    free(blocklen_per_process);
    blocklen_per_process =NULL;
  }

  if (NULL != bytes_remaining){
    free(bytes_remaining);
    bytes_remaining =NULL;
  }

  if (NULL != memory_displacements){
    free(memory_displacements);
    memory_displacements= NULL;
  }

  if (NULL != file_offsets_for_agg){
    free(file_offsets_for_agg);
    file_offsets_for_agg = NULL;
  }

  if (NULL != sorted_file_offsets){
    free(sorted_file_offsets);
    sorted_file_offsets = NULL;
  }

  if (NULL != sendtype){
    free(sendtype);
    sendtype=NULL;
  }

  if (NULL != receive_buf){
    free(receive_buf);
    receive_buf=NULL;
  }
  return ret;
  
}


int read_local_heap_sort (local_io_array *io_array,
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
  
  if ( 0 == num_entries ) {
      return OMPI_SUCCESS;
  }

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


int read_find_next_index( int proc_index,
			  int c_index,
			  mca_io_ompio_file_t *fh, 
			  local_io_array *global_iov_array,
			  int global_iov_count,
			  int *sorted){
  int i;
  
  for(i=c_index+1; i<global_iov_count;i++){
    if (read_get_process_id(global_iov_array[sorted[i]].process_id,
		       fh) == proc_index)
      return i;
  }
  return -1;
}


int read_get_process_id (int rank,
			 mca_io_ompio_file_t *fh){
  int i;
  for (i=0; i<=fh->f_procs_per_group; i++){
    if (fh->f_procs_in_group[i] == rank){
      return i;
    }
  }
  return -1;
}



