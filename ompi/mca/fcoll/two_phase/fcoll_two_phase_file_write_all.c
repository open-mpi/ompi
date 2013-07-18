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
#include "fcoll_two_phase.h"

#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fcoll/fcoll.h"
#include "ompi/mca/io/ompio/io_ompio.h"
#include "ompi/mca/io/io.h"
#include "opal/mca/base/base.h"
#include "math.h"
#include "ompi/mca/pml/pml.h"
#include <unistd.h>

#define DEBUG_ON 0
#define TIME_BREAKDOWN 0

/* Two Phase implementation from ROMIO ported to OMPIO infrastructure 
   This is exactly similar to ROMIO's two_phase */



/* Datastructure to support specifying the flat-list. */
typedef struct flat_list_node {  
    MPI_Datatype type;
    int count;                
    OMPI_MPI_OFFSET_TYPE *blocklens;
    OMPI_MPI_OFFSET_TYPE *indices;  
    struct flat_list_node *next;  
} Flatlist_node;



/* local function declarations  */
static int two_phase_exch_and_write(mca_io_ompio_file_t *fh,
				    void *buf,
				    MPI_Datatype datatype,
				    mca_io_ompio_access_array_t *others_req,
				    struct iovec *offset_len,
				    int contig_access_count,
				    OMPI_MPI_OFFSET_TYPE min_st_offset,
				    OMPI_MPI_OFFSET_TYPE fd_size,
				    OMPI_MPI_OFFSET_TYPE *fd_start,
				    OMPI_MPI_OFFSET_TYPE *fd_end,
				    Flatlist_node *flat_buf,
				    size_t *buf_idx, int striping_unit,
				    int *aggregator_list);



static int  two_phase_exchage_data(mca_io_ompio_file_t *fh,
				   void *buf,
				   char *write_buf,
				   struct iovec *offset_length,
				   int *send_size, int *start_pos,
				   int *recv_size,
				   OMPI_MPI_OFFSET_TYPE off,
				   OMPI_MPI_OFFSET_TYPE size, int *count,
				   int *partial_recv, int *sent_to_proc,
				   int contig_access_count,
				   OMPI_MPI_OFFSET_TYPE min_st_offset,
				   OMPI_MPI_OFFSET_TYPE fd_size,
				   OMPI_MPI_OFFSET_TYPE *fd_start,
				   OMPI_MPI_OFFSET_TYPE *fd_end,
				   Flatlist_node *flat_buf,
				   mca_io_ompio_access_array_t *others_req,
				   int *send_buf_idx, int *curr_to_proc,
				   int *done_to_proc, int iter,
				   size_t *buf_idx, MPI_Aint buftype_extent,
				   int striping_unit, int *aggregator_list,
				   int *hole);
				   

static int two_phase_fill_send_buffer(mca_io_ompio_file_t *fh,
				      void *buf,
				      Flatlist_node *flat_buf,
				      char **send_buf,
				      struct iovec *offset_length,
				      int *send_size,
				      MPI_Request *send_req,
				      int *sent_to_proc,
				      int contig_access_count, 
				      OMPI_MPI_OFFSET_TYPE min_st_offset,
				      OMPI_MPI_OFFSET_TYPE fd_size,
				      OMPI_MPI_OFFSET_TYPE *fd_start,
				      OMPI_MPI_OFFSET_TYPE *fd_end,
				      int *send_buf_idx,
				      int *curr_to_proc, 
				      int *done_to_proc,
				      int iter, MPI_Aint buftype_extent,
				      int striping_unit, int *aggregator_list);
#if TIME_BREAKDOWN
static int is_aggregator(int rank,
			 int nprocs_for_coll,
			 int *aggregator_list);
#endif

void two_phase_heap_merge(mca_io_ompio_access_array_t *others_req,
			  int *count,
			  OMPI_MPI_OFFSET_TYPE *srt_off,
			  int *srt_len,
			  int *start_pos,
			  int myrank,
			  int nprocs,
			  int nprocs_recv,
			  int total_elements);


/* local function declarations  ends here!*/


#if TIME_BREAKDOWN
double write_time = 0.0, start_write_time = 0.0, end_write_time = 0.0;
double comm_time = 0.0, start_comm_time = 0.0, end_comm_time = 0.0;
double exch_write = 0.0, start_exch = 0.0, end_exch = 0.0;
#endif

int
mca_fcoll_two_phase_file_write_all (mca_io_ompio_file_t *fh,
                                    void *buf,
                                    int count,
                                    struct ompi_datatype_t *datatype,
                                    ompi_status_public_t *status)
{

    

  int i, j,interleave_count=0, striping_unit=0;
  uint32_t iov_count=0,ti;
  struct iovec *decoded_iov=NULL, *temp_iov=NULL;
  size_t max_data = 0, total_bytes = 0; 
  int domain_size=0, *count_my_req_per_proc=NULL, count_my_req_procs;
  int count_other_req_procs,  ret=OMPI_SUCCESS;
  size_t *buf_indices=NULL;
  int local_count = 0, local_size=0,*aggregator_list = NULL;
  struct iovec *iov = NULL;
  
  OMPI_MPI_OFFSET_TYPE start_offset, end_offset, fd_size;
  OMPI_MPI_OFFSET_TYPE *start_offsets=NULL, *end_offsets=NULL;
  OMPI_MPI_OFFSET_TYPE *fd_start=NULL, *fd_end=NULL, min_st_offset;
  Flatlist_node *flat_buf=NULL;
  mca_io_ompio_access_array_t *my_req=NULL, *others_req=NULL;
  MPI_Aint send_buf_addr;
#if TIME_BREAKDOWN
  print_entry nentry;
#endif
  
  
  if (opal_datatype_is_contiguous_memory_layout(&datatype->super,1)) {
    fh->f_flags = fh->f_flags |  OMPIO_CONTIGUOUS_MEMORY;
  }
  
  
  if (! (fh->f_flags & OMPIO_CONTIGUOUS_MEMORY)) {
    
    ret =   ompi_io_ompio_decode_datatype (fh,
					   datatype,
					   count,
					   buf,
					   &max_data,
					   &temp_iov,
					   &iov_count);
    if (OMPI_SUCCESS != ret ){
      goto exit;
    }

    send_buf_addr = (OPAL_PTRDIFF_TYPE)buf;
    decoded_iov = (struct iovec *)malloc 
      (iov_count * sizeof(struct iovec));

    for (ti = 0; ti < iov_count; ti ++){
      decoded_iov[ti].iov_base = (IOVBASE_TYPE *)(
	(OPAL_PTRDIFF_TYPE)temp_iov[ti].iov_base - 
	send_buf_addr);
      decoded_iov[ti].iov_len = 
	temp_iov[ti].iov_len ;
      #if DEBUG_ON
      printf("d_offset[%d]: %ld, d_len[%d]: %ld\n",
	     ti, (OPAL_PTRDIFF_TYPE)decoded_iov[ti].iov_base,
	     ti, decoded_iov[ti].iov_len);
      #endif
    }
    
  }
  else{
    max_data = count * datatype->super.size;
  }
    
  if ( MPI_STATUS_IGNORE != status ) {
    status->_ucount = max_data;
  }

  
  if(-1 == mca_fcoll_two_phase_num_io_procs){
    ret = ompi_io_ompio_set_aggregator_props (fh, 
					      mca_fcoll_two_phase_num_io_procs,
					      max_data);
    if ( OMPI_SUCCESS != ret){
      return  ret;
    }
    
    mca_fcoll_two_phase_num_io_procs = 
      ceil((float)fh->f_size/fh->f_procs_per_group);
    
  }
  
  if (mca_fcoll_two_phase_num_io_procs > fh->f_size){
    mca_fcoll_two_phase_num_io_procs = fh->f_size;
  }
  
#if DEBUG_ON
  printf("Number of aggregators : %ld\n", mca_fcoll_two_phase_num_io_procs);
#endif

  aggregator_list = (int *) malloc (mca_fcoll_two_phase_num_io_procs *
				    sizeof(int));
  
  if ( NULL == aggregator_list ) {
    return OMPI_ERR_OUT_OF_RESOURCE;
  }
  
  for (i =0; i< mca_fcoll_two_phase_num_io_procs; i++){
    aggregator_list[i] = i;
  }
  
  
  ret = ompi_io_ompio_generate_current_file_view (fh, 
						  max_data, 
						  &iov, 
						  &local_count);
  
  
  if ( OMPI_SUCCESS != ret ){
    goto exit;
  }
  
  
  ret = fh->f_comm->c_coll.coll_allreduce (&max_data,
					   &total_bytes,
					   1,
					   MPI_DOUBLE,
					   MPI_SUM,
					   fh->f_comm,
					   fh->f_comm->c_coll.coll_allreduce_module);
  
  if ( OMPI_SUCCESS != ret ) {
    goto exit;
  }
  
  
    
  if (!(fh->f_flags & OMPIO_CONTIGUOUS_MEMORY)) {
    
    /* This datastructre translates between OMPIO->ROMIO its a little hacky!*/
    /* But helps to re-use romio's code for handling non-contiguous file-type*/
    flat_buf = (Flatlist_node *)malloc(sizeof(Flatlist_node));
    if ( NULL == flat_buf ){
      ret = OMPI_ERR_OUT_OF_RESOURCE;
      goto exit;
    }
    
    flat_buf->type = datatype;
    flat_buf->next = NULL;
    flat_buf->count = 0;
       
    
    local_size = iov_count/count;
    
    flat_buf->indices = 
      (OMPI_MPI_OFFSET_TYPE *)malloc(local_size * 
					       sizeof(OMPI_MPI_OFFSET_TYPE));
    if ( NULL == flat_buf->indices ){
      ret = OMPI_ERR_OUT_OF_RESOURCE;
      goto exit;
      
    }
      
    flat_buf->blocklens = 
      (OMPI_MPI_OFFSET_TYPE *)malloc(local_size * 
				     sizeof(OMPI_MPI_OFFSET_TYPE));
    if ( NULL == flat_buf->blocklens ){
      ret = OMPI_ERR_OUT_OF_RESOURCE;
	 goto exit;
    }
    
    flat_buf->count = local_size;
    i=0;j=0;
    while(j < local_size){
      flat_buf->indices[j] = (OMPI_MPI_OFFSET_TYPE)(intptr_t)decoded_iov[i].iov_base;
      flat_buf->blocklens[j] = decoded_iov[i].iov_len;
      if(i < (int)iov_count)
	i+=1;
      j+=1;
    }
    
#if DEBUG_ON
    printf("flat_buf_count : %d\n", flat_buf->count);
    for(i=0;i<flat_buf->count;i++){
      printf("%d: blocklen[%d] : %lld, indices[%d]: %lld \n",
	     fh->f_rank, i, flat_buf->blocklens[i], i ,flat_buf->indices[i]);
	 
    }
#endif
  }
  
#if DEBUG_ON
    printf("%d: fcoll:two_phase:write_all->total_bytes:%ld, local_count: %d\n",
	   fh->f_rank,total_bytes, local_count);
    for (i=0 ; i<local_count ; i++) {
      printf("%d: fcoll:two_phase:write_all:OFFSET:%ld,LENGTH:%ld\n",
	     fh->f_rank,
	     (size_t)iov[i].iov_base,
	     (size_t)iov[i].iov_len);
    }
    
    
#endif
    
    start_offset = (OMPI_MPI_OFFSET_TYPE)(uintptr_t)iov[0].iov_base;
    end_offset = (OMPI_MPI_OFFSET_TYPE)(uintptr_t)iov[local_count-1].iov_base +
      (OMPI_MPI_OFFSET_TYPE)iov[local_count-1].iov_len - 1; 
    
#if DEBUG_ON
    printf("%d: fcoll:two_phase:write_all:START OFFSET:%ld,END OFFSET:%ld\n",
	   fh->f_rank,
	   (size_t)start_offset,
	   (size_t)end_offset);
    
#endif

    start_offsets = (OMPI_MPI_OFFSET_TYPE *)malloc
	(fh->f_size*sizeof(OMPI_MPI_OFFSET_TYPE));
 
    if ( NULL == start_offsets ){
      ret = OMPI_ERR_OUT_OF_RESOURCE;
      goto exit; 
    }

    end_offsets = (OMPI_MPI_OFFSET_TYPE *)malloc
	(fh->f_size*sizeof(OMPI_MPI_OFFSET_TYPE));
    
    if ( NULL == end_offsets ){
      ret =  OMPI_ERR_OUT_OF_RESOURCE;
      goto exit;
    }
    
    
    ret = fh->f_comm->c_coll.coll_allgather(&start_offset,
					    1,
					    OMPI_OFFSET_DATATYPE,
					    start_offsets,
					    1,
					    OMPI_OFFSET_DATATYPE,
					    fh->f_comm,
					    fh->f_comm->c_coll.coll_allgather_module);

    if ( OMPI_SUCCESS != ret ){
      goto exit;
    }


    ret = fh->f_comm->c_coll.coll_allgather(&end_offset,
					    1,
					    OMPI_OFFSET_DATATYPE,
					    end_offsets,
					    1,
					    OMPI_OFFSET_DATATYPE,
					    fh->f_comm,
					    fh->f_comm->c_coll.coll_allgather_module);


    if ( OMPI_SUCCESS != ret ){
      goto exit;
    }
				      
#if DEBUG_ON
    for (i=0;i<fh->f_size;i++){
	printf("%d: fcoll:two_phase:write_all:start[%d]:%ld,end[%d]:%ld\n",
	       fh->f_rank,i,
	       (size_t)start_offsets[i],i,
	       (size_t)end_offsets[i]);
    }
#endif



    for (i=1; i<fh->f_size; i++){
      if ((start_offsets[i] < end_offsets[i-1]) && 
	  (start_offsets[i] <= end_offsets[i])){
	interleave_count++;
      }
    }

#if DEBUG_ON
    printf("%d: fcoll:two_phase:write_all:interleave_count:%d\n",
	   fh->f_rank,interleave_count);
#endif 
    
    
    ret = mca_fcoll_two_phase_domain_partition(fh,
					       start_offsets,
					       end_offsets,
					       &min_st_offset,
					       &fd_start,
					       &fd_end,
					       domain_size, 
					       &fd_size,
					       striping_unit,
					       mca_fcoll_two_phase_num_io_procs);
    if ( OMPI_SUCCESS != ret ){
      goto exit;
    }
    
    
#if  DEBUG_ON
	for (i=0;i<mca_fcoll_two_phase_num_io_procs;i++){
	  printf("fd_start[%d] : %lld, fd_end[%d] : %lld, local_count: %d\n",
		   i, fd_start[i], i, fd_end[i], local_count);
	}
#endif
	
	
	ret = mca_fcoll_two_phase_calc_my_requests (fh,
						    iov,
						    local_count,
						    min_st_offset,
						    fd_start,
						    fd_end,
						    fd_size,
						    &count_my_req_procs,
						    &count_my_req_per_proc,
						    &my_req,
						    &buf_indices,
						    striping_unit,
						    mca_fcoll_two_phase_num_io_procs,
						    aggregator_list);
	if ( OMPI_SUCCESS != ret ){
	  goto exit;
	}
	
	

	ret = mca_fcoll_two_phase_calc_others_requests(fh,
						       count_my_req_procs,
						       count_my_req_per_proc,
						       my_req,
						       &count_other_req_procs,
						       &others_req);
	if (OMPI_SUCCESS != ret ){
	  goto exit;
	}
	
	
#if DEBUG_ON
	printf("count_other_req_procs : %d\n", count_other_req_procs);
#endif

#if TIME_BREAKDOWN
	  start_exch = MPI_Wtime();
#endif
	
	ret = two_phase_exch_and_write(fh,
				       buf,
				       datatype,
				       others_req,
				       iov,
				       local_count,
				       min_st_offset,
				       fd_size,
				       fd_start,
				       fd_end,
				       flat_buf,
				       buf_indices,
				       striping_unit,
				       aggregator_list);

	if (OMPI_SUCCESS != ret){
	  goto exit;
	}
	

#if TIME_BREAKDOWN
	end_exch = MPI_Wtime();
	exch_write += (end_exch - start_exch);
	
	nentry.time[0] = write_time;
	nentry.time[1] = comm_time;
	nentry.time[2] = exch_write;
	if (is_aggregator(fh->f_rank,
			  mca_fcoll_two_phase_num_io_procs,
			  aggregator_list)){
	  nentry.aggregator = 1;
	}
	else{
	  nentry.aggregator = 0;
	}
	nentry.nprocs_for_coll = mca_fcoll_two_phase_num_io_procs;
	if (!ompi_io_ompio_full_print_queue(WRITE_PRINT_QUEUE)){
	  ompi_io_ompio_register_print_entry(WRITE_PRINT_QUEUE,
					     nentry);
	}
#endif

 exit : 
	if (flat_buf != NULL) {

	  if (flat_buf->blocklens != NULL) {
	    free (flat_buf->blocklens);
	  }
	  
	  if (flat_buf->indices != NULL) {
	    free (flat_buf->indices);
	  }
	  free (flat_buf);

	}



	if (start_offsets != NULL) {
	  free(start_offsets);
	}
	
	if (end_offsets != NULL){
	  free(end_offsets);
	}
	if (aggregator_list != NULL){
	  free(aggregator_list);
	}

	return ret;
}


static int two_phase_exch_and_write(mca_io_ompio_file_t *fh,
				    void *buf,
				    MPI_Datatype datatype,
				    mca_io_ompio_access_array_t *others_req,
				    struct iovec *offset_len,
				    int contig_access_count,
				    OMPI_MPI_OFFSET_TYPE min_st_offset,
				    OMPI_MPI_OFFSET_TYPE fd_size,
				    OMPI_MPI_OFFSET_TYPE *fd_start,
				    OMPI_MPI_OFFSET_TYPE *fd_end,
				    Flatlist_node *flat_buf,
				    size_t *buf_idx, int striping_unit,
				    int *aggregator_list)
    
{

  
    int i, j, ntimes, max_ntimes, m;
    int *curr_offlen_ptr=NULL, *count=NULL, *send_size=NULL, *recv_size=NULL;
    int *partial_recv=NULL, *start_pos=NULL, req_len, flag;
    int *sent_to_proc=NULL, ret = OMPI_SUCCESS;
    int *send_buf_idx=NULL, *curr_to_proc=NULL, *done_to_proc=NULL;
    OMPI_MPI_OFFSET_TYPE st_loc=-1, end_loc=-1, off, done;
    OMPI_MPI_OFFSET_TYPE size=0, req_off, len;
    MPI_Aint buftype_extent;
    int  hole;
    size_t byte_size;
    MPI_Datatype byte = MPI_BYTE;
    #if DEBUG_ON
    int ii,jj;
    #endif

    char *write_buf=NULL;


    opal_datatype_type_size(&byte->super,
			    &byte_size);
    
    for (i = 0; i < fh->f_size; i++){
	if (others_req[i].count) {
	    st_loc = others_req[i].offsets[0];
	    end_loc = others_req[i].offsets[0];
	    break;
	}
    }
    
    for (i=0;i<fh->f_size;i++){
	for(j=0;j< others_req[i].count; j++){
	    st_loc = OMPIO_MIN(st_loc, others_req[i].offsets[j]);
	    end_loc = OMPIO_MAX(end_loc, (others_req[i].offsets[j] + others_req[i].lens[j] - 1));
	    
	}
    }
    

    ntimes = (int) ((end_loc - st_loc + mca_fcoll_two_phase_cycle_buffer_size)/mca_fcoll_two_phase_cycle_buffer_size); 
    
    if ((st_loc == -1) && (end_loc == -1)) {
	ntimes = 0;
    }
    
    fh->f_comm->c_coll.coll_allreduce (&ntimes,
				       &max_ntimes,
				       1,
				       MPI_INT,
				       MPI_MAX,
				       fh->f_comm,
				       fh->f_comm->c_coll.coll_allreduce_module);

    if (ntimes){
      write_buf = (char *) malloc (mca_fcoll_two_phase_cycle_buffer_size);
      if ( NULL == write_buf ){
	return OMPI_ERR_OUT_OF_RESOURCE;
      }
    }

    curr_offlen_ptr = (int *) calloc(fh->f_size, sizeof(int)); 
    
    if ( NULL == curr_offlen_ptr ){
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    count = (int *) malloc(fh->f_size*sizeof(int));
    
    if ( NULL == count ){
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
   
    partial_recv = (int *)calloc(fh->f_size, sizeof(int));

    if ( NULL == partial_recv ){
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    send_size = (int *) calloc(fh->f_size,sizeof(int));

    if ( NULL == send_size ){
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    recv_size = (int *) calloc(fh->f_size,sizeof(int));

    if ( NULL == recv_size ){
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    send_buf_idx = (int *) malloc(fh->f_size*sizeof(int));

    if ( NULL == send_buf_idx ){
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    sent_to_proc = (int *) calloc(fh->f_size, sizeof(int));
    
    if ( NULL == sent_to_proc){
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    curr_to_proc = (int *) malloc(fh->f_size*sizeof(int));
    
    if ( NULL == curr_to_proc ){
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    done_to_proc = (int *) malloc(fh->f_size*sizeof(int));
    
    if ( NULL == done_to_proc ){
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    start_pos = (int *) malloc(fh->f_size*sizeof(int));
    
    if ( NULL == start_pos ){
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
 
  
    done = 0;
    off = st_loc;
    
    ompi_datatype_type_extent(datatype, &buftype_extent);
    for (m=0;m <ntimes; m++){
	for (i=0; i< fh->f_size; i++) count[i] = recv_size[i] = 0;
	
	size = OMPIO_MIN((unsigned)mca_fcoll_two_phase_cycle_buffer_size,
			 end_loc-st_loc+1-done);
	for (i=0;i<fh->f_size;i++){
	    if(others_req[i].count){
		start_pos[i] = curr_offlen_ptr[i];
		for (j=curr_offlen_ptr[i]; j<others_req[i].count; j++) {
		    		    if (partial_recv[i]) {
			/* this request may have been partially
			   satisfied in the previous iteration. */
			req_off = others_req[i].offsets[j] +
			    partial_recv[i]; 
                        req_len = others_req[i].lens[j] -
			    partial_recv[i];
			partial_recv[i] = 0;
			/* modify the off-len pair to reflect this change */
			others_req[i].offsets[j] = req_off;
			others_req[i].lens[j] = req_len;
		    }
		    else {
			req_off = others_req[i].offsets[j];
                        req_len = others_req[i].lens[j];
		    }
		    if (req_off < off + size) {
			count[i]++;
			#if DEBUG_ON
			printf("%d: req_off : %lld, off : %lld, size : %lld, count[%d]: %d\n", fh->f_rank,
			       req_off,
			       off,
			       size,i,
			       count[i]);
			#endif
			MPI_Address(write_buf+req_off-off,
				    &(others_req[i].mem_ptrs[j]));
			#if DEBUG_ON
			printf("%d : mem_ptrs : %ld\n", fh->f_rank,
			       others_req[i].mem_ptrs[j]);
			#endif
			recv_size[i] += (int) (OMPIO_MIN(off + size - req_off,
							 (unsigned)req_len));
			
			if (off+size-req_off < (unsigned)req_len){
			    
			    partial_recv[i] = (int)(off + size - req_off);
			    break;
			}
		    }
		    else break;
		}
		curr_offlen_ptr[i] = j;
	    }
	}	

	ret = two_phase_exchage_data(fh, buf, write_buf,
				     offset_len,send_size,
				     start_pos,recv_size,off,size,
				     count, partial_recv, sent_to_proc,
				     contig_access_count,
				     min_st_offset,
				     fd_size, fd_start,
				     fd_end, flat_buf, others_req,
				     send_buf_idx, curr_to_proc,
				     done_to_proc, m, buf_idx, 
				     buftype_extent, striping_unit,
				     aggregator_list, &hole);
	
	if ( OMPI_SUCCESS != ret ){
	  goto exit;
	}
	
	
	
	flag = 0;
	for (i=0; i<fh->f_size; i++)
	    if (count[i]) flag = 1;


	
	if (flag){

#if TIME_BREAKDOWN	  
	    start_write_time = MPI_Wtime();
#endif

	    #if DEBUG_ON
	    printf("rank : %d enters writing\n", fh->f_rank);
	    printf("size : %ld, off : %ld\n",size, off);
	    for (ii=0, jj=0;jj<size;jj+=4, ii++){
	      printf("%d : write_buf[%d]: %d\n", fh->f_rank, ii,((int *)write_buf[jj]));
	    }
	    #endif
	    len = size * byte_size;
	    fh->f_io_array = (mca_io_ompio_io_array_t *)malloc 
		(sizeof(mca_io_ompio_io_array_t));
	    if (NULL == fh->f_io_array) {
		opal_output(1, "OUT OF MEMORY\n");
		return OMPI_ERR_OUT_OF_RESOURCE;
	    }

	    fh->f_io_array[0].offset  =(IOVBASE_TYPE *)(intptr_t) off;
	    fh->f_io_array[0].length = len;
	    fh->f_io_array[0].memory_address = write_buf;
	    fh->f_num_of_io_entries = 1;

	    #if DEBUG_ON
            for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
                printf("%d: ADDRESS: %p  OFFSET: %ld   LENGTH: %d\n",
		       fh->f_rank,
                       fh->f_io_array[i].memory_address,
                       fh->f_io_array[i].offset,
                       fh->f_io_array[i].length);
            }
            #endif

	    if (fh->f_num_of_io_entries){
		if (OMPI_SUCCESS != fh->f_fbtl->fbtl_pwritev (fh, NULL)) {
		    opal_output(1, "WRITE FAILED\n");
		    return OMPI_ERROR;
		}
	    }
#if TIME_BREAKDOWN
	      end_write_time = MPI_Wtime();
	      write_time += (end_write_time - start_write_time);
#endif


	}
	/***************** DONE WRITING *****************************************/
	/****RESET **********************/
	fh->f_num_of_io_entries = 0;
	if (NULL != fh->f_io_array) {
	    free (fh->f_io_array);
	    fh->f_io_array = NULL;
	}

	off += size;
	done += size;
	
    }
    for (i=0; i<fh->f_size; i++) count[i] = recv_size[i] = 0;
    for (m=ntimes; m<max_ntimes; m++) {
      ret = two_phase_exchage_data(fh, buf, write_buf,
				   offset_len,send_size,
				   start_pos,recv_size,off,size,
				   count, partial_recv, sent_to_proc,
				   contig_access_count,
				   min_st_offset,
				   fd_size, fd_start,
				   fd_end, flat_buf,others_req,
				   send_buf_idx, curr_to_proc,
				   done_to_proc, m, buf_idx,
				   buftype_extent, striping_unit,
				   aggregator_list, &hole);
      if ( OMPI_SUCCESS != ret ){
	goto exit;
      }
    }
    
 exit:    
    
    if (ntimes){
      if ( NULL != write_buf ){
	free(write_buf);
      }
    }
    if ( NULL != curr_offlen_ptr ){
      free(curr_offlen_ptr);
    }
    if ( NULL != count ){ 
      free(count);
    }
    if ( NULL != partial_recv ){
      free(partial_recv);
    }
    if ( NULL != send_size ){
      free(send_size);
    }
    if ( NULL != recv_size ){
      free(recv_size);
    }
    if ( NULL != sent_to_proc ){
      free(sent_to_proc);
    }
    if ( NULL != start_pos ){
      free(start_pos);
    }
    if ( NULL != send_buf_idx ){
      free(send_buf_idx);
    }
    if ( NULL != curr_to_proc ){
      free(curr_to_proc);
    }
    if ( NULL != done_to_proc ){
      free(done_to_proc);
    }

    return ret;
}

static int two_phase_exchage_data(mca_io_ompio_file_t *fh,
				  void *buf,
				  char *write_buf,
				  struct iovec *offset_length,
				  int *send_size,int *start_pos,
				  int *recv_size,
				  OMPI_MPI_OFFSET_TYPE off,
				  OMPI_MPI_OFFSET_TYPE size, int *count,
				  int *partial_recv, int *sent_to_proc,
				  int contig_access_count,
				  OMPI_MPI_OFFSET_TYPE min_st_offset,
				  OMPI_MPI_OFFSET_TYPE fd_size,
				  OMPI_MPI_OFFSET_TYPE *fd_start,
				  OMPI_MPI_OFFSET_TYPE *fd_end,
				  Flatlist_node *flat_buf,
				  mca_io_ompio_access_array_t *others_req,
				  int *send_buf_idx, int *curr_to_proc,
				  int *done_to_proc, int iter,
				  size_t *buf_idx,MPI_Aint buftype_extent,
				  int striping_unit, int *aggregator_list,
				  int *hole){
  
    int *tmp_len=NULL, sum, *srt_len=NULL, nprocs_recv, nprocs_send,  k,i,j;
    int ret=OMPI_SUCCESS;
    MPI_Request *requests=NULL, *send_req=NULL;
    ompi_datatype_t **recv_types=NULL;
    OMPI_MPI_OFFSET_TYPE *srt_off=NULL;
    char **send_buf = NULL; 
    
#if TIME_BREAKDOWN
      start_comm_time = MPI_Wtime();
#endif
        
    ret = fh->f_comm->c_coll.coll_alltoall (recv_size,
					    1,
					    MPI_INT,
					    send_size,
					    1,
					    MPI_INT,
					    fh->f_comm,
					    fh->f_comm->c_coll.coll_alltoall_module);
    
    if ( OMPI_SUCCESS != ret ){
      return ret;
    }

    nprocs_recv = 0;
    for (i=0;i<fh->f_size;i++){
      if (recv_size[i]){
	nprocs_recv++;
      }
    }
    
    
    recv_types = (ompi_datatype_t **)
	malloc (( nprocs_recv + 1 ) * sizeof(ompi_datatype_t *));
    
    if ( NULL == recv_types ){
      ret = OMPI_ERR_OUT_OF_RESOURCE;
      goto exit;
    }

    tmp_len = (int *) malloc(fh->f_size*sizeof(int));
    
    if ( NULL == tmp_len ) {
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    j = 0;
    for (i=0;i<fh->f_size;i++){
	if (recv_size[i]) {
	    if (partial_recv[i]) {
		k = start_pos[i] + count[i] - 1;
		tmp_len[i] = others_req[i].lens[k];
		others_req[i].lens[k] = partial_recv[i];
	    }
	    ompi_datatype_create_hindexed(count[i], 
					  &(others_req[i].lens[start_pos[i]]),
					  &(others_req[i].mem_ptrs[start_pos[i]]), 
					  MPI_BYTE, recv_types+j);
	    ompi_datatype_commit(recv_types+j);
	    j++;
	}
    }

    sum = 0;
    for (i=0;i<fh->f_size;i++) sum += count[i];
    srt_off = (OMPI_MPI_OFFSET_TYPE *) 
      malloc((sum+1)*sizeof(OMPI_MPI_OFFSET_TYPE));
    
    if ( NULL == srt_off ){
      ret = OMPI_ERR_OUT_OF_RESOURCE;
      goto exit;
    }
    
    srt_len = (int *) malloc((sum+1)*sizeof(int));
    
    if ( NULL == srt_len ) {
      ret = OMPI_ERR_OUT_OF_RESOURCE;
      goto exit;
    }


    two_phase_heap_merge(others_req, count, srt_off, srt_len, start_pos, fh->f_size,fh->f_rank,  nprocs_recv, sum);


    for (i=0; i<fh->f_size; i++) 
        if (partial_recv[i]) {
            k = start_pos[i] + count[i] - 1;
            others_req[i].lens[k] = tmp_len[i];
        }
    
    if ( NULL != tmp_len ){
      free(tmp_len); 
    }

    *hole = 0;
    if (off != srt_off[0]){
	*hole = 1;
    }
    else{
	for (i=1;i<sum;i++){
	    if (srt_off[i] <= srt_off[0] + srt_len[0]){
		int new_len = srt_off[i] + srt_len[i] - srt_off[0];
		if(new_len > srt_len[0]) 
		    srt_len[0] = new_len;
	    }
	    else
		break;
	}
	if (i < sum || size != srt_len[0])
	    *hole = 1;
    }


    if ( NULL != srt_off ){
      free(srt_off);
    }
    if ( NULL != srt_len ){
      free(srt_len);
    }

    if (nprocs_recv){
	if (*hole){
	    if (off > 0){
		fh->f_io_array = (mca_io_ompio_io_array_t *)malloc 
		    (sizeof(mca_io_ompio_io_array_t));
		if (NULL == fh->f_io_array) {
		    opal_output(1, "OUT OF MEMORY\n");
		    return OMPI_ERR_OUT_OF_RESOURCE;
		}
		fh->f_io_array[0].offset  =(IOVBASE_TYPE *)(intptr_t) off;
		fh->f_num_of_io_entries = 1;
		fh->f_io_array[0].length = size;
		fh->f_io_array[0].memory_address = write_buf;
		if (fh->f_num_of_io_entries){
		    if (OMPI_SUCCESS != fh->f_fbtl->fbtl_preadv (fh, NULL)) {
			opal_output(1, "READ FAILED\n");
			return OMPI_ERROR;
		    }
		}
		
	    }
	    fh->f_num_of_io_entries = 0;
	    if (NULL != fh->f_io_array) {
		free (fh->f_io_array);
		fh->f_io_array = NULL;
	    }
	}
    }
    
    nprocs_send = 0;
    for (i=0; i <fh->f_size; i++) if (send_size[i]) nprocs_send++;

    #if DEBUG_ON
    printf("%d : nprocs_send : %d\n", fh->f_rank,nprocs_send);
    #endif

    requests = (MPI_Request *) 	
	malloc((nprocs_send+nprocs_recv+1)*sizeof(MPI_Request)); 

    if ( NULL == requests ){
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    j = 0;
    for (i=0; i<fh->f_size; i++) {
	if (recv_size[i]) {
	  ret = MCA_PML_CALL(irecv(MPI_BOTTOM,
				   1,
				   recv_types[j],
				   i,
				   fh->f_rank+i+100*iter,
				   fh->f_comm,
				   requests+j));

	  if ( OMPI_SUCCESS != ret ){
	    goto exit;
	  }
	  j++;
	}
    }
    send_req = requests + nprocs_recv;
    
    
    if (fh->f_flags & OMPIO_CONTIGUOUS_MEMORY) {
	j = 0;
	for (i=0; i <fh->f_size; i++) 
	  if (send_size[i]) {
	    ret = MCA_PML_CALL(isend(((char *) buf) + buf_idx[i],
				     send_size[i],
				     MPI_BYTE,
				     i,
				     fh->f_rank+i+100*iter,
				     MCA_PML_BASE_SEND_STANDARD, 
				     fh->f_comm,
				     send_req+j));	

	    if ( OMPI_SUCCESS != ret ){
	      goto exit;
	    }
	    
	    j++;
	    buf_idx[i] += send_size[i];
	  }
    }
    else if(nprocs_send && (!(fh->f_flags & OMPIO_CONTIGUOUS_MEMORY))){
      send_buf = (char **) malloc(fh->f_size*sizeof(char*));
      if ( NULL == send_buf ){
	ret = OMPI_ERR_OUT_OF_RESOURCE;
	goto exit;
      }
      for (i=0; i < fh->f_size; i++){
	if (send_size[i]) {
	  send_buf[i] = (char *) malloc(send_size[i]);
	  
	  if ( NULL == send_buf[i] ){
	    ret = OMPI_ERR_OUT_OF_RESOURCE;
	    goto exit;
	  }
	}
      }
      
      ret = two_phase_fill_send_buffer(fh, buf,flat_buf, send_buf,
				       offset_length, send_size,
				       send_req,sent_to_proc,
				       contig_access_count, 
				       min_st_offset, fd_size,
				       fd_start, fd_end, send_buf_idx,
				       curr_to_proc, done_to_proc,
				       iter, buftype_extent, striping_unit,
				       aggregator_list);
      
      if ( OMPI_SUCCESS != ret ){
	goto exit;
      }
    }


    for (i=0; i<nprocs_recv; i++) ompi_datatype_destroy(recv_types+i);
    if (NULL != recv_types){
      free(recv_types);
      recv_types = NULL;
    }

    ret = ompi_request_wait_all (nprocs_send+nprocs_recv,
				 requests,
				 MPI_STATUS_IGNORE);
    

    if ( NULL != requests ){
      free(requests);
    }

#if TIME_BREAKDOWN
      end_comm_time = MPI_Wtime();
      comm_time += (end_comm_time - start_comm_time);
#endif

 exit:    
    return ret;
}


#define TWO_PHASE_BUF_INCR \
{ \
    while (buf_incr) { \
        size_in_buf = OMPIO_MIN(buf_incr, flat_buf_sz); \
        user_buf_idx += size_in_buf; \
        flat_buf_sz -= size_in_buf;\
        if (!flat_buf_sz) { \
            if (flat_buf_idx < (flat_buf->count - 1)) flat_buf_idx++; \
            else { \
                flat_buf_idx = 0; \
                n_buftypes++; \
            } \
            user_buf_idx = flat_buf->indices[flat_buf_idx] + \
		(OMPI_MPI_OFFSET_TYPE)n_buftypes*(OMPI_MPI_OFFSET_TYPE)buftype_extent; \
            flat_buf_sz = flat_buf->blocklens[flat_buf_idx]; \
        } \
        buf_incr -= size_in_buf; \
    } \
}


#define TWO_PHASE_BUF_COPY \
{ \
    while (size) { \
        size_in_buf = OMPIO_MIN(size, flat_buf_sz); \
        memcpy(&(send_buf[p][send_buf_idx[p]]), \
               ((char *) buf) + user_buf_idx, size_in_buf); \
        send_buf_idx[p] += size_in_buf; \
        user_buf_idx += size_in_buf; \
        flat_buf_sz -= size_in_buf; \
        if (!flat_buf_sz) { \
            if (flat_buf_idx < (flat_buf->count - 1)) flat_buf_idx++; \
            else { \
                flat_buf_idx = 0; \
                n_buftypes++; \
            } \
            user_buf_idx = flat_buf->indices[flat_buf_idx] + \
                              (OMPI_MPI_OFFSET_TYPE)n_buftypes*(OMPI_MPI_OFFSET_TYPE)buftype_extent; \
            flat_buf_sz = flat_buf->blocklens[flat_buf_idx]; \
        } \
        size -= size_in_buf; \
        buf_incr -= size_in_buf; \
    } \
    TWO_PHASE_BUF_INCR \
}





static int two_phase_fill_send_buffer(mca_io_ompio_file_t *fh,
				      void *buf,
				      Flatlist_node *flat_buf,
				      char **send_buf,
				      struct iovec *offset_length,
				      int *send_size,
				      MPI_Request *requests,
				      int *sent_to_proc,
				      int contig_access_count, 
				      OMPI_MPI_OFFSET_TYPE min_st_offset,
				      OMPI_MPI_OFFSET_TYPE fd_size,
				      OMPI_MPI_OFFSET_TYPE *fd_start,
				      OMPI_MPI_OFFSET_TYPE *fd_end,
				      int *send_buf_idx,
				      int *curr_to_proc, 
				      int *done_to_proc,
				      int iter, MPI_Aint buftype_extent,
				      int striping_unit, int *aggregator_list){

    int i, p, flat_buf_idx;
    OMPI_MPI_OFFSET_TYPE flat_buf_sz, size_in_buf, buf_incr, size;
    int jj, n_buftypes, ret=OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE off, len, rem_len, user_buf_idx;

    for (i=0; i < fh->f_size; i++) {
	send_buf_idx[i] = curr_to_proc[i] = 0;
	done_to_proc[i] = sent_to_proc[i];
    }
    jj = 0;
    
    user_buf_idx = flat_buf->indices[0];
    flat_buf_idx = 0;
    n_buftypes = 0;
    flat_buf_sz = flat_buf->blocklens[0];
    
    for (i=0; i<contig_access_count; i++) { 
	
      off     = (OMPI_MPI_OFFSET_TYPE) (intptr_t)offset_length[i].iov_base;
	rem_len = (OMPI_MPI_OFFSET_TYPE)offset_length[i].iov_len;
	

	while (rem_len != 0) {
	    len = rem_len;
	    p = mca_fcoll_two_phase_calc_aggregator(fh,
						    off,
						    min_st_offset,
						    &len,
						    fd_size,
						    fd_start,
						    fd_end,
						    striping_unit,
						    mca_fcoll_two_phase_num_io_procs,
						    aggregator_list);

	    if (send_buf_idx[p] < send_size[p]) {
		if (curr_to_proc[p]+len > done_to_proc[p]) {
		    if (done_to_proc[p] > curr_to_proc[p]) {
			size = OMPIO_MIN(curr_to_proc[p] + len - 
					 done_to_proc[p], send_size[p]-send_buf_idx[p]);
			buf_incr = done_to_proc[p] - curr_to_proc[p];
			TWO_PHASE_BUF_INCR
		        buf_incr = curr_to_proc[p] + len - done_to_proc[p];
			curr_to_proc[p] = done_to_proc[p] + size;
		        TWO_PHASE_BUF_COPY
		    }
		    else {
			size = OMPIO_MIN(len,send_size[p]-send_buf_idx[p]);
			buf_incr = len;
			curr_to_proc[p] += size;
			TWO_PHASE_BUF_COPY
		    }
		    if (send_buf_idx[p] == send_size[p]) {

		      ret = MCA_PML_CALL(isend(send_buf[p],
					       send_size[p],
					       MPI_BYTE,
					       p,
					       fh->f_rank+p+100*iter,
					       MCA_PML_BASE_SEND_STANDARD, 
					       fh->f_comm,
					       requests+jj));	
		      
		      if ( OMPI_SUCCESS != ret ){
			return ret;
		      }
		      jj++;
		    }
		}
		else {
		    curr_to_proc[p] += len;
		    buf_incr = len;
		    TWO_PHASE_BUF_INCR
		}
	    }
	    else {
		buf_incr = len;
		TWO_PHASE_BUF_INCR
            }
	    off     += len;
	    rem_len -= len;
	}
    }
    for (i=0; i < fh->f_size; i++) {
      if (send_size[i]){
	sent_to_proc[i] = curr_to_proc[i];
      }
    }

    return ret;
}
    

	



void two_phase_heap_merge( mca_io_ompio_access_array_t *others_req,
			   int *count, 
			   OMPI_MPI_OFFSET_TYPE *srt_off,
			   int *srt_len,
			   int *start_pos,
			   int nprocs,
			   int myrank,
			   int nprocs_recv,
			   int total_elements)
{



    typedef struct {
	OMPI_MPI_OFFSET_TYPE *off_list;
	int *len_list;
	int nelem;
    } heap_struct;

    heap_struct *a, tmp;
    int i, j, heapsize, l, r, k, smallest;

    a = (heap_struct *) malloc((nprocs_recv+1)*sizeof(heap_struct));

    j = 0;
    for (i=0; i<nprocs; i++)
	if (count[i]) {
	    a[j].off_list = &(others_req[i].offsets[start_pos[i]]);
	    a[j].len_list = &(others_req[i].lens[start_pos[i]]);
	    a[j].nelem = count[i];
	    j++;
	}

    heapsize = nprocs_recv;

    for (i=heapsize/2 - 1; i>=0; i--) {
	k = i;
	for(;;) {
	    l = 2*(k+1) - 1;
	    r = 2*(k+1);
	    if ((l < heapsize) && 
		(*(a[l].off_list) < *(a[k].off_list)))
		smallest = l;
	    else smallest = k;

	    if ((r < heapsize) && 
		(*(a[r].off_list) < *(a[smallest].off_list)))
		smallest = r;

	    if (smallest != k) {
		tmp.off_list = a[k].off_list;
		tmp.len_list = a[k].len_list;
		tmp.nelem = a[k].nelem;

		a[k].off_list = a[smallest].off_list;
		a[k].len_list = a[smallest].len_list;
		a[k].nelem = a[smallest].nelem;
		
		a[smallest].off_list = tmp.off_list;
		a[smallest].len_list = tmp.len_list;
		a[smallest].nelem = tmp.nelem;
	    
		k = smallest;
	    }
	    else break;
	}
    }


    for (i=0; i<total_elements; i++) {
        /* extract smallest element from heap, i.e. the root */
	srt_off[i] = *(a[0].off_list);
	srt_len[i] = *(a[0].len_list);
	(a[0].nelem)--;

	if (!a[0].nelem) {
	    a[0].off_list = a[heapsize-1].off_list;
	    a[0].len_list = a[heapsize-1].len_list;
	    a[0].nelem = a[heapsize-1].nelem;
	    heapsize--;
	}
	else {
	    (a[0].off_list)++;
	    (a[0].len_list)++;
	}

	/* Heapify(a, 0, heapsize); */
	k = 0;
	for (;;) {
	    l = 2*(k+1) - 1;
	    r = 2*(k+1);

	    if ((l < heapsize) && 
		(*(a[l].off_list) < *(a[k].off_list)))
		smallest = l;
	    else smallest = k;

	    if ((r < heapsize) && 
		(*(a[r].off_list) < *(a[smallest].off_list)))
		smallest = r;

	    if (smallest != k) {
		tmp.off_list = a[k].off_list;
		tmp.len_list = a[k].len_list;
		tmp.nelem = a[k].nelem;

		a[k].off_list = a[smallest].off_list;
		a[k].len_list = a[smallest].len_list;
		a[k].nelem = a[smallest].nelem;
		
		a[smallest].off_list = tmp.off_list;
		a[smallest].len_list = tmp.len_list;
		a[smallest].nelem = tmp.nelem;
	    
		k = smallest;
	    }
	    else break;
	}
    }
    free(a);
}
#if TIME_BREAKDOWN
int is_aggregator(int rank, 
		  int nprocs_for_coll,
		  int *aggregator_list){
  
  int i=0;
  for (i=0; i<nprocs_for_coll; i++){
    if (aggregator_list[i] == rank)
      return 1;
  }
  return 0;
}
#endif
