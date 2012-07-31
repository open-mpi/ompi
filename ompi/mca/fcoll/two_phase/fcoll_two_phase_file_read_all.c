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

#define TIME_BREAKDOWN 0
#define DEBUG_ON 0


/* Two Phase implementation from ROMIO ported to OMPIO infrastructure 
   This is exactly similar to ROMIO's two_phase implementation*/



/* Datastructure to support specifying the flat-list. */
typedef struct flat_list_node {  
    MPI_Datatype type;
    int count;                
    OMPI_MPI_OFFSET_TYPE *blocklens;
    OMPI_MPI_OFFSET_TYPE *indices;  
    struct flat_list_node *next;  
} Flatlist_node;



/* local function declarations  */
static int two_phase_read_and_exch(mca_io_ompio_file_t *fh,
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
				   int *buf_idx, int striping_unit,
				   int *aggregator_list);



static int  two_phase_exchage_data(mca_io_ompio_file_t *fh,
				   void *buf,
				   struct iovec *offset_length,
				   int *send_size, int *start_pos,
				   int *recv_size,
				   int *count,
				   int *partial_send, int *recd_from_proc,
				   int contig_access_count,
				   OMPI_MPI_OFFSET_TYPE min_st_offset,
				   OMPI_MPI_OFFSET_TYPE fd_size,
				   OMPI_MPI_OFFSET_TYPE *fd_start,
				   OMPI_MPI_OFFSET_TYPE *fd_end,
				   Flatlist_node *flat_buf,
				   mca_io_ompio_access_array_t *others_req,
				   int iter,
				   int *buf_idx, MPI_Aint buftype_extent,
				   int striping_unit, int *aggregator_list);

static int two_phase_fill_user_buffer(mca_io_ompio_file_t *fh,
				      void *buf,
				      Flatlist_node *flat_buf,
				      char **recv_buf,
				      struct iovec *offset_length,
				      unsigned *recv_size,
				      MPI_Request *requests,
				      int *recd_from_proc,
				      int contig_access_count, 
				      OMPI_MPI_OFFSET_TYPE min_st_offset,
				      OMPI_MPI_OFFSET_TYPE fd_size,
				      OMPI_MPI_OFFSET_TYPE *fd_start,
				      OMPI_MPI_OFFSET_TYPE *fd_end,
				      MPI_Aint buftype_extent,
				      int striping_unit, int *aggregator_list);

/* local function declarations  ends here!*/

int
mca_fcoll_two_phase_file_read_all (mca_io_ompio_file_t *fh,
				   void *buf,
				   int count,
				   struct ompi_datatype_t *datatype,
				   ompi_status_public_t *status)
{

    

    int i, j,interleave_count=0, striping_unit=0;
    size_t max_data = 0, total_bytes = 0; 
    int domain_size=0, *count_my_req_per_proc=NULL, count_my_req_procs;
    int count_other_req_procs, *buf_indices, ret=OMPI_SUCCESS;
    int local_count = 0, local_size=0,*aggregator_list = NULL;
    struct iovec *iov = NULL, *decoded_iov=NULL;
    struct iovec *temp_iov = NULL;
    uint32_t iov_count, ti;
    MPI_Aint recv_buf_addr;
    OMPI_MPI_OFFSET_TYPE start_offset, end_offset, fd_size;
    OMPI_MPI_OFFSET_TYPE *start_offsets=NULL, *end_offsets=NULL;
    OMPI_MPI_OFFSET_TYPE *fd_start=NULL, *fd_end=NULL, min_st_offset;
    Flatlist_node *flat_buf=NULL;
    mca_io_ompio_access_array_t *my_req=NULL, *others_req=NULL;


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
      
      
    
      recv_buf_addr = (OPAL_PTRDIFF_TYPE)(buf);  
      decoded_iov = (struct iovec *) malloc
	(iov_count * sizeof(struct iovec));
      for (ti = 0; ti < iov_count; ti ++){
	
	decoded_iov[ti].iov_base = (IOVBASE_TYPE *)
	  ((OPAL_PTRDIFF_TYPE)temp_iov[ti].iov_base - 
	   recv_buf_addr);
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
      /*This is similar to ranklist in romio's two-phase.
       There is nothing fancy in the way romio assigns it too!!*/
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
      /*Flattened datatype for ompio is in decoded_iov it translated into
       flatbuf*/
      flat_buf = (Flatlist_node *)malloc(sizeof(Flatlist_node));
      if ( NULL == flat_buf ){
	ret = OMPI_ERR_OUT_OF_RESOURCE;
	goto exit;
      }

      flat_buf->type = datatype;
      flat_buf->next = NULL;
      flat_buf->count = 0;
       
       if(decoded_iov[0].iov_base == 0 ||
	  (OMPI_MPI_OFFSET_TYPE)decoded_iov[iov_count-1].iov_base +
	  (OMPI_MPI_OFFSET_TYPE)decoded_iov[iov_count-1].iov_len == (OMPI_MPI_OFFSET_TYPE)total_bytes){
	 local_size = iov_count/count + 1;
       }
       else
	 local_size = iov_count/count + 2;
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
	 if (0 == j && (OMPI_MPI_OFFSET_TYPE)decoded_iov[i].iov_base > 0){
	   flat_buf->indices[j] = 0;
	   flat_buf->blocklens[j] = 0;
	   j+=1;
	   flat_buf->indices[j] = (OMPI_MPI_OFFSET_TYPE)decoded_iov[i].iov_base;
	   flat_buf->blocklens[j] = decoded_iov[i].iov_len;
	 }
	 else if ((local_size - 1  == j) && 
		  (OMPI_MPI_OFFSET_TYPE)decoded_iov[iov_count-1].iov_base +
		  (OMPI_MPI_OFFSET_TYPE)decoded_iov[iov_count-1].iov_len != (OMPI_MPI_OFFSET_TYPE)total_bytes){
	   flat_buf->indices[j] = total_bytes;
	   flat_buf->blocklens[j] = 0;
	 }
	 else {
	   flat_buf->indices[j] = (OMPI_MPI_OFFSET_TYPE)decoded_iov[i].iov_base;
	   flat_buf->blocklens[j] = decoded_iov[i].iov_len;
	 }
	 if(i < (int)iov_count)
	   i+=1;
	 j+=1;
       }
       
#if DEBUG_ON
       for(i=0;i<flat_buf->count;i++){
	 printf("%d: blocklen[%d] : %lld, indices[%d]: %lld \n",
		fh->f_rank, i, flat_buf->blocklens[i], i ,flat_buf->indices[i]);
	 
       }
#endif
     }

#if DEBUG_ON
    printf("%d: fcoll:two_phase:read_all->total_bytes:%ld, local_count: %d\n",
	   fh->f_rank,total_bytes, local_count);
    for (i=0 ; i<local_count ; i++) {
	printf("%d: fcoll:two_phase:read_all:OFFSET:%ld,LENGTH:%ld\n",
	       fh->f_rank,
	       (size_t)iov[i].iov_base,
	       (size_t)iov[i].iov_len);
    }
	
    
#endif
    
    start_offset = (OMPI_MPI_OFFSET_TYPE)iov[0].iov_base;
    end_offset = (OMPI_MPI_OFFSET_TYPE)iov[local_count-1].iov_base +
	(OMPI_MPI_OFFSET_TYPE)iov[local_count-1].iov_len - 1; 
    
#if DEBUG_ON
    printf("%d: fcoll:two_phase:read_all:START OFFSET:%ld,END OFFSET:%ld\n",
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
					    MPI_LONG,
					    start_offsets,
					    1,
					    MPI_LONG,
					    fh->f_comm,
					    fh->f_comm->c_coll.coll_allgather_module);

    if ( OMPI_SUCCESS != ret ){
      goto exit;
    }


    ret = fh->f_comm->c_coll.coll_allgather(&end_offset,
					    1,
					    MPI_LONG,
					    end_offsets,
					    1,
					    MPI_LONG,
					    fh->f_comm,
					    fh->f_comm->c_coll.coll_allgather_module);


    if ( OMPI_SUCCESS != ret ){
      goto exit;
    }
				      
    


    
#if DEBUG_ON
    for (i=0;i<fh->f_size;i++){
	printf("%d: fcoll:two_phase:read_all:start[%d]:%ld,end[%d]:%ld\n",
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
    	printf("%d: fcoll:two_phase:read_all:interleave_count:%d\n",
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
	

	ret = two_phase_read_and_exch(fh,
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
	


 exit : 
	if (flat_buf != NULL) {
	  free (flat_buf);

	  if (flat_buf->blocklens != NULL) {
	    free (flat_buf->blocklens);
	  }
	  
	  if (flat_buf->indices != NULL) {
	    free (flat_buf->indices);
	  }
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


static int two_phase_read_and_exch(mca_io_ompio_file_t *fh,
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
				   int *buf_idx, int striping_unit,
				   int *aggregator_list)
    
{

  
    int i, j, ntimes, max_ntimes, m;
    int *curr_offlen_ptr=NULL, *count=NULL, *send_size=NULL, *recv_size=NULL;
    int *partial_send=NULL, *start_pos=NULL, req_len, flag;
    int *recd_from_proc=NULL, ret = OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE st_loc=-1, end_loc=-1, off, done, for_next_iter;
    OMPI_MPI_OFFSET_TYPE size=0, req_off, real_size,  real_off, len, for_curr_iter;
    MPI_Aint buftype_extent;
    int byte_size;


    char *read_buf=NULL, *tmp_buf=NULL;
    /* To be moved to fbtl */
    MPI_Type_size(MPI_BYTE, &byte_size);
    
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
      read_buf = (char *) malloc (mca_fcoll_two_phase_cycle_buffer_size);
      if ( NULL == read_buf ){
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
   
    partial_send = (int *)calloc(fh->f_size, sizeof(int));

    if ( NULL == partial_send ){
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

    recd_from_proc = (int *) calloc(fh->f_size, sizeof(int));
    
    if ( NULL == recd_from_proc){
      return OMPI_ERR_OUT_OF_RESOURCE;
    }

    start_pos = (int *) malloc(fh->f_size*sizeof(int));
    
    if ( NULL == start_pos ){
      return OMPI_ERR_OUT_OF_RESOURCE;
    }
 
  
    done = 0;
    off = st_loc;
    for_curr_iter = for_next_iter = 0;
    
    MPI_Type_extent(datatype, &buftype_extent);

    for (m=0;m <ntimes; m++){
	for (i=0; i< fh->f_size; i++) count[i] = recv_size[i] = 0;
	
	size = OMPIO_MIN((unsigned)mca_fcoll_two_phase_cycle_buffer_size,
			 end_loc-st_loc+1-done);
	real_off = off - for_curr_iter;
	real_size = size + for_curr_iter;

	for (i=0;i<fh->f_size; i++){
	  count[i] = send_size[i] = 0;
	}
	for_next_iter = 0;

	for (i=0;i<fh->f_size;i++){
	  
	    if(others_req[i].count){
		start_pos[i] = curr_offlen_ptr[i];
		for (j=curr_offlen_ptr[i]; j<others_req[i].count; j++) {
		  if (partial_send[i]) {
		    /* this request may have been partially
		       satisfied in the previous iteration. */
		    req_off = others_req[i].offsets[j] +
		      partial_send[i]; 
		    req_len = others_req[i].lens[j] -
		      partial_send[i];
		    partial_send[i] = 0;
		    /* modify the off-len pair to reflect this change */
		    others_req[i].offsets[j] = req_off;
		    others_req[i].lens[j] = req_len;
		  }
		  else {
		    req_off = others_req[i].offsets[j];
		    req_len = others_req[i].lens[j];
		  }
		  if (req_off < real_off + real_size) {
		    count[i]++;
#if DEBUG_ON
		    printf("%d: req_off : %lld, real_off : %lld, real_size : %lld, count[%d]: %d\n", fh->f_rank,
			   req_off,
			   real_off,
			   real_size,i,
			   count[i]);
#endif
		    MPI_Address(read_buf+req_off-real_off,
				&(others_req[i].mem_ptrs[j]));
#if DEBUG_ON
		    printf("%d : mem_ptrs : %ld\n", fh->f_rank,
			   others_req[i].mem_ptrs[j]);
#endif
		    send_size[i] += (int) (OMPIO_MIN(real_off + real_size - req_off,
						     (OMPI_MPI_OFFSET_TYPE)(unsigned)req_len));
		    
		    if (real_off+real_size-req_off < (OMPI_MPI_OFFSET_TYPE)(unsigned)req_len){
		      
		      partial_send[i] = (int)(real_off + real_size - req_off);
		      if ((j+1 < others_req[i].count) && 
			  (others_req[i].offsets[j+1] < real_off+real_size)){
			
			for_next_iter = OMPIO_MAX(for_next_iter, 
						  real_off + real_size - others_req[i].offsets[j+1]);
		      }
		      break;
		    }
		  }
		  else break;
		}
		curr_offlen_ptr[i] = j;
	    }
	}	
	
	flag = 0;

	for (i=0; i<fh->f_size; i++){
	  if (count[i]){
	    flag = 1;
	  }	  
	}

	if (flag){
	    len = size * byte_size;

	    fh->f_io_array = (mca_io_ompio_io_array_t *)malloc 
		(sizeof(mca_io_ompio_io_array_t));
	    if (NULL == fh->f_io_array) {
		opal_output(1, "OUT OF MEMORY\n");
		return OMPI_ERR_OUT_OF_RESOURCE;
	    }
	    fh->f_io_array[0].offset  =(IOVBASE_TYPE *) off;
	    fh->f_io_array[0].length = len;
	    fh->f_io_array[0].memory_address = read_buf+for_curr_iter;
	    fh->f_num_of_io_entries = 1;

	    #if DEBUG_ON
            for (i=0 ; i<fh->f_num_of_io_entries ; i++) {
                printf("%d: ADDRESS: %p  OFFSET: %ld   LENGTH: %ld\n",
		       fh->f_rank,
                       fh->f_io_array[i].memory_address,
                       (OPAL_PTRDIFF_TYPE)fh->f_io_array[i].offset,
                       fh->f_io_array[i].length);
            }
            #endif

	    if (fh->f_num_of_io_entries){
		if (OMPI_SUCCESS != fh->f_fbtl->fbtl_preadv (fh, NULL)) {
		    opal_output(1, "READ FAILED\n");
		    return OMPI_ERROR;
		}
	    }
	    /***************** DONE READING *****************************************/
	    /****RESET **********************/
	    fh->f_num_of_io_entries = 0;
	    if (NULL != fh->f_io_array) {
	      free (fh->f_io_array);
	      fh->f_io_array = NULL;
	    }

	}
	
	for_curr_iter = for_next_iter;
	
	two_phase_exchage_data(fh,
			       buf,
			       offset_len,
			       send_size, start_pos,
			       recv_size,
			       count,
			       partial_send, 
			       recd_from_proc,
			       contig_access_count,
			       min_st_offset,
			       fd_size,
			       fd_start,
			       fd_end,
			       flat_buf,
			       others_req,
			       m,
			       buf_idx, buftype_extent,
			       striping_unit, aggregator_list);

	
	if(for_next_iter){
	  tmp_buf = (char *) malloc (for_next_iter);
	  if (NULL == tmp_buf){
	    return OMPI_ERR_OUT_OF_RESOURCE;
	  }
	  memcpy(tmp_buf, read_buf+real_size-for_next_iter, for_next_iter);
	  free(read_buf);
	  read_buf = (char *) malloc (for_next_iter+mca_fcoll_two_phase_cycle_buffer_size);
	  memcpy(read_buf, tmp_buf, for_next_iter);
	  free(tmp_buf);
	  
	}

	off += size;
	done += size;
	
    }
    for (i=0; i<fh->f_size; i++) count[i] = send_size[i] = 0;
    for (m=ntimes; m<max_ntimes; m++) {
      
     
     ret =  two_phase_exchage_data(fh,
				   buf,
				   offset_len,
				   send_size, start_pos,
				   recv_size,
				   count,
				   partial_send, 
				   recd_from_proc,
				   contig_access_count,
				   min_st_offset,
				   fd_size,
				   fd_start,
				   fd_end,
				   flat_buf,
				   others_req,
				   m,
				   buf_idx, buftype_extent,
				   striping_unit, aggregator_list);
     
     if ( OMPI_SUCCESS != ret ){
       goto exit;
     }
    }
    
 exit:    
    
    if (ntimes){
      if ( NULL != read_buf ){
	free(read_buf);
      }
    }
    if ( NULL != curr_offlen_ptr ){
      free(curr_offlen_ptr);
    }
    if ( NULL != count ){ 
      free(count);
    }
    if ( NULL != partial_send ){
      free(partial_send);
    }
    if ( NULL != send_size ){
      free(send_size);
    }
    if ( NULL != recv_size ){
      free(recv_size);
    }
    if ( NULL != recd_from_proc ){
      free(recd_from_proc);
    }
    if ( NULL != start_pos ){
      free(start_pos);
    }


    return ret;
}

static int  two_phase_exchage_data(mca_io_ompio_file_t *fh,
				   void *buf,
				   struct iovec *offset_length,
				   int *send_size, int *start_pos,
				   int *recv_size,
				   int *count,
				   int *partial_send, int *recd_from_proc,
				   int contig_access_count,
				   OMPI_MPI_OFFSET_TYPE min_st_offset,
				   OMPI_MPI_OFFSET_TYPE fd_size,
				   OMPI_MPI_OFFSET_TYPE *fd_start,
				   OMPI_MPI_OFFSET_TYPE *fd_end,
				   Flatlist_node *flat_buf,
				   mca_io_ompio_access_array_t *others_req,
				   int iter,
				   int *buf_idx, MPI_Aint buftype_extent,
				   int striping_unit, int *aggregator_list){
  
  
  int i, j, k=0, tmp=0, nprocs_recv, nprocs_send;
  char **recv_buf = NULL; 
  MPI_Request *requests;
  MPI_Datatype send_type;
  int ret = OMPI_SUCCESS;
  
  ret = fh->f_comm->c_coll.coll_alltoall (send_size,
					  1,
					  MPI_INT,
					  recv_size,
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
  
  nprocs_send = 0;
  for (i=0;i<fh->f_size;i++){
    if (send_size[i]){
      nprocs_send++;
    }
  }
  
  requests = (MPI_Request *)
    malloc((nprocs_send+nprocs_recv+1)*sizeof(MPI_Request));
  
  if ( NULL == requests){
    return OMPI_ERR_OUT_OF_RESOURCE;  
  }

  
  if (fh->f_flags & OMPIO_CONTIGUOUS_MEMORY) {
    j=0;
    for (i=0; i <fh->f_size; i++){
      if (recv_size[i]){
	ret = MCA_PML_CALL(irecv(((char *) buf)+ buf_idx[i],
				 recv_size[i],
				 MPI_BYTE,
				 i,
				 fh->f_rank+i+100*iter,
				 fh->f_comm,
				 requests+j));
	
	if ( OMPI_SUCCESS != ret ){
	  return ret;
	}
	j++;
	buf_idx[i] += recv_size[i];
      }
    }
  }
  else{
    recv_buf = (char **) malloc(fh->f_size * sizeof(char*));
    if ( NULL == recv_buf ){
      return OMPI_ERR_OUT_OF_RESOURCE;  
    }
    for (i=0; i < fh->f_size; i++) {
      if (recv_size[i]) recv_buf[i] = (char *) malloc(recv_size[i]);
      if ( NULL == recv_buf[i] ){
	return OMPI_ERR_OUT_OF_RESOURCE;  
      }
      j = 0;
      for (i=0; i < fh->f_size; i++) {
	if (recv_size[i]) {
	  ret = MCA_PML_CALL(irecv(recv_buf[i],
				   recv_size[i],
				   MPI_BYTE,
				   i,
				   fh->f_rank+i+100*iter,
				   fh->f_comm,
				   requests+j));
	  j++;
	}
      }
      
    }
  }

  
  j = 0;
  for (i=0; i< fh->f_size; i++){
    if (send_size[i]) {
      if (partial_send[i]) {
	k = start_pos[i] + count[i] - 1;
	tmp = others_req[i].lens[k];
	others_req[i].lens[k] = partial_send[i];
      }
      MPI_Type_hindexed(count[i],
			&(others_req[i].lens[start_pos[i]]),
			&(others_req[i].mem_ptrs[start_pos[i]]),
			MPI_BYTE, &send_type);
      MPI_Type_commit(&send_type);
      ret = MCA_PML_CALL(isend( MPI_BOTTOM,
				1,
				send_type,
				i,
				fh->f_rank+i+100*iter,
				MCA_PML_BASE_SEND_STANDARD, 
				fh->f_comm,
				requests+nprocs_recv+j));	
      if ( OMPI_SUCCESS != ret ){
	return ret;
      }
      
      MPI_Type_free(&send_type);
      if (partial_send[i]) others_req[i].lens[k] = tmp;
      j++;
      
    }
  }

  if(nprocs_recv){
    ret = ompi_request_wait_all(nprocs_recv,
				requests, 
				MPI_STATUS_IGNORE);

    
    if (!(fh->f_flags & OMPIO_CONTIGUOUS_MEMORY)){
      
      two_phase_fill_user_buffer(fh,buf,flat_buf,recv_buf,
				 offset_length,(unsigned *)recv_size,
				 requests,
				 recd_from_proc,
				 contig_access_count, 
				 min_st_offset, fd_size,
				 fd_start, fd_end, buftype_extent,
				 striping_unit, aggregator_list);
    }
  
  }
  
  ret = ompi_request_wait_all(nprocs_send,
			      requests+nprocs_recv, 
			      MPI_STATUS_IGNORE);
  if (NULL != requests){
    free(requests);
  }
  if (!(fh->f_flags & OMPIO_CONTIGUOUS_MEMORY)){
    for (i=0;i< fh->f_size; i++){
      if (recv_size[i]){
	free(recv_buf[i]);
      }
    }
    free(recv_buf);
  }  
  
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
	memcpy(((char *) buf) + user_buf_idx, \
	       &(recv_buf[p][recv_buf_idx[p]]), size_in_buf); \
	recv_buf_idx[p] += size_in_buf; \
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



static int two_phase_fill_user_buffer(mca_io_ompio_file_t *fh,
				      void *buf,
				      Flatlist_node *flat_buf,
				      char **recv_buf,
				      struct iovec *offset_length,
				      unsigned *recv_size,
				      MPI_Request *requests,
				      int *recd_from_proc,
				      int contig_access_count, 
				      OMPI_MPI_OFFSET_TYPE min_st_offset,
				      OMPI_MPI_OFFSET_TYPE fd_size,
				      OMPI_MPI_OFFSET_TYPE *fd_start,
				      OMPI_MPI_OFFSET_TYPE *fd_end,
				      MPI_Aint buftype_extent,
				      int striping_unit, int *aggregator_list){

    int i, p, flat_buf_idx;
    OMPI_MPI_OFFSET_TYPE flat_buf_sz, size_in_buf, buf_incr, size;
    int  n_buftypes, ret=OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE off, len, rem_len, user_buf_idx;
    
    unsigned *curr_from_proc, *done_from_proc, *recv_buf_idx;

    curr_from_proc = (unsigned *) malloc (fh->f_size * sizeof(unsigned));
    done_from_proc = (unsigned *) malloc (fh->f_size * sizeof(unsigned));
    recv_buf_idx = (unsigned *) malloc (fh->f_size * sizeof(unsigned));
    
   
    
    for (i=0; i < fh->f_size; i++) {
	recv_buf_idx[i] = curr_from_proc[i] = 0;
	done_from_proc[i] = recd_from_proc[i];
    }
    
    user_buf_idx = flat_buf->indices[0];
    flat_buf_idx = 0;
    n_buftypes = 0;
    flat_buf_sz = flat_buf->blocklens[0];
    
    for (i=0; i<contig_access_count; i++) { 
	
	off     = (OMPI_MPI_OFFSET_TYPE)offset_length[i].iov_base;
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


	    if (recv_buf_idx[p] < recv_size[p]) {
		if (curr_from_proc[p]+len > done_from_proc[p]) {
		    if (done_from_proc[p] > curr_from_proc[p]) {
			size = OMPIO_MIN(curr_from_proc[p] + len - 
					 done_from_proc[p], recv_size[p]-recv_buf_idx[p]);
			buf_incr = done_from_proc[p] - curr_from_proc[p];
			TWO_PHASE_BUF_INCR
		        buf_incr = curr_from_proc[p] + len - done_from_proc[p];
			curr_from_proc[p] = done_from_proc[p] + size;
		        TWO_PHASE_BUF_COPY
		    }
		    else {
			size = OMPIO_MIN(len,recv_size[p]-recv_buf_idx[p]);
			buf_incr = len;
			curr_from_proc[p] += size;
			TWO_PHASE_BUF_COPY
		    }
		}
		else{
		  curr_from_proc[p] += (unsigned) len;
		  buf_incr = len;
		  TWO_PHASE_BUF_INCR
	       }
	    }
	    else{ 
	      buf_incr = len;
	      TWO_PHASE_BUF_INCR
	    }	
	    off  += len;
	    rem_len -= len;
	}
    }
    for (i=0;i< fh->f_size; i++){
      if(recv_size[i]){
	recd_from_proc[i] = curr_from_proc[i];
      }
    }
    
    free(curr_from_proc);
    free(done_from_proc);
    free(recv_buf_idx);
    return ret;
}
    

	



