/* -*- Mode: C; c-basic-offset:4 ; -*- */
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
 * Copyright (c) 2008-2016 University of Houston. All rights reserved.
 * Copyright (c) 2011-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Inria.  All rights reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/topo/topo.h"
#include "ompi/mca/fcoll/base/fcoll_base_coll_array.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/info/info.h"
#include "ompi/request/request.h"

#include <math.h>
#include <unistd.h>

#include "common_ompio_aggregators.h"

/*
** This file contains all the functionality related to determing the number of aggregators
** and the list of aggregators.
**
** The first group functions determines the number of aggregators based on various characteristics
** 
** 1. simple_grouping:aA simple heuristic based on the amount of data written and size of 
**    of the temporary buffer used by aggregator processes
** 2. fview_based_grouping: analysis the fileview to detect regular patterns
** 3. cart_based_grouping: uses a cartesian communicator to derive certain (probable) properties
**    of the access pattern
*/

int mca_common_ompio_simple_grouping(mca_io_ompio_file_t *fh,
                                     int *num_groups,
                                     mca_common_ompio_contg *contg_groups)
{
    size_t stripe_size = (size_t) fh->f_stripe_size;
    int group_size  = 0;
    int k=0, p=0, g=0;
    int total_procs = 0; 

    if ( 0 < fh->f_stripe_size ) {
        stripe_size = OMPIO_DEFAULT_STRIPE_SIZE;
    }

    if ( 0 != fh->f_cc_size && stripe_size > fh->f_cc_size ) {
        group_size  = (((int)stripe_size/(int)fh->f_cc_size) > fh->f_size ) ? fh->f_size : ((int)stripe_size/(int)fh->f_cc_size);
        *num_groups = fh->f_size / group_size;
    }
    else if ( fh->f_cc_size <= OMPIO_CONTG_FACTOR * stripe_size) {
        *num_groups = fh->f_size/OMPIO_CONTG_FACTOR > 0 ? (fh->f_size/OMPIO_CONTG_FACTOR) : 1 ;
        group_size  = OMPIO_CONTG_FACTOR;
    } 
    else {
        *num_groups = fh->f_size;
        group_size  = 1;
    }

    for ( k=0, p=0; p<*num_groups; p++ ) {
        if ( p == (*num_groups - 1) ) {
            contg_groups[p].procs_per_contg_group = fh->f_size - total_procs;
        }
        else {
            contg_groups[p].procs_per_contg_group = group_size;
            total_procs +=group_size;
        }
        for ( g=0; g<contg_groups[p].procs_per_contg_group; g++ ) {
            contg_groups[p].procs_in_contg_group[g] = k;
            k++;
        }
    }
    return OMPI_SUCCESS;
}

int mca_common_ompio_fview_based_grouping(mca_io_ompio_file_t *fh,
                     		      int *num_groups,
				      mca_common_ompio_contg *contg_groups)
{

    int k = 0;
    int p = 0;
    int g = 0;
    int ret = OMPI_SUCCESS;
    OMPI_MPI_OFFSET_TYPE start_offset_len[3] = {0};
    OMPI_MPI_OFFSET_TYPE *end_offsets = NULL;
    OMPI_MPI_OFFSET_TYPE *start_offsets_lens = NULL;

    //Store start offset,length and corresponding rank in an array
    if(NULL == fh->f_decoded_iov){
      start_offset_len[0] = 0;
      start_offset_len[1] = 0;
    }
    else{
       start_offset_len[0] = (OMPI_MPI_OFFSET_TYPE) fh->f_decoded_iov[0].iov_base;
       start_offset_len[1] = fh->f_decoded_iov[0].iov_len;
    }
    start_offset_len[2] = fh->f_rank;

    start_offsets_lens = (OMPI_MPI_OFFSET_TYPE* )malloc (3 * fh->f_size * sizeof(OMPI_MPI_OFFSET_TYPE));
    if (NULL == start_offsets_lens) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }
    end_offsets = (OMPI_MPI_OFFSET_TYPE* )malloc (fh->f_size * sizeof(OMPI_MPI_OFFSET_TYPE));
    if (NULL == end_offsets) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }
    
    //Allgather start offsets across processes in a group on aggregator
    ret = fh->f_comm->c_coll->coll_allgather (start_offset_len,
                                             3,
                                             OMPI_OFFSET_DATATYPE,
                                             start_offsets_lens,
                                             3,
                                             OMPI_OFFSET_DATATYPE,
                                             fh->f_comm,
                                             fh->f_comm->c_coll->coll_allgather_module);
    if ( OMPI_SUCCESS != ret ) {
        goto exit;
    }


    //Calculate contg chunk size and contg subgroups
    for( k = 0 ; k < fh->f_size; k++){
        end_offsets[k] = start_offsets_lens[3*k] + start_offsets_lens[3*k+1];
        contg_groups[k].contg_chunk_size = 0;
    }
    k = 0;
    while( k < fh->f_size){
        if( k == 0){
            contg_groups[p].contg_chunk_size += start_offsets_lens[3*k+1];
            contg_groups[p].procs_in_contg_group[g] = start_offsets_lens[3*k + 2];
            g++;
            contg_groups[p].procs_per_contg_group = g;
            k++;
        }
        else if( start_offsets_lens[3*k] == end_offsets[k - 1] ){
            contg_groups[p].contg_chunk_size += start_offsets_lens[3*k+1];
            contg_groups[p].procs_in_contg_group[g] = start_offsets_lens[3*k + 2];
            g++;
            contg_groups[p].procs_per_contg_group = g;
            k++;
        }
        else{
            p++;
            g = 0;
            contg_groups[p].contg_chunk_size += start_offsets_lens[3*k+1];
            contg_groups[p].procs_in_contg_group[g] = start_offsets_lens[3*k + 2];
            g++;
            contg_groups[p].procs_per_contg_group = g;
            k++;
        }
    }
    
    *num_groups = p+1;
    ret = OMPI_SUCCESS;

exit:
    if (NULL != start_offsets_lens) {
        free (start_offsets_lens);
    }
    if (NULL != end_offsets) {
        free(end_offsets);
    }
 
    return ret;
}

int mca_common_ompio_cart_based_grouping(mca_io_ompio_file_t *ompio_fh, 
                                         int *num_groups,
                                         mca_common_ompio_contg *contg_groups)
{
    int k = 0;
    int j = 0;
    int n = 0;
    int ret = OMPI_SUCCESS, tmp_rank = 0;
    int coords_tmp[2] = { 0 };

    mca_common_ompio_cart_topo_components cart_topo;
    memset (&cart_topo, 0, sizeof(mca_common_ompio_cart_topo_components)); 

    ret = ompio_fh->f_comm->c_topo->topo.cart.cartdim_get(ompio_fh->f_comm, &cart_topo.ndims);
    if (OMPI_SUCCESS != ret ) {
        goto exit;
    }

    cart_topo.dims = (int*)malloc (cart_topo.ndims * sizeof(int));
    if (NULL == cart_topo.dims) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }
    cart_topo.periods = (int*)malloc (cart_topo.ndims * sizeof(int));
    if (NULL == cart_topo.periods) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }
    cart_topo.coords = (int*)malloc (cart_topo.ndims * sizeof(int));
    if (NULL == cart_topo.coords) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    ret = ompio_fh->f_comm->c_topo->topo.cart.cart_get(ompio_fh->f_comm,
                                                       cart_topo.ndims,
                                                       cart_topo.dims,
                                                       cart_topo.periods,
                                                       cart_topo.coords);
    if ( OMPI_SUCCESS != ret ) {
        opal_output (1, "mca_common_ompio_cart_based_grouping: Error in cart_get \n");
        goto exit;
    }

    ompio_fh->f_init_procs_per_group = cart_topo.dims[1]; //number of elements per row
    ompio_fh->f_init_num_aggrs = cart_topo.dims[0];  //number of rows

    //Make an initial list of potential aggregators
    ompio_fh->f_init_aggr_list = (int *) malloc (ompio_fh->f_init_num_aggrs * sizeof(int));
    if (NULL == ompio_fh->f_init_aggr_list) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    for(k = 0; k < cart_topo.dims[0]; k++){
        coords_tmp[0] = k;
        coords_tmp[1] = k * cart_topo.dims[1];
        ret = ompio_fh->f_comm->c_topo->topo.cart.cart_rank (ompio_fh->f_comm,coords_tmp,&tmp_rank);
        if ( OMPI_SUCCESS != ret ) {
            opal_output (1, "mca_common_ompio_cart_based_grouping: Error in cart_rank\n");
            goto exit;
        }
        ompio_fh->f_init_aggr_list[k] = tmp_rank;
    }

    //Initial Grouping
    ompio_fh->f_init_procs_in_group = (int*)malloc (ompio_fh->f_init_procs_per_group * sizeof(int));
    if (NULL == ompio_fh->f_init_procs_in_group) {
        opal_output (1, "OUT OF MEMORY\n");
        free (ompio_fh->f_init_aggr_list );
        ompio_fh->f_init_aggr_list=NULL;
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    for (j=0 ; j< ompio_fh->f_size ; j++) {
        ompio_fh->f_comm->c_topo->topo.cart.cart_coords (ompio_fh->f_comm, j, cart_topo.ndims, coords_tmp);
	if (coords_tmp[0]  == cart_topo.coords[0]) {
           if ((coords_tmp[1]/ompio_fh->f_init_procs_per_group) ==
	       (cart_topo.coords[1]/ompio_fh->f_init_procs_per_group)) {
	       ompio_fh->f_init_procs_in_group[n] = j;
	       n++;
	   }
        }
    }

    /*print original group */
    /*printf("RANK%d Initial distribution \n",ompio_fh->f_rank);
    for(k = 0; k < ompio_fh->f_init_procs_per_group; k++){
       printf("%d,", ompio_fh->f_init_procs_in_group[k]);
    }
    printf("\n");*/

exit:
    if (NULL != cart_topo.dims) {
       free (cart_topo.dims);
       cart_topo.dims = NULL;
    }
    if (NULL != cart_topo.periods) {
       free (cart_topo.periods);
       cart_topo.periods = NULL;
    }
    if (NULL != cart_topo.coords) {
       free (cart_topo.coords);
       cart_topo.coords = NULL;
    }

    return ret;
}



int mca_common_ompio_finalize_initial_grouping(mca_io_ompio_file_t *fh,
		                           int num_groups,
					   mca_common_ompio_contg *contg_groups)
{

    int z = 0;
    int y = 0;

    fh->f_init_num_aggrs = num_groups;
    if (NULL != fh->f_init_aggr_list) {
        free(fh->f_init_aggr_list);
    }
    fh->f_init_aggr_list = (int*)malloc (fh->f_init_num_aggrs * sizeof(int));
    if (NULL == fh->f_init_aggr_list) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for( z = 0 ;z < num_groups; z++){
        for( y = 0; y < contg_groups[z].procs_per_contg_group; y++){
            if ( fh->f_rank == contg_groups[z].procs_in_contg_group[y] ) {
                fh->f_init_procs_per_group = contg_groups[z].procs_per_contg_group;
                if (NULL != fh->f_init_procs_in_group) {
                    free(fh->f_init_procs_in_group);
                }
                fh->f_init_procs_in_group = (int*)malloc (fh->f_init_procs_per_group * sizeof(int));
                if (NULL == fh->f_init_procs_in_group) {
                    opal_output (1, "OUT OF MEMORY\n");
                    return OMPI_ERR_OUT_OF_RESOURCE;
                }
                memcpy ( fh->f_init_procs_in_group, contg_groups[z].procs_in_contg_group, 
                         contg_groups[z].procs_per_contg_group * sizeof (int));
                
            }
        }
    }

    for( z = 0 ;z < num_groups; z++){
        fh->f_init_aggr_list[z] = contg_groups[z].procs_in_contg_group[0];
    }


   return OMPI_SUCCESS;
}

/*****************************************************************************************************/
/*****************************************************************************************************/
/*****************************************************************************************************/
/* 
** This function is called by the collective I/O operations to determine the final number
** of aggregators.
*/

int mca_common_ompio_set_aggregator_props (struct mca_io_ompio_file_t *fh,
                                       int num_aggregators,
                                       size_t bytes_per_proc)
{
    int j,procs_per_group = 0;
    int ret=OMPI_SUCCESS;

    /*If only one process used, no need to do aggregator selection!*/
    if (fh->f_size == 1){
	num_aggregators = 1;
    }

    fh->f_flags |= OMPIO_AGGREGATOR_IS_SET;

    if (-1 == num_aggregators) {

        if ( SIMPLE == OMPIO_MCA_GET(fh, grouping_option) ||
             NO_REFINEMENT == OMPIO_MCA_GET(fh, grouping_option) ) {
            fh->f_aggregator_index = 0;
            fh->f_final_num_aggrs  = fh->f_init_num_aggrs;
            fh->f_procs_per_group  = fh->f_init_procs_per_group;

            fh->f_procs_in_group = (int*)malloc (fh->f_procs_per_group * sizeof(int));
            if (NULL == fh->f_procs_in_group) {
                opal_output (1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            for (j=0 ; j<fh->f_procs_per_group ; j++) {
                fh->f_procs_in_group[j] = fh->f_init_procs_in_group[j];
            }
        }
        else {
            ret = mca_common_ompio_create_groups(fh,bytes_per_proc);
        }
        return ret;
    }

    /* Forced number of aggregators
    ** calculate the offset at which each group of processes will start 
    */
    procs_per_group = ceil ((float)fh->f_size/num_aggregators);

    /* calculate the number of processes in the local group */
    if (fh->f_size/procs_per_group != fh->f_rank/procs_per_group) {
        fh->f_procs_per_group = procs_per_group;
    }
    else {
        fh->f_procs_per_group = fh->f_size%procs_per_group;
    }

    fh->f_procs_in_group = (int*)malloc (fh->f_procs_per_group * sizeof(int));
    if (NULL == fh->f_procs_in_group) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (j=0 ; j<fh->f_procs_per_group ; j++) {
        fh->f_procs_in_group[j] = (fh->f_rank/procs_per_group) * procs_per_group + j;
    }

    fh->f_aggregator_index = 0;
    fh->f_final_num_aggrs  = num_aggregators;

    return OMPI_SUCCESS;
 }



int mca_common_ompio_create_groups(mca_io_ompio_file_t *fh,
		               size_t bytes_per_proc)
{

    int is_aggregator = 0;
    int final_aggr = 0;
    int final_num_aggrs = 0;
    int ret = OMPI_SUCCESS, ompio_grouping_flag = 0;

    int *decision_list = NULL;

    OMPI_MPI_OFFSET_TYPE *start_offsets_lens = NULL;
    OMPI_MPI_OFFSET_TYPE *end_offsets = NULL;
    OMPI_MPI_OFFSET_TYPE bytes_per_group = 0;
    OMPI_MPI_OFFSET_TYPE *aggr_bytes_per_group = NULL;

    ret = mca_common_ompio_prepare_to_group(fh,
                                        &start_offsets_lens,
                                        &end_offsets,
                                        &aggr_bytes_per_group,
                                        &bytes_per_group,
                                        &decision_list,
                                        bytes_per_proc,
                                        &is_aggregator,
                                        &ompio_grouping_flag);
    if ( OMPI_SUCCESS != ret ) {
        opal_output (1, "mca_common_ompio_create_groups: error in mca_common_ompio_prepare_to_group\n");
        goto exit;
    }

    switch(ompio_grouping_flag){

        case OMPIO_SPLIT:
            ret = mca_common_ompio_split_initial_groups(fh,
                                                    start_offsets_lens,
                                                    end_offsets,
                                                    bytes_per_group);
        break;

        case OMPIO_MERGE:
            ret = mca_common_ompio_merge_initial_groups(fh,
                                                    aggr_bytes_per_group,
                                                    decision_list,
                                                    is_aggregator);
            break;
            
        case  OMPIO_RETAIN:

            ret = mca_common_ompio_retain_initial_groups(fh);

        break;


    }
    if ( OMPI_SUCCESS != ret ) {
        opal_output (1, "mca_common_ompio_create_groups: error in subroutine called within switch statement\n");
        goto exit;
    }
    
    //Set aggregator index
    fh->f_aggregator_index = 0;

    //Calculate final number of aggregators
    if(fh->f_rank == fh->f_procs_in_group[fh->f_aggregator_index]){
	   final_aggr = 1;
    }
    ret = fh->f_comm->c_coll->coll_allreduce (&final_aggr,
                                             &final_num_aggrs,
                                             1,
                                             MPI_INT,
                                             MPI_SUM,
                                             fh->f_comm,
                                             fh->f_comm->c_coll->coll_allreduce_module);
    if ( OMPI_SUCCESS != ret ) {
        opal_output (1, "mca_common_ompio_create_groups: error in allreduce\n");
    }
    
    //Set final number of aggregators in file handle
    fh->f_final_num_aggrs = final_num_aggrs;

exit:

    if (NULL != start_offsets_lens) {
        free (start_offsets_lens);
    }
    if (NULL != end_offsets) {
        free (end_offsets);
    }
    if(NULL != aggr_bytes_per_group){
      free(aggr_bytes_per_group);
    }
    if( NULL != decision_list){
      free(decision_list);
    }


   return OMPI_SUCCESS;
}

int mca_common_ompio_merge_initial_groups(mca_io_ompio_file_t *fh,
		                      OMPI_MPI_OFFSET_TYPE *aggr_bytes_per_group,
				      int *decision_list,
	                              int is_aggregator){

    OMPI_MPI_OFFSET_TYPE sum_bytes = 0;
    MPI_Request *sendreqs = NULL;

    int start = 0;
    int end = 0;
    int i = 0;
    int j = 0;
    int r  = 0;

    int merge_pair_flag = 4;
    int first_merge_flag = 4;
    int *merge_aggrs = NULL;
    int is_new_aggregator= 0;
    int ret = OMPI_SUCCESS;

    if(is_aggregator){
        i = 0;
	sum_bytes = 0;
        //go through the decision list
	//Find the aggregators that could merge

	while(i < fh->f_init_num_aggrs){
	    while(1){
	        if( i >= fh->f_init_num_aggrs){
	            break;
	        }
	        else if((decision_list[i] == OMPIO_MERGE) &&
	                (sum_bytes <= OMPIO_MCA_GET(fh, bytes_per_agg))){
	            sum_bytes = sum_bytes + aggr_bytes_per_group[i];
	            decision_list[i] = merge_pair_flag;
	            i++;
	        }
	        else if((decision_list[i] == OMPIO_MERGE) &&
	                (sum_bytes >= OMPIO_MCA_GET(fh, bytes_per_agg)) ){
	           if(decision_list[i+1] == OMPIO_MERGE){
	               merge_pair_flag++;
	               decision_list[i] = merge_pair_flag;
	               sum_bytes = aggr_bytes_per_group[i];
	               i++;
	           }
	           else{
	               decision_list[i] = merge_pair_flag;
	               i++;
	           }
	        }
	        else{
	            i++;
	            if(decision_list[i] == OMPIO_MERGE)
	               merge_pair_flag++;
	            sum_bytes = 0;
	            break;
	        }
	    }
        }

        //Now go through the new edited decision list and
	//make lists of aggregators to merge and number
	//of groups to me merged.
	i = 0;
	j = 0;

	while(i < fh->f_init_num_aggrs){
	   if(decision_list[i] >= first_merge_flag){
	       start = i;
	       while((decision_list[i] >= first_merge_flag) &&
		      (i < fh->f_init_num_aggrs-1)){
	           if(decision_list[i+1] == decision_list[i]){
	               i++;
	           }
	           else{
	               break;
	           }
	           end = i;
	       }
	       merge_aggrs = (int *)malloc((end - start + 1) * sizeof(int));
	       if (NULL == merge_aggrs) {
		  opal_output (1, "OUT OF MEMORY\n");
		  return OMPI_ERR_OUT_OF_RESOURCE;
	       }
	       j = 0;
	       for( j = 0 ; j < end - start + 1; j++){
	           merge_aggrs[j] = fh->f_init_aggr_list[start+j];
	       }
               if(fh->f_rank == merge_aggrs[0])
	          is_new_aggregator = 1;

	       for( j = 0 ; j < end-start+1 ;j++){
	          if(fh->f_rank == merge_aggrs[j]){
	              ret = mca_common_ompio_merge_groups(fh, merge_aggrs,
                                                      end-start+1);
                      if ( OMPI_SUCCESS != ret ) {
                          opal_output (1, "mca_common_ompio_merge_initial_groups: error in mca_common_ompio_merge_groups\n");
                          free ( merge_aggrs );                          
                          return ret;
                      }
		  }
	       }
               if(NULL != merge_aggrs){
	           free(merge_aggrs);
                   merge_aggrs = NULL;
               }

	   }
           i++;
        }

    }//end old aggregators

    //New aggregators communicate new grouping info to the groups
    if(is_new_aggregator){
       sendreqs = (MPI_Request *)malloc ( 2 *fh->f_procs_per_group * sizeof(MPI_Request));
       if (NULL == sendreqs) {
          return OMPI_ERR_OUT_OF_RESOURCE;
       }
       //Communicate grouping info
       for( j = 0 ; j < fh->f_procs_per_group; j++){
	   if (fh->f_procs_in_group[j] == fh->f_rank ) {
	       continue;
	   }
           //new aggregator sends new procs_per_group to all its members
	   ret = MCA_PML_CALL(isend(&fh->f_procs_per_group,
                                    1,
                                    MPI_INT,
                                    fh->f_procs_in_group[j],
                                    OMPIO_PROCS_PER_GROUP_TAG,
                                    MCA_PML_BASE_SEND_STANDARD,
                                    fh->f_comm,
                                    sendreqs + r++));
           if ( OMPI_SUCCESS != ret ) {
               opal_output (1, "mca_common_ompio_merge_initial_groups: error in Isend\n");
               goto exit;
           }
	   //new aggregator sends distribution of process to all its new members
	   ret = MCA_PML_CALL(isend(fh->f_procs_in_group,
                                    fh->f_procs_per_group,
                                    MPI_INT,
                                    fh->f_procs_in_group[j],
                                    OMPIO_PROCS_IN_GROUP_TAG,
                                    MCA_PML_BASE_SEND_STANDARD,
                                    fh->f_comm,
                                    sendreqs + r++));
           if ( OMPI_SUCCESS != ret ) {
               opal_output (1, "mca_common_ompio_merge_initial_groups: error in Isend 2\n");
               goto exit;
           }
           
       }
    }
    else {
	//All non aggregators
	//All processes receive initial process distribution from aggregators
	ret = MCA_PML_CALL(recv(&fh->f_procs_per_group,
                                1,
                                MPI_INT,
                                MPI_ANY_SOURCE,
                                OMPIO_PROCS_PER_GROUP_TAG,
                                fh->f_comm,
                                MPI_STATUS_IGNORE));
        if ( OMPI_SUCCESS != ret ) {
            opal_output (1, "mca_common_ompio_merge_initial_groups: error in Recv\n");
            return ret;
        }
        
	fh->f_procs_in_group = (int*)malloc (fh->f_procs_per_group * sizeof(int));
	if (NULL == fh->f_procs_in_group) {
	    opal_output (1, "OUT OF MEMORY\n");
	    return OMPI_ERR_OUT_OF_RESOURCE;
	}

	ret = MCA_PML_CALL(recv(fh->f_procs_in_group,
                                fh->f_procs_per_group,
                                MPI_INT,
                                MPI_ANY_SOURCE,
                                OMPIO_PROCS_IN_GROUP_TAG,
                                fh->f_comm,
                                MPI_STATUS_IGNORE));
        if ( OMPI_SUCCESS != ret ) {
            opal_output (1, "mca_common_ompio_merge_initial_groups: error in Recv 2\n");
            return ret;
        }

    }
    
    if(is_new_aggregator) {
	ret = ompi_request_wait_all (r, sendreqs, MPI_STATUSES_IGNORE);
    }

exit:
    if (NULL != sendreqs) {
        free(sendreqs);
    }

    return ret;
}

int mca_common_ompio_split_initial_groups(mca_io_ompio_file_t *fh,
		                      OMPI_MPI_OFFSET_TYPE *start_offsets_lens,
				      OMPI_MPI_OFFSET_TYPE *end_offsets,
				      OMPI_MPI_OFFSET_TYPE bytes_per_group){


    int size_new_group = 0;
    int size_old_group = 0;
    int size_last_group = 0;
    int size_smallest_group = 0;
    int num_groups = 0;
    int ret = OMPI_SUCCESS;
    
    OMPI_MPI_OFFSET_TYPE max_cci = 0;
    OMPI_MPI_OFFSET_TYPE min_cci = 0;

    // integer round up
    size_new_group = (int)(OMPIO_MCA_GET(fh, bytes_per_agg) / bytes_per_group + (OMPIO_MCA_GET(fh, bytes_per_agg) % bytes_per_group ? 1u : 0u));
    size_old_group = fh->f_init_procs_per_group;

    ret = mca_common_ompio_split_a_group(fh,
                                     start_offsets_lens,
                                     end_offsets,
                                     size_new_group,
                                     &max_cci,
                                     &min_cci,
                                     &num_groups,
                                     &size_smallest_group);
    if (OMPI_SUCCESS != ret ) {
        opal_output (1, "mca_common_ompio_split_initial_groups: error in mca_common_ompio_split_a_group\n");
        return ret;
    }


    switch( OMPIO_MCA_GET(fh,grouping_option)){
        case DATA_VOLUME:
            //Just use size as returned by split group
            size_last_group = size_smallest_group;
	break;

	case UNIFORM_DISTRIBUTION:
	    if(size_smallest_group <= OMPIO_UNIFORM_DIST_THRESHOLD * size_new_group){
	        //uneven split need to call split again
	        if( size_old_group % num_groups == 0 ){
	           //most even distribution possible
	           size_new_group = size_old_group / num_groups;
	           size_last_group = size_new_group;
	        }
	        else{
	            //merge the last small group with the previous group
	            size_last_group = size_new_group + size_smallest_group;
	        }
	    }
	    else{
	         //Considered uniform
	         size_last_group = size_smallest_group;
	    }
	break;

	case CONTIGUITY:

   	    while(1){
		 if((max_cci < OMPIO_CONTG_THRESHOLD) &&
		    (size_new_group < size_old_group)){

                    size_new_group = (size_new_group + size_old_group ) / 2;
  	            ret = mca_common_ompio_split_a_group(fh,
                                                     start_offsets_lens,
                                                     end_offsets,
                                                     size_new_group,
                                                     &max_cci,
                                                     &min_cci,
                                                     &num_groups,
                                                     &size_smallest_group);
                    if (OMPI_SUCCESS != ret ) {
                        opal_output (1, "mca_common_ompio_split_initial_groups: error in mca_common_ompio_split_a_group 2\n");
                        return ret;
                    }
                 }
                 else{
                     break;
                 }
            }
	    size_last_group = size_smallest_group;
	break;

	case OPTIMIZE_GROUPING:
            //This case is a combination of Data volume, contiguity and uniform distribution
	    while(1){
	         if((max_cci < OMPIO_CONTG_THRESHOLD) &&
	            (size_new_group < size_old_group)){  //can be a better condition
                 //monitor the previous iteration
		 //break if it has not changed.
                     size_new_group = size_new_group + size_old_group;
                     // integer round up
                     size_new_group = size_new_group / 2 + (size_new_group % 2 ? 1 : 0);
		     ret = mca_common_ompio_split_a_group(fh,
                                                      start_offsets_lens,
                                                      end_offsets,
                                                      size_new_group,
                                                      &max_cci,
                                                      &min_cci,
                                                      &num_groups,
                                                      &size_smallest_group);
                    if (OMPI_SUCCESS != ret ) {
                        opal_output (1, "mca_common_ompio_split_initial_groups: error in mca_common_ompio_split_a_group 3\n");
                        return ret;
                    }
		 }
		 else{
		     break;
		 }
	    }

	   if(size_smallest_group <= OMPIO_UNIFORM_DIST_THRESHOLD * size_new_group){
	       //uneven split need to call split again
	       if( size_old_group % num_groups == 0 ){
	           //most even distribution possible
	           size_new_group = size_old_group / num_groups;
		   size_last_group = size_new_group;
	       }
	       else{
	            //merge the last small group with the previous group
	            size_last_group = size_new_group + size_smallest_group;
	       }
	   }
	   else{
	       //Considered uniform
	       size_last_group = size_smallest_group;
	   }

	break;
    }

    ret = mca_common_ompio_finalize_split(fh, size_new_group, size_last_group);

    return ret;
}


int mca_common_ompio_retain_initial_groups(mca_io_ompio_file_t *fh){

    int i = 0;

    fh->f_procs_per_group = fh->f_init_procs_per_group;
    fh->f_procs_in_group = (int*)malloc (fh->f_procs_per_group * sizeof(int));
    if (NULL == fh->f_procs_in_group) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for( i = 0 ; i < fh->f_procs_per_group; i++){
        fh->f_procs_in_group[i] = fh->f_init_procs_in_group[i];
    }


    return OMPI_SUCCESS;
}

int mca_common_ompio_merge_groups(mca_io_ompio_file_t *fh,
		              int *merge_aggrs,
			      int num_merge_aggrs)
{
    int i = 0;
    int *sizes_old_group;
    int ret;
    int *displs = NULL;

    sizes_old_group = (int*)malloc(num_merge_aggrs * sizeof(int));
    if (NULL == sizes_old_group) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }


    displs = (int*)malloc(num_merge_aggrs * sizeof(int));
    if (NULL == displs) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }


    //merge_aggrs[0] is considered the new aggregator
    //New aggregator collects group sizes of the groups to be merged
    ret = fcoll_base_coll_allgather_array (&fh->f_init_procs_per_group,
                                           1,
                                           MPI_INT,
                                           sizes_old_group,
                                           1,
                                           MPI_INT,
                                           0,
                                           merge_aggrs,
                                           num_merge_aggrs,
                                           fh->f_comm);
    
    if ( OMPI_SUCCESS != ret ) {
        goto exit;
    }
    fh->f_procs_per_group = 0;


    for( i = 0; i < num_merge_aggrs; i++){
        fh->f_procs_per_group = fh->f_procs_per_group + sizes_old_group[i];
    }

    displs[0] = 0;
    for(i = 1; i < num_merge_aggrs; i++){
	  displs[i] = displs[i-1] + sizes_old_group[i-1];
    }

    fh->f_procs_in_group = (int*)malloc (fh->f_procs_per_group * sizeof(int));
    if (NULL == fh->f_procs_in_group) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }

    //New aggregator also collects the grouping distribution
    //This is the actual merge
    //use allgatherv array
    ret = fcoll_base_coll_allgatherv_array (fh->f_init_procs_in_group,
                                            fh->f_init_procs_per_group,
                                            MPI_INT,
                                            fh->f_procs_in_group,
                                            sizes_old_group,
                                            displs,
                                            MPI_INT,
                                            0,
                                            merge_aggrs,
                                            num_merge_aggrs,
                                            fh->f_comm);
    
exit:
    if (NULL != displs) {
        free (displs);
    }
    if (NULL != sizes_old_group) {
        free (sizes_old_group);
    }

    return ret;

}



int mca_common_ompio_split_a_group(mca_io_ompio_file_t *fh,
     		             OMPI_MPI_OFFSET_TYPE *start_offsets_lens,
		             OMPI_MPI_OFFSET_TYPE *end_offsets,
		             int size_new_group,
		             OMPI_MPI_OFFSET_TYPE *max_cci,
		             OMPI_MPI_OFFSET_TYPE *min_cci,
		             int *num_groups,
		             int *size_smallest_group)
{

    OMPI_MPI_OFFSET_TYPE *cci = NULL;
    *num_groups = fh->f_init_procs_per_group / size_new_group;
    *size_smallest_group = size_new_group;
    int i = 0;
    int k = 0;
    int flag = 0; //all groups same size
    int size = 0;

    if( fh->f_init_procs_per_group % size_new_group != 0 ){
        *num_groups = *num_groups + 1;
	*size_smallest_group = fh->f_init_procs_per_group % size_new_group;
	flag = 1;
    }

    cci = (OMPI_MPI_OFFSET_TYPE*)malloc(*num_groups * sizeof( OMPI_MPI_OFFSET_TYPE ));
    if (NULL == cci) {
        opal_output(1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    //check contiguity within new groups
    size = size_new_group;
    for( i = 0; i < *num_groups; i++){
         cci[i] = start_offsets_lens[3*size_new_group*i  + 1];
         //if it is the last group check if it is the smallest group
	 if( (i == *num_groups-1) && flag == 1){
             size = *size_smallest_group;
	 }
	 for( k = 0; k < size-1; k++){
	     if( end_offsets[size_new_group* i + k] == start_offsets_lens[3*size_new_group*i + 3*(k+1)] ){
	         cci[i] += start_offsets_lens[3*size_new_group*i + 3*(k + 1) + 1];
	     }
       	 }
     }

     //get min and max cci
     *min_cci = cci[0];
     *max_cci = cci[0];
     for( i = 1 ; i < *num_groups; i++){
         if(cci[i] > *max_cci){
	     *max_cci = cci[i];
	 }
	 else if(cci[i] < *min_cci){
	     *min_cci = cci[i];
	 }
     }

     free (cci);
     return OMPI_SUCCESS;
}

int mca_common_ompio_finalize_split(mca_io_ompio_file_t *fh,
                                  int size_new_group,
                                  int size_last_group)
{
   //based on new group and last group finalize f_procs_per_group and f_procs_in_group

    int i = 0;
    int j = 0;
    int k = 0;

    for( i = 0; i < fh->f_init_procs_per_group ; i++){

        if( fh->f_rank == fh->f_init_procs_in_group[i]){
             if( i >= fh->f_init_procs_per_group - size_last_group ){
	         fh->f_procs_per_group = size_last_group;
	     }
             else{
	         fh->f_procs_per_group = size_new_group;
	     }
        }
    }


    fh->f_procs_in_group = (int*)malloc (fh->f_procs_per_group * sizeof(int));
    if (NULL == fh->f_procs_in_group) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for( i = 0; i < fh->f_init_procs_per_group ; i++){
        if( fh->f_rank == fh->f_init_procs_in_group[i]){
            if( i >= fh->f_init_procs_per_group - size_last_group ){
	       //distribution of last group
	       for( j = 0; j < fh->f_procs_per_group; j++){
	           fh->f_procs_in_group[j] = fh->f_init_procs_in_group[fh->f_init_procs_per_group - size_last_group + j];
	       }
	    }
	    else{
	         //distribute all other groups
		 for( j = 0 ; j < fh->f_init_procs_per_group; j = j + size_new_group){
	             if(i >= j && i < j+size_new_group  ){
                         for( k = 0; k < fh->f_procs_per_group ; k++){
	                    fh->f_procs_in_group[k] = fh->f_init_procs_in_group[j+k];
			 }
		     }
		 }
	    }

        }
    }

    return OMPI_SUCCESS;
}

int mca_common_ompio_prepare_to_group(mca_io_ompio_file_t *fh,
		                  OMPI_MPI_OFFSET_TYPE **start_offsets_lens,
				  OMPI_MPI_OFFSET_TYPE **end_offsets, // need it?
				  OMPI_MPI_OFFSET_TYPE **aggr_bytes_per_group,
				  OMPI_MPI_OFFSET_TYPE *bytes_per_group,
                                  int **decision_list,
		                  size_t bytes_per_proc,
				  int *is_aggregator,
				  int *ompio_grouping_flag)
{

    OMPI_MPI_OFFSET_TYPE start_offset_len[3] = {0};
    OMPI_MPI_OFFSET_TYPE *aggr_bytes_per_group_tmp = NULL;
    OMPI_MPI_OFFSET_TYPE *start_offsets_lens_tmp = NULL;
    OMPI_MPI_OFFSET_TYPE *end_offsets_tmp = NULL;
    int *decision_list_tmp = NULL;

    int i = 0;
    int j = 0;
    int k = 0;
    int merge_count = 0;
    int split_count = 0; //not req?
    int retain_as_is_count = 0; //not req?
    int ret=OMPI_SUCCESS;

    //Store start offset and length in an array //also add bytes per process
    if(NULL == fh->f_decoded_iov){
         start_offset_len[0] = 0;
         start_offset_len[1] = 0;
    }
    else{
         start_offset_len[0] = (OMPI_MPI_OFFSET_TYPE) fh->f_decoded_iov[0].iov_base;
         start_offset_len[1] = fh->f_decoded_iov[0].iov_len;
    }
    start_offset_len[2] = bytes_per_proc;
    start_offsets_lens_tmp = (OMPI_MPI_OFFSET_TYPE* )malloc (3 * fh->f_init_procs_per_group * sizeof(OMPI_MPI_OFFSET_TYPE));
    if (NULL == start_offsets_lens_tmp) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    //Gather start offsets across processes in a group on aggregator
    ret = fcoll_base_coll_allgather_array (start_offset_len,
                                           3,
                                           OMPI_OFFSET_DATATYPE,
                                           start_offsets_lens_tmp,
                                           3,
                                           OMPI_OFFSET_DATATYPE,
                                           0,
                                           fh->f_init_procs_in_group,
                                           fh->f_init_procs_per_group,
                                           fh->f_comm);
    if ( OMPI_SUCCESS != ret ) {
        opal_output (1, "mca_common_ompio_prepare_to_grou[: error in ompi_fcoll_base_coll_allgather_array\n");
        goto exit;
    }
    end_offsets_tmp = (OMPI_MPI_OFFSET_TYPE* )malloc (fh->f_init_procs_per_group * sizeof(OMPI_MPI_OFFSET_TYPE));
    if (NULL == end_offsets_tmp) {
        opal_output (1, "OUT OF MEMORY\n");
        goto exit;
    }
    for( k = 0 ; k < fh->f_init_procs_per_group; k++){
        end_offsets_tmp[k] = start_offsets_lens_tmp[3*k] + start_offsets_lens_tmp[3*k+1];
    }
    //Every process has the total bytes written in its group
    for(j = 0; j < fh->f_init_procs_per_group; j++){
        *bytes_per_group = *bytes_per_group + start_offsets_lens_tmp[3*j+2];
    }

    *start_offsets_lens = &start_offsets_lens_tmp[0];
    *end_offsets = &end_offsets_tmp[0];


    for( j = 0 ; j < fh->f_init_num_aggrs ; j++){
        if(fh->f_rank == fh->f_init_aggr_list[j])
           *is_aggregator = 1;
    }
    //Decide groups going in for a merge or a split
    //Merge only if the groups are consecutive
    if(*is_aggregator == 1){
       aggr_bytes_per_group_tmp = (OMPI_MPI_OFFSET_TYPE*)malloc (fh->f_init_num_aggrs * sizeof(OMPI_MPI_OFFSET_TYPE));
       if (NULL == aggr_bytes_per_group_tmp) {
          opal_output (1, "OUT OF MEMORY\n");
          ret = OMPI_ERR_OUT_OF_RESOURCE;
          goto exit;
       }
    decision_list_tmp = (int* )malloc (fh->f_init_num_aggrs * sizeof(int));
    if (NULL == decision_list_tmp) {
        opal_output (1, "OUT OF MEMORY\n");
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto exit;
    }
    //Communicate bytes per group between all aggregators
    ret = fcoll_base_coll_allgather_array (bytes_per_group,
                                           1,
                                           OMPI_OFFSET_DATATYPE,
                                           aggr_bytes_per_group_tmp,
                                           1,
                                           OMPI_OFFSET_DATATYPE,
                                           0,
                                           fh->f_init_aggr_list,
                                           fh->f_init_num_aggrs,
                                           fh->f_comm);
    if ( OMPI_SUCCESS != ret ) {
        opal_output (1, "mca_common_ompio_prepare_to_grou[: error in ompi_fcoll_base_coll_allgather_array 2\n");
        free(decision_list_tmp);
        goto exit;
    }
    
    for( i = 0; i < fh->f_init_num_aggrs; i++){
       if((size_t)(aggr_bytes_per_group_tmp[i])>
          (size_t)OMPIO_MCA_GET(fh, bytes_per_agg)){
          decision_list_tmp[i] = OMPIO_SPLIT;
          split_count++;
       }
       else if((size_t)(aggr_bytes_per_group_tmp[i])<
               (size_t)OMPIO_MCA_GET(fh,bytes_per_agg)){
            decision_list_tmp[i] = OMPIO_MERGE;
            merge_count++;
       }
       else{
	   decision_list_tmp[i] = OMPIO_RETAIN;
	   retain_as_is_count++;
	   }
    }

    *aggr_bytes_per_group = &aggr_bytes_per_group_tmp[0];
    //Go through the decision list to see if non consecutive
    //processes intend to merge, if yes retain original grouping
    for( i = 0; i < fh->f_init_num_aggrs ; i++){
        if(decision_list_tmp[i] == OMPIO_MERGE){
	    if( (i == 0) &&
	        (decision_list_tmp[i+1] != OMPIO_MERGE)){ //first group
		    decision_list_tmp[i] = OMPIO_RETAIN;
            }
	    else if( (i == fh->f_init_num_aggrs-1) &&
	             (decision_list_tmp[i-1] != OMPIO_MERGE)){

	        decision_list_tmp[i] = OMPIO_RETAIN;
	    }
	    else if(!((decision_list_tmp[i-1] == OMPIO_MERGE) ||
                      (decision_list_tmp[i+1] == OMPIO_MERGE))){

		 decision_list_tmp[i] = OMPIO_RETAIN;
	    }
        }
    }

    //Set the flag as per the decision list
    for( i = 0 ; i < fh->f_init_num_aggrs; i++){
        if((decision_list_tmp[i] == OMPIO_MERGE)&&
	   (fh->f_rank == fh->f_init_aggr_list[i]))
           *ompio_grouping_flag = OMPIO_MERGE;

       	if((decision_list_tmp[i] == OMPIO_SPLIT)&&
	   (fh->f_rank == fh->f_init_aggr_list[i]))
           *ompio_grouping_flag = OMPIO_SPLIT;

	if((decision_list_tmp[i] == OMPIO_RETAIN)&&
	   (fh->f_rank == fh->f_init_aggr_list[i]))
           *ompio_grouping_flag = OMPIO_RETAIN;
    }

    //print decision list of aggregators
    /*printf("RANK%d  : Printing decsion list   : \n",fh->f_rank);
    for( i = 0; i < fh->f_init_num_aggrs; i++){
        if(decision_list_tmp[i] == OMPIO_MERGE)
            printf("MERGE,");
        else if(decision_list_tmp[i] == OMPIO_SPLIT)
            printf("SPLIT, ");
	else if(decision_list_tmp[i] == OMPIO_RETAIN)
	    printf("RETAIN, " );
    }
    printf("\n\n");
    */
    *decision_list = &decision_list_tmp[0];
    }
    //Communicate flag to all group members
    ret = fcoll_base_coll_bcast_array (ompio_grouping_flag,
                                       1,
                                       MPI_INT,
                                       0,
                                       fh->f_init_procs_in_group,
                                       fh->f_init_procs_per_group,
                                       fh->f_comm);   

exit:
    /* Do not free aggr_bytes_per_group_tmp, 
    ** start_offsets_lens_tmp, and end_offsets_tmp
    ** here. The memory is released in the layer above.
    */


    return ret;
}


