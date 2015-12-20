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
 * Copyright (c) 2008-2015 University of Houston. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "ompi/mca/sharedfp/sharedfp.h"
#include "ompi/mca/pml/pml.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/datatype/ompi_datatype.h"
#include <stdlib.h>
#include <stdio.h>

#include <unistd.h>
#include "io_ompio.h"

static OMPI_MPI_OFFSET_TYPE get_contiguous_chunk_size (mca_io_ompio_file_t *);


int mca_io_ompio_set_view_internal(mca_io_ompio_file_t *fh,
				   OMPI_MPI_OFFSET_TYPE disp,
				   ompi_datatype_t *etype,
				   ompi_datatype_t *filetype,
				   const char *datarep,
				   ompi_info_t *info)
{

    size_t max_data = 0;
    int i;
    int num_groups = 0;
    contg *contg_groups;

    size_t ftype_size;
    OPAL_PTRDIFF_TYPE ftype_extent, lb, ub;
    ompi_datatype_t *newfiletype;

    if ( NULL != fh->f_etype ) {
        ompi_datatype_destroy (&fh->f_etype);
    }
    if ( NULL != fh->f_filetype ) {
        ompi_datatype_destroy (&fh->f_filetype);
    }
    if ( NULL != fh->f_orig_filetype ) {
        ompi_datatype_destroy (&fh->f_orig_filetype);
    }
    if (NULL != fh->f_decoded_iov) {
        free (fh->f_decoded_iov);
        fh->f_decoded_iov = NULL;
    }

    if (NULL != fh->f_datarep) {
        free (fh->f_datarep);
        fh->f_datarep = NULL;
    }

    /* Reset the flags first */
    fh->f_flags = 0;

    fh->f_flags |= OMPIO_FILE_VIEW_IS_SET;
    fh->f_datarep = strdup (datarep);
    ompi_datatype_duplicate (filetype, &fh->f_orig_filetype );

    opal_datatype_get_extent(&filetype->super, &lb, &ftype_extent);
    opal_datatype_type_size (&filetype->super, &ftype_size);

    if ( etype == filetype                             &&
	 ompi_datatype_is_predefined (filetype )       &&
	 ftype_extent == (OPAL_PTRDIFF_TYPE)ftype_size ){
	ompi_datatype_create_contiguous(MCA_IO_DEFAULT_FILE_VIEW_SIZE,
					&ompi_mpi_byte.dt,
					&newfiletype);
	ompi_datatype_commit (&newfiletype);
    }
    else {
        newfiletype = filetype;
    }



    fh->f_iov_count   = 0;
    fh->f_disp        = disp;
    fh->f_offset      = disp;
    fh->f_total_bytes = 0;
    fh->f_index_in_file_view=0;
    fh->f_position_in_file_view=0;

    ompi_io_ompio_decode_datatype (fh,
                                   newfiletype,
                                   1,
                                   NULL,
                                   &max_data,
                                   &fh->f_decoded_iov,
                                   &fh->f_iov_count);

    opal_datatype_get_extent(&newfiletype->super, &lb, &fh->f_view_extent);
    opal_datatype_type_ub   (&newfiletype->super, &ub);
    opal_datatype_type_size (&etype->super, &fh->f_etype_size);
    opal_datatype_type_size (&newfiletype->super, &fh->f_view_size);
    ompi_datatype_duplicate (etype, &fh->f_etype);
    ompi_datatype_duplicate (newfiletype, &fh->f_filetype);

    fh->f_cc_size = get_contiguous_chunk_size (fh);

    if (opal_datatype_is_contiguous_memory_layout(&etype->super,1)) {
        if (opal_datatype_is_contiguous_memory_layout(&filetype->super,1) &&
	    fh->f_view_extent == (OPAL_PTRDIFF_TYPE)fh->f_view_size ) {
            fh->f_flags |= OMPIO_CONTIGUOUS_FVIEW;
        }
    }

    contg_groups = (contg*) calloc ( 1, fh->f_size * sizeof(contg));
    if (NULL == contg_groups) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for( i = 0; i < fh->f_size; i++){
       contg_groups[i].procs_in_contg_group = (int*)calloc (1,fh->f_size * sizeof(int));
       if(NULL == contg_groups[i].procs_in_contg_group){
          int j;
          opal_output (1, "OUT OF MEMORY\n");
          for(j=0; j<i; j++) {
              free(contg_groups[j].procs_in_contg_group);
          }
          free(contg_groups);
          return OMPI_ERR_OUT_OF_RESOURCE;
       }
    }

    if ( SIMPLE != mca_io_ompio_grouping_option ) {
        if( OMPI_SUCCESS != mca_io_ompio_fview_based_grouping(fh,
                                                          &num_groups,
                                                          contg_groups)){
            opal_output(1, "mca_io_ompio_fview_based_grouping() failed\n");
            free(contg_groups);
            return OMPI_ERROR;
        }
    }
    else {
        if( OMPI_SUCCESS != mca_io_ompio_simple_grouping(fh,
                                                         &num_groups,
                                                         contg_groups)){
            opal_output(1, "mca_io_ompio_simple_grouping() failed\n");
            free(contg_groups);
            return OMPI_ERROR;
        }
    }
    
    
    mca_io_ompio_finalize_initial_grouping(fh,
                                           num_groups,
                                           contg_groups);
    for( i = 0; i < fh->f_size; i++){
       free(contg_groups[i].procs_in_contg_group);
    }
    free(contg_groups);

    if ( etype == filetype                              &&
	 ompi_datatype_is_predefined (filetype )        &&
	 ftype_extent == (OPAL_PTRDIFF_TYPE)ftype_size ){
	ompi_datatype_destroy ( &newfiletype );
    }


    if (OMPI_SUCCESS != mca_fcoll_base_file_select (fh, NULL)) {
        opal_output(1, "mca_fcoll_base_file_select() failed\n");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int mca_io_ompio_file_set_view (ompi_file_t *fp,
                                OMPI_MPI_OFFSET_TYPE disp,
                                ompi_datatype_t *etype,
                                ompi_datatype_t *filetype,
                                const char *datarep,
                                ompi_info_t *info)
{
    int ret=OMPI_SUCCESS;
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;
    mca_io_ompio_file_t *sh;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;

    /* we need to call the internal file set view twice: once for the individual
       file pointer, once for the shared file pointer (if it is existent)
    */
    fh = &data->ompio_fh;
    ret = mca_io_ompio_set_view_internal(fh, disp, etype, filetype, datarep, info);

    if ( NULL != fh->f_sharedfp_data) {
        sh = ((struct mca_sharedfp_base_data_t *)fh->f_sharedfp_data)->sharedfh;
        ret = mca_io_ompio_set_view_internal(sh, disp, etype, filetype, datarep, info);
    }

    return ret;
}

int mca_io_ompio_file_get_view (struct ompi_file_t *fp,
                                OMPI_MPI_OFFSET_TYPE *disp,
                                struct ompi_datatype_t **etype,
                                struct ompi_datatype_t **filetype,
                                char *datarep)
{
    mca_io_ompio_data_t *data;
    mca_io_ompio_file_t *fh;

    data = (mca_io_ompio_data_t *) fp->f_io_selected_data;
    fh = &data->ompio_fh;

    *disp = fh->f_disp;
    ompi_datatype_duplicate (fh->f_etype, etype);
    ompi_datatype_duplicate (fh->f_orig_filetype, filetype);
    strcpy (datarep, fh->f_datarep);

    return OMPI_SUCCESS;
}

OMPI_MPI_OFFSET_TYPE get_contiguous_chunk_size (mca_io_ompio_file_t *fh)
{
    int uniform = 0;
    OMPI_MPI_OFFSET_TYPE avg[3] = {0,0,0};
    OMPI_MPI_OFFSET_TYPE global_avg[3] = {0,0,0};
    int i = 0;

    /* This function does two things: first, it determines the average data chunk
    ** size in the file view for each process and across all processes.
    ** Second, it establishes whether the view across all processes is uniform.
    ** By definition, uniform means:
    ** 1. the file view of each process has the same number of contiguous sections
    ** 2. each section in the file view has exactly the same size
    */

    for (i=0 ; i<(int)fh->f_iov_count ; i++) {
        avg[0] += fh->f_decoded_iov[i].iov_len;
        if (i && 0 == uniform) {
            if (fh->f_decoded_iov[i].iov_len != fh->f_decoded_iov[i-1].iov_len) {
                uniform = 1;
            }
        }
    }
    if ( 0 != fh->f_iov_count ) {
	avg[0] = avg[0]/fh->f_iov_count;
    }
    avg[1] = (OMPI_MPI_OFFSET_TYPE) fh->f_iov_count;
    avg[2] = (OMPI_MPI_OFFSET_TYPE) uniform;

    fh->f_comm->c_coll.coll_allreduce (avg,
                                       global_avg,
                                       3,
                                       OMPI_OFFSET_DATATYPE,
                                       MPI_SUM,
                                       fh->f_comm,
                                       fh->f_comm->c_coll.coll_allreduce_module);
    global_avg[0] = global_avg[0]/fh->f_size;
    global_avg[1] = global_avg[1]/fh->f_size;

#if 0 
    /* Disabling the feature since we are not using it anyway. Saves us one allreduce operation. */
    int global_uniform=0;

    if ( global_avg[0] == avg[0] &&
	 global_avg[1] == avg[1] &&
	 0 == avg[2]             &&
	 0 == global_avg[2] ) {
	uniform = 0;
    }
    else {
	uniform = 1;
    }

    /* second confirmation round to see whether all processes agree
    ** on having a uniform file view or not
    */
    fh->f_comm->c_coll.coll_allreduce (&uniform,
				       &global_uniform,
				       1,
				       MPI_INT,
				       MPI_MAX,
				       fh->f_comm,
				       fh->f_comm->c_coll.coll_allreduce_module);

    if ( 0 == global_uniform  ){
	/* yes, everybody agrees on having a uniform file view */
	fh->f_flags |= OMPIO_UNIFORM_FVIEW;
    }
#endif
    return global_avg[0];
}

int mca_io_ompio_simple_grouping(mca_io_ompio_file_t *fh,
                                 int *num_groups,
                                 contg *contg_groups)
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

int mca_io_ompio_fview_based_grouping(mca_io_ompio_file_t *fh,
                     		      int *num_groups,
				      contg *contg_groups)
{

    int k = 0;
    int p = 0;
    int g = 0;
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
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    end_offsets = (OMPI_MPI_OFFSET_TYPE* )malloc (fh->f_size * sizeof(OMPI_MPI_OFFSET_TYPE));
    if (NULL == end_offsets) {
        opal_output (1, "OUT OF MEMORY\n");
        free(start_offsets_lens);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    
    //Allgather start offsets across processes in a group on aggregator
    fh->f_comm->c_coll.coll_allgather (start_offset_len,
                                       3,
                                       OMPI_OFFSET_DATATYPE,
                                       start_offsets_lens,
                                       3,
                                       OMPI_OFFSET_DATATYPE,
                                       fh->f_comm,
                                       fh->f_comm->c_coll.coll_allgather_module);
    
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
    free (start_offsets_lens);
    free (end_offsets);
 
    return OMPI_SUCCESS;
}

int mca_io_ompio_finalize_initial_grouping(mca_io_ompio_file_t *fh,
		                           int num_groups,
					   contg *contg_groups)
{

    int z = 0;
    int y = 0;

    fh->f_init_num_aggrs = num_groups;
    fh->f_init_aggr_list = (int*)malloc (fh->f_init_num_aggrs * sizeof(int));
    if (NULL == fh->f_init_aggr_list) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for( z = 0 ;z < num_groups; z++){
        for( y = 0; y < contg_groups[z].procs_per_contg_group; y++){
            if ( fh->f_rank == contg_groups[z].procs_in_contg_group[y] ) {
                fh->f_init_procs_per_group = contg_groups[z].procs_per_contg_group;
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

