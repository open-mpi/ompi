/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2015 University of Houston. All rights reserved.
 * Copyright (c) 2011-2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2012-2013 Inria.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
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
#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/info/info.h"
#include "ompi/request/request.h"

#include <math.h>
#include <unistd.h>

#ifdef HAVE_SYS_STATFS_H
#include <sys/statfs.h> /* or <sys/vfs.h> */
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_MOUNT_H
#include <sys/mount.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#include "io_ompio.h"

mca_io_ompio_print_queue *coll_write_time=NULL;
mca_io_ompio_print_queue *coll_read_time=NULL;


static int mca_io_ompio_create_groups(mca_io_ompio_file_t *fh,
                                      size_t bytes_per_proc);


static int mca_io_ompio_prepare_to_group(mca_io_ompio_file_t *fh,
                                         OMPI_MPI_OFFSET_TYPE **start_offsets_lens,
                                         OMPI_MPI_OFFSET_TYPE **end_offsets,
                                         OMPI_MPI_OFFSET_TYPE **aggr_bytes_per_group,
                                         OMPI_MPI_OFFSET_TYPE *bytes_per_group,
					 int **decision_list,
                                         size_t bytes_per_proc,
                                         int *is_aggregator,
                                         int *ompio_grouping_flag);

static int mca_io_ompio_retain_initial_groups(mca_io_ompio_file_t *fh);


static int mca_io_ompio_split_initial_groups(mca_io_ompio_file_t *fh,
		                             OMPI_MPI_OFFSET_TYPE *start_offsets_lens,
					     OMPI_MPI_OFFSET_TYPE *end_offsets,
					     OMPI_MPI_OFFSET_TYPE bytes_per_group);


static int mca_io_ompio_split_a_group(mca_io_ompio_file_t *fh,
                                      OMPI_MPI_OFFSET_TYPE *start_offsets_lens,
                                      OMPI_MPI_OFFSET_TYPE *end_offsets,
                                      int size_new_group,
                                      OMPI_MPI_OFFSET_TYPE *max_cci,
                                      OMPI_MPI_OFFSET_TYPE *min_cci,
                                      int *num_groups,
                                      int *size_smallest_group);


static int mca_io_ompio_finalize_split(mca_io_ompio_file_t *fh,
                                       int size_new_group,
                                       int size_last_group);

static int mca_io_ompio_merge_initial_groups(mca_io_ompio_file_t *fh,
		                             OMPI_MPI_OFFSET_TYPE *aggr_bytes_per_group,
					     int *decision_list,
					     int is_aggregator);

static int mca_io_ompio_merge_groups(mca_io_ompio_file_t *fh,
                                     int *merge_aggrs,
                                     int num_merge_aggrs);


int ompi_io_ompio_set_file_defaults (mca_io_ompio_file_t *fh)
{

   if (NULL != fh) {
        ompi_datatype_t *types[2];
        int blocklen[2] = {1, 1};
        OPAL_PTRDIFF_TYPE d[2], base;
        int i;

        fh->f_io_array = NULL;
        fh->f_perm = OMPIO_PERM_NULL;
        fh->f_flags = 0;
        fh->f_bytes_per_agg = mca_io_ompio_bytes_per_agg;
        fh->f_datarep = strdup ("native");

        fh->f_offset = 0;
        fh->f_disp = 0;
        fh->f_position_in_file_view = 0;
        fh->f_index_in_file_view = 0;
        fh->f_total_bytes = 0;

        fh->f_init_procs_per_group = -1;
        fh->f_init_procs_in_group = NULL;

	fh->f_procs_per_group = -1;
        fh->f_procs_in_group = NULL;

        fh->f_init_num_aggrs = -1;
        fh->f_init_aggr_list = NULL;


        /* Default file View */
        fh->f_iov_type = MPI_DATATYPE_NULL;
        fh->f_stripe_size = mca_io_ompio_bytes_per_agg;
	/*Decoded iovec of the file-view*/
	fh->f_decoded_iov = NULL;
        fh->f_etype = NULL;
        fh->f_filetype = NULL;
        fh->f_orig_filetype = NULL;

	mca_io_ompio_set_view_internal(fh,
				       0,
				       &ompi_mpi_byte.dt,
				       &ompi_mpi_byte.dt,
                                       "native",
				       fh->f_info);


	/*Create a derived datatype for the created iovec */
	types[0] = &ompi_mpi_long.dt;
        types[1] = &ompi_mpi_long.dt;

        d[0] = (OPAL_PTRDIFF_TYPE) fh->f_decoded_iov;
        d[1] = (OPAL_PTRDIFF_TYPE) &fh->f_decoded_iov[0].iov_len;

        base = d[0];
        for (i=0 ; i<2 ; i++) {
            d[i] -= base;
        }

        ompi_datatype_create_struct (2,
                                     blocklen,
                                     d,
                                     types,
                                     &fh->f_iov_type);
        ompi_datatype_commit (&fh->f_iov_type);

        return OMPI_SUCCESS;
    }
    else {
        return OMPI_ERROR;
    }
}

int ompi_io_ompio_generate_current_file_view (struct mca_io_ompio_file_t *fh,
                                              size_t max_data,
                                              struct iovec **f_iov,
                                              int *iov_count)
{

    struct iovec *iov = NULL;
    size_t bytes_to_write;
    size_t sum_previous_counts = 0;
    int j, k;
    int block = 1;

   /* allocate an initial iovec, will grow if needed */
    iov = (struct iovec *) calloc
        (OMPIO_IOVEC_INITIAL_SIZE, sizeof (struct iovec));
    if (NULL == iov) {
        opal_output(1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    sum_previous_counts = fh->f_position_in_file_view;
    j = fh->f_index_in_file_view;
    bytes_to_write = max_data;
    k = 0;

    while (bytes_to_write) {
        OPAL_PTRDIFF_TYPE disp;
        /* reallocate if needed */
        if (OMPIO_IOVEC_INITIAL_SIZE*block <= k) {
            block ++;
            iov = (struct iovec *)realloc
                (iov, OMPIO_IOVEC_INITIAL_SIZE *block *sizeof(struct iovec));
            if (NULL == iov) {
                opal_output(1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }

        if (fh->f_decoded_iov[j].iov_len -
            (fh->f_total_bytes - sum_previous_counts) <= 0) {
            sum_previous_counts += fh->f_decoded_iov[j].iov_len;
            j = j + 1;
            if (j == (int)fh->f_iov_count) {
                j = 0;
                sum_previous_counts = 0;
                fh->f_offset += fh->f_view_extent;
                fh->f_position_in_file_view = sum_previous_counts;
                fh->f_index_in_file_view = j;
                fh->f_total_bytes = 0;
            }
        }

        disp = (OPAL_PTRDIFF_TYPE)(fh->f_decoded_iov[j].iov_base) +
            (fh->f_total_bytes - sum_previous_counts);
        iov[k].iov_base = (IOVBASE_TYPE *)(intptr_t)(disp + fh->f_offset);

        if ((fh->f_decoded_iov[j].iov_len -
             (fh->f_total_bytes - sum_previous_counts))
            >= bytes_to_write) {
            iov[k].iov_len = bytes_to_write;
        }
        else {
            iov[k].iov_len =  fh->f_decoded_iov[j].iov_len -
                (fh->f_total_bytes - sum_previous_counts);
        }

        fh->f_total_bytes += iov[k].iov_len;
        bytes_to_write -= iov[k].iov_len;
        k = k + 1;
    }
    fh->f_position_in_file_view = sum_previous_counts;
    fh->f_index_in_file_view = j;
    *iov_count = k;
    *f_iov = iov;

    if (mca_io_ompio_record_offset_info){

	int tot_entries=0, *recvcounts=NULL, *displs=NULL;
	mca_io_ompio_offlen_array_t *per_process=NULL;
	mca_io_ompio_offlen_array_t  *all_process=NULL;
	int *sorted=NULL, *column_list=NULL, *values=NULL;
	int *row_index=NULL, i=0, l=0, m=0;
	int column_index=0, r_index=0;
	int blocklen[3] = {1, 1, 1};
	OPAL_PTRDIFF_TYPE d[3], base;
	ompi_datatype_t *types[3];
	ompi_datatype_t *io_array_type=MPI_DATATYPE_NULL;
	int **adj_matrix=NULL;
	FILE *fp;


        recvcounts = (int *) malloc (fh->f_size * sizeof(int));
        if (NULL == recvcounts){
            return OMPI_ERR_OUT_OF_RESOURCE;
	}
        displs = (int *) malloc (fh->f_size * sizeof(int));
        if (NULL == displs){
            free(recvcounts);
            return OMPI_ERR_OUT_OF_RESOURCE;
	}

        fh->f_comm->c_coll.coll_gather (&k,
                                        1,
                                        MPI_INT,
                                        recvcounts,
                                        1,
					MPI_INT,
                                        OMPIO_ROOT,
                                        fh->f_comm,
                                        fh->f_comm->c_coll.coll_gather_module);

        per_process = (mca_io_ompio_offlen_array_t *)
	    malloc (k * sizeof(mca_io_ompio_offlen_array_t));
	if (NULL == per_process){
            opal_output(1,"Error while allocating per process!\n");
            free(recvcounts);
            free(displs);
            return  OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (i=0;i<k;i++){
            per_process[i].offset =
                (OMPI_MPI_OFFSET_TYPE)(intptr_t)iov[i].iov_base;
            per_process[i].length =
                (MPI_Aint)iov[i].iov_len;
            per_process[i].process_id = fh->f_rank;
        }

	types[0] = &ompi_mpi_long.dt;
        types[1] = &ompi_mpi_long.dt;
        types[2] = &ompi_mpi_int.dt;

	d[0] = (OPAL_PTRDIFF_TYPE)&per_process[0];
        d[1] = (OPAL_PTRDIFF_TYPE)&per_process[0].length;
        d[2] = (OPAL_PTRDIFF_TYPE)&per_process[0].process_id;
        base = d[0];
        for (i=0;i<3;i++){
            d[i] -= base;
        }
        ompi_datatype_create_struct (3,
                                     blocklen,
                                     d,
                                     types,
                                     &io_array_type);
        ompi_datatype_commit (&io_array_type);

	if (OMPIO_ROOT == fh->f_rank){
            tot_entries = recvcounts[0];
            displs[0] = 0;
            for(i=1;i<fh->f_size;i++){
                displs[i] = displs[i-1] + recvcounts[i-1];
                tot_entries += recvcounts[i];
            }
            all_process = (mca_io_ompio_offlen_array_t *)
                malloc (tot_entries * sizeof(mca_io_ompio_offlen_array_t));
            if (NULL == all_process){
                opal_output(1,"Error while allocating per process!\n");
                free(per_process);
                free(recvcounts);
                free(displs);
                return  OMPI_ERR_OUT_OF_RESOURCE;
            }

            sorted = (int *) malloc
                (tot_entries * sizeof(int));
            if (NULL == sorted){
                opal_output(1,"Error while allocating per process!\n");
                free(all_process);
                free(per_process);
                free(recvcounts);
                free(displs);
                return  OMPI_ERR_OUT_OF_RESOURCE;
            }

            adj_matrix = (int **) malloc (fh->f_size *
                                          sizeof(int *));
            if (NULL == adj_matrix) {
                opal_output(1,"Error while allocating per process!\n");
                free(sorted);
                free(all_process);
                free(per_process);
                free(recvcounts);
                free(displs);
                return  OMPI_ERR_OUT_OF_RESOURCE;
            }
            for (i=0;i<fh->f_size;i++){
                adj_matrix[i] = (int *) malloc (fh->f_size *
                                                sizeof (int ));
                if (NULL == adj_matrix[i]) {
                    for (j=0; j<i; j++) {
                        free(adj_matrix[j]);
                    }
                    free(adj_matrix);
                    free(sorted);
                    free(all_process);
                    free(per_process);
                    free(recvcounts);
                    free(displs);
                    return  OMPI_ERR_OUT_OF_RESOURCE;
                 }
            }

            for (i=0;i<fh->f_size;i++){
                for (j=0;j<fh->f_size;j++){
                    adj_matrix[i][j] = 0;
                }
            }
	}
	fh->f_comm->c_coll.coll_gatherv (per_process,
					 k,
					 io_array_type,
					 all_process,
					 recvcounts,
					 displs,
					 io_array_type,
					 OMPIO_ROOT,
					 fh->f_comm,
					 fh->f_comm->c_coll.coll_gatherv_module);

	ompi_datatype_destroy(&io_array_type);

	if (OMPIO_ROOT == fh->f_rank){

	    ompi_io_ompio_sort_offlen(all_process,
				      tot_entries,
				      sorted);

	    for (i=0;i<tot_entries-1;i++){
		j = all_process[sorted[i]].process_id;
		l = all_process[sorted[i+1]].process_id;
		adj_matrix[j][l] += 1;
		adj_matrix[l][j] += 1;
	    }

	    /*Compress sparse matrix based on CRS to write to file */
	    m = 0;
	    for (i=0; i<fh->f_size; i++){
		for (j=0; j<fh->f_size; j++){
		    if (adj_matrix[i][j] > 0){
			m++;
		    }
		}
	    }
	    fp = fopen("fileview_info.out", "w+");
            if ( NULL == fp ) {
                for (i=0; i<fh->f_size; i++) {
                    free(adj_matrix[i]);
                }
                free(adj_matrix);
                free(sorted);
                free(all_process);
                free(per_process);
                free(recvcounts);
                free(displs);
		return MPI_ERR_OTHER;
            }
	    fprintf(fp,"FILEVIEW\n");
	    column_list = (int *) malloc ( m * sizeof(int));
	    if (NULL == column_list){
		opal_output(1,"Error while allocating column list\n");
                fclose(fp);
                for (i=0; i<fh->f_size; i++) {
                    free(adj_matrix[i]);
                }
                free(adj_matrix);
                free(sorted);
                free(all_process);
                free(per_process);
                free(recvcounts);
                free(displs);
		return OMPI_ERR_OUT_OF_RESOURCE;
	    }
	    values = (int *) malloc ( m * sizeof(int));
	    if (NULL == values){
		opal_output(1,"Error while allocating values list\n");
                fclose(fp);
                for (i=0; i<fh->f_size; i++) {
                    free(adj_matrix[i]);
                }
                free(adj_matrix);
                free(column_list);
                free(sorted);
                free(all_process);
                free(per_process);
                free(recvcounts);
                free(displs);
		return OMPI_ERR_OUT_OF_RESOURCE;
	    }

	    row_index = (int *) malloc ((fh->f_size + 1) *
					sizeof(int));
	    if (NULL == row_index){
		opal_output(1,"Error while allocating row_index list\n");
                fclose(fp);
                for (i=0; i<fh->f_size; i++) {
                    free(adj_matrix[i]);
                }
                free(adj_matrix);
                free(values);
                free(column_list);
                free(sorted);
                free(all_process);
                free(per_process);
                free(recvcounts);
                free(displs);
		return OMPI_ERR_OUT_OF_RESOURCE;
	    }
	    fprintf(fp,"%d %d\n", m, fh->f_size+1);
	    column_index = 0;
	    r_index = 1;
	    row_index[0] = r_index;
	    for (i=0; i<fh->f_size; i++){
		for (j=0; j<fh->f_size; j++){
		    if (adj_matrix[i][j] > 0){
			values[column_index]= adj_matrix[i][j];
			column_list[column_index]= j;
			fprintf(fp,"%d ", column_list[column_index]);
			column_index++;
			r_index++;
		    }

		}
		row_index[i+1]= r_index;
	    }

	    fprintf(fp,"\n");
	    for (i=0; i<m;i++){
		fprintf(fp, "%d ", values[i]);
	    }
	    fprintf(fp, "\n");
	    for (i=0; i< (fh->f_size + 1); i++){
		fprintf(fp, "%d ", row_index[i]);
	    }
	    fprintf(fp, "\n");
	    fclose(fp);

	    if (NULL != recvcounts){
		free(recvcounts);
		recvcounts = NULL;
	    }
	    if (NULL != displs){
		free(displs);
		displs = NULL;
	    }
	    if (NULL != sorted){
		free(sorted);
		sorted = NULL;
	    }
	    if (NULL != per_process){
		free(per_process);
		per_process = NULL;
	    }
	    if (NULL != all_process){
		free(all_process);
		all_process = NULL;
	    }
	    free(column_list);
	    free(values);
	    if (NULL != row_index){
		free(row_index);
		row_index = NULL;
	    }
	    if (NULL != adj_matrix){
		for (i=0;i<fh->f_size;i++){
		    free(adj_matrix[i]);
		}
		free(adj_matrix);
		adj_matrix = NULL;
	    }
	}
    }
    return OMPI_SUCCESS;
}

int ompi_io_ompio_set_explicit_offset (mca_io_ompio_file_t *fh,
                                       OMPI_MPI_OFFSET_TYPE offset)
{
    int i = 0;
    int k = 0;

    if ( fh->f_view_size  > 0 ) {
	/* starting offset of the current copy of the filew view */
	fh->f_offset = (fh->f_view_extent *
			((offset*fh->f_etype_size) / fh->f_view_size)) + fh->f_disp;


	/* number of bytes used within the current copy of the file view */
	fh->f_total_bytes = (offset*fh->f_etype_size) % fh->f_view_size;
	i = fh->f_total_bytes;


	/* Initialize the block id and the starting offset of the current block
	   within the current copy of the file view to zero */
	fh->f_index_in_file_view = 0;
	fh->f_position_in_file_view = 0;

	/* determine block id that the offset is located in and
	   the starting offset of that block */
	k = fh->f_decoded_iov[fh->f_index_in_file_view].iov_len;
	while (i >= k) {
	    fh->f_position_in_file_view = k;
	    fh->f_index_in_file_view++;
	    k += fh->f_decoded_iov[fh->f_index_in_file_view].iov_len;
	}
    }

    return OMPI_SUCCESS;
}

int ompi_io_ompio_decode_datatype (struct mca_io_ompio_file_t *fh,
                                   ompi_datatype_t *datatype,
                                   int count,
                                   const void *buf,
                                   size_t *max_data,
                                   struct iovec **iov,
                                   uint32_t *iovec_count)
{



    opal_convertor_t convertor;
    size_t remaining_length = 0;
    uint32_t i;
    uint32_t temp_count;
    struct iovec *temp_iov=NULL;
    size_t temp_data;


    opal_convertor_clone (fh->f_convertor, &convertor, 0);

    if (OMPI_SUCCESS != opal_convertor_prepare_for_send (&convertor,
                                                         &(datatype->super),
                                                         count,
                                                         buf)) {
        opal_output (1, "Cannot attach the datatype to a convertor\n");
        return OMPI_ERROR;
    }

    if ( 0 == datatype->super.size ) {
	*max_data = 0;
	*iovec_count = 0;
	*iov = NULL;
	return OMPI_SUCCESS;
    }

    remaining_length = count * datatype->super.size;

    temp_count = OMPIO_IOVEC_INITIAL_SIZE;
    temp_iov = (struct iovec*)malloc(temp_count * sizeof(struct iovec));
    if (NULL == temp_iov) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    while (0 == opal_convertor_raw(&convertor,
				   temp_iov,
                                   &temp_count,
                                   &temp_data)) {
#if 0
        printf ("%d: New raw extraction (iovec_count = %d, max_data = %lu)\n",
                fh->f_rank,temp_count, (unsigned long)temp_data);
        for (i = 0; i < temp_count; i++) {
            printf ("%d: \t{%p, %lu}\n",fh->f_rank,
		    temp_iov[i].iov_base,
		    (unsigned long)temp_iov[i].iov_len);
        }
#endif

        *iovec_count = *iovec_count + temp_count;
        *max_data = *max_data + temp_data;
        *iov = (struct iovec *) realloc (*iov, *iovec_count * sizeof(struct iovec));
        if (NULL == *iov) {
            opal_output(1, "OUT OF MEMORY\n");
            free(temp_iov);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (i=0 ; i<temp_count ; i++) {
            (*iov)[i+(*iovec_count-temp_count)].iov_base = temp_iov[i].iov_base;
            (*iov)[i+(*iovec_count-temp_count)].iov_len = temp_iov[i].iov_len;
        }

        remaining_length -= temp_data;
        temp_count = OMPIO_IOVEC_INITIAL_SIZE;
    }
#if 0
    printf ("%d: LAST raw extraction (iovec_count = %d, max_data = %d)\n",
            fh->f_rank,temp_count, temp_data);
    for (i = 0; i < temp_count; i++) {
        printf ("%d: \t offset[%d]: %ld; length[%d]: %ld\n", fh->f_rank,i,temp_iov[i].iov_base, i,temp_iov[i].iov_len);
    }
#endif
    *iovec_count = *iovec_count + temp_count;
    *max_data = *max_data + temp_data;
    if ( temp_count > 0 ) {
	*iov = (struct iovec *) realloc (*iov, *iovec_count * sizeof(struct iovec));
	if (NULL == *iov) {
	    opal_output(1, "OUT OF MEMORY\n");
            free(temp_iov);
	    return OMPI_ERR_OUT_OF_RESOURCE;
	}
    }
    for (i=0 ; i<temp_count ; i++) {
        (*iov)[i+(*iovec_count-temp_count)].iov_base = temp_iov[i].iov_base;
        (*iov)[i+(*iovec_count-temp_count)].iov_len = temp_iov[i].iov_len;
    }

    remaining_length -= temp_data;

#if 0
    if (0 == fh->f_rank) {
        printf ("%d Entries: \n",*iovec_count);
        for (i=0 ; i<*iovec_count ; i++) {
            printf ("\t{%p, %d}\n",
                    (*iov)[i].iov_base,
                    (*iov)[i].iov_len);
        }
    }
#endif
    if (remaining_length != 0) {
        printf( "Not all raw description was been extracted (%lu bytes missing)\n",
                (unsigned long) remaining_length );
    }

    free (temp_iov);

    return OMPI_SUCCESS;
}

int ompi_io_ompio_sort (mca_io_ompio_io_array_t *io_array,
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

int ompi_io_ompio_sort_iovec (struct iovec *iov,
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

    if (0 == num_entries) {
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
                (iov[temp_arr[left]].iov_base > iov[temp_arr[j]].iov_base)) {
                largest = left;
            }
            else {
                largest = j;
            }
            if ((right <= heap_size) &&
                (iov[temp_arr[right]].iov_base >
                 iov[temp_arr[largest]].iov_base)) {
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
                (iov[temp_arr[left]].iov_base >
                 iov[temp_arr[j]].iov_base)) {
                largest = left;
            }
            else {
                largest = j;
            }
            if ((right <= heap_size) &&
                (iov[temp_arr[right]].iov_base >
                 iov[temp_arr[largest]].iov_base)) {
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

int ompi_io_ompio_sort_offlen (mca_io_ompio_offlen_array_t *io_array,
                               int num_entries,
                               int *sorted){

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

int ompi_io_ompio_set_aggregator_props (struct mca_io_ompio_file_t *fh,
                                        int num_aggregators,
                                        size_t bytes_per_proc)
{
    int j,procs_per_group = 0;

    /*If only one process used, no need to do aggregator selection!*/
    if (fh->f_size == 1){
	num_aggregators = 1;
    }

    fh->f_flags |= OMPIO_AGGREGATOR_IS_SET;

    if (-1 == num_aggregators) {
        if ( SIMPLE == mca_io_ompio_grouping_option ||
            NO_REFINEMENT == mca_io_ompio_grouping_option ) {
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
            mca_io_ompio_create_groups(fh,bytes_per_proc);
        }
        return OMPI_SUCCESS;
    }

   //Forced number of aggregators
    /* calculate the offset at which each group of processes will start */
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




int ompi_io_ompio_break_file_view (mca_io_ompio_file_t *fh,
                                   struct iovec *iov,
                                   int count,
                                   int stripe_count,
                                   size_t stripe_size,
                                   struct iovec **broken_iov,
                                   int *broken_count)
{



    struct iovec *temp_iov = NULL;
    int i = 0;
    int k = 0;
    int block = 1;
    int broken = 0;
    size_t remaining = 0;
    size_t temp = 0;
    OPAL_PTRDIFF_TYPE current_offset = 0;


    /* allocate an initial iovec, will grow if needed */
    temp_iov = (struct iovec *) malloc
        (count * sizeof (struct iovec));
    if (NULL == temp_iov) {
        opal_output(1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    while (i < count) {
        if (count*block <= k) {
            block ++;
            temp_iov = (struct iovec *)realloc
                (temp_iov, count * block *sizeof(struct iovec));
            if (NULL == temp_iov) {
                opal_output(1, "OUT OF MEMORY\n");
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
        if (0 == broken) {
            temp = (OPAL_PTRDIFF_TYPE)(iov[i].iov_base)%stripe_size;
            if ((stripe_size-temp) >= iov[i].iov_len) {
                temp_iov[k].iov_base = iov[i].iov_base;
                temp_iov[k].iov_len = iov[i].iov_len;
                i++;
                k++;
            }
            else {
                temp_iov[k].iov_base = iov[i].iov_base;
                temp_iov[k].iov_len = stripe_size-temp;
                current_offset = (OPAL_PTRDIFF_TYPE)(temp_iov[k].iov_base) +
                    temp_iov[k].iov_len;
                remaining = iov[i].iov_len - temp_iov[k].iov_len;
                k++;
                broken ++;
            }
            continue;
        }
        temp = current_offset%stripe_size;
        if ((stripe_size-temp) >= remaining) {
            temp_iov[k].iov_base = (IOVBASE_TYPE *)current_offset;
            temp_iov[k].iov_len = remaining;
            i++;
            k++;
            broken = 0;
            current_offset = 0;
            remaining = 0;
        }
        else {
            temp_iov[k].iov_base = (IOVBASE_TYPE *)current_offset;
            temp_iov[k].iov_len = stripe_size-temp;
            current_offset += temp_iov[k].iov_len;
            remaining -= temp_iov[k].iov_len;
            k++;
            broken ++;
        }
    }
    *broken_iov = temp_iov;
    *broken_count = k;

    return 1;
}
int ompi_io_ompio_distribute_file_view (mca_io_ompio_file_t *fh,
                                        struct iovec *broken_iov,
                                        int broken_count,
                                        int num_aggregators,
                                        size_t stripe_size,
                                        int **fview_count,
                                        struct iovec **iov,
                                        int *count)
{


    int *num_entries;
    int *broken_index;
    int temp = 0;
    int *fview_cnt = NULL;
    int global_fview_count = 0;
    int i = 0;
    int *displs = NULL;
    int rc = OMPI_SUCCESS;
    struct iovec *global_fview = NULL;
    struct iovec **broken = NULL;
    MPI_Request *req=NULL, *sendreq=NULL;


    num_entries = (int *) malloc (sizeof (int) * num_aggregators);
    if (NULL == num_entries) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    broken_index = (int *) malloc (sizeof (int) * num_aggregators);
    if (NULL == broken_index) {
        opal_output (1, "OUT OF MEMORY\n");
        free(num_entries);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    memset (num_entries, 0x0, num_aggregators * sizeof (int));
    memset (broken_index, 0x0, num_aggregators * sizeof (int));

    /* calculate how many entries in the broken iovec belong to each aggregator */
    for (i=0 ; i<broken_count ; i++) {
        temp = (int)((OPAL_PTRDIFF_TYPE)broken_iov[i].iov_base/stripe_size) %
            num_aggregators;
        num_entries [temp] ++;
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        fview_cnt = (int *) malloc (sizeof (int) * fh->f_size);
        if (NULL == fview_cnt) {
            opal_output (1, "OUT OF MEMORY\n");
            free(num_entries);
            free(broken_index);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        req = (MPI_Request *)malloc (fh->f_size * sizeof(MPI_Request));
        if (NULL == req) {
            free(num_entries);
            free(broken_index);
            free(fview_cnt);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    }

    sendreq = (MPI_Request *)malloc (num_aggregators * sizeof(MPI_Request));
    if (NULL == sendreq) {
        free(num_entries);
        free(broken_index);
        if (0 == fh->f_rank%fh->f_aggregator_index) {
            free(fview_cnt);
            free(req);
        }
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* gather at each aggregator how many entires from the broken file view it
       expects from each process */
    if (0 == fh->f_rank%fh->f_aggregator_index) {
        for (i=0; i<fh->f_size ; i++) {
            rc = MCA_PML_CALL(irecv(&fview_cnt[i],
                                    1,
                                    MPI_INT,
                                    i,
                                    OMPIO_TAG_GATHER,
                                    fh->f_comm,
                                    &req[i]));
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }

    for (i=0 ; i<num_aggregators ; i++) {
        rc = MCA_PML_CALL(isend(&num_entries[i],
                                1,
                                MPI_INT,
                                i*fh->f_aggregator_index,
                                OMPIO_TAG_GATHER,
                                MCA_PML_BASE_SEND_STANDARD,
                                fh->f_comm,
                                &sendreq[i]));
        if (OMPI_SUCCESS != rc) {
            goto exit;
        }
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        rc = ompi_request_wait_all (fh->f_size, req, MPI_STATUSES_IGNORE);
        if (OMPI_SUCCESS != rc) {
            goto exit;
        }
    }
    rc = ompi_request_wait_all (num_aggregators, sendreq, MPI_STATUSES_IGNORE);
    if (OMPI_SUCCESS != rc) {
        goto exit;
    }

    /*
    for (i=0 ; i<num_aggregators ; i++) {
        fh->f_comm->c_coll.coll_gather (&num_entries[i],
                                        1,
                                        MPI_INT,
                                        fview_cnt,
                                        1,
                                        MPI_INT,
                                        i*fh->f_aggregator_index,
                                        fh->f_comm,
                                        fh->f_comm->c_coll.coll_gather_module);
    }
    */

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        displs = (int*) malloc (fh->f_size * sizeof (int));
        if (NULL == displs) {
            opal_output (1, "OUT OF MEMORY\n");
            free(fview_cnt);
            free(num_entries);
            free(broken_index);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        displs[0] = 0;
        global_fview_count = fview_cnt[0];
        for (i=1 ; i<fh->f_size ; i++) {
            global_fview_count += fview_cnt[i];
            displs[i] = displs[i-1] + fview_cnt[i-1];
        }

        if (global_fview_count) {
            global_fview = (struct iovec*)malloc (global_fview_count *
                                                  sizeof(struct iovec));
            if (NULL == global_fview) {
                opal_output (1, "OUT OF MEMORY\n");
                free(num_entries);
                free(broken_index);
                free(fview_cnt);
                free(displs);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    broken = (struct iovec**)malloc (num_aggregators * sizeof(struct iovec *));
    if (NULL == broken) {
        opal_output (1, "OUT OF MEMORY\n");
        free(num_entries);
        free(broken_index);
        if (0 == fh->f_rank%fh->f_aggregator_index) {
            free(global_fview);
            free(displs);
            free(fview_cnt);
        }
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    for (i=0 ; i<num_aggregators ; i++) {
        broken[i] = NULL;
        if (0 != num_entries[i]) {
            broken[i] = (struct iovec*) malloc (num_entries[i] *
                                                sizeof (struct iovec));
            if (NULL == broken[i]) {
                int j;
                opal_output (1, "OUT OF MEMORY\n");
                free(num_entries);
                free(broken_index);
                for (j=0; j<i; j++) {
                    free(broken[j]);
                }
                if (0 == fh->f_rank%fh->f_aggregator_index) {
                    free(global_fview);
                    free(displs);
                    free(fview_cnt);
                }
                free(broken);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    for (i=0 ; i<broken_count ; i++) {
        temp = (int)((OPAL_PTRDIFF_TYPE)broken_iov[i].iov_base/stripe_size) %
            num_aggregators;
        broken[temp][broken_index[temp]].iov_base = broken_iov[i].iov_base;
        broken[temp][broken_index[temp]].iov_len = broken_iov[i].iov_len;
        broken_index[temp] ++;
    }
    /*
    for (i=0 ; i<num_aggregators ; i++) {
        int j;
        for (j=0 ; j<num_entries[i] ; j++) {
            printf("%d->%d: OFFSET: %d   LENGTH: %d\n",
                   fh->f_rank,
                   i,
                   broken[i][j].iov_base,
                   broken[i][j].iov_len);
        }
    }
    sleep(1);
    */

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        ptrdiff_t lb, extent;
        rc = ompi_datatype_get_extent(fh->f_iov_type, &lb, &extent);
        if (OMPI_SUCCESS != rc) {
                goto exit;
        }
        for (i=0; i<fh->f_size ; i++) {
            if (fview_cnt[i]) {
                char *ptmp;
                ptmp = ((char *) global_fview) + (extent * displs[i]);
                rc = MCA_PML_CALL(irecv(ptmp,
                                        fview_cnt[i],
                                        fh->f_iov_type,
                                        i,
                                        OMPIO_TAG_GATHERV,
                                        fh->f_comm,
                                        &req[i]));
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }

    for (i=0 ; i<num_aggregators ; i++) {
        if (num_entries[i]) {
            rc = MCA_PML_CALL(isend(broken[i],
                                    num_entries[i],
                                    fh->f_iov_type,
                                    i*fh->f_aggregator_index,
                                    OMPIO_TAG_GATHERV,
                                    MCA_PML_BASE_SEND_STANDARD,
                                    fh->f_comm,
                                    &sendreq[i]));
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        for (i=0; i<fh->f_size ; i++) {
            if (fview_cnt[i]) {
                rc = ompi_request_wait (&req[i], MPI_STATUS_IGNORE);
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }

    for (i=0; i<num_aggregators ; i++) {
        if (num_entries[i]) {
            rc = ompi_request_wait (&sendreq[i], MPI_STATUS_IGNORE);
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }

    /*
    for (i=0 ; i<num_aggregators ; i++) {
        fh->f_comm->c_coll.coll_gatherv (broken[i],
                                         num_entries[i],
                                         fh->f_iov_type,
                                         global_fview,
                                         fview_cnt,
                                         displs,
                                         fh->f_iov_type,
                                         i*fh->f_aggregator_index,
                                         fh->f_comm,
                                         fh->f_comm->c_coll.coll_gatherv_module);
    }
    */
    /*
    for (i=0 ; i<global_fview_count ; i++) {
        printf("%d: OFFSET: %d   LENGTH: %d\n",
               fh->f_rank,
               global_fview[i].iov_base,
               global_fview[i].iov_len);
    }
    */
 exit:

    if (NULL != broken) {
        for (i=0 ; i<num_aggregators ; i++) {
            if (NULL != broken[i]) {
                free (broken[i]);
            }
        }
        free (broken);
    }
    if (NULL != req) {
        free (req);
    }
    if (NULL != sendreq) {
        free (sendreq);
    }
    free (num_entries);
    free (broken_index);
    if (NULL != displs) {
        free (displs);
    }

    *fview_count = fview_cnt;
    *iov = global_fview;
    *count = global_fview_count;

    return rc;
}

int ompi_io_ompio_gather_data (mca_io_ompio_file_t *fh,
                               void *send_buf,
                               size_t total_bytes_sent,
                               int *bytes_sent,
                               struct iovec *broken_iovec,
                               int broken_index,
                               size_t partial,
                               void *global_buf,
                               int *bytes_per_process,
                               int *displs,
                               int num_aggregators,
                               size_t stripe_size)
{
    void **sbuf = NULL;
    size_t bytes_remaining;
    size_t *temp_position;
    size_t part;
    int current;
    int temp = 0;
    int i = 0;
    int rc = OMPI_SUCCESS;
    MPI_Request *req=NULL, *sendreq=NULL;


    current = broken_index;
    part = partial;

    sbuf = (void**) malloc (num_aggregators * sizeof(void *));
    if (NULL == sbuf) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    temp_position = (size_t *) malloc (num_aggregators * sizeof(size_t));
    if (NULL == temp_position) {
        opal_output (1, "OUT OF MEMORY\n");
        free(sbuf);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    memset (temp_position, 0x0, num_aggregators * sizeof (size_t));

    for (i=0 ; i<num_aggregators ; i++) {
        sbuf[i] = NULL;
        if (0 != bytes_sent[i]) {
            sbuf[i] = (void *) malloc (bytes_sent[i]);
            if (NULL == sbuf[i]) {
                opal_output (1, "OUT OF MEMORY\n");
                free(sbuf);
                free(temp_position);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    bytes_remaining = total_bytes_sent;

    while (bytes_remaining) {
        temp = (int)((OPAL_PTRDIFF_TYPE)broken_iovec[current].iov_base/stripe_size)
            % num_aggregators;

        if (part) {
            if (bytes_remaining > part) {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf[temp]+
                                         temp_position[temp]),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                         (total_bytes_sent-bytes_remaining)),
                        part);
                bytes_remaining -= part;
                temp_position[temp] += part;
                part = 0;
                current ++;
            }
            else  {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf[temp]+
                                         temp_position[temp]),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                         (total_bytes_sent-bytes_remaining)),
                        bytes_remaining);
                break;
            }
        }
        else {
            if (bytes_remaining > broken_iovec[current].iov_len) {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf[temp]+
                                         temp_position[temp]),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                         (total_bytes_sent-bytes_remaining)),
                        broken_iovec[current].iov_len);
                bytes_remaining -= broken_iovec[current].iov_len;
                temp_position[temp] += broken_iovec[current].iov_len;
                current ++;
            }
            else {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)sbuf[temp]+
                                         temp_position[temp]),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)send_buf +
                                         (total_bytes_sent-bytes_remaining)),
                        bytes_remaining);
                break;
            }
        }
    }

    sendreq = (MPI_Request *)malloc (num_aggregators * sizeof(MPI_Request));
    if (NULL == sendreq) {
        free(sbuf);
        free(temp_position);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        req = (MPI_Request *)malloc (fh->f_size * sizeof(MPI_Request));
        if (NULL == req) {
            free(sbuf);
            free(temp_position);
            free(sendreq);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = MCA_PML_CALL(irecv((char *)global_buf + displs[i],
                                        bytes_per_process[i],
                                        MPI_BYTE,
                                        i,
                                        OMPIO_TAG_GATHERV,
                                        fh->f_comm,
                                        &req[i]));
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }

    for (i=0 ; i<num_aggregators ; i++) {
        if (bytes_sent[i]) {
            rc = MCA_PML_CALL(isend(sbuf[i],
                                    bytes_sent[i],
                                    MPI_BYTE,
                                    i*fh->f_aggregator_index,
                                    OMPIO_TAG_GATHERV,
                                    MCA_PML_BASE_SEND_STANDARD,
                                    fh->f_comm,
                                    &sendreq[i]));
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = ompi_request_wait (&req[i], MPI_STATUS_IGNORE);
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }
    for (i=0; i<num_aggregators ; i++) {
        if (bytes_sent[i]) {
            rc = ompi_request_wait (&sendreq[i], MPI_STATUS_IGNORE);
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }
    /*
    for (i=0 ; i<num_aggregators ; i++) {
        fh->f_comm->c_coll.coll_gatherv (sbuf[i],
                                         bytes_sent[i],
                                         MPI_BYTE,
                                         global_buf,
                                         bytes_per_process,
                                         displs,
                                         MPI_BYTE,
                                         i*fh->f_aggregator_index,
                                         fh->f_comm,
                                         fh->f_comm->c_coll.coll_gatherv_module);
    }
    */

 exit:
    for (i=0 ; i<num_aggregators ; i++) {
        if (NULL != sbuf[i]) {
            free (sbuf[i]);
        }
    }
    free (sbuf);
    if (NULL != req) {
        free (req);
    }
    if (NULL != sendreq) {
        free (sendreq);
    }
    free (temp_position);

    return rc;
}

int ompi_io_ompio_scatter_data (mca_io_ompio_file_t *fh,
                                void *receive_buf,
                                size_t total_bytes_recv,
                                int *bytes_received,
                                struct iovec *broken_iovec,
                                int broken_index,
                                size_t partial,
                                void *global_buf,
                                int *bytes_per_process,
                                int *displs,
                                int num_aggregators,
                                size_t stripe_size)
{
    void **rbuf = NULL;
    size_t bytes_remaining;
    size_t *temp_position = NULL;
    size_t part;
    int current;
    int temp = 0;
    int i = 0;
    int rc = OMPI_SUCCESS;
    MPI_Request *req=NULL, *recvreq=NULL;


    current = broken_index;
    part = partial;

    rbuf = (void**) malloc (num_aggregators * sizeof(void *));
    if (NULL == rbuf) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    temp_position = (size_t *) malloc (num_aggregators * sizeof(size_t));
    if (NULL == temp_position) {
        opal_output (1, "OUT OF MEMORY\n");
        free(rbuf);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    memset (temp_position, 0x0, num_aggregators * sizeof (size_t));

    for (i=0 ; i<num_aggregators ; i++) {
        rbuf[i] = NULL;
        if (0 != bytes_received[i]) {
            rbuf[i] = (void *) malloc (bytes_received[i]);
            if (NULL == rbuf[i]) {
                int j;
                opal_output (1, "OUT OF MEMORY\n");
                free(temp_position);
                for (j=0; j<i; j++) {
                    free(rbuf[j]);
                }
                free(rbuf);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        }
    }

    recvreq = (MPI_Request *)malloc (num_aggregators * sizeof(MPI_Request));
    if (NULL == recvreq) {
        free(temp_position);
        for (i=0; i<num_aggregators; i++) {
            free(rbuf[i]);
        }
        free(rbuf);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    for (i=0 ; i<num_aggregators ; i++) {
        if (bytes_received[i]) {
            rc = MCA_PML_CALL(irecv(rbuf[i],
                                    bytes_received[i],
                                    MPI_BYTE,
                                    i*fh->f_aggregator_index,
                                    OMPIO_TAG_SCATTERV,
                                    fh->f_comm,
                                    &recvreq[i]));
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }

    if (0 == fh->f_rank%fh->f_aggregator_index) {
        req = (MPI_Request *)malloc (fh->f_size * sizeof(MPI_Request));
        if (NULL == req) {
            free(temp_position);
            for (i=0; i<num_aggregators; i++) {
                free(rbuf[i]);
            }
            free(rbuf);
            free(recvreq);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = MCA_PML_CALL(isend((char *)global_buf + displs[i],
                                        bytes_per_process[i],
                                        MPI_BYTE,
                                        i,
                                        OMPIO_TAG_SCATTERV,
                                        MCA_PML_BASE_SEND_STANDARD,
                                        fh->f_comm,
                                        &req[i]));
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }

    for (i=0; i<num_aggregators ; i++) {
        if (bytes_received[i]) {
            rc = ompi_request_wait (&recvreq[i], MPI_STATUS_IGNORE);
            if (OMPI_SUCCESS != rc) {
                goto exit;
            }
        }
    }
    if (0 == fh->f_rank%fh->f_aggregator_index) {
        for (i=0; i<fh->f_size ; i++) {
            if (bytes_per_process[i]) {
                rc = ompi_request_wait (&req[i], MPI_STATUS_IGNORE);
                if (OMPI_SUCCESS != rc) {
                    goto exit;
                }
            }
        }
    }
    /*
    for (i=0 ; i<num_aggregators ; i++) {
        fh->f_comm->c_coll.coll_scatterv (global_buf,
                                          bytes_per_process,
                                          displs,
                                          MPI_BYTE,
                                          rbuf[i],
                                          bytes_received[i],
                                          MPI_BYTE,
                                          i*fh->f_aggregator_index,
                                          fh->f_comm,
                                          fh->f_comm->c_coll.coll_scatterv_module);
    }
    */
    bytes_remaining = total_bytes_recv;

    while (bytes_remaining) {
        temp = (int)((OPAL_PTRDIFF_TYPE)broken_iovec[current].iov_base/stripe_size)
            % num_aggregators;

        if (part) {
            if (bytes_remaining > part) {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)receive_buf +
                                         (total_bytes_recv-bytes_remaining)),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf[temp]+
                                         temp_position[temp]),
                        part);
                bytes_remaining -= part;
                temp_position[temp] += part;
                part = 0;
                current ++;
            }
            else  {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)receive_buf +
                                         (total_bytes_recv-bytes_remaining)),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf[temp]+
                                         temp_position[temp]),
                        bytes_remaining);
                break;
            }
        }
        else {
            if (bytes_remaining > broken_iovec[current].iov_len) {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)receive_buf +
                                         (total_bytes_recv-bytes_remaining)),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf[temp]+
                                         temp_position[temp]),
                        broken_iovec[current].iov_len);
                bytes_remaining -= broken_iovec[current].iov_len;
                temp_position[temp] += broken_iovec[current].iov_len;
                current ++;
            }
            else {
                memcpy ((IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)receive_buf +
                                         (total_bytes_recv-bytes_remaining)),
                        (IOVBASE_TYPE *)((OPAL_PTRDIFF_TYPE)rbuf[temp]+
                                         temp_position[temp]),
                        bytes_remaining);
                break;
            }
        }
    }

 exit:
    for (i=0 ; i<num_aggregators ; i++) {
        if (NULL != rbuf[i]) {
            free (rbuf[i]);
            rbuf[i] = NULL;
        }
    }
    if (NULL != req) {
        free (req);
    }
    if (NULL != recvreq) {
        free (recvreq);
    }
    if (NULL != rbuf) {
        free (rbuf);
        rbuf = NULL;
    }
    if (NULL != temp_position) {
        free (temp_position);
        temp_position = NULL;
    }

    return rc;
}

void mca_io_ompio_get_num_aggregators ( int *num_aggregators)
{
    *num_aggregators = mca_io_ompio_num_aggregators;
    return;
}

void mca_io_ompio_get_bytes_per_agg ( int *bytes_per_agg)
{
    *bytes_per_agg = mca_io_ompio_bytes_per_agg;
    return;
}

/* Print queue related function implementations */
int ompi_io_ompio_set_print_queue (mca_io_ompio_print_queue **q,
				   int queue_type){

    int  ret = OMPI_SUCCESS;

    switch(queue_type) {

    case WRITE_PRINT_QUEUE:
	*q = coll_write_time;
	break;
    case READ_PRINT_QUEUE:
	*q = coll_read_time;
	break;
    }

    if (NULL == q){
	ret = OMPI_ERROR;
    }
    return ret;

}


int ompi_io_ompio_initialize_print_queue(mca_io_ompio_print_queue *q){

    int ret = OMPI_SUCCESS;
    q->first = 0;
    q->last = QUEUESIZE - 1;
    q->count = 0;
    return ret;
}
int ompi_io_ompio_register_print_entry (int queue_type,
					mca_io_ompio_print_entry x){

    int ret = OMPI_SUCCESS;
    mca_io_ompio_print_queue *q=NULL;

    ret = ompi_io_ompio_set_print_queue(&q, queue_type);

    if (ret != OMPI_ERROR){
	if (q->count >= QUEUESIZE){
	    return OMPI_ERROR;
	}
	else{
	    q->last = (q->last + 1) % QUEUESIZE;
	    q->entry[q->last] = x;
	    q->count = q->count + 1;
	}
    }
    return ret;
}

int  ompi_io_ompio_unregister_print_entry (int queue_type,
					   mca_io_ompio_print_entry *x){

    int ret = OMPI_SUCCESS;
    mca_io_ompio_print_queue *q=NULL;
    ret = ompi_io_ompio_set_print_queue(&q, queue_type);
    if (ret != OMPI_ERROR){
	if (q->count <= 0){
	    return OMPI_ERROR;
	}
	else{
	    *x = q->entry[q->first];
	    q->first = (q->first+1) % QUEUESIZE;
	    q->count = q->count - 1;
	}
    }
    return OMPI_SUCCESS;
}

int ompi_io_ompio_empty_print_queue(int queue_type){

    int ret = OMPI_SUCCESS;
    mca_io_ompio_print_queue *q=NULL;
    ret =  ompi_io_ompio_set_print_queue(&q, queue_type);

    assert (ret != OMPI_ERROR);
    (void)ret;  // silence compiler warning
    if (q->count == 0)
	    return 1;
    else
	return 0;


}

int ompi_io_ompio_full_print_queue(int queue_type){


    int ret = OMPI_SUCCESS;
    mca_io_ompio_print_queue *q=NULL;
    ret =  ompi_io_ompio_set_print_queue(&q, queue_type);

    assert ( ret != OMPI_ERROR);
    (void)ret;  // silence compiler warning
    if (q->count < QUEUESIZE)
	    return 0;
    else
	return 1;

}


int ompi_io_ompio_print_time_info(int queue_type,
				  char *name,
				  mca_io_ompio_file_t *fh){

    int i = 0, j=0, nprocs_for_coll = 0, ret = OMPI_SUCCESS, count = 0;
    double *time_details = NULL, *final_sum = NULL;
    double *final_max = NULL, *final_min = NULL;
    double *final_time_details=NULL;
    mca_io_ompio_print_queue *q=NULL;

    ret =  ompi_io_ompio_set_print_queue(&q, queue_type);

    assert (ret != OMPI_ERROR);
    nprocs_for_coll = q->entry[0].nprocs_for_coll;
    time_details = (double *) malloc (4*sizeof(double));
    if ( NULL == time_details){
	ret = OMPI_ERR_OUT_OF_RESOURCE;
	goto exit;

    }

    if (!fh->f_rank){

	final_min = (double *) malloc (3*sizeof(double));
	if ( NULL == final_min){
	    ret = OMPI_ERR_OUT_OF_RESOURCE;
	    goto exit;
	}

	final_max = (double *) malloc (3*sizeof(double));
	if ( NULL == final_max){
	    ret = OMPI_ERR_OUT_OF_RESOURCE;
	    goto exit;

	}

	final_sum = (double *) malloc (3*sizeof(double));
	if ( NULL == final_sum){
	    ret = OMPI_ERR_OUT_OF_RESOURCE;
	    goto exit;
	}

	final_time_details =
	    (double *)malloc
	    (fh->f_size * 4 * sizeof(double));
	if (NULL == final_time_details){
	    ret = OMPI_ERR_OUT_OF_RESOURCE;
	    goto exit;
	}

	count = 4 * fh->f_size;
	for(i=0;i<count;i++){
	    final_time_details[i] = 0.0;
	}


    }

    for (i = 0; i < 4; i++){
	time_details[i] = 0.0;
    }

    if (q->count > 0){
	for (i=0; i < q->count; i++){
	    for (j=0;j<3;j++){
		if (!fh->f_rank){
		    final_min[j] = 100000.0;
		    final_max[j] = 0.0;
		    final_sum[j] = 0.0;
		}
		time_details[j] += q->entry[i].time[j];
	    }
	    time_details[3]  = q->entry[i].aggregator;
	}
    }

    fh->f_comm->c_coll.coll_gather(time_details,
				   4,
				   MPI_DOUBLE,
				   final_time_details,
				   4,
				   MPI_DOUBLE,
				   0,
				   fh->f_comm,
				   fh->f_comm->c_coll.coll_gather_module);



    if (!fh->f_rank){

	for (i=0;i<count;i+=4){
	    if (final_time_details[i+3] == 1){
		final_sum[0] += final_time_details[i];
		final_sum[1] += final_time_details[i+1];
		final_sum[2] += final_time_details[i+2];

		if ( final_time_details[i] < final_min[0])
		    final_min[0] = final_time_details[i];
		if ( final_time_details[i+1] < final_min[1])
		    final_min[1] = final_time_details[i+1];
		if ( final_time_details[i+2] < final_min[2])
		    final_min[2] = final_time_details[i+2];



		if ( final_time_details[i] > final_max[0])
		    final_max[0] = final_time_details[i];
		if ( final_time_details[i+1] > final_max[1])
		    final_max[1] = final_time_details[i+1];
		if ( final_time_details[i+2] > final_max[2])
		    final_max[2] = final_time_details[i+2];

	    }
	}

	printf ("\n# MAX-%s AVG-%s MIN-%s MAX-COMM AVG-COMM MIN-COMM",
		name, name, name);
	printf (" MAX-EXCH AVG-EXCH MIN-EXCH\n");
	printf (" %f %f %f %f %f %f %f %f %f\n\n",
		final_max[0], final_sum[0]/nprocs_for_coll, final_min[0],
		final_max[1], final_sum[1]/nprocs_for_coll, final_min[1],
		final_max[2], final_sum[2]/nprocs_for_coll, final_min[2]);

    }

 exit:
    if ( NULL != final_max){
	free(final_max);
	final_max = NULL;
    }
    if (NULL != final_min){
	free(final_min);
	final_min = NULL;
    }
    if (NULL != final_sum){
	free(final_sum);
	final_sum = NULL;
    }
    if (NULL != time_details){
	free(time_details);
	time_details = NULL;
    }

    return ret;
}

int mca_io_ompio_create_groups(mca_io_ompio_file_t *fh,
		               size_t bytes_per_proc)
{

    int is_aggregator = 0;
    int final_aggr = 0;
    int final_num_aggrs = 0;
    int ompio_grouping_flag = 0;

    int *decision_list = NULL;

    OMPI_MPI_OFFSET_TYPE *start_offsets_lens = NULL;
    OMPI_MPI_OFFSET_TYPE *end_offsets = NULL;
    OMPI_MPI_OFFSET_TYPE bytes_per_group = 0;
    OMPI_MPI_OFFSET_TYPE *aggr_bytes_per_group = NULL;

    mca_io_ompio_prepare_to_group(fh,
	                          &start_offsets_lens,
	                          &end_offsets,
	                          &aggr_bytes_per_group,
				  &bytes_per_group,
	                          &decision_list,
                                  bytes_per_proc,
	                          &is_aggregator,
	                          &ompio_grouping_flag);

    switch(ompio_grouping_flag){

        case OMPIO_SPLIT:
            mca_io_ompio_split_initial_groups(fh,
                                              start_offsets_lens,
	       			              end_offsets,
					      bytes_per_group);
        break;

        case OMPIO_MERGE:
            mca_io_ompio_merge_initial_groups(fh,
		                              aggr_bytes_per_group,
					      decision_list,
				              is_aggregator);
        break;

        case  OMPIO_RETAIN:

            mca_io_ompio_retain_initial_groups(fh);

        break;


    }

    //Set aggregator index
    fh->f_aggregator_index = 0;

    //Calculate final number of aggregators
    if(fh->f_rank == fh->f_procs_in_group[fh->f_aggregator_index]){
	   final_aggr = 1;
    }
    fh->f_comm->c_coll.coll_allreduce (&final_aggr,
		                       &final_num_aggrs,
			               1,
			               MPI_INT,
			               MPI_SUM,
			               fh->f_comm,
			               fh->f_comm->c_coll.coll_allreduce_module);

    //Set final number of aggregators in file handle
    fh->f_final_num_aggrs = final_num_aggrs;

    //Print final number of aggregators if required

    /*if(fh->f_rank == 0){
        printf("Rank %d : has final_num_aggrs = %d\n",fh->f_rank,final_num_aggrs);
    }*/

    //Print final grouping
    /*if (fh->f_procs_in_group[fh->f_aggregator_index] == fh->f_rank)  {
        for (j=0 ; j<fh->f_procs_per_group; j++) {
            printf ("%d: Proc %d: %d\n", fh->f_rank, j, fh->f_procs_in_group[j]);
        }

	printf("\n\n");
    }

   */
    if (NULL != start_offsets_lens) {
        free (start_offsets_lens);
        start_offsets_lens =  NULL;
    }
    if (NULL != end_offsets) {
        free (end_offsets);
        end_offsets =  NULL;
    }
    if(NULL != aggr_bytes_per_group){
      free(aggr_bytes_per_group);
      aggr_bytes_per_group = NULL;
    }
    if( NULL != decision_list){
      free(decision_list);
      decision_list = NULL;
    }


   return OMPI_SUCCESS;
}

int mca_io_ompio_merge_initial_groups(mca_io_ompio_file_t *fh,
		                      OMPI_MPI_OFFSET_TYPE *aggr_bytes_per_group,
				      int *decision_list,
	                              int is_aggregator){

    OMPI_MPI_OFFSET_TYPE sum_bytes = 0;

    MPI_Request *sendreq = NULL;

    int start = 0;
    int end = 0;
    int i = 0;
    int j = 0;
    int r  = 0;

    int merge_pair_flag = 4;
    int first_merge_flag = 4;

    int *merge_aggrs = NULL;

    int is_new_aggregator= 0;


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
	                (sum_bytes <= mca_io_ompio_bytes_per_agg)){
	            sum_bytes = sum_bytes + aggr_bytes_per_group[i];
	            decision_list[i] = merge_pair_flag;
	            i++;
	        }
	        else if((decision_list[i] == OMPIO_MERGE) &&
	                (sum_bytes >= mca_io_ompio_bytes_per_agg)){
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
	              mca_io_ompio_merge_groups(fh,
		                                merge_aggrs,
			                        end-start+1);
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
       sendreq = (MPI_Request *)malloc ( 2 *fh->f_procs_per_group * sizeof(MPI_Request));
       if (NULL == sendreq) {
          return OMPI_ERR_OUT_OF_RESOURCE;
       }
       //Communicate grouping info
       for( j = 0 ; j < fh->f_procs_per_group; j++){
	   if (fh->f_procs_in_group[j] == fh->f_rank ) {
	       continue;
	   }
           //new aggregator sends new procs_per_group to all its members
	   MCA_PML_CALL(isend(&fh->f_procs_per_group,
                              1,
                              MPI_INT,
                              fh->f_procs_in_group[j],
                              OMPIO_PROCS_PER_GROUP_TAG,
                              MCA_PML_BASE_SEND_STANDARD,
                              fh->f_comm,
                              &sendreq[r++]));
	   //new aggregator sends distribution of process to all its new members
	   MCA_PML_CALL(isend(fh->f_procs_in_group,
			      fh->f_procs_per_group,
			      MPI_INT,
			      fh->f_procs_in_group[j],
			      OMPIO_PROCS_IN_GROUP_TAG,
			      MCA_PML_BASE_SEND_STANDARD,
			      fh->f_comm,
			      &sendreq[r++]));

       }
    }
    else {
	//All non aggregators
	//All processes receive initial process distribution from aggregators
	MCA_PML_CALL(recv(&fh->f_procs_per_group,
			  1,
			  MPI_INT,
			  MPI_ANY_SOURCE,
			  OMPIO_PROCS_PER_GROUP_TAG,
			  fh->f_comm,
			  MPI_STATUS_IGNORE));

	fh->f_procs_in_group = (int*)malloc (fh->f_procs_per_group * sizeof(int));
	if (NULL == fh->f_procs_in_group) {
	    opal_output (1, "OUT OF MEMORY\n");
	    return OMPI_ERR_OUT_OF_RESOURCE;
	}

	MCA_PML_CALL(recv(fh->f_procs_in_group,
			  fh->f_procs_per_group,
			  MPI_INT,
			  MPI_ANY_SOURCE,
			  OMPIO_PROCS_IN_GROUP_TAG,
			  fh->f_comm,
			  MPI_STATUS_IGNORE));
    }

    if(is_new_aggregator) {
	ompi_request_wait_all (r, sendreq, MPI_STATUSES_IGNORE);
	free (sendreq);
    }

    return OMPI_SUCCESS;
}

int mca_io_ompio_split_initial_groups(mca_io_ompio_file_t *fh,
		                      OMPI_MPI_OFFSET_TYPE *start_offsets_lens,
				      OMPI_MPI_OFFSET_TYPE *end_offsets,
				      OMPI_MPI_OFFSET_TYPE bytes_per_group){


    int size_new_group = 0;
    int size_old_group = 0;
    int size_last_group = 0;
    int size_smallest_group = 0;
    int num_groups = 0;

    OMPI_MPI_OFFSET_TYPE max_cci = 0;
    OMPI_MPI_OFFSET_TYPE min_cci = 0;

    size_new_group = ceil ((float)mca_io_ompio_bytes_per_agg * fh->f_init_procs_per_group/ bytes_per_group);
    size_old_group = fh->f_init_procs_per_group;

    mca_io_ompio_split_a_group(fh,
                               start_offsets_lens,
                               end_offsets,
                               size_new_group,
                               &max_cci,
                               &min_cci,
                               &num_groups,
                               &size_smallest_group);

    switch(mca_io_ompio_grouping_option){
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

		    size_new_group = floor( (float) (size_new_group + size_old_group ) / 2 );
  	            mca_io_ompio_split_a_group(fh,
		                               start_offsets_lens,
		                               end_offsets,
		                               size_new_group,
		                               &max_cci,
		                               &min_cci,
		                               &num_groups,
		                               &size_smallest_group);
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
	      	     size_new_group = ceil( (float) (size_new_group + size_old_group ) / 2 );
		     mca_io_ompio_split_a_group(fh,
		                                start_offsets_lens,
		                                end_offsets,
		                                size_new_group,
		                                &max_cci,
		                                &min_cci,
		                                &num_groups,
		                                &size_smallest_group);
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

    mca_io_ompio_finalize_split(fh,
                                size_new_group,
	                        size_last_group);


    return OMPI_SUCCESS;
}


int mca_io_ompio_retain_initial_groups(mca_io_ompio_file_t *fh){

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

int mca_io_ompio_merge_groups(mca_io_ompio_file_t *fh,
		              int *merge_aggrs,
			      int num_merge_aggrs)
{
    int i = 0;
    int *sizes_old_group;

    int *displs;



    sizes_old_group = (int*)malloc(num_merge_aggrs * sizeof(int));
    if (NULL == sizes_old_group) {
        opal_output (1, "OUT OF MEMORY\n");
        return OMPI_ERR_OUT_OF_RESOURCE;
    }


    displs = (int*)malloc(num_merge_aggrs * sizeof(int));
    if (NULL == displs) {
        opal_output (1, "OUT OF MEMORY\n");
        free(sizes_old_group);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }


    //merge_aggrs[0] is considered the new aggregator
    //New aggregator collects group sizes of the groups to be merged
    ompi_io_ompio_allgather_array (&fh->f_init_procs_per_group,
     		                   1,
				   MPI_INT,
				   sizes_old_group,
				   1,
				   MPI_INT,
				   0,
				   merge_aggrs,
				   num_merge_aggrs,
				   fh->f_comm);

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
        free(sizes_old_group);
        free(displs);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    //New aggregator also collects the grouping distribution
    //This is the actual merge
    //use allgatherv array
    ompi_io_ompio_allgatherv_array (fh->f_init_procs_in_group,
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


    free(displs);
    free (sizes_old_group);

    return OMPI_SUCCESS;

}



int mca_io_ompio_split_a_group(mca_io_ompio_file_t *fh,
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
     //if cci is not needed anymore
     if (NULL != cci) {
        free (cci);
	cci =  NULL;
     }
     return OMPI_SUCCESS;
}

int mca_io_ompio_finalize_split(mca_io_ompio_file_t *fh,
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

int mca_io_ompio_prepare_to_group(mca_io_ompio_file_t *fh,
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
    end_offsets_tmp = (OMPI_MPI_OFFSET_TYPE* )malloc (fh->f_init_procs_per_group * sizeof(OMPI_MPI_OFFSET_TYPE));
    if (NULL == end_offsets_tmp) {
        opal_output (1, "OUT OF MEMORY\n");
        free(start_offsets_lens_tmp);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    //Gather start offsets across processes in a group on aggregator
    ompi_io_ompio_allgather_array (start_offset_len,
                                   3,
                                   OMPI_OFFSET_DATATYPE,
                                   start_offsets_lens_tmp,
                                   3,
                                   OMPI_OFFSET_DATATYPE,
                                   0,
                                   fh->f_init_procs_in_group,
                                   fh->f_init_procs_per_group,
                                   fh->f_comm);
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
          return OMPI_ERR_OUT_OF_RESOURCE;
       }
    decision_list_tmp = (int* )malloc (fh->f_init_num_aggrs * sizeof(int));
    if (NULL == decision_list_tmp) {
        opal_output (1, "OUT OF MEMORY\n");
        free(aggr_bytes_per_group_tmp);
        free(start_offsets_lens_tmp);
        free(end_offsets_tmp);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    //Communicate bytes per group between all aggregators
    ompi_io_ompio_allgather_array (bytes_per_group,
                                   1,
                                   OMPI_OFFSET_DATATYPE,
                                   aggr_bytes_per_group_tmp,
                                   1,
                                   OMPI_OFFSET_DATATYPE,
                                   0,
                                   fh->f_init_aggr_list,
                                   fh->f_init_num_aggrs,
                                   fh->f_comm);

    for( i = 0; i < fh->f_init_num_aggrs; i++){
       if((size_t)(aggr_bytes_per_group_tmp[i])>
          (size_t)mca_io_ompio_bytes_per_agg){
          decision_list_tmp[i] = OMPIO_SPLIT;
          split_count++;
       }
       else if((size_t)(aggr_bytes_per_group_tmp[i])<
               (size_t)mca_io_ompio_bytes_per_agg){
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
    ompi_io_ompio_bcast_array (ompio_grouping_flag,
                               1,
                               MPI_INT,
                               0,
                               fh->f_init_procs_in_group,
                               fh->f_init_procs_per_group,
                               fh->f_comm);



    return OMPI_SUCCESS;
}


