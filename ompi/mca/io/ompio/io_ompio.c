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
 * Copyright (c) 2008-2016 University of Houston. All rights reserved.
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
#include "ompi/mca/fcoll/base/fcoll_base_coll_array.h"
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
    int ret = OMPI_SUCCESS;
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
        free (displs);
        free (sizes_old_group);
        return ret;
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
        free(sizes_old_group);
        free(displs);
        return OMPI_ERR_OUT_OF_RESOURCE;
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
    
    free (displs);
    free (sizes_old_group);

    return ret;

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
    fcoll_base_coll_allgather_array (start_offset_len,
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
    fcoll_base_coll_allgather_array (bytes_per_group,
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
    fcoll_base_coll_bcast_array (ompio_grouping_flag,
                                 1,
                                 MPI_INT,
                                 0,
                                 fh->f_init_procs_in_group,
                                 fh->f_init_procs_per_group,
                                 fh->f_comm);
    


    return OMPI_SUCCESS;
}


