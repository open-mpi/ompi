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
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#ifndef MCA_IO_OMPIO_AGGREGATORS_H
#define MCA_IO_OMPIO_AGGREGATORS_H


/*AGGREGATOR GROUPING DECISIONS*/
#define OMPIO_MERGE                     1
#define OMPIO_SPLIT                     2
#define OMPIO_RETAIN                    3

typedef struct {
	int ndims;
	int *dims;
	int *periods;
	int *coords;
	int reorder;
} mca_io_ompio_cart_topo_components;


typedef struct{
        OMPI_MPI_OFFSET_TYPE contg_chunk_size;
        int *procs_in_contg_group;
	int procs_per_contg_group;
} mca_io_ompio_contg;



/*Aggregator selection methods*/
OMPI_DECLSPEC int mca_io_ompio_set_aggregator_props (struct mca_io_ompio_file_t *fh,
                                                     int num_aggregators,
                                                     size_t bytes_per_proc);

int mca_io_ompio_cart_based_grouping(mca_io_ompio_file_t *ompio_fh);

int mca_io_ompio_fview_based_grouping(mca_io_ompio_file_t *fh, int *num_groups,
                                      mca_io_ompio_contg *contg_groups);
int mca_io_ompio_simple_grouping(mca_io_ompio_file_t *fh, int *num_groups,
                                 mca_io_ompio_contg *contg_groups);

int mca_io_ompio_finalize_initial_grouping(mca_io_ompio_file_t *fh,  int num_groups,
                                           mca_io_ompio_contg *contg_groups);

int mca_io_ompio_create_groups(mca_io_ompio_file_t *fh, size_t bytes_per_proc);

int mca_io_ompio_prepare_to_group(mca_io_ompio_file_t *fh,
                                  OMPI_MPI_OFFSET_TYPE **start_offsets_lens,
                                  OMPI_MPI_OFFSET_TYPE **end_offsets,
                                  OMPI_MPI_OFFSET_TYPE **aggr_bytes_per_group,
                                  OMPI_MPI_OFFSET_TYPE *bytes_per_group,
                                  int **decision_list,
                                  size_t bytes_per_proc,
                                  int *is_aggregator,
                                  int *ompio_grouping_flag);

int mca_io_ompio_retain_initial_groups(mca_io_ompio_file_t *fh);


int mca_io_ompio_split_initial_groups(mca_io_ompio_file_t *fh,
                                      OMPI_MPI_OFFSET_TYPE *start_offsets_lens,
                                      OMPI_MPI_OFFSET_TYPE *end_offsets,
                                      OMPI_MPI_OFFSET_TYPE bytes_per_group);


int mca_io_ompio_split_a_group(mca_io_ompio_file_t *fh,
                               OMPI_MPI_OFFSET_TYPE *start_offsets_lens,
                               OMPI_MPI_OFFSET_TYPE *end_offsets,
                               int size_new_group,
                               OMPI_MPI_OFFSET_TYPE *max_cci,
                               OMPI_MPI_OFFSET_TYPE *min_cci,
                               int *num_groups, int *size_smallest_group);

int mca_io_ompio_finalize_split(mca_io_ompio_file_t *fh, int size_new_group,
                                int size_last_group);

int mca_io_ompio_merge_initial_groups(mca_io_ompio_file_t *fh,
                                      OMPI_MPI_OFFSET_TYPE *aggr_bytes_per_group,
                                      int *decision_list, int is_aggregator);

int mca_io_ompio_merge_groups(mca_io_ompio_file_t *fh, int *merge_aggrs,
                              int num_merge_aggrs);


#endif 
