/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2016 University of Houston. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COMMON_OMPIO_CALLBACKS_H
#define MCA_COMMON_OMPIO_CALLBACKS_H

struct mca_io_ompio_file_t;

/* functions to retrieve the number of aggregators and the size of the
   temporary buffer on aggregators from the fcoll modules */
typedef int (*mca_common_ompio_get_mca_parameter_value_fn_t) ( char *mca_parameter_name, int name_length );

typedef int (*mca_common_ompio_generate_current_file_view_fn_t) (struct mca_io_ompio_file_t *fh,
                                                                 size_t max_data,
                                                                 struct iovec **f_iov,
                                                                 int *iov_count);
typedef void (*mca_common_ompio_get_num_aggregators_fn_t ) ( int *num_aggregators );
typedef void (*mca_common_ompio_get_bytes_per_agg_fn_t ) ( int *bytes_per_agg );
typedef int (*mca_common_ompio_decode_datatype_fn_t) (struct mca_io_ompio_file_t *fh,
						  struct ompi_datatype_t *datatype,
						  int count,  const void *buf,
						  size_t *max_data,  struct iovec **iov,
						  uint32_t *iov_count);

typedef int (*mca_common_ompio_set_aggregator_props_fn_t) (struct mca_io_ompio_file_t *fh,
                                                               int num_aggregators,
                                                               size_t bytes_per_proc);




OMPI_DECLSPEC int mca_common_ompio_set_callbacks(mca_common_ompio_generate_current_file_view_fn_t generate_current_file_view,
                                                 mca_common_ompio_get_mca_parameter_value_fn_t get_mca_parameter_value,
                                                 mca_common_ompio_get_num_aggregators_fn_t get_num_aggregators,
                                                 mca_common_ompio_get_bytes_per_agg_fn_t get_bytes_per_agg );

#endif
