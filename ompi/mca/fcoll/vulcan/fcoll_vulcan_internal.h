/*
 * Copyright (c) 2024      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_FCOLL_VULCAN_INTERNAL_H
#define MCA_FCOLL_VULCAN_INTERNAL_H

#include "ompi_config.h"


BEGIN_C_DECLS
/* Used for loading file-offsets per aggregator*/
typedef struct mca_io_ompio_local_io_array{
    OMPI_MPI_OFFSET_TYPE offset;
    MPI_Aint             length;
    int                  process_id;
}mca_io_ompio_local_io_array;

typedef struct mca_io_ompio_aggregator_data {
    int *disp_index, *sorted, n;
    size_t *fview_count;
    int *max_disp_index;
    int **blocklen_per_process;
    MPI_Aint **displs_per_process, total_bytes, bytes_per_cycle, total_bytes_written;
    MPI_Comm comm;
    char *global_buf, *prev_global_buf;
    ompi_datatype_t **recvtype, **prev_recvtype;
    struct iovec *global_iov_array;
    int current_index, current_position;
    int bytes_to_write_in_cycle, bytes_remaining, procs_per_group;    
    int *procs_in_group, iov_index;
    size_t bytes_sent, prev_bytes_sent;
    struct iovec *decoded_iov;
    int bytes_to_write, prev_bytes_to_write;
    mca_common_ompio_io_array_t *io_array, *prev_io_array;
    int num_io_entries, prev_num_io_entries;
} mca_io_ompio_aggregator_data;


#define SWAP_REQUESTS(_r1,_r2) { \
    ompi_request_t **_t=_r1;     \
    _r1=_r2;                     \
    _r2=_t;}

#define SWAP_AGGR_POINTERS(_aggr,_num) {                        \
    int _i;                                                     \
    char *_t;                                                   \
    for (_i=0; _i<_num; _i++ ) {                                \
        _aggr[_i]->prev_io_array=_aggr[_i]->io_array;             \
        _aggr[_i]->prev_num_io_entries=_aggr[_i]->num_io_entries; \
        _aggr[_i]->prev_bytes_sent=_aggr[_i]->bytes_sent;         \
        _aggr[_i]->prev_bytes_to_write=_aggr[_i]->bytes_to_write; \
        _t=_aggr[_i]->prev_global_buf;                            \
        _aggr[_i]->prev_global_buf=_aggr[_i]->global_buf;         \
        _aggr[_i]->global_buf=_t;                                 \
        _t=(char *)_aggr[_i]->recvtype;                           \
        _aggr[_i]->recvtype=_aggr[_i]->prev_recvtype;             \
        _aggr[_i]->prev_recvtype=(ompi_datatype_t **)_t;          }                                                             \
}

int mca_fcoll_vulcan_break_file_view (struct iovec *decoded_iov, int iov_count,
                                      struct iovec *local_iov_array, int local_count,
                                      struct iovec ***broken_decoded_iovs, int **broken_iov_counts,
                                      struct iovec ***broken_iov_arrays, int **broken_counts,
                                      MPI_Aint **broken_total_lengths,
                                      int stripe_count, size_t stripe_size);

int mca_fcoll_vulcan_get_configuration (ompio_file_t *fh, int num_io_procs,
                                        size_t max_data);

int mca_fcoll_vulcan_minmax (ompio_file_t *fh, struct iovec *iov, int iov_count,
			     int num_aggregators, long *new_stripe_size);

void mca_fcoll_vulcan_calc_blocklen_disps (mca_io_ompio_aggregator_data *data, int aggregator,
                                           int rank, size_t *bytes_comm);

int mca_fcoll_vulcan_calc_file_offsets(mca_io_ompio_aggregator_data *data,
                                       mca_io_ompio_local_io_array *file_offsets_for_agg,
                                       int *sorted_file_offsets, MPI_Aint *memory_displacements,
                                       int entries_per_aggregator, int rank, int index);

void mca_fcoll_vulcan_calc_io_array(mca_common_ompio_io_array_t *io_array, int *num_io_entries, int max_io_arrays,
                                    char *global_buf, mca_io_ompio_local_io_array *file_offsets_for_agg,
                                    int *sorted_offsets, MPI_Aint *memory_displacements, int rank);

END_C_DECLS

#endif /* MCA_FCOLL_VULCAN_INTERNAL_H */
