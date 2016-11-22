/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_PTPCOLL_ALLREDUCE_H
#define MCA_BCOL_PTPCOLL_ALLREDUCE_H

#include "ompi_config.h"
#include "ompi/op/op.h"
#include "ompi/datatype/ompi_datatype.h"
#include "bcol_ptpcoll.h"
#include "bcol_ptpcoll_utils.h"

enum {
	BLOCK_OFFSET = 0,
	LOCAL_REDUCE_SEG_OFFSET,
	BLOCK_COUNT,
	SEG_SIZE,
	NOFFSETS
};

BEGIN_C_DECLS
int bcol_ptpcoll_allreduce_narraying(mca_bcol_ptpcoll_module_t *ptpcoll_module,
        const int buffer_index, void *data_buffer,
		struct ompi_op_t *op,
		const int count, struct ompi_datatype_t *dtype, const int
		buffer_size, const int relative_group_index);


int bcol_ptpcoll_allreduce_narraying_init(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);

int bcol_ptpcoll_allreduce_recursivek_scatter_reduce(mca_bcol_ptpcoll_module_t *ptpcoll_module,
						const int buffer_index, void *sbuf,
					    void *rbuf,
						struct ompi_op_t *op,
						const int count, struct ompi_datatype_t *dtype,
						const int relative_group_index,
						const int padded_start_byte);

int bcol_ptpcoll_allreduce_knomial_allgather(mca_bcol_ptpcoll_module_t *ptpcoll_module,
				const int buffer_index,
				void *sbuf,void *rbuf, int count, struct
				ompi_datatype_t *dtype,
				const int relative_group_index,
				const int padded_start_byte);

int bcol_ptpcoll_allreduce_recursivek_scatter_reduce_allgather_init(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);


int compute_knomial_allgather_offsets(int group_index, int count, struct
				ompi_datatype_t *dtype,int k_radix,int n_exchanges,
				int **offsets);


int bcol_ptpcoll_allreduce_recursivek_scatter_reduce_extra(mca_bcol_ptpcoll_module_t *ptpcoll_module,
						int buffer_index,
						void *sbuf,
					    void *rbuf,
						struct ompi_op_t *op,
						const int count, struct ompi_datatype_t *dtype);

int bcol_ptpcoll_allreduce_knomial_allgather_extra(mca_bcol_ptpcoll_module_t *ptpcoll_module,
						int buffer_index,
						void *sbuf,
					    void *rbuf,
						const int count, struct ompi_datatype_t *dtype);

int bcol_ptpcoll_allreduce_recursivek_scatter_reduce_allgather_extra_init(bcol_function_args_t *input_args,
        struct mca_bcol_base_function_t *const_args);

int bcol_ptpcoll_allreduce_init(mca_bcol_base_module_t *super);

#if 0
int knomial_reduce_scatter_offsets(int group_index,int count, struct ompi_datatype_t *dtype, int k_radix,
				int n_exchanges, int nth_exchange, size_t *recv_offset, size_t
				*block_offset, size_t *block_count, size_t *block_size, size_t
				*seg_size);

int allgather_offsets(int group_index,int count, struct ompi_datatype_t *dtype, int k_radix,
				int n_exchanges, int nth_exchange, size_t *send_offset, size_t
				*block_offset, size_t *block_count, size_t *block_size, size_t
				*seg_size);
#endif

END_C_DECLS

#endif
