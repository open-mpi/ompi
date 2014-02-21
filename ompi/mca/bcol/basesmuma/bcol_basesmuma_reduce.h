#ifndef __BASESMUMA_REDUCE_H_

#define __BASESMUMA_REDUCE_H_

#include "ompi_config.h"
#include "ompi/mca/bcol/basesmuma/bcol_basesmuma.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "bcol_basesmuma_utils.h"
#include <unistd.h>

enum {
    BLOCK_OFFSET = 0,
    LOCAL_REDUCE_SEG_OFFSET,
    BLOCK_COUNT,
    SEG_SIZE,
	NOFFSETS
};

int compute_knomial_reduce_offsets(int group_index, int count, struct
				ompi_datatype_t *dtype,int k_radix,int n_exchanges,
				int **offsets);

int compute_knomial_reduce_offsets_reverse(int group_index, int count, struct
				ompi_datatype_t *dtype,int k_radix,int n_exchanges,
				int **offsets);

int bcol_basesmuma_lmsg_reduce_recursivek_scatter_reduce(mca_bcol_basesmuma_module_t *sm_module,
						const int buffer_index, void *sbuf,
					    void *rbuf,
						struct ompi_op_t *op,
						const int count, struct ompi_datatype_t *dtype,
						const int relative_group_index,
						const int padded_start_byte,
					volatile int8_t ready_flag,
						volatile mca_bcol_basesmuma_payload_t *data_buffs);

int bcol_basesmuma_lmsg_reduce_knomial_gather(mca_bcol_basesmuma_module_t *basesmuma_module,
				const int buffer_index,
				void *sbuf,void *rbuf, int count, struct
				ompi_datatype_t *dtype,
				const int my_group_index,
				const int padded_start_byte,
				volatile int8_t rflag,
				volatile mca_bcol_basesmuma_payload_t *data_buffs);

int bcol_basesmuma_lmsg_reduce_extra_root(mca_bcol_basesmuma_module_t *sm_module,
						const int buffer_index, void *sbuf,
					    void *rbuf,
						struct ompi_op_t *op,
						const int count, struct ompi_datatype_t *dtype,
						const int relative_group_index,
						const int padded_start_byte,
					volatile int8_t rflag,
						volatile mca_bcol_basesmuma_payload_t *data_buffs);



int bcol_basesmuma_lmsg_reduce_extra_non_root(mca_bcol_basesmuma_module_t *sm_module,
						const int buffer_index, void *sbuf,
					    void *rbuf,
						int root,
						struct ompi_op_t *op,
						const int count, struct ompi_datatype_t *dtype,
						const int relative_group_index,
						const int group_size,
						const int padded_start_byte,
					volatile int8_t rflag,
						volatile mca_bcol_basesmuma_payload_t *data_buffs);

int bcol_basesmuma_lmsg_reduce(bcol_function_args_t *input_args,
        mca_bcol_base_function_t *c_input_args);

int bcol_basesmuma_lmsg_reduce_extra(bcol_function_args_t *input_args,
        mca_bcol_base_function_t *c_input_args);

void basesmuma_reduce_recv(int my_group_index, int peer,
						   void *recv_buffer,
						   int recv_size,
					   volatile int8_t ready_flag_val,
					   volatile mca_bcol_basesmuma_payload_t *data_buffs);

void  basesmuma_reduce_send(int my_group_index,
						   int peer,
						   void *send_buffer,
						   int snd_size,
						   int send_offset,
					   volatile int8_t ready_flag_val,
					   volatile mca_bcol_basesmuma_payload_t *data_buffs);

#endif
