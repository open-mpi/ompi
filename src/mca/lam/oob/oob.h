/* -*- C -*-
 *
 * $HEADER$
 */

#ifndef MCA_OOB_H_
#define MCA_OOB_H_

#include "lam_config.h"

#include "lam/runtime/oob.h"
#include "mca/mca.h"

typedef int (*mca_oob_query_fn_t)(int *priority);
typedef int (*mca_oob_init_fn_t)(char* parallel_job_id, int vpid);
typedef int (*mca_oob_send_fn_t)(char* parallel_job_id, int vpid, int tag, 
                                 void* data, size_t data_len);
typedef int (*mca_oob_recv_fn_t)(char* parallel_job_id, int* tag, int* vpid, 
                                 void** data, size_t* data_len);
typedef int (*mca_oob_recv_nb_fn_t)(char* parallel_job_id, int* tag, int* vpid, 
                                    void** data, size_t* data_len);
typedef int (*mca_oob_recv_cb_fn_t)(char* parallel_job_id, int tag, 
                                    lam_oob_recv_cb_t callback);

/*
 * Ver 1.0.0
 */
typedef struct mca_oob_module_1_0_0 {
  mca_1_0_0_t super;

  mca_oob_query_fn_t oobm_query;
  mca_oob_init_fn_t oobm_init;
} mca_oob_module_1_0_0_t;

typedef struct mca_oob_1_0_0 {
  mca_oob_send_fn_t oob_send;
  mca_oob_recv_fn_t oob_recv;
  mca_oob_recv_nb_fn_t oob_recv_nb;
  mca_oob_recv_cb_fn_t oob_recv_cb;
} mca_oob_module_1_0_0_t;

typedef mca_oob_module_1_0_0_t mca_oob_module_t;
typedef mca_oob_1_0_0_t mca_oob_t;

#endif
