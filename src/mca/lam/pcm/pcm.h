/* -*- C -*-
 *
 * $HEADER$
 */

#ifndef MCA_PCM_H_
#define MCA_PCM_H_

#include "lam_config.h"

#include "mca/mca.h"

typedef int (*mca_pcm_query_fn_t)(int *priority);
typedef int (*mca_pcm_init_fn_t)(char* parallel_job_id, int vpid);


/*
 * Ver 1.0.0
 */
typedef struct mca_pcm_module_1_0_0 {
  mca_1_0_0_t super;

  mca_pcm_query_fn_t pcmm_query;
  mca_pcm_init_fn_t pcmm_init;
} mca_pcm_module_1_0_0_t;

typedef struct mca_pcm_1_0_0 {
  mca_pcm_publish_fn_t pcm_publish;
  mca_pcm_lookup_fn_t pcm_lookup;
  mca_pcm_finalize_fn_t pcm_finalize;
} mca_pcm_module_1_0_0_t;

typedef mca_pcm_module_1_0_0_t mca_pcm_module_t;
typedef mca_pcm_1_0_0_t mca_pcm_t;

#endif
