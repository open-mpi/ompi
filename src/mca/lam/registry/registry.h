/* -*- C -*-
 *
 * $HEADER$
 */

#ifndef MCA_REGISTRY_H_
#define MCA_REGISTRY_H_

#include "lam_config.h"

#include "mca/mca.h"

typedef int (*mca_registry_query_fn_t)(int *priority);
typedef int (*mca_registry_init_fn_t)(char* parallel_job_id, int vpid);
typedef int (*mca_registry_publish_fn_t)(char* key, void* data, size_t data_len);
typedef int (*mca_registry_lookup_fn_t)(char* key, void** data, size_t* data_len);
typedef int (*mca_registry_finalize_fn_t)(void);

/*
 * Ver 1.0.0
 */
typedef struct mca_registry_module_1_0_0 {
  mca_1_0_0_t super;

  mca_registry_query_fn_t registry_m_query;
  mca_registry_init_fn_t registry_m_init;
} mca_registry_module_1_0_0_t;

typedef struct mca_registry_1_0_0 {
  mca_registry_publish_fn_t registry_publish;
  mca_registry_lookup_fn_t registry_lookup;
  mca_registry_finalize_fn_t registry_finalize;
} mca_registry_module_1_0_0_t;

typedef mca_registry_module_1_0_0_t mca_registry_module_t;
typedef mca_registry_1_0_0_t mca_registry_t;

#endif
