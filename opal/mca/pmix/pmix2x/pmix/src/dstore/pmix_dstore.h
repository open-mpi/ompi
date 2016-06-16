/*
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_DSTORE_H
#define PMIX_DSTORE_H

#include <src/include/pmix_config.h>


#include <pmix/pmix_common.h>
#include "src/buffer_ops/buffer_ops.h"


BEGIN_C_DECLS


int pmix_dstore_init(void);
void pmix_dstore_finalize(void);
int pmix_dstore_store(const char *nspace, int rank, pmix_kval_t *kv);
int pmix_dstore_fetch(const char *nspace, int rank, const char *key, pmix_value_t **kvs);

/**
 * Initialize the module. Returns an error if the module cannot
 * run, success if it can and wants to be used.
 */
typedef int (*pmix_dstore_base_module_init_fn_t)(void);

/**
 * Finalize the module. Tear down any allocated storage, disconnect
 * from any system support.
 */
typedef int (*pmix_dstore_base_module_fini_fn_t)(void);

/**
* store key/value pair in datastore.
*
* @param nspace   namespace string
*
* @param rank     rank.
*
* @param kv       key/value pair.
*
* @return PMIX_SUCCESS on success.
*/
typedef int (*pmix_dstore_base_module_store_fn_t)(const char *nspace, int rank, pmix_kval_t *kv);

/**
* fetch value in datastore.
*
* @param nspace   namespace string
*
* @param rank     rank.
*
* @param key      key.
*
* @return kvs(key/value pair) and PMIX_SUCCESS on success.
*/
typedef int (*pmix_dstrore_base_module_fetch_fn_t)(const char *nspace, int rank, const char *key, pmix_value_t **kvs);


/**
* structure for dstore modules
*/
typedef struct {
    const char *name;
    pmix_dstore_base_module_init_fn_t        init;
    pmix_dstore_base_module_fini_fn_t        finalize;
    pmix_dstore_base_module_store_fn_t       store;
    pmix_dstrore_base_module_fetch_fn_t      fetch;
} pmix_dstore_base_module_t;

END_C_DECLS

#endif /* PMIX_DSTORE_H */
