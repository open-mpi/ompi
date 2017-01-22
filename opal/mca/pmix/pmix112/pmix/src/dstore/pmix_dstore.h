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


int pmix_dstore_init(pmix_info_t info[], size_t ninfo);
void pmix_dstore_finalize(void);
int pmix_dstore_store(const char *nspace, int rank, pmix_kval_t *kv);

/*
 * Return codes:
 * - PMIX_ERR_BAD_PARAM - bad parameters - can't proceed.
 * - PMIX_ERROR - fatal error
 * - PMIX_ERR_NOT_FOUND - we have the BLOB for the process but the 
 *   requested key wasn't found there
 * - PMIX_ERR_PROC_ENTRY_NOT_FOUND - the BLOB for the process wasn't
 *   found - need to request it from the server.
 */
int pmix_dstore_fetch(const char *nspace, int rank,
                      const char *key, pmix_value_t **kvs);
int pmix_dstore_patch_env(const char *nspace, char ***env);
int pmix_dstore_nspace_add(const char *nspace, pmix_info_t info[], size_t ninfo);
int pmix_dstore_nspace_del(const char *nspace);

/**
 * Initialize the module. Returns an error if the module cannot
 * run, success if it can and wants to be used.
 */
typedef int (*pmix_dstore_base_module_init_fn_t)(pmix_info_t info[], size_t ninfo);

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
typedef int (*pmix_dstore_base_module_fetch_fn_t)(const char *nspace, int rank, const char *key, pmix_value_t **kvs);

/**
* get base dstore path.
*
* @param nspace   namespace string
*
* @param rank     rank.
*
* @return PMIX_SUCCESS on success.
*/
typedef int (*pmix_dstore_base_module_proc_patch_env_fn_t)(const char *nspace, char ***env);

/**
* get base dstore path.
*
* @param nspace   namespace string
*
* @param rank     rank.
*
* @return PMIX_SUCCESS on success.
*/
typedef int (*pmix_dstore_base_module_add_nspace_fn_t)(const char *nspace,
                                                        pmix_info_t info[],
                                                        size_t ninfo);

/**
* finalize nspace.
*
* @param nspace   namespace string
*
* @return PMIX_SUCCESS on success.
*/
typedef int (*pmix_dstore_base_module_del_nspace_fn_t)(const char *nspace);

/**
* structure for dstore modules
*/
typedef struct {
    const char *name;
    pmix_dstore_base_module_init_fn_t        init;
    pmix_dstore_base_module_fini_fn_t        finalize;
    pmix_dstore_base_module_store_fn_t       store;
    pmix_dstore_base_module_fetch_fn_t       fetch;
    pmix_dstore_base_module_proc_patch_env_fn_t   patch_env;
    pmix_dstore_base_module_add_nspace_fn_t  nspace_add;
    pmix_dstore_base_module_del_nspace_fn_t  nspace_del;

} pmix_dstore_base_module_t;

END_C_DECLS

#endif /* PMIX_DSTORE_H */
