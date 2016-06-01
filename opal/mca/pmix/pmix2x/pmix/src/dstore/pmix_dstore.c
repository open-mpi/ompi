/*
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>
#include <pmix/pmix_common.h>
#include "src/include/pmix_globals.h"

#include "pmix_dstore.h"
#include "pmix_esh.h"


/*
 * Array of all possible DSTOREs
 */

/****  ENSURE THE FOLLOWING VALUE IS AT LEAST AS
 ****  LARGE AS THE TOTAL NUMBER OF SUPPORTED SPCs
 ****  IN THE ARRAY BELOW
 */

static pmix_dstore_base_module_t *all[] = {
    &pmix_dstore_esh_module,

    /* Always end the array with a NULL */
    NULL
};

pmix_dstore_base_module_t pmix_dstore = {0};

int pmix_dstore_init(void)
{
    pmix_dstore = *all[0];

    if (!pmix_dstore.init) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    return pmix_dstore.init();
}

void pmix_dstore_finalize(void)
{
    if (!pmix_dstore.finalize) {
        pmix_dstore.finalize();
    }

    return ;
}

int pmix_dstore_store(const char *nspace, int rank, pmix_kval_t *kv)
{
    if (!pmix_dstore.store) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    return pmix_dstore.store(nspace, rank, kv);
}

int pmix_dstore_fetch(const char *nspace, int rank, const char *key, pmix_value_t **kvs)
{
    if (!pmix_dstore.fetch) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    return pmix_dstore.fetch(nspace, rank, key, kvs);
}
