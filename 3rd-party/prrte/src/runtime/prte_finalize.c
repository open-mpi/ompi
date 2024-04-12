/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include "prte_config.h"
#include "constants.h"

#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_output.h"

#include "src/mca/base/pmix_mca_base_alias.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/ess/base/base.h"
#include "src/mca/ess/ess.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_locks.h"
#include "src/runtime/runtime.h"
#include "src/util/name_fns.h"
#include "src/util/proc_info.h"

int prte_finalize(void)
{
    int rc, n, i;
    prte_job_t *jdata = NULL, *child_jdata = NULL, *next_jdata = NULL;
    prte_app_context_t *app;
    prte_proc_t *p;
    pmix_pointer_array_t *array;
    prte_node_t *node;
    prte_topology_t *topo;

    PMIX_ACQUIRE_THREAD(&prte_init_lock);
    if (!prte_initialized) {
        PMIX_RELEASE_THREAD(&prte_init_lock);
        return PRTE_ERROR;
    }
    prte_initialized = false;
    PMIX_RELEASE_THREAD(&prte_init_lock);

    /* protect against multiple calls */
    if (pmix_mutex_trylock(&prte_finalize_lock)) {
        return PRTE_SUCCESS;
    }

    /* flag that we are finalizing */
    prte_finalizing = true;

    /* release the cache */
    PMIX_RELEASE(prte_cache);

    /* call the finalize function for this environment */
    if (PRTE_SUCCESS != (rc = prte_ess.finalize())) {
        return rc;
    }
    (void) pmix_mca_base_framework_close(&prte_ess_base_framework);

    // clean up the node array
    for (n = 0; n < prte_node_pool->size; n++) {
        node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, n);
        if (NULL == node) {
            continue;
        }
        pmix_pointer_array_set_item(prte_node_pool, n, NULL);
        PMIX_RELEASE(node);
    }
    PMIX_RELEASE(prte_node_pool);

    for (n = 0; n < prte_job_data->size; n++) {
        jdata = (prte_job_t *) pmix_pointer_array_get_item(prte_job_data, n);
        if (NULL == jdata) {
            continue;
        }
        // Remove all children from the list
        // We do not want to destruct this list here since that occurs in the
        // prte_job_t destructor - which will happen in the next loop.
        PMIX_LIST_FOREACH_SAFE(child_jdata, next_jdata, &jdata->children, prte_job_t)
        {
            pmix_list_remove_item(&jdata->children, &child_jdata->super);
        }
        /* clean up any app contexts as they refcount the jdata object */
        for (i=0; i < jdata->apps->size; i++) {
            app = (prte_app_context_t*)pmix_pointer_array_get_item(jdata->apps, i);
            if (NULL != app) {
                pmix_pointer_array_set_item(jdata->apps, i, NULL);
                PMIX_RELEASE(app);
            }
        }
        // clean up any procs
        for (i=0; i < jdata->procs->size; i++) {
            p = (prte_proc_t*)pmix_pointer_array_get_item(jdata->procs, i);
            if (NULL != p) {
                pmix_pointer_array_set_item(jdata->procs, i, NULL);
                PMIX_RELEASE(p);
            }
        }
        pmix_pointer_array_set_item(prte_job_data, n, NULL);
        PMIX_RELEASE(jdata);
    }
    PMIX_RELEASE(prte_job_data);

    for (n = 0; n < prte_node_topologies->size; n++) {
        topo = (prte_topology_t *) pmix_pointer_array_get_item(prte_node_topologies, n);
        if (NULL == topo) {
            continue;
        }
        pmix_pointer_array_set_item(prte_node_topologies, n, NULL);
        PMIX_RELEASE(topo);
    }
    PMIX_RELEASE(prte_node_topologies);

    /* Close the general debug stream */
    pmix_output_close(prte_debug_output);

    pmix_mca_base_alias_cleanup();

    prte_proc_info_finalize();

    pmix_output_finalize();

    /* now shutdown PMIx - need to do this last as it finalizes
     * the utilities and class system we depend upon */
    PMIx_server_finalize();

    return PRTE_SUCCESS;
}
