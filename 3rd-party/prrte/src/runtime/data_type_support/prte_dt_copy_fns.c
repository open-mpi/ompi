/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#include <string.h>

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/rmaps/rmaps_types.h"
#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_globals.h"
#include "src/util/pmix_argv.h"

/**
 * JOB
 */
int prte_job_copy(prte_job_t **dest, prte_job_t *src)
{
    (*dest) = src;
    PMIX_RETAIN(src);

    return PRTE_SUCCESS;
}

/**
 * NODE
 */
int prte_node_copy(prte_node_t **dest, prte_node_t *src)
{
    prte_node_t *node;

    node = PMIX_NEW(prte_node_t);
    node->name = strdup(src->name);
    node->state = src->state;
    node->slots = src->slots;
    node->slots_inuse = src->slots_inuse;
    node->slots_max = src->slots_max;
    node->topology = src->topology;
    node->flags = src->flags;
    (*dest) = node;

    return PRTE_SUCCESS;
}

/**
 * PROC
 */
int prte_proc_copy(prte_proc_t **dest, prte_proc_t *src)
{
    (*dest) = src;
    PMIX_RETAIN(src);
    return PRTE_SUCCESS;
}

/*
 * APP CONTEXT
 */
int prte_app_copy(prte_app_context_t **dest, prte_app_context_t *src)
{
    prte_value_t *kv, *kvnew;
    pmix_status_t rc;

    /* create the new object */
    *dest = PMIX_NEW(prte_app_context_t);
    if (NULL == *dest) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return PRTE_ERR_OUT_OF_RESOURCE;
    }

    /* copy data into it */
    (*dest)->idx = src->idx;
    if (NULL != src->app) {
        (*dest)->app = strdup(src->app);
    }
    (*dest)->num_procs = src->num_procs;
    (*dest)->argv = PMIX_ARGV_COPY_COMPAT(src->argv);
    (*dest)->env = PMIX_ARGV_COPY_COMPAT(src->env);
    if (NULL != src->cwd) {
        (*dest)->cwd = strdup(src->cwd);
    }

    PMIX_LIST_FOREACH(kv, &src->attributes, prte_value_t)
    {
        kvnew = PMIX_NEW(prte_value_t);
        PMIX_VALUE_XFER_DIRECT(rc, &kvnew->value, &kv->value);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(kvnew);
            return prte_pmix_convert_status(rc);
        }
        pmix_list_append(&(*dest)->attributes, &kvnew->super);
    }

    return PRTE_SUCCESS;
}

/*
 * JOB_MAP
 */
int prte_map_copy(struct prte_job_map_t **d, struct prte_job_map_t *s)
{
    int32_t i;
    prte_job_map_t **dest = (prte_job_map_t **) d;
    prte_job_map_t *src = (prte_job_map_t *) s;

    if (NULL == src) {
        *dest = NULL;
        return PRTE_SUCCESS;
    }

    /* create the new object */
    *dest = PMIX_NEW(prte_job_map_t);
    if (NULL == *dest) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return PRTE_ERR_OUT_OF_RESOURCE;
    }

    /* copy data into it */
    (*dest)->mapping = src->mapping;
    (*dest)->ranking = src->ranking;
    (*dest)->binding = src->binding;
    (*dest)->num_new_daemons = src->num_new_daemons;
    (*dest)->daemon_vpid_start = src->daemon_vpid_start;
    (*dest)->num_nodes = src->num_nodes;

    /* copy the pointer array - have to do this manually
     * as no dss.copy function is setup for that object
     */
    (*dest)->nodes->lowest_free = src->nodes->lowest_free;
    (*dest)->nodes->number_free = src->nodes->number_free;
    (*dest)->nodes->size = src->nodes->size;
    (*dest)->nodes->max_size = src->nodes->max_size;
    (*dest)->nodes->block_size = src->nodes->block_size;
    for (i = 0; i < src->nodes->size; i++) {
        (*dest)->nodes->addr[i] = src->nodes->addr[i];
    }

    return PRTE_SUCCESS;
}
