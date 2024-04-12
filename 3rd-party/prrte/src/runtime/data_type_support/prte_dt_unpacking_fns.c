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
#include "types.h"

#include <sys/types.h>

#include "src/hwloc/hwloc-internal.h"
#include "src/mca/errmgr/errmgr.h"
#include "src/mca/rmaps/rmaps_types.h"
#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_argv.h"

#include "src/runtime/prte_globals.h"

/*
 * JOB
 * NOTE: We do not pack all of the job object's fields as many of them have no
 * value in sending them to another location. The only purpose in packing and
 * sending a job object is to communicate the data required to dynamically
 * spawn another job - so we only pack that limited set of required data.
 * Therefore, only unpack what was packed
 */
int prte_job_unpack(pmix_data_buffer_t *bkt, prte_job_t **job)
{
    int rc;
    int32_t k, n, count, bookmark;
    prte_job_t *jptr;
    prte_app_idx_t j;
    prte_attribute_t *kv;
    char *tmp;
    prte_info_item_t *val;
    pmix_info_t pval;
    pmix_list_t *cache;

    /* create the prte_job_t object */
    jptr = PMIX_NEW(prte_job_t);
    if (NULL == jptr) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return PRTE_ERR_OUT_OF_RESOURCE;
    }

    /* unpack the nspace */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &jptr->nspace, &n, PMIX_PROC_NSPACE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(jptr);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the flags */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &jptr->flags, &n, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(jptr);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the attributes */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &count, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(jptr);
        return prte_pmix_convert_status(rc);
    }
    for (k = 0; k < count; k++) {
        kv = PMIX_NEW(prte_attribute_t);
        n = 1;
        rc = PMIx_Data_unpack(NULL, bkt, &kv->key, &n, PMIX_UINT16);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(jptr);
            PMIX_RELEASE(kv);
            return prte_pmix_convert_status(rc);
        }
        rc = PMIx_Data_unpack(NULL, bkt, &kv->data, &n, PMIX_VALUE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(jptr);
            PMIX_RELEASE(kv);
            return prte_pmix_convert_status(rc);
        }
        kv->local = PRTE_ATTR_GLOBAL; // obviously not a local value
        pmix_list_append(&jptr->attributes, &kv->super);
    }
    /* unpack any job info */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &count, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(jptr);
        return prte_pmix_convert_status(rc);
    }
    if (0 < count) {
        cache = PMIX_NEW(pmix_list_t);
        prte_set_attribute(&jptr->attributes, PRTE_JOB_INFO_CACHE, PRTE_ATTR_LOCAL, (void *) cache,
                           PMIX_POINTER);
        for (k = 0; k < count; k++) {
            n = 1;
            rc = PMIx_Data_unpack(NULL, bkt, &pval, &n, PMIX_INFO);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(jptr);
                return prte_pmix_convert_status(rc);
            }
            val = PMIX_NEW(prte_info_item_t);
            PMIX_INFO_XFER(&val->info, &pval);
            PMIX_INFO_DESTRUCT(&pval);
            pmix_list_append(cache, &val->super);
        }
    }

    /* unpack the personality */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &count, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(jptr);
        return prte_pmix_convert_status(rc);
    }
    for (k = 0; k < count; k++) {
        n = 1;
        rc = PMIx_Data_unpack(NULL, bkt, &tmp, &n, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(jptr);
            return prte_pmix_convert_status(rc);
        }
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&jptr->personality, tmp);
        free(tmp);
    }

    /* unpack the num apps */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &jptr->num_apps, &n, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(jptr);
        return prte_pmix_convert_status(rc);
    }
    /* if there are apps, unpack them */
    if (0 < jptr->num_apps) {
        prte_app_context_t *app;
        for (j = 0; j < jptr->num_apps; j++) {
            n = 1;
            rc = prte_app_unpack(bkt, &app);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(jptr);
                return prte_pmix_convert_status(rc);
            }
            pmix_pointer_array_add(jptr->apps, app);
        }
    }

    /* unpack num procs and offset */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &jptr->num_procs, &n, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(jptr);
        return prte_pmix_convert_status(rc);
    }
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &jptr->offset, &n, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(jptr);
        return prte_pmix_convert_status(rc);
    }

    if (0 < jptr->num_procs) {
        prte_proc_t *proc;
        for (j = 0; j < jptr->num_procs; j++) {
            rc = prte_proc_unpack(bkt, &proc);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(jptr);
                return prte_pmix_convert_status(rc);
            }
            pmix_pointer_array_add(jptr->procs, proc);
        }
    }

    /* unpack stdin target */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &jptr->stdin_target, &n, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(jptr);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the total slots allocated to the job */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &jptr->total_slots_alloc, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(jptr);
        return prte_pmix_convert_status(rc);
    }

    /* if the map is NULL, then we didn't pack it as there was
     * nothing to pack. Instead, we packed a flag to indicate whether or not
     * the map is included */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &j, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(jptr);
        return prte_pmix_convert_status(rc);
    }
    if (0 < j) {
        /* unpack the map */
        n = 1;
        rc = prte_map_unpack(bkt, &(jptr->map));
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(jptr);
            return prte_pmix_convert_status(rc);
        }
    }

    /* unpack the bookmark */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &bookmark, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(jptr);
        return prte_pmix_convert_status(rc);
    }
    if (0 <= bookmark) {
        /* retrieve it */
        jptr->bookmark = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, bookmark);
    }

    /* unpack the job state */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &jptr->state, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(jptr);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the launcher ID */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &jptr->launcher, &n, PMIX_PROC_NSPACE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(jptr);
        return prte_pmix_convert_status(rc);
    }

    *job = jptr;
    return PRTE_SUCCESS;
}

/*
 * NODE
 */
int prte_node_unpack(pmix_data_buffer_t *bkt, prte_node_t **nd)
{
    pmix_status_t rc;
    int32_t n, k, count;
    prte_node_t *node;
    uint8_t flag;
    prte_attribute_t *kv;

    /* create the node object */
    node = PMIX_NEW(prte_node_t);
    if (NULL == node) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return PRTE_ERR_OUT_OF_RESOURCE;
    }

    /* unpack the node name */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &node->name, &n, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(node);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the number of procs on the node */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &node->num_procs, &n, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(node);
        return prte_pmix_convert_status(rc);
    }

    /* unpack whether we are oversubscribed */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &flag, &n, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(node);
        return prte_pmix_convert_status(rc);
    }
    if (flag) {
        PRTE_FLAG_SET(node, PRTE_NODE_FLAG_OVERSUBSCRIBED);
    }

    /* unpack the state */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &node->state, &n, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(node);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the attributes */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &count, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(node);
        return prte_pmix_convert_status(rc);
    }
    for (k = 0; k < count; k++) {
        kv = PMIX_NEW(prte_attribute_t);
        n = 1;
        rc = PMIx_Data_unpack(NULL, bkt, &kv->key, &n, PMIX_UINT16);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(node);
            PMIX_RELEASE(kv);
            return prte_pmix_convert_status(rc);
        }
        rc = PMIx_Data_unpack(NULL, bkt, &kv->data, &n, PMIX_VALUE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(node);
            PMIX_RELEASE(kv);
            return prte_pmix_convert_status(rc);
        }
        kv->local = PRTE_ATTR_GLOBAL; // obviously not a local value
        pmix_list_append(&node->attributes, &kv->super);
    }
    *nd = node;
    return PRTE_SUCCESS;
}

/*
 * PROC
 */
int prte_proc_unpack(pmix_data_buffer_t *bkt, prte_proc_t **pc)
{
    pmix_status_t rc;
    int32_t n, count, k;
    prte_attribute_t *kv;
    ;
    prte_proc_t *proc;

    /* create the prte_proc_t object */
    proc = PMIX_NEW(prte_proc_t);
    if (NULL == proc) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return PRTE_ERR_OUT_OF_RESOURCE;
    }

    /* unpack the name */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &proc->name, &n, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(proc);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the node it is on */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &proc->parent, &n, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(proc);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the local rank */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &proc->local_rank, &n, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(proc);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the node rank */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &proc->node_rank, &n, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(proc);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the state */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &proc->state, &n, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(proc);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the app context index */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &proc->app_idx, &n, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(proc);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the app_rank */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &proc->app_rank, &n, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(proc);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the cpuset */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &proc->cpuset, &n, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(proc);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the attributes */
    rc = PMIx_Data_unpack(NULL, bkt, &count, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(proc);
        return prte_pmix_convert_status(rc);
    }
    for (k = 0; k < count; k++) {
        kv = PMIX_NEW(prte_attribute_t);
        n = 1;
        rc = PMIx_Data_unpack(NULL, bkt, &kv->key, &n, PMIX_UINT16);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(proc);
            PMIX_RELEASE(kv);
            return prte_pmix_convert_status(rc);
        }
        rc = PMIx_Data_unpack(NULL, bkt, &kv->data, &n, PMIX_VALUE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(proc);
            PMIX_RELEASE(kv);
            return prte_pmix_convert_status(rc);
        }
        kv->local = PRTE_ATTR_GLOBAL; // obviously not a local value
        pmix_list_append(&proc->attributes, &kv->super);
    }
    *pc = proc;
    return PRTE_SUCCESS;
}

/*
 * APP_CONTEXT
 */
int prte_app_unpack(pmix_data_buffer_t *bkt, prte_app_context_t **ap)
{
    int rc;
    prte_app_context_t *app;
    int32_t n, count, k;
    prte_attribute_t *kv;
    char *tmp;

    /* create the app_context object */
    app = PMIX_NEW(prte_app_context_t);
    if (NULL == app) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return PRTE_ERR_OUT_OF_RESOURCE;
    }

    /* get the app index number */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &app->idx, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(app);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the application name */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &app->app, &n, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(app);
        return prte_pmix_convert_status(rc);
    }

    /* get the number of processes */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &app->num_procs, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(app);
        return prte_pmix_convert_status(rc);
    }

    /* get the first rank for this app */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &app->first_rank, &n, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(app);
        return prte_pmix_convert_status(rc);
    }

    /* get the number of argv strings that were packed */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &count, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(app);
        return prte_pmix_convert_status(rc);
    }
    for (k = 0; k < count; k++) {
        n = 1;
        rc = PMIx_Data_unpack(NULL, bkt, &tmp, &n, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(app);
            return prte_pmix_convert_status(rc);
        }
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&app->argv, tmp);
        free(tmp);
    }

    /* get the number of env strings */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &count, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(app);
        return prte_pmix_convert_status(rc);
    }
    for (k = 0; k < count; k++) {
        n = 1;
        rc = PMIx_Data_unpack(NULL, bkt, &tmp, &n, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(app);
            return prte_pmix_convert_status(rc);
        }
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&app->env, tmp);
        free(tmp);
    }

    /* unpack the cwd */
    rc = PMIx_Data_unpack(NULL, bkt, &app->cwd, &n, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(app);
        return prte_pmix_convert_status(rc);
    }

    /* get the flags */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &app->flags, &n, PMIX_INT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(app);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the attributes */
    rc = PMIx_Data_unpack(NULL, bkt, &count, &n, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(app);
        return prte_pmix_convert_status(rc);
    }
    for (k = 0; k < count; k++) {
        kv = PMIX_NEW(prte_attribute_t);
        n = 1;
        rc = PMIx_Data_unpack(NULL, bkt, &kv->key, &n, PMIX_UINT16);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(app);
            PMIX_RELEASE(kv);
            return prte_pmix_convert_status(rc);
        }
        rc = PMIx_Data_unpack(NULL, bkt, &kv->data, &n, PMIX_VALUE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(app);
            PMIX_RELEASE(kv);
            return prte_pmix_convert_status(rc);
        }
        kv->local = PRTE_ATTR_GLOBAL; // obviously not a local value
        pmix_list_append(&app->attributes, &kv->super);
    }
    *ap = app;
    return PRTE_SUCCESS;
}

/*
 * JOB_MAP
 * NOTE: There is no obvious reason to include all the node information when
 * sending a map - hence, we do not pack that field, so don't unpack it here
 */
int prte_map_unpack(pmix_data_buffer_t *bkt, struct prte_job_map_t **mp)
{
    int rc;
    int32_t n;
    prte_job_map_t *map;

    /* create the prte_rmaps_base_map_t object */
    map = PMIX_NEW(prte_job_map_t);
    if (NULL == map) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return PRTE_ERR_OUT_OF_RESOURCE;
    }

    /* unpack the requested mapper */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &map->req_mapper, &n, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(map);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the last mapper */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &map->last_mapper, &n, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(map);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the policies */
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &map->mapping, &n, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(map);
        return prte_pmix_convert_status(rc);
    }
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &map->ranking, &n, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(map);
        return prte_pmix_convert_status(rc);
    }
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &map->binding, &n, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(map);
        return prte_pmix_convert_status(rc);
    }

    /* unpack the number of nodes involved in the job */
    n = 1;
    n = 1;
    rc = PMIx_Data_unpack(NULL, bkt, &map->num_nodes, &n, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(map);
        return prte_pmix_convert_status(rc);
    }

    *mp = map;
    return PRTE_SUCCESS;
}
