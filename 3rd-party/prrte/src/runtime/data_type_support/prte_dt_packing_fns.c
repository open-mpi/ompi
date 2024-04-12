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

#include "src/class/pmix_pointer_array.h"
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
 * spawn another job - so we only pack that limited set of required data
 */
int prte_job_pack(pmix_data_buffer_t *bkt, prte_job_t *job)
{
    pmix_status_t rc;
    int32_t j, count, bookmark;
    prte_app_context_t *app;
    prte_proc_t *proc;
    prte_attribute_t *kv;
    pmix_list_t *cache;
    prte_info_item_t *val;

    /* pack the nspace */
    rc = PMIx_Data_pack(NULL, bkt, (void *) &job->nspace, 1, PMIX_PROC_NSPACE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }
    /* pack the flags */
    rc = PMIx_Data_pack(NULL, bkt, (void *) &job->flags, 1, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the attributes that need to be sent */
    count = 0;
    PMIX_LIST_FOREACH(kv, &job->attributes, prte_attribute_t)
    {
        if (PRTE_ATTR_GLOBAL == kv->local) {
            ++count;
        }
    }
    rc = PMIx_Data_pack(NULL, bkt, (void *) &count, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }
    PMIX_LIST_FOREACH(kv, &job->attributes, prte_attribute_t)
    {
        if (PRTE_ATTR_GLOBAL == kv->local) {
            rc = PMIx_Data_pack(NULL, bkt, (void *) &kv->key, 1, PMIX_UINT16);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                return prte_pmix_convert_status(rc);
            }
            rc = PMIx_Data_pack(NULL, bkt, (void *) &kv->data, 1, PMIX_VALUE);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                return prte_pmix_convert_status(rc);
            }
        }
    }
    /* check for job info attribute */
    cache = NULL;
    if (prte_get_attribute(&job->attributes, PRTE_JOB_INFO_CACHE, (void **) &cache, PMIX_POINTER)
        && NULL != cache) {
        /* we need to pack these as well, but they are composed
         * of prte_info_item_t's on a list. So first pack the number
         * of list elements */
        count = pmix_list_get_size(cache);
        rc = PMIx_Data_pack(NULL, bkt, (void *) &count, 1, PMIX_INT32);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return prte_pmix_convert_status(rc);
        }
        /* now pack each element on the list */
        PMIX_LIST_FOREACH(val, cache, prte_info_item_t)
        {
            rc = PMIx_Data_pack(NULL, bkt, (void *) &val->info, 1, PMIX_INFO);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                return prte_pmix_convert_status(rc);
            }
        }
    } else {
        /* pack a zero to indicate no job info is being passed */
        count = 0;
        rc = PMIx_Data_pack(NULL, bkt, (void *) &count, 1, PMIX_INT32);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return prte_pmix_convert_status(rc);
        }
    }

    /* pack the personality */
    count = PMIX_ARGV_COUNT_COMPAT(job->personality);
    rc = PMIx_Data_pack(NULL, bkt, (void *) &count, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }
    for (j = 0; j < count; j++) {
        rc = PMIx_Data_pack(NULL, bkt, (void *) &job->personality[j], 1, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return prte_pmix_convert_status(rc);
        }
    }

    /* pack the number of apps */
    rc = PMIx_Data_pack(NULL, bkt, (void *) &job->num_apps, 1, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* if there are apps, pack the app_contexts */
    if (0 < job->num_apps) {
        for (j = 0; j < job->apps->size; j++) {
            if (NULL == (app = (prte_app_context_t *) pmix_pointer_array_get_item(job->apps, j))) {
                continue;
            }
            rc = prte_app_pack(bkt, app);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                return prte_pmix_convert_status(rc);
            }
        }
    }

    /* pack the number of procs and offset */
    rc = PMIx_Data_pack(NULL, bkt, (void *) &job->num_procs, 1, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }
    rc = PMIx_Data_pack(NULL, bkt, (void *) &job->offset, 1, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    if (0 < job->num_procs) {
        for (j = 0; j < job->procs->size; j++) {
            if (NULL == (proc = (prte_proc_t *) pmix_pointer_array_get_item(job->procs, j))) {
                continue;
            }
            rc = prte_proc_pack(bkt, proc);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                return prte_pmix_convert_status(rc);
            }
        }
    }

    /* pack the stdin target */
    rc = PMIx_Data_pack(NULL, bkt, (void *) &job->stdin_target, 1, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the total slots allocated to the job */
    rc = PMIx_Data_pack(NULL, bkt, (void *) &job->total_slots_alloc, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* if the map is NULL, then we cannot pack it as there is
     * nothing to pack. However, we have to flag whether or not
     * the map is included so the unpacking routine can know
     * what to do
     */
    if (NULL == job->map) {
        /* pack a zero value */
        j = 0;
    } else {
        /* pack a one to indicate a map is there */
        j = 1;
    }
    rc = PMIx_Data_pack(NULL, bkt, (void *) &j, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the map - this will only pack the fields that control
     * HOW a job is to be mapped. We do -not- pack the mapped procs
     * or nodes as this info does not need to be transmitted
     */
    if (NULL != job->map) {
        rc = prte_map_pack(bkt, job->map);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return prte_pmix_convert_status(rc);
        }
    }

    /* pack the bookmark */
    if (NULL == job->bookmark) {
        bookmark = -1;
    } else {
        bookmark = job->bookmark->index;
    }
    rc = PMIx_Data_pack(NULL, bkt, (void *) &bookmark, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the job state */
    rc = PMIx_Data_pack(NULL, bkt, (void *) &job->state, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the launcher ID */
    rc = PMIx_Data_pack(NULL, bkt, (void *) &job->launcher, 1, PMIX_PROC_NSPACE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    return PRTE_SUCCESS;
}

int prte_node_pack(pmix_data_buffer_t *bkt, prte_node_t *node)
{
    int rc;
    int32_t count;
    uint8_t flag;
    prte_attribute_t *kv;

    /* do not pack the index - it is meaningless on the other end */

    /* pack the node name */
    rc = PMIx_Data_pack(NULL, bkt, (void *) &node->name, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* do not pack the daemon name or launch id */

    /* pack the number of procs on the node */
    rc = PMIx_Data_pack(NULL, bkt, (void *) &node->num_procs, 1, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* do not pack the procs */

    /* pack whether we are oversubscribed or not */
    flag = PRTE_FLAG_TEST(node, PRTE_NODE_FLAG_OVERSUBSCRIBED);
    rc = PMIx_Data_pack(NULL, bkt, (void *) &flag, 1, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the state */
    rc = PMIx_Data_pack(NULL, bkt, (void *) &node->state, 1, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack any shared attributes */
    count = 0;
    PMIX_LIST_FOREACH(kv, &node->attributes, prte_attribute_t)
    {
        if (PRTE_ATTR_GLOBAL == kv->local) {
            ++count;
        }
    }
    rc = PMIx_Data_pack(NULL, bkt, (void *) &count, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }
    if (0 < count) {
        PMIX_LIST_FOREACH(kv, &node->attributes, prte_attribute_t)
        {
            if (PRTE_ATTR_GLOBAL == kv->local) {
                rc = PMIx_Data_pack(NULL, bkt, (void *) &kv->key, 1, PMIX_UINT16);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    return prte_pmix_convert_status(rc);
                }
                rc = PMIx_Data_pack(NULL, bkt, (void *) &kv->data, 1, PMIX_VALUE);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    return prte_pmix_convert_status(rc);
                }
            }
        }
    }
    return PRTE_SUCCESS;
}
/*
 * PROC
 */
int prte_proc_pack(pmix_data_buffer_t *bkt, prte_proc_t *proc)
{
    pmix_status_t rc;
    int32_t count;
    prte_attribute_t *kv;

    /* pack the name */
    rc = PMIx_Data_pack(NULL, bkt, &proc->name, 1, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the daemon/node it is on */
    rc = PMIx_Data_pack(NULL, bkt, &proc->parent, 1, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the local rank */
    rc = PMIx_Data_pack(NULL, bkt, &proc->local_rank, 1, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the node rank */
    rc = PMIx_Data_pack(NULL, bkt, &proc->node_rank, 1, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the state */
    rc = PMIx_Data_pack(NULL, bkt, &proc->state, 1, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the app context index */
    rc = PMIx_Data_pack(NULL, bkt, &proc->app_idx, 1, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the app rank */
    rc = PMIx_Data_pack(NULL, bkt, &proc->app_rank, 1, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the cpuset */
    rc = PMIx_Data_pack(NULL, bkt, (void *) &proc->cpuset, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the attributes that will go */
    count = 0;
    PMIX_LIST_FOREACH(kv, &proc->attributes, prte_attribute_t)
    {
        if (PRTE_ATTR_GLOBAL == kv->local) {
            ++count;
        }
    }
    rc = PMIx_Data_pack(NULL, bkt, &count, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }
    if (0 < count) {
        PMIX_LIST_FOREACH(kv, &proc->attributes, prte_attribute_t)
        {
            if (PRTE_ATTR_GLOBAL == kv->local) {
                rc = PMIx_Data_pack(NULL, bkt, (void *) &kv->key, 1, PMIX_UINT16);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    return prte_pmix_convert_status(rc);
                }
                rc = PMIx_Data_pack(NULL, bkt, (void *) &kv->data, 1, PMIX_VALUE);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    return prte_pmix_convert_status(rc);
                }
            }
        }
    }

    return PRTE_SUCCESS;
}

/*
 * APP CONTEXT
 */
int prte_app_pack(pmix_data_buffer_t *bkt, prte_app_context_t *app)
{
    pmix_status_t rc;
    int32_t count, j;
    prte_attribute_t *kv;

    /* pack the application index (for multiapp jobs) */
    rc = PMIx_Data_pack(NULL, bkt, &app->idx, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the application name */
    rc = PMIx_Data_pack(NULL, bkt, &app->app, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the number of processes */
    rc = PMIx_Data_pack(NULL, bkt, &app->num_procs, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the first rank for this app */
    rc = PMIx_Data_pack(NULL, bkt, &app->first_rank, 1, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the number of entries in the argv array */
    count = PMIX_ARGV_COUNT_COMPAT(app->argv);
    rc = PMIx_Data_pack(NULL, bkt, &count, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* if there are entries, pack the argv entries */
    for (j = 0; j < count; j++) {
        rc = PMIx_Data_pack(NULL, bkt, (void *) &app->argv[j], 1, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return prte_pmix_convert_status(rc);
        }
    }

    /* pack the number of entries in the enviro array */
    count = PMIX_ARGV_COUNT_COMPAT(app->env);
    rc = PMIx_Data_pack(NULL, bkt, &count, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* if there are entries, pack the enviro entries */
    for (j = 0; j < count; j++) {
        rc = PMIx_Data_pack(NULL, bkt, (void *) &app->env[j], 1, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return prte_pmix_convert_status(rc);
        }
    }

    /* pack the cwd */
    rc = PMIx_Data_pack(NULL, bkt, &app->cwd, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the flags */
    rc = PMIx_Data_pack(NULL, bkt, &app->flags, 1, PMIX_INT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack attributes */
    count = 0;
    PMIX_LIST_FOREACH(kv, &app->attributes, prte_attribute_t)
    {
        if (PRTE_ATTR_GLOBAL == kv->local) {
            ++count;
        }
    }
    rc = PMIx_Data_pack(NULL, bkt, &count, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }
    if (0 < count) {
        PMIX_LIST_FOREACH(kv, &app->attributes, prte_attribute_t)
        {
            if (PRTE_ATTR_GLOBAL == kv->local) {
                rc = PMIx_Data_pack(NULL, bkt, (void *) &kv->key, 1, PMIX_UINT16);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    return prte_pmix_convert_status(rc);
                }
                rc = PMIx_Data_pack(NULL, bkt, (void *) &kv->data, 1, PMIX_VALUE);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    return prte_pmix_convert_status(rc);
                }
            }
        }
    }

    return PRTE_SUCCESS;
}

/*
 * JOB_MAP
 * NOTE: There is no obvious reason to include all the node information when
 * sending a map
 */
int prte_map_pack(pmix_data_buffer_t *bkt, struct prte_job_map_t *mp)
{
    pmix_status_t rc;
    prte_job_map_t *map = (prte_job_map_t *) mp;

    /* pack the requested mapper */
    rc = PMIx_Data_pack(NULL, bkt, &map->req_mapper, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the last mapper */
    rc = PMIx_Data_pack(NULL, bkt, &map->last_mapper, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the policies */
    rc = PMIx_Data_pack(NULL, bkt, &map->mapping, 1, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }
    rc = PMIx_Data_pack(NULL, bkt, &map->ranking, 1, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }
    rc = PMIx_Data_pack(NULL, bkt, &map->binding, 1, PMIX_UINT16);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    /* pack the number of nodes involved in the job */
    rc = PMIx_Data_pack(NULL, bkt, &map->num_nodes, 1, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return prte_pmix_convert_status(rc);
    }

    return PRTE_SUCCESS;
}
