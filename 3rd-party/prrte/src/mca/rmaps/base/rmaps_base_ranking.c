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
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      Huawei Technologies Co., Ltd.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <string.h>

#include "src/class/pmix_pointer_array.h"
#include "src/hwloc/hwloc-internal.h"
#include "src/util/pmix_output.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_show_help.h"
#include "types.h"

#include "src/mca/rmaps/base/base.h"
#include "src/mca/rmaps/base/rmaps_private.h"

static void compute_app_rank(prte_job_t *jdata)
{
    int i, j, k;
    prte_app_context_t *app;
    prte_proc_t *proc;

    for (i=0; i < jdata->apps->size; i++) {
        app = (prte_app_context_t*)pmix_pointer_array_get_item(jdata->apps, i);
        if (NULL == app) {
            continue;
        }
        k=0;
        /* loop thru all procs in job to find those from this app_context */
        for (j=0; j < jdata->procs->size; j++) {
            proc = (prte_proc_t*)pmix_pointer_array_get_item(jdata->procs, j);
            if (NULL == proc) {
                continue;
            }
            if (proc->app_idx != app->idx) {
                continue;
            }
            proc->app_rank = k++;
        }
    }
}

int prte_rmaps_base_compute_vpids(prte_job_t *jdata,
                                  prte_rmaps_options_t *options)
{
    int m, n;
    unsigned k, nobjs, pass;
    prte_node_t *node;
    prte_proc_t *proc;
    int rc;
    hwloc_obj_t obj;
    pmix_rank_t rank, lrank;

    if (options->userranked) {
        /* ranking has already been done, but we still need to
         * compute the local and app ranks (node rank is computed
         * on-the-fly during mapping) */
        for (n=0; n < jdata->map->nodes->size; n++) {
            node = (prte_node_t*)pmix_pointer_array_get_item(jdata->map->nodes, n);
            if (NULL == node) {
                continue;
            }
            lrank = 0;
            for (m=0; m < node->procs->size; m++) {
                proc = (prte_proc_t*)pmix_pointer_array_get_item(node->procs, m);
                if (NULL == proc) {
                    continue;
                }
                if (!PMIX_CHECK_NSPACE(jdata->nspace, proc->name.nspace)) {
                    continue;
                }
                proc->local_rank = lrank;
                PMIX_RETAIN(proc);
                rc = pmix_pointer_array_set_item(jdata->procs, proc->name.rank, proc);
                if (PMIX_SUCCESS != rc) {
                    PMIX_RELEASE(proc);
                    return rc;
                }
                ++lrank;
            }
        }
        compute_app_rank(jdata);
        return PRTE_SUCCESS;
    }

    /* if we are ranking by SLOT, then we simply go thru
     * each node and rank all thr procs from this app
     * in the order in which they are in the node's
     * proc array - this is the order in which they
     * were assigned */
    if (PRTE_RANK_BY_SLOT == options->rank) {
        rank = 0;
        for (n=0; n < jdata->map->nodes->size; n++) {
            node = (prte_node_t*)pmix_pointer_array_get_item(jdata->map->nodes, n);
            if (NULL == node) {
                continue;
            }
            lrank = 0;
            for (m=0; m < node->procs->size; m++) {
                proc = (prte_proc_t*)pmix_pointer_array_get_item(node->procs, m);
                if (NULL == proc) {
                    continue;
                }
                if (!PMIX_CHECK_NSPACE(jdata->nspace, proc->name.nspace)) {
                    continue;
                }
                proc->name.rank = rank;
                proc->local_rank = lrank;
                PMIX_RETAIN(proc);
                rc = pmix_pointer_array_set_item(jdata->procs, proc->name.rank, proc);
                if (PMIX_SUCCESS != rc) {
                    PMIX_RELEASE(proc);
                    return rc;
                }
                ++rank;
                ++lrank;
            }
        }
        compute_app_rank(jdata);
        return PRTE_SUCCESS;
    }

    /* if we are ranking by NODE, then we use the number of nodes
     * used by this app (which is stored in the "options" struct)
     * and increment the rank for each proc on each node by that */
    if (PRTE_RANK_BY_NODE == options->rank) {
        for (n=0; n < jdata->map->nodes->size; n++) {
            node = (prte_node_t*)pmix_pointer_array_get_item(jdata->map->nodes, n);
            if (NULL == node) {
                continue;
            }
            rank = n;
            lrank = 0;
            for (m=0; m < node->procs->size; m++) {
                proc = (prte_proc_t*)pmix_pointer_array_get_item(node->procs, m);
                if (NULL == proc) {
                    continue;
                }
                if (!PMIX_CHECK_NSPACE(jdata->nspace, proc->name.nspace)) {
                    continue;
                }
                proc->name.rank = rank;
                proc->local_rank = lrank;
                PMIX_RETAIN(proc);
                rc = pmix_pointer_array_set_item(jdata->procs, proc->name.rank, proc);
                if (PMIX_SUCCESS != rc) {
                    PMIX_RELEASE(proc);
                    return rc;
                }
                rank += options->nnodes;
                ++lrank;
            }
        }
        compute_app_rank(jdata);
        return PRTE_SUCCESS;
    }

    /* if we are ranking FILL, we rank all procs on a given
     * object on each node prior to moving to the next object
     * on that node */
    if (PRTE_RANK_BY_FILL == options->rank) {
        rank = 0;
        for (n=0; n < jdata->map->nodes->size; n++) {
            node = (prte_node_t*)pmix_pointer_array_get_item(jdata->map->nodes, n);
            if (NULL == node) {
                continue;
            }
            lrank = 0;
            nobjs = prte_hwloc_base_get_nbobjs_by_type(node->topology->topo,
                                                       options->maptype, options->cmaplvl);
            for (k=0; k < nobjs; k++) {
                obj = prte_hwloc_base_get_obj_by_type(node->topology->topo,
                                                      options->maptype, options->cmaplvl, k);
                for (m=0; m < node->procs->size; m++) {
                    proc = (prte_proc_t*)pmix_pointer_array_get_item(node->procs, m);
                    if (NULL == proc) {
                        continue;
                    }
                    if (!PMIX_CHECK_NSPACE(jdata->nspace, proc->name.nspace)) {
                        continue;
                    }
                    if (obj != proc->obj) {
                        continue;
                    }
                    /* this proc is on this object, so rank it */
                    proc->name.rank = rank;
                    proc->local_rank = lrank;
                    PMIX_RETAIN(proc);
                    rc = pmix_pointer_array_set_item(jdata->procs, proc->name.rank, proc);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_RELEASE(proc);
                        return rc;
                    }
                    rank++;
                    lrank++;
                }
            }
        }
        compute_app_rank(jdata);
        return PRTE_SUCCESS;
    }

    /* if we are ranking SPAN, we rank round-robin across the
     * all the objects on the nodes, treating all the objects as
     * being part of one giant "super-node"
     *
     * Even though we are ranking by SPAN, we cannot assume that
     * we mapped by span, and so we cannot assume that the procs
     * are in the node's proc array in object order. Hence, we have
     * to search for them even though that eats up time */
    if (PRTE_RANK_BY_SPAN == options->rank) {
        rank = 0;
        pass = 0;
        while (rank < jdata->num_procs) {
            for (n=0; n < jdata->map->nodes->size && rank < jdata->num_procs; n++) {
                node = (prte_node_t*)pmix_pointer_array_get_item(jdata->map->nodes, n);
                if (NULL == node) {
                    continue;
                }
                nobjs = prte_hwloc_base_get_nbobjs_by_type(node->topology->topo,
                                                           options->maptype, options->cmaplvl);
                lrank = pass * nobjs;
                /* make a pass across all objects on this node */
                for (k=0; k < nobjs && rank < jdata->num_procs; k++) {
                    /* get this object */
                    obj = prte_hwloc_base_get_obj_by_type(node->topology->topo,
                                                          options->maptype, options->cmaplvl, k);
                    /* find an unranked proc on this object */
                    for (m=0; m < node->procs->size && rank < jdata->num_procs; m++) {
                        proc = (prte_proc_t*)pmix_pointer_array_get_item(node->procs, m);
                        if (NULL == proc) {
                            continue;
                        }
                        if (!PMIX_CHECK_NSPACE(jdata->nspace, proc->name.nspace)) {
                            continue;
                        }
                        if (obj != proc->obj) {
                            continue;
                        }
                        if (PMIX_RANK_INVALID == proc->name.rank) {
                            proc->name.rank = rank;
                            proc->local_rank = lrank;
                            PMIX_RETAIN(proc);
                            rc = pmix_pointer_array_set_item(jdata->procs, proc->name.rank, proc);
                            if (PMIX_SUCCESS != rc) {
                                PMIX_RELEASE(proc);
                                return rc;
                            }
                            ++rank;
                            ++lrank;
                            break;
                        }
                    }
                }
            }
            ++pass;
        }
        compute_app_rank(jdata);
        return PRTE_SUCCESS;
    }

    /* cannot be anything else */
    return PRTE_ERR_NOT_IMPLEMENTED;
}

/* when we restart a process on a different node, we have to
 * ensure that the node and local ranks assigned to the proc
 * don't overlap with any pre-existing proc on that node. If
 * we don't, then it would be possible for procs to conflict
 * when opening static ports, should that be enabled.
 */
void prte_rmaps_base_update_local_ranks(prte_job_t *jdata, prte_node_t *oldnode,
                                        prte_node_t *newnode, prte_proc_t *newproc)
{
    int k;
    prte_node_rank_t node_rank;
    prte_local_rank_t local_rank;
    prte_proc_t *proc;

    PMIX_OUTPUT_VERBOSE((5, prte_rmaps_base_framework.framework_output,
                         "%s rmaps:base:update_local_ranks",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    /* if the node hasn't changed, then we can just use the
     * pre-defined values
     */
    if (oldnode == newnode) {
        return;
    }

    /* if the node has changed, then search the new node for the
     * lowest unused local and node rank
     */
    node_rank = 0;
retry_nr:
    for (k = 0; k < newnode->procs->size; k++) {
        /* if this proc is NULL, skip it */
        if (NULL == (proc = (prte_proc_t *) pmix_pointer_array_get_item(newnode->procs, k))) {
            continue;
        }
        if (node_rank == proc->node_rank) {
            node_rank++;
            goto retry_nr;
        }
    }
    newproc->node_rank = node_rank;

    local_rank = 0;
retry_lr:
    for (k = 0; k < newnode->procs->size; k++) {
        /* if this proc is NULL, skip it */
        if (NULL == (proc = (prte_proc_t *) pmix_pointer_array_get_item(newnode->procs, k))) {
            continue;
        }
        /* ignore procs from other jobs */
        if (!PMIX_CHECK_NSPACE(proc->name.nspace, jdata->nspace)) {
            continue;
        }
        if (local_rank == proc->local_rank) {
            local_rank++;
            goto retry_lr;
        }
    }
    newproc->local_rank = local_rank;
}
