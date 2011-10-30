/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/hwloc/base/base.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "rmaps_ppr.h"

static int ppr(orte_job_t *jdata);

orte_rmaps_base_module_t orte_rmaps_ppr_module = {
    ppr
};

static orte_proc_t* setup_proc(orte_job_t *jdata, orte_node_t *node,
                               orte_app_idx_t idx);
static void prune(orte_jobid_t jobid,
                  orte_app_idx_t app_idx,
                  orte_node_t *node,
                  opal_hwloc_level_t *level,
                  orte_vpid_t *nmapped);

static int ppr(orte_job_t *jdata)
{
    int rc, local_limit, j;
    orte_rmaps_ppr_component_t *c = &mca_rmaps_ppr_component;
    orte_node_t *node;
    orte_proc_t *proc;
    orte_app_context_t *app;
    orte_vpid_t total_procs, nprocs_mapped;
    hwloc_obj_t obj;
    hwloc_obj_type_t lowest;
    opal_hwloc_level_t level;
    unsigned cache_level;
    opal_list_t node_list;
    opal_list_item_t *item;
    orte_std_cntr_t num_slots;
    unsigned int nobjs, i;
    orte_app_idx_t idx;

    /* only handle initial launch of loadbalanced
     * or NPERxxx jobs - allow restarting of failed apps
     */
    if (ORTE_JOB_STATE_INIT != jdata->state) {
        opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                            "mca:rmaps:ppr: job %s not in initial state - ppr cannot map",
                            ORTE_JOBID_PRINT(jdata->jobid));
        return ORTE_ERR_TAKE_NEXT_OPTION;
    }
    if (NULL != jdata->map->req_mapper &&
        0 != strcasecmp(jdata->map->req_mapper, c->super.base_version.mca_component_name)) {
        /* a mapper has been specified, and it isn't me */
        opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                            "mca:rmaps:ppr: job %s not using ppr mapper",
                            ORTE_JOBID_PRINT(jdata->jobid));
        return ORTE_ERR_TAKE_NEXT_OPTION;
    }

    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:ppr: mapping job %s",
                        ORTE_JOBID_PRINT(jdata->jobid));
 
    /* flag that I did the mapping */
    if (NULL != jdata->map->last_mapper) {
        free(jdata->map->last_mapper);
    }
    jdata->map->last_mapper = strdup(c->super.base_version.mca_component_name);

    /* convenience */
    local_limit = mca_rmaps_ppr_component.ppr[mca_rmaps_ppr_component.start];
    level = mca_rmaps_ppr_component.start;

    /* find the lowest level that was defined in the ppr */
    lowest = opal_hwloc_levels[mca_rmaps_ppr_component.start];
    if (OPAL_HWLOC_L3CACHE_LEVEL == mca_rmaps_ppr_component.start) {
        cache_level = 3;
    } else if (OPAL_HWLOC_L2CACHE_LEVEL == mca_rmaps_ppr_component.start) {
        cache_level = 2;
    } else if (OPAL_HWLOC_L1CACHE_LEVEL == mca_rmaps_ppr_component.start) {
        cache_level = 1;
    }

    for (idx=0; idx < (orte_app_idx_t)jdata->apps->size; idx++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, idx))) {
            continue;
        }

        /* if the number of total procs was given, set that
         * limit - otherwise, set to max so we simply fill
         * all the nodes with the pattern
         */
        if (0 < app->num_procs) {
            total_procs = app->num_procs;
        } else {
            total_procs = ORTE_VPID_MAX;
        }

        /* get the available nodes */
        OBJ_CONSTRUCT(&node_list, opal_list_t);
        if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                  jdata->map->policy))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }

        /* cycle across the nodes */
        nprocs_mapped = 0;
        while (NULL != (node = (orte_node_t*)opal_list_remove_first(&node_list))) {
            /* add the node to the map */
            if (ORTE_SUCCESS > (rc = opal_pointer_array_add(jdata->map->nodes, (void*)node))) {
                ORTE_ERROR_LOG(rc);
                goto error;
            }
            OBJ_RETAIN(node);  /* maintain accounting on object */
            jdata->map->num_nodes++;
            /* if we are mapping solely at the node level, just put
             * that many procs on this node
             */
            if (HWLOC_OBJ_MACHINE == lowest) {
                for (j=0; j < local_limit && nprocs_mapped < total_procs; j++) {
                    if (NULL == (proc = setup_proc(jdata, node, idx))) {
                        rc = ORTE_ERR_OUT_OF_RESOURCE;
                        goto error;
                    }
                    nprocs_mapped++;
                    proc->locale = obj;
                }
            } else {
                /* get the number of lowest resources on this node */
                nobjs = opal_hwloc_base_get_nbobjs_by_type(node->topology,
                                                           lowest, cache_level,
                                                           OPAL_HWLOC_AVAILABLE);

                /* map the specified number of procs to each such resource on this node,
                 * recording the locale of each proc so we know its cpuset
                 */
                for (i=0; i < nobjs; i++) {
                    obj = opal_hwloc_base_get_obj_by_type(node->topology,
                                                          lowest, cache_level,
                                                          i, OPAL_HWLOC_AVAILABLE);
                    for (j=0; j < local_limit && nprocs_mapped < total_procs; j++) {
                        if (NULL == (proc = setup_proc(jdata, node, idx))) {
                            rc = ORTE_ERR_OUT_OF_RESOURCE;
                            goto error;
                        }
                        nprocs_mapped++;
                        proc->locale = obj;
                    }
                }

                if (mca_rmaps_ppr_component.pruning_reqd) {
                    /* go up the ladder and prune the procs according to
                     * the specification, adjusting the count of procs on the
                     * node as we go
                     */
                    level--;
                    prune(jdata->jobid, idx, node, &level, &nprocs_mapped);
                }
            }

            /* set the total slots used to the number of procs placed
             * on this node
             */
            node->slots_inuse = node->num_procs;

            /* if no-oversubscribe was specified, check to see if
             * we have violated the total slot specification - regardless,
             * if slots_max was given, we are not allowed to violate it!
             */
            if ((!(jdata->map->oversubscribe) && node->slots < node->slots_inuse) ||
                (0 < node->slots_max && node->slots_max < node->slots_inuse)) {
                orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:alloc-error",
                               true, node->num_procs, app->app);
                rc = ORTE_ERR_SILENT;
                goto error;
            }

            /* update the number of procs in the job and the app */
            jdata->num_procs += node->num_procs;
            app->num_procs = node->num_procs;

            /* if we haven't mapped all the procs, continue on to the
             * next node
             */
            if (total_procs == nprocs_mapped) {
                break;
            }
        }
        if (nprocs_mapped < total_procs) {
            /* couldn't map them all */
            orte_show_help("help-orte-rmaps-ppr.txt", "ppr-too-many-procs",
                           true, app->app, app->num_procs, mca_rmaps_ppr_component.given_ppr);
            rc = ORTE_ERR_SILENT;
            goto error;
        }
        /* compute vpids and add proc objects to the job */
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_vpids(jdata))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
    }


    /* compute and save local ranks */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_local_ranks(jdata))) {
        ORTE_ERROR_LOG(rc);
        goto error;
    }

    /* define the daemons that we will use for this job */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_define_daemons(jdata))) {
        ORTE_ERROR_LOG(rc);
    }

 error:
    while (NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);
    return rc;
}

static hwloc_obj_t find_split(hwloc_topology_t topo, hwloc_obj_t obj)
{
    unsigned k;
    hwloc_obj_t nxt;

    if (1 < obj->arity) {
        return obj;
    }
    for (k=0; k < obj->arity; k++) {
        nxt = find_split(topo, obj->children[k]);
        if (NULL != nxt) {
            return nxt;
        }
    }
    return NULL;
}

/* recursively climb the topology, pruning procs beyond that allowed
 * by the given ppr
 */
static void prune(orte_jobid_t jobid,
                  orte_app_idx_t app_idx,
                  orte_node_t *node,
                  opal_hwloc_level_t *level,
                  orte_vpid_t *nmapped)
{
    hwloc_obj_t obj, top;
    unsigned int i, nobjs;
    hwloc_obj_type_t lvl;
    unsigned cache_level, k;
    int nprocs;
    hwloc_cpuset_t avail, cpus, childcpus;
    int n, limit, nmax, nunder, idx, idxmax;
    orte_proc_t *proc, *pptr, *procmax;
    opal_hwloc_level_t ll;
    char dang[64];

    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:ppr: pruning level %d",
                        *level);

    /* convenience */
    ll = *level;

    /* convenience */
    lvl = opal_hwloc_levels[ll];
    limit = mca_rmaps_ppr_component.ppr[ll];

    if (0 == limit) {
        /* no limit at this level, so move up if necessary */
        if (0 == ll) {
            /* done */
            return;
        }
        *level -= 1;
        prune(jobid, app_idx, node, level, nmapped);
        return;
    }

    /* handle the darn cache thing again */
    if (OPAL_HWLOC_L3CACHE_LEVEL == ll) {
        cache_level = 3;
    } else if (OPAL_HWLOC_L2CACHE_LEVEL == ll) {
        cache_level = 2;
    } else if (OPAL_HWLOC_L1CACHE_LEVEL == ll) {
        cache_level = 1;
    }

    /* get the number of resources at this level on this node */
    nobjs = opal_hwloc_base_get_nbobjs_by_type(node->topology,
                                               lvl, cache_level,
                                               OPAL_HWLOC_AVAILABLE);

    /* for each resource, compute the number of procs sitting
     * underneath it and check against the limit
     */
    for (i=0; i < nobjs; i++) {
        obj = opal_hwloc_base_get_obj_by_type(node->topology,
                                              lvl, cache_level,
                                              i, OPAL_HWLOC_AVAILABLE);
        /* get the available cpuset */
        avail = opal_hwloc_base_get_available_cpus(node->topology, obj);

        /* look at the intersection of this object's cpuset and that
         * of each proc in the job/app - if they intersect, then count this proc
         * against the limit
         */
        nprocs = 0;
        for (n=0; n < node->procs->size; n++) {
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, n))) {
                continue;
            }
            if (proc->name.jobid != jobid ||
                proc->app_idx != app_idx) {
                continue;
            }
            cpus = opal_hwloc_base_get_available_cpus(node->topology, proc->locale);
            if (hwloc_bitmap_intersects(avail, cpus)) {
                nprocs++;
            }
        }
        opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                            "mca:rmaps:ppr: found %d procs limit %d",
                            nprocs, limit);

        /* check against the limit */
        while (limit < nprocs) {
            /* need to remove procs - do this in a semi-intelligent
             * manner to provide a little load balancing by cycling
             * across the objects beneath this one, removing procs
             * in a round-robin fashion until the limit is satisfied
             *
             * NOTE: I'm sure someone more knowledgeable with hwloc
             * will come up with a more efficient way to do this, so
             * consider this is a starting point
             */

            /* find the first level that has more than
             * one child beneath it - if all levels
             * have only one child, then return this
             * object
             */
            top = find_split(node->topology, obj);
            hwloc_obj_type_snprintf(dang, 64, top, 1);
            opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                                "mca:rmaps:ppr: SPLIT AT LEVEL %s", dang);

            /* cycle across the children of this object */
            nmax = 0;
            procmax = NULL;
            idx = 0;
            /* find the child with the most procs underneath it */
            for (k=0; k < top->arity && limit < nprocs; k++) {
                /* get this object's available cpuset */
                childcpus = opal_hwloc_base_get_available_cpus(node->topology, top->children[k]);
                nunder = 0;
                pptr = NULL;
                for (n=0; n < node->procs->size; n++) {
                    if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, n))) {
                        continue;
                    }
                    if (proc->name.jobid != jobid ||
                        proc->app_idx != app_idx) {
                        continue;
                    }
                    cpus = opal_hwloc_base_get_available_cpus(node->topology, proc->locale);
                    if (hwloc_bitmap_intersects(childcpus, cpus)) {
                        nunder++;
                        if (NULL == pptr) {
                            /* save the location of the first proc under this object */
                            pptr = proc;
                            idx = n;
                        }
                    }
                }
                if (nmax < nunder) {
                    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                                        "mca:rmaps:ppr: PROCS UNDER CHILD %d %d MAX %d",
                                        k, nunder, nmax);
                    nmax = nunder;
                    procmax = pptr;
                    idxmax = idx;
                }
            }
            if (NULL == procmax) {
                /* can't find anything to remove - error out */
                goto error;
            }
            /* remove it */
            opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                                "mca:rmaps:ppr: removing proc at posn %d",
                                idxmax);
            opal_pointer_array_set_item(node->procs, idxmax, NULL);
            node->num_procs--;
            nprocs--;
            *nmapped -= 1;
            OBJ_RELEASE(procmax);
        }
    }
    /* finished with this level - move up if necessary */
    if (0 == ll) {
        return;
    }
    *level -= 1;
    prune(jobid, app_idx, node, level, nmapped);
    return;

 error:
    opal_output(0, "INFINITE LOOP");
}

static orte_proc_t* setup_proc(orte_job_t *jdata,
                               orte_node_t *node,
                               orte_app_idx_t idx)
{
    orte_proc_t *proc;
    int rc;

    proc = OBJ_NEW(orte_proc_t);
    /* set the jobid */
    proc->name.jobid = jdata->jobid;
    /* we do not set the vpid here - this will be done
     * during a second phase, but we do set the epoch here
     since they all start with the same value. */
    ORTE_EPOCH_SET(proc->name.epoch,ORTE_EPOCH_MIN);
    /* flag the proc as ready for launch */
    proc->state = ORTE_PROC_STATE_INIT;
    proc->app_idx = idx;

    OBJ_RETAIN(node);  /* maintain accounting on object */    
    proc->node = node;
    proc->nodename = node->name;
    node->num_procs++;
    if (0 > (rc = opal_pointer_array_add(node->procs, (void*)proc))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(proc);
        return NULL;
    }
    /* retain the proc struct so that we correctly track its release */
    OBJ_RETAIN(proc);

    return proc;
}
