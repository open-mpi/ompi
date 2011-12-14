/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2011 Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <string.h>

#include "opal/util/output.h"
#include "opal/mca/hwloc/base/base.h"

#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "rmaps_rr.h"

int orte_rmaps_rr_byslot(orte_job_t *jdata,
                         orte_app_context_t *app,
                         opal_list_t *node_list,
                         orte_std_cntr_t num_slots,
                         orte_vpid_t num_procs)
{
    int rc, i, nprocs_mapped;
    orte_node_t *node;
    orte_proc_t *proc;
    opal_list_item_t *item;
    int num_procs_to_assign, extra_procs_to_assign=0, nxtra_nodes=0;
#if OPAL_HAVE_HWLOC
    hwloc_obj_t obj=NULL;
#endif
    float balance;
    bool add_one=false;
    bool oversubscribed = false;

    opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:rr: mapping by slot for job %s slots %d num_procs %lu",
                        ORTE_JOBID_PRINT(jdata->jobid), (int)num_slots, (unsigned long)num_procs);

    /* check to see if we can map all the procs */
    if (num_slots < (int)app->num_procs) {
        if (ORTE_MAPPING_NO_OVERSUBSCRIBE & ORTE_GET_MAPPING_DIRECTIVE(jdata->map->mapping)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:alloc-error",
                           true, app->num_procs, app->app);
            return ORTE_ERR_SILENT;
        }
        oversubscribed = true;
    }

    /* first pass: map the number of procs to each node until we
     * map all specified procs or use all allocated slots
     */
    nprocs_mapped = 0;
    for (item = opal_list_get_first(node_list);
         item != opal_list_get_end(node_list);
         item = opal_list_get_next(item)) {
        node = (orte_node_t*)item;
        opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                            "mca:rmaps:rr:slot working node %s",
                            node->name);
#if OPAL_HAVE_HWLOC
        /* get the root object as we are not assigning
         * locale except at the node level
         */
        if (NULL != node->topology) {
            obj = hwloc_get_root_obj(node->topology);
        }
#endif
        if (node->slots_alloc <= node->slots_inuse) {
            opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                                "mca:rmaps:rr:slot working node %s is full - skipping",
                                node->name);
            continue;
        }
        num_procs_to_assign = node->slots_alloc - node->slots_inuse;
        for (i=0; i < num_procs_to_assign && nprocs_mapped < app->num_procs; i++) {
            /* add this node to the map - do it only once */
            if (!node->mapped) {
                if (ORTE_SUCCESS > (rc = opal_pointer_array_add(jdata->map->nodes, (void*)node))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                node->mapped = true;
                OBJ_RETAIN(node);  /* maintain accounting on object */
                ++(jdata->map->num_nodes);
            }
            if (NULL == (proc = orte_rmaps_base_setup_proc(jdata, node, app->idx))) {
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            nprocs_mapped++;
#if OPAL_HAVE_HWLOC
            proc->locale = obj;
#endif
        }
    }

    if (nprocs_mapped == app->num_procs) {
        /* we are done */
        return ORTE_SUCCESS;
    }

    opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:rr:slot job %s is oversubscribed - performing second pass",
                        ORTE_JOBID_PRINT(jdata->jobid));

    /* second pass: if we haven't mapped everyone yet, it is
     * because we are oversubscribed. Figure out how many procs
     * to add
     */
    balance = (float)(app->num_procs - nprocs_mapped) / (float)opal_list_get_size(node_list);
    extra_procs_to_assign = (int)balance;
    if (0 < (balance - (float)extra_procs_to_assign)) {
        /* compute how many nodes need an extra proc */
        nxtra_nodes = app->num_procs - nprocs_mapped - (extra_procs_to_assign * opal_list_get_size(node_list));
        /* add one so that we add an extra proc to the first nodes
         * until all procs are mapped
         */
        extra_procs_to_assign++;
        /* flag that we added one */
        add_one = true;
    }

    for (item = opal_list_get_first(node_list);
         item != opal_list_get_end(node_list);
         item = opal_list_get_next(item)) {
        node = (orte_node_t*)item;
        opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                            "mca:rmaps:rr:slot working node %s",
                            node->name);
#if OPAL_HAVE_HWLOC
        /* get the root object as we are not assigning
         * locale except at the node level
         */
        if (NULL != node->topology) {
            obj = hwloc_get_root_obj(node->topology);
        }
#endif
        /* add this node to the map - do it only once */
        if (!node->mapped) {
            if (ORTE_SUCCESS > (rc = opal_pointer_array_add(jdata->map->nodes, (void*)node))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            node->mapped = true;
            OBJ_RETAIN(node);  /* maintain accounting on object */
            ++(jdata->map->num_nodes);
        }
        if (add_one) {
            if (0 == nxtra_nodes) {
                --extra_procs_to_assign;
                add_one = false;
            } else {
                --nxtra_nodes;
            }
        }
        num_procs_to_assign = (node->slots_alloc - node->slots_inuse) + extra_procs_to_assign;
        opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                            "mca:rmaps:rr:slot adding up to %d procs to node %s",
                            num_procs_to_assign, node->name);
        for (i=0; i < num_procs_to_assign && nprocs_mapped < app->num_procs; i++) {
            if (NULL == (proc = orte_rmaps_base_setup_proc(jdata, node, app->idx))) {
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            nprocs_mapped++;
#if OPAL_HAVE_HWLOC
            proc->locale = obj;
#endif
        }
        /* not all nodes are equal, so only set oversubscribed for
         * this node if it is in that state
         */
        if (node->slots_alloc < (int)node->num_procs) {
            /* flag the node as oversubscribed so that sched-yield gets
             * properly set
             */
            node->oversubscribed = true;
        }
    }
    return ORTE_SUCCESS;
}

int orte_rmaps_rr_bynode(orte_job_t *jdata,
                         orte_app_context_t *app,
                         opal_list_t *node_list,
                         orte_std_cntr_t num_slots,
                         orte_vpid_t num_procs)
{
    int j, nprocs_mapped, lag, delta;
    orte_node_t *node;
    orte_proc_t *proc;
    opal_list_item_t *item;
    int num_procs_to_assign, navg, idx;
    int extra_procs_to_assign=0, nxtra_nodes=0;
#if OPAL_HAVE_HWLOC
    hwloc_obj_t obj=NULL;
#endif
    float balance;
    bool add_one=false;
    bool oversubscribed=false;

    opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:rr: mapping by node for job %s slots %d num_procs %lu",
                        ORTE_JOBID_PRINT(jdata->jobid),
                        (int)num_slots, (unsigned long)num_procs);

    /* quick check to see if we can map all the procs */
    if (num_slots < (int)app->num_procs) {
        if (ORTE_MAPPING_NO_OVERSUBSCRIBE & ORTE_GET_MAPPING_DIRECTIVE(jdata->map->mapping)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:alloc-error",
                           true, app->num_procs, app->app);
            return ORTE_ERR_SILENT;
        }
        oversubscribed = true;
    }

    /* divide the procs evenly across all nodes - this is the
     * average we have to maintain as we go, but we adjust
     * the number on each node to reflect its available slots.
     * Obviously, if all nodes have the same number of slots,
     * then the avg is what we get on each node - this is
     * the most common situation.
     */
    navg = app->num_procs / opal_list_get_size(node_list);
    if (0 == navg) {
        /* if there are less procs than nodes, we have to
         * place at least one/node
         */
        navg = 1;
    }

    /* compute how many extra procs to put on each node */
    balance = (float)(app->num_procs - (navg * (float)opal_list_get_size(node_list))) / (float)opal_list_get_size(node_list);
    extra_procs_to_assign = (int)balance;
    if (0 < (balance - (float)extra_procs_to_assign)) {
        /* compute how many nodes need an extra proc */
        nxtra_nodes = app->num_procs - ((navg + extra_procs_to_assign) * opal_list_get_size(node_list));
        /* add one so that we add an extra proc to the first nodes
         * until all procs are mapped
         */
        extra_procs_to_assign++;
        /* flag that we added one */
        add_one = true;
    }

    opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:rr: mapping by node navg %d extra_procs %d extra_nodes %d",
                        navg, extra_procs_to_assign, nxtra_nodes);

    nprocs_mapped = 0;
    lag = 0;
    for (item = opal_list_get_first(node_list);
         item != opal_list_get_end(node_list);
         item = opal_list_get_next(item)) {
        node = (orte_node_t*)item;
#if OPAL_HAVE_HWLOC
        /* get the root object as we are not assigning
         * locale except at the node level
         */
        if (NULL != node->topology) {
            obj = hwloc_get_root_obj(node->topology);
        }
#endif
        /* add this node to the map, but only do so once */
        if (!node->mapped) {
            if (ORTE_SUCCESS > (idx = opal_pointer_array_add(jdata->map->nodes, (void*)node))) {
                ORTE_ERROR_LOG(idx);
                return idx;
            }
            node->mapped = true;
            OBJ_RETAIN(node);  /* maintain accounting on object */
            ++(jdata->map->num_nodes);
        }
        /* compute the number of procs to go on this node */
        if (add_one) {
            if (0 == nxtra_nodes) {
                --extra_procs_to_assign;
                add_one = false;
            } else {
                --nxtra_nodes;
            }
        }
        if (oversubscribed) {
            /* everybody just takes their share */
            num_procs_to_assign = navg + extra_procs_to_assign;
        } else {
            /* if we are not oversubscribed, then there are enough
             * slots to handle all the procs. However, not every
             * node will have the same number of slots, so we
             * have to track how many procs to "shift" elsewhere
             * to make up the difference
             */
            if (node->slots_alloc <= node->slots_inuse) {
                /* if there are no extras to take, then we can
                 * ignore this node
                 */
                if (0 == extra_procs_to_assign) {
                    /* update how many we are lagging behind */
                    lag += navg;
                    continue;
                }
                /* everybody has to take at least the extras */
                num_procs_to_assign = extra_procs_to_assign;
                /* update how many we are lagging behind */
                lag += navg;
            } else {
                /* if slots_alloc < avg, then take all */
                if ((node->slots_alloc - node->slots_inuse) < navg) {
                    num_procs_to_assign = (node->slots_alloc - node->slots_inuse) + extra_procs_to_assign;
                    /* update how many we are lagging behind */
                    lag += navg - (node->slots_alloc - node->slots_inuse);
                } else {
                    /* take the avg plus as much of the "lag" as we can */
                    delta = 0;
                    if (0 < lag) {
                        delta = (node->slots_alloc - node->slots_inuse) - navg;
                        if (lag < delta) {
                            delta = lag;
                        }
                        lag -= delta;
                    }
                    num_procs_to_assign = navg + delta + extra_procs_to_assign;
                }
            }
        }
        for (j=0; j < num_procs_to_assign && nprocs_mapped < app->num_procs; j++) {
            if (NULL == (proc = orte_rmaps_base_setup_proc(jdata, node, app->idx))) {
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            nprocs_mapped++;
#if OPAL_HAVE_HWLOC
            proc->locale = obj;
#endif
        }
        /* not all nodes are equal, so only set oversubscribed for
         * this node if it is in that state
         */
        if (node->slots_alloc < (int)node->num_procs) {
            /* flag the node as oversubscribed so that sched-yield gets
             * properly set
             */
            node->oversubscribed = true;
        }
        if (nprocs_mapped == app->num_procs) {
            /* we are done */
            break;
        }
    }

    return ORTE_SUCCESS;
}

#if OPAL_HAVE_HWLOC
static  int byobj_span(orte_job_t *jdata,
                       orte_app_context_t *app,
                       opal_list_t *node_list,
                       orte_std_cntr_t num_slots,
                       orte_vpid_t num_procs,
                       hwloc_obj_type_t target, unsigned cache_level);

/* mapping by hwloc object looks a lot like mapping by node,
 * but has the added complication of possibly having different
 * numbers of objects on each node
 */
int orte_rmaps_rr_byobj(orte_job_t *jdata,
                        orte_app_context_t *app,
                        opal_list_t *node_list,
                        orte_std_cntr_t num_slots,
                        orte_vpid_t num_procs,
                        hwloc_obj_type_t target, unsigned cache_level)
{
    int i, j, nprocs_mapped;
    orte_node_t *node;
    orte_proc_t *proc;
    opal_list_item_t *item;
    int num_procs_to_assign, nperobj, nprocs, nxtra_objs=0;
    int extra_procs_to_assign=0, nxtra_nodes=0, idx;
    hwloc_obj_t obj=NULL;
    unsigned int nobjs;
    float balance;
    bool add_one=false;
    bool oversubscribed = false;

    /* there are two modes for mapping by object: span and not-span. The
     * span mode essentially operates as if there was just a single
     * "super-node" in the system - i.e., it balances the load across
     * all objects of the indicated type regardless of their location.
     * In essence, it acts as if we placed one proc on each object, cycling
     * across all objects on all nodes, and then wrapped around to place
     * another proc on each object, doing so until all procs were placed.
     *
     * In contrast, the non-span mode operates similar to byslot mapping.
     * All slots on each node are filled, assigning each proc to an object
     * on that node in a balanced fashion, and then the mapper moves on
     * to the next node. Thus, procs tend to be "front loaded" onto the
     * list of nodes, as opposed to being "load balanced" in the span mode
     */
    if (ORTE_MAPPING_SPAN & jdata->map->mapping) {
        return byobj_span(jdata, app, node_list, num_slots,
                          num_procs, target, cache_level);
    }

    opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:rr: mapping no-span by %s for job %s slots %d num_procs %lu",
                        hwloc_obj_type_string(target),
                        ORTE_JOBID_PRINT(jdata->jobid),
                        (int)num_slots, (unsigned long)num_procs);

    /* quick check to see if we can map all the procs - can't
     * do more because we don't know how many total objects exist
     * across all the nodes
     */
    if (num_slots < (int)app->num_procs) {
        if (ORTE_MAPPING_NO_OVERSUBSCRIBE & ORTE_GET_MAPPING_DIRECTIVE(jdata->map->mapping)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:alloc-error",
                           true, app->num_procs, app->app);
            return ORTE_ERR_SILENT;
        }
        oversubscribed = true;
        /* compute how many extra procs to put on each node */
        balance = (float)((jdata->num_procs + app->num_procs) - num_slots) / (float)opal_list_get_size(node_list);
        extra_procs_to_assign = (int)balance;
        if (0 < (balance - (float)extra_procs_to_assign)) {
            /* compute how many nodes need an extra proc */
            nxtra_nodes = (jdata->num_procs + app->num_procs) - num_slots - (extra_procs_to_assign * opal_list_get_size(node_list));
            /* add one so that we add an extra proc to the first nodes
             * until all procs are mapped
             */
            extra_procs_to_assign++;
            /* flag that we added one */
            add_one = true;
        }
    }

    opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:rr: mapping no-span by %s extra_procs %d extra_nodes %d",
                        hwloc_obj_type_string(target),
                        extra_procs_to_assign, nxtra_nodes);

    nprocs_mapped = 0;
    for (item = opal_list_get_first(node_list);
         item != opal_list_get_end(node_list);
         item = opal_list_get_next(item)) {
        node = (orte_node_t*)item;
        /* bozo check */
        if (NULL == node->topology) {
            orte_show_help("help-orte-rmaps-ppr.txt", "ppr-topo-missing",
                           true, node->name);
            return ORTE_ERR_SILENT;
        }
        /* add this node to the map, if reqd */
        if (!node->mapped) {
            if (ORTE_SUCCESS > (idx = opal_pointer_array_add(jdata->map->nodes, (void*)node))) {
                ORTE_ERROR_LOG(idx);
                return idx;
            }
            node->mapped = true;
            OBJ_RETAIN(node);  /* maintain accounting on object */
            ++(jdata->map->num_nodes);
        }

         /* compute the number of procs to go on this node */
        if (add_one) {
            if (0 == nxtra_nodes) {
                --extra_procs_to_assign;
                add_one = false;
            } else {
                --nxtra_nodes;
            }
        }
        if (node->slots_alloc <= node->slots_inuse) {
            /* everybody takes at least the extras */
            num_procs_to_assign = extra_procs_to_assign;
        } else {
            num_procs_to_assign = (node->slots_alloc - node->slots_inuse) + extra_procs_to_assign;
        }

        /* get the number of objects of this type on this node */
        nobjs = opal_hwloc_base_get_nbobjs_by_type(node->topology, target, cache_level, OPAL_HWLOC_AVAILABLE);
        opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                            "mca:rmaps:rr:byobj: found %d objs on node %s", nobjs, node->name);
        /* compute the number of procs to go on each object */
        nperobj = num_procs_to_assign / nobjs;
        opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                            "mca:rmaps:rr:byobj: placing %d procs on each object", nperobj);
        if ((int)(nperobj * nobjs) < num_procs_to_assign) {
            /* compute how many objs need an extra proc */
            nxtra_objs = num_procs_to_assign - nperobj * nobjs;
            opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                                "mca:rmaps:rr:byobj: adding 1 extra proc to the first %d objects, if needed", nxtra_objs);
        }
        /* loop through the number of objects */
        for (i=0; i < (int)nobjs && nprocs_mapped < (int)app->num_procs; i++) {
            /* get the hwloc object */
            if (NULL == (obj = opal_hwloc_base_get_obj_by_type(node->topology, target, cache_level, i, OPAL_HWLOC_AVAILABLE))) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                return ORTE_ERR_NOT_FOUND;
            }
            /* map the reqd number of procs */
            if (0 < nxtra_objs) {
                nprocs = nperobj + 1;
                --nxtra_objs;
            } else {
                nprocs = nperobj;
            }
            for (j=0; j < nprocs && nprocs_mapped < app->num_procs; j++) {
                if (NULL == (proc = orte_rmaps_base_setup_proc(jdata, node, app->idx))) {
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                nprocs_mapped++;
                proc->locale = obj;
            }
        }
        /* not all nodes are equal, so only set oversubscribed for
         * this node if it is in that state
         */
        if (node->slots_alloc < (int)node->num_procs) {
            /* flag the node as oversubscribed so that sched-yield gets
             * properly set
             */
            node->oversubscribed = true;
        }
        if (nprocs_mapped == app->num_procs) {
            /* we are done */
            break;
        }
    }

    return ORTE_SUCCESS;
}

static int byobj_span(orte_job_t *jdata,
                      orte_app_context_t *app,
                      opal_list_t *node_list,
                      orte_std_cntr_t num_slots,
                      orte_vpid_t num_procs,
                      hwloc_obj_type_t target, unsigned cache_level)
{
    int i, j, nprocs_mapped, lag, delta, navg;
    orte_node_t *node;
    orte_proc_t *proc;
    opal_list_item_t *item;
    int num_procs_to_assign, nperobj, nprocs, nxtra_objs=0;
    int extra_procs_to_assign=0, nxtra_nodes=0, idx;
    hwloc_obj_t obj=NULL;
    unsigned int nobjs;
    float balance;
    bool add_one=false;
    bool oversubscribed=false;

    opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:rr: mapping span by %s for job %s slots %d num_procs %lu",
                        hwloc_obj_type_string(target),
                        ORTE_JOBID_PRINT(jdata->jobid),
                        (int)num_slots, (unsigned long)num_procs);

    /* quick check to see if we can map all the procs - can't
     * do more because we don't know how many total objects exist
     * across all the nodes
     */
    if (num_slots < (int)app->num_procs) {
        if (ORTE_MAPPING_NO_OVERSUBSCRIBE & ORTE_GET_MAPPING_DIRECTIVE(jdata->map->mapping)) {
            orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:alloc-error",
                           true, app->num_procs, app->app);
            return ORTE_ERR_SILENT;
        }
        oversubscribed = true;
    }

    /* divide the procs evenly across all nodes - this is the
     * average we have to maintain as we go, but we adjust
     * the number on each node to reflect its available slots.
     * Obviously, if all nodes have the same number of slots,
     * then the avg is what we get on each node - this is
     * the most common situation.
     */
    navg = app->num_procs / opal_list_get_size(node_list);
    if (0 == navg) {
        /* if there are less procs than nodes, we have to
         * place at least one/node
         */
        navg = 1;
    }


    /* compute how many extra procs to put on each node */
    balance = (float)((jdata->num_procs + app->num_procs) - (navg * opal_list_get_size(node_list))) / (float)opal_list_get_size(node_list);
    extra_procs_to_assign = (int)balance;
    if (0 < (balance - (float)extra_procs_to_assign)) {
        /* compute how many nodes need an extra proc */
        nxtra_nodes = (jdata->num_procs + app->num_procs) - ((navg + extra_procs_to_assign) * opal_list_get_size(node_list));
        /* add one so that we add an extra proc to the first nodes
         * until all procs are mapped
         */
        extra_procs_to_assign++;
        /* flag that we added one */
        add_one = true;
    }

    opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                        "mca:rmaps:rr: mapping by %s navg %d extra_procs %d extra_nodes %d",
                        hwloc_obj_type_string(target),
                        navg, extra_procs_to_assign, nxtra_nodes);

    nprocs_mapped = 0;
    lag = 0;
    for (item = opal_list_get_first(node_list);
         item != opal_list_get_end(node_list);
         item = opal_list_get_next(item)) {
        node = (orte_node_t*)item;
        /* bozo check */
        if (NULL == node->topology) {
            orte_show_help("help-orte-rmaps-ppr.txt", "ppr-topo-missing",
                           true, node->name);
            return ORTE_ERR_SILENT;
        }
        /* add this node to the map, if reqd */
        if (!node->mapped) {
            if (ORTE_SUCCESS > (idx = opal_pointer_array_add(jdata->map->nodes, (void*)node))) {
                ORTE_ERROR_LOG(idx);
                return idx;
            }
            node->mapped = true;
            OBJ_RETAIN(node);  /* maintain accounting on object */
            ++(jdata->map->num_nodes);
        }
        /* compute the number of procs to go on this node */
        if (add_one) {
            if (0 == nxtra_nodes) {
                --extra_procs_to_assign;
                add_one = false;
            } else {
                --nxtra_nodes;
            }
        }
        if (oversubscribed) {
            /* everybody just takes their share */
            num_procs_to_assign = navg + extra_procs_to_assign;
        } else {
            /* if we are not oversubscribed, then there are enough
             * slots to handle all the procs. However, not every
             * node will have the same number of slots, so we
             * have to track how many procs to "shift" elsewhere
             * to make up the difference
             */
            if (node->slots_alloc <= node->slots_inuse) {
                /* if there are no extras to take, then we can
                 * safely remove this node as we don't need it
                 */
                if (0 == extra_procs_to_assign) {
                    opal_pointer_array_set_item(jdata->map->nodes, idx, NULL);
                    OBJ_RELEASE(node);
                    --(jdata->map->num_nodes);
                    /* update how many we are lagging behind */
                    lag += navg;
                    continue;
                }
                /* everybody has to take at least the extras */
                num_procs_to_assign = extra_procs_to_assign;
                /* update how many we are lagging behind */
                lag += navg;
            } else {
                /* if slots_alloc < avg, then take all */
                if ((node->slots_alloc - node->slots_inuse) < navg) {
                    num_procs_to_assign = (node->slots_alloc - node->slots_inuse) + extra_procs_to_assign;
                    /* update how many we are lagging behind */
                    lag += navg - (node->slots_alloc - node->slots_inuse);
                } else {
                    /* take the avg plus as much of the "lag" as we can */
                    delta = 0;
                    if (0 < lag) {
                        delta = (node->slots_alloc - node->slots_inuse) - navg;
                        if (lag < delta) {
                            delta = lag;
                        }
                        lag -= delta;
                    }
                    num_procs_to_assign = navg + delta + extra_procs_to_assign;
                }
            }
        }

        /* get the number of objects of this type on this node */
        nobjs = opal_hwloc_base_get_nbobjs_by_type(node->topology, target, cache_level, OPAL_HWLOC_AVAILABLE);
        opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                            "mca:rmaps:rr:byobj: found %d objs on node %s", nobjs, node->name);
        /* compute the number of procs to go on each object */
        nperobj = num_procs_to_assign / nobjs;
        opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                            "mca:rmaps:rr:byobj: placing %d procs on each object", nperobj);
        if ((int)(nperobj * nobjs) < num_procs_to_assign) {
            /* compute how many objs need an extra proc */
            nxtra_objs = num_procs_to_assign - nperobj * nobjs;
            opal_output_verbose(2, orte_rmaps_base.rmaps_output,
                                "mca:rmaps:rr:byobj: adding 1 extra proc to the first %d objects, if needed", nxtra_objs);
        }
        /* loop through the number of objects */
        for (i=0; i < (int)nobjs && nprocs_mapped < (int)app->num_procs; i++) {
            /* get the hwloc object */
            if (NULL == (obj = opal_hwloc_base_get_obj_by_type(node->topology, target, cache_level, i, OPAL_HWLOC_AVAILABLE))) {
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                return ORTE_ERR_NOT_FOUND;
            }
            /* map the reqd number of procs */
            if (0 < nxtra_objs) {
                nprocs = nperobj + 1;
                --nxtra_objs;
            } else {
                nprocs = nperobj;
            }
            for (j=0; j < nprocs && nprocs_mapped < app->num_procs; j++) {
                if (NULL == (proc = orte_rmaps_base_setup_proc(jdata, node, app->idx))) {
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                nprocs_mapped++;
                proc->locale = obj;
            }
            /* keep track of the node we last used */
            jdata->bookmark = node;
        }
        /* not all nodes are equal, so only set oversubscribed for
         * this node if it is in that state
         */
        if (node->slots_alloc < (int)node->num_procs) {
            /* flag the node as oversubscribed so that sched-yield gets
             * properly set
             */
            node->oversubscribed = true;
        }
        if (nprocs_mapped == app->num_procs) {
            /* we are done */
            break;
        }
    }

    return ORTE_SUCCESS;
}
#endif

