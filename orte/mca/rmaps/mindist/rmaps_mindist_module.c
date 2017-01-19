/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2011 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2016 Intel, Inc. All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include <string.h>

#include "opal/mca/base/mca_base_var.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/error_strings.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rmaps/mindist/rmaps_mindist.h"

static int mindist_map(orte_job_t *jdata);

orte_rmaps_base_module_t orte_rmaps_mindist_module = {
    mindist_map
};

/*
 * Create a round-robin mapping for the job.
 */
static int mindist_map(orte_job_t *jdata)
{
    orte_app_context_t *app;
    int i, j;
    unsigned int k;
    hwloc_obj_t obj = NULL;
    opal_list_t node_list;
    opal_list_t numa_list;
    opal_list_item_t *item;
    opal_list_item_t *numa_item;
    opal_rmaps_numa_node_t *numa;
    orte_node_t *node;
    orte_proc_t *proc;
    int nprocs_mapped;
    int extra_procs, navg, nextra=0;
    orte_std_cntr_t num_nodes, num_slots;
    unsigned int npus, total_npus, num_procs_to_assign=0, required;
    int rc;
    mca_base_component_t *c = &mca_rmaps_mindist_component.base_version;
    bool initial_map=true;
    bool bynode = false;
    int ret;

    /* this mapper can only handle initial launch
     * when mindist mapping is desired
     */
    if (ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_RESTART)) {
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:mindist: job %s is being restarted - mindist cannot map",
                            ORTE_JOBID_PRINT(jdata->jobid));
        return ORTE_ERR_TAKE_NEXT_OPTION;
    }
    if (NULL != jdata->map->req_mapper &&
        0 != strcasecmp(jdata->map->req_mapper, c->mca_component_name)) {
        /* a mapper has been specified, and it isn't me */
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:mindist: job %s not using mindist mapper",
                            ORTE_JOBID_PRINT(jdata->jobid));
        return ORTE_ERR_TAKE_NEXT_OPTION;
    }
    if (ORTE_MAPPING_BYDIST != ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
        /* not me */
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:mindist: job %s not using mindist mapper",
                            ORTE_JOBID_PRINT(jdata->jobid));
        return ORTE_ERR_TAKE_NEXT_OPTION;
    }

    /* there are two modes for mapping by dist: span and not-span. The
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
        /* do a bynode mapping */
        bynode = true;
    } else {
        /* do a byslot mapping */
        bynode = false;
    }

    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:mindist: mapping job %s",
                        ORTE_JOBID_PRINT(jdata->jobid));

    /* flag that I did the mapping */
    if (NULL != jdata->map->last_mapper) {
        free(jdata->map->last_mapper);
    }
    jdata->map->last_mapper = strdup(c->mca_component_name);

    /* start at the beginning... */
    jdata->num_procs = 0;

    /* cycle through the app_contexts, mapping them sequentially */
    for(i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }

        /* setup the nodelist here in case we jump to error */
        OBJ_CONSTRUCT(&node_list, opal_list_t);

        /* if the number of processes wasn't specified, then we know there can be only
         * one app_context allowed in the launch, and that we are to launch it across
         * all available slots. We'll double-check the single app_context rule first
         */
        if (0 == app->num_procs && 1 < jdata->num_apps) {
            orte_show_help("help-orte-rmaps-md.txt", "multi-apps-and-zero-np",
                           true, jdata->num_apps, NULL);
            rc = ORTE_ERR_SILENT;
            goto error;
        }

        /* for each app_context, we have to get the list of nodes that it can
         * use since that can now be modified with a hostfile and/or -host
         * option
         */
        if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                  jdata->map->mapping, initial_map, false))) {
            ORTE_ERROR_LOG(rc);
            goto error;
        }
        num_nodes = (orte_std_cntr_t)opal_list_get_size(&node_list);
        /* flag that all subsequent requests should not reset the node->mapped flag */
        initial_map = false;

        /* if a bookmark exists from some prior mapping, set us to start there */
        jdata->bookmark = orte_rmaps_base_get_starting_point(&node_list, jdata);

        if (0 == app->num_procs) {
            /* set the num_procs to equal the number of slots on these mapped nodes */
            app->num_procs = num_slots;
        }

        nprocs_mapped = 0;
        if (!num_nodes) {
            rc = ORTE_ERR_SILENT;
            goto error;
        }
        if (bynode) {
            /* calculate num_procs_to_assign for bynode case */
            navg = app->num_procs / num_nodes;
            nextra = app->num_procs - navg * num_nodes;
            num_procs_to_assign = navg;
            if (nextra > 0)
                num_procs_to_assign++;
        }

        /* iterate through the list of nodes */
        for (item = opal_list_get_first(&node_list);
                item != opal_list_get_end(&node_list);
                item = opal_list_get_next(item)) {
            node = (orte_node_t*)item;

            if (NULL == node->topology) {
                orte_show_help("help-orte-rmaps-base.txt", "rmaps:no-topology",
                        true, node->name);
                rc = ORTE_ERR_SILENT;
                goto error;
            }
            /* get the root object as we are not assigning
             * locale except at the node level
             */
            obj = hwloc_get_root_obj(node->topology);
            if (NULL == obj) {
                orte_show_help("help-orte-rmaps-base.txt", "rmaps:no-topology",
                        true, node->name);
                rc = ORTE_ERR_SILENT;
                goto error;
            }

            /* get the number of available pus */
            if (opal_hwloc_use_hwthreads_as_cpus) {
                total_npus = opal_hwloc_base_get_nbobjs_by_type(node->topology, HWLOC_OBJ_PU, 0, OPAL_HWLOC_AVAILABLE);
            } else {
                total_npus = opal_hwloc_base_get_nbobjs_by_type(node->topology, HWLOC_OBJ_CORE, 0, OPAL_HWLOC_AVAILABLE);
            }
            if (bynode) {
                if (total_npus < num_procs_to_assign * orte_rmaps_base.cpus_per_rank) {
                    /* check if oversubscribing is allowed */
                    if (ORTE_MAPPING_NO_OVERSUBSCRIBE & ORTE_GET_MAPPING_DIRECTIVE(jdata->map->mapping)) {
                        orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:alloc-error",
                                true, app->num_procs, app->app);
                        rc = ORTE_ERR_SILENT;
                        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
                        goto error;
                    } else {
                        ORTE_FLAG_SET(node, ORTE_NODE_FLAG_OVERSUBSCRIBED);
                    }
                }
            }
            /* first we need to fill summary object for root with information about nodes
             * so we call opal_hwloc_base_get_nbobjs_by_type */
            opal_hwloc_base_get_nbobjs_by_type(node->topology, HWLOC_OBJ_NODE, 0, OPAL_HWLOC_AVAILABLE);
            OBJ_CONSTRUCT(&numa_list, opal_list_t);
            ret = opal_hwloc_get_sorted_numa_list(node->topology, orte_rmaps_base.device, &numa_list);
            if (ret > 1) {
                orte_show_help("help-orte-rmaps-md.txt", "orte-rmaps-mindist:several-devices",
                               true, orte_rmaps_base.device, ret, node->name);
                ORTE_SET_MAPPING_POLICY(jdata->map->mapping, ORTE_MAPPING_BYSLOT);
                rc = ORTE_ERR_TAKE_NEXT_OPTION;
                goto error;
            } else if (ret < 0) {
                orte_show_help("help-orte-rmaps-md.txt", "orte-rmaps-mindist:device-not-found",
                        true, orte_rmaps_base.device, node->name);
                ORTE_SET_MAPPING_POLICY(jdata->map->mapping, ORTE_MAPPING_BYSLOT);
                rc = ORTE_ERR_TAKE_NEXT_OPTION;
                goto error;
            }
            if (opal_list_get_size(&numa_list) > 0) {
                j = 0;
                required = 0;
                OPAL_LIST_FOREACH(numa, &numa_list, opal_rmaps_numa_node_t) {
                    /* get the hwloc object for this numa */
                    if (NULL == (obj = opal_hwloc_base_get_obj_by_type(node->topology, HWLOC_OBJ_NODE, 0, numa->index, OPAL_HWLOC_AVAILABLE))) {
                        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                        return ORTE_ERR_NOT_FOUND;
                    }
                    npus = opal_hwloc_base_get_npus(node->topology, obj);
                    if (bynode) {
                        required = ((num_procs_to_assign-j) > npus/orte_rmaps_base.cpus_per_rank) ? (npus/orte_rmaps_base.cpus_per_rank) : (num_procs_to_assign-j);
                    } else {
                        required = npus/orte_rmaps_base.cpus_per_rank;
                    }
                    for (k = 0; (k < required) && (nprocs_mapped < app->num_procs); k++) {
                        if (NULL == (proc = orte_rmaps_base_setup_proc(jdata, node, i))) {
                            rc = ORTE_ERR_OUT_OF_RESOURCE;
                            goto error;
                        }
                        nprocs_mapped++;
                        j++;
                        orte_set_attribute(&proc->attributes, ORTE_PROC_HWLOC_LOCALE, ORTE_ATTR_LOCAL, obj, OPAL_PTR);
                    }
                    if ((nprocs_mapped == (int)app->num_procs) || (bynode && ((int)num_procs_to_assign == j))) {
                        break;
                    }
                }
                if (0 != j) {
                    /* add the node to the map, if needed */
                    if (!ORTE_FLAG_TEST(node, ORTE_NODE_FLAG_MAPPED)) {
                        if (ORTE_SUCCESS > (rc = opal_pointer_array_add(jdata->map->nodes, (void*)node))) {
                            ORTE_ERROR_LOG(rc);
                            goto error;
                        }
                        ORTE_FLAG_SET(node, ORTE_NODE_FLAG_MAPPED);
                        OBJ_RETAIN(node);  /* maintain accounting on object */
                        jdata->map->num_nodes++;
                    }
                    opal_output_verbose(2, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:mindist: assigned %d procs to node %s",
                            j, node->name);
                }
            } else {
                if (hwloc_get_nbobjs_by_type(node->topology, HWLOC_OBJ_SOCKET) > 1) {
                    /* don't have info about pci locality */
                    orte_show_help("help-orte-rmaps-md.txt", "orte-rmaps-mindist:no-pci-locality-info",
                            true, node->name);
                }
                /* else silently switch to byslot mapper since distance info is irrelevant for this machine configuration */
                ORTE_SET_MAPPING_POLICY(jdata->map->mapping, ORTE_MAPPING_BYSLOT);
                rc = ORTE_ERR_TAKE_NEXT_OPTION;
                goto error;
            }
            while (NULL != (numa_item = opal_list_remove_first(&numa_list))) {
                OBJ_RELEASE(numa_item);
            }
            OBJ_DESTRUCT(&numa_list);
            if (bynode) {
                nextra--;
                if (nextra == 0) {
                    num_procs_to_assign--;
                }
            }
        }

        /* If we get to the end of all the nodes and still have procs remaining, then
         * we check the oversubscribed flag - if oversubscription is allowed, then
         * begin assigning procs round-robin *bynode* until all procs have been assigned.
         * This ensures that the overload is evenly distributed across all nodes.
         */

        extra_procs = app->num_procs - nprocs_mapped;
        if (extra_procs > 0) {
            /* check if oversubscribing is allowed */
            if (ORTE_MAPPING_NO_OVERSUBSCRIBE & ORTE_GET_MAPPING_DIRECTIVE(jdata->map->mapping)) {
                orte_show_help("help-orte-rmaps-base.txt", "orte-rmaps-base:alloc-error",
                        true, app->num_procs, app->app);
                rc = ORTE_ERR_SILENT;
                ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
                goto error;
            }
            opal_output_verbose(2, orte_rmaps_base_framework.framework_output,
                    "mca:rmaps:mindist job %s is oversubscribed - performing second pass",
                    ORTE_JOBID_PRINT(jdata->jobid));
            num_procs_to_assign = extra_procs/num_nodes;
            nextra = extra_procs % num_nodes;
            if (nextra > 0) {
                num_procs_to_assign++;
            }
            for (item = opal_list_get_first(&node_list);
                    item != opal_list_get_end(&node_list);
                    item = opal_list_get_next(item)) {
                node = (orte_node_t*)item;

                if (nprocs_mapped == app->num_procs)
                    break;
                ORTE_FLAG_SET(node, ORTE_NODE_FLAG_OVERSUBSCRIBED);
                opal_output_verbose(2, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:mindist: second pass assigning %d extra procs to node %s",
                        (int)num_procs_to_assign, node->name);
                OBJ_CONSTRUCT(&numa_list, opal_list_t);
                opal_hwloc_get_sorted_numa_list(node->topology, orte_rmaps_base.device, &numa_list);
                if (opal_list_get_size(&numa_list) > 0) {
                    numa_item = opal_list_get_first(&numa_list);
                    k = 0;
                    obj = hwloc_get_obj_by_type(node->topology, HWLOC_OBJ_NODE,((opal_rmaps_numa_node_t*)numa_item)->index);
                    npus = opal_hwloc_base_get_npus(node->topology, obj);
                    for (j = 0; j < (int)num_procs_to_assign && nprocs_mapped < (int)app->num_procs; j++) {
                        if (NULL == (proc = orte_rmaps_base_setup_proc(jdata, node, i))) {
                            rc = ORTE_ERR_OUT_OF_RESOURCE;
                            goto error;
                        }
                        nprocs_mapped++;
                        k++;
                        orte_set_attribute(&proc->attributes, ORTE_PROC_HWLOC_LOCALE, ORTE_ATTR_LOCAL, obj, OPAL_PTR);
                        if (k > npus/orte_rmaps_base.cpus_per_rank-1) {
                            numa_item = opal_list_get_next(numa_item);
                            if (numa_item == opal_list_get_end(&numa_list)) {
                                numa_item = opal_list_get_first(&numa_list);
                            }
                            obj = hwloc_get_obj_by_type(node->topology, HWLOC_OBJ_NODE,((opal_rmaps_numa_node_t*)numa_item)->index);
                            npus = opal_hwloc_base_get_npus(node->topology, obj);
                            k = 0;
                        }
                    }
                }
                while (NULL != (numa_item = opal_list_remove_first(&numa_list))) {
                    OBJ_RELEASE(numa_item);
                }
                OBJ_DESTRUCT(&numa_list);
                nextra--;
                if (nextra == 0) {
                    num_procs_to_assign--;
                }
            }
        }

        /* compute vpids and add proc objects to the job - do this after
         * each app_context so that the ranks within each context are
         * contiguous
         */
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_compute_vpids(jdata, app, &node_list))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* track the total number of processes we mapped - must update
         * this value AFTER we compute vpids so that computation
         * is done correctly
         */
        jdata->num_procs += app->num_procs;

        /* cleanup the node list - it can differ from one app_context
         * to another, so we have to get it every time
         */
        while (NULL != (item = opal_list_remove_first(&node_list))) {
            OBJ_RELEASE(item);
        }
        OBJ_DESTRUCT(&node_list);
    }
    free(orte_rmaps_base.device);
    return ORTE_SUCCESS;

error:
    while(NULL != (item = opal_list_remove_first(&node_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node_list);

    return rc;
}
