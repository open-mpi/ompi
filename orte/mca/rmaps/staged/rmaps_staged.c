/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved
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

#include "opal/mca/hwloc/base/base.h"
#include "opal/dss/dss.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/state/state.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"
#include "rmaps_staged.h"

static int staged_mapper(orte_job_t *jdata);

orte_rmaps_base_module_t orte_rmaps_staged_module = {
    staged_mapper
};

static int staged_mapper(orte_job_t *jdata)
{
    mca_base_component_t *c=&mca_rmaps_staged_component.base_version;
    int i, j, k, rc;
    orte_app_context_t *app;
    opal_list_t node_list, desired;
    orte_std_cntr_t num_slots;
    orte_proc_t *proc;
    orte_node_t *node, *next;
    bool work_to_do = false, first_pass = false;
    opal_list_item_t *item, *it2;
    char *cptr, **minimap;
    orte_vpid_t load;

    /* only use this mapper if it was specified */
    if (NULL == jdata->map->req_mapper ||
        0 != strcasecmp(jdata->map->req_mapper, c->mca_component_name) ||
        ORTE_MAPPING_STAGED != ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
        /* I wasn't specified */
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:staged: job %s not using staged mapper",
                            ORTE_JOBID_PRINT(jdata->jobid));
        return ORTE_ERR_TAKE_NEXT_OPTION;
    }

    opal_output_verbose(2, orte_rmaps_base_framework.framework_output,
                        "%s mca:rmaps:staged: mapping job %s with %d procs",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_JOBID_PRINT(jdata->jobid), (int)jdata->num_procs);
 
    /* flag that I did the mapping */
    if (NULL != jdata->map->last_mapper) {
        free(jdata->map->last_mapper);
    }
    jdata->map->last_mapper = strdup(c->mca_component_name);

    /* if there are no nodes in the map, then this is our first
     * pass thru this job
     */
    if (0 == jdata->map->num_nodes) {
        first_pass = true;
    }

    /* we assume that the app_contexts are in priority order,
     * with the highest priority being the first entry in the
     * job's app_context array. Loop across the app_contexts
     * in order, looking for apps that have not been
     * fully mapped
     */
    for (i=0; i < jdata->apps->size; i++) {
        if (NULL == (app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        /* has it been fully mapped? */
        if (ORTE_APP_STATE_ALL_MAPPED <= app->state) {
            continue;
        }
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "%s mca:rmaps:staged: working app %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), app->app);

        /* find nodes that meet any constraints provided in the form of
         * -hostfile or -host directives
         */
        OBJ_CONSTRUCT(&node_list, opal_list_t);
        /* get nodes based on a strict interpretation of the location hints */
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                   jdata->map->mapping, false, true))) {
            /* we were unable to get any nodes that match those
             * specified in the app
             */
            if (ORTE_ERR_RESOURCE_BUSY == rc) {
                /* if the return is "busy", then at least one of the
                 * specified resources must exist, but no slots are
                 * currently available. This means there is at least
                 * a hope of eventually being able to map this app
                 * within its specified constraints, so continue working
                 */
                if (orte_soft_locations) {
                    /* if soft locations were given, then we know that
                     * none of the nodes in this allocation are available,
                     * so there is no point in continuing to check the
                     * remaining apps
                     */
                    while (NULL != (item = opal_list_remove_first(&node_list))) {
                        OBJ_RELEASE(item);
                    }
                    OBJ_DESTRUCT(&node_list);
                    goto complete;
                }
                opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                    "%s mca:rmaps:staged: all nodes for this app are currently busy",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                OBJ_DESTRUCT(&node_list);
                continue;
            } else {
                /* this indicates that there are no nodes that match
                 * the specified constraints, so there is no hope of
                 * ever being able to execute this app. This is an
                 * unrecoverable error - note that a return of
                 * "silent" means that the function already printed
                 * an error message, so the error_log will print nothing
                 */
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* if a max number of procs/node was given for this
         * app, remove all nodes from the list that exceed
         * that limit
         */
        if (0 < app->max_procs_per_node) {
            item = opal_list_get_first(&node_list);
            while (item != opal_list_get_end(&node_list)) {
                it2 = opal_list_get_next(item);
                node = (orte_node_t*)item;
                if (app->max_procs_per_node <= node->num_procs) {
                    opal_list_remove_item(&node_list, item);
                    OBJ_RELEASE(item);
                }
                item = it2;
            }
        }

        /* if we have no available nodes, then move on to next app */
        if (0 == opal_list_get_size(&node_list)) {
            OBJ_DESTRUCT(&node_list);
            continue;
        }

        /* if the app specified locations, soft or not, search the list of nodes
         * for those that match the requested locations and move those
         * to the desired list so we use them first
         */
        if (NULL != app->dash_host) {
            OBJ_CONSTRUCT(&desired, opal_list_t);
            /* no particular order is required */
            for (j=0; j < opal_argv_count(app->dash_host); j++) {
                minimap = opal_argv_split(app->dash_host[j], ',');
                for (k=0; k < opal_argv_count(minimap); k++) {
                    cptr = minimap[k];
                    for (item = opal_list_get_first(&node_list);
                         item != opal_list_get_end(&node_list);
                         item = opal_list_get_next(item)) {
                        node = (orte_node_t*)item;
                        if (0 == strcmp(node->name, cptr) ||
                            (0 == strcmp("localhost", cptr) &&
                             0 == strcmp(node->name, orte_process_info.nodename))) {
                            opal_list_remove_item(&node_list, item);
                            opal_list_append(&desired, item);
                            opal_output_verbose(10, orte_rmaps_base_framework.framework_output,
                                                "%s mca:rmaps:staged: placing node %s on desired list",
                                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                                node->name);
                            break;
                        }
                    }
                }
                opal_argv_free(minimap);
            }
            /* if no nodes made the transition and the app specified soft
             * locations, then we can skip to look at the non-desired list
             */
            if (0 == opal_list_get_size(&desired)) {
                OBJ_DESTRUCT(&desired);
                if (orte_soft_locations) {
                    goto process;
                } else {
                    /* move on to next app */
                    continue;
                }
            }
            /* cycle thru the procs for this app and attempt to map them
             * to the desired nodes using a load-balancing algo
             */
            for (j=0; j < app->procs.size; j++) {
                if (NULL == (proc = opal_pointer_array_get_item(&app->procs, j))) {
                    continue;
                }
                if (ORTE_PROC_STATE_UNDEF != proc->state) {
                    /* this proc has already been mapped or executed */
                    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                        "%s mca:rmaps:staged: proc %s has already been mapped",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                        ORTE_NAME_PRINT(&proc->name));
                    continue;
                }
                /* flag that there is at least one proc still to
                 * be executed
                 */
                work_to_do = true;
                /* track number mapped */
                jdata->num_mapped++;
                /* find the lightest-loaded node on the desired list */
                node = NULL;
                load = ORTE_VPID_MAX;
                for (item = opal_list_get_first(&desired);
                     item != opal_list_get_end(&desired);
                     item = opal_list_get_next(item)) {
                    next = (orte_node_t*)item;
                    if (next->num_procs < load) {
                        node = next;
                        load = next->num_procs;
                    }
                }
                /* put the proc there */
                proc->node = node;
                proc->nodename = node->name;
                /* the local rank is the number of procs
                 * on this node from this job - we don't
                 * directly track this number, so it must
                 * be found by looping across the node->procs
                 * array and counting it each time. For now,
                 * since we don't use this value in this mode
                 * of operation, just set it to something arbitrary
                 */
                proc->local_rank = node->num_procs;
                /* the node rank is simply the number of procs
                 * on the node at this time
                 */
                proc->node_rank = node->num_procs;
                /* track number of procs on node and number of slots used */
                node->num_procs++;
                node->slots_inuse++;
                opal_output_verbose(10, orte_rmaps_base_framework.framework_output,
                                    "%s Proc %s on node %s: slots %d inuse %d",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    ORTE_NAME_PRINT(&proc->name), node->name,
                                    (int)node->slots, (int)node->slots_inuse);
                if (node->slots_inuse == node->slots) {
                    opal_list_remove_item(&desired, &node->super);
                    OBJ_RELEASE(node);
                }
                if (0 > (rc = opal_pointer_array_add(node->procs, (void*)proc))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(proc);
                    return rc;
                }
                /* retain the proc struct so that we correctly track its release */
                OBJ_RETAIN(proc);
                proc->state = ORTE_PROC_STATE_INIT;
                /* flag the proc as updated so it will be included
                 * in the next pidmap message
                 */
                proc->updated =true;
                /* add the node to the map, if needed */
                if (!node->mapped) {
                    if (ORTE_SUCCESS > (rc = opal_pointer_array_add(jdata->map->nodes, (void*)node))) {
                        ORTE_ERROR_LOG(rc);
                        return rc;
                    }
                    node->mapped = true;
                    OBJ_RETAIN(node);  /* maintain accounting on object */
                    jdata->map->num_nodes++;
                }
                if (0 == opal_list_get_size(&desired)) {
                    /* nothing more we can do */
                    break;
                }
            }
            /* clear the list */
            while (NULL != (item = opal_list_remove_first(&desired))) {
                OBJ_RELEASE(item);
            }
            OBJ_DESTRUCT(&desired);
        }

    process:
        for (j=0; j < app->procs.size; j++) {
            if (NULL == (proc = opal_pointer_array_get_item(&app->procs, j))) {
                continue;
            }
            if (ORTE_PROC_STATE_UNDEF != proc->state) {
                /* this proc has already been mapped or executed */
	        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
				    "%s mca:rmaps:staged: proc %s has already been mapped",
				    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
				    ORTE_NAME_PRINT(&proc->name));
                continue;
            }
            /* find the lightest-loaded node on the node list */
            node = NULL;
            load = ORTE_VPID_MAX;
            for (item = opal_list_get_first(&node_list);
                 item != opal_list_get_end(&node_list);
                 item = opal_list_get_next(item)) {
                next = (orte_node_t*)item;
                if (next->num_procs < load) {
                    node = next;
                    load = next->num_procs;
                }
            }
            /* flag that there is at least one proc still to
             * be executed
             */
            work_to_do = true;
            /* track number mapped */
            jdata->num_mapped++;
            /* map this proc to the first available slot */
            OBJ_RETAIN(node);  /* maintain accounting on object */    
	    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
				"%s mca:rmaps:staged: assigning proc %s to node %s",
				ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
				ORTE_NAME_PRINT(&proc->name), node->name);
            proc->node = node;
            proc->nodename = node->name;
	    /* the local rank is the number of procs
	     * on this node from this job - we don't
	     * directly track this number, so it must
	     * be found by looping across the node->procs
	     * array and counting it each time. For now,
	     * since we don't use this value in this mode
	     * of operation, just set it to something arbitrary
	     */
	    proc->local_rank = node->num_procs;
	    /* the node rank is simply the number of procs
	     * on the node at this time
	     */
	    proc->node_rank = node->num_procs;
	    /* track number of procs on node and number of slots used */
            node->num_procs++;
            node->slots_inuse++;
            opal_output_verbose(10, orte_rmaps_base_framework.framework_output,
                                "%s Proc %s on node %s: slots %d inuse %d",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&proc->name), node->name,
                                (int)node->slots, (int)node->slots_inuse);
            if (node->slots_inuse == node->slots) {
                opal_list_remove_item(&node_list, &node->super);
                OBJ_RELEASE(node);
            }
            if (0 > (rc = opal_pointer_array_add(node->procs, (void*)proc))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(proc);
                return rc;
            }
            /* retain the proc struct so that we correctly track its release */
            OBJ_RETAIN(proc);
            proc->state = ORTE_PROC_STATE_INIT;
            /* flag the proc as updated so it will be included
             * in the next pidmap message
             */
            proc->updated =true;
            /* add the node to the map, if needed */
            if (!node->mapped) {
                if (ORTE_SUCCESS > (rc = opal_pointer_array_add(jdata->map->nodes, (void*)node))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                node->mapped = true;
                OBJ_RETAIN(node);  /* maintain accounting on object */
                jdata->map->num_nodes++;
            }
            if (0 == opal_list_get_size(&node_list)) {
                /* nothing more we can do */
                break;
            }
        }
	/* clear the list */
	while (NULL != (item = opal_list_remove_first(&node_list))) {
            OBJ_RELEASE(item);
	}
	OBJ_DESTRUCT(&node_list);
    }

 complete:
    /* if there isn't at least one proc that can be launched,
     * then indicate that we don't need to proceed with the
     * launch sequence
     */
    if (!work_to_do) {
        return ORTE_ERR_RESOURCE_BUSY;
    }
 
    /* flag that the job was updated so it will be
     * included in the pidmap message
     */
    jdata->updated = true;

    /* if we successfully mapped ALL procs in the first pass,
     * then this job is capable of supporting MPI procs
     */
    if (first_pass && jdata->num_mapped == jdata->num_procs) {
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "%s mca:rmaps:staged: job %s is MPI-capable",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_JOBID_PRINT(jdata->jobid));
        jdata->gang_launched = true;
    }

    return ORTE_SUCCESS;
}
