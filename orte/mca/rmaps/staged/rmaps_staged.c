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

#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/dss/dss.h"
#include "opal/util/output.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"

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
    int i, j, rc;
    orte_app_context_t *app;
    opal_list_t node_list;
    orte_std_cntr_t num_slots;
    orte_proc_t *proc;
    orte_node_t *node;
    bool work_to_do = false;
    opal_list_item_t *item;

    /* only use this mapper if it was specified */
    if (NULL == jdata->map->req_mapper ||
        0 != strcasecmp(jdata->map->req_mapper, c->mca_component_name) ||
        !(ORTE_MAPPING_STAGED & ORTE_GET_MAPPING_POLICY(jdata->map->mapping))) {
        /* I wasn't specified */
        opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                            "mca:rmaps:staged: job %s not using staged mapper",
                            ORTE_JOBID_PRINT(jdata->jobid));
        return ORTE_ERR_TAKE_NEXT_OPTION;
    }

    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                        "%s mca:rmaps:staged: mapping job %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_JOBID_PRINT(jdata->jobid));
 
    /* flag that I did the mapping */
    if (NULL != jdata->map->last_mapper) {
        free(jdata->map->last_mapper);
    }
    jdata->map->last_mapper = strdup(c->mca_component_name);

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
        opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                            "%s mca:rmaps:staged: working app %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), app->app);

        /* find nodes that meet any constraints provided in the form of
         * -hostfile or -host directives
         */
        OBJ_CONSTRUCT(&node_list, opal_list_t);
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_get_target_nodes(&node_list, &num_slots, app,
                                                                   jdata->map->mapping, false, true)) &&
            ORTE_ERR_SILENT != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* if nothing is available, then move on */
        if (0 == num_slots || 0 == opal_list_get_size(&node_list)) {
	    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
				"%s mca:rmaps:staged: no nodes available for this app",
				ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            OBJ_DESTRUCT(&node_list);
            continue;
        }
        /* assign any unmapped procs to an available slot */
        for (j=0; j < app->procs.size; j++) {
            if (NULL == (proc = opal_pointer_array_get_item(&app->procs, j))) {
                continue;
            }
            if (ORTE_PROC_STATE_UNDEF != proc->state) {
                /* this proc has already been mapped or executed */
	        opal_output_verbose(5, orte_rmaps_base.rmaps_output,
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
            /* map this proc to the first available slot */
            node = (orte_node_t*)opal_list_get_first(&node_list);
            OBJ_RETAIN(node);  /* maintain accounting on object */    
	    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
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
            if (node->slots_inuse == node->slots_alloc) {
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

    return ORTE_SUCCESS;
}
