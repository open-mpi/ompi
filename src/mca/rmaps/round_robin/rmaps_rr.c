/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "orte_config.h"
#include <errno.h>
#include <unistd.h>
#include <string.h>

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "util/output.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"
#include "mca/rmaps/base/base.h"
#include "mca/rmgr/base/base.h"
#include "mca/rmaps/base/rmaps_base_map.h"
#include "mca/ras/base/ras_base_node.h"
#include "rmaps_rr.h"


/*
 * Create a default mapping for the application.
 */

static int orte_rmaps_rr_map_app(
    orte_app_context_t* app, 
    orte_rmaps_base_map_t* map,
    orte_jobid_t jobid,
    orte_vpid_t vpid_start,
    int rank,
    ompi_list_t* nodes)
{
    /* build a nodelist and assign process slots in order */
    size_t num_alloc = 0;
    size_t proc_index = 0;
    ompi_list_item_t* item;

    item =  ompi_list_get_first(nodes);
    while(item != ompi_list_get_end(nodes)) {
        ompi_list_item_t* next = ompi_list_get_next(item);
        orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
        orte_rmaps_base_node_t* rmaps_node = OBJ_NEW(orte_rmaps_base_node_t);
        size_t i, num_procs;
        if(NULL == rmaps_node) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        rmaps_node->node_name = strdup(node->node_name);

        if(num_alloc + node->node_slots_alloc >= (size_t)app->num_procs) {
            num_procs = app->num_procs - num_alloc;
        } else {
            num_procs = node->node_slots_alloc;
        }

        /* assign the next num_procs to this node */
        for(i=0; i<num_procs; i++) {
            orte_rmaps_base_proc_t* proc = OBJ_NEW(orte_rmaps_base_proc_t);
            orte_process_name_t* proc_name;
            int rc;

            /* create the process name as an offset from the vpid-start */
            rc = orte_ns.create_process_name(&proc_name, node->node_cellid, jobid, vpid_start+rank);
            if(rc != ORTE_SUCCESS) {
                OBJ_RELEASE(proc);
                return rc;
            }
            proc->proc_node = rmaps_node;
            proc->proc_name = *proc_name;
            proc->proc_rank = rank;
            rank++;
            orte_ns.free_name(&proc_name);
            OBJ_RETAIN(proc); /* bump reference count for the node */
            ompi_list_append(&rmaps_node->node_procs, &proc->super);
            map->procs[proc_index++] = proc;
        }

        node->node_slots_alloc -= num_procs;
        if(node->node_slots_alloc == 0) {
            ompi_list_remove_item(nodes,item);
            OBJ_RELEASE(item);
        }
        num_alloc += num_procs;
        if(num_alloc == (size_t)app->num_procs)
            break;
        item = next;
        ompi_list_append(&map->nodes, &rmaps_node->super);
    } 
    map->num_procs = num_alloc;
    return ORTE_SUCCESS;
}
   

/*
 * Create a default mapping for the job.
 */

static int orte_rmaps_rr_map(orte_jobid_t jobid)
{
    orte_app_context_t** context;
    size_t i, num_context;
    ompi_list_t nodes;
    ompi_list_t mapping;
    ompi_list_item_t* item;
    orte_vpid_t vpid_start;
    size_t num_procs = 0;
    int rank = 0;
    int rc = ORTE_SUCCESS;

    /* query for the application context and allocated nodes */
    if(ORTE_SUCCESS != (rc = orte_rmgr_base_get_app_context(jobid, &context, &num_context))) {
        return rc;
    }

    /* total number of procs required */
    for(i=0; i<num_context; i++) {
        orte_app_context_t* app = context[i];
        num_procs += app->num_procs;
    }

    /* allocate a vpid range for the job */
    if(ORTE_SUCCESS != (rc = orte_ns.reserve_range(jobid, num_procs, &vpid_start))) {
        return rc;
    }

    /* query for all nodes allocated to this job */
    OBJ_CONSTRUCT(&nodes, ompi_list_t);
    if(ORTE_SUCCESS != (rc = orte_ras_base_node_query_alloc(&nodes,jobid))) {
        OBJ_DESTRUCT(&nodes);
        return rc;
    }

    /* construct a default mapping */
    OBJ_CONSTRUCT(&mapping, ompi_list_t);
    for(i=0; i<num_context; i++) {
        orte_app_context_t* app = context[i];
        orte_rmaps_base_map_t* map = OBJ_NEW(orte_rmaps_base_map_t);
        if(NULL == map) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
        ompi_list_append(&mapping, &map->super);

        map->app = app;
        map->procs = malloc(sizeof(orte_rmaps_base_proc_t*)*app->num_procs);
        if(NULL == map->procs) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
        if(ORTE_SUCCESS != (rc = orte_rmaps_rr_map_app(app,map,jobid,vpid_start,rank,&nodes))) {
            goto cleanup;
        }
        rank += app->num_procs;
    }

    /* save mapping to the registry */
    if(ORTE_SUCCESS != (rc = orte_rmaps_base_set_map(jobid, &mapping))) {
        goto cleanup;
    }
    
    /* save vpid start/range on the job segment */
    rc = orte_rmaps_base_set_vpid_range(jobid,vpid_start,num_procs);

cleanup:
    while(NULL != (item = ompi_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);
    while(NULL != (item = ompi_list_remove_first(&mapping))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&mapping);
    return rc;
}


static int orte_rmaps_rr_finalize(void)
{
    return ORTE_SUCCESS;
}


orte_rmaps_base_module_t orte_rmaps_round_robin_module = {
    orte_rmaps_rr_map,
    orte_rmaps_rr_finalize
};

