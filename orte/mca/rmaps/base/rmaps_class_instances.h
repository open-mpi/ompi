/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef ORTE_MCA_RMAPS_CLASS_INST_H
#define ORTE_MCA_RMAPS_CLASS_INST_H

/*
 * includes
 */
#include "orte_config.h"
#include "orte/orte_constants.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/ras/ras_types.h"

#include "orte/mca/rmaps/rmaps.h"

/*
 * Functions for use solely within the RMAPS framework
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
/*
 * orte_mapped_proc_t
 */
static void orte_rmaps_mapped_proc_construct(orte_mapped_proc_t* proc)
{
    proc->name.cellid = ORTE_CELLID_INVALID;
    proc->name.jobid = ORTE_JOBID_INVALID;
    proc->name.vpid = ORTE_VPID_INVALID;
    proc->rank = 0;
    proc->app_idx = 0;
    proc->pid = 0;
}

OBJ_CLASS_INSTANCE(orte_mapped_proc_t,
                   opal_list_item_t,
                   orte_rmaps_mapped_proc_construct, NULL);

/*
 * orte_mapped_node_t
 */
static void orte_rmaps_mapped_node_construct(orte_mapped_node_t* node)
{
    node->nodename = NULL;
    node->launch_id = -1;
    node->username = NULL;
    node->daemon = NULL;
    node->oversubscribed = false;
    node->num_procs = 0;
    OBJ_CONSTRUCT(&node->procs, opal_list_t);
}

static void orte_rmaps_mapped_node_destruct(orte_mapped_node_t* node)
{
    opal_list_item_t* item;
    
    if (NULL != node->nodename) {
        free(node->nodename);
    }
    
    if (NULL != node->username) {
        free(node->username);
    }
    
    if (NULL != node->daemon) {
        free(node->daemon);
    }
    
    while (NULL != (item = opal_list_remove_first(&node->procs))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node->procs);
}

OBJ_CLASS_INSTANCE(orte_mapped_node_t,
                   opal_list_item_t,
                   orte_rmaps_mapped_node_construct,
                   orte_rmaps_mapped_node_destruct);

/*
 * orte_job_map_t
 */

static void orte_rmaps_job_map_construct(orte_job_map_t* map)
{
    map->job = ORTE_JOBID_INVALID;
    map->mapping_mode = NULL;
    map->vpid_start = ORTE_VPID_INVALID;
    map->vpid_range = 0;
    map->num_apps = 0;
    map->apps = NULL;
    map->num_nodes = 0;
    OBJ_CONSTRUCT(&map->nodes, opal_list_t);
}

static void orte_rmaps_job_map_destruct(orte_job_map_t* map)
{
    orte_std_cntr_t i=0;
    opal_list_item_t* item;
    
    if (NULL != map->mapping_mode) free(map->mapping_mode);
    
    if (NULL != map->apps) {
        for(i=0; i < map->num_apps; i++) {
            if (NULL != map->apps[i]) {
                OBJ_RELEASE(map->apps[i]);
            }
        }
        free(map->apps);
    }
    
    while (NULL != (item = opal_list_remove_first(&map->nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&map->nodes);
}

OBJ_CLASS_INSTANCE(orte_job_map_t,
                   opal_object_t,
                   orte_rmaps_job_map_construct,
                   orte_rmaps_job_map_destruct);
    
    
/*
 * external API functions will be documented in the mca/rmaps/rmaps.h file
 */

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
