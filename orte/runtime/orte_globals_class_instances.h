/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
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
 *
 */
/** @file
 */

#ifndef ORTE_RUNTIME_GLOBAL_CLASS_INSTANCES_H_
#define ORTE_RUNTIME_GLOBAL_CLASS_INSTANCES_H_

#include "orte_config.h"
#include "orte/types.h"

#include "opal/util/argv.h"

#include "orte/mca/plm/plm_types.h"

#include "orte/runtime/orte_globals.h"

BEGIN_C_DECLS

/*
 * CONSTRUCTORS, DESTRUCTORS, AND CLASS INSTANTIATIONS
 * FOR ORTE CLASSES
 */

static void orte_app_context_construct(orte_app_context_t* app_context)
{
    app_context->idx=0;
    app_context->app=NULL;
    app_context->num_procs=0;
    app_context->argv=NULL;
    app_context->env=NULL;
    app_context->cwd=NULL;
    app_context->hostfile=NULL;
    app_context->add_hostfile=NULL;
    app_context->num_map = 0;
    app_context->map_data = NULL;
    app_context->prefix_dir = NULL;
    app_context->preload_binary = false;
    app_context->preload_files  = NULL;
    app_context->preload_files_dest_dir  = NULL;
}

static void orte_app_context_destructor(orte_app_context_t* app_context)
{
    orte_std_cntr_t i;
    
    if (NULL != app_context->app) {
        free (app_context->app);
    }
    
    /* argv and env lists created by util/argv copy functions */
    if (NULL != app_context->argv) {
        opal_argv_free(app_context->argv);
    }
    
    if (NULL != app_context->env) {
        opal_argv_free(app_context->env);
    }
    
    if (NULL != app_context->cwd) {
        free (app_context->cwd);
    }
    
    if (NULL != app_context->hostfile) {
        free(app_context->hostfile);
    }
    
    if (NULL != app_context->add_hostfile) {
        free(app_context->add_hostfile);
    }
    
    if (NULL != app_context->map_data) {
        for (i = 0; i < app_context->num_map; ++i) {
            if (NULL != app_context->map_data[i]) {
                OBJ_RELEASE(app_context->map_data[i]);
            }
        }
        if (NULL != app_context->map_data) {
            free(app_context->map_data);
        }
    }
    
    if (NULL != app_context->prefix_dir) {
        free(app_context->prefix_dir);
    }
    
    app_context->preload_binary = false;
    
    if(NULL != app_context->preload_files) {
        free(app_context->preload_files);
    }
    
    if(NULL != app_context->preload_files_dest_dir) {
        free(app_context->preload_files_dest_dir);
    }
}

OBJ_CLASS_INSTANCE(orte_app_context_t,
                   opal_object_t,
                   orte_app_context_construct,
                   orte_app_context_destructor);


static void orte_app_context_map_construct(orte_app_context_map_t *a)
{
    a->map_type = ORTE_APP_CONTEXT_MAP_INVALID;
    a->map_data = NULL;
}

static void orte_app_context_map_destruct(orte_app_context_map_t *a)
{
    if (NULL != a->map_data) {
        free(a->map_data);
    }
}

OBJ_CLASS_INSTANCE(orte_app_context_map_t,
                   opal_object_t,
                   orte_app_context_map_construct,
                   orte_app_context_map_destruct);


static void orte_job_construct(orte_job_t* job)
{
    job->jobid = ORTE_JOBID_INVALID;
    job->apps = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(job->apps,
                            1,
                            ORTE_GLOBAL_ARRAY_MAX_SIZE,
                            2);
    job->num_apps = 0;
    job->local_spawn = false;
    job->total_slots_alloc = 0;
    job->num_procs = 0;
    job->procs = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(job->procs,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE,
                            ORTE_GLOBAL_ARRAY_MAX_SIZE,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE);
    
    job->map = NULL;
    job->bookmark = NULL;
    job->oversubscribe_override = false;
    job->state = ORTE_JOB_STATE_UNDEF;

    job->num_launched = 0;
    job->num_reported = 0;
    job->num_terminated = 0;
    job->abort = false;
    job->aborted_proc = NULL;

#if OPAL_ENABLE_FT == 1
    job->ckpt_state = 0;
    job->ckpt_snapshot_ref = NULL;
    job->ckpt_snapshot_loc = NULL;
#endif
}

static void orte_job_destruct(orte_job_t* job)
{
    orte_std_cntr_t i;
    orte_vpid_t j;
    
    for (i=0; i < job->num_apps; i++) {
        if (NULL != job->apps->addr[i]) OBJ_RELEASE(job->apps->addr[i]);
    }
    OBJ_RELEASE(job->apps);
    
    for (j=0; j < job->num_procs; j++) {
        if (NULL != job->procs->addr[j]) OBJ_RELEASE(job->procs->addr[j]);
    }
    OBJ_RELEASE(job->procs);
    
    if (NULL != job->map) OBJ_RELEASE(job->map);

#if OPAL_ENABLE_FT == 1
    if (NULL != job->ckpt_snapshot_ref) {
        free(job->ckpt_snapshot_ref);
    }
    if (NULL != job->ckpt_snapshot_loc) {
        free(job->ckpt_snapshot_loc);
    }
#endif
}

OBJ_CLASS_INSTANCE(orte_job_t,
                   opal_list_item_t,
                   orte_job_construct,
                   orte_job_destruct);


static void orte_node_construct(orte_node_t* node)
{
    node->name = NULL;
    node->nodeid = ORTE_NODEID_INVALID;
    node->allocate = false;
    node->index = -1;
    node->daemon = NULL;
    node->daemon_launched = false;
    node->launch_id = -1;

    node->num_procs = 0;
    node->procs = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(node->procs,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE,
                            ORTE_GLOBAL_ARRAY_MAX_SIZE,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE);
    
    node->oversubscribed = false;
    node->arch = 0;
    node->state = ORTE_NODE_STATE_UNKNOWN;
    node->slots = 0;
    node->slots_inuse = 0;
    node->slots_alloc = 0;
    node->slots_max = 0;
    node->username = NULL;
}

static void orte_node_destruct(orte_node_t* node)
{
    orte_vpid_t i;
    
    if (NULL != node->name) {
        free(node->name);
    }

    if (NULL != node->daemon) OBJ_RELEASE(node->daemon);
    
    for (i=0; i < node->num_procs; i++) {
        if (NULL != node->procs->addr[i]) OBJ_RELEASE(node->procs->addr[i]);
    }
    OBJ_RELEASE(node->procs);
    
    if (NULL != node->username) {
        free(node->username);
    }
}

OBJ_CLASS_INSTANCE(orte_node_t,
                   opal_list_item_t,
                   orte_node_construct,
                   orte_node_destruct);



static void orte_proc_construct(orte_proc_t* proc)
{
    proc->name = *ORTE_NAME_INVALID;
    proc->pid = 0;
    proc->local_rank = ORTE_VPID_INVALID;
    proc->state = ORTE_PROC_STATE_UNDEF;
    proc->app_idx = -1;
    proc->slot_list = NULL;
    proc->node = NULL;
    proc->nodename = NULL;
    proc->rml_uri = NULL;
#if OPAL_ENABLE_FT == 1
    proc->ckpt_state = 0;
    proc->ckpt_snapshot_ref = NULL;
    proc->ckpt_snapshot_loc = NULL;
#endif
}

static void orte_proc_destruct(orte_proc_t* proc)
{
    if (NULL != proc->slot_list) {
        free(proc->slot_list);
    }

    if (NULL != proc->node) OBJ_RELEASE(proc->node);
    
    if (NULL != proc->nodename) free(proc->nodename);

    if (NULL != proc->rml_uri) free(proc->rml_uri);
    
#if OPAL_ENABLE_FT == 1
    if (NULL != proc->ckpt_snapshot_ref) {
        free(proc->ckpt_snapshot_ref);
    }
    if (NULL != proc->ckpt_snapshot_loc) {
        free(proc->ckpt_snapshot_loc);
    }
#endif
}

OBJ_CLASS_INSTANCE(orte_proc_t,
                   opal_list_item_t,
                   orte_proc_construct,
                   orte_proc_destruct);

static void orte_job_map_construct(orte_job_map_t* map)
{
    map->policy = ORTE_RMAPS_BYSLOT;    /* default to byslot mapping as per orterun options */
    map->no_use_local = false;
    map->pernode = false;
    map->npernode = 0;
    map->oversubscribe = true;  /* default to allowing oversubscribe */
    map->display_map = false;
    map->num_new_daemons = 0;
    map->daemon_vpid_start = ORTE_VPID_INVALID;
    map->num_nodes = 0;
    map->nodes = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(map->nodes,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE,
                            ORTE_GLOBAL_ARRAY_MAX_SIZE,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE);
}

static void orte_job_map_destruct(orte_job_map_t* map)
{
    orte_std_cntr_t i;
    
    for (i=0; i < map->nodes->size; i++) {
        if (NULL != map->nodes->addr[i]) {
            OBJ_RELEASE(map->nodes->addr[i]);
        }
    }
    OBJ_RELEASE(map->nodes);
}

OBJ_CLASS_INSTANCE(orte_job_map_t,
                   opal_object_t,
                   orte_job_map_construct,
                   orte_job_map_destruct);


END_C_DECLS

#endif /* ORTE_RUNTIME_GLOBAL_CLASS_INSTANCES_H_ */

