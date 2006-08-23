/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/schema/schema.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rmaps/base/rmaps_base_map.h"
#include "orte/mca/smr/smr_types.h"


/**
 * orte_rmaps_base_node_t
 */

static void orte_rmaps_base_node_construct(orte_rmaps_base_node_t* node)
{
    node->node = NULL;
    OBJ_CONSTRUCT(&node->node_procs, opal_list_t);
}

static void orte_rmaps_base_node_destruct(orte_rmaps_base_node_t* node)
{
    opal_list_item_t* item;
    if(NULL != node->node) {
        OBJ_RELEASE(node->node);
        node->node = NULL;
    }
    while(NULL != (item = opal_list_remove_first(&node->node_procs))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&node->node_procs);
}

OBJ_CLASS_INSTANCE(
    orte_rmaps_base_node_t,
    opal_list_item_t,
    orte_rmaps_base_node_construct,
    orte_rmaps_base_node_destruct);

/**
 * orte_rmaps_base_proc_t
 */

static void orte_rmaps_base_proc_construct(orte_rmaps_base_proc_t* proc)
{
    proc->app = NULL;
    proc->proc_node = NULL;
    proc->pid = 0;
    proc->local_pid = 0;
}

static void orte_rmaps_base_proc_destruct(orte_rmaps_base_proc_t* proc)
{
    if (NULL != proc->app) {
        free(proc->app);
        proc->app = NULL;
    }
}

OBJ_CLASS_INSTANCE(
    orte_rmaps_base_proc_t,
    opal_list_item_t,
    orte_rmaps_base_proc_construct,
    orte_rmaps_base_proc_destruct);


/**
 * orte_rmaps_base_map_t
 */

static void orte_rmaps_base_map_construct(orte_rmaps_base_map_t* map)
{
    map->app = NULL;
    map->procs = NULL;
    map->num_procs = 0;
    OBJ_CONSTRUCT(&map->nodes, opal_list_t);
}

static void orte_rmaps_base_map_destruct(orte_rmaps_base_map_t* map)
{
    orte_std_cntr_t i=0;
    opal_list_item_t* item;

    for(i=0; i<map->num_procs; i++) {
        OBJ_RELEASE(map->procs[i]);
    }
    while(NULL != (item = opal_list_remove_first(&map->nodes)))
        OBJ_RELEASE(item);
    if(NULL != map->procs) {
        free(map->procs);
        map->procs = NULL;
    }
    if(NULL != map->app) {
        OBJ_RELEASE(map->app);
        map->app = NULL;
    }
    OBJ_DESTRUCT(&map->nodes);
}

OBJ_CLASS_INSTANCE(
    orte_rmaps_base_map_t,
    opal_list_item_t,
    orte_rmaps_base_map_construct,
    orte_rmaps_base_map_destruct);


/*
 * Compare two proc entries
 */

static int orte_rmaps_value_compare(orte_gpr_value_t** val1, orte_gpr_value_t** val2)
{
    orte_std_cntr_t i;
    orte_std_cntr_t app1 = 0;
    orte_std_cntr_t app2 = 0;
    orte_std_cntr_t rank1 = 0;
    orte_std_cntr_t rank2 = 0;
    orte_std_cntr_t *sptr;
    orte_gpr_value_t* value;
    int rc;

    for(i=0, value=*val1; i<value->cnt; i++) {
        orte_gpr_keyval_t* keyval = value->keyvals[i];
        if(strcmp(keyval->key, ORTE_PROC_RANK_KEY) == 0) {
            if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(rc);
            }
            rank1 = *sptr;
            continue;
        }
        if(strcmp(keyval->key, ORTE_PROC_APP_CONTEXT_KEY) == 0) {
            if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(rc);
            }
            app1 = *sptr;
            continue;
        }
    }
    for(i=0, value=*val2; i<value->cnt; i++) {
        orte_gpr_keyval_t* keyval = value->keyvals[i];
        if(strcmp(keyval->key, ORTE_PROC_RANK_KEY) == 0) {
            if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(rc);
            }
            rank2 = *sptr;
            continue;
        }
        if(strcmp(keyval->key, ORTE_PROC_APP_CONTEXT_KEY) == 0) {
            if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(rc);
            }
            app2 = *sptr;
            continue;
        }
    }
    if(app1 < app2)
        return -1;
    if(app1 > app2)
        return +1;
    if(rank1 < rank2)
        return -1;
    if(rank1 > rank2)
        return +1;
    return 0;
}


/**
 * Obtain the mapping for this job, and the list of nodes confined to that mapping.
 *
 * Use this instead of orte_ras_base_node_query when past the RMAPS framework
 *  since components like the PLS are only conserned with those nodes that they
 *  been mapped on, not all of the nodes allocated to their job. In the case
 *  where we are allocated 10 nodes from the RAS, but only map to 2 of them
 *  then we don't try to launch orteds on all 10 nodes, just the 2 mapped.
 */
int orte_rmaps_base_mapped_node_query(opal_list_t* mapping_list, opal_list_t* nodes_alloc, orte_jobid_t jobid)
{
    opal_list_item_t *item_a, *item_m, *item_n;
    int num_mapping = 0;
    int rc = ORTE_SUCCESS;
    bool matched = false;

    /* get the mapping for this job */
    rc = orte_rmaps_base_get_map(jobid, mapping_list);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    num_mapping = opal_list_get_size(mapping_list);

    /* Create a list of nodes that are in the mapping */
    for( item_m  = opal_list_get_first(mapping_list);
         item_m != opal_list_get_end(mapping_list);
         item_m  = opal_list_get_next(item_m)) {
        orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*)item_m;

        /* Iterate over all the nodes mapped and check them against the
         * allocated node list */
        for( item_n  = opal_list_get_first(&(map->nodes));
             item_n != opal_list_get_end(&(map->nodes));
             item_n  = opal_list_get_next(item_n)) {
            orte_rmaps_base_node_t* rmaps_node = (orte_rmaps_base_node_t*)item_n;
            matched = false;

            /* If this node is in the list already, skip it */
            if(num_mapping > 1) {
                for( item_a  = opal_list_get_first(nodes_alloc);
                     item_a != opal_list_get_end(nodes_alloc);
                     item_a  = opal_list_get_next(item_a)) {
                    orte_ras_node_t* ras_node = (orte_ras_node_t*)item_a;
                    if( rmaps_node->node == ras_node) {
                        matched = true;
                        break;
                    }
                }
                if(matched) {
                    continue;
                }
            }

            /* Otherwise
             *  - Add it to the allocated list of nodes
             */
            OBJ_RETAIN(rmaps_node->node);
            opal_list_append(nodes_alloc, &rmaps_node->node->super);
        }
    }

    return rc;
}


/**
 * Lookup node (if it exists) in the list. If it doesn't exist, create a new
 * node and append to the table.
 */

static orte_rmaps_base_node_t*
orte_rmaps_lookup_node(opal_list_t* rmaps_nodes, opal_list_t* ras_nodes, char* node_name, orte_rmaps_base_proc_t* proc)
{
    opal_list_item_t* item;
    for(item =  opal_list_get_first(rmaps_nodes);
        item != opal_list_get_end(rmaps_nodes);
        item =  opal_list_get_next(item)) {
        orte_rmaps_base_node_t* node = (orte_rmaps_base_node_t*)item;
        if(strcmp(node->node->node_name, node_name) == 0) {
            OBJ_RETAIN(proc);
            opal_list_append(&node->node_procs, &proc->super);
            return node;
        }
    }
    for(item = opal_list_get_first(ras_nodes);
        item != opal_list_get_end(ras_nodes);
        item = opal_list_get_next(item)) {
        orte_ras_node_t* ras_node = (orte_ras_node_t*)item;
        if(strcmp(ras_node->node_name, node_name) == 0) {
            orte_rmaps_base_node_t* node = OBJ_NEW(orte_rmaps_base_node_t);
            OBJ_RETAIN(ras_node);
            node->node = ras_node;
            OBJ_RETAIN(proc);
            opal_list_append(&node->node_procs, &proc->super);
            opal_list_prepend(rmaps_nodes, &node->super);
            return node;
        }
    }
    return NULL;
}


/**
 *  Query the process mapping from the registry.
 */

int orte_rmaps_base_get_map(orte_jobid_t jobid, opal_list_t* mapping_list)
{
    orte_app_context_t** app_context = NULL;
    orte_rmaps_base_map_t** mapping = NULL;
    opal_list_t nodes;
    opal_list_item_t* item;
    orte_std_cntr_t i, num_context = 0;
    orte_std_cntr_t *sptr;
    orte_process_name_t *pptr;
    pid_t *pidptr;
    char* segment = NULL;
    orte_gpr_value_t** values;
    orte_std_cntr_t v, num_values;
    int rc;
    char* keys[] = {
        ORTE_PROC_RANK_KEY,
        ORTE_PROC_NAME_KEY,
        ORTE_PROC_APP_CONTEXT_KEY,
        ORTE_PROC_PID_KEY,
        ORTE_PROC_LOCAL_PID_KEY,
        ORTE_NODE_NAME_KEY,
        NULL
    };

    /* query the application context */
    if(ORTE_SUCCESS != (rc = orte_rmgr_base_get_app_context(jobid, &app_context, &num_context))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* query the node list */
    OBJ_CONSTRUCT(&nodes, opal_list_t);
    if(ORTE_SUCCESS != (rc = orte_ras_base_node_query_alloc(&nodes,jobid))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* build the mapping */
    if(NULL == (mapping = (orte_rmaps_base_map_t**)malloc(sizeof(orte_rmaps_base_map_t*) * num_context))) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    for(i=0; i<num_context; i++) {
        orte_rmaps_base_map_t* map = OBJ_NEW(orte_rmaps_base_map_t);
        orte_app_context_t* app = app_context[i];
        map->app = app;
        if (0 < app->num_procs) {
            map->procs = (orte_rmaps_base_proc_t**)malloc(sizeof(orte_rmaps_base_proc_t*) * app->num_procs);
            if(NULL == map->procs) {
                OBJ_RELEASE(map);
                rc = ORTE_ERR_OUT_OF_RESOURCE;
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        } else {
            if (1 < num_context) {  /** can't have multiple contexts if zero num_procs */
                ORTE_ERROR_LOG(ORTE_ERR_INVALID_NUM_PROCS);
                rc = ORTE_ERR_INVALID_NUM_PROCS;
                goto cleanup;
            }
        }
        map->num_procs = 0;
        mapping[i] = map;
    }
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* query the process list from the registry */
    rc = orte_gpr.get(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
        segment,
        NULL,
        keys,
        &num_values,
        &values);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* sort the response */
    qsort(values, num_values, sizeof(orte_gpr_value_t*),
        (int (*)(const void*,const void*))orte_rmaps_value_compare);

    /* build the proc list */
    for(v=0; v<num_values; v++) {
        orte_gpr_value_t* value = values[v];
        orte_rmaps_base_map_t* map = NULL;
        orte_rmaps_base_proc_t* proc;
        char* node_name = NULL;
        orte_std_cntr_t kv, app_index;

        proc = OBJ_NEW(orte_rmaps_base_proc_t);
        if(NULL == proc) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        for(kv = 0; kv<value->cnt; kv++) {
            orte_gpr_keyval_t* keyval = value->keyvals[kv];
            if(strcmp(keyval->key, ORTE_PROC_RANK_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                proc->proc_rank = *sptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_NAME_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&pptr, keyval->value, ORTE_NAME))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                proc->proc_name = *pptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_APP_CONTEXT_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                app_index = *sptr;
                if(app_index >= num_context) {
                    rc = ORTE_ERR_BAD_PARAM;
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                map = mapping[app_index];
                proc->app = strdup(app_context[app_index]->app);
                continue;
            }
            if (strcmp(keyval->key, ORTE_PROC_PID_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&pidptr, keyval->value, ORTE_PID))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                proc->pid = *pidptr;
                continue;
            }
            if (strcmp(keyval->key, ORTE_PROC_LOCAL_PID_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&pidptr, keyval->value, ORTE_PID))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                proc->local_pid = *pidptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_NAME_KEY) == 0) {
                /* use the dss.copy function here to protect us against zero-length strings */
                if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&node_name, keyval->value->data, ORTE_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                continue;
            }
        }
        /* global record */
        if(NULL == map) {
            OBJ_RELEASE(proc);
            continue;
        }
        /*
         * This seems like a dummy check, but it ensures that we fail
         * rather than overrun our array. This can happen if the 
         * indicies on the app schemas are incorrect
         */
        if(map->num_procs < map->app->num_procs) {
            map->procs[map->num_procs++] = proc;
            proc->proc_node = orte_rmaps_lookup_node(&map->nodes, &nodes, node_name, proc);
        }
        else {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            goto cleanup;
        }
    }

    /* cleanup any nodes allocated and not mapped */
    while(NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);

    /* release temporary variables */
    for(i=0; i<num_context; i++) {
        opal_list_append(mapping_list, &mapping[i]->super);
    }
    free(segment);
    free(app_context);
    free(mapping);
    return ORTE_SUCCESS;

cleanup:
    if(NULL != segment)
        free(segment);
    if(NULL != app_context) {
        for(i=0; i<num_context; i++) {
            OBJ_RELEASE(app_context[i]);
        }
        free(app_context);
    }
    if(NULL != mapping) {
        for(i=0; i<num_context; i++) {
            if(NULL != mapping[i])
                OBJ_RELEASE(mapping[i]);
        }
        free(mapping);
    }

    /* cleanup any nodes allocated and not mapped */
    while(NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);
    return rc;
}

/**
 *  Query the process mapping for a specified node from the registry.
 */

int orte_rmaps_base_get_node_map(
    orte_cellid_t cellid,
    orte_jobid_t jobid,
    const char* hostname,
    opal_list_t* mapping_list)
{
    orte_app_context_t** app_context = NULL;
    orte_rmaps_base_map_t** mapping = NULL;
    orte_ras_node_t *ras_node = NULL;
    orte_gpr_keyval_t *condition;
    orte_std_cntr_t i, num_context = 0;
    orte_std_cntr_t *sptr;
    pid_t *pidptr;
    orte_process_name_t *pptr;
    char* segment = NULL;
    orte_gpr_value_t** values;
    orte_std_cntr_t v, num_values;
    int rc;
    char* keys[] = {
        ORTE_PROC_RANK_KEY,
        ORTE_PROC_NAME_KEY,
        ORTE_PROC_APP_CONTEXT_KEY,
        ORTE_PROC_PID_KEY,
        ORTE_PROC_LOCAL_PID_KEY,
        ORTE_NODE_NAME_KEY,
        NULL
    };

    /* allocate the node */
    if(NULL == (ras_node = orte_ras_base_node_lookup(cellid,hostname))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    /* query the application context */
    if(ORTE_SUCCESS != (rc = orte_rmgr_base_get_app_context(jobid, &app_context, &num_context))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if(NULL == (mapping = (orte_rmaps_base_map_t**)malloc(sizeof(orte_rmaps_base_map_t*) * num_context))) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    for(i=0; i<num_context; i++) {
        orte_rmaps_base_map_t* map = OBJ_NEW(orte_rmaps_base_map_t);
        orte_app_context_t* app = app_context[i];
        OBJ_RETAIN(app);
        map->app = app;
        map->procs = (orte_rmaps_base_proc_t**)malloc(sizeof(orte_rmaps_base_proc_t*) * app->num_procs);
        if(NULL == map->procs) {
            OBJ_RELEASE(map);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        map->num_procs = 0;
        mapping[i] = map;
    }

    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* setup condition/filter for query - return only processes that
     * are assigned to the specified node name
     */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&condition, ORTE_NODE_NAME_KEY, ORTE_STRING, (void*)hostname))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* query the process list from the registry */
    rc = orte_gpr.get_conditional(
         ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
         segment,
         NULL,
         keys,
         1,
         &condition,
         &num_values,
         &values);

    /* sort the response */
    qsort(values, num_values, sizeof(orte_gpr_value_t*),
        (int (*)(const void*,const void*))orte_rmaps_value_compare);

    /* build the proc list */
    for(v=0; v<num_values; v++) {
        orte_gpr_value_t* value = values[v];
        orte_rmaps_base_map_t* map = NULL;
        orte_rmaps_base_node_t *node = NULL;
        orte_rmaps_base_proc_t* proc;
        char* node_name = NULL;
        orte_std_cntr_t kv, app_index;

        proc = OBJ_NEW(orte_rmaps_base_proc_t);
        if(NULL == proc) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        for(kv = 0; kv<value->cnt; kv++) {
            orte_gpr_keyval_t* keyval = value->keyvals[kv];
            if(strcmp(keyval->key, ORTE_PROC_RANK_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                proc->proc_rank = *sptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_NAME_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&pptr, keyval->value, ORTE_NAME))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                proc->proc_name = *pptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_PROC_APP_CONTEXT_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, keyval->value, ORTE_STD_CNTR))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                app_index = *sptr;
                if(app_index >= num_context) {
                    rc = ORTE_ERR_BAD_PARAM;
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                map = mapping[app_index];
                if(opal_list_get_size(&map->nodes) == 0) {
                    node = OBJ_NEW(orte_rmaps_base_node_t);
                    if(NULL == node) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        goto cleanup;
                    }
                    OBJ_RETAIN(ras_node);
                    node->node = ras_node;
                    opal_list_append(&map->nodes, &node->super);
                } else {
                    node = (orte_rmaps_base_node_t*)opal_list_get_first(&map->nodes);
                }
                proc->app = strdup(app_context[app_index]->app);
                continue;
            }
            if (strcmp(keyval->key, ORTE_PROC_PID_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&pidptr, keyval->value, ORTE_PID))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                proc->pid = *pidptr;
                continue;
            }
            if (strcmp(keyval->key, ORTE_PROC_LOCAL_PID_KEY) == 0) {
                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&pidptr, keyval->value, ORTE_PID))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                proc->local_pid = *pidptr;
                continue;
            }
            if(strcmp(keyval->key, ORTE_NODE_NAME_KEY) == 0) {
                /* use the dss.copy function here to protect us against zero-length strings */
                if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&node_name, keyval->value->data, ORTE_STRING))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                continue;
            }
       }
        /* skip this entry? */
        if(NULL == map ||
           proc->proc_name.cellid != cellid) {
            OBJ_RELEASE(proc);
            continue;
        }
        map->procs[map->num_procs++] = proc;
        OBJ_RETAIN(proc);
        opal_list_append(&node->node_procs, &proc->super);
        proc->proc_node = node;
    }

    /* return mapping for the entries that have procs on this node */
    for(i=0; i<num_context; i++) {
        orte_rmaps_base_map_t* map = mapping[i];
        if(map->num_procs) {
            opal_list_append(mapping_list, &map->super);
        } else {
            OBJ_RELEASE(map);
        }
    }

    /* decrement reference count on node */
    OBJ_RELEASE(ras_node);

    /* release all app context - note the reference count was bumped
     * if saved in the map
    */
    for(i=0; i<num_context; i++) {
        OBJ_RELEASE(app_context[i]);
    }
    free(segment);
    free(app_context);
    free(mapping);
    OBJ_RELEASE(condition);
    return ORTE_SUCCESS;

cleanup:
    if(NULL != segment)
        free(segment);
    if(NULL != app_context) {
        for(i=0; i<num_context; i++) {
            OBJ_RELEASE(app_context[i]);
        }
        free(app_context);
    }
    if(NULL != mapping) {
        for(i=0; i<num_context; i++) {
            if(NULL != mapping[i])
                OBJ_RELEASE(mapping[i]);
        }
        free(mapping);
    }
    if (NULL != condition)
        OBJ_RELEASE(condition);

    return rc;
}

/**
 * Set the process mapping in the registry.
 */

int orte_rmaps_base_set_map(orte_jobid_t jobid, opal_list_t* mapping_list)
{
    orte_std_cntr_t i, j;
    orte_std_cntr_t index=0;
    orte_std_cntr_t num_procs = 0;
    int rc = ORTE_SUCCESS;
    opal_list_item_t* item;
    orte_gpr_value_t** values;
    char *segment;

    for(item =  opal_list_get_first(mapping_list);
        item != opal_list_get_end(mapping_list);
        item =  opal_list_get_next(item)) {
        orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*)item;
        num_procs += map->num_procs;
    }
    if(num_procs == 0) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /**
     * allocate value array. We need to reserve one extra spot so we can set the counter
     * for the process INIT state to indicate that all procs are at that state. This will
     * allow the INIT trigger to fire.
     */
    values = (orte_gpr_value_t**)malloc((1+num_procs) * sizeof(orte_gpr_value_t*));
    if(NULL == values) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment,jobid))) {
        ORTE_ERROR_LOG(rc);
        free(values);
        return rc;
    }

    /** setup the last value in the array to update the INIT counter */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&(values[num_procs]),
                                            ORTE_GPR_OVERWRITE|ORTE_GPR_TOKENS_AND,
                                            segment, 1, 1))) {
        ORTE_ERROR_LOG(rc);
        free(values);
        free(segment);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[num_procs]->keyvals[0]), ORTE_PROC_NUM_AT_INIT, ORTE_STD_CNTR, &num_procs))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    values[num_procs]->tokens[0] = strdup(ORTE_JOB_GLOBALS); /* counter is in the job's globals container */


    for(i=0; i<num_procs; i++) {
        if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&(values[i]),
                                    ORTE_GPR_OVERWRITE|ORTE_GPR_TOKENS_AND,
                                    segment, 7, 0))) {
             ORTE_ERROR_LOG(rc);
             for(j=0; j<i; j++) {
                 OBJ_RELEASE(values[j]);
             }
             free(values);
             free(segment);
             return rc;
         }
    }

    /* iterate through all processes and initialize value array */
    for(item =  opal_list_get_first(mapping_list);
        item != opal_list_get_end(mapping_list);
        item =  opal_list_get_next(item)) {
        orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*)item;
        orte_std_cntr_t p;
        for(p=0; p<map->num_procs; p++) {
            orte_rmaps_base_proc_t* proc = map->procs[p];
            orte_gpr_value_t* value = values[index++];
            orte_proc_state_t proc_state=ORTE_PROC_STATE_INIT;

            /* initialize keyvals */
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_PROC_RANK_KEY, ORTE_STD_CNTR, &(proc->proc_rank)))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[1]), ORTE_PROC_NAME_KEY, ORTE_NAME, &(proc->proc_name)))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[2]), ORTE_NODE_NAME_KEY, ORTE_STRING, proc->proc_node->node->node_name))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[3]), ORTE_PROC_APP_CONTEXT_KEY, ORTE_STD_CNTR, &(map->app->idx)))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[4]), ORTE_PROC_STATE_KEY, ORTE_PROC_STATE, &proc_state))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[5]), ORTE_PROC_PID_KEY, ORTE_PID, &(proc->pid)))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[6]), ORTE_PROC_LOCAL_PID_KEY, ORTE_PID, &(proc->local_pid)))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }

            /* set the tokens */
            if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&(value->tokens), &(value->num_tokens), &(proc->proc_name)))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
    }

    /* insert all values in one call */
    if (ORTE_SUCCESS != (rc = orte_gpr.put((1+num_procs), values))) {
        ORTE_ERROR_LOG(rc);
    }

cleanup:
    for(i=0; i<num_procs; i++) {
        if(NULL != values[i]) {
            OBJ_RELEASE(values[i]);
        }
    }
    if(NULL != values)
        free(values);
    return rc;
}


/*
 *  Set the vpid start and range on the "global" job segment.
 */

int orte_rmaps_base_set_vpid_range(orte_jobid_t jobid, orte_vpid_t start, orte_vpid_t range)
{
    orte_gpr_value_t *value;
    char *segment;
    int rc;

    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, ORTE_GPR_OVERWRITE, segment, 2, 1))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }
    free(segment);
    value->tokens[0] = strdup(ORTE_JOB_GLOBALS);

    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_JOB_VPID_START_KEY, ORTE_VPID, &start))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[1]), ORTE_JOB_VPID_RANGE_KEY, ORTE_VPID, &range))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }

    rc = orte_gpr.put(1, &value);
    if (ORTE_SUCCESS != rc) ORTE_ERROR_LOG(rc);

    OBJ_RELEASE(value);
    return rc;
}


/*
 *  Get the vpid start and range from the "global" job segment.
 */

int orte_rmaps_base_get_vpid_range(orte_jobid_t jobid, orte_vpid_t *start, orte_vpid_t *range)
{
    char *segment;
    char *tokens[2];
    char *keys[3];
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, num_values = 0;
    orte_vpid_t *vptr;
    int rc;

    /* query the job segment on the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    tokens[0] = ORTE_JOB_GLOBALS;
    tokens[1] = NULL;

    keys[0] = ORTE_JOB_VPID_START_KEY;
    keys[1] = ORTE_JOB_VPID_RANGE_KEY;
    keys[2] = NULL;

    rc = orte_gpr.get(
        ORTE_GPR_KEYS_AND|ORTE_GPR_TOKENS_OR,
        segment,
        tokens,
        keys,
        &num_values,
        &values
        );
    if(rc != ORTE_SUCCESS) {
        free(segment);
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if(num_values != 1) {
        rc = ORTE_ERR_NOT_FOUND;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    for(i=0; i<values[0]->cnt; i++) {
         if(strcmp(values[0]->keyvals[i]->key, ORTE_JOB_VPID_START_KEY) == 0) {
             if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&vptr, values[0]->keyvals[i]->value, ORTE_VPID))) {
                 ORTE_ERROR_LOG(rc);
                 goto cleanup;
             }
             *start = *vptr;
             continue;
         }
         if(strcmp(values[0]->keyvals[i]->key, ORTE_JOB_VPID_RANGE_KEY) == 0) {
             if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&vptr, values[0]->keyvals[i]->value, ORTE_VPID))) {
                 ORTE_ERROR_LOG(rc);
                 goto cleanup;
             }
             *range = *vptr;
             continue;
         }
    }

cleanup:
    for(i=0; i<num_values; i++)
        OBJ_RELEASE(values[i]);
    free(segment);
    free(values);
    return rc;
}

